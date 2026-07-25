;;; qq-media.el --- QQ-specific media helpers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Resource helpers specialized for NapCat/QQ
;; resource types such as avatars, base emojis, and OneBot file/image segments.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'appkit-media)
(require 'qq-account)
(require 'qq-api)
(require 'qq-customize)
(require 'qq-remote-media)
(require 'qq-rpc)
(require 'qq-server)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-media-animated-face-image-height)

(defvar qq-media-cache-update-hook nil
  "Hook run after media resource/image cache updates.")

(defvar qq-media--resource-cache (make-hash-table :test #'equal)
  "Simple in-memory resource cache keyed by logical resource identity.")

(defvar qq-media--image-cache (make-hash-table :test #'equal)
  "In-memory image object cache keyed by logical resource identity.")

(defvar qq-media--preview-missing-cache (make-hash-table :test #'equal)
  "Preview keys whose current media source could not produce an image.")

(defvar qq-media--fetching-cache (make-hash-table :test #'equal)
  "Set of logical resource identities currently being fetched.")

(defvar qq-media--download-state-table (make-hash-table :test #'equal)
  "Download state plist table keyed by QQ media logical identity.")

(defvar qq-media--native-record-playbacks (make-hash-table :test #'equal)
  "Telega-style playback state keyed by native record media ID.")

(defvar qq-media--native-record-current-id nil
  "Media ID of the one preparing, playing, or paused native record.")

(defvar qq-media--native-record-progress-timer nil
  "Timer refreshing the one active native record progress row.")

(defvar qq-media--account-generation 0
  "Account generation owning asynchronous media cache callbacks.")

(defvar qq-media--custom-faces nil
  "Cached list of favorite custom-face alists from NapCat.")

(defvar qq-media--custom-faces-fetched-at nil
  "Float time when `qq-media--custom-faces' was last fetched.")

(defvar qq-media--custom-face-waiters nil
  "Callbacks waiting for the shared favorite-face refresh.")

(defvar qq-media--custom-face-refresh-owner nil
  "Owner plist for the current favorite-face refresh, or nil.

The plist records the account `:generation', current page `:request', and
the adopted transport `:token'.")

(defvar qq-media--custom-face-completion-pairs nil
  "Active `(LABEL . FACE)' pairs for favorite-face `completing-read'.")

(defun qq-media--custom-face-owner-generation-current-p (owner)
  "Return non-nil when OWNER belongs to the current account generation."
  (= (or (plist-get owner :generation) -1)
     qq-media--account-generation))

(defun qq-media--custom-face-owner-current-p (owner)
  "Return non-nil when OWNER owns the current favorite-face request."
  (and (eq owner qq-media--custom-face-refresh-owner)
       (qq-media--custom-face-owner-generation-current-p owner)))

(defun qq-media--custom-face-request-current-p (owner request)
  "Return non-nil when REQUEST is OWNER's current favorite-face page."
  (and (qq-media--custom-face-owner-current-p owner)
       (eq request (plist-get owner :request))))

(defun qq-media--cancel-custom-face-request (token)
  "Cancel favorite-face request TOKEN, isolating cancellation failures."
  (when token
    (condition-case err
        (qq-api-cancel-request token)
      (error
       (message "qq: favorite-face request cancellation failed: %s"
                (error-message-string err)))
      (quit
       (message "qq: favorite-face request cancellation was interrupted")))))

(defun qq-media--revoke-custom-face-work ()
  "Revoke favorite-face callbacks and cancel their transport request."
  ;; Invalidate callback ownership before cancellation.  Some transports run
  ;; an errback synchronously from their cancellation path.
  (cl-incf qq-media--account-generation)
  (let ((token (and qq-media--custom-face-refresh-owner
                    (plist-get qq-media--custom-face-refresh-owner :token))))
    (setq qq-media--custom-face-refresh-owner nil
          qq-media--custom-face-waiters nil
          qq-media--custom-faces nil
          qq-media--custom-faces-fetched-at nil
          qq-media--custom-face-completion-pairs nil)
    (qq-media--cancel-custom-face-request token)))

(defun qq-media-clear-cache ()
  "Clear all account-owned media caches and asynchronous work."
  (interactive)
  (qq-media--stop-all-native-record-playback)
  (qq-media--revoke-custom-face-work)
  (condition-case nil
      (appkit-media-clear-video-decoration-cache 'qq)
    (error nil)
    (quit nil))
  (let (keys transfers remote-operations)
    (maphash
     (lambda (key fetching)
       (push key keys)
       (cond
        ((appkit-media-transfer-p fetching)
         (push fetching transfers))
        ((and (qq-remote-media-operation-p fetching)
              (qq-remote-media-operation-active-p fetching))
         (push fetching remote-operations))))
     qq-media--fetching-cache)
    (maphash
     (lambda (_key entry)
       (when-let* ((transfer (plist-get entry :transfer)))
         (when (appkit-media-transfer-p transfer)
           (push transfer transfers))))
     qq-media--download-state-table)
    (dolist (key keys)
      (condition-case nil
          (appkit-media-cancel-video-preview (concat "qq:" key))
        (error nil)
        (quit nil)))
    (dolist (transfer transfers)
      (condition-case nil
          (appkit-media-cancel-transfer transfer)
        (error nil)
        (quit nil)))
    (dolist (operation remote-operations)
      (condition-case nil
          (qq-remote-media-cancel-operation operation)
        (error nil)
        (quit nil))))
  (clrhash qq-media--resource-cache)
  (clrhash qq-media--image-cache)
  (clrhash qq-media--preview-missing-cache)
  (clrhash qq-media--fetching-cache)
  (clrhash qq-media--download-state-table)
  (when (file-directory-p qq-media-cache-directory)
    (ignore-errors (delete-directory qq-media-cache-directory t)))
  (message "qq: media cache cleared"))

(defun qq-media--appkit-resource (resource)
  "Adapt QQ RESOURCE to the strict appkit media resource contract."
  (appkit-media-resource-create
   :file (alist-get 'file resource)
   :url (alist-get 'url resource)
   :name (alist-get 'name resource)
   :mime-type (alist-get 'mime-type resource)))

(defun qq-media--cached-resource (key)
  "Return cached resource for KEY when still usable."
  (let ((resource (copy-tree (gethash key qq-media--resource-cache))))
    (when resource
      (if (or (appkit-media-file-present-p (alist-get 'file resource))
              (appkit-media-url-present-p (alist-get 'url resource)))
          resource
        (remhash key qq-media--resource-cache)
        nil))))

(defun qq-media--cache-resource (key resource)
  "Store RESOURCE under KEY and return RESOURCE."
  (puthash key (copy-tree resource) qq-media--resource-cache)
  resource)

(defun qq-media--cached-image (key)
  "Return cached image object for KEY when valid."
  (let ((image (gethash key qq-media--image-cache)))
    (when image
      (condition-case _
          (progn
            (image-size image t)
            image)
        (error
         (remhash key qq-media--image-cache)
         nil)))))

(defun qq-media--cache-image (key image)
  "Store IMAGE object under KEY and return IMAGE."
  (when image
    (puthash key image qq-media--image-cache))
  image)

(defun qq-media--note-cache-updated (&optional media-key)
  "Notify UI that media cache content changed.

When MEDIA-KEY is non-nil, it identifies the logical cache entry that changed.

Defer the hook to the next command loop via `run-at-time'.  Asynchronous
transfer callbacks can run outside a safe redisplay context; immediate
`erase-buffer' in special-mode forward viewers is unreliable from filters."
  (run-at-time
   0 nil
   (lambda ()
     (run-hook-with-args 'qq-media-cache-update-hook media-key))))

(defun qq-media--native-media-id (segment expected-type)
  "Return SEGMENT's native media ID when its type is EXPECTED-TYPE."
  (when (equal (alist-get 'type segment) expected-type)
    (let ((media-id (alist-get 'media_id (alist-get 'data segment))))
      (and (qq-remote-media--id-p media-id) media-id))))

(defun qq-media--native-record-media-id (segment)
  "Return validated native record media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "record"))

(defun qq-media--native-image-media-id (segment)
  "Return validated native image media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "image"))

(defun qq-media--native-video-media-id (segment)
  "Return validated native video media ID from SEGMENT, or nil."
  (qq-media--native-media-id segment "video"))

(defun qq-media--native-record-key (media-id)
  "Return logical media cache key for native record MEDIA-ID."
  (format "record:%s" media-id))

(defun qq-media--native-image-key (media-id)
  "Return logical media cache key for native image MEDIA-ID."
  (format "image:%s" media-id))

(defun qq-media--native-video-key (media-id)
  "Return logical content cache key for native video MEDIA-ID."
  (format "video:%s" media-id))

(defun qq-media--native-video-thumbnail-key (media-id)
  "Return logical thumbnail cache key for native video MEDIA-ID."
  (format "video-thumbnail:%s" media-id))

(defun qq-media-native-record-playback-state (segment-or-media-id)
  "Return public playback state for SEGMENT-OR-MEDIA-ID, or nil.

The result intentionally excludes the process, operation, local-access ID,
and ephemeral filesystem path retained by the private player state."
  (let ((media-id (if (stringp segment-or-media-id)
                      segment-or-media-id
                    (qq-media--native-record-media-id segment-or-media-id))))
    (when-let* ((entry
                 (and media-id
                      (gethash media-id qq-media--native-record-playbacks))))
      ;; Emacs does not guarantee that a process exit sentinel has run before
      ;; the next state read.  Finalize an exited process synchronously so UI
      ;; state and local-access lease ownership cannot remain stuck at
      ;; `playing'.  The sentinel checks exact process ownership and is
      ;; idempotent if an exit event was already queued.
      (when-let* ((process (plist-get entry :process))
                  ((processp process))
                  ((not (process-live-p process))))
        (qq-media--native-record-player-sentinel process "state poll\n")
        (setq entry (gethash media-id qq-media--native-record-playbacks)))
      (list :media-id media-id
            :status (plist-get entry :status)
            :duration-seconds (plist-get entry :duration-seconds)
            :played-seconds (qq-media--native-record-played-seconds entry)
            :error (plist-get entry :error)))))

(defun qq-media--native-record-played-seconds (entry)
  "Return current playback position represented by native record ENTRY."
  (let* ((played (or (plist-get entry :played-seconds) 0.0))
         (started-at (plist-get entry :started-at))
         (duration (plist-get entry :duration-seconds))
         (position
          (if (and (eq (plist-get entry :status) 'playing)
                   (numberp started-at))
              (+ played (max 0.0 (- (float-time) started-at)))
            played)))
    (if (and (numberp duration) (> duration 0))
        (min (float duration) position)
      position)))

(defun qq-media--cancel-native-record-progress-timer ()
  "Cancel the native record progress redisplay timer."
  (when (timerp qq-media--native-record-progress-timer)
    (cancel-timer qq-media--native-record-progress-timer))
  (setq qq-media--native-record-progress-timer nil))

(defun qq-media--native-record-progress-tick ()
  "Redisplay the active native record while its player is running."
  (let* ((media-id qq-media--native-record-current-id)
         (entry (and media-id
                     (gethash media-id qq-media--native-record-playbacks)))
         (process (and entry (plist-get entry :process))))
    (if (and (eq (plist-get entry :status) 'playing)
             (processp process)
             (process-live-p process))
        (qq-media--notify-native-record-state media-id)
      (qq-media--cancel-native-record-progress-timer))))

(defun qq-media--ensure-native-record-progress-timer ()
  "Start one shared native record progress redisplay timer."
  (unless (timerp qq-media--native-record-progress-timer)
    (setq qq-media--native-record-progress-timer
          (run-at-time 1 1 #'qq-media--native-record-progress-tick))))

(defun qq-media--record-player-command ()
  "Return normalized native-record player arguments, or nil."
  (appkit-media-command-arguments qq-media-record-player-command))

(defun qq-media-native-record-playback-available-p ()
  "Return non-nil when the configured native-record player is runnable."
  (appkit-media-command-runnable-p qq-media-record-player-command))

(defun qq-media--notify-native-record-state (media-id)
  "Notify open chats that native record MEDIA-ID changed playback state."
  (qq-media--note-cache-updated (qq-media--native-record-key media-id)))

(defun qq-media--close-local-access (access-id)
  "Best-effort revoke local resource ACCESS-ID."
  (when (qq-resource--local-access-id-p access-id)
    (condition-case error-data
        (when (and (qq-server-ready-p)
                   (qq-rpc-method-available-p "resource.close_local"))
          (qq-resource-close-local
           access-id nil
           (lambda (_body reason)
             (message "qq: failed to close local resource lease: %s" reason))))
      (error
       ;; The service TTL remains the disconnected-client fallback.
       (message "qq: could not request resource lease closure: %s"
                (error-message-string error-data))))))

(defun qq-media--dispose-native-record-entry (media-id &optional status error-text)
  "Stop and clean native record MEDIA-ID, recording STATUS and ERROR-TEXT."
  (when-let* ((entry (gethash media-id qq-media--native-record-playbacks)))
    (let ((operation (plist-get entry :operation))
          (process (plist-get entry :process))
          (buffer (plist-get entry :buffer))
          (access-id (plist-get entry :access-id))
          (owner-handle (plist-get entry :owner-handle)))
      ;; Revoke callback ownership before cancellation; transport cancellation
      ;; may synchronously run an errback.
      (setq entry (plist-put entry :operation nil))
      (setq entry (plist-put entry :process nil))
      (setq entry (plist-put entry :buffer nil))
      (setq entry (plist-put entry :access-id nil))
      (setq entry (plist-put entry :owner-handle nil))
      (setq entry (plist-put entry :status (or status 'stopped)))
      (setq entry (plist-put entry :error error-text))
      (puthash media-id entry qq-media--native-record-playbacks)
      (when (and (qq-remote-media-operation-p operation)
                 (qq-remote-media-operation-active-p operation))
        (qq-remote-media-cancel-operation operation))
      (when (processp process)
        (set-process-filter process nil)
        (set-process-sentinel process nil)
        (when (process-live-p process)
          (delete-process process))
        (set-process-plist process nil))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (and (appkit-handle-p owner-handle)
                 (appkit-handle-alive-p owner-handle))
        (appkit-retire-handle owner-handle))
      (qq-media--close-local-access access-id)
      (when (equal qq-media--native-record-current-id media-id)
        (setq qq-media--native-record-current-id nil)
        (qq-media--cancel-native-record-progress-timer))
      (qq-media--notify-native-record-state media-id)
      entry)))

(defun qq-media--stop-all-native-record-playback ()
  "Cancel every native-record preparation/player and revoke its lease."
  (qq-media--cancel-native-record-progress-timer)
  (let (media-ids)
    (maphash (lambda (media-id _entry) (push media-id media-ids))
             qq-media--native-record-playbacks)
    (dolist (media-id media-ids)
      (qq-media--dispose-native-record-entry media-id 'stopped))
    (clrhash qq-media--native-record-playbacks)
    (setq qq-media--native-record-current-id nil)))

(defun qq-media--native-record-player-sentinel (process _event)
  "Finalize exact native record player PROCESS after exit."
  (unless (process-live-p process)
    (let* ((media-id (plist-get (process-plist process) :qq-media-id))
           (entry (and media-id
                       (gethash media-id qq-media--native-record-playbacks))))
      (when (and entry (eq process (plist-get entry :process)))
        (let ((buffer (plist-get entry :buffer))
              (access-id (plist-get entry :access-id))
              (owner-handle (plist-get entry :owner-handle))
              (successful (= (process-exit-status process) 0))
              (played-seconds
               (qq-media--native-record-played-seconds entry)))
          (setq entry (plist-put entry :process nil))
          (setq entry (plist-put entry :buffer nil))
          (setq entry (plist-put entry :access-id nil))
          (setq entry (plist-put entry :owner-handle nil))
          (setq entry (plist-put entry :status
                                 (if successful 'finished 'failed)))
          (setq entry (plist-put entry :played-seconds
                                 (if successful
                                     (or (plist-get entry :duration-seconds)
                                         played-seconds)
                                   played-seconds)))
          (setq entry (plist-put entry :started-at nil))
          (setq entry (plist-put entry :error
                                 (unless successful "record player exited abnormally")))
          (puthash media-id entry qq-media--native-record-playbacks)
          (set-process-plist process nil)
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (when (and (appkit-handle-p owner-handle)
                     (appkit-handle-alive-p owner-handle))
            (appkit-retire-handle owner-handle))
          (qq-media--close-local-access access-id)
          (when (equal qq-media--native-record-current-id media-id)
            (setq qq-media--native-record-current-id nil)
            (qq-media--cancel-native-record-progress-timer))
          (qq-media--notify-native-record-state media-id))))))

(defun qq-media--start-native-record-player (media-id result)
  "Start configured player for prepared native record MEDIA-ID using RESULT."
  (let* ((access (alist-get 'access result))
         (access-id (alist-get 'access_id access))
         (path (alist-get 'path access))
         (argv (qq-media--record-player-command))
         (program (car argv))
         (entry (gethash media-id qq-media--native-record-playbacks))
         (buffer (generate-new-buffer " *qq-record-player*"))
         process)
    (if (not (and entry
                  program
                  (qq-media-native-record-playback-available-p)
                  (stringp path)
                  (file-regular-p path)))
        (progn
          (when (buffer-live-p buffer)
            (kill-buffer buffer))
          (qq-media--close-local-access access-id)
          (qq-media--dispose-native-record-entry
           media-id 'failed "record playback input or player is unavailable")
          (message "qq: Native record playback input or player is unavailable")
          nil)
      (setq entry (plist-put entry :access-id access-id))
      (setq entry (plist-put entry :buffer buffer))
      (puthash media-id entry qq-media--native-record-playbacks)
      (condition-case error-data
          (progn
            (setq process
                  (make-process
                   :name "qq-record-player"
                   :buffer buffer
                   :command (append argv (list path))
                   :noquery t
                   ;; Install the real sentinel only after PROCESS owns a
                   ;; playback entry.  A short-lived player can otherwise exit
                   ;; between `make-process' and `set-process-plist', leaking
                   ;; both its buffer and the local-access lease.
                   :sentinel #'ignore))
            (set-process-query-on-exit-flag process nil)
            (set-process-plist process (list :qq-media-id media-id))
            (setq entry (gethash media-id qq-media--native-record-playbacks))
            ;; Starting a process may admit process/timer callbacks.  Do not
            ;; resurrect playback if its exact Appkit owner stopped while the
            ;; constructor was returning.
            (if (not
                 (and entry
                      (eq (plist-get entry :status) 'preparing)
                      (let ((owner-handle (plist-get entry :owner-handle)))
                        (or (null owner-handle)
                            (and (appkit-handle-p owner-handle)
                                 (appkit-handle-alive-p owner-handle))))))
                (progn
                  (set-process-filter process nil)
                  (set-process-sentinel process nil)
                  (when (process-live-p process)
                    (delete-process process))
                  (set-process-plist process nil)
                  (when (buffer-live-p buffer)
                    (kill-buffer buffer))
                  (qq-media--close-local-access access-id)
                  nil)
              (setq entry (plist-put entry :operation nil))
              (setq entry (plist-put entry :process process))
              (setq entry (plist-put entry :status 'playing))
              (setq entry (plist-put entry :started-at (float-time)))
              (setq entry (plist-put entry :error nil))
              (puthash media-id entry qq-media--native-record-playbacks)
              (setq qq-media--native-record-current-id media-id)
              (set-process-sentinel
               process #'qq-media--native-record-player-sentinel)
              (qq-media--ensure-native-record-progress-timer)
              ;; Emacs does not promise to replay an exit event which arrived
              ;; while the sentinel was `ignore'.  Finalization is idempotent
              ;; because it checks exact process ownership.
              (unless (process-live-p process)
                (qq-media--native-record-player-sentinel process "finished\n"))
              (qq-media--notify-native-record-state media-id)
              process))
        (error
         (qq-media--dispose-native-record-entry
          media-id 'failed (error-message-string error-data))
         nil)))))

(defun qq-media--native-record-prepared (media-id result)
  "Consume playback preparation RESULT for exact MEDIA-ID."
  (let ((entry (gethash media-id qq-media--native-record-playbacks)))
    (if (not (and entry
                  (equal qq-media--native-record-current-id media-id)
                  (eq (plist-get entry :status) 'preparing)
                  (qq-account-get (plist-get entry :account-id))))
        (qq-media--close-local-access
         (alist-get 'access_id (alist-get 'access result)))
      (qq-media--start-native-record-player media-id result))))

(defun qq-media--native-record-prepare-failed (media-id _body reason)
  "Record playback preparation failure REASON for MEDIA-ID."
  (let ((entry (gethash media-id qq-media--native-record-playbacks)))
    (when (and entry
               (equal qq-media--native-record-current-id media-id)
               (eq (plist-get entry :status) 'preparing))
      (qq-media--dispose-native-record-entry media-id 'failed reason)
      (message "qq: Record playback preparation failed: %s" reason))))

(cl-defun qq-media-play-native-record (segment &key owner)
  "Prepare, play, pause, or resume native record SEGMENT.

OWNER is the Appkit owner captured by the rendering card.  As with Telega
voice notes, clicking a playing record pauses it and clicking again resumes."
  (let* ((media-id (qq-media--native-record-media-id segment))
         (entry (and media-id
                     (gethash media-id qq-media--native-record-playbacks)))
         (process (and entry (plist-get entry :process)))
         (status (and entry (plist-get entry :status))))
    (unless media-id
      (user-error "qq: Native record has no materializable media ID"))
    (pcase status
      ('preparing
       (qq-media--dispose-native-record-entry media-id 'stopped)
       (message "qq: canceled record playback preparation"))
      ('playing
       (if (process-live-p process)
           (condition-case error-data
               (progn
                 (signal-process process 'SIGSTOP)
                 (setq entry
                       (plist-put
                        entry :played-seconds
                        (qq-media--native-record-played-seconds entry)))
                 (setq entry (plist-put entry :started-at nil))
                 (setq entry (plist-put entry :status 'paused))
                 (puthash media-id entry qq-media--native-record-playbacks)
                 (qq-media--cancel-native-record-progress-timer)
                 (qq-media--notify-native-record-state media-id))
             (error
              (qq-media--dispose-native-record-entry
               media-id 'failed (error-message-string error-data))))
         (qq-media--dispose-native-record-entry media-id 'stopped)))
      ('paused
       (if (process-live-p process)
           (condition-case error-data
               (progn
                 (signal-process process 'SIGCONT)
                 (setq entry (plist-put entry :started-at (float-time)))
                 (setq entry (plist-put entry :status 'playing))
                 (puthash media-id entry qq-media--native-record-playbacks)
                 (qq-media--ensure-native-record-progress-timer)
                 (qq-media--notify-native-record-state media-id))
             (error
              (qq-media--dispose-native-record-entry
               media-id 'failed (error-message-string error-data))))
         (qq-media--dispose-native-record-entry media-id 'stopped)))
      (_
       (unless (qq-media-native-record-playback-available-p)
         (user-error
          "qq: Record player unavailable; customize `qq-media-record-player-command'"))
       (when (and qq-media--native-record-current-id
                  (not (equal qq-media--native-record-current-id media-id)))
         (qq-media--dispose-native-record-entry
          qq-media--native-record-current-id 'stopped))
       (let* ((account-id (qq-runtime-current-account-id))
              (owner-handle
               (and account-id owner
                    (appkit-register-handle
                     owner 'function
                     (apply-partially
                      #'qq-media--dispose-native-record-entry
                      media-id 'stopped))))
              (next (list :status 'preparing
                          :account-id account-id
                          :duration-seconds
                          (alist-get 'duration_seconds
                                     (alist-get 'data segment))
                          :played-seconds 0.0
                          :started-at nil
                          :owner-handle owner-handle
                          :operation nil
                          :process nil
                          :access-id nil
                          :error nil)))
         (unless account-id
           (user-error "qq: Select an online account before playing a record"))
         (puthash media-id next qq-media--native-record-playbacks)
         (setq qq-media--native-record-current-id media-id)
         (qq-media--notify-native-record-state media-id)
         (condition-case error-data
             (let ((operation
                    (qq-remote-media-prepare-record-playback
                     media-id
                     (apply-partially #'qq-media--native-record-prepared media-id)
                     (apply-partially
                      #'qq-media--native-record-prepare-failed media-id))))
               ;; Synchronous test transports may already have handed off.
               (when-let* ((current
                            (gethash media-id qq-media--native-record-playbacks)))
                 (when (eq (plist-get current :status) 'preparing)
                   (setq current (plist-put current :operation operation))
                   (puthash media-id current
                            qq-media--native-record-playbacks))))
           (error
            (qq-media--dispose-native-record-entry
             media-id 'failed (error-message-string error-data))
            (signal (car error-data) (cdr error-data)))))))
    (qq-media-native-record-playback-state media-id)))

(defun qq-media--native-remote-media-changed (_reason media-id)
  "Redisplay cards affected by remote MEDIA-ID state changes."
  (if-let* ((media (and media-id (qq-remote-media media-id))))
      (pcase (alist-get 'kind media)
        ("record" (qq-media--notify-native-record-state media-id))
        ("image"
         (qq-media--note-cache-updated
          (qq-media--native-image-key media-id)))
        ("video"
         (qq-media--note-cache-updated
          (qq-media--native-video-key media-id))
         (qq-media--note-cache-updated
          (qq-media--native-video-thumbnail-key media-id)))
        (_ (qq-media--note-cache-updated nil)))
    (qq-media--note-cache-updated nil)))

(add-hook 'qq-remote-media-changed-hook
          #'qq-media--native-remote-media-changed)

(defun qq-media--image-from-file (file height)
  "Create an Emacs image object from FILE at pixel HEIGHT, or nil."
  (when (appkit-media-file-present-p file)
    (condition-case _
        (let ((image (create-image file nil nil
                                   :height (max 1 height) :ascent 'center)))
          (if (fboundp 'appkit-media--mark-inline-animation-image)
              (appkit-media--mark-inline-animation-image image file)
            image))
      (error nil))))

(defun qq-media--preview-image-from-file (file spec)
  "Create a preview image object from FILE using appkit media helpers.

SPEC may be a numeric maximum height for compact decorative images."
  (when (appkit-media-file-present-p file)
    (appkit-media-preview-image-from-file
     file
     qq-media-preview-image-max-width
     (if (numberp spec) spec qq-media-preview-image-height))))

(defun qq-media--image-display-string (image fallback)
  "Return display string for IMAGE, or FALLBACK when IMAGE is nil."
  (appkit-media-image-display-string image fallback))

(defun qq-media-composer-image-preview (file)
  "Return a telega-style one-line composer preview for local image FILE."
  (when (appkit-media-file-present-p file)
    (appkit-media-one-line-preview-image-from-file
     file qq-media-preview-image-max-width)))

(defun qq-media--remote-image-cache-file-base (key)
  "Return disk cache file base for remote image KEY."
  (expand-file-name (md5 key) qq-media-cache-directory))

(defun qq-media--remote-image-cache-existing-file (key)
  "Return existing disk cache file for remote image KEY, or nil."
  (appkit-media-image-cache-existing-file
   (qq-media--remote-image-cache-file-base key)))

(defun qq-media--prefer-remote-image-resource-p (key resource)
  "Return non-nil when RESOURCE at KEY should prefer remote image refresh.

Currently this is enabled for user avatars so stale NapCat local avatar
files do not permanently override fresher public avatar URLs."
  (and (stringp key)
       (string-prefix-p "avatar:" key)
       resource
       (appkit-media-url-present-p (alist-get 'url resource))))

(defun qq-media--resource-image-file (key resource)
  "Return local image file for RESOURCE at KEY, consulting disk cache when needed."
  (let* ((cached-file (and key (qq-media--remote-image-cache-existing-file key)))
         (prefer-remote (qq-media--prefer-remote-image-resource-p key resource))
         (file (alist-get 'file resource)))
    (cond
     ((appkit-media-file-present-p cached-file)
      (setf (alist-get 'file resource nil nil #'eq) cached-file)
      (qq-media--cache-resource key resource)
      cached-file)
     ((and prefer-remote (appkit-media-url-present-p (alist-get 'url resource)))
      nil)
     ((appkit-media-file-present-p file)
      file)
     (t nil))))

(defun qq-media--finish-resource-image-fetch (key &optional file image resource)
  "Finalize image fetch for KEY using FILE, IMAGE, and RESOURCE.

Always clear the fetching flag and notify UI.  Previously we only called
`qq-media--note-cache-updated' when IMAGE was non-nil, so a successful file
download that failed `create-image' (or a failed URL fetch) left forward
viewers stuck on \"[loading preview]\" while RET open still worked via the
non-preview resource path."
  (when (and resource file)
    (setf (alist-get 'file resource nil nil #'eq) file)
    (qq-media--cache-resource key resource))
  (when image
    (qq-media--cache-image key image))
  (remhash key qq-media--fetching-cache)
  (qq-media--note-cache-updated key))

(defun qq-media--start-resource-image-download (key resource spec builder)
  "Download remote image RESOURCE for KEY, then build image with BUILDER."
  (let* ((url (alist-get 'url resource))
         (cache-base (qq-media--remote-image-cache-file-base key))
         (disk-cache-file (qq-media--remote-image-cache-existing-file key))
         (cache-file (qq-media--resource-image-file key resource))
         (cached-image (and cache-file (funcall builder cache-file spec))))
    (cond
     (cached-image
      (qq-media--finish-resource-image-fetch key cache-file cached-image resource))
     ((not (appkit-media-url-present-p url))
      (qq-media--finish-resource-image-fetch key nil nil resource))
     (t
      ;; This branch has explicitly selected the remote URL.  Never pass an
      ;; older local avatar (or a local file whose preview failed) to Appkit,
      ;; whose canonical resource policy correctly prefers local files.
      (when (appkit-media-file-present-p disk-cache-file)
        (ignore-errors (delete-file disk-cache-file)))
      (condition-case err
          (let ((remote-resource (copy-tree resource)))
            (setf (alist-get 'file remote-resource nil nil #'eq) nil)
            (let ((transfer
                   (appkit-media-cache-image-resource-async
                    (qq-media--appkit-resource remote-resource)
                    cache-base
                    (lambda (target-file)
                      (qq-media--finish-resource-image-fetch
                       key target-file
                       (funcall builder target-file spec)
                       resource))
                    (lambda (_reason)
                      (qq-media--finish-resource-image-fetch
                       key nil nil resource)))))
              ;; Setup failures report synchronously and clear KEY.  Preserve
              ;; that terminal state instead of restoring a stale handle.
              (when (gethash key qq-media--fetching-cache)
                (puthash key transfer qq-media--fetching-cache))))
        (error
         (message "qq: failed to start remote image fetch for %s: %s"
                  key
                  (error-message-string err))
         (qq-media--finish-resource-image-fetch key nil nil resource)))))))

(defun qq-media--ensure-resource-image (key fetcher spec &optional image-builder)
  "Return cached image for KEY, triggering FETCHER when needed.

FETCHER must accept SUCCESS and ERROR callbacks.  SUCCESS receives one
resource alist.  SPEC is forwarded to IMAGE-BUILDER, which defaults to
`qq-media--image-from-file'."
  (let ((builder (or image-builder #'qq-media--image-from-file)))
    (or (qq-media--cached-image key)
        (let* ((resource (qq-media--cached-resource key))
               (original-file (and resource (alist-get 'file resource)))
               (file (and resource (qq-media--resource-image-file key resource))))
          (cond
           ((appkit-media-file-present-p file)
            (qq-media--cache-image key (funcall builder file spec)))
           ((gethash key qq-media--fetching-cache)
            nil)
           ((and resource
                 (qq-media--prefer-remote-image-resource-p key resource)
                 (appkit-media-file-present-p original-file))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            (qq-media--cache-image key (funcall builder original-file spec)))
           ((and resource (appkit-media-url-present-p (alist-get 'url resource)))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            nil)
           (t
            (puthash key t qq-media--fetching-cache)
            (funcall
             fetcher
             (lambda (fetched-resource)
               (when (gethash key qq-media--fetching-cache)
                 (let* ((resource* (qq-media--cache-resource key fetched-resource))
                        (original-file* (and resource* (alist-get 'file resource*)))
                        (file* (and resource* (qq-media--resource-image-file key resource*))))
                   (cond
                    ((appkit-media-file-present-p file*)
                     (qq-media--finish-resource-image-fetch
                      key file*
                      (funcall builder file* spec)
                      resource*))
                    ((and (qq-media--prefer-remote-image-resource-p key resource*)
                          (appkit-media-file-present-p original-file*))
                     (let ((image (funcall builder original-file* spec)))
                       (when image
                         (qq-media--cache-image key image)
                         (qq-media--note-cache-updated key)))
                     (qq-media--start-resource-image-download
                      key resource* spec builder))
                    ((appkit-media-url-present-p (alist-get 'url resource*))
                     (qq-media--start-resource-image-download
                      key resource* spec builder))
                    (t
                     (qq-media--finish-resource-image-fetch
                      key nil nil resource*))))))
             (lambda (_response _reason)
               (when (gethash key qq-media--fetching-cache)
                 (qq-media--finish-resource-image-fetch key nil nil nil))))
            nil))))))

(defun qq-media--native-image-animated-p (segment)
  "Return non-nil when native image SEGMENT is known to be animated."
  (let* ((data (alist-get 'data segment))
         (summary (alist-get 'summary data)))
    (or (qq-media--json-truthy-p (alist-get 'animated data))
        (and (stringp summary)
             (string-match-p "动画" summary)))))

(defun qq-media--fetch-native-image-part-resource
    (segment media-id part key callback errback)
  "Materialize image-like PART of native MEDIA-ID from SEGMENT into KEY.

The Gateway grants only a short-lived local access path.  Copy it through
Appkit's atomic image cache before closing the lease; CALLBACK therefore sees
only a client-owned stable file.  ERRBACK receives failures."
  (let ((error-fn (or errback #'qq-api--default-error))
        (animated-p
         (and (eq part 'content)
              (qq-media--native-image-animated-p segment))))
    (if (not media-id)
        (funcall error-fn nil "segment has no native media handle")
      (condition-case error-data
          (let ((operation
                 (qq-remote-media-prepare-part-local-access
                  media-id part
                  (lambda (result)
                    (let* ((access (alist-get 'access result))
                           (access-id (alist-get 'access_id access))
                           (path (alist-get 'path access))
                           (closed nil))
                      (cl-labels
                          ((close-access
                            ()
                            (unless closed
                              (setq closed t)
                              (qq-media--close-local-access access-id)))
                           (finish
                            (file)
                            (close-access)
                            (let ((resource
                                   `((file . ,file)
                                     (name . ,(file-name-nondirectory file))
                                     ,@(when animated-p
                                         '((animated . t))))))
                              (if animated-p
                                  (qq-media--prepare-animated-face-resource
                                   resource callback)
                                (funcall callback resource))))
                           (fail-copy
                            (reason)
                            (close-access)
                            (funcall error-fn nil reason)))
                        (condition-case copy-error
                            (appkit-media-cache-image-resource-async
                             `((file . ,path))
                             (qq-media--remote-image-cache-file-base key)
                             #'finish #'fail-copy)
                          ((error quit)
                           (close-access)
                           (funcall
                            error-fn nil
                            (error-message-string copy-error)))))))
                  error-fn)))
            ;; Preview ownership places a marker in this table before invoking
            ;; the fetcher.  Do not overwrite a synchronous completion.
            (when (and (qq-remote-media-operation-active-p operation)
                       (gethash key qq-media--fetching-cache))
              (puthash key operation qq-media--fetching-cache))
            operation)
        ((error quit)
         (funcall error-fn nil (error-message-string error-data))
         nil)))))

(defun qq-media--fetch-native-image-resource
  (segment key callback errback)
  "Materialize native image SEGMENT into KEY.

Call CALLBACK with the persistent resource; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-image-media-id segment) 'content
   key callback errback))

(defun qq-media--fetch-native-video-thumbnail-resource
  (segment key callback errback)
  "Materialize native video SEGMENT thumbnail into KEY.

Call CALLBACK with the persistent resource; call ERRBACK on failure."
  (qq-media--fetch-native-image-part-resource
   segment (qq-media--native-video-media-id segment) 'thumbnail
   key callback errback))

(defun qq-media--fetch-native-video-resource
  (segment key callback errback)
  "Materialize native video SEGMENT content into KEY.

Call CALLBACK with the persistent resource; call ERRBACK on failure."
  (let ((media-id (qq-media--native-video-media-id segment))
        (error-fn (or errback #'qq-api--default-error))
        (target
         (expand-file-name
          (format "native-video-%s.mp4" (secure-hash 'sha256 key))
          qq-media-cache-directory)))
    (if (not media-id)
        (funcall error-fn nil "video segment has no native media handle")
      (condition-case error-data
          (qq-remote-media-prepare-part-local-access
           media-id 'content
           (lambda (result)
             (let* ((access (alist-get 'access result))
                    (access-id (alist-get 'access_id access))
                    (path (alist-get 'path access))
                    (closed nil))
               (cl-labels
                   ((close-access
                     ()
                     (unless closed
                       (setq closed t)
                       (qq-media--close-local-access access-id)))
                    (finish
                     (file)
                     (close-access)
                     (funcall
                      callback
                      `((file . ,file)
                        (name . ,(file-name-nondirectory file))
                        (mime-type . "video/mp4"))))
                    (fail-copy
                     (reason)
                     (close-access)
                     (funcall error-fn nil reason)))
                 (condition-case copy-error
                     (appkit-media-copy-or-download-resource-async
                      `((file . ,path)) target #'finish #'fail-copy)
                   ((error quit)
                    (close-access)
                    (funcall
                     error-fn nil
                     (error-message-string copy-error)))))))
           error-fn)
        ((error quit)
         (funcall error-fn nil (error-message-string error-data))
         nil)))))

(defun qq-media--resource-fetching-p (key)
  "Return non-nil when KEY is currently being fetched."
  (gethash key qq-media--fetching-cache))

(defun qq-media--resolve-resource (key fetcher callback)
  "Resolve resource by KEY using FETCHER, then run CALLBACK.

FETCHER is called with a one-argument callback that receives the resource
alist returned by NapCat."
  (if-let* ((cached (qq-media--cached-resource key)))
      (funcall callback cached)
    (funcall fetcher
             (lambda (resource)
               (let ((cached-resource (qq-media--cache-resource key resource)))
                 (qq-media--note-cache-updated key)
                 (funcall callback cached-resource))))))

(defun qq-media--segment-file-keys (segment)
  "Return candidate file keys from SEGMENT, in preference order."
  (let* ((data (alist-get 'data segment))
         (keys (list (alist-get 'file_id data)
                     (alist-get 'file data)
                     (alist-get 'path data))))
    (delete-dups
     (seq-filter
      (lambda (key)
        (and key
             (or (not (stringp key))
                 (not (string-empty-p (string-trim key))))))
      keys))))

(defun qq-media--segment-file-key (segment)
  "Return best file key from SEGMENT."
  (car (qq-media--segment-file-keys segment)))

(defun qq-media--video-resolver-cache-identity (segment)
  "Return a stable cache identity derived from SEGMENT's complete resolver.

The native resolver, rather than a display filename or opaque `data.file',
is the authoritative identity of a resolvable video.  Hash the complete
  validated object so cache, download, and preview state cannot collide merely
  because two messages use the same filename."
  (when-let* ((resolver (qq-media--video-resolver segment))
              (peer (alist-get 'peer resolver)))
    ;; Build an explicit field-order-independent tuple.  JSON object member
    ;; order is not semantic and must not split one native resource identity.
    (let ((identity
           (pcase (alist-get 'kind resolver)
             ("message"
              (list "message"
                    (alist-get 'chat_type peer)
                    (alist-get 'peer_uid peer)
                    (alist-get 'guild_id peer)
                    (alist-get 'message_id resolver)
                    (alist-get 'element_id resolver)))
             ("snapshot"
              (list "snapshot"
                    (alist-get 'chat_type peer)
                    (alist-get 'peer_uid peer)
                    (alist-get 'guild_id peer)
                    (alist-get 'file_uuid resolver))))))
      (format "resolver:%s"
              (secure-hash 'sha256 (prin1-to-string identity))))))

(defun qq-media--absolute-local-file-present-p (file)
  "Return non-nil when FILE is an existing absolute local path.

Protocol `file' values are often opaque names or handles.  Even when such a
relative string happens to exist below `default-directory', it is not proof
that the server designated that local file."
  (and (stringp file)
       (file-name-absolute-p file)
       (appkit-media-file-present-p file)))

(defun qq-media--video-local-file-present-p (file &optional resource)
  "Return non-nil when FILE is a plausible playable video local file.

RESOURCE may provide MIME metadata.  Image artifacts are video posters or
bounded GIF previews and must never be promoted to the playable video source."
  (and (qq-media--absolute-local-file-present-p file)
       (not (appkit-media-image-file-name-p file))
       (let ((mime-type (and resource (alist-get 'mime-type resource))))
         (not (and (stringp mime-type)
                   (string-prefix-p "image/" (downcase mime-type)))))))

(defun qq-media--segment-existing-path (segment)
  "Return an existing local filesystem path from SEGMENT, or nil.

Outbound attach/pending segments carry absolute paths in `file' or `path'.
Those must never be sent to NapCat `get_image'/`get_file' (Telega-style
local-first rendering)."
  (let* ((data (alist-get 'data segment))
         (candidates (list (alist-get 'path data)
                           (alist-get 'file data))))
    (catch 'found
      (dolist (candidate candidates)
        (when (qq-media--absolute-local-file-present-p candidate)
          (throw 'found candidate)))
      nil)))

(defun qq-media--segment-remote-file-keys (segment)
  "Return SEGMENT file keys that are not existing local paths.

Only these keys are safe to pass to NapCat `get_image'/`get_file'."
  (let (remote)
    (dolist (key (qq-media--segment-file-keys segment))
      (unless (qq-media--absolute-local-file-present-p key)
        (push key remote)))
    (nreverse remote)))

(defun qq-media--resource-from-local+url (local url)
  "Build a resource alist from LOCAL path and optional URL."
  (append
   (when (appkit-media-file-present-p local)
     `((file . ,local)))
   (when (appkit-media-url-present-p url)
     `((url . ,url)))))

(defun qq-media--resolve-fileish-segment (segment action callback errback &optional final-error)
  "Resolve file-like SEGMENT with Telega-style priority.

Order:
1. Existing local path on the segment (outbound attach / pending)
2. NapCat ACTION (`get_image'/`get_file') with remote file keys only
3. Direct `url' from the segment
4. ERRBACK

NapCat/NT still owns remote materialization and QQ disk cache; the client
never treats a local absolute path as a NapCat file id."
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (url (plist-get capabilities :remote-url))
         (local (qq-media--segment-existing-path segment))
         (remote-keys (qq-media--segment-remote-file-keys segment))
         (error-fn (or errback #'qq-api--default-error))
         (fail-msg (or final-error
                       "media segment has neither local file, file id, nor URL")))
    (cond
     (local
      (funcall callback (qq-media--resource-from-local+url local url)))
     (remote-keys
      (qq-media--call-fileish-action
       action remote-keys
       callback
       (lambda (response reason)
         (if (appkit-media-url-present-p url)
             (funcall callback `((url . ,url)))
           (funcall error-fn response (or reason fail-msg))))
       fail-msg))
     ((appkit-media-url-present-p url)
      (funcall callback `((url . ,url))))
     (t
      (funcall error-fn nil fail-msg)))))

(defun qq-media-imageish-file-segment-p (segment)
  "Return non-nil when SEGMENT is a file that should preview like an image."
  (and (equal (alist-get 'type segment) "file")
       (let* ((data (alist-get 'data segment))
              (name (or (alist-get 'name data)
                        (alist-get 'file data))))
         (appkit-media-image-file-name-p name))))

(defun qq-media-videoish-segment-p (segment)
  "Return non-nil when SEGMENT carries a video preview source."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (name (or (alist-get 'name data)
                   (alist-get 'file data)
                   (alist-get 'url data))))
    (or (equal type "video")
        (and (equal type "file")
             (appkit-media-video-file-name-p name)))))

(defun qq-media-segment-preview-capable-p (segment)
  "Return non-nil when SEGMENT supports inline preview rendering."
  (or (member (alist-get 'type segment) '("image" "mface"))
      (qq-media-imageish-file-segment-p segment)
      (qq-media-videoish-segment-p segment)))

(defun qq-media--call-fileish-action (action file-keys callback errback &optional final-error)
  "Call ACTION with FILE-KEYS until one succeeds.

On success pass decoded data to CALLBACK.  When all candidates fail, call
ERRBACK with FINAL-ERROR or the last backend reason."
  (letrec ((try-next
            (lambda (keys last-reason last-response)
              (if (null keys)
                  (funcall errback last-response (or final-error last-reason "file not found"))
                (qq-api-call
                 action
                 `((file . ,(car keys)))
                 (lambda (response)
                   (funcall callback (qq-api--response-data response)))
                 (lambda (response reason)
                   (funcall try-next (cdr keys) reason response)))))))
    (funcall try-next file-keys nil nil)))

(defun qq-media--segment-url (segment)
  "Return best direct URL from SEGMENT, or nil."
  (alist-get 'url (alist-get 'data segment)))

(defun qq-media--video-remote-status (segment)
  "Return normalized remote status symbol for video SEGMENT.

The wire values are `available', `resolvable', `expired', `unavailable', and
`unresolved'.  A missing or otherwise unknown value is an invalid protocol
state and must not enable a remote operation."
  (let ((value (alist-get 'remote_status (alist-get 'data segment))))
    (cond
     ((equal value "available") 'available)
     ((equal value "resolvable") 'resolvable)
     ((equal value "expired") 'expired)
     ((equal value "unavailable") 'unavailable)
     ((equal value "unresolved") 'unresolved)
     (t 'invalid))))

(defun qq-media--video-resolver (segment)
  "Return validated exact resolver carried by video SEGMENT, or nil."
  (when (and (equal (alist-get 'type segment) "video")
             (eq (qq-media--video-remote-status segment) 'resolvable))
    (condition-case nil
        (qq-api-validate-video-resolver
         (alist-get 'resolver (alist-get 'data segment))
         "video segment resolver" t)
      (error nil))))

(defun qq-media--transfer-status-text (state)
  "Return compact user-visible transfer status for download STATE."
  (let ((status (plist-get state :status))
        (path (plist-get state :path))
        (error-text (plist-get state :error)))
    (pcase status
      ('downloading "downloading…")
      ('downloaded
       (if (and (stringp path) (not (string-empty-p path)))
           (format "local: %s" (file-name-nondirectory path))
         "downloaded"))
      ('error
       (if (and (stringp error-text) (not (string-empty-p error-text)))
           (format "download failed: %s"
                   (truncate-string-to-width error-text 68 nil nil t))
         "download failed"))
      (_ nil))))

(defun qq-media--legacy-segment-capabilities (segment)
  "Return the centralized action/status model for media SEGMENT.

The result is a plist with `:open', `:download', `:save', `:copy-url',
`:status', `:local-file', `:remote-status', `:resolve-remote', `:remote-url',
and `:remote-error'.  Video remote state comes exclusively from
`remote_status'; a real local file remains usable independently of that
state.  `available' uses its non-empty URL, while `resolvable' invokes only
its explicit fork-native resolver on user demand.  Terminal and invalid
states never probe a second interface such as get_file."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (supported (member type
                            '("image" "file" "record" "video" "face" "mface")))
         (video-p (equal type "video"))
         (remote-status (if video-p
                            (qq-media--video-remote-status segment)
                          'not-applicable))
         (video-resolver (and video-p (qq-media--video-resolver segment)))
         (local-file (qq-media-segment-local-file segment))
         (url (qq-media--segment-url segment))
         (url-p (appkit-media-url-present-p url))
         (usable-url-p (and url-p
                            (or (not video-p)
                                (eq remote-status 'available))))
         (remote-keys (qq-media--segment-remote-file-keys segment))
         (face-id (and (equal type "face") (alist-get 'id data)))
         (remote-source-p (if video-p
                              (or usable-url-p video-resolver)
                            (or remote-keys usable-url-p face-id)))
         (resolve-remote
          (and supported remote-source-p
               (or (not video-p)
                   (memq remote-status '(available resolvable)))))
         (remote-url
          (and usable-url-p
               url))
         (download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (open (and supported (or local-file resolve-remote)))
         (download (and supported resolve-remote (not local-file)
                        (not (memq download-status
                                   '(downloading downloaded)))))
         (save (and supported (or local-file resolve-remote)))
         (copy-url (and remote-url t))
         (remote-error
          (and video-p
               (pcase remote-status
                 ('expired "video resource has expired")
                 ('unavailable "video resource is unavailable")
                 ('invalid "video resource has invalid remote_status")
                 ('unresolved "video resource is unresolved")
                 ('resolvable
                  (unless video-resolver
                    "video resource has an invalid resolver"))
                 ('available
                  (unless usable-url-p
                    "available video resource has no URL")))))
         (status
          (if video-p
              (pcase remote-status
                ('expired "Expired")
                ('unavailable "Unavailable")
                ('unresolved "Unresolved")
                ('resolvable
                 (qq-media--transfer-status-text download-state))
                ('invalid "Invalid remote status")
                (_ (qq-media--transfer-status-text download-state)))
            (qq-media--transfer-status-text download-state))))
    (list :open open
          :download download
          :save save
          :copy-url copy-url
          :status status
          :local-file local-file
          :remote-status remote-status
          :resolve-remote resolve-remote
          :remote-url remote-url
          :remote-error remote-error
          :download-state download-state)))

(defconst qq-media--native-record-required-methods
  '("media.materialize"
    "media.cancel"
    "resource.derive_playable_record"
    "resource.open_local"
    "resource.close_local")
  "Native protocol methods required for record playback preparation.")

(defun qq-media--native-record-methods-ready-p ()
  "Return non-nil when native record playback can start for this account."
  (and (qq-runtime-current-account-id)
       (qq-server-ready-p)
       (cl-every #'qq-rpc-method-available-p
                 qq-media--native-record-required-methods)))

(defun qq-media--native-record-capabilities (media-id)
  "Return the action/status model for native record MEDIA-ID."
  (let* ((playback (gethash media-id qq-media--native-record-playbacks))
         (playback-status (plist-get playback :status))
         (playback-error (plist-get playback :error))
         (remote (qq-remote-media media-id))
         (content (qq-remote-media-part remote 'content))
         (phase (alist-get 'phase content))
         (problem (alist-get 'error content))
         (problem-message (alist-get 'message problem))
         (active (memq playback-status '(preparing playing paused)))
         (player-ready (qq-media-native-record-playback-available-p))
         (methods-ready (qq-media--native-record-methods-ready-p))
         (open (or active (and player-ready methods-ready)))
         (remote-error
          (cond
           ((and (eq playback-status 'failed) playback-error)
            playback-error)
           ((equal phase "failed")
            (or problem-message "remote record materialization failed"))
           ((not player-ready)
            "configured record player is unavailable")
           ((not methods-ready)
            "native record playback methods are unavailable")))
         (status
          (pcase playback-status
            ('preparing "Preparing…")
            ('playing "Playing")
            ('paused "Paused")
            ('finished "Finished")
            ('failed
             (format "Playback failed: %s"
                     (truncate-string-to-width
                      (or playback-error "unknown player error") 68 nil nil t)))
            (_
             (cond
              ((equal phase "materializing")
               (let ((done (alist-get 'bytes_done content))
                     (total (or (alist-get 'bytes_total content)
                                (alist-get 'expected_size content))))
                 (if total
                     (format "Preparing %s/%s bytes" done total)
                   (format "Preparing %s bytes" done))))
              ((equal phase "materialized") "Ready")
              ((equal phase "failed")
               (format "Retry: %s"
                       (truncate-string-to-width
                        (or problem-message "materialization failed")
                        68 nil nil t)))
              ((not player-ready) "Player unavailable")
              ((not (qq-runtime-current-account-id)) "Select an account")
              ((not methods-ready) "Playback unavailable")
              (t "Remote voice"))))))
    (list :open (and open t)
          :download nil
          :save nil
          :copy-url nil
          :status status
          :local-file nil
          :remote-status (or phase 'unprojected)
          :resolve-remote nil
          :remote-url nil
          :remote-error remote-error
          :download-state nil)))

(defconst qq-media--native-image-required-methods
  '("media.materialize"
    "media.cancel"
    "resource.status"
    "resource.open_local"
    "resource.close_local")
  "Native protocol methods required for image materialization.")

(defun qq-media--native-image-methods-ready-p ()
  "Return non-nil when native image materialization can start."
  (and (qq-runtime-current-account-id)
       (qq-server-ready-p)
       (cl-every #'qq-rpc-method-available-p
                 qq-media--native-image-required-methods)))

(defun qq-media--native-image-capabilities (segment media-id)
  "Return the action/status model for native image SEGMENT and MEDIA-ID."
  (let* ((local-file (qq-media-segment-local-file segment))
         (remote (qq-remote-media media-id))
         (content (qq-remote-media-part remote 'content))
         (phase (alist-get 'phase content))
         (problem (alist-get 'error content))
         (problem-message (alist-get 'message problem))
         (methods-ready (qq-media--native-image-methods-ready-p))
         (download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (resolve-remote (and methods-ready (not local-file)))
         (remote-error
          (cond
           ((not (qq-runtime-current-account-id))
            "select an account before loading this image")
           ((not methods-ready)
            "native image materialization methods are unavailable")
           ((equal phase "failed")
            (or problem-message "remote image materialization failed"))))
         (status
          (or (qq-media--transfer-status-text download-state)
              (pcase phase
                ("materializing"
                 (let ((done (alist-get 'bytes_done content))
                       (total (or (alist-get 'bytes_total content)
                                  (alist-get 'expected_size content))))
                   (if total
                       (format "Loading %s/%s bytes" done total)
                     (format "Loading %s bytes" done))))
                ("materialized" "Ready")
                ("failed"
                 (format "Retry: %s"
                         (truncate-string-to-width
                          (or problem-message "materialization failed")
                          68 nil nil t)))
                (_ (unless local-file "Remote image"))))))
    (list :open (and (or local-file methods-ready) t)
          :download (and resolve-remote
                         (not (memq download-status
                                    '(downloading downloaded))))
          :save (and (or local-file methods-ready) t)
          :copy-url nil
          :status status
          :local-file local-file
          :remote-status (or phase 'unprojected)
          :resolve-remote resolve-remote
          :remote-url nil
          :remote-error remote-error
          :download-state download-state)))

(defun qq-media--native-video-capabilities (segment media-id)
  "Return the action/status model for native video SEGMENT and MEDIA-ID."
  (let* ((local-file (qq-media-segment-local-file segment))
         (remote (qq-remote-media media-id))
         (content (qq-remote-media-part remote 'content))
         (phase (alist-get 'phase content))
         (problem (alist-get 'error content))
         (problem-message (alist-get 'message problem))
         (methods-ready (qq-media--native-image-methods-ready-p))
         (download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (resolve-remote (and methods-ready (not local-file)))
         (remote-error
          (cond
           ((not (qq-runtime-current-account-id))
            "select an account before loading this video")
           ((not methods-ready)
            "native video materialization methods are unavailable")
           ((equal phase "failed")
            (or problem-message "remote video materialization failed"))))
         (status
          (or (qq-media--transfer-status-text download-state)
              (pcase phase
                ("materializing"
                 (let ((done (alist-get 'bytes_done content))
                       (total (or (alist-get 'bytes_total content)
                                  (alist-get 'expected_size content))))
                   (if total
                       (format "Loading %s/%s bytes" done total)
                     (format "Loading %s bytes" done))))
                ("materialized" "Ready")
                ("failed"
                 (format "Retry: %s"
                         (truncate-string-to-width
                          (or problem-message "materialization failed")
                          68 nil nil t)))
                (_ (unless local-file "Remote video"))))))
    (list :open (and (or local-file methods-ready) t)
          :download (and resolve-remote
                         (not (memq download-status
                                    '(downloading downloaded))))
          :save (and (or local-file methods-ready) t)
          :copy-url nil
          :status status
          :local-file local-file
          :remote-status (or phase 'unprojected)
          :resolve-remote resolve-remote
          :remote-url nil
          :remote-error remote-error
          :download-state download-state)))

(defun qq-media-segment-capabilities (segment)
  "Return the centralized action/status model for media SEGMENT.

Native records and images use only their opaque remote-media handles.  Other
segments remain on the dormant v1 resource model while that client is
retired."
  (cond
   ((qq-media--native-record-media-id segment)
    (qq-media--native-record-capabilities
     (qq-media--native-record-media-id segment)))
   ((qq-media--native-image-media-id segment)
    (qq-media--native-image-capabilities
     segment (qq-media--native-image-media-id segment)))
   ((qq-media--native-video-media-id segment)
    (qq-media--native-video-capabilities
     segment (qq-media--native-video-media-id segment)))
   (t
    (qq-media--legacy-segment-capabilities segment))))

(defun qq-media--segment-resource-key (segment)
  "Return logical resource cache key for SEGMENT, or nil."
  (let* ((type (alist-get 'type segment))
         (resolver-identity (and (equal type "video")
                                 (qq-media--video-resolver-cache-identity
                                  segment)))
         (file-key (qq-media--segment-file-key segment))
         (url (qq-media--segment-url segment))
         (data (alist-get 'data segment))
         (record-media-id (qq-media--native-record-media-id segment))
         (image-media-id (qq-media--native-image-media-id segment))
         (video-media-id (qq-media--native-video-media-id segment))
         (emoji-id (alist-get 'id data)))
    (pcase type
      ("image" (or (and image-media-id
                         (qq-media--native-image-key image-media-id))
                    (and file-key (format "image:%s" file-key))
                   (and (appkit-media-url-present-p url) (format "image-url:%s" url))))
      ("video"
       (or (and video-media-id
                (qq-media--native-video-key video-media-id))
           (and resolver-identity
                (format "video:%s" resolver-identity))
           (and file-key (format "video:%s" file-key))
           (and (appkit-media-url-present-p url)
                (format "video-url:%s" url))))
      ("file"
       (or (and file-key (format "%s:%s" type file-key))
           (and (appkit-media-url-present-p url) (format "%s-url:%s" type url))))
      ("record" (or (and record-media-id
                          (qq-media--native-record-key record-media-id))
                    (and file-key (format "record:%s" file-key))))
      ("face" (and emoji-id (format "face:%s" emoji-id)))
      ("mface" (or (and file-key (format "mface:%s" file-key))
                   (and (appkit-media-url-present-p url) (format "mface-url:%s" url))))
      (_ nil))))

(defun qq-media--fetch-segment-resource (segment callback &optional errback)
  "Fetch media resource for SEGMENT and pass it to CALLBACK.

Uses local path → NapCat get_* → URL (see `qq-media--resolve-fileish-segment')."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (emoji-id (alist-get 'id data))
         (error-fn (or errback #'qq-api--default-error)))
    (pcase type
      ("image"
       (if-let* ((media-id (qq-media--native-image-media-id segment)))
           (qq-media--fetch-native-image-resource
            segment (qq-media--native-image-key media-id)
            callback error-fn)
         (qq-media--resolve-fileish-segment
          segment "get_image" callback error-fn
          "image segment has neither local file, file id, nor URL")))
      ("video"
       (if-let* ((media-id (qq-media--native-video-media-id segment)))
           (qq-media--fetch-native-video-resource
            segment (qq-media--native-video-key media-id)
            callback error-fn)
         (qq-media--resolve-fileish-segment
          segment "get_file" callback error-fn
          "video segment has neither local file, file id, nor URL")))
      ("file"
       (qq-media--resolve-fileish-segment
        segment
        (if (qq-media-imageish-file-segment-p segment)
            "get_image"
          "get_file")
        callback error-fn
        (format "%s segment has neither local file, file id, nor URL" type)))
      ("record"
       (let ((remote-keys (qq-media--segment-remote-file-keys segment))
             (local (qq-media--segment-existing-path segment)))
         (cond
          (local
           (funcall callback `((file . ,local))))
          (remote-keys
           (letrec ((try-next
                     (lambda (keys last-reason last-response)
                       (if (null keys)
                           (funcall error-fn last-response
                                    (or last-reason "record segment has no usable file id"))
                         (qq-api-call
                          "get_record"
                          `((file . ,(car keys))
                            (out_format . "mp3"))
                          (lambda (response)
                            (funcall callback (qq-api--response-data response)))
                          (lambda (response reason)
                            (funcall try-next (cdr keys) reason response)))))))
             (funcall try-next remote-keys nil nil)))
          (t
           (funcall error-fn nil "record segment has no file id")))))
      ("face"
       (cond
        ((not emoji-id)
         (funcall error-fn nil "face segment has no id"))
        ((qq-media--face-resource-from-local emoji-id)
         (funcall callback (qq-media--face-resource-from-local emoji-id)))
        (t
         (funcall error-fn nil
                  "base face has no local resource in the native backend"))))
      ("mface"
       (qq-media--resolve-fileish-segment
        segment "get_image" callback error-fn
        "mface segment has neither local file, file id, nor URL"))
      (_
       (funcall error-fn nil (format "unsupported segment type for resource fetch: %s" type))))))

(defun qq-media-resolve-segment-resource (segment callback &optional errback)
  "Resolve media resource for SEGMENT and pass it to CALLBACK."
  (let ((cache-key (qq-media--segment-resource-key segment))
        (capabilities (qq-media-segment-capabilities segment))
        (video-resolver (qq-media--video-resolver segment)))
    (let ((url (plist-get capabilities :remote-url))
          (local (plist-get capabilities :local-file))
          (remote-error (plist-get capabilities :remote-error)))
      (cond
       (local
        (let ((resource (qq-media--resource-from-local+url local url)))
          (when cache-key
            (qq-media--cache-resource cache-key resource)
            (qq-media--note-cache-updated cache-key))
          (funcall callback resource)))
       ((not (plist-get capabilities :resolve-remote))
        (if errback
            (funcall errback nil (or remote-error "media resource is unavailable"))
          (user-error "qq: %s" (or remote-error "media resource is unavailable"))))
       ;; The video wire model has already resolved the one official URL.  Never
       ;; send its file token through the generic get_file resolver.
       ((and (equal (alist-get 'type segment) "video") url)
        (let ((resource `((url . ,url))))
          (when cache-key
            (qq-media--cache-resource cache-key resource)
            (qq-media--note-cache-updated cache-key))
          (funcall callback resource)))
       ;; A live-message or forward-snapshot resolver is a complete native
       ;; capability, not a fallback.  Resolve it afresh for each operation so
       ;; an old signed URL is never treated as permanently valid.
       ((and (equal (alist-get 'type segment) "video") video-resolver)
        (qq-api-resolve-video
         video-resolver
         (lambda (remote)
           (pcase (alist-get 'state remote)
             ("available"
              (funcall callback `((url . ,(alist-get 'url remote)))))
             ("expired"
              (if errback
                  (funcall errback nil "video resource has expired")
                (user-error "qq: video resource has expired")))
             ("unavailable"
              (if errback
                  (funcall errback nil "video resource is unavailable")
                (user-error "qq: video resource is unavailable")))
             ("unresolved"
              (if errback
                  (funcall errback nil "video resource is unresolved")
                (user-error "qq: video resource is unresolved")))))
         errback))
       ((and cache-key (qq-media--cached-resource cache-key))
        (funcall callback (qq-media--cached-resource cache-key)))
       (cache-key
        (qq-media--fetch-segment-resource
         segment
         (lambda (resource)
           (let ((cached-resource (qq-media--cache-resource cache-key resource)))
             (qq-media--note-cache-updated cache-key)
             (funcall callback cached-resource)))
         errback))
       ((appkit-media-url-present-p url)
        (funcall callback `((url . ,url))))
       (errback
        (funcall errback nil "resource has neither local file, cache key, nor URL"))
       (t
        (user-error "qq: segment has neither local file nor URL"))))))

(cl-defun qq-media-segment-open (segment &key owner)
  "Open OneBot message SEGMENT using QQ-aware resource resolution.

OWNER is the exact Appkit app generation that owns any external media player."
  (if (qq-media--native-record-media-id segment)
      (qq-media-play-native-record segment :owner owner)
    (let ((kind (qq-media-segment-kind segment))
          (cache-key (qq-media--segment-resource-key segment)))
      (if (eq kind 'video)
          (qq-media-segment-play segment :owner owner)
        (if-let* ((file (qq-media-segment-local-file segment)))
            (qq-media-open-resource
             `((file . ,file)) kind cache-key :owner owner)
          (qq-media-resolve-segment-resource
           segment
           (lambda (resource)
             (qq-media-open-resource
              resource kind cache-key :owner owner))))))))

(defun qq-media-segment-openable-p (segment)
  "Return non-nil when SEGMENT can be opened via `qq-media'."
  (plist-get (qq-media-segment-capabilities segment) :open))

(defun qq-media-segment-playable-p (segment)
  "Return non-nil when SEGMENT supports `qq-media-segment-play'."
  (and (eq (qq-media-segment-kind segment) 'video)
       (plist-get (qq-media-segment-capabilities segment) :open)))

(defun qq-media-segment-kind (segment)
  "Return semantic open kind for OneBot SEGMENT."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (name (or (alist-get 'name data)
                   (alist-get 'file data)
                   (appkit-media-url-filename (alist-get 'url data)))))
    (cond
     ((equal type "video") 'video)
     ((member type '("image" "face" "mface")) 'image)
     ((and (equal type "file") (appkit-media-image-file-name-p name)) 'image)
     ((and (equal type "file") (appkit-media-video-file-name-p name)) 'video)
     (t 'file))))

(cl-defun qq-media-open-resource
    (resource &optional kind cache-key &key owner)
  "Open QQ RESOURCE through the shared browser-free media backend.

CACHE-KEY also records the resolved local resource in QQ's logical cache.
OWNER is forwarded exactly to lifecycle-own an external video player."
  (appkit-media-open-resource
   (qq-media--appkit-resource resource)
   :kind kind
   :cache-key cache-key
   :cache-directory qq-media-cache-directory
   :cache-update-function
   (and cache-key
        (lambda (updated-resource)
          (qq-media--cache-resource cache-key updated-resource)))
   :client-label "qq"
   :owner owner))

(defun qq-media-segment-default-save-name (segment)
  "Return default filename for saving SEGMENT locally."
  (let* ((data (alist-get 'data segment))
         (type (or (alist-get 'type segment) "media"))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (seed (or (qq-media--segment-file-key segment)
                   (alist-get 'media_id data)
                   (alist-get 'id data)
                   (alist-get 'emoji_id data)
                   (substring (md5 (prin1-to-string segment)) 0 8)))
         (name (or (alist-get 'name data)
                   (alist-get 'file data)
                   (and cached-resource
                        (let ((file (alist-get 'file cached-resource)))
                          (and (stringp file) (file-name-nondirectory file))))
                   (appkit-media-url-filename (qq-media--segment-url segment))
                   (format "%s-%s.bin" type seed))))
    (appkit-media-sanitize-filename name)))

(defun qq-media-segment-download-key (segment)
  "Return stable download-state key for SEGMENT."
  (or (qq-media--segment-resource-key segment)
      (format "download:%s" (md5 (prin1-to-string segment)))))

(defun qq-media-segment-download-path (segment)
  "Return default local download path for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (safe-name (qq-media-segment-default-save-name segment)))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     qq-media-download-directory)))

(defun qq-media-segment-download-state (segment)
  "Return normalized download state plist for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (entry (copy-tree (or (gethash key qq-media--download-state-table) '())))
         (path (or (plist-get entry :path)
                   (qq-media-segment-download-path segment)))
         (status (plist-get entry :status)))
    (setq entry (plist-put entry :path path))
    (when (and (eq status 'downloaded)
               (not (appkit-media-file-present-p path)))
      (setq entry (plist-put entry :status 'not-downloaded))
      (setq entry (plist-put entry :error nil))
      (setq status 'not-downloaded))
    (unless (plist-get entry :status)
      (setq entry (plist-put entry :status (if (appkit-media-file-present-p path)
                                               'downloaded
                                             'not-downloaded))))
    (puthash key entry qq-media--download-state-table)
    entry))

(defun qq-media--put-segment-download-state (segment entry)
  "Store download-state ENTRY for SEGMENT and notify UI."
  (let ((key (qq-media-segment-download-key segment)))
    (puthash key entry qq-media--download-state-table)
    (qq-media--note-cache-updated key)
    ;; A previous preview attempt may have had only a poster or no source at
    ;; all.  Once the actual media exists, allow preview extraction to retry.
    (when (and (eq (plist-get entry :status) 'downloaded)
               (qq-media--absolute-local-file-present-p
                (plist-get entry :path)))
      (when-let* ((preview-key (qq-media-segment-preview-key segment)))
        (when (gethash preview-key qq-media--preview-missing-cache)
          (remhash preview-key qq-media--preview-missing-cache)
          (qq-media--note-cache-updated preview-key)))))
  entry)

(defun qq-media-segment-local-file (segment)
  "Return best local file path for SEGMENT, or nil."
  (let* ((video-p (qq-media-videoish-segment-p segment))
         (segment-file (qq-media--segment-existing-path segment))
         (download-state (qq-media-segment-download-state segment))
         (download-path (plist-get download-state :path))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (cached-file (and cached-resource (alist-get 'file cached-resource)))
         (preview-key (qq-media-segment-preview-key segment))
         ;; An image preview is itself a usable local image.  A video preview
         ;; is only a poster or a bounded GIF and must never become the source
         ;; handed to the video player.
         (preview-resource (and (not video-p)
                                preview-key
                                (qq-media--cached-resource preview-key)))
         (preview-file (and (not video-p)
                            (or (and preview-resource
                                     (alist-get 'file preview-resource))
                                (and preview-key
                                     (qq-media--remote-image-cache-existing-file
                                      preview-key))))))
    (cond
     ((if video-p
          (qq-media--video-local-file-present-p segment-file)
        (qq-media--absolute-local-file-present-p segment-file))
      segment-file)
     ((if video-p
          (qq-media--video-local-file-present-p download-path)
        (qq-media--absolute-local-file-present-p download-path))
      download-path)
     ((if video-p
          (qq-media--video-local-file-present-p cached-file cached-resource)
        (qq-media--absolute-local-file-present-p cached-file))
      cached-file)
     ((qq-media--absolute-local-file-present-p preview-file) preview-file)
     (t nil))))

(cl-defun qq-media-segment-start-download
    (segment &optional open-after &key owner)
  "Download SEGMENT into `qq-media-download-directory'.

When OPEN-AFTER is non-nil, OWNER lifecycle-owns a video player opened after
the asynchronous download."
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (entry (plist-get capabilities :download-state))
         (path (plist-get entry :path))
         (status (plist-get entry :status)))
    (cond
     ((eq status 'downloading)
      (user-error "qq: media download already in progress"))
     ((and (eq status 'downloaded)
           (appkit-media-file-present-p path))
      (when open-after
        (qq-media-segment-open-local segment :owner owner))
      path)
     ((not (plist-get capabilities :download))
      (user-error "qq: %s"
                  (or (plist-get capabilities :remote-error)
                      "media resource cannot be downloaded")))
     (t
      (let ((token (make-symbol "qq-media-download")))
        (cl-labels
            ((owned-entry ()
               (let ((current (qq-media-segment-download-state segment)))
                 (and (eq token (plist-get current :token)) current)))
             (finish (status reason)
               (when-let* ((current (owned-entry)))
                 (setq current (plist-put current :status status))
                 (setq current (plist-put current :error reason))
                 (setq current (plist-put current :path path))
                 (setq current (plist-put current :transfer nil))
                 (setq current (plist-put current :token nil))
                 (qq-media--put-segment-download-state segment current)
                 t)))
          (setq entry (plist-put entry :status 'downloading))
          (setq entry (plist-put entry :error nil))
          (setq entry (plist-put entry :path path))
          (setq entry (plist-put entry :transfer nil))
          (setq entry (plist-put entry :token token))
          (qq-media--put-segment-download-state segment entry)
          (message "qq: downloading media -> %s" path)
          (qq-media-resolve-segment-resource
           segment
           (lambda (resource)
             (when (owned-entry)
               (condition-case err
                   (let ((transfer
                          (appkit-media-copy-or-download-resource-async
                           (qq-media--appkit-resource resource) path
                           (lambda (_file)
                             (when (finish 'downloaded nil)
                               (message "qq: downloaded media -> %s" path)
                               (when open-after
                                 (qq-media-segment-open-local
                                  segment :owner owner))))
                           (lambda (reason)
                             (when (finish 'error reason)
                               (message "qq: media download failed: %s"
                                        reason))))))
                     ;; Local copies and setup errors finish synchronously.
                     (when-let* ((current (owned-entry)))
                       (setq current (plist-put current :transfer transfer))
                       (qq-media--put-segment-download-state segment current)))
                 ((error quit)
                  (let ((reason (error-message-string err)))
                    (when (finish 'error reason)
                      (message "qq: media download setup failed: %s"
                               reason)))))))
           (lambda (_response reason)
             (when (finish 'error reason)
               (message "qq: media download failed: %s" reason))))
          nil))))))

(cl-defun qq-media-segment-open-local (segment &key owner)
  "Open the best local file available for SEGMENT.

OWNER lifecycle-owns an external player when SEGMENT is a video."
  (if-let* ((file (qq-media-segment-local-file segment)))
      (qq-media-open-resource
       `((file . ,file))
       (qq-media-segment-kind segment)
       (qq-media--segment-resource-key segment)
       :owner owner)
    (user-error "qq: media segment has no local file yet")))

(defun qq-media-segment-save-as (segment &optional target-path)
  "Save SEGMENT to TARGET-PATH, prompting when nil."
  (interactive)
  (let* ((capabilities (qq-media-segment-capabilities segment))
         (_ (unless (plist-get capabilities :save)
              (user-error "qq: %s"
                          (or (plist-get capabilities :remote-error)
                              "media resource cannot be saved"))))
         (default-name (qq-media-segment-default-save-name segment))
         (target (or target-path
                     (read-file-name "Save media as: "
                                     nil
                                     default-name
                                     nil
                                     default-name))))
    (cl-labels
        ((save-resource (resource)
           (appkit-media-copy-or-download-resource-async
            (qq-media--appkit-resource resource) target
            (lambda (_file) (message "qq: saved media -> %s" target))
            (lambda (reason)
              (message "qq: failed to save media: %s" reason)))))
      (condition-case err
          (if-let* ((local-file (qq-media-segment-local-file segment)))
              (save-resource
               `((file . ,local-file)
                 (name . ,(qq-media-segment-default-save-name segment))))
            (qq-media-resolve-segment-resource
             segment #'save-resource
             (lambda (_response reason)
               (message "qq: failed to save media: %s" reason))))
        (error
         (user-error "qq: failed to save media: %s"
                     (error-message-string err)))))))

(cl-defun qq-media-segment-play (segment &key owner)
  "Play video SEGMENT, preferring local files when available.

OWNER is captured before remote resolution and forwarded unchanged to Appkit;
an asynchronous callback never resolves a replacement runtime app."
  (unless (qq-media-segment-playable-p segment)
    (user-error "qq: segment is not playable"))
  (if-let* ((file (qq-media-segment-local-file segment)))
      (appkit-media-play-video-source file "qq" :owner owner)
    (qq-media-resolve-segment-resource
     segment
     (lambda (resource)
       (condition-case err
           (let ((resolved-file (alist-get 'file resource))
                 (url (or (alist-get 'url resource)
                          (plist-get (qq-media-segment-capabilities segment)
                                     :remote-url))))
             (cond
              ((qq-media--video-local-file-present-p resolved-file resource)
               (appkit-media-play-video-source
                resolved-file "qq" :owner owner))
              ((appkit-media-url-present-p url)
               (appkit-media-play-video-source url "qq" :owner owner))
              (t
               (error "video segment has no playable source"))))
         ((error quit)
          (message "qq: failed to play video: %s"
                   (error-message-string err)))))
     (lambda (_response reason)
       (message "qq: failed to play video: %s" reason)))))

(defun qq-media-message-primary-segment (message)
  "Return the most useful openable segment from MESSAGE, or nil."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (qq-media-segment-openable-p segment)
          (setq found segment))
        (setq segments (cdr segments))))
    found))

(defun qq-media-message-has-openable-resource-p (message)
  "Return non-nil when MESSAGE has at least one openable resource segment."
  (not (null (qq-media-message-primary-segment message))))

(cl-defun qq-media-open-message-resource (message &key owner)
  "Open the most relevant resource from MESSAGE.

OWNER lifecycle-owns an external player when the primary segment is a video."
  (if-let* ((segment (qq-media-message-primary-segment message)))
      (qq-media-segment-open segment :owner owner)
    (user-error "qq: message has no openable media segment")))

(defun qq-media--avatar-owner ()
  "Return the managed account owning an avatar request."
  (let ((owner (qq-runtime-current-account-id)))
    (unless (and owner (qq-account-get owner))
      (user-error "qq: avatar resolution requires a managed QQ account"))
    owner))

(defun qq-media--project-avatar-locator
    (result owner identity-key identity)
  "Project RESULT into an HTTPS avatar resource.

OWNER is the requesting account.  IDENTITY-KEY and IDENTITY identify the
exact user or group whose avatar was requested."
  (unless
      (and
       (qq-account--exact-object-keys-p
        result (list 'account_id identity-key 'url))
       (equal (alist-get 'account_id result) owner)
       (equal (alist-get identity-key result) identity)
       (qq-account--non-empty-string-p (alist-get 'url result))
       (string-prefix-p "https://" (alist-get 'url result)))
    (error "qq: Gateway returned an invalid avatar locator"))
  `((url . ,(alist-get 'url result))))

(defun qq-media--fetch-avatar-locator
    (method identity-key identity callback errback)
  "Resolve one native avatar through METHOD.

IDENTITY-KEY names the exact decimal IDENTITY parameter.  CALLBACK receives
an owned media resource alist; ERRBACK follows the native RPC convention."
  (unless (qq-account--canonical-decimal-p identity)
    (user-error "qq: avatar resolution requires an exact decimal identity"))
  (let ((owner (qq-media--avatar-owner)))
    (qq-rpc-call
     method
     `((account_id . ,owner) (,identity-key . ,identity))
     :current-p (lambda () (and (qq-account-get owner) t))
     :stale-code "account_removed"
     :stale-message "QQ account was removed during avatar resolution"
     :projector
     (lambda (result)
       (qq-media--project-avatar-locator
        result owner identity-key identity))
     :callback
     (lambda (resource)
       (qq-runtime-with-account owner
         (qq-account--invoke callback resource)))
     :errback
     (lambda (body reason)
       (qq-runtime-with-account owner
         (qq-account--invoke errback body reason))))))

(defun qq-media--fetch-native-user-avatar-locator
    (user-id callback &optional errback)
  "Resolve USER-ID's native HTTPS avatar and call CALLBACK."
  (qq-media--fetch-avatar-locator
   "contact.get_user_avatar" 'user_uin user-id callback errback))

(defun qq-media--fetch-native-group-avatar
    (group-id callback &optional errback)
  "Resolve GROUP-ID's native HTTPS avatar and call CALLBACK."
  (qq-media--fetch-avatar-locator
   "contact.get_group_avatar" 'group_uin group-id callback errback))

(defun qq-media--native-user-avatar-resource (user-id)
  "Return an authoritative cached avatar resource for USER-ID, or nil.

The native friend directory is preferred.  Message snapshots remain a useful
fallback for identities observed outside that directory."
  (or
   (when-let* ((friend (qq-state-friend user-id))
               (url (alist-get 'avatar_url friend))
               ((appkit-media-url-present-p url)))
     `((url . ,url)))
   (catch 'resource
     (dolist (session (qq-state-sessions))
       (dolist (message (qq-state-session-messages (alist-get 'key session)))
         (when (and (equal (format "%s" (or (alist-get 'sender-id message) ""))
                           (format "%s" user-id))
                    (appkit-media-url-present-p
                     (alist-get 'sender-avatar-url message)))
           (throw 'resource
                  `((url . ,(alist-get 'sender-avatar-url message)))))))
     nil)))

(defun qq-media--fetch-native-user-avatar (user-id done error)
  "Resolve USER-ID avatar and call DONE or ERROR."
  (if-let* ((resource (qq-media--native-user-avatar-resource user-id)))
      (funcall done resource)
    (if (and (qq-runtime-current-account-id)
             (member "contact.get_user_avatar"
                     (qq-server-capabilities)))
        (qq-media--fetch-native-user-avatar-locator user-id done error)
      (funcall error nil "native avatar locator is unavailable"))))

(defun qq-media-open-user-avatar (user-id)
  "Open avatar for USER-ID."
  (unless user-id
    (user-error "qq: missing user id for avatar"))
  (let ((key (format "avatar:%s" user-id)))
    (qq-media--resolve-resource
     key
     (lambda (done)
       (qq-media--fetch-native-user-avatar
        user-id done #'qq-api--default-error))
     (lambda (resource)
       (qq-media-open-resource resource 'image key)))))

(defun qq-media-open-group-avatar (group-id)
  "Open group avatar for GROUP-ID."
  (unless group-id
    (user-error "qq: missing group id for avatar"))
  (qq-media--resolve-resource
   (format "group-avatar:%s" group-id)
   (lambda (done)
     (qq-media--fetch-native-group-avatar
      group-id done #'qq-api--default-error))
   (lambda (resource)
     (qq-media-open-resource
      resource 'image (format "group-avatar:%s" group-id)))))

(defun qq-media-open-session-avatar (session)
  "Open session avatar for SESSION."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group (qq-media-open-group-avatar target-id))
      ('dataline (user-error "qq: dataline sessions have no QQ avatar"))
      (_ (qq-media-open-user-avatar target-id)))))

(defun qq-media--guild-member-avatar-key (guild-id native-id)
  "Return the cache key for GUILD-ID member NATIVE-ID."
  (format "guild-member-avatar:%s:%s" guild-id native-id))

(defun qq-media--message-avatar-identity (message)
  "Return the native avatar identity represented by MESSAGE.

The result is either `(:guild-member GUILD-ID NATIVE-ID)' or
`(:user USER-ID)'.  Guild member ids and ordinary QQ user ids deliberately
remain disjoint: a Guild tiny id is not a QQ account id."
  (if-let* ((session-key (alist-get 'session-key message))
            ((eq (qq-state-session-key-type session-key) 'guild-channel))
            (guild-id
             (alist-get 'guild-id
                        (qq-state-session-key-identity session-key)))
            (native-id (alist-get 'sender-native-id message)))
      (list :guild-member guild-id native-id)
    (when-let* ((sender-id (or (alist-get 'sender-id message)
                               (alist-get 'user-id message)))
                ((not (equal (format "%s" sender-id) "0"))))
      (list :user sender-id))))

(defun qq-media--message-snapshot-avatar-url (message)
  "Return MESSAGE's authoritative non-Guild snapshot avatar URL, or nil.

Merged-forward nodes carry per-entry avatar URLs because their projected QQ
UIN can be a repeated fallback shared by unrelated authors.  Guild messages
retain their native member cache identity and its existing URL handling."
  (let ((identity (qq-media--message-avatar-identity message))
        (url (alist-get 'sender-avatar-url message)))
    (and (not (eq (car-safe identity) :guild-member))
         (appkit-media-url-present-p url)
         url)))

(defun qq-media--message-snapshot-avatar-key (message)
  "Return the URL-scoped avatar cache key for MESSAGE, or nil."
  (when-let* ((url (qq-media--message-snapshot-avatar-url message)))
    (format "message-avatar-url:%s" url)))

(defun qq-media-message-avatar-cache-key (message)
  "Return the logical avatar cache key affecting MESSAGE, or nil."
  (or (qq-media--message-snapshot-avatar-key message)
      (pcase (qq-media--message-avatar-identity message)
        (`(:guild-member ,guild-id ,native-id)
         (qq-media--guild-member-avatar-key guild-id native-id))
        (`(:user ,user-id)
         (format "avatar:%s" user-id)))))

(defun qq-media-open-message-avatar (message)
  "Open sender avatar for MESSAGE."
  (if-let* ((url (qq-media--message-snapshot-avatar-url message))
            (key (qq-media--message-snapshot-avatar-key message)))
      (qq-media-open-image-url key url)
    (pcase (qq-media--message-avatar-identity message)
      (`(:guild-member ,guild-id ,native-id)
       (qq-media-open-guild-member-avatar guild-id native-id))
      (`(:user ,user-id)
       (qq-media-open-user-avatar user-id))
      (_
       (user-error "qq: message sender has no native avatar identity")))))

(defun qq-media--guild-member-avatar-resource (profile)
  "Return the avatar resource projected from Guild member PROFILE."
  `((url . ,(alist-get 'avatar_url profile))))

(defun qq-media-open-guild-member-avatar (guild-id native-id)
  "Open the native channel avatar for GUILD-ID member NATIVE-ID."
  (let ((key (qq-media--guild-member-avatar-key guild-id native-id)))
    (qq-media--resolve-resource
     key
     (lambda (done)
       (qq-api-get-guild-member-profile
        guild-id native-id
        (lambda (profile)
          (funcall done (qq-media--guild-member-avatar-resource profile)))))
     (lambda (resource)
       (qq-media-open-resource resource 'image key)))))

(defun qq-media-guild-member-avatar-image (guild-id native-id)
  "Return the inline native avatar for GUILD-ID member NATIVE-ID."
  (qq-media--ensure-resource-image
   (qq-media--guild-member-avatar-key guild-id native-id)
   (lambda (done error)
     (qq-api-get-guild-member-profile
      guild-id native-id
      (lambda (profile)
        (funcall done (qq-media--guild-member-avatar-resource profile)))
      error))
   qq-media-avatar-image-height))

(defun qq-media-message-avatar-image (message)
  "Return the identity-correct inline sender avatar for MESSAGE."
  (let ((identity (qq-media--message-avatar-identity message))
        (avatar-url (alist-get 'sender-avatar-url message))
        (snapshot-url (qq-media--message-snapshot-avatar-url message)))
    (cond
     (snapshot-url
      (qq-media--ensure-resource-image
       (qq-media--message-snapshot-avatar-key message)
       (lambda (done _error)
         (funcall done `((url . ,snapshot-url))))
       qq-media-avatar-image-height))
     (t
      (pcase identity
        (`(:guild-member ,guild-id ,native-id)
         (if (appkit-media-url-present-p avatar-url)
             (qq-media--ensure-resource-image
              (qq-media--guild-member-avatar-key guild-id native-id)
              (lambda (done _error)
                (funcall done `((url . ,avatar-url))))
              qq-media-avatar-image-height)
           (qq-media-guild-member-avatar-image guild-id native-id)))
        (`(:user ,user-id)
         (qq-media-avatar-image user-id)))))))

(defun qq-media-avatar-image (user-id)
  "Return inline avatar image for USER-ID, triggering fetch when needed."
  (let ((key (format "avatar:%s" user-id)))
    (qq-media--ensure-resource-image
     key
     (lambda (done error)
       (qq-media--fetch-native-user-avatar user-id done error))
     qq-media-avatar-image-height)))

(defun qq-media-avatar-display-string (user-id)
  "Return inline display string for USER-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-avatar-image user-id)
   "@"))

(defun qq-media-avatar-cached-display-string (user-id)
  "Return cached inline avatar for USER-ID without starting network work."
  (qq-media--image-display-string
   (qq-media--cached-image (format "avatar:%s" user-id))
   "@"))

(defun qq-media-url-preview-image (key url &optional max-height)
  "Return preview image for remote URL under cache KEY.

Trigger an asynchronous download when the image is not cached yet."
  (when (appkit-media-url-present-p url)
    (qq-media--ensure-resource-image
     key
     (lambda (done _error)
       (funcall done `((url . ,url))))
     max-height
     #'qq-media--preview-image-from-file)))

(defun qq-media-url-preview-display-string (key url fallback &optional max-height)
  "Return display string for remote image URL cached under KEY.

Use FALLBACK until the preview is available."
  (qq-media--image-display-string
   (qq-media-url-preview-image key url max-height)
   fallback))

(defun qq-media-poke-image-cache-key (url)
  "Return the cache key used for a decorative POKE image URL."
  (and (appkit-media-url-present-p url)
       (format "poke-image-url:%s" url)))

(defun qq-media-open-image-url (key url)
  "Open remote image URL using media cache KEY."
  (unless (appkit-media-url-present-p url)
    (user-error "qq: image has no remote URL"))
  (qq-media-open-resource `((url . ,url)) 'image key))

(defun qq-media-group-avatar-image (group-id)
  "Return inline group avatar image for GROUP-ID, triggering fetch when needed."
  (qq-media--ensure-resource-image
   (format "group-avatar:%s" group-id)
   (lambda (done error)
     (qq-media--fetch-native-group-avatar group-id done error))
   qq-media-avatar-image-height))

(defun qq-media-group-avatar-display-string (group-id)
  "Return inline display string for GROUP-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-group-avatar-image group-id)
   "#"))

(defun qq-media-group-avatar-cached-display-string (group-id)
  "Return cached inline group avatar for GROUP-ID without fetching it."
  (qq-media--image-display-string
   (qq-media--cached-image (format "group-avatar:%s" group-id))
   "#"))

(defun qq-media-session-avatar-display-string (session)
  "Return inline avatar display string for SESSION."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group (qq-media-group-avatar-display-string target-id))
      ('dataline "📱")
      (_ (qq-media-avatar-display-string target-id)))))

(defvar qq-media--face-names-table nil
  "Lazy hash table: face id string → QDes name (e.g. \"/斜眼笑\").")

(defun qq-media--face-names-file ()
  "Return resolved path to the face-names JSON, preferring a readable file."
  (let* ((configured qq-media-face-names-file)
         (lib (or (locate-library "qq-media.el")
                  (locate-library "qq-customize.el")))
         (beside (and lib
                      (expand-file-name
                       "qq-face-names.json"
                       (file-name-directory (file-truename lib))))))
    (cond
     ((and (stringp configured) (file-readable-p configured)) configured)
     ((and (stringp beside) (file-readable-p beside)) beside)
     (t configured))))

(defun qq-media--load-face-names-table ()
  "Load face-names JSON into `qq-media--face-names-table'."
  (or qq-media--face-names-table
      (let ((table (make-hash-table :test #'equal))
            (file (qq-media--face-names-file)))
        (when (and (stringp file) (file-readable-p file))
          (condition-case err
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (json-false nil)
                     (data (json-read-file file)))
                (dolist (pair data)
                  (when (and (consp pair)
                             (stringp (car pair))
                             (stringp (cdr pair)))
                    (puthash (car pair) (cdr pair) table))))
            (error
             (message "qq: failed to load face names from %s: %s"
                      file (error-message-string err)))))
        (setq qq-media--face-names-table table)
        table)))

(defun qq-media-face-name (emoji-id)
  "Return human-readable QQ face name for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (table (qq-media--load-face-names-table)))
    (or (gethash id table)
        ;; Also accept numeric keys that json may have stored differently.
        (and (string-match-p "\\`[0-9]+\\'" id)
             (gethash id table)))))

(defun qq-media--face-id-number (id)
  "Return numeric value of face ID string, or nil."
  (and (stringp id)
       (string-match-p "\\`[0-9]+\\'" id)
       (string-to-number id)))

(defun qq-media-face-completion-candidates ()
  "Return base face candidates sorted by numeric id.

Each candidate looks like \"/斜眼笑  (178)\" so users can search by name
or numeric id.  Use `qq-media-face-id-from-completion'.

Order is by face id (0, 1, 2, …) — the same order as QQ's default
emoji panel.  Pair with `qq-media-face-completion-table' so Vertico
does not re-sort by string length/history."
  (let ((table (qq-media--load-face-names-table))
        (candidates nil))
    (maphash
     (lambda (id name)
       (push (cons id (format "%s  (%s)"
                              (or name (format "[face:%s]" id))
                              id))
             candidates))
     table)
    (mapcar
     #'cdr
     (sort candidates
           (lambda (a b)
             (let ((ida (qq-media--face-id-number (car a)))
                   (idb (qq-media--face-id-number (car b))))
               (cond
                ((and ida idb) (< ida idb))
                (ida t)
                (idb nil)
                (t (string-lessp (car a) (car b))))))))))

(defun qq-media-face-id-from-completion (candidate)
  "Extract face id string from a `qq-media-face-completion-candidates' CANDIDATE."
  (let ((text (and (stringp candidate)
                   (substring-no-properties candidate))))
    (cond
     ((and text
           (string-match "(\\([0-9]+\\))\\'" text))
      (match-string 1 text))
     ((and text
           (string-match-p "\\`[0-9]+\\'" text))
      text)
     (t nil))))

(defun qq-media--face-completion-prefix (id)
  "Return minibuffer prefix string with the face image for ID, or spaces."
  (let* ((file (qq-media--local-base-emoji-file id))
         (image (and file
                     (qq-media--image-from-file
                      file
                      (max 1 qq-media-face-image-height)))))
    (if image
        (concat (propertize " " 'display image) " ")
      "  ")))

(defun qq-media-face-affixation-function (candidates)
  "Affixation function: show local face PNG before each CANDIDATE.

Uses LinuxQQ `default-emojis/<id>.png' only (sync).  Missing files get
a blank spacer so columns stay aligned."
  (mapcar
   (lambda (cand)
     (let ((id (qq-media-face-id-from-completion cand)))
       (list cand
             (if id (qq-media--face-completion-prefix id) "  ")
             "")))
   candidates))

(defun qq-media-face-completion-table ()
  "Completion table for base QQ faces with images and stable id order.

Metadata:
- `display-sort-function'/`cycle-sort-function' = identity (keep id order)
- `affixation-function' = face PNG prefix for Vertico/Icomplete"
  (let ((candidates (qq-media-face-completion-candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata
            (category . qq-face)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . qq-media-face-affixation-function))
        (complete-with-action action candidates string pred)))))


;;; Favorite / custom faces (收藏表情)

(defun qq-media--json-truthy-p (value)
  "Return non-nil when JSON VALUE is a true-ish flag (not :false/:null)."
  (and value
       (not (eq value :false))
       (not (eq value :null))
       (not (equal value 0))
       (not (equal value "false"))
       (not (equal value "0"))))

(defun qq-media--face-alist-p (value)
  "Return non-nil when VALUE looks like one face resource alist."
  (and (consp value)
       (listp value)
       (consp (car value))
       ;; first element is (KEY . VAL), not another nested face list
       (symbolp (car (car value)))))

(defun qq-media--normalize-custom-face-list (data)
  "Normalize DATA from fetch_custom_face_info into a list of face alists.

NapCat returns a JSON array → vector or list of alists.  A single face
alist must not be confused with a list of faces: for a list of faces the
first element is itself an alist; for one face the first element is a
pair `(url . …)'."
  (cond
   ((null data) nil)
   ((vectorp data)
    (qq-media--normalize-custom-face-list (append data nil)))
   ((not (listp data)) nil)
   ;; List of face alists: car is an alist (caar is a pair).
   ((and (consp (car data))
         (qq-media--face-alist-p (car data)))
    data)
   ;; Single face alist: car is (symbol . value).
   ((qq-media--face-alist-p data)
    (list data))
   (t nil)))

(defun qq-media-custom-faces (&optional force)
  "Return cached favorite custom faces, or nil if not loaded.

With FORCE non-nil, ignore the cache (caller should still refresh via
`qq-media-refresh-custom-faces')."
  (unless force
    qq-media--custom-faces))

(defun qq-media-custom-faces-loaded-p ()
  "Return non-nil after the favorite-face cache has been fetched."
  (numberp qq-media--custom-faces-fetched-at))

(defun qq-media--refresh-custom-faces-page
    (owner callback errback requested-count)
  "Fetch one favorite-face page owned by OWNER.

CALLBACK, ERRBACK, and REQUESTED-COUNT have the same meaning as in
`qq-media-refresh-custom-faces'.  Full responses continue recursively under
the exact same owner, so a reset invalidates every page in the chain."
  (when (qq-media--custom-face-owner-current-p owner)
    (let ((request-identity (list 'favorite-face-page requested-count))
          request-token)
      ;; Publish page ownership before dispatch.  A synchronous full-page
      ;; response may recursively publish the next page before this dispatch
      ;; returns its token.
      (setf (plist-get owner :request) request-identity
            (plist-get owner :token) nil)
      (setq request-token
            (qq-api-fetch-custom-face-info
             (lambda (data)
               (when (qq-media--custom-face-request-current-p
                      owner request-identity)
                 (let* ((faces (qq-media--normalize-custom-face-list data))
                        (n (length faces))
                        (max-count
                         (max requested-count
                              qq-media-custom-face-count-max)))
                   (if (and (>= n requested-count)
                            (< requested-count max-count))
                       (qq-media--refresh-custom-faces-page
                        owner callback errback
                        (min max-count
                             (max (* requested-count 2)
                                  (1+ requested-count))))
                     (setq qq-media--custom-faces faces
                           qq-media--custom-faces-fetched-at (float-time))
                     (when (and (>= n requested-count)
                                (>= requested-count max-count))
                       (message
                        "qq: favorite faces may be truncated (%d returned, count max %d)"
                        n max-count))
                     (unwind-protect
                         (when callback
                           (funcall callback faces))
                       ;; Shared waiter callbacks clear OWNER themselves.  A
                       ;; direct refresh still needs a terminal page boundary.
                       (when (qq-media--custom-face-request-current-p
                              owner request-identity)
                         (setq qq-media--custom-face-refresh-owner nil)))))))
             (lambda (response reason)
               (when (qq-media--custom-face-request-current-p
                      owner request-identity)
                 (unwind-protect
                     (when errback
                       (funcall errback response reason))
                   (when (qq-media--custom-face-request-current-p
                          owner request-identity)
                     (setq qq-media--custom-face-refresh-owner nil)))))
             requested-count))
      (if (qq-media--custom-face-request-current-p owner request-identity)
          (setf (plist-get owner :token) request-token)
        ;; The dispatch synchronously settled, advanced, or was reset.  Its
        ;; returned token must not replace the page that now owns the chain.
        (qq-media--cancel-custom-face-request request-token))
      (and (qq-media--custom-face-owner-current-p owner)
           (plist-get owner :token)))))

(defun qq-media-refresh-custom-faces (&optional callback errback count)
  "Fetch favorite custom faces from NapCat and cache them.

CALLBACK is called with the face list on success.  The request is owned by the
current account generation, including all automatic larger-page retries.
ERRBACK receives the transport response and failure reason.
COUNT is the initial requested page size.

`fetch_custom_face_info' is capped by its `count' argument (see
`qq-media-custom-face-count').  When the response is full — length equals the
requested count — this function retries with a larger count (doubling, capped
by `qq-media-custom-face-count-max') so large favorites libraries are not
silently truncated at 96/page-size."
  (let ((owner (or qq-media--custom-face-refresh-owner
                   (list :generation qq-media--account-generation
                         :request nil
                         :token nil))))
    (unless qq-media--custom-face-refresh-owner
      (setq qq-media--custom-face-refresh-owner owner))
    (qq-media--refresh-custom-faces-page
     owner callback errback
     (max 1 (or count qq-media-custom-face-count)))))

(defun qq-media--finish-custom-face-waiters (owner faces)
  "Finish OWNER's shared favorite-face waiters successfully with FACES."
  (when (qq-media--custom-face-owner-current-p owner)
    (let ((waiters (prog1 (nreverse qq-media--custom-face-waiters)
                     (setq qq-media--custom-face-waiters nil
                           qq-media--custom-face-refresh-owner nil))))
      (dolist (waiter waiters)
        (when-let* ((current-generation-p
                     (qq-media--custom-face-owner-generation-current-p owner))
                    (callback (car waiter)))
          (condition-case err
              (funcall callback faces)
            (error
             (message "qq: favorite-face callback failed: %s"
                      (error-message-string err)))
            (quit
             (message "qq: favorite-face callback was interrupted"))))))))

(defun qq-media--fail-custom-face-waiters (owner response reason)
  "Fail OWNER's shared favorite-face waiters with RESPONSE and REASON."
  (when (qq-media--custom-face-owner-current-p owner)
    (let ((waiters (prog1 (nreverse qq-media--custom-face-waiters)
                     (setq qq-media--custom-face-waiters nil
                           qq-media--custom-face-refresh-owner nil))))
      (dolist (waiter waiters)
        (when-let* ((current-generation-p
                     (qq-media--custom-face-owner-generation-current-p owner))
                    (errback (cdr waiter)))
          (condition-case err
              (funcall errback response reason)
            (error
             (message "qq: favorite-face errback failed: %s"
                      (error-message-string err)))
            (quit
             (message "qq: favorite-face errback was interrupted"))))))))

(defun qq-media-ensure-custom-faces (&optional callback errback force)
  "Call CALLBACK with the authoritative favorite-face cache.

Coalesce concurrent NapCat requests and notify every waiter.  ERRBACK receives
the transport response and reason.  With FORCE non-nil, refresh even when the
cache was already loaded."
  (if (and (not force) (qq-media-custom-faces-loaded-p))
      (progn
        (when callback
          (funcall callback (qq-media-custom-faces)))
        t)
    (push (cons callback errback) qq-media--custom-face-waiters)
    (unless qq-media--custom-face-refresh-owner
      (let ((owner (list :generation qq-media--account-generation
                         :request nil
                         :token nil)))
        (setq qq-media--custom-face-refresh-owner owner)
        (condition-case err
            (qq-media-refresh-custom-faces
             (apply-partially
              #'qq-media--finish-custom-face-waiters owner)
             (apply-partially
              #'qq-media--fail-custom-face-waiters owner))
          (error
           (when (qq-media--custom-face-owner-current-p owner)
             (qq-media--fail-custom-face-waiters
              owner nil (error-message-string err)))))))
    qq-media--custom-face-refresh-owner))

(defun qq-media-custom-face-id (face)
  "Return a stable string id for favorite FACE alist."
  (or (let ((md5 (alist-get 'md5 face)))
        (and (stringp md5) (not (string-empty-p md5)) md5))
      (let ((res (alist-get 'res_id face)))
        (and (stringp res) (not (string-empty-p res)) res))
      (let ((emo (alist-get 'emo_id face)))
        (and emo (format "emo:%s" emo)))
      (format "fav:%s" (sxhash-equal face))))

(defun qq-media-custom-face-label (face &optional index)
  "Return a human completion label for favorite FACE.

INDEX, when non-nil, is included so completing-read candidates stay unique
even when several favorites share an empty `desc'."
  (let* ((desc (alist-get 'desc face))
         (md5 (alist-get 'md5 face))
         (emo (alist-get 'emo_id face))
         (mark (qq-media--json-truthy-p (alist-get 'is_mark_face face)))
         (short (cond
                 ((and (stringp desc) (not (string-empty-p (string-trim desc))))
                  (string-trim desc))
                 ((and (stringp md5) (>= (length md5) 8))
                  (concat (substring md5 0 8) "…"))
                 ((and emo (not (equal emo 0)) (not (equal emo "0")))
                  (format "#%s" emo))
                 (t "favorite")))
         (kind (if mark "mface" "fav"))
         (base (format "[%s] %s" kind short)))
    (if index
        (format "%s  (%d)" base (1+ index))
      base)))

(defun qq-media-custom-face-file (face)
  "Return best local image path for FACE, or nil."
  (seq-find
   #'appkit-media-file-present-p
   (list (alist-get 'file face)
         (alist-get 'original_file face)
         (alist-get 'thumb_file face))))

(defun qq-media-custom-face-thumb (face)
  "Return best local thumb path for FACE, or nil."
  (seq-find
   #'appkit-media-file-present-p
   (list (alist-get 'thumb_file face)
         (alist-get 'file face)
         (alist-get 'original_file face))))

(defun qq-media-custom-face-display-string (face)
  "Return composer/timeline display string for favorite FACE."
  (let* ((thumb (qq-media-custom-face-thumb face))
         (image (and thumb
                     (qq-media--image-from-file
                      thumb
                      (max qq-media-face-image-height 32))))
         (fallback (qq-media-custom-face-label face)))
    (qq-media--image-display-string image fallback)))

(defun qq-media-custom-face-sendable-p (face)
  "Return non-nil when favorite FACE has a valid outbound resource."
  (when (qq-media--face-alist-p face)
    (let ((mark (qq-media--json-truthy-p (alist-get 'is_mark_face face)))
          (e-id (alist-get 'e_id face))
          (url (alist-get 'url face)))
      (or (and mark
               (stringp e-id)
               (not (string-empty-p e-id)))
          (qq-media-custom-face-file face)
          (appkit-media-url-present-p url)))))

(defun qq-media-custom-face-completion-candidates (&optional faces)
  "Return completion candidates for favorite FACES (default: cache).

Each entry is `(LABEL . FACE)'.  Labels are unique (md5 + index).
Order follows the NapCat favorites list (not re-sorted by string)."
  (let ((faces (seq-filter #'qq-media-custom-face-sendable-p
                           (or faces qq-media--custom-faces)))
        (candidates nil)
        (i 0))
    (dolist (face faces)
      (when (qq-media--face-alist-p face)
        (let* ((label (qq-media-custom-face-label face i))
               ;; Attach FACE so affixation / lookup work even if the
               ;; completion UI strips the pairs list context.
               (labeled (propertize label 'qq-custom-face face)))
          (push (cons labeled face) candidates)
          (setq i (1+ i)))))
    (nreverse candidates)))

(defun qq-media-custom-face-from-completion (candidate &optional pairs)
  "Return face alist matching completion CANDIDATE in PAIRS.

PAIRS defaults to the active completion pairs, then the cache."
  (or (and (stringp candidate)
           (get-text-property 0 'qq-custom-face candidate))
      (let* ((key (and (stringp candidate)
                       (substring-no-properties candidate)))
             (pairs (or pairs
                        qq-media--custom-face-completion-pairs
                        (qq-media-custom-face-completion-candidates))))
        (when key
          (or (cdr (assoc key pairs))
              ;; assoc with text-propertized cars: match by plain string.
              (cdr (seq-find (lambda (p)
                               (equal key (substring-no-properties (car p))))
                             pairs)))))))

(defun qq-media--custom-face-completion-prefix (face)
  "Return minibuffer prefix with thumb image for FACE, or spaces."
  (let* ((thumb (qq-media-custom-face-thumb face))
         (image (and thumb
                     (qq-media--image-from-file
                      thumb
                      (max qq-media-face-image-height 32)))))
    (if image
        (concat (propertize " " 'display image) " ")
      "  ")))

(defun qq-media-custom-face-affixation-function (candidates)
  "Affixation function: show favorite thumb before each CANDIDATE."
  (mapcar
   (lambda (cand)
     (let ((face (qq-media-custom-face-from-completion cand)))
       (list cand
             (if face
                 (qq-media--custom-face-completion-prefix face)
               "  ")
             "")))
   candidates))

(defun qq-media-custom-face-completion-table (&optional faces)
  "Completion table for favorite custom faces with images and stable order.

Keeps NapCat favorites order (identity display/cycle sort) and prefixes
each candidate with its local thumb when available."
  (let* ((pairs (qq-media-custom-face-completion-candidates faces))
         (labels (mapcar #'car pairs)))
    (setq qq-media--custom-face-completion-pairs pairs)
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata
            (category . qq-custom-face)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . qq-media-custom-face-affixation-function))
        (complete-with-action action labels string pred)))))

(defun qq-media-custom-face-to-segment (face)
  "Convert favorite FACE alist into an outbound OneBot segment.

Personal favorites (most common) are sent as image with `sub_type' 1
(KCUSTOM sticker).  Market favorites (`is_mark_face') become mface when
e_id is present."
  (let* ((mark (qq-media--json-truthy-p (alist-get 'is_mark_face face)))
         (e-id (alist-get 'e_id face))
         (ep-id (alist-get 'ep_id face))
         (file (qq-media-custom-face-file face))
         (url (alist-get 'url face))
         (desc (alist-get 'desc face))
         (md5 (alist-get 'md5 face))
         (summary (if (and (stringp desc) (not (string-empty-p (string-trim desc))))
                      (string-trim desc)
                    "[收藏表情]")))
    (cond
     ((and mark
           (stringp e-id)
           (not (string-empty-p e-id)))
      `((type . "mface")
        (data . ((emoji_id . ,e-id)
                 (emoji_package_id . ,(condition-case _
                                          (string-to-number (format "%s" (or ep-id 0)))
                                        (error 0)))
                 (key . ,(or (alist-get 'key face) ""))
                 (summary . ,summary)))))
     ((or file (appkit-media-url-present-p url))
      `((type . "image")
        (data . (,@(when file `((file . ,file)))
                 ,@(when (appkit-media-url-present-p url) `((url . ,url)))
                 (name . ,(or md5 "custom-face"))
                 (summary . ,summary)
                 ;; PicSubType.KCUSTOM — QQ treats this as a sticker/emoji image.
                 (sub_type . 1)))))
     (t
      (user-error "qq: favorite face has neither local file nor URL")))))

(defun qq-media-face-text-fallback (emoji-id)
  "Return plain-text fallback for face EMOJI-ID (never a CQ blob)."
  (or (qq-media-face-name emoji-id)
      (format "[face:%s]" emoji-id)))

(defun qq-media--local-base-emoji-file (emoji-id)
  "Return path to LinuxQQ default face image for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (dir qq-media-default-emoji-directory))
    (when (and (stringp dir)
               (not (string-empty-p id))
               (file-directory-p dir))
      (seq-find
       #'file-exists-p
       (mapcar (lambda (ext)
                 (expand-file-name (concat id "." ext) dir))
               '("png" "gif" "webp" "jpg" "jpeg"))))))

(defun qq-media--face-resource-from-local (emoji-id)
  "Return resource alist for local face EMOJI-ID, or nil."
  (when-let* ((file (qq-media--local-base-emoji-file emoji-id)))
    `((file . ,file)
      (emoji_id . ,(format "%s" emoji-id))
      (description . ,(qq-media-face-name emoji-id)))))

(defun qq-media--prepare-animated-face-resource (resource callback)
  "Pass RESOURCE to CALLBACK, converting native APNG to animated GIF.

Emacs' PNG loader displays APNG as a single frame.  QQ's native base emoji
service returns APNG resources, so convert those once into the existing media
cache; GIF is then handled by appkit's bounded inline-animation machinery."
  (let* ((resource (copy-tree resource))
         (file (alist-get 'file resource))
         (animated (qq-media--json-truthy-p (alist-get 'animated resource)))
         (ffmpeg (and animated (executable-find "ffmpeg"))))
    (if (not (and ffmpeg
                  (appkit-media-file-present-p file)
                  (string-match-p "\\.png\\'" (downcase file))))
        (funcall callback resource)
      (let* ((target (expand-file-name
                      (format "face-animation-%s.gif" (md5 file))
                      qq-media-cache-directory)))
        (if (appkit-media-file-present-p target)
            (progn
              (setf (alist-get 'file resource) target)
              (funcall callback resource))
          (make-directory qq-media-cache-directory t)
          (let ((buffer (generate-new-buffer " *qq-face-apng*")))
            (make-process
             :name (format "qq-face-apng-%s" (substring (md5 file) 0 8))
             :buffer buffer
             :noquery t
             :command
             (list ffmpeg "-nostdin" "-y" "-loglevel" "error"
                   "-i" file "-filter_complex"
                   (concat "[0:v]fps=20,scale=128:-1:flags=lanczos,split[a][b];"
                           "[a]palettegen=max_colors=128[p];"
                           "[b][p]paletteuse=dither=bayer:bayer_scale=3")
                   "-loop" "0" target)
             :sentinel
             (lambda (process _event)
               (when (memq (process-status process) '(exit signal))
                 (unwind-protect
                     (progn
                       (when (and (= (process-exit-status process) 0)
                                  (appkit-media-file-present-p target))
                         (setf (alist-get 'file resource) target))
                       (funcall callback resource))
                   (when (buffer-live-p (process-buffer process))
                     (kill-buffer (process-buffer process)))))))))))))

(defun qq-media--face-image-from-file (file height)
  "Create a base-face image from FILE, enlarging animated resources."
  (let ((image (qq-media--image-from-file file height)))
    (if (and image (appkit-media-inline-animation-image-p image))
        (qq-media--image-from-file file qq-media-animated-face-image-height)
      image)))

(defun qq-media-face-image (emoji-id)
  "Return inline QQ base face image for EMOJI-ID.

Resolve only from LinuxQQ `default-emojis/<id>.png'.  A missing native
resource returns nil so the caller can render the face description; it must
not make a removed OneBot request from the timeline renderer."
  (let* ((id (format "%s" emoji-id))
         (key (format "face:%s" id))
         (local (qq-media--face-resource-from-local id)))
    (when local
      (qq-media--cache-resource key local))
    (when local
      (qq-media--ensure-resource-image
       key
       (lambda (done _error)
         (funcall done local))
       qq-media-face-image-height
       #'qq-media--face-image-from-file))))

(defun qq-media-face-display-string (emoji-id &optional description)
  "Return inline display string for QQ face EMOJI-ID.

Prefer the face image (local default-emojis first).  When the image is
not ready yet, show DESCRIPTION or the known human face name
(`/斜眼笑') rather than CQ."
  (qq-media--image-display-string
   (qq-media-face-image emoji-id)
   (or (and (stringp description)
            (not (string-empty-p description))
            description)
       (qq-media-face-text-fallback emoji-id))))

(defun qq-media-segment-preview-key (segment)
  "Return preview cache key for SEGMENT, or nil when unsupported."
  (when (qq-media-segment-preview-capable-p segment)
    (let* ((native-image-id (qq-media--native-image-media-id segment))
           (native-video-id (qq-media--native-video-media-id segment))
           (type (alist-get 'type segment))
           (resolver-identity
            (and (equal type "video")
                 (qq-media--video-resolver-cache-identity segment)))
           (preview-type (cond
                          ((qq-media-imageish-file-segment-p segment)
                           "file-image")
                          ((qq-media-videoish-segment-p segment)
                           (appkit-media-video-preview-policy-key))
                          (t type)))
           (file-key (qq-media--segment-file-key segment))
           (url (qq-media--segment-url segment)))
      (cond
       (native-image-id
        (qq-media--native-image-key native-image-id))
       (native-video-id
        (qq-media--native-video-thumbnail-key native-video-id))
       (resolver-identity
        (format "preview:%s:%s" preview-type resolver-identity))
       (file-key
        (format "preview:%s:%s" preview-type file-key))
       ((appkit-media-url-present-p url)
        (format "preview:%s-url:%s" preview-type url))
       (t nil)))))

(defun qq-media-segment-cache-keys (segment)
  "Return logical media cache keys that can affect SEGMENT rendering."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (face-id (and (equal type "face") (alist-get 'id data)))
         keys)
    (when face-id
      (push (format "face:%s" face-id) keys))
    (when-let* ((resource-key (qq-media--segment-resource-key segment)))
      (push resource-key keys))
    (when-let* ((download-key (qq-media-segment-download-key segment)))
      (push download-key keys))
    (when-let* ((preview-key (qq-media-segment-preview-key segment)))
      (push preview-key keys))
    (when (and (equal type "poke")
               (appkit-media-url-present-p (alist-get 'image-url data)))
      (push (qq-media-poke-image-cache-key (alist-get 'image-url data)) keys))
    (delete-dups (delq nil keys))))

(defun qq-media--video-segment-preview-image (segment key)
  "Return cached preview for video SEGMENT, starting extraction if needed."
  (or (qq-media--cached-image key)
      (unless (or (gethash key qq-media--fetching-cache)
                  (gethash key qq-media--preview-missing-cache))
        (let* ((data (alist-get 'data segment))
               (capabilities (qq-media-segment-capabilities segment))
               (local-file (plist-get capabilities :local-file))
               (source (or (and (appkit-media-file-present-p local-file)
                                local-file)
                           (plist-get capabilities :remote-url)))
               (preview-source
                (seq-find
                 (lambda (candidate)
                   (or (appkit-media-file-present-p candidate)
                       (appkit-media-url-present-p candidate)))
                 (list (alist-get 'thumb data)
                       (alist-get 'thumbnail data)
                       (alist-get 'thumbnail_url data))))
               (source-size (alist-get 'file_size data))
               (duration (alist-get 'duration_secs data))
               (cache-base (qq-media--remote-image-cache-file-base key))
               (cache-file (qq-media--remote-image-cache-existing-file key)))
          (cond
           ((and cache-file (appkit-media-file-present-p cache-file))
            (qq-media--cache-image
             key
             (qq-media--preview-image-from-file cache-file nil)))
           ((not (or (appkit-media-file-present-p source)
                     (appkit-media-url-present-p source)
                     (appkit-media-file-present-p preview-source)
                     (appkit-media-url-present-p preview-source)))
            (puthash key t qq-media--preview-missing-cache)
            nil)
           (t
            (puthash key t qq-media--fetching-cache)
            (make-directory (file-name-directory cache-base) t)
            (condition-case err
                (appkit-media-start-video-preview
                 :key (concat "qq:" key)
                 :source source
                 :preview-source preview-source
                 :source-size source-size
                 :duration duration
                 :cache-base cache-base
                 :callback
                 (lambda (image _target-file)
                   (remhash key qq-media--fetching-cache)
                   (if image
                       (qq-media--cache-image key image)
                     (puthash key t qq-media--preview-missing-cache))
                   (qq-media--note-cache-updated key)))
              (error
               (remhash key qq-media--fetching-cache)
               (puthash key t qq-media--preview-missing-cache)
               (message "qq: video preview failed for %s: %s"
                        key (error-message-string err))))
            nil))))))

(defun qq-media-segment-preview-image (segment)
  "Return inline preview image for SEGMENT, triggering fetch when needed.

Live evidence (emacsclient, forward image
`25BA8E226776B3099D323947E8FE87BE.png`):
- `get_image` with the bare NT file name never invokes success/error
  (left `fetching' stuck for 12s+ with no resource cache).
- The segment `url' downloads successfully through appkit in a few seconds and
  builds a preview image.

So when the wire segment already carries a URL (or local path), seed that
into the preview resource cache *before* `ensure', so we take the URL
download branch instead of blocking forever on `get_image'.  Only fall back
to NapCat `get_image' when there is no usable URL/local path.

Preview failures are soft (no NapCat error spam)."
  (let ((key (qq-media-segment-preview-key segment))
        (native-image-id (qq-media--native-image-media-id segment))
        (native-video-id (qq-media--native-video-media-id segment))
        (local (qq-media--segment-existing-path segment))
        (url (qq-media--segment-url segment)))
    (when key
      (cond
       (native-video-id
        (when-let* ((image
                     (qq-media--ensure-resource-image
                      key
                      (lambda (done error)
                        (qq-media--fetch-native-video-thumbnail-resource
                         segment key done error))
                      nil
                      #'qq-media--preview-image-from-file)))
          (or (appkit-media-video-preview-display-image image 'qq)
              image)))
       ((qq-media-videoish-segment-p segment)
          (when-let* ((image (qq-media--video-segment-preview-image segment key)))
            (or (appkit-media-video-preview-display-image image 'qq)
                image)))
       (t
        (when (or local (appkit-media-url-present-p url))
          (qq-media--cache-resource
           key
           (qq-media--resource-from-local+url local url)))
        (qq-media--ensure-resource-image
         key
         (lambda (done error)
           (cond
            (native-image-id
             (qq-media--fetch-native-image-resource
              segment key done error))
            ((qq-media-segment-preview-capable-p segment)
               (qq-media--resolve-fileish-segment
                segment
                "get_image"
                done
                ;; Soft-fail: clear fetching without user-error / NapCat spam.
                (lambda (_response _reason)
                  (funcall error nil "preview image not found"))
                "preview image not found"))
            (t
             (funcall done nil))))
         nil
         #'qq-media--preview-image-from-file))))))

(defun qq-media-segment-preview-fetching-p (segment)
  "Return non-nil when preview fetch for SEGMENT is currently active."
  (when-let* ((key (qq-media-segment-preview-key segment)))
    (qq-media--resource-fetching-p key)))

(provide 'qq-media)

;;; qq-media.el ends here
