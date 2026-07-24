;;; qq-gateway-media.el --- Native remote-media projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Remote QQ media is neither a staged resource nor a Prepared Attachment.
;; This module projects opaque `media-*' handles and owns the inbound record
;; materialize -> derive PCM WAV -> local access pipeline.  Signed URLs,
;; native file UUIDs, and canonical blob paths never enter Emacs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway-dispatch)
(require 'qq-gateway)
(require 'qq-gateway-rpc)
(require 'qq-gateway-resource)
(require 'qq-runtime)

(defvar qq-gateway-media-changed-hook nil
  "Hook called with REASON and MEDIA-ID after remote-media state changes.")

(defvar qq-gateway-media-desync-hook nil
  "Hook called with an unsolicited remote-media stream error body.")

(defvar qq-gateway-media--media (make-hash-table :test #'equal))
(defvar qq-gateway-media--order nil)
(defvar qq-gateway-media--gateway-instance-id nil)
(defvar qq-gateway-media--refresh-owner nil
  "Identity of the newest authoritative remote-media registry refresh.")
(defvar qq-gateway-media--resync-request-id nil
  "Identity of the in-flight automatic remote-media registry resync.")
(defvar qq-gateway-media--playable-resources (make-hash-table :test #'equal)
  "Ready or staging PCM WAV resource ID cached for each remote media ID.")

(cl-defstruct (qq-gateway-media-operation
               (:constructor qq-gateway-media-operation-create))
  "One cancellable inbound-record playback preparation."
  active-p
  media-id
  account-id
  request-id
  watch
  source-resource-id
  playback-resource-id)

(defun qq-gateway-media--id-p (value)
  "Return non-nil when VALUE is an opaque remote-media identity."
  (and (stringp value)
       (string-match-p
        "\\`media-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'"
        value)))

(defun qq-gateway-media (media-id)
  "Return a copy of remote MEDIA-ID, or nil."
  (qq-gateway-value-copy
   (and media-id (gethash media-id qq-gateway-media--media))))

(defun qq-gateway-media-list ()
  "Return copied remote-media snapshots in authoritative order."
  (delq nil (mapcar #'qq-gateway-media qq-gateway-media--order)))

(defun qq-gateway-media--clear (reason)
  "Clear projected remote media for REASON."
  (let ((changed (or qq-gateway-media--order
                     (> (hash-table-count qq-gateway-media--media) 0)))
        (resync-marker qq-gateway-media--resync-request-id))
    (qq-gateway-rpc-cancel-latest
     'qq-gateway-media--refresh-owner "superseded_request"
     "Gateway media refresh context was cleared")
    (when (eq resync-marker qq-gateway-media--resync-request-id)
      (setq qq-gateway-media--resync-request-id nil))
    (setq qq-gateway-media--media (make-hash-table :test #'equal)
          qq-gateway-media--order nil
          qq-gateway-media--playable-resources (make-hash-table :test #'equal))
    (when changed
      (qq-gateway--run-hook 'qq-gateway-media-changed-hook reason nil))))

(defun qq-gateway-media-reset ()
  "Forget client remote-media projection without mutating service state."
  (setq qq-gateway-media--gateway-instance-id nil)
  (qq-gateway-media--clear 'reset))

(defun qq-gateway-media--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same remote media."
  (cl-every (lambda (key)
              (equal (alist-get key left) (alist-get key right)))
            '(media_id account_id message_id segment_index kind created_at)))

(defun qq-gateway-media--upsert (snapshot reason)
  "Merge SNAPSHOT into the remote-media projection for REASON."
  (let* ((media-id (alist-get 'media_id snapshot))
         (existing (gethash media-id qq-gateway-media--media)))
    (when (and existing
               (not (qq-gateway-media--same-identity-p existing snapshot)))
      (error "qq: Gateway remote-media identity changed for %s" media-id))
    (when (and existing
               (< (alist-get 'updated_at snapshot)
                  (alist-get 'updated_at existing)))
      (setq snapshot existing))
    (unless existing
      (setq qq-gateway-media--order
            (append qq-gateway-media--order (list media-id))))
    (unless (equal snapshot existing)
      (puthash media-id snapshot qq-gateway-media--media)
      (qq-gateway--run-hook 'qq-gateway-media-changed-hook reason media-id))
    (qq-gateway-value-copy snapshot)))

(defun qq-gateway-media--replace (snapshots reason)
  "Atomically replace projected media with SNAPSHOTS for REASON."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((media-id (alist-get 'media_id snapshot)))
        (when (gethash media-id next)
          (error "qq: Gateway media list duplicates %s" media-id))
        (puthash media-id snapshot next)
        (push media-id order)))
    (setq qq-gateway-media--media next
          qq-gateway-media--order (nreverse order))
    (qq-gateway--run-hook 'qq-gateway-media-changed-hook reason nil)
    (qq-gateway-media-list)))

(defun qq-gateway-media--remove (media-id reason)
  "Remove opaque MEDIA-ID from projection for REASON."
  (when (gethash media-id qq-gateway-media--media)
    (remhash media-id qq-gateway-media--media)
    (remhash media-id qq-gateway-media--playable-resources)
    (setq qq-gateway-media--order (delete media-id qq-gateway-media--order))
    (qq-gateway--run-hook 'qq-gateway-media-changed-hook reason media-id)
    t))

(defun qq-gateway-media-refresh (&optional callback errback reason)
  "Fetch authoritative remote-media registry.

CALLBACK receives copied snapshots; ERRBACK receives a response and reason.
REASON defaults to `resync'."
  (qq-gateway-rpc-latest-call
   'qq-gateway-media--refresh-owner "media.list" nil
   :projector
   (lambda (result)
     (qq-gateway-media--replace
      (alist-get 'media result) (or reason 'resync)))
   :callback callback
   :errback errback))

(defun qq-gateway-media-status (media-id &optional callback errback)
  "Fetch and project remote MEDIA-ID.

CALLBACK receives the projected snapshot; ERRBACK receives failure details."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-gateway-rpc-call
   "media.status" `((media_id . ,media-id))
   :projector
   (lambda (result)
     (qq-gateway-media--upsert (alist-get 'media result) 'status))
   :callback callback
   :errback errback))

(defun qq-gateway-media-materialize (media-id &optional callback errback)
  "Start materializing remote MEDIA-ID without waiting for its download.

CALLBACK receives the current media snapshot, normally in `materializing'
phase.  Progress and completion arrive through `media.changed'.  ERRBACK
receives failure details."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-gateway-rpc-call
   "media.materialize" `((media_id . ,media-id))
   :projector
   (lambda (result)
     (qq-gateway-media--upsert
      (alist-get 'media result) 'materialize-response))
   :callback callback
   :errback errback))

(defun qq-gateway-media-cancel (media-id &optional callback errback)
  "Idempotently cancel active materialization of remote MEDIA-ID.

Completed media stays materialized.  An active operation returns to
`available', preserving the reusable remote handle."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-gateway-rpc-call
   "media.cancel" `((media_id . ,media-id))
   :projector
   (lambda (result)
     (qq-gateway-media--upsert
      (alist-get 'media result) 'cancel-response))
   :callback callback
   :errback errback))

(defun qq-gateway-media-await-materialized (media-id callback errback)
  "Observe MEDIA-ID until it materializes or reaches another terminal state.

Return a `qq-gateway-watch' that removes only this local observer.  CALLBACK
receives the materialized media snapshot."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (let (observer watch)
    (setq watch
          (qq-gateway-watch-create
           :active-p t
           :cancel-function
           (lambda ()
             (remove-hook 'qq-gateway-media-changed-hook observer))))
    (setq observer
          (lambda (_reason changed-id)
            (when (and (qq-gateway-watch-active-p watch)
                       (or (null changed-id) (equal changed-id media-id)))
              (let ((media (qq-gateway-media media-id)))
                (cond
                 ((null media)
                  (qq-gateway-watch-cancel watch)
                  (qq-gateway--client-error
                   errback "media_disappeared" "Remote media disappeared"))
                 ((equal (alist-get 'phase media) "materialized")
                  (qq-gateway-watch-cancel watch)
                  (qq-gateway--invoke callback media))
                 ((equal (alist-get 'phase media) "failed")
                  (qq-gateway-watch-cancel watch)
                  (let ((problem (alist-get 'error media)))
                    (qq-gateway--client-error
                     errback
                     (or (alist-get 'code problem) "media_materialize_failed")
                     "%s"
                     (or (alist-get 'message problem)
                         "Remote media materialization failed"))))
                 ((equal (alist-get 'phase media) "available")
                  (qq-gateway-watch-cancel watch)
                  (qq-gateway--client-error
                   errback "media_materialize_canceled"
                   "Remote media materialization was canceled")))))))
    (add-hook 'qq-gateway-media-changed-hook observer)
    (funcall observer 'initial media-id)
    watch))

(defun qq-gateway-media-release (media-id &optional callback errback)
  "Idempotently release remote MEDIA-ID without releasing its resources.

CALLBACK receives the release receipt; ERRBACK receives failure details."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Media ID must be an opaque media- UUID"))
  (qq-gateway-rpc-call
   "media.release" `((media_id . ,media-id))
   :callback callback
   :errback errback))

(defun qq-gateway-media--cancel-operation-local (operation)
  "Cancel local request and observer ownership held by OPERATION."
  (when-let* ((request-id (qq-gateway-media-operation-request-id operation)))
    (qq-gateway-transport-cancel request-id)
    (setf (qq-gateway-media-operation-request-id operation) nil))
  (when-let* ((watch (qq-gateway-media-operation-watch operation)))
    (qq-gateway-watch-cancel watch)
    (setf (qq-gateway-media-operation-watch operation) nil)))

(defun qq-gateway-media-cancel-operation (operation)
  "Cancel playback-preparation OPERATION locally and in the service."
  (when (and (qq-gateway-media-operation-p operation)
             (qq-gateway-media-operation-active-p operation))
    (setf (qq-gateway-media-operation-active-p operation) nil)
    (qq-gateway-media--cancel-operation-local operation)
    (when (qq-gateway--method-available-p "media.cancel")
      (condition-case error-data
          (qq-gateway-media-cancel
           (qq-gateway-media-operation-media-id operation)
           nil
           (lambda (_body failure)
             (message "qq: Media cancellation failed: %s" failure)))
        (error
         (message "qq: Media cancellation failed: %s"
                  (error-message-string error-data)))))
    t))

(defun qq-gateway-media--account-current-p (operation)
  "Return non-nil when OPERATION's stable account still exists."
  (and (qq-gateway-account
        (qq-gateway-media-operation-account-id operation))
       t))

(defun qq-gateway-media-prepare-record-playback
    (media-id callback &optional errback)
  "Prepare local PCM WAV access for remote record MEDIA-ID.

The service materializes native Silk, derives a separate playback WAV, then
issues a revocable local access grant.  CALLBACK receives an alist containing
`media_id', `source_resource_id', `playback_resource_id', and `access'.
ERRBACK receives a response body and human-readable failure reason.
Return a cancellable `qq-gateway-media-operation'."
  (unless (qq-gateway-media--id-p media-id)
    (user-error "qq: Record media ID must be an opaque media- UUID"))
  (let* ((account-id (qq-runtime-current-account-id))
         (operation
          (qq-gateway-media-operation-create
           :active-p t :media-id media-id :account-id account-id)))
    (unless account-id
      (user-error "qq: Select an account before playing a record"))
    (cl-labels
        ((fail
          (body reason)
          (when (qq-gateway-media-operation-active-p operation)
            (setf (qq-gateway-media-operation-active-p operation) nil)
            (qq-gateway-media--cancel-operation-local operation)
            (qq-gateway--invoke errback body reason)))
         (ensure-account
          ()
          (if (qq-gateway-media--account-current-p operation)
              t
            (fail '((code . "account_removed")
                    (message . "QQ account was removed during record playback preparation"))
                  "QQ account was removed during record playback preparation")
            nil))
         (start-request
          (thunk)
          (when (and (qq-gateway-media-operation-active-p operation)
                     (ensure-account))
            (condition-case error-data
                (let ((request-id (funcall thunk)))
                  (when (and request-id
                             (qq-gateway-media-operation-active-p operation))
                    (setf
                     (qq-gateway-media-operation-request-id operation)
                     request-id)))
              (error (fail nil (error-message-string error-data))))))
         (await-resource
          (resource-id ready-callback)
          (let ((watch
                 (qq-gateway-resource-await-ready
                  resource-id ready-callback #'fail)))
            (when (qq-gateway-watch-active-p watch)
              (setf (qq-gateway-media-operation-watch operation) watch))))
         (handoff-access
          (access)
          (if (not (and (qq-gateway-media-operation-active-p operation)
                        (ensure-account)))
              (qq-gateway-resource-close-local (alist-get 'access_id access))
            (setf (qq-gateway-media-operation-active-p operation) nil
                  (qq-gateway-media-operation-request-id operation) nil)
            (let ((result
                   `((media_id . ,media-id)
                     (source_resource_id
                      . ,(qq-gateway-media-operation-source-resource-id operation))
                     (playback_resource_id
                      . ,(qq-gateway-media-operation-playback-resource-id operation))
                     (access . ,access))))
              (if (not callback)
                  (qq-gateway-resource-close-local
                   (alist-get 'access_id access))
                (condition-case error-data
                    (funcall callback result)
                  (error
                   (qq-gateway-resource-close-local
                    (alist-get 'access_id access))
                   (message "qq: record playback callback failed: %s"
                            (error-message-string error-data))))))))
         (open-playable
          (resource)
          (when (and (qq-gateway-media-operation-active-p operation)
                     (ensure-account))
            (setf (qq-gateway-media-operation-watch operation) nil
                  (qq-gateway-media-operation-playback-resource-id operation)
                  (alist-get 'resource_id resource))
            (puthash media-id (alist-get 'resource_id resource)
                     qq-gateway-media--playable-resources)
            (start-request
             (lambda ()
               (qq-gateway-resource-open-local
                (alist-get 'resource_id resource) #'handoff-access #'fail)))))
         (await-playable
          (resource)
          (let ((resource-id (alist-get 'resource_id resource)))
            (setf (qq-gateway-media-operation-request-id operation) nil
                  (qq-gateway-media-operation-playback-resource-id operation)
                  resource-id)
            (puthash media-id resource-id qq-gateway-media--playable-resources)
            (await-resource resource-id #'open-playable)))
         (derive-playable
         (source)
          (start-request
           (lambda ()
             (qq-gateway-resource-derive-playable-record
              (alist-get 'resource_id source) nil #'await-playable #'fail))))
         (source-ready
          (source)
          (when (and (qq-gateway-media-operation-active-p operation)
                     (ensure-account))
            (setf (qq-gateway-media-operation-watch operation) nil)
            (let* ((cached-id (gethash media-id
                                       qq-gateway-media--playable-resources))
                   (cached (and cached-id (qq-gateway-resource cached-id))))
              (pcase (and cached (alist-get 'phase cached))
                ("ready" (open-playable cached))
                ("staging" (await-resource cached-id #'open-playable))
                (_
                 (remhash media-id qq-gateway-media--playable-resources)
                 (derive-playable source))))))
         (source-status
          (resource)
          (when (qq-gateway-media-operation-active-p operation)
            (setf (qq-gateway-media-operation-request-id operation) nil
                  (qq-gateway-media-operation-source-resource-id operation)
                  (alist-get 'resource_id resource))
            (await-resource (alist-get 'resource_id resource) #'source-ready)))
         (materialized
          (media)
          (when (qq-gateway-media-operation-active-p operation)
            (setf (qq-gateway-media-operation-watch operation) nil)
            (let ((account-id (alist-get 'account_id media))
                  (resource-id (alist-get 'resource_id media)))
              (if (not (and (ensure-account)
                            (equal account-id
                                   (qq-gateway-media-operation-account-id
                                    operation))))
                  (when (qq-gateway-media-operation-active-p operation)
                    (fail nil "Remote record belongs to another managed account"))
                (start-request
                 (lambda ()
                   (qq-gateway-resource-status
                    resource-id #'source-status #'fail)))))))
         (await-media
          ()
          (let ((watch
                 (qq-gateway-media-await-materialized
                  media-id #'materialized #'fail)))
            (when (qq-gateway-watch-active-p watch)
              (setf (qq-gateway-media-operation-watch operation) watch))))
         (materialize-started
          (_media)
          (when (qq-gateway-media-operation-active-p operation)
            (setf (qq-gateway-media-operation-request-id operation) nil)
            (await-media))))
      (start-request
       (lambda ()
         (qq-gateway-media-materialize
          media-id #'materialize-started #'fail)))
      operation)))

(defun qq-gateway-media--drop-stale-playable-resources (_reason resource-id)
  "Forget cached playable mappings affected by RESOURCE-ID."
  (let (stale-media-ids)
    (maphash
     (lambda (media-id cached-id)
       (when (and (or (null resource-id) (equal resource-id cached-id))
                  (let ((resource (qq-gateway-resource cached-id)))
                    (or (null resource)
                        (member (alist-get 'phase resource)
                                '("failed" "released")))))
         (push media-id stale-media-ids)))
     qq-gateway-media--playable-resources)
    (dolist (media-id stale-media-ids)
      (remhash media-id qq-gateway-media--playable-resources))))

(defun qq-gateway-media--request-resync (reason)
  "Request one authoritative remote-media refresh for REASON."
  (qq-gateway-rpc-request-single-flight
   'qq-gateway-media--resync-request-id 'media-resync
   (lambda (success failure)
     (qq-gateway-media-refresh success failure reason))
   "media"))

(defun qq-gateway-media--handle-ready (instance-id)
  "Synchronize remote media after Gateway ready INSTANCE-ID."
  (unless (equal instance-id qq-gateway-media--gateway-instance-id)
    (qq-gateway-media--clear 'gateway-changed))
  (setq qq-gateway-media--gateway-instance-id
        (qq-gateway-value-copy instance-id))
  (if (qq-gateway--method-available-p "media.list")
      (qq-gateway-media--request-resync 'ready)
    (qq-gateway-media--clear 'capability-unavailable)))

(defun qq-gateway-media--handle-event (event data)
  "Project native remote-media EVENT with DATA."
  (pcase event
    ("media.changed"
     (qq-gateway-media--upsert (alist-get 'media data) 'changed))
    ("media.removed"
     (qq-gateway-media--remove (alist-get 'media_id data) 'removed))
    (_ (error "qq: Unowned Gateway media event %s" event))))

(defun qq-gateway-media--handle-protocol-error (body)
  "Resynchronize after unsolicited remote-media stream error BODY."
  (when (equal (alist-get 'code body) "media_event_stream_lagged")
    (qq-gateway--run-hook 'qq-gateway-media-desync-hook body)
    (qq-gateway-media--request-resync 'resync)))

(add-hook 'qq-gateway-resource-changed-hook
          #'qq-gateway-media--drop-stale-playable-resources)
(add-hook 'qq-gateway-ready-hook #'qq-gateway-media--handle-ready)
(dolist (event '("media.changed" "media.removed"))
  (qq-gateway-dispatch-register-event event #'qq-gateway-media--handle-event))
(qq-gateway-dispatch-register-error
 "media_event_stream_lagged" #'qq-gateway-media--handle-protocol-error)

(provide 'qq-gateway-media)

;;; qq-gateway-media.el ends here
