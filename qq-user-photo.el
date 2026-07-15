;;; qq-user-photo.el --- Native QQ photo-wall buffers -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Dedicated photo-wall view backed only by Linux QQ native profile data.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'ewoc)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-media)
(require 'qq-runtime)

(declare-function qq-api-cancel-request "qq-api" (request-token))

(cl-defstruct (qq-user-photo--entry
               (:constructor qq-user-photo--entry-create))
  key
  type
  object
  index
  text)

(defvar-local qq-user-photo--ewoc nil
  "Persistent keyed EWOC used by this photo-wall view.")

(defvar-local qq-user-photo--node-table nil
  "Stable photo key to EWOC node table.")

(defvar-local qq-user-photo--user-id nil
  "QQ number whose photo wall is displayed.")

(defvar-local qq-user-photo--display-name nil
  "Display name used by the current photo-wall buffer.")

(defvar-local qq-user-photo--photos nil
  "Native photo-wall entries displayed by the current buffer.")

(defvar-local qq-user-photo--request nil
  "Active photo-wall request token.")

(defvar-local qq-user-photo--request-owner nil
  "Owner object for the active photo-wall request.")

(defvar-local qq-user-photo--loading nil
  "Non-nil while native photo-wall data is loading.")

(defvar-local qq-user-photo--error nil
  "Last photo-wall loading error, or nil.")

(defun qq-user-photo--view-id (user-id)
  "Return Appkit view identity for USER-ID's photo wall."
  (list 'user-photo user-id))

(defun qq-user-photo--photo-key (photo)
  "Return stable presentation key for PHOTO in the current buffer."
  (list 'photo qq-user-photo--user-id (alist-get 'id photo)))

(defun qq-user-photo--note-entry (key text &optional type)
  "Return a status entry identified by KEY with TEXT and optional TYPE."
  (qq-user-photo--entry-create
   :key (cons 'note key) :type (or type 'note) :text text))

(defun qq-user-photo--project-entries ()
  "Project photo-wall state into stable keyed presentation entries."
  (let (entries)
    (when qq-user-photo--loading
      (push (qq-user-photo--note-entry
             'loading
             (if qq-user-photo--photos
                 "Refreshing photo wall…"
               "Loading photo wall…"))
            entries))
    (when qq-user-photo--error
      (push (qq-user-photo--note-entry
             'error qq-user-photo--error 'error-note)
            entries))
    (cl-loop for photo in qq-user-photo--photos
             for index from 1
             do (push (qq-user-photo--entry-create
                       :key (qq-user-photo--photo-key photo)
                       :type 'photo
                       :object photo
                       :index index)
                      entries))
    (unless (or qq-user-photo--loading
                qq-user-photo--error
                qq-user-photo--photos)
      (push (qq-user-photo--note-entry 'empty "No public photos.") entries))
    (nreverse entries)))

(defun qq-user-photo--buffer-name (user-id)
  "Return photo-wall buffer name for USER-ID."
  (format "*qq-photos:%s*" user-id))

(defun qq-user-photo--header-line ()
  "Return dynamic header line for a photo-wall buffer."
  (format " QQ Photos · %s (%s)%s"
          (or qq-user-photo--display-name qq-user-photo--user-id "unknown")
          (or qq-user-photo--user-id "unknown")
          (if qq-user-photo--loading " · loading" "")))

(defun qq-user-photo-url (photo)
  "Return best exact native URL from PHOTO."
  (or (alist-get 'original_url photo)
      (alist-get 'thumbnail_url photo)))

(defun qq-user-photo-preview-url (photo)
  "Return best preview URL from PHOTO."
  (or (alist-get 'thumbnail_url photo)
      (alist-get 'original_url photo)))

(defun qq-user-photo-cache-key (user-id photo)
  "Return stable media cache key for USER-ID and PHOTO."
  (format "photo-wall:%s:%s:%s"
          user-id
          (alist-get 'id photo)
          (md5 (or (qq-user-photo-preview-url photo) ""))))

(defun qq-user-photo-preview-display-string (user-id photo fallback)
  "Return inline preview for USER-ID PHOTO, or FALLBACK while loading."
  (qq-media-url-preview-display-string
   (qq-user-photo-cache-key user-id photo)
   (qq-user-photo-preview-url photo)
   fallback))

(defun qq-user-photo-open-entry (user-id photo)
  "Open USER-ID's native PHOTO entry."
  (qq-media-open-image-url
   (qq-user-photo-cache-key user-id photo)
   (qq-user-photo-url photo)))

(defun qq-user-photo--button-action (button)
  "Open native photo represented by BUTTON."
  (qq-user-photo-open-entry
   (button-get button 'qq-user-id)
   (button-get button 'qq-user-photo)))

(defun qq-user-photo-make-button (start end user-id photo)
  "Turn START to END into a photo button for USER-ID and PHOTO."
  (make-text-button
   start end
   'follow-link t
   'action #'qq-user-photo--button-action
   'help-echo "查看照片"
   'face 'default
   'mouse-face 'highlight
   'qq-user-id user-id
   'qq-user-photo photo
   'rear-nonsticky '(qq-user-id qq-user-photo mouse-face)))

(defun qq-user-photo--photo-at-point ()
  "Return native photo entry at point, or nil."
  (or (get-text-property (point) 'qq-user-photo)
      (get-text-property (line-beginning-position) 'qq-user-photo)))

(defun qq-user-photo--insert-photo (entry)
  "Insert one projected photo ENTRY."
  (let* ((photo (qq-user-photo--entry-object entry))
         (index (qq-user-photo--entry-index entry))
         (start (point))
         (preview-url (qq-user-photo-preview-url photo))
         (cache-key (qq-user-photo-cache-key qq-user-photo--user-id photo)))
    (insert
     (qq-media-url-preview-display-string
      cache-key preview-url (format "Photo %d" index))
     "\n\n")
    (qq-user-photo-make-button start (point) qq-user-photo--user-id photo)
    (add-text-properties
     start (point)
     (list 'qq-user-photo-key (qq-user-photo--entry-key entry)))))

(defun qq-user-photo--ewoc-printer (entry)
  "Insert one projected photo-wall ENTRY."
  (pcase (qq-user-photo--entry-type entry)
    ('photo (qq-user-photo--insert-photo entry))
    ((or 'note 'error-note)
     (appkit-view-insert-note-line
      (or (qq-user-photo--entry-text entry) "")
      :face (if (eq (qq-user-photo--entry-type entry) 'error-note)
                'error
              'shadow)
      :line-properties
      (list 'qq-user-photo-key (qq-user-photo--entry-key entry))))
    (type (error "qq: unknown photo-wall entry type %S" type))))

(defun qq-user-photo--live-current-view ()
  "Return this buffer's live photo-wall view, or nil."
  (let ((view (appkit-current-view)))
    (and (derived-mode-p 'qq-user-photo-mode)
         (appkit-view-live-p view)
         (equal (appkit-view-id view)
                (qq-user-photo--view-id qq-user-photo--user-id))
         view)))

(defun qq-user-photo--cancel-buffer-work (buffer)
  "Cancel photo-wall work still owned by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-user-photo-mode)
        (qq-user-photo--cancel-request)))))

(defun qq-user-photo--setup-view (view)
  "Register lifecycle cleanup for newly attached VIEW."
  (appkit-register-handle
   view 'function
   (apply-partially #'qq-user-photo--cancel-buffer-work
                    (appkit-view-buffer view))))

(defun qq-user-photo--ensure-view ()
  "Return the live Appkit view owning the current photo-wall buffer."
  (unless qq-user-photo--user-id
    (error "QQ: cannot attach a photo-wall view without a user identity"))
  (let* ((app (qq-runtime-app))
         (view-id (qq-user-photo--view-id qq-user-photo--user-id))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-user-photo--sync-invalidations
            (appkit-view-parts current) '(photos))
      current)
     ((appkit-view-live-p current)
      (error "QQ: photo-wall buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id view-id
              :mode 'qq-user-photo-mode
              :sync-function #'qq-user-photo--sync-invalidations
              :parts '(photos))))
        (qq-user-photo--setup-view view)
        view)))))

(cl-defun qq-user-photo--request-sync
    (&optional view &key entry resource)
  "Request a coalesced photo sync for live VIEW.

ENTRY and RESOURCE identify a presentation-only media update.  Without ENTRY,
the stable-key projection is reconciled."
  (when-let* ((view (or view (qq-user-photo--live-current-view))))
    (if entry
        (appkit-request-sync view :entry entry :resource resource)
      (appkit-request-sync view :structure t :part 'photos))))

(defun qq-user-photo--sync-now (view)
  "Consume pending invalidations for live VIEW immediately."
  (when (appkit-view-live-p view)
    (appkit-sync-invalidations view)))

(defun qq-user-photo--sync-invalidations (view invalidations)
  "Consume coalesced photo-wall INVALIDATIONS for VIEW."
  (when (appkit-view-live-p view)
    (let* ((force-keys (appkit-invalidations-entry-keys invalidations))
           (full-p (or (appkit-invalidations-structure-p invalidations)
                       (appkit-invalidations-parts invalidations)
                       (appkit-invalidations-position-p invalidations)
                       (null qq-user-photo--ewoc)))
           (snapshot
            (with-current-buffer (appkit-view-buffer view)
              (appkit-position-capture
               :anchor-property 'qq-user-photo-key
               :preserve-window-start t))))
      (when (or full-p force-keys)
        (appkit-with-content-update view
				    (unless qq-user-photo--ewoc
				      (erase-buffer)
				      (setq qq-user-photo--ewoc
					    (ewoc-create #'qq-user-photo--ewoc-printer nil nil t)))
				    (if full-p
					(setq qq-user-photo--node-table
					      (appkit-ewoc-reconcile
					       qq-user-photo--ewoc
					       (qq-user-photo--project-entries)
					       #'qq-user-photo--entry-key
					       :force-keys force-keys))
				      (dolist (key force-keys)
					(appkit-ewoc-invalidate-key
					 qq-user-photo--ewoc qq-user-photo--node-table key)))
				    (force-mode-line-update)
				    (when snapshot
				      (appkit-position-restore snapshot)))))))

(defun qq-user-photo-open-at-point ()
  "Open the native photo-wall image at point."
  (interactive)
  (let ((photo (qq-user-photo--photo-at-point)))
    (unless photo
      (user-error "qq: no photo at point"))
    (qq-user-photo-open-entry qq-user-photo--user-id photo)))

(defun qq-user-photo--request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER still load USER-ID in BUFFER."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-photo-mode)
              (eq view (appkit-current-view))
              (equal qq-user-photo--user-id user-id)
              (eq qq-user-photo--request-owner owner)))))

(defun qq-user-photo-refresh ()
  "Refresh the current native photo wall."
  (interactive)
  (unless qq-user-photo--user-id
    (user-error "qq: this photo buffer has no user identity"))
  (let ((view (qq-user-photo--ensure-view)))
    (when qq-user-photo--request
      (qq-api-cancel-request qq-user-photo--request))
    (let ((buffer (current-buffer))
          (user-id qq-user-photo--user-id)
          (owner (list 'photo-wall qq-user-photo--user-id)))
      (setq qq-user-photo--loading t
            qq-user-photo--error nil
            qq-user-photo--request nil
            qq-user-photo--request-owner owner)
      (qq-user-photo--request-sync view)
      (condition-case error-data
          (let ((request
		 (qq-api-get-user-photo-wall
                  user-id
                  (lambda (photos)
                    (when (qq-user-photo--request-current-p
                           view buffer user-id owner)
                      (with-current-buffer buffer
			(setq qq-user-photo--photos photos
                              qq-user-photo--loading nil
                              qq-user-photo--error nil
                              qq-user-photo--request nil
                              qq-user-photo--request-owner nil)
			(qq-user-photo--request-sync view))))
                  (lambda (_response reason)
                    (when (qq-user-photo--request-current-p
                           view buffer user-id owner)
                      (with-current-buffer buffer
			(setq qq-user-photo--loading nil
                              qq-user-photo--error
                              (format "Unable to load photo wall: %s"
                                      (or reason "unknown error"))
                              qq-user-photo--request nil
                              qq-user-photo--request-owner nil)
			(qq-user-photo--request-sync view)))))))
            (when (eq qq-user-photo--request-owner owner)
              (setq qq-user-photo--request request)))
	(error
	 (when (qq-user-photo--request-current-p view buffer user-id owner)
           (with-current-buffer buffer
             (setq qq-user-photo--loading nil
                   qq-user-photo--error
                   (format "Unable to load photo wall: %s"
                           (error-message-string error-data))
                   qq-user-photo--request nil
                   qq-user-photo--request-owner nil)
             (qq-user-photo--request-sync view)))))
      (qq-user-photo--sync-now view))))

(defun qq-user-photo--cancel-request ()
  "Cancel the active photo-wall request."
  (when qq-user-photo--request
    (qq-api-cancel-request qq-user-photo--request))
  (setq qq-user-photo--request nil
        qq-user-photo--request-owner nil))

(defun qq-user-photo-button-backward ()
  "Move point to the previous photo button."
  (interactive)
  (forward-button -1))

(defvar qq-user-photo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-user-photo-refresh)
    (define-key map (kbd "RET") #'qq-user-photo-open-at-point)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-user-photo-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-user-photo-mode'.")

(define-derived-mode qq-user-photo-mode special-mode "QQ-Photos"
  "Major mode for one native QQ photo wall."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-user-photo--header-line)))
  (setq-local qq-user-photo--ewoc nil)
  (setq-local qq-user-photo--node-table nil)
  (add-hook 'change-major-mode-hook #'qq-user-photo--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-user-photo--cancel-request nil t))

(defun qq-user-photo-open (user-id display-name)
  "Open native photo wall for USER-ID titled with DISPLAY-NAME."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: photo wall requires a decimal string user id"))
  (let* ((app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id (qq-user-photo--view-id user-id)
           :mode 'qq-user-photo-mode
           :buffer-name (qq-user-photo--buffer-name user-id)
           :sync-function #'qq-user-photo--sync-invalidations
           :parts '(photos)
           :setup #'qq-user-photo--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq qq-user-photo--user-id user-id
            qq-user-photo--display-name display-name)
      (unless qq-user-photo--loading
        (qq-user-photo-refresh))
      (when qq-user-photo--loading
        (qq-user-photo--request-sync view)
        (qq-user-photo--sync-now view)))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user-photo--handle-media-cache-update (media-key)
  "Request targeted updates for photo rows affected by MEDIA-KEY."
  (when (and (stringp media-key)
             (string-prefix-p "photo-wall:" media-key))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when-let* ((view (and (derived-mode-p 'qq-user-photo-mode)
                               (qq-user-photo--live-current-view))))
          (dolist (photo qq-user-photo--photos)
            (when (equal media-key
                         (qq-user-photo-cache-key
                          qq-user-photo--user-id photo))
              (qq-user-photo--request-sync
               view
               :entry (qq-user-photo--photo-key photo)
               :resource media-key))))))))

(add-hook 'qq-media-cache-update-hook
          #'qq-user-photo--handle-media-cache-update)

(provide 'qq-user-photo)

;;; qq-user-photo.el ends here
