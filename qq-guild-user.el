;;; qq-guild-user.el --- Native QQ channel member profiles -*- lexical-binding: t; -*-

;;; Commentary:

;; QQ channel tinyIds form an identity domain independent from QQ UINs.  This
;; view intentionally exposes only fields returned by GPro simple profiles.

;;; Code:

(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'qq-api)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-view)

(declare-function qq-api-cancel-request "qq-api" (request-token))

(defvar-local qq-guild-user--guild-id nil)
(defvar-local qq-guild-user--native-id nil)
(defvar-local qq-guild-user--profile nil)
(defvar-local qq-guild-user--request nil)
(defvar-local qq-guild-user--request-owner nil)
(defvar-local qq-guild-user--loading nil)
(defvar-local qq-guild-user--error nil)
(defvar-local qq-guild-user--media-hook nil
  "View-owned media cache hook installed for this member buffer.")

(defconst qq-guild-user--position-property 'qq-guild-user-position-key
  "Text property carrying stable member-page position keys.")

(defface qq-guild-user-card-title
  '((t :inherit bold :height 1.15))
  "Face used for QQ channel member display names."
  :group 'qq)

(defface qq-guild-user-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face used for QQ channel member page actions."
  :group 'qq)

(defun qq-guild-user--view-id (guild-id native-id)
  "Return the Appkit identity for NATIVE-ID in GUILD-ID."
  (list 'guild-user guild-id native-id))

(defun qq-guild-user--buffer-name (guild-id native-id)
  "Return the fallback buffer name for NATIVE-ID in GUILD-ID."
  (format "*qq-guild-user:%s:%s*" guild-id native-id))

(defun qq-guild-user--position-key (kind &optional value)
  "Return a stable current-member position key for KIND and VALUE."
  (list 'guild-user qq-guild-user--guild-id
        qq-guild-user--native-id kind value))

(defun qq-guild-user--mark-position (start key)
  "Mark text from START through point with stable position KEY."
  (add-text-properties
   start (point)
   (list qq-guild-user--position-property key 'rear-nonsticky t)))

(defun qq-guild-user--avatar-key ()
  "Return the exact avatar cache key for the current member."
  (qq-media--guild-member-avatar-key
   qq-guild-user--guild-id qq-guild-user--native-id))

(defun qq-guild-user--display-name ()
  "Return the authoritative current member display name."
  (or (alist-get 'display_name qq-guild-user--profile)
      qq-guild-user--native-id
      "QQ channel member"))

(defun qq-guild-user--guild-name ()
  "Return the current Guild's directory name when available."
  (or (alist-get 'name (qq-state-guild qq-guild-user--guild-id))
      qq-guild-user--guild-id))

(defun qq-guild-user--header-line ()
  "Return the dynamic QQ channel member header."
  (format " QQ频道成员 · %s · %s%s"
          (qq-guild-user--guild-name)
          (qq-guild-user--display-name)
          (if qq-guild-user--loading " · loading" "")))

(defun qq-guild-user--insert-field (label value)
  "Insert LABEL and non-empty VALUE as one profile row."
  (when (and value (not (equal value "")))
    (let ((row-start (point))
          (label-start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties label-start (point) '(face bold))
      (insert (format "%s\n" value))
      (qq-guild-user--mark-position
       row-start (qq-guild-user--position-key 'field label)))))

(defun qq-guild-user--avatar-display-string ()
  "Return the current member avatar display string."
  (if-let* ((url (alist-get 'avatar_url qq-guild-user--profile)))
      (qq-media-url-preview-display-string
       (qq-guild-user--avatar-key) url "@" qq-media-avatar-image-height)
    "@"))

(defun qq-guild-user-open-avatar ()
  "Open the current QQ channel member avatar."
  (interactive)
  (unless qq-guild-user--profile
    (user-error "QQ: Channel member profile is not loaded"))
  (qq-media-open-image-url
   (qq-guild-user--avatar-key)
   (alist-get 'avatar_url qq-guild-user--profile)))

(defun qq-guild-user-copy-id ()
  "Copy the current native QQ channel member tinyId."
  (interactive)
  (unless qq-guild-user--native-id
    (user-error "QQ: Channel member identity is missing"))
  (kill-new qq-guild-user--native-id)
  (message "qq: copied channel member id %s" qq-guild-user--native-id))

(defun qq-guild-user--insert-note (text key &optional face)
  "Insert member-page note TEXT with stable KEY and optional FACE."
  (appkit-view-insert-note-line
   text :face face
   :line-properties
   (list qq-guild-user--position-property
         (qq-guild-user--position-key 'note key))))

(defun qq-guild-user--render-content (view)
  "Render current member state inside live Appkit VIEW."
  (let ((snapshot
         (with-current-buffer (appkit-view-buffer view)
           (appkit-position-capture
            :anchor-property qq-guild-user--position-property
            :preserve-window-start t))))
    (appkit-with-content-update view
      (erase-buffer)
      (cond
       ((and qq-guild-user--loading (null qq-guild-user--profile))
        (qq-guild-user--insert-note
         "Loading channel member profile…" 'loading))
       ((and qq-guild-user--error (null qq-guild-user--profile))
        (qq-guild-user--insert-note qq-guild-user--error 'error 'error))
       ((null qq-guild-user--profile)
        (qq-guild-user--insert-note
         "No channel member profile loaded." 'empty))
       (t
        (let ((summary-start (point))
              (avatar-start (point)))
          (insert (qq-guild-user--avatar-display-string))
          (make-text-button
           avatar-start (point)
           'follow-link t
           'action (lambda (_button) (qq-guild-user-open-avatar))
           'help-echo "查看频道头像")
          (insert "  "
                  (propertize (qq-guild-user--display-name)
                              'face 'qq-guild-user-card-title)
                  "\n\n")
          (qq-guild-user--mark-position
           summary-start (qq-guild-user--position-key 'summary)))
        (let ((actions-start (point)))
          (appkit-ui-insert-action-button
           " 查看头像 " #'qq-guild-user-open-avatar
           :face 'qq-guild-user-action-button :help-echo "查看头像 (a)")
          (insert "  ")
          (appkit-ui-insert-action-button
           " 复制频道 ID " #'qq-guild-user-copy-id
           :face 'qq-guild-user-action-button :help-echo "复制频道 tinyId (w)")
          (insert "\n")
          (qq-guild-user--mark-position
           actions-start (qq-guild-user--position-key 'actions)))
        (when qq-guild-user--error
          (qq-guild-user--insert-note qq-guild-user--error 'error 'error))
        (qq-guild-user--insert-note
         "g 刷新 · a 头像 · w 复制 · q 退出" 'instructions)
        (insert "\n")
        (appkit-view-insert-heading-line
         "频道身份" :face 'bold
         :line-properties
         (list qq-guild-user--position-property
               (qq-guild-user--position-key 'heading 'identity)))
        (qq-guild-user--insert-field "频道" (qq-guild-user--guild-name))
        (qq-guild-user--insert-field
         "显示名" (alist-get 'display_name qq-guild-user--profile))
        (let ((member-name (alist-get 'member_name qq-guild-user--profile))
              (nickname (alist-get 'nickname qq-guild-user--profile)))
          (unless (equal member-name
                         (alist-get 'display_name qq-guild-user--profile))
            (qq-guild-user--insert-field "成员名" member-name))
          (unless (or (equal nickname member-name)
                      (equal nickname
                             (alist-get 'display_name qq-guild-user--profile)))
            (qq-guild-user--insert-field "昵称" nickname)))
        (qq-guild-user--insert-field "频道 ID" qq-guild-user--native-id)
        (insert "\n")
        (qq-guild-user--insert-note
         "频道 ID 是 GPro tinyId，不是 QQ 号。" 'identity-domain)))
      (force-mode-line-update)
      (when snapshot
        (appkit-position-restore snapshot)))))

(defun qq-guild-user--sync-invalidations (view invalidations)
  "Synchronize member VIEW from coalesced INVALIDATIONS."
  (when (and (appkit-view-live-p view)
             (appkit-invalidations-any-p invalidations))
    (qq-guild-user--render-content view)))

(cl-defun qq-guild-user--request-sync (&optional view &key resource)
  "Request one atomic member-page sync for live VIEW.

RESOURCE identifies a presentation-only avatar update."
  (when-let* ((view (or view (qq-guild-user--live-current-view))))
    (if resource
        (appkit-request-sync view :part 'profile :resource resource)
      (appkit-request-sync view :structure t :part 'profile))))

(defun qq-guild-user--sync-now (view)
  "Consume pending member-page invalidations for live VIEW immediately."
  (when (appkit-view-live-p view)
    (appkit-sync-invalidations view)))

(defun qq-guild-user-render ()
  "Request and immediately synchronize the current member page."
  (interactive)
  (let ((view (qq-guild-user--ensure-view)))
    (qq-guild-user--request-sync view)
    (qq-guild-user--sync-now view)))

(defun qq-guild-user--request-current-p
    (view buffer guild-id native-id owner)
  "Return non-nil when VIEW and OWNER load GUILD-ID and NATIVE-ID in BUFFER."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-guild-user-mode)
              (eq view (appkit-current-view))
              (equal qq-guild-user--guild-id guild-id)
              (equal qq-guild-user--native-id native-id)
              (eq qq-guild-user--request-owner owner)))))

(defun qq-guild-user-refresh ()
  "Refresh the current native QQ channel member profile."
  (interactive)
  (unless (and qq-guild-user--guild-id qq-guild-user--native-id)
    (user-error "QQ: Channel member identity is missing"))
  (let ((view (qq-guild-user--ensure-view)))
    (let ((buffer (current-buffer))
          (guild-id qq-guild-user--guild-id)
          (native-id qq-guild-user--native-id)
          (old-request qq-guild-user--request)
          (owner (list 'guild-user-profile
                       qq-guild-user--guild-id qq-guild-user--native-id)))
      (setq qq-guild-user--loading t
            qq-guild-user--error nil
            qq-guild-user--request nil
            qq-guild-user--request-owner owner)
      (when old-request
        (qq-api-cancel-request old-request))
      (qq-guild-user--request-sync view)
      (condition-case error-data
          (let ((request
                 (qq-api-get-guild-member-profile
                  guild-id native-id
                  (lambda (profile)
                    (when (qq-guild-user--request-current-p
                           view buffer guild-id native-id owner)
                      (with-current-buffer buffer
                        (setq qq-guild-user--profile profile
                              qq-guild-user--loading nil
                              qq-guild-user--error nil
                              qq-guild-user--request nil
                              qq-guild-user--request-owner nil)
                        (qq-guild-user--request-sync view))))
                  (lambda (_response reason)
                    (when (qq-guild-user--request-current-p
                           view buffer guild-id native-id owner)
                      (with-current-buffer buffer
                        (setq qq-guild-user--loading nil
                              qq-guild-user--error
                              (format "Unable to load channel member: %s"
                                      (or reason "unknown error"))
                              qq-guild-user--request nil
                              qq-guild-user--request-owner nil)
                        (qq-guild-user--request-sync view)))))))
            (when (eq qq-guild-user--request-owner owner)
              (setq qq-guild-user--request request)))
        (error
         (when (qq-guild-user--request-current-p
                view buffer guild-id native-id owner)
           (with-current-buffer buffer
             (setq qq-guild-user--loading nil
                   qq-guild-user--error
                   (format "Unable to load channel member: %s"
                           (error-message-string error-data))
                   qq-guild-user--request nil
                   qq-guild-user--request-owner nil)
             (qq-guild-user--request-sync view)))))
      (qq-guild-user--sync-now view))))

(defun qq-guild-user--cancel-request ()
  "Cancel the active channel member profile request."
  (let ((request qq-guild-user--request))
    (setq qq-guild-user--request nil
          qq-guild-user--request-owner nil
          qq-guild-user--loading nil)
    (when request
      (qq-api-cancel-request request))))

(defun qq-guild-user--live-current-view ()
  "Return this buffer's live canonical member view, or nil."
  (let ((view (appkit-current-view)))
    (and qq-guild-user--guild-id
         qq-guild-user--native-id
         (derived-mode-p 'qq-guild-user-mode)
         (appkit-view-live-p view)
         (eq (appkit-view-buffer view) (current-buffer))
         (equal (appkit-view-id view)
                (qq-guild-user--view-id
                 qq-guild-user--guild-id qq-guild-user--native-id))
         view)))

(defun qq-guild-user--reset-buffer-work (buffer)
  "Cancel and clear member work and data still retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-guild-user-mode)
        (qq-guild-user--cancel-request)
        (when qq-guild-user--media-hook
          (remove-hook 'qq-media-cache-update-hook
                       qq-guild-user--media-hook)
          (setq qq-guild-user--media-hook nil))
        (setq qq-guild-user--profile nil
              qq-guild-user--error nil)))))

(defun qq-guild-user--release-view-work (view buffer)
  "Release member work in BUFFER when it is still owned by VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq (appkit-current-view) view)))
    (qq-guild-user--reset-buffer-work buffer)))

(defun qq-guild-user--handle-media-cache-update (view media-key)
  "Request a member avatar update for live VIEW after MEDIA-KEY changes."
  (when (and (stringp media-key)
             (appkit-view-live-p view))
    (with-current-buffer (appkit-view-buffer view)
      (when (and (eq view (qq-guild-user--live-current-view))
                 (equal media-key (qq-guild-user--avatar-key)))
        (qq-guild-user--request-sync view :resource media-key)))))

(defun qq-guild-user--setup-view (view)
  "Reset retained state and register lifecycle work for new VIEW."
  (let* ((buffer (appkit-view-buffer view))
         (media-hook
          (apply-partially
           #'qq-guild-user--handle-media-cache-update view)))
    (qq-guild-user--reset-buffer-work buffer)
    (with-current-buffer buffer
      (setq-local qq-guild-user--media-hook media-hook))
    (add-hook 'qq-media-cache-update-hook media-hook)
    (appkit-register-handle
     view 'hook
     (list 'qq-media-cache-update-hook media-hook nil buffer))
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-guild-user--release-view-work view buffer))))

(defun qq-guild-user--ensure-view ()
  "Return the live Appkit view owning the current member buffer."
  (unless (and qq-guild-user--guild-id qq-guild-user--native-id)
    (error "QQ: Cannot attach a member view without its native identity"))
  (let* ((app (qq-runtime-app))
         (view-id
          (qq-guild-user--view-id
           qq-guild-user--guild-id qq-guild-user--native-id))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal view-id (appkit-view-id current)))
      (setf (appkit-view-state current)
            (list qq-guild-user--guild-id qq-guild-user--native-id)
            (appkit-view-sync-function current)
            #'qq-guild-user--sync-invalidations
            (appkit-view-parts current) '(profile))
     current)
     ((appkit-view-live-p current)
      (error "QQ: Member buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id view-id
              :state (list qq-guild-user--guild-id
                           qq-guild-user--native-id)
              :mode 'qq-guild-user-mode
              :sync-function #'qq-guild-user--sync-invalidations
              :parts '(profile))))
        (qq-guild-user--setup-view view)
        view)))))

(defun qq-guild-user-button-backward ()
  "Move point to the previous member page button."
  (interactive)
  (forward-button -1))

(defvar qq-guild-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-guild-user-refresh)
    (define-key map (kbd "a") #'qq-guild-user-open-avatar)
    (define-key map (kbd "w") #'qq-guild-user-copy-id)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-guild-user-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-guild-user-mode'.")

(define-derived-mode qq-guild-user-mode special-mode "QQ-Guild-User"
  "Major mode for one native QQ channel member profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-guild-user--header-line)))
  (add-hook 'change-major-mode-hook #'qq-guild-user--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-guild-user--cancel-request nil t))

;;;###autoload
(defun qq-guild-user-open (guild-id native-id)
  "Open the native channel member NATIVE-ID in GUILD-ID."
  (unless (qq-protocol--nonzero-decimal-string-p guild-id)
    (user-error "QQ: Channel member page requires a native Guild id"))
  (unless (qq-protocol--nonzero-decimal-string-p native-id)
    (user-error "QQ: Channel member page requires a native tinyId"))
  (let* ((app (qq-runtime-app))
         (view-id (qq-guild-user--view-id guild-id native-id))
         (fresh-p (null (appkit-view-for-id app view-id)))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :state (list guild-id native-id)
           :mode 'qq-guild-user-mode
           :buffer-name (qq-guild-user--buffer-name guild-id native-id)
           :sync-function #'qq-guild-user--sync-invalidations
           :parts '(profile)
           :setup #'qq-guild-user--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq qq-guild-user--guild-id guild-id
            qq-guild-user--native-id native-id)
      (if fresh-p
          (qq-guild-user-refresh)
        (qq-guild-user--request-sync view)
        (qq-guild-user--sync-now view)))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-guild-user)

;;; qq-guild-user.el ends here
