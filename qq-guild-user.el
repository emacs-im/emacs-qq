;;; qq-guild-user.el --- Native QQ channel member profiles -*- lexical-binding: t; -*-

;;; Commentary:

;; QQ channel tinyIds form an identity domain independent from QQ UINs.  This
;; view intentionally exposes only fields returned by GPro simple profiles.

;;; Code:

(require 'subr-x)
(require 'qq-api)
(require 'qq-media)
(require 'qq-state)
(require 'appkit-position)
(require 'appkit-ui)
(require 'appkit-view)

(declare-function qq-api-cancel-request "qq-api" (request-token))

(defconst qq-guild-user-buffer-name "*qq-guild-user*"
  "Name of the shared QQ channel member profile buffer.")

(defvar-local qq-guild-user--guild-id nil)
(defvar-local qq-guild-user--native-id nil)
(defvar-local qq-guild-user--profile nil)
(defvar-local qq-guild-user--request nil)
(defvar-local qq-guild-user--request-owner nil)
(defvar-local qq-guild-user--loading nil)
(defvar-local qq-guild-user--error nil)

(defface qq-guild-user-card-title
  '((t :inherit bold :height 1.15))
  "Face used for QQ channel member display names."
  :group 'qq)

(defface qq-guild-user-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face used for QQ channel member page actions."
  :group 'qq)

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
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (insert (format "%s\n" value))))

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
    (user-error "qq: channel member profile is not loaded"))
  (qq-media-open-image-url
   (qq-guild-user--avatar-key)
   (alist-get 'avatar_url qq-guild-user--profile)))

(defun qq-guild-user-copy-id ()
  "Copy the current native QQ channel member tinyId."
  (interactive)
  (unless qq-guild-user--native-id
    (user-error "qq: channel member identity is missing"))
  (kill-new qq-guild-user--native-id)
  (message "qq: copied channel member id %s" qq-guild-user--native-id))

(defun qq-guild-user-render ()
  "Render the current native QQ channel member profile."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-guild-user--header-line)))
       (cond
        ((and qq-guild-user--loading (null qq-guild-user--profile))
         (appkit-view-insert-note-line "Loading channel member profile…"))
        ((and qq-guild-user--error (null qq-guild-user--profile))
         (appkit-view-insert-note-line qq-guild-user--error :face 'error))
        ((null qq-guild-user--profile)
         (appkit-view-insert-note-line "No channel member profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-guild-user--avatar-display-string))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'action (lambda (_button) (qq-guild-user-open-avatar))
            'help-echo "查看频道头像")
           (insert "  "
                   (propertize (qq-guild-user--display-name)
                               'face 'qq-guild-user-card-title)
                   "\n\n"))
         (appkit-ui-insert-action-button
          " 查看头像 " #'qq-guild-user-open-avatar
          :face 'qq-guild-user-action-button :help-echo "查看头像 (a)")
         (insert "  ")
         (appkit-ui-insert-action-button
          " 复制频道 ID " #'qq-guild-user-copy-id
          :face 'qq-guild-user-action-button :help-echo "复制频道 tinyId (w)")
         (insert "\n")
         (when qq-guild-user--error
           (appkit-view-insert-note-line qq-guild-user--error :face 'error))
         (appkit-view-insert-note-line "g 刷新 · a 头像 · w 复制 · q 退出")
         (insert "\n")
         (appkit-view-insert-heading-line "频道身份" :face 'bold)
         (qq-guild-user--insert-field "频道" (qq-guild-user--guild-name))
         (qq-guild-user--insert-field "显示名"
                                      (alist-get 'display_name qq-guild-user--profile))
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
         (appkit-view-insert-note-line
          "频道 ID 是 GPro tinyId，不是 QQ 号。" :face 'shadow)))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-guild-user--request-current-p (buffer guild-id native-id owner)
  "Return non-nil when OWNER still loads the selected identity in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-guild-user-mode)
              (equal qq-guild-user--guild-id guild-id)
              (equal qq-guild-user--native-id native-id)
              (eq qq-guild-user--request-owner owner)))))

(defun qq-guild-user-refresh ()
  "Refresh the current native QQ channel member profile."
  (interactive)
  (unless (and qq-guild-user--guild-id qq-guild-user--native-id)
    (user-error "qq: channel member identity is missing"))
  (when qq-guild-user--request
    (qq-api-cancel-request qq-guild-user--request))
  (let ((buffer (current-buffer))
        (guild-id qq-guild-user--guild-id)
        (native-id qq-guild-user--native-id)
        (owner (list 'guild-user-profile
                     qq-guild-user--guild-id qq-guild-user--native-id)))
    (setq qq-guild-user--loading t
          qq-guild-user--error nil
          qq-guild-user--request nil
          qq-guild-user--request-owner owner)
    (qq-guild-user-render)
    (let ((request
           (qq-api-get-guild-member-profile
            guild-id native-id
            (lambda (profile)
              (when (qq-guild-user--request-current-p
                     buffer guild-id native-id owner)
                (with-current-buffer buffer
                  (setq qq-guild-user--profile profile
                        qq-guild-user--loading nil
                        qq-guild-user--error nil
                        qq-guild-user--request nil
                        qq-guild-user--request-owner nil)
                  (qq-guild-user-render))))
            (lambda (_response reason)
              (when (qq-guild-user--request-current-p
                     buffer guild-id native-id owner)
                (with-current-buffer buffer
                  (setq qq-guild-user--loading nil
                        qq-guild-user--error
                        (format "Unable to load channel member: %s"
                                (or reason "unknown error"))
                        qq-guild-user--request nil
                        qq-guild-user--request-owner nil)
                  (qq-guild-user-render)))))))
      (when (eq qq-guild-user--request-owner owner)
        (setq qq-guild-user--request request)))))

(defun qq-guild-user--cancel-request ()
  "Cancel the active channel member profile request."
  (when qq-guild-user--request
    (qq-api-cancel-request qq-guild-user--request))
  (setq qq-guild-user--request nil
        qq-guild-user--request-owner nil))

(defun qq-guild-user--select (guild-id native-id)
  "Prepare the shared member buffer for GUILD-ID and NATIVE-ID."
  (unless (and (equal qq-guild-user--guild-id guild-id)
               (equal qq-guild-user--native-id native-id))
    (qq-guild-user--cancel-request)
    (setq qq-guild-user--profile nil
          qq-guild-user--loading nil
          qq-guild-user--error nil))
  (setq qq-guild-user--guild-id guild-id
        qq-guild-user--native-id native-id))

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
  (add-hook 'change-major-mode-hook #'qq-guild-user--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-guild-user--cancel-request nil t))

;;;###autoload
(defun qq-guild-user-open (guild-id native-id)
  "Open the native channel member NATIVE-ID in GUILD-ID."
  (unless (qq-protocol--nonzero-decimal-string-p guild-id)
    (user-error "qq: channel member page requires a native Guild id"))
  (unless (qq-protocol--nonzero-decimal-string-p native-id)
    (user-error "qq: channel member page requires a native tinyId"))
  (let ((buffer (get-buffer-create qq-guild-user-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-guild-user-mode)
        (qq-guild-user-mode))
      (qq-guild-user--select guild-id native-id)
      (qq-guild-user-render)
      (when (and (null qq-guild-user--profile)
                 (not qq-guild-user--loading))
        (qq-guild-user-refresh)))
    (pop-to-buffer buffer)
    buffer))

(defun qq-guild-user--handle-media-cache-update (media-key)
  "Rerender the member buffer after its avatar MEDIA-KEY changes."
  (when-let* (((stringp media-key))
              (buffer (get-buffer qq-guild-user-buffer-name)))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'qq-guild-user-mode)
                 (equal media-key (qq-guild-user--avatar-key)))
        (qq-guild-user-render)))))

(add-hook 'qq-media-cache-update-hook
          #'qq-guild-user--handle-media-cache-update)

(provide 'qq-guild-user)

;;; qq-guild-user.el ends here
