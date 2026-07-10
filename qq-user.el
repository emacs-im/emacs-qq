;;; qq-user.el --- Native QQ user profile buffers -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Dedicated user profile view backed by the fork-native `emacs_get_user'
;; action.  Actions use keys rather than inline button rows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-api)
(require 'qq-media)
(require 'qq-state)
(require 'qq-view)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-api-cancel-request "qq-api" (request-token))

(defvar-local qq-user--user-id nil
  "QQ number displayed by the current user buffer.")

(defvar-local qq-user--profile nil
  "Native user profile displayed by the current user buffer.")

(defvar-local qq-user--request nil
  "Active profile request token for the current user buffer.")

(defvar-local qq-user--request-owner nil
  "Owner object for the current profile request.")

(defvar-local qq-user--loading nil
  "Non-nil while the current user profile is loading.")

(defvar-local qq-user--error nil
  "Last profile loading error string, or nil.")

(defun qq-user--buffer-name (user-id)
  "Return profile buffer name for USER-ID."
  (format "*qq-user:%s*" user-id))

(defun qq-user--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-user--display-name ()
  "Return the best title for the current profile."
  (or (qq-user--present-string (alist-get 'remark qq-user--profile))
      (qq-user--present-string (alist-get 'nickname qq-user--profile))
      qq-user--user-id
      "QQ user"))

(defun qq-user--header-line ()
  "Return dynamic header line for the current user buffer."
  (format " QQ User · %s (%s)%s"
          (qq-user--display-name)
          (or qq-user--user-id "unknown")
          (if qq-user--loading " · loading" "")))

(defun qq-user--insert-field (label value &optional face)
  "Insert profile LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face
        (add-text-properties start (point) (list 'face face))))))

(defun qq-user--sex-label (sex)
  "Return display label for native SEX string."
  (pcase sex
    ("male" "男")
    ("female" "女")
    ("private" "保密")
    (_ nil)))

(defun qq-user--birthday-label (birthday)
  "Return display label for BIRTHDAY object."
  (when (consp birthday)
    (let ((year (alist-get 'year birthday))
          (month (alist-get 'month birthday))
          (day (alist-get 'day birthday)))
      (when (and (integerp month) (<= 1 month 12)
                 (integerp day) (<= 1 day 31))
        (if (and (integerp year) (> year 0))
            (format "%04d-%02d-%02d" year month day)
          (format "%02d-%02d" month day))))))

(defun qq-user--location-label (location)
  "Return display label for LOCATION object."
  (when (consp location)
    (let ((parts (delq nil
                       (mapcar #'qq-user--present-string
                               (list (alist-get 'country location)
                                     (alist-get 'province location)
                                     (alist-get 'city location))))))
      (and parts (string-join parts " · ")))))

(defun qq-user--status-label (status)
  "Return display label for STATUS object."
  (when (consp status)
    (qq-user--present-string (alist-get 'description status))))

(defun qq-user--relationship-label (relationship)
  "Return display label for RELATIONSHIP object."
  (when (consp relationship)
    (when-let* ((kind (pcase (alist-get 'kind relationship)
                        ("friend" "好友")
                        ("stranger" "陌生人"))))
      (string-join
       (delq nil
             (list kind
                   (and (eq (alist-get 'special_care relationship) t)
                        "特别关心")
                   (and (eq (alist-get 'blocked_by_me relationship) t)
                        "已屏蔽")))
       " · "))))

(defun qq-user--level-label (level)
  "Return display label for QQ LEVEL object."
  (when (consp level)
    (let ((parts
           (cl-loop for (field label) in '((crowns "皇冠")
                                            (suns "太阳")
                                            (moons "月亮")
                                            (stars "星星"))
                    for count = (alist-get field level)
                    when (and (integerp count) (> count 0))
                    collect (format "%d %s" count label))))
      (and parts (string-join parts " · ")))))

(defun qq-user--vip-label (vip)
  "Return display label for VIP object."
  (when (consp vip)
    (pcase (alist-get 'kind vip)
      ((or "svip" "vip")
       (let ((name (if (equal (alist-get 'kind vip) "svip") "SVIP" "VIP"))
             (level (alist-get 'level vip)))
         (concat name
                 (if (and (integerp level) (> level 0))
                     (format " %d" level)
                   "")
                 (if (eq (alist-get 'annual vip) t) " · 年费" ""))))
      ("none" "无")
      (_ nil))))

(defun qq-user-render ()
  "Render the current user profile buffer."
  (interactive)
  (qq-view-render-preserving-position
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-user--header-line)))
       (qq-view-insert-note-line
        "g refresh  m message  a avatar  w copy QQ  q quit")
       (insert "\n")
       (cond
        (qq-user--loading
         (qq-view-insert-note-line "Loading user profile…"))
        (qq-user--error
         (qq-view-insert-note-line qq-user--error :face 'error))
        ((null qq-user--profile)
         (qq-view-insert-note-line "No user profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-media-avatar-display-string qq-user--user-id)
                   "  "
                   (propertize (qq-user--display-name) 'face 'bold)
                   "\n")
           (add-text-properties
            avatar-start (point)
            (list 'qq-user-id qq-user--user-id)))
         (when-let* ((nickname (qq-user--present-string
                                (alist-get 'nickname qq-user--profile)))
                     (remark (qq-user--present-string
                              (alist-get 'remark qq-user--profile)))
                     ((not (equal nickname remark))))
           (qq-user--insert-field "昵称" nickname))
         (qq-user--insert-field "QQ" qq-user--user-id)
         (qq-user--insert-field "QID" (alist-get 'qid qq-user--profile))
         (qq-user--insert-field
          "关系" (qq-user--relationship-label
                  (alist-get 'relationship qq-user--profile)))
         (qq-user--insert-field
          "状态" (qq-user--status-label (alist-get 'status qq-user--profile)))
         (qq-user--insert-field "性别" (qq-user--sex-label
                                        (alist-get 'sex qq-user--profile)))
         (let ((age (alist-get 'age qq-user--profile)))
           (when (and (integerp age) (> age 0))
             (qq-user--insert-field "年龄" age)))
         (qq-user--insert-field
          "生日" (qq-user--birthday-label (alist-get 'birthday qq-user--profile)))
         (qq-user--insert-field
          "地区" (qq-user--location-label (alist-get 'location qq-user--profile)))
         (qq-user--insert-field "职业" (alist-get 'occupation qq-user--profile))
         (qq-user--insert-field "学校" (alist-get 'college qq-user--profile))
         (qq-user--insert-field
          "等级" (qq-user--level-label (alist-get 'qq_level qq-user--profile)))
         (qq-user--insert-field
          "会员" (qq-user--vip-label (alist-get 'vip qq-user--profile)))
         (when-let* ((labels (alist-get 'labels qq-user--profile))
                     ((listp labels))
                     ((not (null labels))))
           (qq-user--insert-field "标签" (string-join labels " · ")))
         (when-let* ((signature (qq-user--present-string
                                 (alist-get 'signature qq-user--profile))))
           (insert "\n")
           (qq-view-insert-heading-line "个性签名" :face 'bold)
           (insert signature "\n"))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-user--request-current-p (buffer user-id owner)
  "Return non-nil when OWNER still loads USER-ID in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (equal qq-user--user-id user-id)
              (eq qq-user--request-owner owner)))))

(defun qq-user-refresh ()
  "Refresh the current native user profile."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (when qq-user--request
    (qq-api-cancel-request qq-user--request))
  (let ((buffer (current-buffer))
        (user-id qq-user--user-id)
        (owner (list 'user-profile qq-user--user-id)))
    (setq qq-user--loading t
          qq-user--error nil
          qq-user--request nil
          qq-user--request-owner owner)
    (qq-user-render)
    (let ((request
           (qq-api-get-user
            user-id
            (lambda (profile)
              (when (qq-user--request-current-p buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--profile profile
                        qq-user--loading nil
                        qq-user--error nil
                        qq-user--request nil
                        qq-user--request-owner nil)
                  (qq-user-render))))
            (lambda (response reason)
              (when (qq-user--request-current-p buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--loading nil
                        qq-user--error (format "Unable to load profile: %s"
                                               (or reason "unknown error"))
                        qq-user--request nil
                        qq-user--request-owner nil)
                  (qq-user-render)))
              (qq-api--default-error response reason)))))
      (when (eq qq-user--request-owner owner)
        (setq qq-user--request request)))))

(defun qq-user-open-chat ()
  "Open a private chat with the current profile user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (qq-chat-open (qq-state-session-key 'private qq-user--user-id)))

(defun qq-user-open-avatar ()
  "Open the current profile user's avatar."
  (interactive)
  (qq-media-open-user-avatar qq-user--user-id))

(defun qq-user-copy-id ()
  "Copy the current profile user's QQ number."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (kill-new qq-user--user-id)
  (message "qq: copied user id %s" qq-user--user-id))

(defun qq-user--cancel-request ()
  "Cancel the current user profile request when present."
  (when qq-user--request
    (qq-api-cancel-request qq-user--request))
  (setq qq-user--request nil
        qq-user--request-owner nil))

(defvar qq-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-user-refresh)
    (define-key map (kbd "m") #'qq-user-open-chat)
    (define-key map (kbd "a") #'qq-user-open-avatar)
    (define-key map (kbd "w") #'qq-user-copy-id)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-user-mode'.")

(define-derived-mode qq-user-mode special-mode "QQ-User"
  "Major mode for one native QQ user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (add-hook 'kill-buffer-hook #'qq-user--cancel-request nil t))

;;;###autoload
(defun qq-user-open (user-id)
  "Open the native user profile for decimal string USER-ID."
  (interactive "sQQ number: ")
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: user profile requires a decimal string user id"))
  (let ((buffer (get-buffer-create (qq-user--buffer-name user-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-user-mode)
        (qq-user-mode))
      (setq qq-user--user-id user-id)
      (qq-user-render)
      (unless qq-user--loading
        (qq-user-refresh)))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user--handle-media-cache-update (media-key)
  "Rerender matching user buffers after MEDIA-KEY changes."
  (when (and (stringp media-key)
             (string-prefix-p "avatar:" media-key))
    (let ((user-id (substring media-key (length "avatar:"))))
      (when-let* ((buffer (get-buffer (qq-user--buffer-name user-id))))
        (with-current-buffer buffer
          (when (derived-mode-p 'qq-user-mode)
            (qq-user-render)))))))

(add-hook 'qq-media-cache-update-hook #'qq-user--handle-media-cache-update)

(provide 'qq-user)

;;; qq-user.el ends here
