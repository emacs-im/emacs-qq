;;; qq-group.el --- Native QQ group profile buffer -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Telega-style group profile page backed by the strict `emacs_get_group'
;; action.  Unknown native enum values remain visible as numeric codes.

;;; Code:

(require 'button)
(require 'subr-x)
(require 'qq-api)
(require 'qq-media)
(require 'qq-state)
(require 'appkit-ui)
(require 'appkit-view)
(require 'appkit-position)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-contacts-search-group-members
                  "qq-contacts" (group-id query))
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-api-cancel-request "qq-api" (request-token))

(defface qq-group-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face used for action buttons on QQ group cards."
  :group 'qq)

(defface qq-group-card-title
  '((t :inherit bold :height 1.15))
  "Face used for the primary title on QQ group cards."
  :group 'qq)

(defvar-local qq-group--group-id nil
  "Group code displayed by the current group buffer.")

(defvar-local qq-group--profile nil
  "Native group profile displayed by the current group buffer.")

(defvar-local qq-group--request nil
  "Active group-profile request token.")

(defvar-local qq-group--request-owner nil
  "Owner object for the active group-profile request.")

(defvar-local qq-group--loading nil
  "Non-nil while the group profile is loading.")

(defvar-local qq-group--error nil
  "Last group-profile loading error, or nil.")

(defun qq-group--buffer-name (_group-id)
  "Return the shared group profile buffer name."
  "*qq-group*")

(defun qq-group--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-group--display-name ()
  "Return the best title for the current group profile."
  (or (qq-group--present-string (alist-get 'remark qq-group--profile))
      (qq-group--present-string (alist-get 'name qq-group--profile))
      qq-group--group-id
      "QQ group"))

(defun qq-group--header-line ()
  "Return the dynamic header line for the group profile."
  (format " QQ Group · %s (%s)%s"
          (qq-group--display-name)
          (or qq-group--group-id "unknown")
          (if qq-group--loading " · loading" "")))

(defun qq-group--insert-field (label value &optional face)
  "Insert group profile LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face
        (add-text-properties start (point) (list 'face face))))))

(defun qq-group--timestamp-label (value)
  "Return a readable local timestamp for positive integer VALUE."
  (when (and (integerp value) (> value 0))
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time value))))

(defun qq-group--boolean-label (value)
  "Return an exact display label for JSON boolean VALUE."
  (cond ((eq value t) "是")
        ((eq value :false) "否")
        (t nil)))

(defun qq-group--permission-label (value)
  "Return a localized label for verified permission VALUE."
  (pcase value
    ("member" "成员")
    ("admin" "管理员")
    ("owner" "群主")))

(defun qq-group--join-mode-label (value)
  "Return a localized label for verified native join-mode VALUE."
  (pcase value
    ("open" "允许任何人加入")
    ("approval" "需管理员审核")
    ("closed" "不允许任何人加入")
    ("question_answer" "需正确回答问题")
    ("question_approval" "回答问题后由管理员审核")))

(defun qq-group-open-chat ()
  "Open the current group chat."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-chat-open (qq-state-session-key 'group qq-group--group-id)))

(defun qq-group-open-avatar ()
  "Open the current group's avatar."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (qq-media-open-group-avatar qq-group--group-id))

(defun qq-group-open-owner ()
  "Open the current group owner's user page."
  (interactive)
  (let ((owner-id (alist-get 'owner_id qq-group--profile)))
    (unless (qq-api-user-id-p owner-id)
      (user-error "qq: group owner identity is unavailable"))
    (require 'qq-user)
    (qq-user-open owner-id)))

(defun qq-group-copy-id ()
  "Copy the current group code."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (kill-new qq-group--group-id)
  (message "qq: copied group id %s" qq-group--group-id))

(defun qq-group-search-members (&optional query)
  "Search exact native members of the current group for QUERY."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (setq query (or query (read-string "搜索群成员: ")))
  (require 'qq-contacts)
  (qq-contacts-search-group-members qq-group--group-id query))

(defun qq-group--insert-action-buttons ()
  "Insert primary group action buttons."
  (insert "  ")
  (appkit-ui-insert-action-button
   " 打开群聊 " #'qq-group-open-chat
   :face 'qq-group-action-button :help-echo "打开群聊 (m)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 查看头像 " #'qq-group-open-avatar
   :face 'qq-group-action-button :help-echo "查看群头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 搜索成员 " #'qq-group-search-members
   :face 'qq-group-action-button :help-echo "原生搜索群成员 (s)")
  (when (qq-api-user-id-p (alist-get 'owner_id qq-group--profile))
    (insert "  ")
    (appkit-ui-insert-action-button
     " 群主资料 " #'qq-group-open-owner
     :face 'qq-group-action-button :help-echo "打开群主资料 (o)"))
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制群号 " #'qq-group-copy-id
   :face 'qq-group-action-button :help-echo "复制群号 (w)")
  (insert "\n"))

(defun qq-group-render ()
  "Render the current group profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group--header-line)))
       (cond
        (qq-group--loading
         (appkit-view-insert-note-line "Loading group profile…"))
        (qq-group--error
         (appkit-view-insert-note-line qq-group--error :face 'error))
        ((null qq-group--profile)
         (appkit-view-insert-note-line "No group profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-media-group-avatar-display-string qq-group--group-id))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'action (lambda (_button) (qq-group-open-avatar))
            'help-echo "查看群头像"
            'qq-group-id qq-group--group-id)
           (insert "  "
                   (propertize (qq-group--display-name)
                               'face 'qq-group-card-title)
                   "\n   "
                   (propertize
                    (format "%d / %d 位成员"
                            (alist-get 'member_count qq-group--profile)
                            (alist-get 'max_member_count qq-group--profile))
                    'face 'shadow)
                   "\n"))
         (insert "\n")
         (qq-group--insert-action-buttons)
         (appkit-view-insert-note-line
          "g 刷新 · m 群聊 · s 搜索成员 · a 头像 · o 群主 · w 复制 · q 退出")
         (insert "\n")
         (appkit-view-insert-heading-line "资料" :face 'bold)
         (let ((name (qq-group--present-string
                      (alist-get 'name qq-group--profile)))
               (remark (qq-group--present-string
                        (alist-get 'remark qq-group--profile))))
           (when (and name remark (not (equal name remark)))
             (qq-group--insert-field "群名称" name)))
         (qq-group--insert-field "群号" qq-group--group-id)
         (qq-group--insert-field "群主" (alist-get 'owner_id qq-group--profile))
         (qq-group--insert-field
          "成员"
          (format "%d / %d"
                  (alist-get 'member_count qq-group--profile)
                  (alist-get 'max_member_count qq-group--profile)))
         (qq-group--insert-field
          "活跃成员" (alist-get 'active_member_count qq-group--profile))
         (qq-group--insert-field
          "创建时间" (qq-group--timestamp-label
                      (alist-get 'created_at qq-group--profile)))
         (qq-group--insert-field
          "入群时间" (qq-group--timestamp-label
                      (alist-get 'joined_at qq-group--profile)))
         (qq-group--insert-field
          "置顶" (qq-group--boolean-label (alist-get 'pinned qq-group--profile)))
         (qq-group--insert-field
          "群分类" (qq-group--present-string
                    (alist-get 'name (alist-get 'category qq-group--profile))))
         (qq-group--insert-field "群等级" (alist-get 'grade qq-group--profile))
         (qq-group--insert-field
          "我的权限" (qq-group--permission-label
                      (alist-get 'self_permission qq-group--profile)))
         (when-let* ((mute (alist-get 'mute qq-group--profile)))
           (qq-group--insert-field
            "全员禁言至" (qq-group--timestamp-label (alist-get 'all_until mute)))
           (qq-group--insert-field
            "我的禁言至" (qq-group--timestamp-label (alist-get 'self_until mute))))
         (when-let* ((join (alist-get 'join qq-group--profile)))
           (qq-group--insert-field
            "入群模式" (qq-group--join-mode-label (alist-get 'mode join)))
           (qq-group--insert-field "入群问题" (alist-get 'question join))
           (qq-group--insert-field "我的答案" (alist-get 'answer join)))
         (qq-group--insert-field
          "认证" (qq-group--present-string
                  (alist-get 'text (alist-get 'certification qq-group--profile))))
         (when-let* ((school (alist-get 'school qq-group--profile)))
           (qq-group--insert-field
            "学校"
            (string-join
             (delq nil
                   (list (qq-group--present-string (alist-get 'name school))
                         (qq-group--present-string (alist-get 'location school))))
             " · ")))
         (when-let* ((location (alist-get 'location qq-group--profile)))
           (qq-group--insert-field "地点" (alist-get 'text location)))
         (dolist (section '(("群简介" . description)
                            ("群公告" . announcement)))
           (when-let* ((text (qq-group--present-string
                              (alist-get (cdr section) qq-group--profile))))
             (insert "\n")
             (appkit-view-insert-heading-line (car section) :face 'bold)
             (insert text "\n")))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-group--request-current-p (buffer group-id owner)
  "Return non-nil when OWNER still loads GROUP-ID in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-mode)
              (equal qq-group--group-id group-id)
              (eq qq-group--request-owner owner)))))

(defun qq-group-refresh ()
  "Refresh the current native group profile."
  (interactive)
  (unless qq-group--group-id
    (user-error "qq: this buffer has no group identity"))
  (when qq-group--request
    (qq-api-cancel-request qq-group--request))
  (let ((buffer (current-buffer))
        (group-id qq-group--group-id)
        (owner (list 'group-profile qq-group--group-id)))
    (setq qq-group--loading t
          qq-group--error nil
          qq-group--request nil
          qq-group--request-owner owner)
    (qq-group-render)
    (condition-case error-data
        (let ((request
               (qq-api-get-group
                group-id
                (lambda (profile)
                  (when (qq-group--request-current-p buffer group-id owner)
                    (with-current-buffer buffer
                      (setq qq-group--profile profile
                            qq-group--loading nil
                            qq-group--error nil
                            qq-group--request nil
                            qq-group--request-owner nil)
                      (qq-group-render))))
                (lambda (response reason)
                  (when (qq-group--request-current-p buffer group-id owner)
                    (with-current-buffer buffer
                      (setq qq-group--loading nil
                            qq-group--error
                            (format "Unable to load group: %s"
                                    (or reason "unknown error"))
                            qq-group--request nil
                            qq-group--request-owner nil)
                      (qq-group-render)))
                  (qq-api--default-error response reason)))))
          (when (eq qq-group--request-owner owner)
            (setq qq-group--request request)))
      (error
       (when (qq-group--request-current-p buffer group-id owner)
         (setq qq-group--loading nil
               qq-group--error
               (format "Unable to load group: %s"
                       (error-message-string error-data))
               qq-group--request nil
               qq-group--request-owner nil)
         (qq-group-render))))))

(defun qq-group--cancel-request ()
  "Cancel the active group-profile request."
  (when qq-group--request
    (qq-api-cancel-request qq-group--request))
  (setq qq-group--request nil
        qq-group--request-owner nil))

(defun qq-group--select-group (group-id)
  "Prepare the shared group buffer to display GROUP-ID."
  (unless (equal qq-group--group-id group-id)
    (qq-group--cancel-request)
    (setq qq-group--profile nil
          qq-group--loading nil
          qq-group--error nil))
  (setq qq-group--group-id group-id))

(defun qq-group-button-backward ()
  "Move point to the previous page button."
  (interactive)
  (forward-button -1))

(defvar qq-group-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-group-refresh)
    (define-key map (kbd "m") #'qq-group-open-chat)
    (define-key map (kbd "a") #'qq-group-open-avatar)
    (define-key map (kbd "s") #'qq-group-search-members)
    (define-key map (kbd "o") #'qq-group-open-owner)
    (define-key map (kbd "w") #'qq-group-copy-id)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-group-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-group-mode'.")

(define-derived-mode qq-group-mode special-mode "QQ-Group"
  "Major mode for one native QQ group profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (add-hook 'change-major-mode-hook #'qq-group--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-group--cancel-request nil t))

;;;###autoload
(defun qq-group-open (group-id)
  "Open the native group profile for canonical uint32 string GROUP-ID."
  (interactive "sQQ group number: ")
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group profile requires a canonical uint32 group id"))
  (let ((buffer (get-buffer-create (qq-group--buffer-name group-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-group-mode)
        (qq-group-mode))
      (qq-group--select-group group-id)
      (qq-group-render)
      (when (and (null qq-group--profile)
                 (not qq-group--loading))
        (qq-group-refresh)))
    (pop-to-buffer buffer)
    buffer))

(defun qq-group--handle-media-cache-update (media-key)
  "Rerender the group buffer after its avatar cache changes."
  (when-let* (((stringp media-key))
              (buffer (get-buffer (qq-group--buffer-name nil))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'qq-group-mode)
                 (equal media-key (format "group-avatar:%s" qq-group--group-id)))
        (qq-group-render)))))

(add-hook 'qq-media-cache-update-hook #'qq-group--handle-media-cache-update)

(provide 'qq-group)

;;; qq-group.el ends here
