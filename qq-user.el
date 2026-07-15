;;; qq-user.el --- Native QQ user profile buffers -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Dedicated user profile view backed by the fork-native `emacs_get_user'
;; action, with a Telega-style summary card and asynchronously filled details.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-transaction)
(require 'qq-api)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-user-photo)
(require 'appkit-ui)
(require 'appkit-view)
(require 'appkit-position)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-api-cancel-request "qq-api" (request-token))
(declare-function qq-user-photo-make-button
                  "qq-user-photo" (start end user-id photo))

(defconst qq-user--view-id 'user-profile
  "Stable Appkit identity of the singleton user-profile view.")

(defface qq-user-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face used for action buttons on QQ user cards."
  :group 'qq)

(defface qq-user-card-title
  '((t :inherit bold :height 1.15))
  "Face used for the primary title on QQ user cards."
  :group 'qq)
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

(defvar-local qq-user--like-count nil
  "Verified received profile-like count, or nil when unavailable.")

(defvar-local qq-user--like-loading nil
  "Non-nil while the received profile-like count is loading.")

(defvar-local qq-user--like-error nil
  "Last profile-like loading error string, or nil.")

(defvar-local qq-user--like-request nil
  "Active profile-like request token for the current user buffer.")

(defvar-local qq-user--like-request-owner nil
  "Owner object for the active profile-like request.")

(defvar-local qq-user--send-like-request nil
  "Active request token for adding a profile like.")

(defvar-local qq-user--send-like-request-owner nil
  "Owner object for the active profile-like mutation.")

(defvar-local qq-user--like-limit-date nil
  "Local date on which QQ reported the current target's daily like limit.")

(defvar-local qq-user--photos nil
  "Native photo-wall entries shown inline on the user page.")

(defvar-local qq-user--photo-request nil
  "Active inline photo-wall request token.")

(defvar-local qq-user--photo-request-owner nil
  "Owner object for the active inline photo-wall request.")

(defvar-local qq-user--photo-loading nil
  "Non-nil while inline photo-wall data is loading.")

(defvar-local qq-user--photo-loaded nil
  "Non-nil when inline photo-wall data was loaded successfully.")

(cl-defstruct (qq-user--friend-add-state
               (:constructor qq-user--friend-add-state-create))
  user-id
  candidate
  preparation
  verification
  questions
  status
  error
  request)

(defvar-local qq-user--search-result nil
  "Validated global-search result used only as profile-page context.")

(defvar-local qq-user--friend-add-state nil
  "One-use friend-add workflow attached to the current search result.")

(defvar-local qq-user--media-hook-function nil
  "View-owned media cache hook installed for this user buffer.")

(defun qq-user--buffer-name (user-id)
  "Return profile buffer name for USER-ID."
  (ignore user-id)
  "*qq-user*")

(defun qq-user--profile-key (&optional user-id)
  "Return the stable presentation key for USER-ID's profile.

USER-ID defaults to the opaque identity selected in the current buffer."
  (list 'user-profile (or user-id qq-user--user-id)))

(defun qq-user--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-user--display-name ()
  "Return the best title for the current profile."
  (or (qq-user--present-string (alist-get 'remark qq-user--profile))
      (qq-user--present-string (alist-get 'nickname qq-user--profile))
      (qq-user--present-string (alist-get 'nickname qq-user--search-result))
      qq-user--user-id
      "QQ user"))

(defun qq-user--avatar-display-string ()
  "Return the current user's avatar, honoring exact search context."
  (if-let* ((url (qq-user--present-string
                  (alist-get 'avatar_url qq-user--search-result))))
      (qq-media-url-preview-display-string
       (format "avatar:%s" qq-user--user-id)
       url "@" qq-media-avatar-image-height)
    (qq-media-avatar-display-string qq-user--user-id)))

(defun qq-user--self-p ()
  "Return non-nil when the current profile belongs to this account."
  (and qq-user--user-id
       (equal qq-user--user-id (qq-state-self-user-id))))

(defun qq-user--like-limit-reached-p ()
  "Return non-nil when QQ reported today's limit for the current profile."
  (equal qq-user--like-limit-date (format-time-string "%Y-%m-%d")))

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
                        "已屏蔽")
                   (and (eq (alist-get 'muted relationship) t)
                        "消息免打扰")))
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

(defun qq-user--photo-at-point ()
  "Return inline native photo at point, or nil."
  (get-text-property (point) 'qq-user-photo))

(defun qq-user--insert-photo-wall ()
  "Insert asynchronous photo-wall summary and previews."
  (let ((count (and qq-user--photo-loaded (length qq-user--photos))))
    (qq-user--insert-field
     "照片墙"
     (cond (qq-user--photo-loading "加载中…")
           ((integerp count) count)
           (t nil)))
    (when qq-user--photos
      (let ((index 0))
        (dolist (photo qq-user--photos)
          (setq index (1+ index))
          (let ((start (point)))
            (insert
             (qq-user-photo-preview-display-string
              qq-user--user-id photo (format "照片 %d" index))
             " ")
            (qq-user-photo-make-button
             start (point) qq-user--user-id photo)))
        (insert "\n")))))

(defun qq-user--friend-p ()
  "Return non-nil when the authoritative profile says user is a friend."
  (equal (alist-get 'kind (alist-get 'relationship qq-user--profile))
         "friend"))

(defun qq-user--friend-add-current-p (state user-id &optional view)
  "Return non-nil when STATE still owns USER-ID and optional VIEW."
  (and (or (null view) (qq-user--view-current-p view))
       (derived-mode-p 'qq-user-mode)
       (or (null view) (eq view (appkit-current-view)))
       (eq state qq-user--friend-add-state)
       (equal (qq-user--friend-add-state-user-id state) user-id)
       (equal user-id qq-user--user-id)))

(defun qq-user--friend-add-button-label ()
  "Return the action label for the current friend-add state."
  (when-let* ((state qq-user--friend-add-state))
    (pcase (qq-user--friend-add-state-status state)
      ('candidate " 加好友 ")
      ('preparing " 获取申请设置中… ")
      ('prepared " 填写并发送申请 ")
      ('submitting " 正在发送… ")
      (_ nil))))

(defun qq-user--insert-friend-add-status ()
  "Insert current friend-add questions, progress, or terminal result."
  (when-let* ((state qq-user--friend-add-state))
    (pcase (qq-user--friend-add-state-status state)
      ('preparing
       (appkit-view-insert-note-line "正在获取好友验证设置…" :face 'shadow))
      ('prepared
       (let ((questions (qq-user--friend-add-state-questions state)))
         (when questions
           (appkit-view-insert-heading-line "好友验证问题" :face 'bold)
           (dolist (question questions)
             (insert "  • " question "\n")))
         (appkit-view-insert-note-line
          "申请设置已就绪；点击“填写并发送申请”继续。"
          :face 'shadow)))
      ('submitting
       (appkit-view-insert-note-line "正在发送好友申请…" :face 'shadow))
      ('submitted
       (appkit-view-insert-note-line "好友申请已发送" :face 'success))
      ('error
       (appkit-view-insert-note-line
        (format "好友申请流程已失效：%s。请重新搜索该用户。"
                (or (qq-user--friend-add-state-error state) "未知错误"))
        :face 'error)))))

(defun qq-user--friend-add-fail
    (view buffer state user-id _response reason)
  "Finish STATE in BUFFER for USER-ID and VIEW with failure REASON."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (qq-user--friend-add-current-p state user-id view)
        (setf (qq-user--friend-add-state-request state) nil
              (qq-user--friend-add-state-candidate state) nil
              (qq-user--friend-add-state-preparation state) nil
              (qq-user--friend-add-state-status state) 'error
              (qq-user--friend-add-state-error state)
              (or reason "未知错误"))
        (when view (qq-user--request-sync view))))))

(defun qq-user--friend-add-prepared (view buffer state user-id result)
  "Apply prepared friend-add RESULT to STATE in BUFFER for USER-ID and VIEW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (qq-user--friend-add-current-p state user-id view)
        (setf (qq-user--friend-add-state-request state) nil
              (qq-user--friend-add-state-preparation state)
              (alist-get 'preparation result)
              (qq-user--friend-add-state-verification state)
              (alist-get 'verification result)
              (qq-user--friend-add-state-questions state)
              (copy-sequence (alist-get 'questions result))
              (qq-user--friend-add-state-status state) 'prepared
              (qq-user--friend-add-state-error state) nil)
        (when view (qq-user--request-sync view))))))

(defun qq-user--prepare-friend-add (state)
  "Consume STATE's candidate and request exact friend verification settings."
  (unless (eq (qq-user--friend-add-state-status state) 'candidate)
    (user-error "qq: this friend request is not awaiting preparation"))
  (let ((view (qq-user--live-current-view))
        (candidate (qq-user--friend-add-state-candidate state))
        (user-id qq-user--user-id)
        (buffer (current-buffer)))
    (unless candidate
      (user-error "qq: friend-search candidate is unavailable"))
    ;; Remove the one-use capability before dispatch.  No error path may retry it.
    (setf (qq-user--friend-add-state-candidate state) nil
          (qq-user--friend-add-state-status state) 'preparing
          (qq-user--friend-add-state-error state) nil)
    (if view (qq-user--request-sync view) (qq-user-render))
    (condition-case error-data
        (let ((request
               (qq-api-friend-add-prepare
                user-id candidate
                (apply-partially #'qq-user--friend-add-prepared
                                 view buffer state user-id)
                (apply-partially #'qq-user--friend-add-fail
                                 view buffer state user-id))))
          (when (and (qq-user--friend-add-current-p state user-id view)
                     (eq (qq-user--friend-add-state-status state) 'preparing))
            (setf (qq-user--friend-add-state-request state) request)))
      (error
       (qq-user--friend-add-fail
        view buffer state user-id nil (error-message-string error-data))))
    (when view (qq-user--sync-now view))))

(defun qq-user--friend-group-choices ()
  "Return display-name to native category-id pairs for friend placement."
  (let ((label-counts (make-hash-table :test #'equal)) rows)
    (dolist (category (qq-state-friend-categories))
      (let ((id (alist-get 'category_id category))
            (name (or (qq-user--present-string (alist-get 'name category))
                      "未命名分组"))
            (count (length (alist-get 'friends category))))
        (when (and (integerp id) (<= 0 id #xffffffff))
          (let ((label (format "%s · %d 位好友" name count)))
            (puthash label (1+ (gethash label label-counts 0)) label-counts)
            (push (list id label) rows)))))
    (mapcar
     (lambda (row)
       (let ((id (car row)) (label (cadr row)))
         (cons (if (> (gethash label label-counts) 1)
                   (format "%s · 分组 %d" label id)
                 label)
               id)))
     (nreverse rows))))

(defun qq-user--read-friend-group-id ()
  "Read one native friend category id from current authoritative state."
  (let ((choices (qq-user--friend-group-choices)))
    (unless choices
      (user-error
       "qq: friend categories are unavailable; refresh contacts first"))
    ;; QQ's native add-friend default is category 0.  Do not derive a default
    ;; from display order: category 9999 (special care) is commonly first.
    (let* ((default (car (rassq 0 choices)))
           (selected (completing-read "好友分组: " choices nil t nil nil
                                      default)))
      (or (cdr (assoc selected choices))
          (user-error "qq: invalid friend category selection")))))

(defun qq-user--read-friend-verification (state)
  "Read exact verification fields required by prepared STATE.

Return a closed alist containing `verification_message' and `answers'."
  (let* ((verification (qq-user--friend-add-state-verification state))
         (questions (qq-user--friend-add-state-questions state))
         verification-message
         answers)
    (pcase verification
      ("none")
      ("message"
       (setq verification-message (read-string "验证留言: "))
       (when (string-empty-p (string-trim verification-message))
         (user-error "qq: 好友验证留言不能为空"))
       (when (> (qq-api--utf-16-code-unit-length verification-message) 300)
         (user-error "qq: 好友验证留言不能超过 300 个 UTF-16 code units")))
      ((or "question_answer" "question_and_audit")
       (let ((count (length questions)))
         (setq answers
               (cl-loop
                for question in questions
                for index from 1
                for answer =
                (read-string
                 (format "问题 %d/%d（%s）答案: " index count question))
                do (when (string-empty-p (string-trim answer))
                     (user-error "qq: 好友验证答案不能为空"))
                do (when (> (qq-api--utf-16-code-unit-length answer) 300)
                     (user-error
                      "qq: 好友验证答案不能超过 300 个 UTF-16 code units"))
                collect answer))))
      (kind (error "qq: invalid friend verification kind %S" kind)))
    `((verification_message . ,(or verification-message ""))
      (answers . ,answers))))

(defun qq-user--read-friend-permissions ()
  "Read every permission exposed by the official friend-add form."
  `((only_chat . ,(y-or-n-p "权限：仅聊天？ "))
    (qzone_not_watch . ,(y-or-n-p "权限：不看对方的 QQ 空间？ "))
    (qzone_not_watched . ,(y-or-n-p "权限：不让对方看我的 QQ 空间？ "))))

(defun qq-user--friend-add-submitted (view buffer state user-id _result)
  "Mark STATE in BUFFER as submitted for USER-ID and VIEW."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (qq-user--friend-add-current-p state user-id view)
        (setf (qq-user--friend-add-state-request state) nil
              (qq-user--friend-add-state-preparation state) nil
              (qq-user--friend-add-state-status state) 'submitted
              (qq-user--friend-add-state-error state) nil)
        (when view (qq-user--request-sync view))
        (message "qq: 好友申请已发送")))))

(defun qq-user--submit-friend-add (state)
  "Collect fields for prepared STATE and submit its one-use capability."
  (unless (eq (qq-user--friend-add-state-status state) 'prepared)
    (user-error "qq: this friend request is not prepared"))
  (let* ((verification-options
          (qq-user--read-friend-verification state))
         (remark (read-string "好友备注（可空）: "))
         (group-id (qq-user--read-friend-group-id))
         (permissions (qq-user--read-friend-permissions))
         (view (qq-user--live-current-view))
         (user-id qq-user--user-id)
         (buffer (current-buffer))
         (options
          (qq-api--normalize-friend-add-options
           `((verification_message
              . ,(alist-get 'verification_message verification-options))
             (answers . ,(alist-get 'answers verification-options))
             (remark . ,remark)
             (friend_group_id . ,group-id)
             ,@permissions))))
    (when (yes-or-no-p (format "确认向 %s 发送好友申请？ "
                               (qq-user--display-name)))
      (let ((preparation (qq-user--friend-add-state-preparation state)))
        (unless preparation
          (user-error "qq: friend-add preparation is unavailable"))
        ;; Keep preparation through all user prompts, then remove it immediately
        ;; before the mutating dispatch.  Cancellation above remains retryable.
        (setf (qq-user--friend-add-state-preparation state) nil
              (qq-user--friend-add-state-status state) 'submitting
              (qq-user--friend-add-state-error state) nil)
        (if view (qq-user--request-sync view) (qq-user-render))
        (condition-case error-data
            (let ((request
                   (qq-api-friend-add-submit
                    user-id preparation options
                    (apply-partially #'qq-user--friend-add-submitted
                                     view buffer state user-id)
                    (apply-partially #'qq-user--friend-add-fail
                                     view buffer state user-id))))
              (when (and (qq-user--friend-add-current-p state user-id view)
                         (eq (qq-user--friend-add-state-status state)
                             'submitting))
                (setf (qq-user--friend-add-state-request state) request)))
          (error
           (qq-user--friend-add-fail
            view buffer state user-id nil
            (error-message-string error-data))))
        (when view (qq-user--sync-now view))))))

(defun qq-user-add-friend ()
  "Advance the exact add-friend flow for the current search result."
  (interactive)
  (qq-user--ensure-view)
  (when (qq-user--self-p)
    (user-error "qq: cannot add yourself as a friend"))
  (when (qq-user--friend-p)
    (user-error "qq: this user is already a friend"))
  (let ((state (or qq-user--friend-add-state
                   (user-error
                    "qq: add friends from an exact global search result"))))
    (pcase (qq-user--friend-add-state-status state)
      ('candidate (qq-user--prepare-friend-add state))
      ('prepared (qq-user--submit-friend-add state))
      ((or 'preparing 'submitting)
       (user-error "qq: friend request is already in progress"))
      ('submitted (message "qq: 好友申请已发送"))
      ('error
       (user-error "qq: friend request expired; search this user again"))
      (status (error "qq: invalid friend-add UI state %S" status)))))

(defun qq-user--insert-action-buttons ()
  "Insert the primary Telega-style user action row."
  (insert "  ")
  (appkit-ui-insert-action-button
   " 发消息 " #'qq-user-open-chat
   :face 'qq-user-action-button :help-echo "打开私聊 (m)")
  (when (and (not (qq-user--self-p))
             (not (qq-user--friend-p))
             (qq-user--friend-add-button-label))
    (insert "  ")
    (appkit-ui-insert-action-button
     (qq-user--friend-add-button-label)
     #'qq-user-add-friend
     :face 'qq-user-action-button
     :help-echo "准备或提交好友申请 (+)"))
  (unless (qq-user--self-p)
    (insert "  ")
    (appkit-ui-insert-action-button
     (cond (qq-user--send-like-request-owner " 点赞中… ")
           ((qq-user--like-limit-reached-p) " 今日已达上限 ")
           (t " 点赞 "))
     #'qq-user-like
     :face 'qq-user-action-button
     :help-echo (if (qq-user--like-limit-reached-p)
                    "今日对该用户的资料点赞已达上限"
                  "给资料卡点赞 (l)")))
  (insert "  ")
  (appkit-ui-insert-action-button
   " 查看头像 " #'qq-user-open-avatar
   :face 'qq-user-action-button :help-echo "查看头像 (a)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 照片墙 " #'qq-user-open-photo-wall
   :face 'qq-user-action-button :help-echo "打开照片墙 (p)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 复制 QQ " #'qq-user-copy-id
   :face 'qq-user-action-button :help-echo "复制 QQ 号 (w)")
  (insert "\n"))

(defun qq-user-render ()
  "Render the current user profile buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-user--header-line)))
       (cond
        ((and qq-user--loading (null qq-user--search-result))
         (appkit-view-insert-note-line "Loading user profile…"))
        ((and qq-user--error (null qq-user--search-result))
         (appkit-view-insert-note-line qq-user--error :face 'error))
        ((and (null qq-user--profile) (null qq-user--search-result))
         (appkit-view-insert-note-line "No user profile loaded."))
        (t
         (let ((avatar-start (point)))
           (insert (qq-user--avatar-display-string))
           (make-text-button
            avatar-start (point)
            'follow-link t
            'action (lambda (_button) (qq-user-open-avatar))
            'help-echo "查看头像"
            'qq-user-id qq-user--user-id)
           (insert "  "
                   (propertize (qq-user--display-name)
                               'face 'qq-user-card-title)
                   "\n")
           (when-let* ((status (qq-user--status-label
                                (alist-get 'status qq-user--profile))))
             (insert "   " (propertize status 'face 'shadow) "\n")))
         (insert "\n")
         (qq-user--insert-action-buttons)
         (qq-user--insert-friend-add-status)
         (when qq-user--loading
           (appkit-view-insert-note-line "正在加载完整用户资料…" :face 'shadow))
         (when qq-user--error
           (appkit-view-insert-note-line qq-user--error :face 'error))
         (appkit-view-insert-note-line
          (concat "g 刷新 · m 私聊"
                  (when (and qq-user--friend-add-state
                             (not (qq-user--friend-p)))
                    " · + 加好友")
                  (unless (qq-user--self-p)
                    (if (qq-user--like-limit-reached-p)
                        " · l 今日已达上限"
                      " · l 点赞"))
                  " · a 头像 · p 照片墙 · w 复制 · q 退出"))
         (insert "\n")
         (appkit-view-insert-heading-line "资料" :face 'bold)
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
         (when-let* ((relationship (alist-get 'relationship qq-user--profile))
                     (category (alist-get 'friend_category relationship))
                     (name (qq-user--present-string (alist-get 'name category))))
           (qq-user--insert-field "分组" name))
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
         (cond
          ((and (integerp qq-user--like-count)
                (>= qq-user--like-count 0))
           (qq-user--insert-field "获赞" qq-user--like-count))
          (qq-user--like-loading
           (qq-user--insert-field "获赞" "加载中…" 'shadow))
          (qq-user--like-error
           (qq-user--insert-field
            "获赞"
            (propertize "获取失败" 'help-echo qq-user--like-error)
            'error)))
         (when-let* ((labels (alist-get 'labels qq-user--profile))
                     ((listp labels))
                     ((not (null labels))))
           (qq-user--insert-field "标签" (string-join labels " · ")))
         (when-let* ((signature (qq-user--present-string
                                 (alist-get 'signature qq-user--profile))))
           (insert "\n")
           (appkit-view-insert-heading-line "个性签名" :face 'bold)
           (insert signature "\n"))
         (insert "\n")
         (qq-user--insert-photo-wall)))
       (add-text-properties
        (point-min) (point-max)
        (list 'qq-user-profile-key (qq-user--profile-key)
              'rear-nonsticky '(qq-user-profile-key)))
       (goto-char (point-min))))
   :anchor-property 'qq-user-profile-key
   :preserve-window-start t))

(defun qq-user--view-current-p (view)
  "Return non-nil when VIEW still owns this user-profile buffer."
  (and (appkit-view-live-p view)
       (equal (appkit-view-id view) qq-user--view-id)
       (with-current-buffer (appkit-view-buffer view)
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-view))))))

(defun qq-user--live-current-view ()
  "Return the live user-profile view attached to this buffer, or nil."
  (let ((view (appkit-current-view)))
    (and (qq-user--view-current-p view) view)))

(defun qq-user--live-view ()
  "Return the registered live user-profile view without starting QQ."
  (when (appkit-app-live-p qq-runtime--app)
    (when-let* ((view (appkit-view-for-id qq-runtime--app qq-user--view-id)))
      (and (qq-user--view-current-p view) view))))

(cl-defun qq-user--request-sync (&optional view &key resource)
  "Request one coalesced profile sync for live VIEW.

RESOURCE identifies a presentation-only media dependency update."
  (when-let* ((view (or view (qq-user--live-current-view))))
    (if resource
        (appkit-request-sync
         view :entry (qq-user--profile-key) :resource resource)
      (appkit-request-sync view :structure t :part 'profile))))

(defun qq-user--sync-now (view)
  "Consume pending invalidations for live user-profile VIEW."
  (when (qq-user--view-current-p view)
    (appkit-sync-invalidations view)))

(defun qq-user--sync-invalidations (view invalidations)
  "Render user profile VIEW from coalesced INVALIDATIONS."
  (when (and (qq-user--view-current-p view)
             (appkit-invalidations-any-p invalidations))
    (appkit-with-content-update view
      (qq-user-render))))

(defun qq-user--request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER still load USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-view))
              (equal qq-user--user-id user-id)
              (eq qq-user--request-owner owner)))))

(defun qq-user-refresh ()
  "Refresh the current native user profile."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (let ((view (qq-user--ensure-view)))
    (when qq-user--request
      (qq-api-cancel-request qq-user--request))
    (let ((buffer (current-buffer))
          (user-id qq-user--user-id)
          (owner (list 'user-profile qq-user--user-id)))
      (setq qq-user--loading t
            qq-user--error nil
            qq-user--request nil
            qq-user--request-owner owner)
      (qq-user--request-sync view)
      (condition-case error-data
          (let ((request
                 (qq-api-get-user
                  user-id
                  (lambda (profile)
                    (when (qq-user--request-current-p
                           view buffer user-id owner)
                      (with-current-buffer buffer
                        (setq qq-user--profile profile
                              qq-user--loading nil
                              qq-user--error nil
                              qq-user--request nil
                              qq-user--request-owner nil)
                        (qq-user--request-sync view))))
                  (lambda (response reason)
                    (when (qq-user--request-current-p
                           view buffer user-id owner)
                      (with-current-buffer buffer
                        (setq qq-user--loading nil
                              qq-user--error
                              (format "Unable to load profile: %s"
                                      (or reason "unknown error"))
                              qq-user--request nil
                              qq-user--request-owner nil)
                        (qq-user--request-sync view)
                        (qq-api--default-error response reason)))))))
            (when (eq qq-user--request-owner owner)
              (setq qq-user--request request)))
        (error
         (when (qq-user--request-current-p view buffer user-id owner)
           (setq qq-user--loading nil
                 qq-user--error
                 (format "Unable to load profile: %s"
                         (error-message-string error-data))
                 qq-user--request nil
                 qq-user--request-owner nil)
           (qq-user--request-sync view)))))
    (qq-user--refresh-like view)
    (qq-user--refresh-photos view)
    (qq-user--sync-now view)))

(defun qq-user--like-request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER load likes for USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-view))
              (equal qq-user--user-id user-id)
              (eq qq-user--like-request-owner owner)))))

(defun qq-user--refresh-like (&optional view)
  "Refresh received profile-like count using the current user VIEW."
  (setq view (or view (qq-user--ensure-view)))
  (when qq-user--like-request
    (qq-api-cancel-request qq-user--like-request))
  (let ((buffer (current-buffer))
        (user-id qq-user--user-id)
        (owner (list 'user-like qq-user--user-id)))
    (setq qq-user--like-count nil
          qq-user--like-loading t
          qq-user--like-error nil
          qq-user--like-request nil
          qq-user--like-request-owner owner)
    (qq-user--request-sync view)
    (let ((request
           (qq-api-get-user-like
            user-id
            (lambda (count)
              (when (qq-user--like-request-current-p
                     view buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--like-count count
                        qq-user--like-loading nil
                        qq-user--like-error nil
                        qq-user--like-request nil
                        qq-user--like-request-owner nil)
                  (qq-user--request-sync view))))
            (lambda (_response reason)
              (when (qq-user--like-request-current-p
                     view buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--like-count nil
                        qq-user--like-loading nil
                        qq-user--like-error (or reason "unknown error")
                        qq-user--like-request nil
                        qq-user--like-request-owner nil)
                  (qq-user--request-sync view)))))))
      (when (eq qq-user--like-request-owner owner)
        (setq qq-user--like-request request)))))

(defun qq-user--photo-request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER load photos for USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-view))
              (equal qq-user--user-id user-id)
              (eq qq-user--photo-request-owner owner)))))

(defun qq-user--refresh-photos (&optional view)
  "Refresh inline photo-wall entries using the current user VIEW."
  (setq view (or view (qq-user--ensure-view)))
  (when qq-user--photo-request
    (qq-api-cancel-request qq-user--photo-request))
  (let ((buffer (current-buffer))
        (user-id qq-user--user-id)
        (owner (list 'user-photos qq-user--user-id)))
    (setq qq-user--photos nil
          qq-user--photo-loading t
          qq-user--photo-loaded nil
          qq-user--photo-request nil
          qq-user--photo-request-owner owner)
    (qq-user--request-sync view)
    (let ((request
           (qq-api-get-user-photo-wall
            user-id
            (lambda (photos)
              (when (qq-user--photo-request-current-p
                     view buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--photos photos
                        qq-user--photo-loading nil
                        qq-user--photo-loaded t
                        qq-user--photo-request nil
                        qq-user--photo-request-owner nil)
                  (qq-user--request-sync view))))
            (lambda (_response _reason)
              (when (qq-user--photo-request-current-p
                     view buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user--photos nil
                        qq-user--photo-loading nil
                        qq-user--photo-loaded nil
                        qq-user--photo-request nil
                        qq-user--photo-request-owner nil)
                  (qq-user--request-sync view)))))))
      (when (eq qq-user--photo-request-owner owner)
        (setq qq-user--photo-request request)))))

(defun qq-user-open-chat ()
  "Open a private chat with the current profile user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (qq-chat-open (qq-state-session-key 'private qq-user--user-id)))

(defun qq-user--send-like-request-current-p (view buffer user-id owner)
  "Return non-nil when VIEW and OWNER still like USER-ID in BUFFER."
  (and (qq-user--view-current-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-mode)
              (eq view (appkit-current-view))
              (equal qq-user--user-id user-id)
              (eq qq-user--send-like-request-owner owner)))))

(defun qq-user-like ()
  "Add one native QQ profile like to the current user."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (when (qq-user--self-p)
    (user-error "qq: cannot like your own profile"))
  (when (qq-user--like-limit-reached-p)
    (user-error "qq: 今日对该用户的资料点赞已达上限"))
  (when qq-user--send-like-request-owner
    (user-error "qq: profile like is already in progress"))
  (let ((view (qq-user--ensure-view)))
    (let ((buffer (current-buffer))
          (user-id qq-user--user-id)
          (owner (list 'send-user-like qq-user--user-id)))
      (setq qq-user--send-like-request nil
            qq-user--send-like-request-owner owner)
      (qq-user--request-sync view)
      (let ((request
             (qq-api-like-user
              user-id
              (lambda (result)
                (when (qq-user--send-like-request-current-p
                       view buffer user-id owner)
                  (with-current-buffer buffer
                    (setq qq-user--send-like-request nil
                          qq-user--send-like-request-owner nil)
                    (pcase (alist-get 'outcome result)
                      ("liked"
                       (qq-user--refresh-like)
                       (qq-user--request-sync view)
                       (message "qq: 已给 %s 的资料新增 1 个赞"
                                (qq-user--display-name)))
                      ("daily_limit"
                       (setq qq-user--like-limit-date
                             (format-time-string "%Y-%m-%d"))
                       (qq-user--request-sync view)
                       (message "qq: 今日对 %s 的资料点赞已达上限"
                                (qq-user--display-name)))))))
              (lambda (response reason)
                (when (qq-user--send-like-request-current-p
                       view buffer user-id owner)
                  (with-current-buffer buffer
                    (setq qq-user--send-like-request nil
                          qq-user--send-like-request-owner nil)
                    (qq-user--request-sync view)
                    (qq-api--default-error response reason)))))))
        (when (eq qq-user--send-like-request-owner owner)
          (setq qq-user--send-like-request request))))
    (qq-user--sync-now view)))

(defun qq-user-open-avatar ()
  "Open the current profile user's avatar."
  (interactive)
  (if-let* ((url (qq-user--present-string
                  (alist-get 'avatar_url qq-user--search-result))))
      (qq-media-open-image-url (format "avatar:%s" qq-user--user-id) url)
    (qq-media-open-user-avatar qq-user--user-id)))

(defun qq-user-copy-id ()
  "Copy the current profile user's QQ number."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (kill-new qq-user--user-id)
  (message "qq: copied user id %s" qq-user--user-id))

(defun qq-user-open-photo-wall ()
  "Open the current user's native QQ photo wall."
  (interactive)
  (unless qq-user--user-id
    (user-error "qq: this buffer has no user identity"))
  (qq-user-photo-open qq-user--user-id (qq-user--display-name)))

(defun qq-user-open-photo-at-point ()
  "Open inline native photo at point."
  (interactive)
  (let ((photo (qq-user--photo-at-point)))
    (unless photo
      (user-error "qq: no profile photo at point"))
    (qq-user-photo-open-entry qq-user--user-id photo)))

(defun qq-user--cancel-request ()
  "Cancel asynchronous work owned by the current user view."
  (when qq-user--request
    (qq-api-cancel-request qq-user--request))
  (when qq-user--like-request
    (qq-api-cancel-request qq-user--like-request))
  (when qq-user--send-like-request
    (qq-api-cancel-request qq-user--send-like-request))
  (when qq-user--photo-request
    (qq-api-cancel-request qq-user--photo-request))
  (when-let* ((state qq-user--friend-add-state)
              (request (qq-user--friend-add-state-request state)))
    (qq-api-cancel-request request))
  (setq qq-user--request nil
        qq-user--request-owner nil
        qq-user--like-request nil
        qq-user--like-request-owner nil
        qq-user--send-like-request nil
        qq-user--send-like-request-owner nil
        qq-user--photo-request nil
        qq-user--photo-request-owner nil
        qq-user--loading nil
        qq-user--like-loading nil
        qq-user--photo-loading nil
        qq-user--search-result nil
        qq-user--friend-add-state nil))

(defun qq-user--clear-view-data ()
  "Clear account-scoped data projected by the current user view."
  (setq qq-user--profile nil
        qq-user--error nil
        qq-user--like-count nil
        qq-user--like-error nil
        qq-user--like-limit-date nil
        qq-user--photos nil
        qq-user--photo-loaded nil
        qq-user--search-result nil
        qq-user--friend-add-state nil))

(defun qq-user--reset-buffer-work (buffer)
  "Reset requests, data, and media hook state retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-user-mode)
        (when qq-user--media-hook-function
          (remove-hook 'qq-media-cache-update-hook
                       qq-user--media-hook-function))
        (setq qq-user--media-hook-function nil)
        (qq-user--cancel-request)
        (qq-user--clear-view-data)))))

(defun qq-user--release-view-work (view buffer)
  "Release BUFFER work when it is still owned by user-profile VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view (appkit-current-view))))
    (qq-user--reset-buffer-work buffer)))

(defun qq-user--setup-view (view)
  "Reset replacement state and register lifecycle work for user VIEW."
  (let ((buffer (appkit-view-buffer view)))
    (qq-user--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-user--release-view-work view buffer))
    (let ((hook (apply-partially
                 #'qq-user--handle-media-cache-update view)))
      (with-current-buffer buffer
        (setq qq-user--media-hook-function hook))
      (appkit-register-handle
       view 'hook
       (list 'qq-media-cache-update-hook hook nil buffer))
      (add-hook 'qq-media-cache-update-hook hook))))

(defun qq-user--ensure-view ()
  "Return the live Appkit view owning the current user buffer."
  (unless qq-user--user-id
    (error "QQ: cannot attach a user view without an opaque user identity"))
  (let* ((app (qq-runtime-app))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal qq-user--view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-user--sync-invalidations
            (appkit-view-parts current) '(profile))
      current)
     ((appkit-view-live-p current)
      (error "QQ: user buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id qq-user--view-id
              :mode 'qq-user-mode
              :sync-function #'qq-user--sync-invalidations
              :parts '(profile))))
        (qq-user--setup-view view)
        view)))))

(defun qq-user--select-user (user-id)
  "Prepare the shared user buffer to display USER-ID."
  (unless (equal qq-user--user-id user-id)
    (qq-user--cancel-request)
    (qq-user--clear-view-data))
  (setq qq-user--user-id user-id))

(defun qq-user-button-backward ()
  "Move point to the previous page button."
  (interactive)
  (forward-button -1))

(defvar qq-user-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-user-refresh)
    (define-key map (kbd "m") #'qq-user-open-chat)
    (define-key map (kbd "l") #'qq-user-like)
    (define-key map (kbd "+") #'qq-user-add-friend)
    (define-key map (kbd "a") #'qq-user-open-avatar)
    (define-key map (kbd "p") #'qq-user-open-photo-wall)
    (define-key map (kbd "RET") #'qq-user-open-photo-at-point)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-user-button-backward)
    (define-key map (kbd "w") #'qq-user-copy-id)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-user-mode'.")

(define-derived-mode qq-user-mode special-mode "QQ-User"
  "Major mode for one native QQ user profile."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local header-line-format '(:eval (qq-user--header-line)))
  (add-hook 'change-major-mode-hook #'qq-user--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-user--cancel-request nil t))

;;;###autoload
(defun qq-user-open (user-id)
  "Open the native user profile for decimal string USER-ID."
  (interactive "sQQ number: ")
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: user profile requires a decimal string user id"))
  (let* ((app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id qq-user--view-id
           :mode 'qq-user-mode
           :buffer-name (qq-user--buffer-name user-id)
           :sync-function #'qq-user--sync-invalidations
           :parts '(profile)
           :setup #'qq-user--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (qq-user--select-user user-id)
      (when (and (null qq-user--profile)
                 (not qq-user--loading))
        (qq-user-refresh))
      (qq-user--request-sync view)
      (qq-user--sync-now view))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user--global-search-result-p (result)
  "Return non-nil when RESULT is an exact validated global-user row."
  (and (listp result)
       (proper-list-p result)
       (= (length result) 5)
       (cl-every (lambda (cell)
                   (and (consp cell)
                        (memq (car cell)
                              '(user_id uid nickname avatar_url candidate))))
                 result)
       (= (length (delete-dups (mapcar #'car result))) 5)
       (let ((user-id (alist-get 'user_id result))
             (uid (alist-get 'uid result))
             (candidate (alist-get 'candidate result)))
         (and (stringp user-id)
              (string-match-p "\\`[1-9][0-9]*\\'" user-id)
              (or (null uid) (qq-api-non-empty-string-p uid))
              (stringp (alist-get 'nickname result))
              (qq-api-non-empty-string-p (alist-get 'avatar_url result))
              (stringp candidate)
              (string-match-p
               "\\`[A-Za-z0-9_-]\\{43\\}\\'" candidate)))))

;;;###autoload
(defun qq-user-open-search-result (result)
  "Open the exact global-user search RESULT with its one-use candidate."
  (unless (qq-user--global-search-result-p result)
    (error "qq: invalid global-user search result"))
  (let* ((copy (copy-tree result))
         (user-id (alist-get 'user_id copy))
         (app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id qq-user--view-id
           :mode 'qq-user-mode
           :buffer-name (qq-user--buffer-name user-id)
           :sync-function #'qq-user--sync-invalidations
           :parts '(profile)
           :setup #'qq-user--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (qq-user--select-user user-id)
      (when-let* ((old qq-user--friend-add-state)
                  (request (qq-user--friend-add-state-request old)))
        (qq-api-cancel-request request))
      (setq qq-user--search-result copy
            qq-user--friend-add-state
            (qq-user--friend-add-state-create
             :user-id user-id
             :candidate (alist-get 'candidate copy)
             :status 'candidate))
      (when (and (null qq-user--profile)
                 (not qq-user--loading))
        (qq-user-refresh))
      (qq-user--request-sync view)
      (qq-user--sync-now view))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user--handle-media-cache-update (view media-key)
  "Request a targeted VIEW update after MEDIA-KEY changes."
  (when (and (stringp media-key) (qq-user--view-current-p view))
    (with-current-buffer (appkit-view-buffer view)
      (when (or (equal media-key (format "avatar:%s" qq-user--user-id))
                (string-prefix-p
                 (format "photo-wall:%s:" qq-user--user-id)
                 media-key))
        (qq-user--request-sync view :resource media-key)))))

(provide 'qq-user)

;;; qq-user.el ends here
