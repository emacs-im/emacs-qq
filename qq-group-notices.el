;;; qq-group-notices.el --- Read-only QQ group notices -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A small read-only view backed by the strict `emacs_get_group_notices'
;; action.  Announcement mutations intentionally remain outside this module.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'qq-api)
(require 'appkit-position)
(require 'appkit-view)

(declare-function qq-user-open "qq-user" (user-id))

(defface qq-group-notices-title
  '((t :inherit bold :height 1.1))
  "Face used for group-notice titles."
  :group 'qq)

(defvar-local qq-group-notices--group-id nil
  "Group whose announcements are displayed in this buffer.")

(defvar-local qq-group-notices--group-name nil
  "Optional presentation name of the current group.")

(defvar-local qq-group-notices--items nil
  "Closed announcement objects currently displayed.")

(defvar-local qq-group-notices--loading nil
  "Non-nil while announcements are loading.")

(defvar-local qq-group-notices--error nil
  "Last announcement loading error, or nil.")

(defvar-local qq-group-notices--request nil
  "Active announcement request token.")

(defvar-local qq-group-notices--request-owner nil
  "Opaque owner of the active announcement request.")

(defun qq-group-notices--buffer-name ()
  "Return the shared announcement buffer name."
  "*qq-group-notices*")

(defun qq-group-notices--header-line ()
  "Return the dynamic header line for this announcement view."
  (format " QQ Group Notices · %s (%s)%s"
          (or qq-group-notices--group-name qq-group-notices--group-id "QQ group")
          (or qq-group-notices--group-id "unknown")
          (if qq-group-notices--loading " · loading" "")))

(defun qq-group-notices--timestamp-label (timestamp)
  "Format non-negative integer TIMESTAMP for display."
  (when (and (integerp timestamp) (>= timestamp 0))
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time timestamp))))

(defun qq-group-notices--open-sender (button)
  "Open the sender stored on BUTTON."
  (let ((sender-id (button-get button 'qq-user-id)))
    (unless (qq-api-user-id-p sender-id)
      (user-error "qq: announcement sender is unavailable"))
    (require 'qq-user)
    (qq-user-open sender-id)))

(defun qq-group-notices--insert-sender (sender-id)
  "Insert clickable SENDER-ID, or an unavailable marker."
  (if (not sender-id)
      (insert "发布者未知")
    (let ((start (point)))
      (insert sender-id)
      (make-text-button
       start (point)
       'follow-link t
       'action #'qq-group-notices--open-sender
       'help-echo "打开发布者资料"
       'qq-user-id sender-id))))

(defun qq-group-notices--image-label (image index)
  "Return a compact label for notice IMAGE at INDEX."
  (let ((width (alist-get 'width image))
        (height (alist-get 'height image)))
    (if (and (integerp width) (integerp height))
        (format "[图片 %d · %d×%d]" index width height)
      (format "[图片 %d]" index))))

(defun qq-group-notices--insert-item (notice index)
  "Insert announcement NOTICE at one-based INDEX."
  (let ((title (alist-get 'title notice))
        (sender-id (alist-get 'sender_id notice))
        (read-count (alist-get 'read_count notice))
        (images (alist-get 'images notice)))
    (appkit-view-insert-heading-line
     (or (and (stringp title) (not (string-empty-p title)) title)
         (format "群公告 %d" index))
     :face 'qq-group-notices-title)
    (let ((meta-start (point)))
      (insert (or (qq-group-notices--timestamp-label
                   (alist-get 'published_at notice))
                  "时间未知")
              " · ")
      (qq-group-notices--insert-sender sender-id)
      (when (integerp read-count)
        (insert (format " · %d 人已读" read-count)))
      (when (eq (alist-get 'confirmation_required notice) t)
        (insert (if (eq (alist-get 'all_confirmed notice) t)
                    " · 已全部确认"
                  " · 需要确认")))
      (insert "\n")
      (add-text-properties meta-start (point) '(face shadow)))
    (let ((text (alist-get 'text notice)))
      (unless (string-empty-p text)
        (insert text "\n")))
    (when images
      (cl-loop for image in images
               for image-index from 1
               do (insert (qq-group-notices--image-label image image-index) " "))
      (insert "\n"))
    (insert "\n")))

(defun qq-group-notices-render ()
  "Render the current read-only announcement view."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-group-notices--header-line)))
       (cond
        (qq-group-notices--loading
         (appkit-view-insert-note-line "正在加载群公告…"))
        (qq-group-notices--error
         (appkit-view-insert-note-line qq-group-notices--error :face 'error))
        ((null qq-group-notices--items)
         (appkit-view-insert-note-line "暂无群公告。"))
        (t
         (appkit-view-insert-note-line "g 刷新 · RET/TAB 打开发布者 · q 退出")
         (insert "\n")
         (cl-loop for notice in qq-group-notices--items
                  for index from 1
                  do (qq-group-notices--insert-item notice index))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-group-notices--request-current-p (buffer group-id owner)
  "Return non-nil when OWNER still loads GROUP-ID in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-notices-mode)
              (equal qq-group-notices--group-id group-id)
              (eq qq-group-notices--request-owner owner)))))

(defun qq-group-notices-refresh ()
  "Refresh the current group's read-only announcement list."
  (interactive)
  (unless qq-group-notices--group-id
    (user-error "qq: this buffer has no group identity"))
  (when qq-group-notices--request
    (qq-api-cancel-request qq-group-notices--request))
  (let ((buffer (current-buffer))
        (group-id qq-group-notices--group-id)
        (owner (list 'group-notices qq-group-notices--group-id)))
    (setq qq-group-notices--loading t
          qq-group-notices--error nil
          qq-group-notices--request nil
          qq-group-notices--request-owner owner)
    (qq-group-notices-render)
    (condition-case error-data
        (let ((request
               (qq-api-get-group-notices
                group-id
                (lambda (notices)
                  (when (qq-group-notices--request-current-p
                         buffer group-id owner)
                    (with-current-buffer buffer
                      (setq qq-group-notices--items notices
                            qq-group-notices--loading nil
                            qq-group-notices--error nil
                            qq-group-notices--request nil
                            qq-group-notices--request-owner nil)
                      (qq-group-notices-render))))
                (lambda (response reason)
                  (when (qq-group-notices--request-current-p
                         buffer group-id owner)
                    (with-current-buffer buffer
                      (setq qq-group-notices--loading nil
                            qq-group-notices--error
                            (format "无法加载群公告：%s"
                                    (or reason "未知错误"))
                            qq-group-notices--request nil
                            qq-group-notices--request-owner nil)
                      (qq-group-notices-render)))
                  (qq-api--default-error response reason)))))
          (when (eq qq-group-notices--request-owner owner)
            (setq qq-group-notices--request request)))
      (error
       (when (qq-group-notices--request-current-p buffer group-id owner)
         (setq qq-group-notices--loading nil
               qq-group-notices--error
               (format "无法加载群公告：%s"
                       (error-message-string error-data))
               qq-group-notices--request nil
               qq-group-notices--request-owner nil)
         (qq-group-notices-render))))))

(defun qq-group-notices--cancel-request ()
  "Cancel and forget the active announcement request."
  (when qq-group-notices--request
    (qq-api-cancel-request qq-group-notices--request))
  (setq qq-group-notices--request nil
        qq-group-notices--request-owner nil))

(defun qq-group-notices-button-backward ()
  "Move point to the previous announcement button."
  (interactive)
  (forward-button -1))

(defvar qq-group-notices-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-group-notices-refresh)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-group-notices-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-group-notices-mode'.")

(define-derived-mode qq-group-notices-mode special-mode "QQ-Group-Notices"
  "Major mode for a read-only QQ group announcement list."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (add-hook 'change-major-mode-hook #'qq-group-notices--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-group-notices--cancel-request nil t))

;;;###autoload
(defun qq-group-notices-open (group-id &optional group-name)
  "Open read-only announcements for GROUP-ID and optional GROUP-NAME."
  (interactive "sQQ group number: ")
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group notices require a canonical uint32 group id"))
  (let ((buffer (get-buffer-create (qq-group-notices--buffer-name))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-group-notices-mode)
        (qq-group-notices-mode))
      (unless (equal qq-group-notices--group-id group-id)
        (qq-group-notices--cancel-request)
        (setq qq-group-notices--items nil
              qq-group-notices--loading nil
              qq-group-notices--error nil))
      (setq qq-group-notices--group-id group-id
            qq-group-notices--group-name group-name)
      (qq-group-notices-render)
      (when (and (null qq-group-notices--items)
                 (not qq-group-notices--loading))
        (qq-group-notices-refresh)))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-group-notices)

;;; qq-group-notices.el ends here
