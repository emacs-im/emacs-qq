;;; qq-red-packet.el --- Native QQ red-packet views -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A closed view over Linux QQ's wallet APIs.  Message capabilities remain in
;; NapCat; Emacs identifies a packet only by session and exact NT message ID.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-api)
(require 'qq-state)
(require 'appkit-position)
(require 'appkit-ui)
(require 'appkit-view)

(autoload 'qq-user-open "qq-user" nil t)
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-api-cancel-request "qq-api" (request-token))
(declare-function qq-api-get-red-packet-detail
                  "qq-api"
                  (session-key message-id callback &optional errback))
(declare-function qq-api-grab-red-packet
                  "qq-api"
                  (session-key message-id callback &optional errback))

(defface qq-red-packet-title
  '((t :inherit bold :height 1.2))
  "Face used for the primary red-packet title."
  :group 'qq)

(defface qq-red-packet-action-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face used for red-packet page actions."
  :group 'qq)

(defvar-local qq-red-packet--session-key nil)
(defvar-local qq-red-packet--message-id nil)
(defvar-local qq-red-packet--segment nil)
(defvar-local qq-red-packet--outgoing-p nil)
(defvar-local qq-red-packet--detail nil)
(defvar-local qq-red-packet--loading nil)
(defvar-local qq-red-packet--error nil)
(defvar-local qq-red-packet--request nil)
(defvar-local qq-red-packet--request-owner nil)
(defvar-local qq-red-packet--grabbing nil)
(defvar-local qq-red-packet--grab-request nil)
(defvar-local qq-red-packet--grab-owner nil)
(defvar-local qq-red-packet--notice nil)

(defun qq-red-packet--kind ()
  "Return the closed wallet kind of the current packet."
  (alist-get 'wallet_kind (alist-get 'data qq-red-packet--segment)))

(defun qq-red-packet--password-p ()
  "Return non-nil when the current packet is a password packet."
  (equal (qq-red-packet--kind) "password-red-packet"))

(defun qq-red-packet--detail-supported-p ()
  "Return non-nil when Linux QQ exposes detail for this packet kind."
  (equal (qq-red-packet--kind) "red-packet"))

(defun qq-red-packet--kind-label ()
  "Return the user-facing packet kind label."
  (if (qq-red-packet--password-p) "口令红包" "QQ 红包"))

(defun qq-red-packet--presentation ()
  "Return the non-empty presentation carried by the current packet."
  (let* ((data (alist-get 'data qq-red-packet--segment))
         (receiver (alist-get 'receiver data))
         (sender (alist-get 'sender data)))
    (if (seq-some (lambda (key)
                    (let ((value (alist-get key receiver)))
                      (and (stringp value) (not (string-empty-p value)))))
                  '(title sub_title content notice))
        receiver
      sender)))

(defun qq-red-packet--title ()
  "Return the best current packet title."
  (let ((presentation (qq-red-packet--presentation)))
    (or (let ((title (alist-get 'title presentation)))
          (and (stringp title) (not (string-empty-p title)) title))
        "QQ 红包")))

(defun qq-red-packet--format-amount (value)
  "Format decimal minor-unit VALUE exactly as a yuan amount."
  (if (and (stringp value) (string-match-p "\\`[0-9]+\\'" value))
      (let* ((padded (concat (make-string (max 0 (- 3 (length value))) ?0)
                             value))
             (split (- (length padded) 2)))
        (format "¥%s.%s" (substring padded 0 split) (substring padded split)))
    (format "%s" value)))

(defun qq-red-packet--format-time (seconds)
  "Format non-negative epoch SECONDS for the packet page."
  (if (and (integerp seconds) (> seconds 0))
      (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time seconds))
    "—"))

(defun qq-red-packet--insert-field (label value &optional face)
  "Insert packet LABEL and VALUE when VALUE is present."
  (when (and value (not (equal value "")))
    (let ((start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties start (point) '(face bold)))
    (let ((start (point)))
      (insert (format "%s" value) "\n")
      (when face (add-text-properties start (point) (list 'face face))))))

(defun qq-red-packet--claimed-by-self-p ()
  "Return non-nil when loaded detail contains the current account receipt."
  (let ((self-id (qq-state-self-user-id)))
    (and self-id qq-red-packet--detail
         (seq-some (lambda (receipt)
                     (equal (alist-get 'user_id receipt) self-id))
                   (alist-get 'receipts qq-red-packet--detail)))))

(defun qq-red-packet--insert-actions ()
  "Insert the page action row."
  (insert "  ")
  (when (qq-red-packet--detail-supported-p)
    (appkit-ui-insert-action-button
     (if qq-red-packet--loading " 刷新中… " " 刷新详情 ")
     #'qq-red-packet-refresh
     :face 'qq-red-packet-action-button
     :help-echo "刷新红包详情 (g)"))
  (when (and (not qq-red-packet--outgoing-p)
             (not (qq-red-packet--claimed-by-self-p)))
    (insert "  ")
    (appkit-ui-insert-action-button
     (if qq-red-packet--grabbing
         " 领取中… "
       (if (qq-red-packet--password-p) " 领取口令红包 " " 领取红包 "))
     #'qq-red-packet-grab
     :face 'qq-red-packet-action-button
     :help-echo (if (qq-red-packet--password-p)
                    "发送口令文本并领取这个红包 (c)"
                  "显式领取这个红包 (c)")))
  (insert "\n"))

(defun qq-red-packet--insert-receipt (receipt)
  "Insert one interactive packet RECEIPT row."
  (let* ((user-id (alist-get 'user_id receipt))
         (name (or (alist-get 'name receipt) user-id))
         (start (point)))
    (appkit-ui-insert-action-button
     name (lambda () (qq-user-open user-id))
     :face 'qq-msg-user-title
     :help-echo (format "打开 %s 的资料" name))
    (insert (format "  %s  %s\n"
                    (qq-red-packet--format-amount
                     (alist-get 'amount receipt))
                    (qq-red-packet--format-time
                     (alist-get 'received_at receipt))))
    (add-text-properties start (point) '(rear-nonsticky t))))

(defun qq-red-packet-render ()
  "Render the current red-packet detail buffer."
  (interactive)
  (appkit-position-render-preserving
   (lambda ()
     (let ((inhibit-read-only t)
           (presentation (qq-red-packet--presentation)))
       (erase-buffer)
       (setq-local header-line-format
                   '(:eval (format " %s · %s"
                                   (qq-red-packet--kind-label)
                                   (qq-red-packet--title))))
       (insert (propertize (format "🧧  %s" (qq-red-packet--kind-label))
                           'face 'qq-red-packet-title)
               "\n")
       (insert "   " (qq-red-packet--title) "\n")
       (when-let* ((subtitle (alist-get 'sub_title presentation))
                   ((stringp subtitle))
                   ((not (string-empty-p subtitle))))
         (insert "   " (propertize subtitle 'face 'shadow) "\n"))
       (insert "\n")
       (qq-red-packet--insert-actions)
       (appkit-view-insert-note-line
        (if (qq-red-packet--detail-supported-p)
            "g 刷新详情 · c 领取 · TAB 切换按钮 · q 退出"
          "c 领取 · TAB 切换按钮 · q 退出"))
       (when qq-red-packet--notice
         (appkit-view-insert-note-line qq-red-packet--notice :face 'success))
       (when qq-red-packet--loading
         (appkit-view-insert-note-line "正在读取 QQ 原生红包详情…" :face 'shadow))
       (when qq-red-packet--grabbing
         (appkit-view-insert-note-line "正在领取红包…" :face 'shadow))
       (when qq-red-packet--error
         (appkit-view-insert-note-line qq-red-packet--error :face 'error))
       (when qq-red-packet--detail
         (let ((order (alist-get 'send_order qq-red-packet--detail)))
           (insert "\n")
           (appkit-view-insert-heading-line "红包详情" :face 'bold)
           (qq-red-packet--insert-field "发送者" (alist-get 'sender_name order))
           (qq-red-packet--insert-field "祝福语" (alist-get 'wishing order))
           (qq-red-packet--insert-field
            "金额"
            (format "%s / %s"
                    (qq-red-packet--format-amount
                     (alist-get 'received_amount order))
                    (qq-red-packet--format-amount
                     (alist-get 'total_amount order))))
           (qq-red-packet--insert-field
            "领取"
            (format "%s / %s"
                    (alist-get 'received_count order)
                    (alist-get 'total_count order)))
           (qq-red-packet--insert-field
            "发送时间" (qq-red-packet--format-time
                         (alist-get 'created_at order)))
           (qq-red-packet--insert-field
            "失效时间" (qq-red-packet--format-time
                         (alist-get 'expires_at order)))
           (qq-red-packet--insert-field "原生状态" (alist-get 'state order))
           (when-let* ((receipts (alist-get 'receipts qq-red-packet--detail))
                       ((not (null receipts))))
             (insert "\n")
             (appkit-view-insert-heading-line "领取记录" :face 'bold)
             (dolist (receipt receipts)
               (qq-red-packet--insert-receipt receipt)))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-red-packet--request-current-p (buffer owner)
  "Return non-nil when BUFFER still owns detail request OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-red-packet-mode)
              (eq qq-red-packet--request-owner owner)))))

(defun qq-red-packet-refresh ()
  "Refresh the current packet's read-only detail."
  (interactive)
  (unless (qq-red-packet--detail-supported-p)
    (user-error "qq: Linux QQ 不提供口令红包详情"))
  (when qq-red-packet--request
    (qq-api-cancel-request qq-red-packet--request))
  (let ((buffer (current-buffer))
        (owner (list 'red-packet-detail qq-red-packet--message-id)))
    (setq qq-red-packet--loading t
          qq-red-packet--error nil
          qq-red-packet--request nil
          qq-red-packet--request-owner owner)
    (qq-red-packet-render)
    (let ((request
           (qq-api-get-red-packet-detail
            qq-red-packet--session-key qq-red-packet--message-id
            (lambda (detail)
              (when (qq-red-packet--request-current-p buffer owner)
                (with-current-buffer buffer
                  (setq qq-red-packet--detail detail
                        qq-red-packet--loading nil
                        qq-red-packet--error nil
                        qq-red-packet--request nil
                        qq-red-packet--request-owner nil)
                  (qq-red-packet-render))))
            (lambda (_response reason)
              (when (qq-red-packet--request-current-p buffer owner)
                (with-current-buffer buffer
                  (setq qq-red-packet--loading nil
                        qq-red-packet--error (or reason "无法获取红包详情")
                        qq-red-packet--request nil
                        qq-red-packet--request-owner nil)
                  (qq-red-packet-render)))))))
      (when (eq qq-red-packet--request-owner owner)
        (setq qq-red-packet--request request)))))

(defun qq-red-packet--grab-current-p (buffer owner)
  "Return non-nil when BUFFER still owns claim request OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-red-packet-mode)
              (eq qq-red-packet--grab-owner owner)))))

(defun qq-red-packet-grab ()
  "Explicitly claim the current native QQ red packet."
  (interactive)
  (when qq-red-packet--outgoing-p
    (user-error "qq: 不能领取自己发送的红包"))
  (when qq-red-packet--grabbing
    (user-error "qq: 红包领取请求正在进行"))
  (unless (yes-or-no-p
           (if (qq-red-packet--password-p)
               (format "发送口令“%s”并领取红包？ " (qq-red-packet--title))
             (format "领取红包“%s”？ " (qq-red-packet--title))))
    (user-error "qq: 已取消领取红包"))
  (let ((buffer (current-buffer))
        (owner (list 'red-packet-grab qq-red-packet--message-id)))
    (setq qq-red-packet--grabbing t
          qq-red-packet--error nil
          qq-red-packet--notice nil
          qq-red-packet--grab-request nil
          qq-red-packet--grab-owner owner)
    (qq-red-packet-render)
    (let ((request
           (qq-api-grab-red-packet
            qq-red-packet--session-key qq-red-packet--message-id
            (lambda (result)
              (when (qq-red-packet--grab-current-p buffer owner)
                (with-current-buffer buffer
                  (setq qq-red-packet--grabbing nil
                        qq-red-packet--grab-request nil
                        qq-red-packet--grab-owner nil)
                  (if (equal (alist-get 'interaction result) "password")
                      (progn
                        (setq qq-red-packet--notice "口令红包领取请求已提交")
                        (message "qq: 口令红包领取请求已提交")
                        (qq-red-packet-render))
                    (message "qq: 红包领取请求已完成")
                    (qq-red-packet-refresh)))))
            (lambda (_response reason)
              (when (qq-red-packet--grab-current-p buffer owner)
                (with-current-buffer buffer
                  (setq qq-red-packet--grabbing nil
                        qq-red-packet--error (or reason "领取红包失败")
                        qq-red-packet--grab-request nil
                        qq-red-packet--grab-owner nil)
                  (qq-red-packet-render)))))))
      (when (eq qq-red-packet--grab-owner owner)
        (setq qq-red-packet--grab-request request)))))

(defun qq-red-packet--cancel-requests ()
  "Cancel local ownership of all current packet requests."
  (when qq-red-packet--request
    (qq-api-cancel-request qq-red-packet--request))
  (when qq-red-packet--grab-request
    (qq-api-cancel-request qq-red-packet--grab-request))
  (setq qq-red-packet--request nil
        qq-red-packet--request-owner nil
        qq-red-packet--grab-request nil
        qq-red-packet--grab-owner nil))

(defvar qq-red-packet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-red-packet-refresh)
    (define-key map (kbd "c") #'qq-red-packet-grab)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") (lambda () (interactive) (forward-button -1)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-red-packet-mode'.")

(define-derived-mode qq-red-packet-mode special-mode "QQ-Red-Packet"
  "Major mode for one native QQ red packet."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (add-hook 'kill-buffer-hook #'qq-red-packet--cancel-requests nil t))

;;;###autoload
(defun qq-red-packet-open (session-key message-id segment outgoing-p)
  "Open native red packet SEGMENT for MESSAGE-ID in SESSION-KEY."
  (qq-api-validate-message-id message-id "red-packet view")
  (unless (and (equal (alist-get 'type segment) "wallet")
               (member (alist-get 'wallet_kind (alist-get 'data segment))
                       '("red-packet" "password-red-packet")))
    (error "qq: red-packet view requires an interactive wallet kind"))
  (let ((buffer (get-buffer-create (format "*qq-red-packet:%s*" message-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-red-packet-mode)
        (qq-red-packet-mode))
      (qq-red-packet--cancel-requests)
      (setq qq-red-packet--session-key session-key
            qq-red-packet--message-id message-id
            qq-red-packet--segment (copy-tree segment)
            qq-red-packet--outgoing-p outgoing-p
            qq-red-packet--detail nil
            qq-red-packet--loading nil
            qq-red-packet--error nil
            qq-red-packet--notice nil
            qq-red-packet--grabbing nil)
      (qq-red-packet-render)
      (when (qq-red-packet--detail-supported-p)
        (qq-red-packet-refresh)))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-red-packet)

;;; qq-red-packet.el ends here
