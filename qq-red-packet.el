;;; qq-red-packet.el --- Native QQ red-packet views -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A closed view over Linux QQ's wallet APIs.  Message capabilities remain in
;; NapCat; Emacs identifies a packet only by session and exact NT message ID.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'qq-api)
(require 'qq-runtime)
(require 'qq-state)
(require 'appkit-position)
(require 'appkit-transaction)
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

(defconst qq-red-packet--position-property 'qq-red-packet-position-key
  "Text property carrying stable red-packet page position keys.")

(defun qq-red-packet--view-id (session-key message-id)
  "Return the Appkit identity for MESSAGE-ID in SESSION-KEY."
  (list 'red-packet session-key message-id))

(defun qq-red-packet--buffer-name (session-key message-id)
  "Return the fallback buffer name for MESSAGE-ID in SESSION-KEY."
  (format "*qq-red-packet:%s:%s*" session-key message-id))

(defun qq-red-packet--position-key (kind &optional value)
  "Return a stable current-packet position key for KIND and VALUE."
  (list 'red-packet qq-red-packet--session-key
        qq-red-packet--message-id kind value))

(defun qq-red-packet--mark-position (start key)
  "Mark text from START through point with stable position KEY."
  (add-text-properties
   start (point)
   (list qq-red-packet--position-property key 'rear-nonsticky t)))

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
  "Insert packet LABEL and VALUE when present, styling VALUE with FACE."
  (when (and value (not (equal value "")))
    (let ((row-start (point))
          (label-start (point)))
      (insert (format "%-12s" (concat label ":")))
      (add-text-properties label-start (point) '(face bold))
      (let ((value-start (point)))
        (insert (format "%s" value) "\n")
        (when face
          (add-text-properties value-start (point) (list 'face face))))
      (qq-red-packet--mark-position
       row-start (qq-red-packet--position-key 'field label)))))

(defun qq-red-packet--claimed-by-self-p ()
  "Return non-nil when loaded detail contains the current account receipt."
  (let ((self-id (qq-state-self-user-id)))
    (and self-id qq-red-packet--detail
         (seq-some (lambda (receipt)
                     (equal (alist-get 'user_id receipt) self-id))
                   (alist-get 'receipts qq-red-packet--detail)))))

(defun qq-red-packet--insert-actions ()
  "Insert the page action row."
  (let ((start (point)))
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
    (insert "\n")
    (qq-red-packet--mark-position
     start (qq-red-packet--position-key 'actions))))

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
    (qq-red-packet--mark-position
     start (qq-red-packet--position-key 'receipt user-id))))

(defun qq-red-packet--insert-note (text key &optional face)
  "Insert red-packet note TEXT with stable KEY and optional FACE."
  (appkit-view-insert-note-line
   text :face face
   :line-properties
   (list qq-red-packet--position-property
         (qq-red-packet--position-key 'note key))))

(defun qq-red-packet--render-content (view)
  "Render current red-packet state inside live Appkit VIEW."
  (let ((snapshot
         (with-current-buffer (appkit-view-buffer view)
           (appkit-position-capture
            :anchor-property qq-red-packet--position-property
            :preserve-window-start t))))
    (appkit-with-content-update view
      (let ((presentation (qq-red-packet--presentation))
            (summary-start (point)))
        (erase-buffer)
        (setq summary-start (point))
        (insert (propertize (format "🧧  %s" (qq-red-packet--kind-label))
                            'face 'qq-red-packet-title)
                "\n")
        (insert "   " (qq-red-packet--title) "\n")
        (when-let* ((subtitle (alist-get 'sub_title presentation))
                    ((stringp subtitle))
                    ((not (string-empty-p subtitle))))
          (insert "   " (propertize subtitle 'face 'shadow) "\n"))
        (insert "\n")
        (qq-red-packet--mark-position
         summary-start (qq-red-packet--position-key 'summary))
        (qq-red-packet--insert-actions)
        (qq-red-packet--insert-note
         (if (qq-red-packet--detail-supported-p)
             "g 刷新详情 · c 领取 · TAB 切换按钮 · q 退出"
           "c 领取 · TAB 切换按钮 · q 退出")
         'instructions)
        (when qq-red-packet--notice
          (qq-red-packet--insert-note
           qq-red-packet--notice 'notice 'success))
        (when qq-red-packet--loading
          (qq-red-packet--insert-note
           "正在读取 QQ 原生红包详情…" 'loading 'shadow))
        (when qq-red-packet--grabbing
          (qq-red-packet--insert-note
           "正在领取红包…" 'grabbing 'shadow))
        (when qq-red-packet--error
          (qq-red-packet--insert-note qq-red-packet--error 'error 'error))
        (when qq-red-packet--detail
          (let ((order (alist-get 'send_order qq-red-packet--detail)))
            (insert "\n")
            (appkit-view-insert-heading-line
             "红包详情" :face 'bold
             :line-properties
             (list qq-red-packet--position-property
                   (qq-red-packet--position-key 'heading 'detail)))
            (qq-red-packet--insert-field
             "发送者" (alist-get 'sender_name order))
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
             "发送时间"
             (qq-red-packet--format-time (alist-get 'created_at order)))
            (qq-red-packet--insert-field
             "失效时间"
             (qq-red-packet--format-time (alist-get 'expires_at order)))
            (qq-red-packet--insert-field
             "原生状态" (alist-get 'state order))
            (when-let* ((receipts (alist-get 'receipts qq-red-packet--detail))
                        ((not (null receipts))))
              (insert "\n")
              (appkit-view-insert-heading-line
               "领取记录" :face 'bold
               :line-properties
               (list qq-red-packet--position-property
                     (qq-red-packet--position-key 'heading 'receipts)))
              (seq-do #'qq-red-packet--insert-receipt receipts))))
        (force-mode-line-update)
        (when snapshot
          (appkit-position-restore snapshot))))))

(defun qq-red-packet--event-current-p (event)
  "Return non-nil when queued EVENT targets the current packet."
  (and (equal (plist-get event :session-key) qq-red-packet--session-key)
       (equal (plist-get event :message-id) qq-red-packet--message-id)))

(defun qq-red-packet--sync-invalidations (view invalidations)
  "Synchronize red-packet VIEW from coalesced INVALIDATIONS and events."
  (let ((events (appkit-view-pending-events-snapshot view)))
    (when (or (appkit-invalidations-any-p invalidations) events)
      (qq-red-packet--render-content view))
    (when (appkit-view-live-p view)
      (appkit-view-acknowledge-events view (length events))
      (dolist (event events)
        (when (and (eq (plist-get event :type) 'refresh-detail)
                   (qq-red-packet--event-current-p event)
                   (qq-red-packet--detail-supported-p)
                   (not qq-red-packet--loading))
          ;; A successful grab first becomes visible through the transaction
          ;; above.  Only then may its explicit continuation start a detail
          ;; request; transport callbacks never recurse into refresh/render.
          (qq-red-packet--start-detail-request view))))))

(defun qq-red-packet--request-sync (&optional view)
  "Request one atomic full red-packet sync for live VIEW."
  (when-let* ((view (or view (qq-red-packet--live-current-view))))
    (appkit-request-sync view :structure t :part 'packet)))

(defun qq-red-packet--sync-now (view)
  "Consume pending red-packet invalidations for live VIEW immediately."
  (when (appkit-view-live-p view)
    (appkit-sync-invalidations view)))

(defun qq-red-packet-render ()
  "Request and immediately synchronize the current red-packet page."
  (interactive)
  (let ((view (qq-red-packet--ensure-view)))
    (qq-red-packet--request-sync view)
    (qq-red-packet--sync-now view)))

(defun qq-red-packet--request-current-p
    (view buffer session-key message-id owner)
  "Return non-nil when VIEW and OWNER load SESSION-KEY and MESSAGE-ID in BUFFER."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-red-packet-mode)
              (eq view (appkit-current-view))
              (equal qq-red-packet--session-key session-key)
              (equal qq-red-packet--message-id message-id)
              (eq qq-red-packet--request-owner owner)))))

(defun qq-red-packet--start-detail-request (view)
  "Start a detail request owned by live packet VIEW without synchronizing."
  (unless (qq-red-packet--detail-supported-p)
    (user-error "qq: Linux QQ 不提供口令红包详情"))
  (let ((buffer (current-buffer))
        (session-key qq-red-packet--session-key)
        (message-id qq-red-packet--message-id)
        (old-request qq-red-packet--request)
        (owner (list 'red-packet-detail
                     qq-red-packet--session-key
                     qq-red-packet--message-id)))
    ;; Rotate ownership before cancellation so a cancellation-side callback is
    ;; stale even when the transport invokes it synchronously.
    (setq qq-red-packet--loading t
          qq-red-packet--error nil
          qq-red-packet--request nil
          qq-red-packet--request-owner owner)
    (when old-request
      (qq-api-cancel-request old-request))
    (qq-red-packet--request-sync view)
    (condition-case error-data
        (let ((request
               (qq-api-get-red-packet-detail
                session-key message-id
                (lambda (detail)
                  (when (qq-red-packet--request-current-p
                         view buffer session-key message-id owner)
                    (with-current-buffer buffer
                      (setq qq-red-packet--detail detail
                            qq-red-packet--loading nil
                            qq-red-packet--error nil
                            qq-red-packet--request nil
                            qq-red-packet--request-owner nil)
                      (qq-red-packet--request-sync view))))
                (lambda (_response reason)
                  (when (qq-red-packet--request-current-p
                         view buffer session-key message-id owner)
                    (with-current-buffer buffer
                      (setq qq-red-packet--loading nil
                            qq-red-packet--error
                            (or reason "无法获取红包详情")
                            qq-red-packet--request nil
                            qq-red-packet--request-owner nil)
                      (qq-red-packet--request-sync view)))))))
          (when (eq qq-red-packet--request-owner owner)
            (setq qq-red-packet--request request)))
      (error
       (when (qq-red-packet--request-current-p
              view buffer session-key message-id owner)
         (with-current-buffer buffer
           (setq qq-red-packet--loading nil
                 qq-red-packet--error (error-message-string error-data)
                 qq-red-packet--request nil
                 qq-red-packet--request-owner nil)
           (qq-red-packet--request-sync view)))))))

(defun qq-red-packet-refresh ()
  "Refresh the current packet's read-only detail."
  (interactive)
  (let ((view (qq-red-packet--ensure-view)))
    (qq-red-packet--start-detail-request view)
    (qq-red-packet--sync-now view)))

(defun qq-red-packet--grab-current-p
    (view buffer session-key message-id owner)
  "Return non-nil when VIEW and OWNER claim SESSION-KEY and MESSAGE-ID in BUFFER."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-red-packet-mode)
              (eq view (appkit-current-view))
              (equal qq-red-packet--session-key session-key)
              (equal qq-red-packet--message-id message-id)
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
  (let* ((view (qq-red-packet--ensure-view))
         (buffer (current-buffer))
         (session-key qq-red-packet--session-key)
         (message-id qq-red-packet--message-id)
         (owner (list 'red-packet-grab session-key message-id)))
    (setq qq-red-packet--grabbing t
          qq-red-packet--error nil
          qq-red-packet--notice nil
          qq-red-packet--grab-request nil
          qq-red-packet--grab-owner owner)
    (qq-red-packet--request-sync view)
    (condition-case error-data
        (let ((request
               (qq-api-grab-red-packet
                session-key message-id
                (lambda (result)
                  (when (qq-red-packet--grab-current-p
                         view buffer session-key message-id owner)
                    (with-current-buffer buffer
                      (setq qq-red-packet--grabbing nil
                            qq-red-packet--grab-request nil
                            qq-red-packet--grab-owner nil)
                      (if (equal (alist-get 'interaction result) "password")
                          (progn
                            (setq qq-red-packet--notice
                                  "口令红包领取请求已提交")
                            (message "qq: 口令红包领取请求已提交"))
                        (setq qq-red-packet--notice
                              "红包领取请求已完成")
                        (message "qq: 红包领取请求已完成")
                        (appkit-view-enqueue-event
                         view
                         (list :type 'refresh-detail
                               :session-key session-key
                               :message-id message-id)))
                      (qq-red-packet--request-sync view))))
                (lambda (_response reason)
                  (when (qq-red-packet--grab-current-p
                         view buffer session-key message-id owner)
                    (with-current-buffer buffer
                      (setq qq-red-packet--grabbing nil
                            qq-red-packet--error
                            (or reason "领取红包失败")
                            qq-red-packet--grab-request nil
                            qq-red-packet--grab-owner nil)
                      (qq-red-packet--request-sync view)))))))
          (when (eq qq-red-packet--grab-owner owner)
            (setq qq-red-packet--grab-request request)))
      (error
       (when (qq-red-packet--grab-current-p
              view buffer session-key message-id owner)
         (with-current-buffer buffer
           (setq qq-red-packet--grabbing nil
                 qq-red-packet--error (error-message-string error-data)
                 qq-red-packet--grab-request nil
                 qq-red-packet--grab-owner nil)
           (qq-red-packet--request-sync view)))))
    (qq-red-packet--sync-now view)))

(defun qq-red-packet--cancel-requests ()
  "Cancel local ownership of all current packet requests."
  (let ((request qq-red-packet--request)
        (grab-request qq-red-packet--grab-request))
    (setq qq-red-packet--request nil
          qq-red-packet--request-owner nil
          qq-red-packet--grab-request nil
          qq-red-packet--grab-owner nil
          qq-red-packet--loading nil
          qq-red-packet--grabbing nil)
    (when request
      (qq-api-cancel-request request))
    (when grab-request
      (qq-api-cancel-request grab-request))))

(defun qq-red-packet--live-current-view ()
  "Return this buffer's live canonical red-packet view, or nil."
  (let ((view (appkit-current-view)))
    (and qq-red-packet--session-key
         qq-red-packet--message-id
         (derived-mode-p 'qq-red-packet-mode)
         (appkit-view-live-p view)
         (eq (appkit-view-buffer view) (current-buffer))
         (equal (appkit-view-id view)
                (qq-red-packet--view-id
                 qq-red-packet--session-key qq-red-packet--message-id))
         view)))

(defun qq-red-packet--reset-buffer-work (buffer)
  "Cancel and clear packet work and fetched data retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-red-packet-mode)
        (qq-red-packet--cancel-requests)
        (setq qq-red-packet--detail nil
              qq-red-packet--error nil
              qq-red-packet--notice nil)))))

(defun qq-red-packet--release-view-work (view buffer)
  "Release packet work in BUFFER when it is still owned by VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq (appkit-current-view) view)))
    (qq-red-packet--reset-buffer-work buffer)))

(defun qq-red-packet--setup-view (view)
  "Reset retained state and register lifecycle cleanup for new VIEW."
  (let ((buffer (appkit-view-buffer view)))
    (qq-red-packet--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-red-packet--release-view-work view buffer))))

(defun qq-red-packet--ensure-view ()
  "Return the live Appkit view owning the current red-packet buffer."
  (unless (and qq-red-packet--session-key qq-red-packet--message-id)
    (error "QQ: Cannot attach a red-packet view without its identity"))
  (let* ((app (qq-runtime-app))
         (view-id
          (qq-red-packet--view-id
           qq-red-packet--session-key qq-red-packet--message-id))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal view-id (appkit-view-id current)))
      (setf (appkit-view-state current)
            (list qq-red-packet--session-key qq-red-packet--message-id)
            (appkit-view-sync-function current)
            #'qq-red-packet--sync-invalidations
            (appkit-view-parts current) '(packet))
     current)
     ((appkit-view-live-p current)
      (error "QQ: Red-packet buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id view-id
              :state (list qq-red-packet--session-key
                           qq-red-packet--message-id)
              :mode 'qq-red-packet-mode
              :sync-function #'qq-red-packet--sync-invalidations
              :parts '(packet))))
        (qq-red-packet--setup-view view)
        view)))))

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
  (setq-local header-line-format
              '(:eval (format " %s · %s"
                              (qq-red-packet--kind-label)
                              (qq-red-packet--title))))
  (add-hook 'change-major-mode-hook #'qq-red-packet--cancel-requests nil t)
  (add-hook 'kill-buffer-hook #'qq-red-packet--cancel-requests nil t))

;;;###autoload
(defun qq-red-packet-open (session-key message-id segment outgoing-p)
  "Open native red packet SEGMENT for MESSAGE-ID in SESSION-KEY."
  (qq-api-validate-message-id message-id "red-packet view")
  (unless (and (equal (alist-get 'type segment) "wallet")
               (member (alist-get 'wallet_kind (alist-get 'data segment))
                       '("red-packet" "password-red-packet")))
    (error "QQ: Red-packet view requires an interactive wallet kind"))
  (let* ((app (qq-runtime-app))
         (view-id (qq-red-packet--view-id session-key message-id))
         (fresh-p (null (appkit-view-for-id app view-id)))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :state (list session-key message-id)
           :mode 'qq-red-packet-mode
           :buffer-name (qq-red-packet--buffer-name session-key message-id)
           :sync-function #'qq-red-packet--sync-invalidations
           :parts '(packet)
           :setup #'qq-red-packet--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq qq-red-packet--session-key session-key
            qq-red-packet--message-id message-id)
      (when fresh-p
        (setq qq-red-packet--segment (copy-tree segment)
              qq-red-packet--outgoing-p outgoing-p))
      (if (and fresh-p (qq-red-packet--detail-supported-p))
          (qq-red-packet-refresh)
        (qq-red-packet--request-sync view)
        (qq-red-packet--sync-now view)))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-red-packet)

;;; qq-red-packet.el ends here
