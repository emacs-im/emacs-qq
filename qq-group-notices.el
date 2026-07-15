;;; qq-group-notices.el --- Read-only QQ group notices -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A small read-only view backed by the strict `emacs_get_group_notices'
;; action.  Announcement mutations intentionally remain outside this module.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-runtime)

(declare-function qq-user-open "qq-user" (user-id))

(defface qq-group-notices-title
  '((t :inherit bold :height 1.1))
  "Face used for group-notice titles."
  :group 'qq)

(cl-defstruct (qq-group-notices--entry
               (:constructor qq-group-notices--entry-create))
  key
  type
  object
  index
  text)

(defconst qq-group-notices--view-id 'group-notices
  "Appkit identity of the singleton group-notices view.")

(defvar-local qq-group-notices--ewoc nil
  "Persistent keyed EWOC used by the announcement view.")

(defvar-local qq-group-notices--node-table nil
  "Stable announcement key to EWOC node table.")

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

(defun qq-group-notices--notice-key (notice)
  "Return the stable presentation key for NOTICE."
  (list 'notice qq-group-notices--group-id (alist-get 'notice_id notice)))

(defun qq-group-notices--note-entry (key text &optional type)
  "Return a status entry identified by KEY with TEXT and optional TYPE."
  (qq-group-notices--entry-create
   :key (cons 'note key) :type (or type 'note) :text text))

(defun qq-group-notices--project-entries ()
  "Project announcement state into stable keyed presentation entries."
  (let (entries)
    (when qq-group-notices--loading
      (push (qq-group-notices--note-entry
             'loading
             (if qq-group-notices--items
                 "正在刷新群公告…"
               "正在加载群公告…"))
            entries))
    (when qq-group-notices--error
      (push (qq-group-notices--note-entry
             'error qq-group-notices--error 'error-note)
            entries))
    (when qq-group-notices--items
      (push (qq-group-notices--note-entry
             'instructions "g 刷新 · RET/TAB 打开发布者 · q 退出")
            entries)
      (cl-loop for notice in qq-group-notices--items
               for index from 1
               do (push (qq-group-notices--entry-create
                         :key (qq-group-notices--notice-key notice)
                         :type 'notice
                         :object notice
                         :index index)
                        entries)))
    (unless (or qq-group-notices--loading
                qq-group-notices--error
                qq-group-notices--items)
      (push (qq-group-notices--note-entry 'empty "暂无群公告。") entries))
    (nreverse entries)))

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
  (let ((start (point))
        (title (alist-get 'title notice))
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
      (unless (string-empty-p (or text ""))
        (insert text "\n")))
    (when images
      (cl-loop for image in images
               for image-index from 1
               do (insert (qq-group-notices--image-label image image-index) " "))
      (insert "\n"))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'qq-group-notice-key (qq-group-notices--notice-key notice)))))

(defun qq-group-notices--ewoc-printer (entry)
  "Insert one projected announcement ENTRY."
  (pcase (qq-group-notices--entry-type entry)
    ('notice
     (qq-group-notices--insert-item
      (qq-group-notices--entry-object entry)
      (qq-group-notices--entry-index entry)))
    ((or 'note 'error-note)
     (appkit-view-insert-note-line
      (or (qq-group-notices--entry-text entry) "")
      :face (if (eq (qq-group-notices--entry-type entry) 'error-note)
                'error
              'shadow)
      :line-properties
      (list 'qq-group-notice-key (qq-group-notices--entry-key entry))))
    (type (error "qq: unknown group notice entry type %S" type))))

(defun qq-group-notices--live-current-view ()
  "Return this buffer's live group-notices view, or nil."
  (let ((view (appkit-current-view)))
    (and (derived-mode-p 'qq-group-notices-mode)
         (appkit-view-live-p view)
         (equal (appkit-view-id view) qq-group-notices--view-id)
         view)))

(defun qq-group-notices--release-buffer-work (view buffer)
  "Release requests and account-scoped announcements owned by VIEW and BUFFER.

The detached buffer retains its Appkit fingerprint and therefore cannot be
attached to a different account runtime.  Clear both the model and generated
presentation so that keeping the old buffer alive cannot expose content from
the stopped account."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-group-notices-mode)
        (unwind-protect
            (qq-group-notices--cancel-request)
          (unless (appkit-app-live-p (appkit-view-app view))
            (setq qq-group-notices--group-id nil
                  qq-group-notices--group-name nil))
          (setq qq-group-notices--items nil
                qq-group-notices--loading nil
                qq-group-notices--error nil
                qq-group-notices--request nil
                qq-group-notices--request-owner nil
                qq-group-notices--ewoc nil
                qq-group-notices--node-table nil)
          (let ((inhibit-read-only t))
            (widen)
            (erase-buffer)
            (set-buffer-modified-p nil)))))))

(defun qq-group-notices--setup-view (view)
  "Register lifecycle cleanup for newly attached VIEW."
  (appkit-register-handle
   view 'function
   (apply-partially #'qq-group-notices--release-buffer-work
                    view
                    (appkit-view-buffer view))))

(defun qq-group-notices--ensure-view ()
  "Return the live Appkit view owning the current announcement buffer."
  (let* ((app (qq-runtime-app))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal qq-group-notices--view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-group-notices--sync-invalidations
            (appkit-view-parts current) '(notices))
      current)
     ((appkit-view-live-p current)
      (error "QQ: group-notices buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id qq-group-notices--view-id
              :mode 'qq-group-notices-mode
              :sync-function #'qq-group-notices--sync-invalidations
              :parts '(notices))))
        (qq-group-notices--setup-view view)
        view)))))

(defun qq-group-notices--request-sync (&optional view)
  "Request a coalesced announcement sync for live VIEW."
  (when-let* ((view (or view (qq-group-notices--live-current-view))))
    (appkit-request-sync view :structure t :part 'notices)))

(defun qq-group-notices--sync-now (view)
  "Consume pending invalidations for live VIEW immediately."
  (when (appkit-view-live-p view)
    (appkit-sync-invalidations view)))

(defun qq-group-notices--sync-invalidations (view invalidations)
  "Consume coalesced announcement INVALIDATIONS for VIEW."
  (when (and (appkit-view-live-p view)
             (or (appkit-invalidations-structure-p invalidations)
                 (appkit-invalidations-parts invalidations)
                 (appkit-invalidations-entry-keys invalidations)
                 (appkit-invalidations-position-p invalidations)))
    (let ((snapshot
           (with-current-buffer (appkit-view-buffer view)
             (appkit-position-capture
              :anchor-property 'qq-group-notice-key
              :preserve-window-start t))))
      (appkit-with-content-update view
				  (unless qq-group-notices--ewoc
				    (erase-buffer)
				    (setq qq-group-notices--ewoc
					  (ewoc-create #'qq-group-notices--ewoc-printer nil nil t)))
				  (setq qq-group-notices--node-table
					(appkit-ewoc-reconcile
					 qq-group-notices--ewoc
					 (qq-group-notices--project-entries)
					 #'qq-group-notices--entry-key
					 :force-keys (appkit-invalidations-entry-keys invalidations)))
				  (force-mode-line-update)
				  (when snapshot
				    (appkit-position-restore snapshot))))))

(defun qq-group-notices--request-current-p (view buffer group-id owner)
  "Return non-nil when VIEW and OWNER still load GROUP-ID in BUFFER."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-group-notices-mode)
              (eq view (appkit-current-view))
              (equal qq-group-notices--group-id group-id)
              (eq qq-group-notices--request-owner owner)))))

(defun qq-group-notices-refresh ()
  "Refresh the current group's read-only announcement list."
  (interactive)
  (unless qq-group-notices--group-id
    (user-error "qq: this buffer has no group identity"))
  (let ((view (qq-group-notices--ensure-view)))
    (when qq-group-notices--request
      (qq-api-cancel-request qq-group-notices--request))
    (let ((buffer (current-buffer))
          (group-id qq-group-notices--group-id)
          (owner (list 'group-notices qq-group-notices--group-id)))
      (setq qq-group-notices--loading t
            qq-group-notices--error nil
            qq-group-notices--request nil
            qq-group-notices--request-owner owner)
      (qq-group-notices--request-sync view)
      (condition-case error-data
          (let ((request
		 (qq-api-get-group-notices
                  group-id
                  (lambda (notices)
                    (when (qq-group-notices--request-current-p
                           view buffer group-id owner)
                      (with-current-buffer buffer
			(setq qq-group-notices--items notices
                              qq-group-notices--loading nil
                              qq-group-notices--error nil
                              qq-group-notices--request nil
                              qq-group-notices--request-owner nil)
			(qq-group-notices--request-sync view))))
                  (lambda (response reason)
                    (when (qq-group-notices--request-current-p
                           view buffer group-id owner)
                      (with-current-buffer buffer
			(setq qq-group-notices--loading nil
                              qq-group-notices--error
                              (format "无法加载群公告：%s"
                                      (or reason "未知错误"))
                              qq-group-notices--request nil
                              qq-group-notices--request-owner nil)
			(qq-group-notices--request-sync view))
                      (qq-api--default-error response reason))))))
            (when (eq qq-group-notices--request-owner owner)
              (setq qq-group-notices--request request)))
	(error
	 (when (qq-group-notices--request-current-p
		view buffer group-id owner)
           (with-current-buffer buffer
             (setq qq-group-notices--loading nil
                   qq-group-notices--error
                   (format "无法加载群公告：%s"
                           (error-message-string error-data))
                   qq-group-notices--request nil
                   qq-group-notices--request-owner nil)
             (qq-group-notices--request-sync view)))))
      ;; This command is an explicit presentation boundary.  A synchronous
      ;; transport completion is therefore visible before it returns, while an
      ;; asynchronous completion only queues a later coalesced sync.
      (qq-group-notices--sync-now view))))

(defun qq-group-notices--cancel-request ()
  "Cancel and forget the active announcement request."
  (when qq-group-notices--request
    (qq-api-cancel-request qq-group-notices--request))
  (setq qq-group-notices--request nil
        qq-group-notices--request-owner nil
        ;; A buffer may outlive its Appkit view.  Clearing the passive loading
        ;; state lets a replacement view issue its own request instead of
        ;; inheriting a request that no longer has a live owner.
        qq-group-notices--loading nil))

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
  (setq-local header-line-format '(:eval (qq-group-notices--header-line)))
  (setq-local qq-group-notices--ewoc nil)
  (setq-local qq-group-notices--node-table nil)
  (add-hook 'change-major-mode-hook #'qq-group-notices--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-group-notices--cancel-request nil t))

;;;###autoload
(defun qq-group-notices-open (group-id &optional group-name)
  "Open read-only announcements for GROUP-ID and optional GROUP-NAME."
  (interactive "sQQ group number: ")
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group notices require a canonical uint32 group id"))
  (let* ((app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id qq-group-notices--view-id
           :mode 'qq-group-notices-mode
           :buffer-name (qq-group-notices--buffer-name)
           :sync-function #'qq-group-notices--sync-invalidations
           :parts '(notices)
           :setup #'qq-group-notices--setup-view))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (unless (equal qq-group-notices--group-id group-id)
        (qq-group-notices--cancel-request)
        (setq qq-group-notices--items nil
              qq-group-notices--loading nil
              qq-group-notices--error nil))
      (setq qq-group-notices--group-id group-id
            qq-group-notices--group-name group-name)
      (when (and (null qq-group-notices--items)
                 (not qq-group-notices--loading))
        (qq-group-notices-refresh))
      (unless qq-group-notices--loading
        (qq-group-notices--request-sync view)
        (qq-group-notices--sync-now view)))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-group-notices)

;;; qq-group-notices.el ends here
