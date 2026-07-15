;;; qq-guild-forum-post.el --- Native QQ Guild forum post view -*- lexical-binding: t; -*-

;;; Commentary:

;; A forum post owns a native comment directory.  Comments and nested replies
;; retain their Feed identities and opaque cursors; they never enter the chat
;; message timeline.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)

(declare-function qq-guild-forum--insert-post "qq-guild-forum" (message &optional detail-p))
(declare-function qq-guild-forum--discussion-width "qq-guild-forum" ())
(declare-function qq-guild-user-open "qq-guild-user" (guild-id native-id))

(cl-defstruct (qq-guild-forum-post--entry
               (:constructor qq-guild-forum-post--entry-create))
  key
  kind
  value)

(defcustom qq-guild-forum-post-auto-load-threshold 8
  "Lines from the bottom that trigger the next top-level comment page."
  :type 'integer
  :group 'qq)

(defvar-local qq-guild-forum-post--post nil)
(defvar-local qq-guild-forum-post--comments nil)
(defvar-local qq-guild-forum-post--next-cursor nil)
(defvar-local qq-guild-forum-post--finished-p nil)
(defvar-local qq-guild-forum-post--total-comment-count nil)
(defvar-local qq-guild-forum-post--loading nil)
(defvar-local qq-guild-forum-post--error nil)
(defvar-local qq-guild-forum-post--request nil)
(defvar-local qq-guild-forum-post--request-owner nil)
(defvar-local qq-guild-forum-post--ewoc nil)
(defvar-local qq-guild-forum-post--node-table nil)

(defun qq-guild-forum-post--view-id (post)
  "Return the Appkit view identity for POST."
  (list 'guild-forum-post
        (alist-get 'guild-id post)
        (alist-get 'channel-id post)
        (alist-get 'id post)))

(defun qq-guild-forum-post--buffer-name (post)
  "Return a stable detail buffer name for POST."
  (let* ((title (or (alist-get 'forum-title post) ""))
         (label (if (string-empty-p title)
                    (or (alist-get 'sender-name post) "帖子")
                  title)))
    (format "*qq-post:%s*" (truncate-string-to-width label 32 nil nil "…"))))

(defun qq-guild-forum-post--comment-at-point (&optional position)
  "Return the closed comment represented at POSITION or point."
  (let* ((probe (or position (point)))
         (comment-id
          (or (get-text-property probe 'qq-guild-forum-comment-id)
              (save-excursion
                (goto-char probe)
                (get-text-property (line-beginning-position)
                                   'qq-guild-forum-comment-id)))))
    (and comment-id
         (seq-find
          (lambda (comment)
            (equal (alist-get 'comment_id comment) comment-id))
          qq-guild-forum-post--comments))))

(defun qq-guild-forum-post--comment-message (comment)
  "Return a shared-renderer message model for closed COMMENT."
  (let ((sender (alist-get 'sender comment))
        (post qq-guild-forum-post--post))
    `((id . ,(alist-get 'comment_id comment))
      (session-key . ,(alist-get 'session-key post))
      (time . ,(alist-get 'created_at comment))
      (sender-id . ,(alist-get 'native_id sender))
      (sender-native-id . ,(alist-get 'native_id sender))
      (sender-name . ,(alist-get 'display_name sender))
      (sender-avatar-url . ,(alist-get 'avatar_url sender))
      (message-type . "guild-forum-comment")
      (guild-id . ,(alist-get 'guild-id post))
      (channel-id . ,(alist-get 'channel-id post))
      (self-p . nil)
      (segments . ,(qq-state-normalize-closed-segments
                    (alist-get 'segments comment))))))

(defun qq-guild-forum-post--time-string (comment)
  "Return compact local time for COMMENT."
  (format-time-string "%m-%d %H:%M"
                      (seconds-to-time (alist-get 'created_at comment))))

(defun qq-guild-forum-post--loaded-reply-count (comment-id)
  "Return the number of loaded direct replies under COMMENT-ID."
  (seq-count
   (lambda (comment)
     (equal (alist-get 'parent_comment_id comment) comment-id))
   qq-guild-forum-post--comments))

(defun qq-guild-forum-post--insert-reply-target (target)
  "Insert an interactive native Guild reply TARGET."
  (let ((guild-id (alist-get 'guild-id qq-guild-forum-post--post))
        (native-id (alist-get 'native_id target)))
    (insert " 回复 ")
    (appkit-ui-insert-action-button
     (format "@%s" (alist-get 'display_name target))
     (lambda () (qq-guild-user-open guild-id native-id))
     :face 'font-lock-variable-name-face
     :help-echo "Open replied channel member profile")))

(defun qq-guild-forum-post--comment-discussion-entry (comment)
  "Return an Appkit discussion entry for closed forum COMMENT."
  (let* ((comment-id (alist-get 'comment_id comment))
         (parent-id (alist-get 'parent_comment_id comment))
         (nested-p (not (null parent-id)))
         (message (qq-guild-forum-post--comment-message comment))
         (target (alist-get 'reply_to_sender comment))
         (properties
          (list 'qq-guild-forum-comment-id comment-id
                'rear-nonsticky '(qq-guild-forum-comment-id)))
         (reply-count (alist-get 'reply_count comment))
         (loaded (qq-guild-forum-post--loaded-reply-count comment-id)))
    (appkit-discussion-entry-create
     :key (list 'forum-comment comment-id)
     :parent-key (and parent-id (list 'forum-comment parent-id))
     :depth (if nested-p 1 0)
     :avatar (qq-media-message-avatar-image message)
     :avatar-fallback "@"
     :avatar-action
     (lambda ()
       (interactive)
       (qq-media-open-message-avatar message))
     :avatar-help-echo "Open channel member avatar"
     :heading-inserter
     (lambda ()
       (qq-chat--insert-message-sender message 'qq-msg-user-title)
       (when target
         (qq-guild-forum-post--insert-reply-target target)))
     :heading-line-face 'qq-msg-heading
     :time (qq-guild-forum-post--time-string comment)
     :time-face 'qq-msg-status
     :body-inserter
     (lambda (prefix row-properties)
       (qq-chat--insert-message-body message prefix row-properties))
     :footer
     (and (not nested-p)
          (> reply-count 0)
          (format "%d/%d 条回复%s"
                  loaded reply-count
                  (if (eq (alist-get 'replies_finished comment) t)
                      ""
                    " · RET 加载更多")))
     :footer-face 'shadow
     :properties properties)))

(defun qq-guild-forum-post--insert-comment (comment)
  "Insert one closed forum COMMENT through Appkit."
  (appkit-discussion-insert-entry
   (qq-guild-forum-post--comment-discussion-entry comment)
   :width (qq-guild-forum--discussion-width)))

(defun qq-guild-forum-post--entry-printer (entry)
  "Insert persistent forum post detail ENTRY."
  (pcase (qq-guild-forum-post--entry-kind entry)
    ('post (qq-guild-forum--insert-post
            (qq-guild-forum-post--entry-value entry) t))
    ('comment (qq-guild-forum-post--insert-comment
               (qq-guild-forum-post--entry-value entry)))))

(defun qq-guild-forum-post--project-entries ()
  "Return stable EWOC entries for the current post and comments."
  (cons
   (qq-guild-forum-post--entry-create
    :key (list 'post (alist-get 'id qq-guild-forum-post--post))
    :kind 'post
    :value qq-guild-forum-post--post)
   (mapcar
    (lambda (comment)
      (qq-guild-forum-post--entry-create
       :key (list 'comment (alist-get 'comment_id comment))
       :kind 'comment
       :value comment))
    qq-guild-forum-post--comments)))

(defun qq-guild-forum-post--header-line ()
  "Return the current post detail header line."
  (let ((loaded (length qq-guild-forum-post--comments))
        (total (or qq-guild-forum-post--total-comment-count
                   (alist-get 'forum-comment-count qq-guild-forum-post--post)
                   0)))
    (format " %s  ·  %d/%d 条评论%s"
            (or (alist-get 'forum-title qq-guild-forum-post--post) "论坛帖子")
            loaded total
            (cond
             (qq-guild-forum-post--loading " · loading")
             (qq-guild-forum-post--error " · 加载失败")
             (qq-guild-forum-post--finished-p " · 已加载全部")
             (t "")))))

(defun qq-guild-forum-post--reconcile (&optional force-keys)
  "Reconcile the detail projection, redrawing retained FORCE-KEYS."
  (setq qq-guild-forum-post--node-table
        (appkit-ewoc-reconcile
         qq-guild-forum-post--ewoc
         (qq-guild-forum-post--project-entries)
         #'qq-guild-forum-post--entry-key
         :force-keys force-keys)))

(defun qq-guild-forum-post--sync-invalidations (view invalidations)
  "Synchronize post detail VIEW from Appkit INVALIDATIONS."
  (when (appkit-view-live-p view)
    (let ((parts (appkit-invalidations-parts invalidations))
          (entry-keys (appkit-invalidations-entry-keys invalidations)))
      (when (or (appkit-invalidations-structure-p invalidations)
                (memq 'comments parts)
                entry-keys)
        (appkit-with-content-update view
          (let ((snapshot
                 (appkit-position-capture
                  :anchor-property 'qq-guild-forum-comment-id
                  :preserve-window-start t)))
            (with-silent-modifications
              (qq-guild-forum-post--reconcile entry-keys))
            (when snapshot
              (appkit-position-restore snapshot)))))
      (when (or (memq 'header parts) (memq 'comments parts) entry-keys)
        (force-mode-line-update)))))

(defun qq-guild-forum-post--request-current-p (owner)
  "Return non-nil when OWNER still owns the exact detail request."
  (let ((buffer (plist-get owner :buffer))
        (view (plist-get owner :view)))
    (and (buffer-live-p buffer)
         (appkit-view-live-p view)
         (with-current-buffer buffer
           (and (eq owner qq-guild-forum-post--request-owner)
                (eq view (appkit-current-view))
                (equal (plist-get owner :post-id)
                       (alist-get 'id qq-guild-forum-post--post)))))))

(defun qq-guild-forum-post--cancel-request ()
  "Cancel and forget the active comment or reply request."
  (let ((request qq-guild-forum-post--request))
    (setq qq-guild-forum-post--request nil
          qq-guild-forum-post--request-owner nil
          qq-guild-forum-post--loading nil)
    (when request
      (qq-api-cancel-request request))))

(defun qq-guild-forum-post--assert-new-identities (items)
  "Reject ITEMS whose comment identities already exist in this detail view."
  (let ((known (make-hash-table :test #'equal)))
    (dolist (comment qq-guild-forum-post--comments)
      (puthash (alist-get 'comment_id comment) t known))
    (dolist (comment items)
      (let ((comment-id (alist-get 'comment_id comment)))
        (when (gethash comment-id known)
          (error "qq: native forum pagination repeated comment %s" comment-id))
        (puthash comment-id t known)))))

(defun qq-guild-forum-post--merge-comment-page (page initial-p)
  "Merge validated top-level comment PAGE, replacing when INITIAL-P."
  (let ((comments (copy-tree (alist-get 'comments page))))
    (unless initial-p
      (qq-guild-forum-post--assert-new-identities comments))
    (setq qq-guild-forum-post--comments
          (if initial-p comments
            (nconc qq-guild-forum-post--comments comments))
          qq-guild-forum-post--next-cursor (alist-get 'next_cursor page)
          qq-guild-forum-post--finished-p (eq (alist-get 'finished page) t)
          qq-guild-forum-post--total-comment-count
          (alist-get 'total_comment_count page))))

(defun qq-guild-forum-post--merge-reply-page (parent page)
  "Merge validated reply PAGE below top-level PARENT."
  (let* ((parent-id (alist-get 'comment_id parent))
         (expected (alist-get 'reply_count parent))
         (reported (alist-get 'total_reply_count page))
         (replies (copy-tree (alist-get 'replies page))))
    (unless (= expected reported)
      (error "qq: native forum reply count changed from %d to %d"
             expected reported))
    (qq-guild-forum-post--assert-new-identities replies)
    (setq qq-guild-forum-post--comments
          (mapcar
           (lambda (comment)
             (if (equal (alist-get 'comment_id comment) parent-id)
                 (let ((copy (copy-tree comment)))
                   (setf (alist-get 'reply_cursor copy)
                         (alist-get 'next_cursor page)
                         (alist-get 'replies_finished copy)
                         (alist-get 'finished page))
                   copy)
               comment))
           qq-guild-forum-post--comments))
    (let* ((parent-index
            (cl-position parent-id qq-guild-forum-post--comments
                         :key (lambda (comment)
                                (alist-get 'comment_id comment))
                         :test #'equal))
           boundary)
      (unless parent-index
        (error "qq: native forum reply parent %s disappeared" parent-id))
      (setq boundary (1+ parent-index))
      (while (and (< boundary (length qq-guild-forum-post--comments))
                  (equal (alist-get
                          'parent_comment_id
                          (nth boundary qq-guild-forum-post--comments))
                         parent-id))
        (setq boundary (1+ boundary)))
      (setq qq-guild-forum-post--comments
            (append (seq-take qq-guild-forum-post--comments boundary)
                    replies
                    (seq-drop qq-guild-forum-post--comments boundary))))))

(defun qq-guild-forum-post--settle-error (owner response reason)
  "Settle current OWNER as failed with RESPONSE and REASON."
  (when (qq-guild-forum-post--request-current-p owner)
    (with-current-buffer (plist-get owner :buffer)
      (setq qq-guild-forum-post--request nil
            qq-guild-forum-post--request-owner nil
            qq-guild-forum-post--loading nil
            qq-guild-forum-post--error (or reason "加载失败"))
      (appkit-request-sync (plist-get owner :view) :parts '(comments header)))
    (qq-api--default-error response reason)))

(defun qq-guild-forum-post--load-comments (cursor &optional quiet)
  "Load one top-level comment page at CURSOR, optionally QUIET."
  (if qq-guild-forum-post--loading
      (unless quiet (message "qq: forum comments are already loading"))
    (let* ((view (appkit-current-view))
           (buffer (current-buffer))
           (initial-p (string-empty-p cursor))
           (owner (list :buffer buffer :view view
                        :post-id (alist-get 'id qq-guild-forum-post--post)
                        :kind 'comments :cursor cursor))
           (pending t)
           request)
      (setq qq-guild-forum-post--loading 'comments
            qq-guild-forum-post--error nil
            qq-guild-forum-post--request-owner owner)
      (appkit-request-sync view :part 'header)
      (setq request
            (qq-api-fetch-guild-forum-comments
             qq-guild-forum-post--post cursor
             (lambda (page)
               (setq pending nil)
               (when (qq-guild-forum-post--request-current-p owner)
                 (with-current-buffer buffer
                   (condition-case error-data
                       (progn
                         (qq-guild-forum-post--merge-comment-page page initial-p)
                         (setq qq-guild-forum-post--request nil
                               qq-guild-forum-post--request-owner nil
                               qq-guild-forum-post--loading nil)
                         (appkit-request-sync
                          view :structure t :parts '(comments header)))
                     (error
                      (qq-guild-forum-post--settle-error
                       owner nil (error-message-string error-data)))))))
             (lambda (response reason)
               (setq pending nil)
               (qq-guild-forum-post--settle-error owner response reason))))
      (when (and pending (qq-guild-forum-post--request-current-p owner))
        (setq qq-guild-forum-post--request request))
      request)))

(defun qq-guild-forum-post--load-replies (comment &optional quiet)
  "Load the next native reply page for top-level COMMENT."
  (let ((cursor (alist-get 'reply_cursor comment)))
    (cond
     (qq-guild-forum-post--loading
      (unless quiet (message "qq: another forum request is already loading")))
     ((eq (alist-get 'replies_finished comment) t)
      (unless quiet (message "qq: all replies are loaded")))
     ((not (qq-api-non-empty-string-p cursor))
      (error "qq: unfinished native reply directory has no cursor"))
     (t
      (let* ((view (appkit-current-view))
             (buffer (current-buffer))
             (owner (list :buffer buffer :view view
                          :post-id (alist-get 'id qq-guild-forum-post--post)
                          :kind 'replies
                          :comment-id (alist-get 'comment_id comment)))
             (pending t)
             request)
        (setq qq-guild-forum-post--loading 'replies
              qq-guild-forum-post--error nil
              qq-guild-forum-post--request-owner owner)
        (appkit-request-sync view :part 'header)
        (setq request
              (qq-api-fetch-guild-forum-replies
               comment cursor
               (lambda (page)
                 (setq pending nil)
                 (when (qq-guild-forum-post--request-current-p owner)
                   (with-current-buffer buffer
                     (condition-case error-data
                         (progn
                           (qq-guild-forum-post--merge-reply-page comment page)
                           (setq qq-guild-forum-post--request nil
                                 qq-guild-forum-post--request-owner nil
                                 qq-guild-forum-post--loading nil)
                           (appkit-request-sync
                            view :structure t :parts '(comments header)))
                       (error
                        (qq-guild-forum-post--settle-error
                         owner nil (error-message-string error-data)))))))
               (lambda (response reason)
                 (setq pending nil)
                 (qq-guild-forum-post--settle-error owner response reason))))
        (when (and pending (qq-guild-forum-post--request-current-p owner))
          (setq qq-guild-forum-post--request request))
        request)))))

(defun qq-guild-forum-post-refresh ()
  "Reload the authoritative first comment page."
  (interactive)
  (qq-guild-forum-post--cancel-request)
  (setq qq-guild-forum-post--next-cursor nil
        qq-guild-forum-post--finished-p nil
        qq-guild-forum-post--total-comment-count nil)
  (qq-guild-forum-post--load-comments ""))

(defun qq-guild-forum-post-load-older (&optional quiet)
  "Load one older top-level comment page, optionally QUIET."
  (interactive)
  (cond
   (qq-guild-forum-post--finished-p
    (unless quiet (message "qq: all forum comments are loaded")))
   ((not (qq-api-non-empty-string-p qq-guild-forum-post--next-cursor))
    (unless quiet (message "qq: forum comment cursor is not initialized")))
   (t
    (qq-guild-forum-post--load-comments
     qq-guild-forum-post--next-cursor quiet))))

(defun qq-guild-forum-post--near-visible-bottom-p (window)
  "Return non-nil when WINDOW is near the detail buffer bottom."
  (let ((end (window-end window t)))
    (and end
         (<= (count-screen-lines end (point-max))
             (max 1 qq-guild-forum-post-auto-load-threshold)))))

(defun qq-guild-forum-post--window-scroll (window _display-start)
  "Continue top-level comment pagination near the bottom of WINDOW."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (not qq-guild-forum-post--loading)
             (not qq-guild-forum-post--finished-p)
             (qq-guild-forum-post--near-visible-bottom-p window))
    (qq-guild-forum-post-load-older t)))

(defun qq-guild-forum-post-open-at-point ()
  "Load more replies for the top-level comment at point."
  (interactive)
  (let ((comment (or (qq-guild-forum-post--comment-at-point)
                     (user-error "qq: point is not on a forum comment"))))
    (when (alist-get 'parent_comment_id comment)
      (user-error "qq: this reply has no nested reply directory"))
    (qq-guild-forum-post--load-replies comment)))

(defun qq-guild-forum-post--setup-view (view)
  "Register media invalidation ownership for detail VIEW."
  (let ((handler
         (lambda (_key)
           (when (appkit-view-live-p view)
             (appkit-with-live-view view
               (appkit-request-sync
                view :entries
                (mapcar #'qq-guild-forum-post--entry-key
                        (qq-guild-forum-post--project-entries))))))))
    (add-hook 'qq-media-cache-update-hook handler)
    (appkit-register-handle
     view 'hook
     (list 'qq-media-cache-update-hook handler nil (current-buffer)))))

(defvar qq-guild-forum-post-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guild-forum-post-refresh)
    (define-key map (kbd "n") #'appkit-discussion-next-entry)
    (define-key map (kbd "p") #'appkit-discussion-previous-entry)
    (define-key map (kbd "RET") #'qq-guild-forum-post-open-at-point)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode qq-guild-forum-post-mode special-mode "QQ-Forum-Post"
  "Major mode for one native QQ Guild forum post and its comments."
  (setq buffer-read-only t
        truncate-lines nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local qq-guild-forum-post--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (qq-guild-forum-post--header-line)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local qq-guild-forum-post--ewoc
                (ewoc-create #'qq-guild-forum-post--entry-printer nil nil t)))
  (add-hook 'window-scroll-functions
            #'qq-guild-forum-post--window-scroll nil t)
  (add-hook 'change-major-mode-hook
            #'qq-guild-forum-post--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-guild-forum-post--cancel-request nil t))

(defun qq-guild-forum-post-open (post)
  "Open normalized native forum POST and its comment directory."
  (unless (equal (alist-get 'message-type post) "guild-forum-post")
    (user-error "qq: this item is not a native forum post"))
  (let* ((app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id (qq-guild-forum-post--view-id post)
           :mode 'qq-guild-forum-post-mode
           :buffer-name (qq-guild-forum-post--buffer-name post)
           :state (alist-get 'id post)
           :sync-function #'qq-guild-forum-post--sync-invalidations
           :parts '(comments header)
           :setup #'qq-guild-forum-post--setup-view
           :select t))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq qq-guild-forum-post--post (copy-tree post))
      (appkit-request-sync view :structure t :parts '(comments header))
      (appkit-sync-invalidations view)
      (unless (or qq-guild-forum-post--loading
                  qq-guild-forum-post--next-cursor
                  qq-guild-forum-post--finished-p)
        (qq-guild-forum-post--load-comments "" t)))
    buffer))

(provide 'qq-guild-forum-post)

;;; qq-guild-forum-post.el ends here
