;;; qq-guild-forum.el --- Native QQ Guild forum directory -*- lexical-binding: t; -*-

;;; Commentary:

;; A QQ forum channel is a Feed directory, not a message timeline.  This view
;; owns opaque Feed pagination and renders stable post rows without a chat
;; composer.  Post comments remain a separate native protocol surface.

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
(require 'qq-guild-forum-post)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)

(cl-defstruct (qq-guild-forum--entry
               (:constructor qq-guild-forum--entry-create))
  key
  message)

(defcustom qq-guild-forum-auto-load-threshold 8
  "Lines from the visible bottom edge that trigger older Feed pagination."
  :type 'integer
  :group 'qq)

(defvar-local qq-guild-forum--guild-id nil)
(defvar-local qq-guild-forum--channel-id nil)
(defvar-local qq-guild-forum--session-key nil)
(defvar-local qq-guild-forum--next-cursor nil)
(defvar-local qq-guild-forum--finished-p nil)
(defvar-local qq-guild-forum--loading nil)
(defvar-local qq-guild-forum--error nil)
(defvar-local qq-guild-forum--request nil)
(defvar-local qq-guild-forum--request-owner nil)
(defvar-local qq-guild-forum--ewoc nil)
(defvar-local qq-guild-forum--node-table nil)

(defun qq-guild-forum--view-id (guild-id channel-id)
  "Return Appkit view identity for forum GUILD-ID and CHANNEL-ID."
  (list 'guild-forum guild-id channel-id))

(defun qq-guild-forum--buffer-name (guild-id channel-id)
  "Return forum buffer name for GUILD-ID and CHANNEL-ID."
  (let ((channel (qq-state-guild-channel guild-id channel-id)))
    (format "*qq-forum:%s*" (or (alist-get 'name channel) channel-id))))

(defun qq-guild-forum--messages ()
  "Return current forum posts in newest-first directory order."
  (nreverse
   (seq-filter
    (lambda (message)
      (equal (alist-get 'message-type message) "guild-forum-post"))
    (qq-state-session-messages qq-guild-forum--session-key))))

(defun qq-guild-forum--post-at-point (&optional position)
  "Return the forum post represented at POSITION or point."
  (let* ((probe (or position (point)))
         (post-id
          (or (get-text-property probe 'qq-guild-forum-post-id)
              (save-excursion
                (goto-char probe)
                (get-text-property (line-beginning-position)
                                   'qq-guild-forum-post-id)))))
    (and post-id
         (seq-find
          (lambda (message) (equal (alist-get 'id message) post-id))
          (qq-guild-forum--messages)))))

(defun qq-guild-forum--time-string (message)
  "Return compact local time for forum MESSAGE."
  (format-time-string "%m-%d %H:%M"
                      (seconds-to-time (or (alist-get 'time message) 0))))

(defun qq-guild-forum--segments-without-title-prefix (segments title)
  "Remove exact leading text TITLE from a copy of SEGMENTS.

Native Feed titles are often a preview copied from the beginning of the post
body.  Return SEGMENTS unchanged unless consecutive leading text segments
cover TITLE exactly; non-text content is never guessed through."
  (let ((remaining title)
        (source segments)
        projected
        valid-p)
    (while (and source (not (string-empty-p remaining)))
      (let* ((segment (car source))
             (data (alist-get 'data segment))
             (text (and (equal (alist-get 'type segment) "text")
                        (alist-get 'text data))))
        (cond
         ((not (stringp text))
          (setq source nil))
         ((string-prefix-p text remaining)
          (setq remaining (substring remaining (length text))
                source (cdr source)))
         ((string-prefix-p remaining text)
          (let ((copy (copy-tree segment)))
            (setf (alist-get 'text (alist-get 'data copy))
                  (substring text (length remaining)))
            (push copy projected)
            (setq remaining ""
                  source (cdr source))))
         (t (setq source nil)))))
    (setq valid-p (string-empty-p remaining))
    (if valid-p
        (nconc (nreverse projected) (copy-tree source))
      (copy-tree segments))))

(defun qq-guild-forum--body-message (message title)
  "Return a rendering copy of MESSAGE without duplicated TITLE prefix."
  (let ((copy (copy-tree message)))
    (setf (alist-get 'segments copy)
          (qq-guild-forum--segments-without-title-prefix
           (alist-get 'segments copy) title))
    copy))

(defun qq-guild-forum--discussion-width ()
  "Return the current Appkit discussion surface width."
  (or (appkit-view-window-fill-column nil qq-chat-auto-fill-margin-columns)
      (and (integerp fill-column) (> fill-column 0) fill-column)
      80))

(defun qq-guild-forum--post-discussion-entry (message show-comment-count-p)
  "Return Appkit discussion entry for forum MESSAGE.

When SHOW-COMMENT-COUNT-P is non-nil, include the directory metadata footer."
  (let* ((post-id (alist-get 'id message))
         (title (or (alist-get 'forum-title message) ""))
         (body (qq-guild-forum--body-message message title)))
    (appkit-discussion-entry-create
     :key (list 'forum-post post-id)
     :depth 0
     :avatar (qq-media-message-avatar-image message)
     :avatar-fallback "@"
     :avatar-action
     (lambda ()
       (interactive)
       (qq-media-open-message-avatar message))
     :avatar-help-echo "Open channel member avatar"
     :heading-inserter
     (lambda ()
       (qq-chat--insert-message-sender message 'qq-msg-user-title))
     :heading-line-face 'qq-msg-heading
     :time (qq-guild-forum--time-string message)
     :time-face 'qq-msg-status
     :body-inserter
     (lambda (prefix properties)
       (unless (string-empty-p title)
         (appkit-ui-insert-prefixed-lines
          prefix title :face 'bold :properties properties))
       (when (or (alist-get 'segments body) (string-empty-p title))
         (qq-chat--insert-message-body body prefix properties)))
     :footer
     (and show-comment-count-p
          (format "%d 条评论"
                  (or (alist-get 'forum-comment-count message) 0)))
     :footer-face 'shadow
     :properties
     (list 'qq-guild-forum-post-id post-id
           'rear-nonsticky '(qq-guild-forum-post-id)))))

(defun qq-guild-forum--insert-post (message &optional detail-p)
  "Insert native forum MESSAGE through the Appkit discussion surface.

DETAIL-P suppresses directory-only comment-count metadata."
  (appkit-discussion-insert-entry
   (qq-guild-forum--post-discussion-entry message (not detail-p))
   :width (qq-guild-forum--discussion-width)))

(defun qq-guild-forum--entry-printer (entry)
  "Insert persistent forum ENTRY."
  (qq-guild-forum--insert-post (qq-guild-forum--entry-message entry)))

(defun qq-guild-forum--project-entries ()
  "Return stable EWOC entries for current canonical forum posts."
  (mapcar
   (lambda (message)
     (qq-guild-forum--entry-create
      :key (alist-get 'id message) :message message))
   (qq-guild-forum--messages)))

(defun qq-guild-forum--header-line ()
  "Return current forum header line."
  (let ((channel (qq-state-guild-channel
                  qq-guild-forum--guild-id qq-guild-forum--channel-id)))
    (format " %s · ▤ %s  %d 个帖子%s"
            (or (alist-get 'guild_name channel) "QQ频道")
            (or (alist-get 'name channel) qq-guild-forum--channel-id)
            (length (qq-guild-forum--messages))
            (cond
             (qq-guild-forum--loading " · loading")
             (qq-guild-forum--error " · 加载失败")
             (qq-guild-forum--finished-p " · 已到最早")
             (t "")))))

(defun qq-guild-forum--reconcile (&optional force-keys)
  "Reconcile the Feed projection, redrawing retained FORCE-KEYS."
  (setq qq-guild-forum--node-table
        (appkit-ewoc-reconcile
         qq-guild-forum--ewoc
         (qq-guild-forum--project-entries)
         #'qq-guild-forum--entry-key
         :force-keys force-keys)))

(defun qq-guild-forum--sync-invalidations (view invalidations)
  "Synchronize forum VIEW from Appkit INVALIDATIONS."
  (when (appkit-view-live-p view)
    (let* ((parts (appkit-invalidations-parts invalidations))
           (entry-keys (appkit-invalidations-entry-keys invalidations))
           (reconcile-p
            (or (appkit-invalidations-structure-p invalidations)
                (memq 'posts parts)
                entry-keys)))
      (when reconcile-p
        (appkit-with-content-update view
          (let ((snapshot
                 (appkit-position-capture
                  :anchor-property 'qq-guild-forum-post-id
                  :preserve-window-start t)))
            (with-silent-modifications
              (qq-guild-forum--reconcile entry-keys))
            (when snapshot
              (appkit-position-restore snapshot)))))
      (when (or reconcile-p (memq 'header parts))
        (force-mode-line-update)))))

(defun qq-guild-forum--request-current-p (owner)
  "Return non-nil when OWNER owns current forum request and exact view."
  (let ((buffer (plist-get owner :buffer))
        (view (plist-get owner :view)))
    (and (buffer-live-p buffer)
         (appkit-view-live-p view)
         (with-current-buffer buffer
           (and (eq owner qq-guild-forum--request-owner)
                (eq view (appkit-current-view))
                (equal (plist-get owner :session-key)
                       qq-guild-forum--session-key))))))

(defun qq-guild-forum--cancel-request ()
  "Cancel and forget the current forum page request."
  (let ((request qq-guild-forum--request))
    (setq qq-guild-forum--request nil
          qq-guild-forum--request-owner nil
          qq-guild-forum--loading nil)
    (when request
      (qq-api-cancel-request request))))

(defun qq-guild-forum--load-page (cursor &optional quiet)
  "Load one native forum page at opaque CURSOR.

An empty CURSOR replaces the authoritative first page."
  (if qq-guild-forum--loading
      (progn
        (unless quiet (message "qq: forum page is already loading"))
        nil)
    (let* ((view (appkit-current-view))
           (buffer (current-buffer))
           (owner (list :buffer buffer
                        :view view
                        :session-key qq-guild-forum--session-key
                        :cursor cursor))
           (pending t)
           request)
      (setq qq-guild-forum--loading t
            qq-guild-forum--error nil
            qq-guild-forum--request-owner owner)
      (appkit-request-sync view :part 'header)
      (setq request
            (qq-api-fetch-guild-forum-page
             qq-guild-forum--session-key cursor
             (lambda (page)
               (setq pending nil)
               (when (qq-guild-forum--request-current-p owner)
                 (with-current-buffer buffer
                   (setq qq-guild-forum--request nil
                         qq-guild-forum--request-owner nil
                         qq-guild-forum--loading nil
                         qq-guild-forum--next-cursor
                         (alist-get 'next_cursor page)
                         qq-guild-forum--finished-p
                         (eq (alist-get 'finished page) t))
                   (appkit-request-sync
                    view :structure t :parts '(posts header)))
                 (unless quiet
                   (message "qq: loaded %d forum posts"
                            (length (alist-get 'posts page))))))
             (lambda (response reason)
               (setq pending nil)
               (when (qq-guild-forum--request-current-p owner)
                 (with-current-buffer buffer
                   (setq qq-guild-forum--request nil
                         qq-guild-forum--request-owner nil
                         qq-guild-forum--loading nil
                         qq-guild-forum--error (or reason "加载失败"))
                   (appkit-request-sync view :parts '(posts header)))
                 (qq-api--default-error response reason)))))
      (when (and pending (qq-guild-forum--request-current-p owner))
        (setq qq-guild-forum--request request))
      request)))

(defun qq-guild-forum-refresh ()
  "Reload the authoritative newest forum page."
  (interactive)
  (qq-guild-forum--cancel-request)
  (setq qq-guild-forum--finished-p nil
        qq-guild-forum--next-cursor nil)
  (qq-guild-forum--load-page ""))

(defun qq-guild-forum-load-older (&optional quiet)
  "Load one older native Feed page, optionally QUIET."
  (interactive)
  (cond
   (qq-guild-forum--finished-p
    (unless quiet (message "qq: reached beginning of forum history")))
   ((not (qq-api-non-empty-string-p qq-guild-forum--next-cursor))
    (unless quiet (message "qq: forum cursor is not initialized")))
   (t
    (qq-guild-forum--load-page qq-guild-forum--next-cursor quiet))))

(defun qq-guild-forum--near-visible-bottom-p (window)
  "Return non-nil when WINDOW is near the forum buffer bottom."
  (let ((end (window-end window t)))
    (and end
         (<= (count-screen-lines end (point-max))
             (max 1 qq-guild-forum-auto-load-threshold)))))

(defun qq-guild-forum--window-scroll (window _display-start)
  "Continue native Feed pagination when WINDOW approaches the bottom."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (not qq-guild-forum--loading)
             (not qq-guild-forum--finished-p)
             (qq-guild-forum--near-visible-bottom-p window))
    (qq-guild-forum-load-older t)))

(defun qq-guild-forum-next-post ()
  "Move to the next forum post row."
  (interactive)
  (let ((next (next-single-property-change
               (point) 'qq-guild-forum-post-id nil (point-max))))
    (if (< next (point-max))
        (goto-char next)
      (message "qq: no next forum post"))))

(defun qq-guild-forum-previous-post ()
  "Move to the previous forum post row."
  (interactive)
  (let ((previous (previous-single-property-change
                   (line-beginning-position)
                   'qq-guild-forum-post-id nil (point-min))))
    (if (> previous (point-min))
        (goto-char (1- previous))
      (goto-char (point-min)))))

(defun qq-guild-forum-open-post ()
  "Open the native post and its independent comment directory."
  (interactive)
  (let ((post (or (qq-guild-forum--post-at-point)
                  (user-error "qq: point is not on a forum post"))))
    (qq-guild-forum-post-open post)))

(defun qq-guild-forum--setup-view (view)
  "Register state and media ownership for forum VIEW."
  (let ((state-handler
         (lambda (event)
           (when (and (appkit-view-live-p view)
                      (or (eq (plist-get event :type) 'reset)
                          (equal (plist-get event :session-key)
                                 (appkit-view-state view))))
             (appkit-with-live-view view
               (if (memq (plist-get event :type) '(reset history))
                   (appkit-request-sync view :structure t :part 'posts)
                 (appkit-request-sync
                  view
                  :entry (or (plist-get event :message-anchor)
                             (alist-get 'id (plist-get event :message)))))))))
        (media-handler
         (lambda (_key)
           (when (appkit-view-live-p view)
             (appkit-with-live-view view
               (appkit-request-sync
                view :entries
                (mapcar #'qq-guild-forum--entry-key
                        (qq-guild-forum--project-entries))))))))
    (add-hook 'qq-state-change-hook state-handler)
    (add-hook 'qq-media-cache-update-hook media-handler)
    (appkit-register-handle
     view 'hook (list 'qq-state-change-hook state-handler nil (current-buffer)))
    (appkit-register-handle
     view 'hook
     (list 'qq-media-cache-update-hook media-handler nil (current-buffer)))))

(defvar qq-guild-forum-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guild-forum-refresh)
    (define-key map (kbd "n") #'qq-guild-forum-next-post)
    (define-key map (kbd "p") #'qq-guild-forum-previous-post)
    (define-key map (kbd "RET") #'qq-guild-forum-open-post)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode qq-guild-forum-mode special-mode "QQ-Forum"
  "Major mode for a native QQ Guild Feed directory."
  (setq buffer-read-only t
        truncate-lines nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local qq-guild-forum--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (qq-guild-forum--header-line)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local qq-guild-forum--ewoc
                (ewoc-create #'qq-guild-forum--entry-printer nil nil t)))
  (add-hook 'window-scroll-functions #'qq-guild-forum--window-scroll nil t)
  (add-hook 'change-major-mode-hook #'qq-guild-forum--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'qq-guild-forum--cancel-request nil t))

(defun qq-guild-forum-open (guild-id channel-id)
  "Open native forum GUILD-ID and CHANNEL-ID without a chat composer."
  (let* ((channel (or (qq-state-guild-channel guild-id channel-id)
                      (user-error "qq: channel is no longer in the directory")))
         (_kind (unless (equal (alist-get 'kind channel) "forum")
                  (user-error "qq: channel is not a forum")))
         (session-key (qq-state-guild-channel-session-key guild-id channel-id))
         (app (qq-runtime-app))
         (view
          (appkit-open-view
           :app app
           :id (qq-guild-forum--view-id guild-id channel-id)
           :mode 'qq-guild-forum-mode
           :buffer-name (qq-guild-forum--buffer-name guild-id channel-id)
           :state session-key
           :sync-function #'qq-guild-forum--sync-invalidations
           :parts '(posts header)
           :setup #'qq-guild-forum--setup-view
           :select t))
         (buffer (appkit-view-buffer view)))
    (qq-state-upsert-session
     session-key
     `((title . ,(format "%s · ▤ %s"
                        (alist-get 'guild_name channel)
                        (alist-get 'name channel)))
       (guild-name . ,(alist-get 'guild_name channel))
       (channel-name . ,(alist-get 'name channel))
       (channel-kind . "forum"))
     nil)
    (with-current-buffer buffer
      (setq qq-guild-forum--guild-id guild-id
            qq-guild-forum--channel-id channel-id
            qq-guild-forum--session-key session-key)
      (appkit-request-sync view :structure t :parts '(posts header))
      (appkit-sync-invalidations view)
      (unless (or qq-guild-forum--loading
                  qq-guild-forum--next-cursor
                  qq-guild-forum--finished-p)
        (qq-guild-forum--load-page "" t)))
    buffer))

(provide 'qq-guild-forum)

;;; qq-guild-forum.el ends here
