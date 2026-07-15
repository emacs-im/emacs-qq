;;; qq-guilds.el --- QQ Guild directory buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Hierarchical projection of Linux QQ's independent Guild message directory.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-runtime)
(require 'qq-state)

(declare-function qq-api-refresh-guild-directory
                  "qq-api" (&optional callback errback))
(declare-function qq-state-guild-directory "qq-state" ())
(declare-function qq-state-guild-directory-loaded-p "qq-state" ())
(declare-function qq-state-guild "qq-state" (guild-id))
(declare-function qq-state-guild-channel "qq-state" (guild-id channel-id))
(declare-function qq-state-guild-channel-session-key
                  "qq-state" (guild-id channel-id))

(defconst qq-guilds-buffer-name "*qq-guilds*"
  "Name of the QQ Guild directory buffer.")

(defvar-local qq-guilds--loading nil)
(defvar-local qq-guilds--error nil)
(defvar-local qq-guilds--refresh-owner nil)
(defvar-local qq-guilds--refresh-request nil)
(defvar-local qq-guilds--ewoc nil
  "Persistent EWOC containing Guild directory entries.")
(defvar-local qq-guilds--node-table nil
  "Stable Guild directory entry key to EWOC node table.")

(cl-defstruct (qq-guilds--entry
               (:constructor qq-guilds--entry-create))
  key
  type
  text
  face
  channel)

(defconst qq-guilds--view-id 'guilds
  "Appkit identity of the singleton QQ Guild directory view.")

(defface qq-guilds-title
  '((t :inherit font-lock-function-name-face :weight bold :height 1.15))
  "Face used for Guild names."
  :group 'qq)

(defface qq-guilds-channel
  '((t :inherit default))
  "Face used for channel links."
  :group 'qq)

(defconst qq-guilds--channel-kind-labels
  '(("text" . "文字")
    ("forum" . "论坛")
    ("live" . "直播")
    ("application" . "应用")
    ("schedule" . "日程"))
  "Protocol channel kinds and their compact directory labels.")

(defun qq-guilds--header-line ()
  "Return the Guild directory header."
  (let* ((directory (qq-state-guild-directory))
         (guild-count (length (alist-get 'guilds directory)))
         (channel-count (length (alist-get 'channels directory))))
    (format " QQ频道  %d 个频道 · %d 个子频道%s"
            guild-count channel-count
            (cond
             (qq-guilds--loading " · refreshing")
             (qq-guilds--error " · 刷新失败")
             (t "")))))

(defun qq-guilds--channels-by-guild (channels)
  "Return CHANNELS grouped by their exact guild_id."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (channel channels table)
      (push channel (gethash (alist-get 'guild_id channel) table)))
    (maphash (lambda (guild-id items)
               (puthash guild-id (nreverse items) table))
             table)
    table))

(defun qq-guilds--guild-entry-key (guild-id)
  "Return the stable Guild heading key for GUILD-ID."
  (cons 'guild guild-id))

(defun qq-guilds--channel-entry-key (guild-id channel-id)
  "Return the stable channel row key for GUILD-ID and CHANNEL-ID."
  (list 'channel guild-id channel-id))

(defun qq-guilds--display-name (name fallback)
  "Return non-empty NAME, or a printable FALLBACK."
  (if (and (stringp name) (not (string-empty-p name)))
      name
    (format "%s" (or fallback "未命名"))))

(defun qq-guilds--insert-channel (channel key)
  "Insert one clickable CHANNEL row carrying stable KEY."
  (let* ((guild-id (alist-get 'guild_id channel))
         (channel-id (alist-get 'channel_id channel))
         (name (alist-get 'name channel))
         (kind (alist-get 'kind channel))
         (start (point)))
    (insert "    # " (qq-guilds--display-name name channel-id))
    (make-text-button
     start (point)
     'follow-link t
     'face 'qq-guilds-channel
     'mouse-face 'highlight
     'help-echo (format "打开%s频道 %s · #%s"
                        (or (cdr (assoc kind qq-guilds--channel-kind-labels)) "")
                        (alist-get 'guild_name channel) name)
     'qq-guild-id guild-id
     'qq-guild-channel-id channel-id
     'action (lambda (button)
               (qq-guilds-open-channel
                (button-get button 'qq-guild-id)
                (button-get button 'qq-guild-channel-id))))
    (when-let* ((label (cdr (assoc kind qq-guilds--channel-kind-labels))))
      (insert (propertize (format "  %s" label) 'face 'shadow)))
    (when (alist-get 'pinned_at channel)
      (insert (propertize "  置顶" 'face 'shadow)))
    (insert "\n")
    (add-text-properties start (point) (list 'qq-guilds-key key))))

(defun qq-guilds--entry-printer (entry)
  "Insert one persistent Guild directory ENTRY."
  (let ((key (qq-guilds--entry-key entry)))
    (pcase (qq-guilds--entry-type entry)
      ('heading
       (appkit-view-insert-heading-line
        (qq-guilds--entry-text entry)
        :face (qq-guilds--entry-face entry)
        :line-properties (list 'qq-guilds-key key)))
      ('note
       (appkit-view-insert-note-line
        (qq-guilds--entry-text entry)
        :face (qq-guilds--entry-face entry)
        :line-properties (list 'qq-guilds-key key)))
      ('channel
       (qq-guilds--insert-channel (qq-guilds--entry-channel entry) key))
      ('blank
       (insert (propertize "\n" 'qq-guilds-key key)))
      (type (error "QQ: unknown Guild directory entry type %S" type)))))

(defun qq-guilds--project-status-entry ()
  "Return the current transient status entry, or nil."
  (cond
   (qq-guilds--loading
    (qq-guilds--entry-create
     :key 'status :type 'note
     :text "  正在读取 Linux QQ 频道目录…" :face 'shadow))
   (qq-guilds--error
    (qq-guilds--entry-create
     :key 'status :type 'note
     :text (concat "  " qq-guilds--error) :face 'error))))

(defun qq-guilds--project-guild-group (guild channels)
  "Project GUILD and its ordered CHANNELS into stable entries."
  (let* ((guild-id (alist-get 'guild_id guild))
         (heading
          (qq-guilds--entry-create
           :key (qq-guilds--guild-entry-key guild-id)
           :type 'heading
           :text (qq-guilds--display-name (alist-get 'name guild) guild-id)
           :face 'qq-guilds-title)))
    (append
     (list heading)
     (if channels
         (mapcar
          (lambda (channel)
            (qq-guilds--entry-create
             :key (qq-guilds--channel-entry-key
                   guild-id (alist-get 'channel_id channel))
             :type 'channel :channel channel))
          channels)
       (list
        (qq-guilds--entry-create
         :key (list 'guild-empty guild-id) :type 'note
         :text "    暂无消息列表子频道" :face 'shadow)))
     (list
      (qq-guilds--entry-create
       :key (list 'guild-gap guild-id) :type 'blank)))))

(defun qq-guilds--project-orphan-group (guild-id channels)
  "Project orphaned CHANNELS belonging to GUILD-ID."
  (let ((guild-name (alist-get 'guild_name (car channels))))
    (append
     (list
      (qq-guilds--entry-create
       ;; Guild identity does not change when its metadata arrives later.
       ;; Sharing this key with a known Guild preserves the heading node and
       ;; semantic position while its fallback title is upgraded.
       :key (qq-guilds--guild-entry-key guild-id) :type 'heading
       :text (qq-guilds--display-name guild-name guild-id)
       :face 'qq-guilds-title))
     (mapcar
      (lambda (channel)
        (qq-guilds--entry-create
         :key (qq-guilds--channel-entry-key
               guild-id (alist-get 'channel_id channel))
         :type 'channel :channel channel))
      channels)
     (list
      (qq-guilds--entry-create
       :key (list 'orphan-gap guild-id) :type 'blank)))))

(defun qq-guilds--project-entries ()
  "Project authoritative state and transient status into stable entries."
  (let* ((directory (qq-state-guild-directory))
         (guilds (alist-get 'guilds directory))
         (channels (alist-get 'channels directory))
         (by-guild (qq-guilds--channels-by-guild channels))
         (known-guilds (make-hash-table :test #'equal))
         (orphan-seen (make-hash-table :test #'equal))
         orphan-order
         entries)
    (when-let* ((status (qq-guilds--project-status-entry)))
      (push status entries))
    (dolist (guild guilds)
      (puthash (alist-get 'guild_id guild) t known-guilds))
    (dolist (guild guilds)
      (setq entries
            (nconc entries
                   (qq-guilds--project-guild-group
                    guild (gethash (alist-get 'guild_id guild) by-guild)))))
    ;; Keep native rows whose Guild metadata is absent visible, while grouping
    ;; each orphan Guild once instead of inventing a synthetic parent object.
    (dolist (channel channels)
      (let ((guild-id (alist-get 'guild_id channel)))
        (unless (or (gethash guild-id known-guilds)
                    (gethash guild-id orphan-seen))
          (puthash guild-id t orphan-seen)
          (push guild-id orphan-order))))
    (dolist (guild-id (nreverse orphan-order))
      (setq entries
            (nconc entries
                   (qq-guilds--project-orphan-group
                    guild-id (gethash guild-id by-guild)))))
    ;; An in-flight refresh or failed refresh is itself the useful empty-state
    ;; note.  Do not flash a contradictory "no Guilds" row underneath it.
    (when (and (null guilds) (null channels)
               (null qq-guilds--loading) (null qq-guilds--error))
      (setq entries
            (nconc entries
                   (list
                    (qq-guilds--entry-create
                     :key 'empty :type 'note
                     :text "  没有可见的 QQ 频道。" :face 'shadow)))))
    entries))

(defun qq-guilds--reconcile-directory ()
  "Reconcile the persistent EWOC with the current projected directory."
  (setq qq-guilds--node-table
        (appkit-ewoc-reconcile
         qq-guilds--ewoc (qq-guilds--project-entries)
         #'qq-guilds--entry-key)))

(defun qq-guilds--entry-key-at-point (&optional position)
  "Return the stable Guild entry key at POSITION or point."
  (let ((probe (or position (point))))
    (or (get-text-property probe 'qq-guilds-key)
        (save-excursion
          (goto-char probe)
          (get-text-property (line-beginning-position) 'qq-guilds-key)))))

(defun qq-guilds--view-current-p (view buffer)
  "Return non-nil when VIEW still owns Guild directory BUFFER."
  (and (buffer-live-p buffer)
       (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (equal qq-guilds--view-id (appkit-view-id view))
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-guilds-mode)
              (eq view (appkit-current-view))))))

(defun qq-guilds--live-view ()
  "Return the existing live Appkit Guild directory view, or nil.

This registry lookup deliberately does not call `qq-runtime-app'.  State
hooks must not start a QQ application merely to discover that the directory
is closed, and the registered view remains authoritative when its owning
buffer has been renamed."
  (when (appkit-app-live-p qq-runtime--app)
    (when-let* ((view (appkit-view-for-id
                       qq-runtime--app qq-guilds--view-id)))
      (and (qq-guilds--view-current-p view (appkit-view-buffer view)) view))))

(defun qq-guilds--ensure-view ()
  "Return the live Appkit view owning the current Guild directory."
  (let* ((app (qq-runtime-app))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal qq-guilds--view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-guilds--sync-invalidations
            (appkit-view-parts current) '(directory))
      current)
     ((appkit-view-live-p current)
      (error "QQ: Guild directory belongs to a different Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id qq-guilds--view-id
              :mode 'qq-guilds-mode
              :sync-function #'qq-guilds--sync-invalidations
              :parts '(directory))))
        (qq-guilds--setup-view view)
        view)))))

(defun qq-guilds--reset-buffer-work (buffer)
  "Reset requests and transient Guild state retained by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-guilds-mode)
        (qq-guilds--cancel-refresh)
        (setq qq-guilds--error nil)))))

(defun qq-guilds--release-view-work (view buffer)
  "Release BUFFER work while it is still owned by Guild VIEW."
  (when (and (buffer-live-p buffer)
             (with-current-buffer buffer
               (eq view (appkit-current-view))))
    (qq-guilds--reset-buffer-work buffer)))

(defun qq-guilds--setup-view (view)
  "Reset replacement state and register lifecycle cleanup for Guild VIEW."
  (let ((buffer (appkit-view-buffer view)))
    (qq-guilds--reset-buffer-work buffer)
    (appkit-register-handle
     view 'function
     (apply-partially #'qq-guilds--release-view-work view buffer))))

(defun qq-guilds--queue-view-sync (view)
  "Queue one coalesced directory synchronization for live VIEW."
  (when (appkit-view-live-p view)
    (appkit-request-sync view :structure t :part 'directory)))

(defun qq-guilds--sync-invalidations (view invalidations)
  "Consume coalesced Appkit INVALIDATIONS for Guild directory VIEW."
  (unless (ewoc-p qq-guilds--ewoc)
    (error "QQ: Guild directory view is not initialized"))
  (let* ((parts (appkit-invalidations-parts invalidations))
         (position-p (appkit-invalidations-position-p invalidations))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              (memq 'directory parts))))
    (when (and (appkit-view-live-p view)
               (or reconcile-p position-p))
      (appkit-with-content-update view
        (let ((snapshot
               (appkit-position-capture
                :anchor-property 'qq-guilds-key
                :preserve-window-start t)))
          (when reconcile-p
            (with-silent-modifications
              (qq-guilds--reconcile-directory)))
          (when snapshot
            (appkit-position-restore snapshot)))))
    ;; The header is an :eval form and reflects loading/error state without
    ;; forcing redisplay of every window showing the directory.
    (when (or reconcile-p (memq 'header parts))
      (force-mode-line-update))))

(defun qq-guilds-render ()
  "Synchronize the current authoritative Guild directory through Appkit."
  (interactive)
  (let ((view (qq-guilds--ensure-view)))
    (appkit-invalidate view :structure t :part 'directory)
    (appkit-sync-invalidations view)))

(defun qq-guilds--cancel-refresh ()
  "Cancel and forget work owned by the current Guild refresh."
  (setq qq-guilds--refresh-owner nil
        qq-guilds--loading nil)
  (when qq-guilds--refresh-request
    (qq-api-cancel-request qq-guilds--refresh-request))
  (setq qq-guilds--refresh-request nil))

(defun qq-guilds--refresh-current-p (view buffer owner)
  "Return non-nil when VIEW and OWNER still own the refresh in BUFFER."
  (and (qq-guilds--view-current-p view buffer)
       (with-current-buffer buffer
         (eq owner qq-guilds--refresh-owner))))

(defun qq-guilds--finish-refresh (view buffer owner &optional reason)
  "Finish VIEW and OWNER's Guild refresh in BUFFER with optional REASON."
  (when (qq-guilds--refresh-current-p view buffer owner)
    (with-current-buffer buffer
      (setq qq-guilds--refresh-owner nil
            qq-guilds--refresh-request nil
            qq-guilds--loading nil
            qq-guilds--error reason)
      (qq-guilds--queue-view-sync view))))

(defun qq-guilds-refresh ()
  "Refresh the QQ Guild directory."
  (interactive)
  (let ((view (qq-guilds--ensure-view)))
    (qq-guilds--cancel-refresh)
    (let ((buffer (current-buffer))
          (owner (list 'guild-directory-refresh (float-time))))
      (setq qq-guilds--loading t
            qq-guilds--error nil
            qq-guilds--refresh-owner owner)
      (qq-guilds--queue-view-sync view)
      (condition-case error-data
          (let ((request
                 (qq-api-refresh-guild-directory
                  (lambda (_directory)
                    (qq-guilds--finish-refresh view buffer owner))
                  (lambda (response reason)
                    (qq-guilds--finish-refresh
                     view buffer owner
                     (format "读取频道目录失败: %s"
                             (or reason (alist-get 'message response)
                                 "未知错误")))))))
            (when (qq-guilds--refresh-current-p view buffer owner)
              (setq qq-guilds--refresh-request request)))
        (error
         (qq-guilds--finish-refresh
          view buffer owner
          (format "读取频道目录失败: %s"
                  (error-message-string error-data))))))))

(defun qq-guilds--handle-state-change (event)
  "Invalidate the open Guild directory after relevant state EVENT."
  (when (memq (plist-get event :type) '(reset guild-directory-refreshed))
    (when-let* ((view (qq-guilds--live-view)))
      (appkit-with-live-view view
        (qq-guilds--queue-view-sync view)))))

(defun qq-guilds-open-channel (guild-id channel-id)
  "Open the channel identified by GUILD-ID and CHANNEL-ID."
  (interactive)
  (let* ((channel (or (qq-state-guild-channel guild-id channel-id)
                      (user-error "QQ: channel is no longer in the directory")))
         (key (qq-state-guild-channel-session-key guild-id channel-id)))
    (qq-state-upsert-session
     key `((title . ,(format "%s · #%s"
                            (alist-get 'guild_name channel)
                            (alist-get 'name channel)))
           (guild-name . ,(alist-get 'guild_name channel))
           (channel-name . ,(alist-get 'name channel))
           (channel-kind . ,(alist-get 'kind channel)))
     nil)
    (qq-chat-open key)))

(defvar qq-guilds-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guilds-refresh)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "RET") #'push-button)
    map))

(define-derived-mode qq-guilds-mode special-mode "QQ-Guilds"
  "Major mode for the hierarchical QQ Guild directory."
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local qq-guilds--loading nil)
  (setq-local qq-guilds--error nil)
  (setq-local qq-guilds--refresh-owner nil)
  (setq-local qq-guilds--refresh-request nil)
  (setq-local qq-guilds--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (qq-guilds--header-line)))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (setq-local qq-guilds--ewoc
                (ewoc-create #'qq-guilds--entry-printer nil nil t)))
  (add-hook 'change-major-mode-hook #'qq-guilds--cancel-refresh nil t)
  (add-hook 'kill-buffer-hook #'qq-guilds--cancel-refresh nil t))

(defun qq-guilds-open ()
  "Open the QQ Guild directory buffer."
  (interactive)
  (let* ((app (qq-runtime-app))
         (fresh-p (null (appkit-view-for-id app qq-guilds--view-id)))
         (view
          (appkit-open-view
           :app app
           :id qq-guilds--view-id
           :mode 'qq-guilds-mode
           :buffer-name qq-guilds-buffer-name
           :sync-function #'qq-guilds--sync-invalidations
           :parts '(directory)
           :setup #'qq-guilds--setup-view
           :select t))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (unless (or qq-guilds--loading
                  (qq-state-guild-directory-loaded-p))
        ;; Establish the loading model before the first projection, so a
        ;; fresh open cannot expose a contradictory transient empty state.
        (qq-guilds-refresh))
      (appkit-invalidate view :structure t :part 'directory)
      (if fresh-p
          (appkit-sync-invalidations view)
        (appkit-schedule-sync view)))
    buffer))

(add-hook 'qq-state-change-hook #'qq-guilds--handle-state-change)

(provide 'qq-guilds)

;;; qq-guilds.el ends here
