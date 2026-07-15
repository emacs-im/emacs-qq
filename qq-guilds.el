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
(require 'qq-guild-channel)
(require 'qq-guild-channel-type)
(require 'qq-guild-forum)
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
(defvar-local qq-guilds--collapsed-guilds nil
  "Set of collapsed Guild IDs in the current directory view.")
(defvar-local qq-guilds--filter nil
  "Case-insensitive substring filter for the current directory view.")

(cl-defstruct (qq-guilds--entry
               (:constructor qq-guilds--entry-create))
  key
  type
  text
  face
  guild-id
  expanded-p
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

(defun qq-guilds--header-line ()
  "Return the Guild directory header."
  (let* ((directory (qq-state-guild-directory))
         (guild-count (length (alist-get 'guilds directory)))
         (channel-count (length (alist-get 'channels directory))))
    (format " QQ频道  %d 个频道 · %d 个子频道%s%s"
            guild-count channel-count
            (if qq-guilds--filter
                (format " · /%s/" qq-guilds--filter)
              "")
            (cond
             (qq-guilds--loading " · refreshing")
             (qq-guilds--error " · 刷新失败")
             (t "")))))

(defun qq-guilds--guild-entry-key (guild-id)
  "Return the stable Guild heading key for GUILD-ID."
  (cons 'guild guild-id))

(defun qq-guilds--channel-entry-key (guild-id channel-id)
  "Return the stable channel row key for GUILD-ID and CHANNEL-ID."
  (list 'channel guild-id channel-id))

(defun qq-guilds--category-entry-key (guild-id category-id)
  "Return the stable category row key for GUILD-ID and CATEGORY-ID."
  (list 'category guild-id category-id))

(defun qq-guilds--display-name (name fallback)
  "Return non-empty NAME, or a printable FALLBACK."
  (if (and (stringp name) (not (string-empty-p name)))
      name
    (format "%s" (or fallback "未命名"))))

(defun qq-guilds--insert-guild (entry key)
  "Insert one foldable Guild heading ENTRY carrying stable KEY."
  (let* ((guild-id (qq-guilds--entry-guild-id entry))
         (expanded-p (qq-guilds--entry-expanded-p entry))
         (start (point)))
    (insert (if expanded-p "▾ " "▸ ")
            (qq-guilds--entry-text entry))
    (make-text-button
     start (point)
     'follow-link t
     'face (qq-guilds--entry-face entry)
     'mouse-face 'highlight
     'help-echo (if expanded-p "折叠 QQ 频道" "展开 QQ 频道")
     'qq-guild-id guild-id
     'action (lambda (button)
               (qq-guilds-toggle-guild
                (button-get button 'qq-guild-id))))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'qq-guilds-key key
           'qq-guild-id guild-id
           'qq-guild-action 'toggle))))

(defun qq-guilds--insert-channel (channel key)
  "Insert one clickable CHANNEL row carrying stable KEY."
  (let* ((guild-id (alist-get 'guild_id channel))
         (channel-id (alist-get 'channel_id channel))
         (name (alist-get 'name channel))
         (kind (alist-get 'kind channel))
         (session
          (qq-state-session
           (qq-state-guild-channel-session-key guild-id channel-id)))
         (unread (or (alist-get 'unread-count session) 0))
         (start (point)))
    (insert "    " (qq-guild-channel-type-icon kind) " "
            (qq-guilds--display-name name channel-id))
    (make-text-button
     start (point)
     'follow-link t
     'face 'qq-guilds-channel
     'mouse-face 'highlight
     'help-echo (format "打开%s频道 %s · %s"
                        (qq-guild-channel-type-label kind)
                        (alist-get 'guild_name channel) name)
     'qq-guild-id guild-id
     'qq-guild-channel-id channel-id
     'action (lambda (button)
               (qq-guilds-open-channel
                (button-get button 'qq-guild-id)
                (button-get button 'qq-guild-channel-id))))
    (insert (propertize
             (format "  %s" (qq-guild-channel-type-label kind))
             'face 'shadow))
    (when (alist-get 'pinned_at channel)
      (insert (propertize "  置顶" 'face 'shadow)))
    (when (> unread 0)
      (insert (propertize (format "  %d 未读" unread)
                          'face 'font-lock-warning-face)))
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'qq-guilds-key key
           'qq-guild-id guild-id
           'qq-guild-channel-id channel-id
           'qq-guild-action 'open))))

(defun qq-guilds--entry-printer (entry)
  "Insert one persistent Guild directory ENTRY."
  (let ((key (qq-guilds--entry-key entry)))
    (pcase (qq-guilds--entry-type entry)
      ('guild
       (qq-guilds--insert-guild entry key))
      ('category
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

(defun qq-guilds--matches-p (&rest values)
  "Return non-nil when current filter matches one of VALUES."
  (or (null qq-guilds--filter)
      (let ((case-fold-search t))
        (seq-some
         (lambda (value)
           (and (stringp value)
                (string-match-p (regexp-quote qq-guilds--filter) value)))
         values))))

(defun qq-guilds--channel-table (channels)
  "Return exact (GUILD-ID . CHANNEL-ID) to channel table for CHANNELS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (channel channels table)
      (puthash (cons (alist-get 'guild_id channel)
                     (alist-get 'channel_id channel))
               channel table))))

(defun qq-guilds--category-channels (category channel-table guild-match-p)
  "Return visible channels for CATEGORY from CHANNEL-TABLE.

GUILD-MATCH-P means the parent Guild itself matched the active filter."
  (let ((category-match-p
         (qq-guilds--matches-p (alist-get 'name category)))
        (guild-id (alist-get 'guild_id category)))
    (seq-keep
     (lambda (channel-id)
       (let ((channel (gethash (cons guild-id channel-id) channel-table)))
         (unless channel
           (error "qq: directory category references unknown channel %s"
                  channel-id))
         (and (or guild-match-p category-match-p
                  (qq-guilds--matches-p
                   (alist-get 'name channel)
                   (qq-guild-channel-type-label (alist-get 'kind channel))))
              channel)))
     (alist-get 'channel_ids category))))

(defun qq-guilds--project-guild-group (guild categories channel-table)
  "Project GUILD, CATEGORIES, and CHANNEL-TABLE into stable entries."
  (let* ((guild-id (alist-get 'guild_id guild))
         (guild-name (qq-guilds--display-name (alist-get 'name guild) guild-id))
         (guild-match-p (qq-guilds--matches-p guild-name))
         (expanded-p
          (or qq-guilds--filter
              (not (gethash guild-id qq-guilds--collapsed-guilds))))
         (visible-categories
          (seq-keep
           (lambda (category)
             (let ((visible
                    (qq-guilds--category-channels
                     category channel-table guild-match-p)))
               (and visible (cons category visible))))
           categories))
         (visible-p (or (null qq-guilds--filter) guild-match-p
                        visible-categories))
         entries)
    (when visible-p
      (push
       (qq-guilds--entry-create
        :key (qq-guilds--guild-entry-key guild-id)
        :type 'guild :text guild-name :face 'qq-guilds-title
        :guild-id guild-id :expanded-p expanded-p)
       entries)
      (when expanded-p
        (if visible-categories
            (dolist (pair visible-categories)
              (let ((category (car pair))
                    (channels (cdr pair)))
                (unless (eq (alist-get 'uncategorized category) t)
                  (push
                   (qq-guilds--entry-create
                    :key (qq-guilds--category-entry-key
                          guild-id (alist-get 'category_id category))
                    :type 'category
                    :text (format "    ── %s ──"
                                  (qq-guilds--display-name
                                   (alist-get 'name category)
                                   (alist-get 'category_id category)))
                    :face 'shadow)
                   entries))
                (dolist (channel channels)
                  (push
                   (qq-guilds--entry-create
                    :key (qq-guilds--channel-entry-key
                          guild-id (alist-get 'channel_id channel))
                    :type 'channel :channel channel)
                   entries))))
          (push
           (qq-guilds--entry-create
            :key (list 'guild-empty guild-id) :type 'note
            :text "    暂无匹配的子频道" :face 'shadow)
           entries)))
      (push
       (qq-guilds--entry-create
        :key (list 'guild-gap guild-id) :type 'blank)
       entries))
    (nreverse entries)))

(defun qq-guilds--project-entries ()
  "Project authoritative state and transient status into stable entries."
  (let* ((directory (qq-state-guild-directory))
         (guilds (alist-get 'guilds directory))
         (categories (alist-get 'categories directory))
         (channels (alist-get 'channels directory))
         (channel-table (qq-guilds--channel-table channels))
         entries)
    (when-let* ((status (qq-guilds--project-status-entry)))
      (push status entries))
    (dolist (guild guilds)
      (setq entries
            (nconc entries
                   (qq-guilds--project-guild-group
                    guild
                    (seq-filter
                     (lambda (category)
                       (equal (alist-get 'guild_id category)
                              (alist-get 'guild_id guild)))
                     categories)
                    channel-table))))
    ;; An in-flight refresh or failed refresh is itself the useful empty-state
    ;; note.  Do not flash a contradictory "no Guilds" row underneath it.
    (when (and (null guilds)
               (null qq-guilds--loading) (null qq-guilds--error))
      (setq entries
            (nconc entries
                   (list
                    (qq-guilds--entry-create
                     :key 'empty :type 'note
                     :text "  没有可见的 QQ 频道。" :face 'shadow)))))
    (when (and qq-guilds--filter guilds
               (not (seq-some
                     (lambda (entry)
                       (eq (qq-guilds--entry-type entry) 'guild))
                     entries)))
      (setq entries
            (nconc entries
                   (list
                    (qq-guilds--entry-create
                     :key 'filter-empty :type 'note
                     :text "  没有匹配的频道或子频道。" :face 'shadow)))))
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

(defun qq-guilds-toggle-guild (&optional guild-id)
  "Toggle GUILD-ID, or the Guild heading at point."
  (interactive)
  (let* ((key (qq-guilds--entry-key-at-point))
         (id (or guild-id
                 (and (eq (car-safe key) 'guild) (cdr key))
                 (user-error "qq: point is not on a QQ 频道 heading"))))
    (if (gethash id qq-guilds--collapsed-guilds)
        (remhash id qq-guilds--collapsed-guilds)
      (puthash id t qq-guilds--collapsed-guilds))
    (let ((view (qq-guilds--ensure-view)))
      (appkit-request-sync view :structure t :part 'directory)
      (appkit-sync-invalidations view))))

(defun qq-guilds-filter (query)
  "Filter the QQ Guild directory by substring QUERY."
  (interactive
   (list (read-string "Filter QQ channels: " qq-guilds--filter)))
  (setq qq-guilds--filter
        (and (not (string-empty-p query)) query))
  (let ((view (qq-guilds--ensure-view)))
    (appkit-request-sync view :structure t :part 'directory)
    (appkit-sync-invalidations view)))

(defun qq-guilds-clear-filter ()
  "Clear the active QQ Guild directory filter."
  (interactive)
  (unless qq-guilds--filter
    (user-error "qq: no Guild directory filter is active"))
  (setq qq-guilds--filter nil)
  (let ((view (qq-guilds--ensure-view)))
    (appkit-request-sync view :structure t :part 'directory)
    (appkit-sync-invalidations view)))

(defun qq-guilds-activate ()
  "Activate the fold or channel action at point."
  (interactive)
  (pcase (get-text-property (line-beginning-position) 'qq-guild-action)
    ('toggle
     (qq-guilds-toggle-guild
      (get-text-property (line-beginning-position) 'qq-guild-id)))
    ('open
     (qq-guilds-open-channel
      (get-text-property (line-beginning-position) 'qq-guild-id)
      (get-text-property (line-beginning-position) 'qq-guild-channel-id)))
    (_ (user-error "qq: no action at point"))))

(defun qq-guilds-open-channel (guild-id channel-id)
  "Open the channel identified by GUILD-ID and CHANNEL-ID."
  (interactive)
  (let* ((channel (or (qq-state-guild-channel guild-id channel-id)
                      (user-error "QQ: channel is no longer in the directory")))
         (kind (alist-get 'kind channel)))
    (pcase (qq-guild-channel-open-mode kind)
      ('timeline
       (let ((key (qq-state-guild-channel-session-key guild-id channel-id)))
         (qq-state-upsert-session
          key `((title . ,(format "%s · #%s"
                                 (alist-get 'guild_name channel)
                                 (alist-get 'name channel)))
                (guild-name . ,(alist-get 'guild_name channel))
                (channel-name . ,(alist-get 'name channel))
                (channel-kind . ,kind))
          nil)
         (qq-chat-open key)))
      ('forum (qq-guild-forum-open guild-id channel-id))
      ('inspect (qq-guild-channel-open guild-id channel-id))
      (mode (error "qq: unknown Guild channel open mode %S" mode)))))

(defvar qq-guilds-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guilds-refresh)
    (define-key map (kbd "/") #'qq-guilds-filter)
    (define-key map (kbd "C-c C-k") #'qq-guilds-clear-filter)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "TAB") #'qq-guilds-toggle-guild)
    (define-key map (kbd "RET") #'qq-guilds-activate)
    (define-key map (kbd "q") #'quit-window)
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
  (setq-local qq-guilds--collapsed-guilds (make-hash-table :test #'equal))
  (setq-local qq-guilds--filter nil)
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
