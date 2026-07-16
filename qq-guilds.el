;;; qq-guilds.el --- QQ Guild directory buffer -*- lexical-binding: t; -*-

;;; Commentary:

;; Hierarchical projection of Linux QQ's independent Guild message directory.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'appkit-invalidation)
(require 'appkit-transaction)
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
(defvar-local qq-guilds--filter nil
  "Case-insensitive substring filter for the current directory view.")

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

(defun qq-guilds--insert-channel (_surface entry)
  "Insert the application-owned content of channel ENTRY."
  (let* ((channel (appkit-directory-entry-payload entry))
         (guild-id (alist-get 'guild_id channel))
         (channel-id (alist-get 'channel_id channel))
         (name (alist-get 'name channel))
         (kind (alist-get 'kind channel))
         (session
          (qq-state-session
           (qq-state-guild-channel-session-key guild-id channel-id)))
         (unread (or (alist-get 'unread-count session) 0)))
    (insert (qq-guild-channel-type-icon kind) " "
            (qq-guilds--display-name name channel-id))
    (insert (propertize
             (format "  %s" (qq-guild-channel-type-label kind))
             'face 'shadow))
    (when (alist-get 'pinned_at channel)
      (insert (propertize "  置顶" 'face 'shadow)))
    (when (> unread 0)
      (insert (propertize (format "  %d 未读" unread)
                          'face 'font-lock-warning-face)))
    (insert "\n")))

(defun qq-guilds--activate-channel (_surface entry)
  "Open the QQ Guild channel carried by directory ENTRY."
  (let ((channel (appkit-directory-entry-payload entry)))
    (qq-guilds-open-channel
     (alist-get 'guild_id channel)
     (alist-get 'channel_id channel))))

(defun qq-guilds--fold-changed (_surface _entry _expanded-p)
  "Synchronize the current directory after an Appkit fold changed."
  (let ((view (qq-guilds--ensure-view)))
    (appkit-request-sync view :structure t :part 'directory)
    (appkit-sync-invalidations view)))

(defun qq-guilds--project-status-entry ()
  "Return the current transient status entry, or nil."
  (cond
   (qq-guilds--loading
    (appkit-directory-entry-create
     :key 'status :role 'note
     :label "正在读取 Linux QQ 频道目录…" :indent 2 :face 'shadow))
   (qq-guilds--error
    (appkit-directory-entry-create
     :key 'status :role 'note
     :label qq-guilds--error :indent 2 :face 'error))))

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
         (guild-key (qq-guilds--guild-entry-key guild-id))
         (guild-name (qq-guilds--display-name (alist-get 'name guild) guild-id))
         (guild-match-p (qq-guilds--matches-p guild-name))
         (expanded-p
          (appkit-directory-fold-expanded-p
           (appkit-directory-surface) guild-key t qq-guilds--filter))
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
       (appkit-directory-entry-create
        :key guild-key :role 'section :label guild-name
        :face 'qq-guilds-title
        :foldable-p t :fold-key guild-key
        :fold-default-expanded-p t :expanded-p expanded-p
        :fold-locked-reason
        (and qq-guilds--filter
             "qq: clear the directory filter before folding")
        :help-echo (if expanded-p "折叠 QQ 频道" "展开 QQ 频道")
        :properties (list 'qq-guild-id guild-id))
       entries)
      (when expanded-p
        (if visible-categories
            (dolist (pair visible-categories)
              (let ((category (car pair))
                    (channels (cdr pair)))
                (let* ((uncategorized-p
                        (eq (alist-get 'uncategorized category) t))
                       (category-key
                        (qq-guilds--category-entry-key
                         guild-id (alist-get 'category_id category)))
                       (category-expanded-p
                        (or uncategorized-p
                            (appkit-directory-fold-expanded-p
                             (appkit-directory-surface)
                             category-key t qq-guilds--filter))))
                  (unless uncategorized-p
                    (push
                     (appkit-directory-entry-create
                      :key category-key :role 'group
                      :section-key guild-key
                      :label (qq-guilds--display-name
                              (alist-get 'name category)
                              (alist-get 'category_id category))
                      :indent 4 :face 'shadow
                      :foldable-p t :fold-key category-key
                      :fold-default-expanded-p t
                      :expanded-p category-expanded-p
                      :fold-locked-reason
                      (and qq-guilds--filter
                           "qq: clear the directory filter before folding")
                      :help-echo
                      (if category-expanded-p "折叠频道分类" "展开频道分类"))
                     entries))
                  (when category-expanded-p
                    (dolist (channel channels)
                      (let ((channel-id (alist-get 'channel_id channel))
                            (kind (alist-get 'kind channel)))
                        (push
                         (appkit-directory-entry-create
                          :key (qq-guilds--channel-entry-key
                                guild-id channel-id)
                          :role 'item :section-key guild-key
                          :group-key category-key
                          :indent (if uncategorized-p 4 6)
                          :item-p t
                          :unread-p
                          (> (or (alist-get
                                  'unread-count
                                  (qq-state-session
                                   (qq-state-guild-channel-session-key
                                    guild-id channel-id)))
                                 0)
                             0)
                          :payload channel
                          :face 'qq-guilds-channel
                          :help-echo
                          (format "打开%s频道 %s · %s"
                                  (qq-guild-channel-type-label kind)
                                  (alist-get 'guild_name channel)
                                  (alist-get 'name channel))
                          :properties
                          (list 'qq-guild-id guild-id
                                'qq-guild-channel-id channel-id))
                         entries)))))))
          (push
           (appkit-directory-entry-create
            :key (list 'guild-empty guild-id) :role 'note
            :section-key guild-key
            :label "暂无匹配的子频道" :indent 4 :face 'shadow)
           entries)))
      (push
       (appkit-directory-entry-create
        :key (list 'guild-gap guild-id) :role 'spacer)
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
                    (appkit-directory-entry-create
                     :key 'empty :role 'note
                     :label "没有可见的 QQ 频道。" :indent 2
                     :face 'shadow)))))
    (when (and qq-guilds--filter guilds
               (not (seq-some
                     (lambda (entry)
                       (eq (appkit-directory-entry-role entry) 'section))
                     entries)))
      (setq entries
            (nconc entries
                   (list
                    (appkit-directory-entry-create
                     :key 'filter-empty :role 'note
                     :label "没有匹配的频道或子频道。" :indent 2
                     :face 'shadow)))))
    entries))

(defun qq-guilds--reconcile-directory ()
  "Reconcile the Appkit directory with the current QQ projection."
  (appkit-directory-reconcile
   (appkit-directory-surface) (qq-guilds--project-entries)))

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
  (appkit-directory-surface)
  (let* ((parts (appkit-invalidations-parts invalidations))
         (position-p (appkit-invalidations-position-p invalidations))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              (memq 'directory parts))))
    (when (and (appkit-view-live-p view)
               (or reconcile-p position-p))
      (appkit-with-content-update view
        (when reconcile-p
          (qq-guilds--reconcile-directory))))
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
  (let* ((key (if guild-id
                  (qq-guilds--guild-entry-key guild-id)
                (appkit-directory-key-at-point)))
         (surface (appkit-directory-surface))
         (entry (and key
                     (appkit-directory-entry-for-key surface key))))
    (unless (and entry
                 (eq (appkit-directory-entry-role entry) 'section))
      (user-error "qq: point is not on a QQ 频道 heading"))
    (appkit-directory-activate-entry surface entry)))

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
    (set-keymap-parent map appkit-directory-mode-map)
    (define-key map (kbd "g") #'qq-guilds-refresh)
    (define-key map (kbd "/") #'qq-guilds-filter)
    (define-key map (kbd "C-c C-k") #'qq-guilds-clear-filter)
    map))

(define-derived-mode qq-guilds-mode appkit-directory-mode "QQ-Guilds"
  "Major mode for the hierarchical QQ Guild directory."
  (setq-local qq-guilds--loading nil)
  (setq-local qq-guilds--error nil)
  (setq-local qq-guilds--refresh-owner nil)
  (setq-local qq-guilds--refresh-request nil)
  (setq-local qq-guilds--filter nil)
  (setq-local header-line-format '(:eval (qq-guilds--header-line)))
  (appkit-directory-configure
   (appkit-directory-surface)
   :item-inserter #'qq-guilds--insert-channel
   :activate-function #'qq-guilds--activate-channel
   :fold-function #'qq-guilds--fold-changed)
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
