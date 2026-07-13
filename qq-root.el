;;; qq-root.el --- Root buffer for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Persistent keyed-EWOC root view.  Session rows use appkit's shared list
;; reconciliation and one-line presentation infrastructure.

;;; Code:

(require 'cl-lib)
(require 'ewoc)
(require 'subr-x)
(require 'appkit-view)
(require 'appkit-position)
(require 'appkit-ewoc)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-state)

(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-group-open "qq-group" (group-id))

(defconst qq-root-buffer-name "*qq-root*"
  "Name of the emacs-qq root buffer.")

(defconst qq-root--activity-icon-slot-width 4
  "Reserved icon slot width for one-line session rows.")

(defvar-local qq-root--ewoc nil
  "Persistent EWOC containing root metadata and session rows.")

(defvar-local qq-root--node-table nil
  "Stable root entry key to EWOC node table.")

(defvar-local qq-root--fill-column nil
  "Last root width measured from a window that actually displayed it.")

(cl-defstruct (qq-root--entry
               (:constructor qq-root--entry-create))
  key
  type
  text
  face
  session
  width)

(defun qq-root--selected-window ()
  "Return the selected window when it displays the current root buffer."
  (let ((win (selected-window)))
    (and (window-live-p win)
         (eq (window-buffer win) (current-buffer))
         win)))

(defun qq-root--display-window ()
  "Return the widest live window displaying the current root buffer."
  (let ((best nil)
        (best-width -1))
    (dolist (win (get-buffer-window-list (current-buffer) nil t) best)
      (let ((width (if (window-live-p win)
                       (window-width win 'remap)
                     -1)))
        (when (> width best-width)
          (setq best win
                best-width width))))))

(defun qq-root--compute-fill-column (&optional window)
  "Compute root row width from live WINDOW, or return nil."
  (when-let* ((win (or window (qq-root--display-window)))
              (width (appkit-view-window-fill-column
                      win qq-root-auto-fill-margin-columns)))
    (max 60 width)))

(defun qq-root--stable-fill-column ()
  "Return a stable width for the next root reconciliation.

Measure the selected root window when possible.  A passive background update
reuses the last measured width instead
of accidentally borrowing the selected chat window."
  (or (when-let* ((win (qq-root--selected-window)))
        (qq-root--compute-fill-column win))
      (and (integerp qq-root--fill-column)
           (> qq-root--fill-column 0)
           qq-root--fill-column)
      (qq-root--compute-fill-column (qq-root--display-window))
      80))

(defun qq-root--buffer-width ()
  "Return current root row width in columns."
  (max 60 (or qq-root--fill-column
              (setq-local qq-root--fill-column
                          (qq-root--stable-fill-column)))))

(defun qq-root--format-time (timestamp)
  "Return display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%m-%d %H:%M" (seconds-to-time timestamp))
    ""))

(defun qq-root--header-line ()
  "Return dynamic header line for the root buffer."
  (let* ((self-info (qq-state-self-info))
         (status (qq-state-connection-status))
         (nickname (alist-get 'nickname self-info))
         (user-id (alist-get 'user_id self-info)))
    (format " emacs-qq  [%s]  %s%s"
            status
            (or nickname "not logged in")
            (if user-id
                (format " (%s)" user-id)
              ""))))

(defun qq-root--activity-metrics (&optional sessions)
  "Return root activity metrics for SESSIONS or current state."
  (let* ((sessions (or sessions (qq-state-sessions)))
         (unread (cl-count-if (lambda (session)
                                (> (or (alist-get 'unread-count session) 0) 0))
                              sessions))
         (important (cl-count-if #'qq-root--session-important-unread-p sessions))
         (muted (cl-count-if (lambda (session)
                               (and (qq-root--session-muted-p session)
                                    (> (or (alist-get 'unread-count session) 0) 0)))
                             sessions))
         (dms (cl-count-if (lambda (session)
                             (not (eq (alist-get 'type session) 'group)))
                           sessions)))
    (list :all (length sessions)
          :unread unread
          :important important
          :muted muted
          :dms dms)))

(defun qq-root--filter-chip (label count &optional active)
  "Return one root filter chip for LABEL and COUNT."
  (format "[%s%s:%d]"
          (if active "*" "")
          label
          count))

(defun qq-root--filters-line (&optional sessions)
  "Return filter-chip line for SESSIONS or current state."
  (let ((metrics (qq-root--activity-metrics sessions)))
    (string-join
     (list (qq-root--filter-chip "Main" (or (plist-get metrics :all) 0) t)
           (qq-root--filter-chip "Important" (or (plist-get metrics :important) 0))
           (qq-root--filter-chip "Muted" (or (plist-get metrics :muted) 0))
           (qq-root--filter-chip "DMs" (or (plist-get metrics :dms) 0))
           "[activity sort:recent]")
     "  ")))

(defun qq-root--mode-divider-line ()
  "Return a divider line carrying the active root mode marker."
  (let* ((label "(activity/all)")
         (width (max (qq-root--buffer-width) (+ 8 (string-width label))))
         (filler (max 0 (- width (string-width label) 2)))
         (left (/ filler 2))
         (right (- filler left)))
    (concat "_/"
            (make-string left ?-)
            label
            (make-string right ?-))))

(defun qq-root--session-context-label (session)
  "Return one-line context label for SESSION."
  (or (alist-get 'title session)
      (alist-get 'key session)
      "session"))

(defun qq-root--session-muted-p (session)
  "Return non-nil when SESSION has QQ message notifications muted."
  (eq (alist-get 'muted-p session) t))

(defun qq-root--session-mention-kinds (session)
  "Return unread native mention kinds represented by SESSION."
  (delq nil
        (list (and (or (alist-get 'unread-at-me-message-id session)
                       (alist-get 'unread-at-me-message-seq session))
                   'at-me)
              (and (or (alist-get 'unread-at-all-message-id session)
                       (alist-get 'unread-at-all-message-seq session))
                   'at-all))))

(defun qq-root--session-important-unread-p (session)
  "Return non-nil when SESSION has unmuted unread or an unread mention."
  (and (> (or (alist-get 'unread-count session) 0) 0)
       (or (not (qq-root--session-muted-p session))
           (qq-root--session-mention-kinds session))))

(defun qq-root--session-unread-trail (session)
  "Return SESSION's propertized unread trail for the title brackets.

Like telega's chat unread trail, ordinary unread count follows the title and
uses a muted or unmuted face.  Native mention kinds remain independently
prominent even when the session is muted."
  (let* ((unread (or (alist-get 'unread-count session) 0))
         (mentions (qq-root--session-mention-kinds session))
         (count-face (if (qq-root--session-muted-p session)
                         'qq-root-muted-count
                       'qq-root-unmuted-count)))
    (string-join
     (delq nil
           (list (and (> unread 0)
                      (propertize (number-to-string unread)
                                  'face count-face))
                 (and (memq 'at-me mentions)
                      (propertize "@" 'face 'qq-root-mention-count))
                 (and (memq 'at-all mentions)
                      (propertize "@all" 'face 'qq-root-mention-count))))
     " ")))

(defun qq-root--session-icon-face (session)
  "Return the icon face for SESSION."
  (cond
   ((qq-root--session-muted-p session) 'shadow)
   ((eq (alist-get 'type session) 'group) 'font-lock-keyword-face)
   (t 'font-lock-variable-name-face)))

(defun qq-root--insert-session-icon (session)
  "Insert inline avatar/icon for SESSION."
  (let ((start (point)))
    (insert (qq-media-session-avatar-display-string session))
    (add-text-properties
     start
     (point)
     (list 'face (qq-root--session-icon-face session)
           'help-echo (format "Open avatar for %s"
                              (or (alist-get 'title session)
                                  (alist-get 'key session)
                                  "session"))))))

(defun qq-root--session-preview-text (session)
  "Return one-line preview for SESSION.

Mirrors telega `telega-ins--chat-status': peer chat-actions (typing) take
priority over the last-message preview."
  (let* ((session-key (alist-get 'key session))
         (action-text (and qq-chat-show-peer-actions
                           session-key
                           (qq-state-preview-one-line
                            (qq-state-action-text session-key))))
         (preview (qq-state-preview-one-line
                   (alist-get 'last-message-preview session))))
    (if (and (stringp action-text) (not (string-empty-p action-text)))
        (concat (or qq-chat-action-prefix ".. ") action-text)
      preview)))

(defun qq-root--session-one-line-row (session)
  "Return one-line row model for SESSION."
  (let* ((session-key (alist-get 'key session))
         (unread (or (alist-get 'unread-count session) 0))
         (muted (qq-root--session-muted-p session))
         (important (qq-root--session-important-unread-p session)))
    (appkit-view-one-line-row-create
     :icon-inserter (lambda ()
                      (qq-root--insert-session-icon session))
     :context (qq-root--session-context-label session)
     :context-trail (qq-root--session-unread-trail session)
     :preview (qq-root--session-preview-text session)
     ;; Unread activity lives in the title trail.  Keep a dedicated time-tail
     ;; face (meant for a trailing status glyph) off the plain timestamp.
     :time (qq-root--format-time (alist-get 'last-message-time session))
     :time-face 'shadow
     :time-tail-face nil
     :line-properties
     (list 'qq-root-row-type 'session
           'qq-root-session-key session-key
           'qq-root-unread-count unread
           'qq-root-has-unread (and (> unread 0) t)
           'qq-root-muted-p muted
           'qq-root-has-important-unread (and important t))
     :help-echo (format "Open %s%s"
                        session-key
                        (if muted " (message notifications muted)" "")))))

(defun qq-root--insert-session-line (session)
  "Insert one session row for SESSION."
  (appkit-view-insert-one-line-row
   (qq-root--session-one-line-row session)
   :indent 2
   :width (qq-root--buffer-width)
   :icon-slot-width qq-root--activity-icon-slot-width
   :context-width-spec '(0.32 16 30)))

(defun qq-root--session-entry-key (session-key)
  "Return the stable root entry key for SESSION-KEY."
  (cons 'session session-key))

(defun qq-root--entry-printer (entry)
  "Insert one persistent root ENTRY."
  (pcase (qq-root--entry-type entry)
    ('note
     (appkit-view-insert-note-line (qq-root--entry-text entry)
                                  :face (qq-root--entry-face entry)))
    ('blank (insert "\n"))
    ('session (qq-root--insert-session-line (qq-root--entry-session entry)))
    (type (error "qq: unknown root entry type %S" type))))

(defun qq-root--project-entries ()
  "Project current state into stable-keyed root entries."
  (let* ((sessions (qq-state-sessions))
         (width (qq-root--buffer-width))
         (metadata
          (list
           (qq-root--entry-create
            :key 'filters :type 'note
            :text (qq-root--filters-line sessions)
            :face 'font-lock-doc-face)
           (qq-root--entry-create
            :key 'divider :type 'note
            :text (qq-root--mode-divider-line) :face 'shadow)
           (qq-root--entry-create :key 'metadata-gap :type 'blank))))
    (append
     metadata
     (mapcar
      (lambda (session)
        (qq-root--entry-create
         :key (qq-root--session-entry-key (alist-get 'key session))
         :type 'session
         :session session
         :width width))
      sessions)
     (unless sessions
       (list
        (qq-root--entry-create
         :key 'empty :type 'note :text "No sessions available yet."))))))

(defun qq-root--session-entry-keys ()
  "Return stable entry keys for all current sessions."
  (mapcar (lambda (session)
            (qq-root--session-entry-key (alist-get 'key session)))
          (qq-state-sessions)))

(defun qq-root--sync (&optional force-keys)
  "Reconcile the persistent root view, invalidating FORCE-KEYS.

Rows whose data and position are unchanged retain their EWOC nodes."
  (unless (ewoc-p qq-root--ewoc)
    (error "qq: root view is not initialized"))
  (let ((snapshot
         (appkit-position-capture
          :anchor-property 'qq-root-session-key
          :preserve-window-start t))
        (inhibit-read-only t)
        (buffer-undo-list t))
    (setq-local qq-root--fill-column (qq-root--stable-fill-column))
    (with-silent-modifications
      (setq qq-root--node-table
            (appkit-ewoc-reconcile
             qq-root--ewoc
             (qq-root--project-entries)
             #'qq-root--entry-key
             :force-keys force-keys)))
    (when snapshot
      (appkit-position-restore snapshot))
    (force-mode-line-update)
    (force-window-update (current-buffer))))

(defun qq-root--invalidate (keys)
  "Invalidate persistent root rows identified by KEYS."
  (let ((snapshot
         (appkit-position-capture
          :anchor-property 'qq-root-session-key
          :preserve-window-start t))
        (inhibit-read-only t)
        (buffer-undo-list t))
    (with-silent-modifications
      (dolist (key keys)
        (appkit-ewoc-invalidate-key
         qq-root--ewoc qq-root--node-table key)))
    (when snapshot
      (appkit-position-restore snapshot))
    (force-window-update (current-buffer))))

(defun qq-root--session-key-at-point (&optional pos)
  "Return root session key at POS, or current point when POS is nil."
  (let ((probe (or pos (point))))
    (or (get-text-property probe 'qq-root-session-key)
        (get-text-property (line-beginning-position) 'qq-root-session-key))))

(defun qq-root--session-at-point ()
  "Return root session object at point, or nil."
  (when-let* ((session-key (qq-root--session-key-at-point)))
    (qq-state-session session-key)))

(defun qq-root-open-at-point ()
  "Open the session at point."
  (interactive)
  (let ((session-key (qq-root--session-key-at-point)))
    (unless session-key
      (user-error "qq: no session at point"))
    (qq-chat-open session-key)))

(defun qq-root-open-avatar-at-point ()
  "Open avatar/icon for the session at point."
  (interactive)
  (qq-media-open-session-avatar
   (or (qq-root--session-at-point)
       (user-error "qq: no session at point"))))

(defun qq-root-open-user-at-point ()
  "Open the private session user's profile at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (and (eq (alist-get 'type session) 'private)
                 (qq-api-user-id-p user-id))
      (user-error "qq: session has no user profile"))
    (qq-user-open user-id)))

(defun qq-root-open-info-at-point ()
  "Open the user or group profile for the session at point."
  (interactive)
  (let* ((session (or (qq-root--session-at-point)
                      (user-error "qq: no session at point")))
         (type (alist-get 'type session))
         (target-id (or (and (eq type 'private) (alist-get 'peer-uin session))
                        (alist-get 'target-id session))))
    (pcase type
      ('private
       (unless (qq-api-user-id-p target-id)
         (user-error "qq: session has no user profile"))
       (qq-user-open target-id))
      ('group
       (unless (qq-api-group-id-p target-id)
         (user-error "qq: session has no group profile"))
       (qq-group-open target-id))
      (_ (user-error "qq: session has no profile page")))))

(defun qq-root-open-self-user ()
  "Open the logged-in user's profile."
  (interactive)
  (let ((user-id (alist-get 'user_id (qq-state-self-info))))
    (unless (qq-api-user-id-p user-id)
      (user-error "qq: self user profile is unavailable"))
    (qq-user-open user-id)))

(defun qq-root-mouse-open-at-point (event)
  "Open the session clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (qq-root-open-at-point))

(defun qq-root--move-linewise (direction predicate &optional wrap)
  "Move point linewise in DIRECTION until PREDICATE succeeds.

DIRECTION should be 1 or -1.  PREDICATE is called with no arguments at each
candidate line.  When WRAP is non-nil, wrap to buffer edge once."
  (let ((origin (point))
        (wrapped nil)
        (found nil))
    (while (not found)
      (forward-line direction)
      (cond
       ((and (> direction 0) (eobp))
        (if (and wrap (not wrapped))
            (progn
              (setq wrapped t)
              (goto-char (point-min)))
          (setq found 'stop)))
       ((and (< direction 0) (bobp))
        (if (and wrap (not wrapped))
            (progn
              (setq wrapped t)
              (goto-char (point-max))
              (forward-line -1))
          (setq found 'stop)))
       ((funcall predicate)
        (setq found t))))
    (unless (eq found t)
      (goto-char origin)
      nil)))

(defun qq-root-button-forward ()
  "Move point to the next session row."
  (interactive)
  (qq-root--move-linewise
   1
   (lambda ()
     (qq-root--session-key-at-point))
   t))

(defun qq-root-button-backward ()
  "Move point to the previous session row."
  (interactive)
  (qq-root--move-linewise
   -1
   (lambda ()
     (qq-root--session-key-at-point))
   t))

(defun qq-root-tab-dwim ()
  "Move to the next session row."
  (interactive)
  (qq-root-button-forward))

(defun qq-root-next-unread ()
  "Move point to the next unread session row."
  (interactive)
  (unless (qq-root--move-linewise
           1
           (lambda ()
             (> (or (get-text-property (point) 'qq-root-unread-count) 0) 0))
           t)
    (message "qq: no unread sessions")))

(defun qq-root-search ()
  "Prompt for a session and open it."
  (interactive)
  (let* ((sessions (qq-state-sessions))
         (choices
          (mapcar (lambda (session)
                    (cons (format "%s  [%s]"
                                  (or (alist-get 'title session)
                                      (alist-get 'key session))
                                  (alist-get 'key session))
                          (alist-get 'key session)))
                  sessions)))
    (unless choices
      (user-error "qq: no sessions available"))
    (qq-chat-open
     (cdr (assoc (completing-read "Session: " choices nil t) choices)))))

(defun qq-root-refresh ()
  "Request a fresh root snapshot from NapCat."
  (interactive)
  (qq-api-refresh))

(defvar qq-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-root-refresh)
    (define-key map (kbd "s") #'qq-root-search)
    (define-key map (kbd "/") #'qq-root-search)
    (define-key map (kbd "RET") #'qq-root-open-at-point)
    (define-key map (kbd "a") #'qq-root-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-root-open-info-at-point)
    (define-key map (kbd "I") #'qq-root-open-self-user)
    (define-key map [mouse-1] #'qq-root-mouse-open-at-point)
    (define-key map (kbd "n") #'qq-root-button-forward)
    (define-key map (kbd "p") #'qq-root-button-backward)
    (define-key map (kbd "TAB") #'qq-root-tab-dwim)
    (define-key map (kbd "<backtab>") #'qq-root-button-backward)
    (define-key map (kbd "u") #'qq-root-next-unread)
    (define-key map (kbd "?") #'qq-root-transient)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-root-mode'.")

(define-derived-mode qq-root-mode special-mode "QQ-Root"
  "Major mode for the emacs-qq root buffer.

`?' opens `qq-root-transient' (discoverable command menu)."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local qq-root--fill-column nil)
  (setq-local qq-root--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (qq-root--header-line)))
  (let ((inhibit-read-only t)
        (buffer-undo-list t))
    (erase-buffer)
    (setq-local qq-root--ewoc
                (ewoc-create #'qq-root--entry-printer nil nil t)))
  (add-hook 'window-size-change-functions
            #'qq-root--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook
            #'qq-root--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'qq-root--on-text-scale-change nil t))

(defun qq-root-open ()
  "Open the emacs-qq root buffer."
  (interactive)
  (let ((buffer (get-buffer-create qq-root-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-root-mode)
        (qq-root-mode)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (qq-root--reflow-visible t)
      (unless (qq-root--session-key-at-point)
        (goto-char (point-min))
        (qq-root-button-forward)))))

(defun qq-root--reflow-visible (&optional force)
  "Reflow root from its real display window when width changed.

When FORCE is non-nil, invalidate rows even when the width is unchanged so
pixel-valued alignment follows text scaling."
  (when (derived-mode-p 'qq-root-mode)
    (when-let* ((win (or (qq-root--selected-window)
                         (qq-root--display-window)))
                (next (qq-root--compute-fill-column win)))
      (when (or force (not (equal next qq-root--fill-column)))
        (setq-local qq-root--fill-column next)
        (qq-root--sync (and force (qq-root--session-entry-keys)))
        t))))

(defun qq-root--on-window-size-change (&optional _frame)
  "Reflow a visible root buffer after its window geometry changes."
  (qq-root--reflow-visible nil))

(defun qq-root--on-text-scale-change ()
  "Reflow a visible root buffer after text scaling changes."
  (qq-root--reflow-visible t))

(defun qq-root--sync-open-root ()
  "Synchronize the live root buffer."
  (let ((buffer (get-buffer qq-root-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'qq-root-mode)
          (qq-root--sync))))))

(defun qq-root--invalidate-open-root (keys)
  "Invalidate KEYS in the live root buffer."
  (let ((buffer (get-buffer qq-root-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'qq-root-mode)
          (qq-root--invalidate keys))))))

(defun qq-root--session-avatar-media-key (session)
  "Return the exact avatar cache key used by SESSION, or nil."
  (let ((target-id (alist-get 'target-id session)))
    (when target-id
      (pcase (alist-get 'type session)
        ('dataline nil)
        ('group (format "group-avatar:%s" target-id))
        (_ (format "avatar:%s" target-id))))))

(defun qq-root--handle-media-cache-update (media-key)
  "Invalidate root rows whose avatar is identified by MEDIA-KEY."
  (when (stringp media-key)
    (let (keys)
      (dolist (session (qq-state-sessions))
        (when (equal media-key (qq-root--session-avatar-media-key session))
          (push (qq-root--session-entry-key (alist-get 'key session)) keys)))
      (when keys
        (qq-root--invalidate-open-root keys)))))

(defun qq-root--refresh-header ()
  "Refresh the root header without touching persistent entries."
  (when-let* ((buffer (get-buffer qq-root-buffer-name)))
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-root-mode)
        (force-mode-line-update)
        (force-window-update buffer)))))

(defun qq-root--handle-state-change (event)
  "Apply state EVENT to the persistent root view."
  (let ((type (plist-get event :type))
        (session-key (plist-get event :session-key)))
    (pcase type
      ((or 'connection 'self-info)
       (qq-root--refresh-header))
      ('action
       (when session-key
         (qq-root--invalidate-open-root
          (list (qq-root--session-entry-key session-key)))))
      ((or 'reset 'session 'message 'history 'sessions-refreshed
           'friends-refreshed 'groups-refreshed)
       (qq-root--sync-open-root)))))

(add-hook 'qq-media-cache-update-hook #'qq-root--handle-media-cache-update)
(add-hook 'qq-state-change-hook #'qq-root--handle-state-change)

(provide 'qq-root)

;;; qq-root.el ends here
