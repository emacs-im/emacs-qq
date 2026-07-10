;;; qq-root.el --- Root buffer for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Root buffer modeled after disco.el's root view, with a unified one-line list
;; style shared with the chat buffer helpers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'disco-view)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-state)
(require 'qq-view)

(defconst qq-root-buffer-name "*qq-root*"
  "Name of the emacs-qq root buffer.")

(defconst qq-root--activity-icon-slot-width 4
  "Reserved icon slot width for one-line session rows.")

(defvar-local qq-root--rerender-timer nil
  "Idle timer used to debounce root rerenders.")

(defun qq-root--buffer-width ()
  "Return current root rendering width in columns."
  (max 60 (window-body-width (get-buffer-window (current-buffer) t))))

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

(defun qq-root--activity-metrics ()
  "Return simple root activity metrics plist."
  (let* ((sessions (qq-state-sessions))
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

(defun qq-root--filters-line ()
  "Return filter-chip line inspired by disco root view."
  (let ((metrics (qq-root--activity-metrics)))
    (string-join
     (list (qq-root--filter-chip "Main" (or (plist-get metrics :all) 0) t)
           (qq-root--filter-chip "Important" (or (plist-get metrics :important) 0))
           (qq-root--filter-chip "Muted" (or (plist-get metrics :muted) 0))
           (qq-root--filter-chip "DMs" (or (plist-get metrics :dms) 0))
           "[activity sort:recent]")
     "  ")))

(defun qq-root--mode-divider-line ()
  "Return divider line with active mode marker like disco root."
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

(defun qq-root--session-important-unread-p (session)
  "Return non-nil when SESSION has unread messages that are not muted."
  (and (> (or (alist-get 'unread-count session) 0) 0)
       (not (qq-root--session-muted-p session))))

(defun qq-root--session-badge (session)
  "Return root activity badge for SESSION, including trailing space."
  (let ((unread (or (alist-get 'unread-count session) 0)))
    (cond
     ((and (qq-root--session-muted-p session) (> unread 0))
      (format "[mute:%d] " unread))
     ((qq-root--session-muted-p session) "[mute] ")
     ((> unread 0) (format "[%d] " unread))
     (t ""))))

(defun qq-root--session-icon-face (session)
  "Return fallback icon face for SESSION."
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
  "Return one-line preview for SESSION."
  (let* ((session-key (alist-get 'key session))
         (input-text (and session-key (qq-state-input-status-text session-key)))
         (preview (string-trim (or (alist-get 'last-message-preview session) "")))
         (badge (qq-root--session-badge session)))
    (concat
     badge
     (cond
      ((and (stringp input-text) (not (string-empty-p input-text)))
       input-text)
      ((string-empty-p preview)
       "(no preview yet)")
      (t preview)))))

(defun qq-root--session-one-line-row (session)
  "Return one-line row model for SESSION."
  (let* ((session-key (alist-get 'key session))
         (unread (or (alist-get 'unread-count session) 0))
         (muted (qq-root--session-muted-p session))
         (important (qq-root--session-important-unread-p session))
         (badge (qq-root--session-badge session)))
    (disco-view-one-line-row-create
     :icon-inserter (lambda ()
                      (qq-root--insert-session-icon session))
     :context (qq-root--session-context-label session)
     :preview (qq-root--session-preview-text session)
     :preview-leading-length (length badge)
     :preview-leading-face (cond (important 'warning)
                                 (muted 'shadow))
     :time (qq-root--format-time (alist-get 'last-message-time session))
     :time-face 'shadow
     :time-tail-face (and important 'warning)
     :line-properties
     (list 'qq-root-row-type 'session
           'qq-root-session-key session-key
           'qq-root-unread-count unread
           'qq-root-has-unread (and (> unread 0) t)
           'qq-root-muted-p muted
           'qq-root-has-important-unread (and important t)
           'mouse-face 'highlight)
     :help-echo (format "Open %s%s"
                        session-key
                        (if muted " (message notifications muted)" "")))))

(defun qq-root--insert-session-line (session)
  "Insert one session row for SESSION."
  (disco-view-insert-one-line-row
   (qq-root--session-one-line-row session)
   :indent 2
   :width (qq-root--buffer-width)
   :icon-slot-width qq-root--activity-icon-slot-width
   :context-width-spec '(0.32 16 30)))

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
  "Refresh root state and redraw the buffer."
  (interactive)
  (qq-api-refresh)
  (qq-root-render))

(defun qq-root-render ()
  "Render the root buffer from local state."
  (interactive)
  (disco-view-render-preserving-position
   (lambda ()
     (let ((inhibit-read-only t)
           (sessions (qq-state-sessions)))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-root--header-line)))
       (qq-view-insert-note-line (qq-root--filters-line) :face 'font-lock-doc-face)
       (qq-view-insert-note-line (qq-root--mode-divider-line) :face 'shadow)
       (qq-view-insert-note-line
        "g refresh  RET open  a avatar  TAB/n/p move  u next unread  s// search  ?: menu  q quit")
       (insert "\n")
       (if sessions
           (dolist (session sessions)
             (qq-root--insert-session-line session))
         (qq-view-insert-note-line "No sessions available yet.")
         (qq-view-insert-note-line "Press `g` to refresh after transport connects."))
       (goto-char (point-min))
       (forward-line 2)
       (unless (qq-root--session-key-at-point)
         (qq-root-button-forward))))
   :anchor-property 'qq-root-session-key
   :preserve-window-start t))

(defvar qq-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-root-refresh)
    (define-key map (kbd "s") #'qq-root-search)
    (define-key map (kbd "/") #'qq-root-search)
    (define-key map (kbd "RET") #'qq-root-open-at-point)
    (define-key map (kbd "a") #'qq-root-open-avatar-at-point)
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
  (setq-local switch-to-buffer-preserve-window-point nil))

(defun qq-root-open ()
  "Open the emacs-qq root buffer."
  (interactive)
  (let ((buffer (get-buffer-create qq-root-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-root-mode)
        (qq-root-mode))
      (qq-root-render))
    (pop-to-buffer buffer)))

(defun qq-root--cancel-rerender-timer ()
  "Cancel any pending debounced rerender for current root buffer."
  (when (timerp qq-root--rerender-timer)
    (cancel-timer qq-root--rerender-timer)
    (setq qq-root--rerender-timer nil)))

(defun qq-root--rerender-buffer-if-live (buffer)
  "Rerender BUFFER when it is a live root buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq qq-root--rerender-timer nil)
      (when (derived-mode-p 'qq-root-mode)
        (qq-root-render)))))

(defun qq-root--rerender-open-root (&optional _media-key immediate)
  "Rerender the live root buffer after UI-affecting changes.

When IMMEDIATE is non-nil, rerender synchronously.  Otherwise debounce the
refresh until Emacs becomes idle, which avoids point-motion stutter while many
avatar/media cache updates arrive in a burst.  Optional _MEDIA-KEY is ignored
when this function is used as `qq-media-cache-update-hook'."
  (let ((buffer (get-buffer qq-root-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (qq-root--cancel-rerender-timer)
        (if immediate
            (qq-root--rerender-buffer-if-live buffer)
          (setq qq-root--rerender-timer
                (run-with-idle-timer 0.05 nil
                                     #'qq-root--rerender-buffer-if-live
                                     buffer)))))))

(defun qq-root--handle-state-change (event)
  "Refresh visible root buffer after relevant state EVENT changes."
  (when (memq (plist-get event :type)
              '(reset connection self-info session message history input-status
                      sessions-refreshed friends-refreshed groups-refreshed))
    (qq-root--rerender-open-root)))

(add-hook 'qq-media-cache-update-hook #'qq-root--rerender-open-root)
(add-hook 'qq-state-change-hook #'qq-root--handle-state-change)

(provide 'qq-root)

;;; qq-root.el ends here
