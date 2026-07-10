;;; qq-notifications.el --- Desktop notifications for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style delayed desktop notifications with QQ-native mention rules.

;;; Code:

(require 'cl-lib)
(require 'notifications)
(require 'ring)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-state)
(require 'qq-chat)

(defvar qq-notifications-mode nil)

(defvar qq-notifications--last-id nil
  "Currently displayed desktop notification id.")

(defvar qq-notifications--seen-anchors (make-hash-table :test #'equal)
  "Message anchors already scheduled by the notification event hook.")

(defvar qq-notifications--seen-anchor-order nil
  "Newest-first bounded order for `qq-notifications--seen-anchors'.")

(defconst qq-notifications--seen-anchor-limit 512
  "Maximum number of notification deduplication anchors retained.")

(defvar qq-notifications--history nil
  "Ring of normalized messages shown as desktop notifications.")

(defun qq-notifications--history-ring ()
  "Return the notification history ring at its configured size."
  (unless (and (ring-p qq-notifications--history)
               (= (ring-size qq-notifications--history)
                  qq-notifications-history-ring-size))
    (let ((old (and (ring-p qq-notifications--history)
                    (ring-elements qq-notifications--history))))
      (setq qq-notifications--history
            (make-ring (max 1 qq-notifications-history-ring-size)))
      (dolist (entry (reverse (seq-take old qq-notifications-history-ring-size)))
        (ring-insert qq-notifications--history entry))))
  qq-notifications--history)

(defun qq-notifications--remember-anchor (anchor)
  "Remember notification ANCHOR while keeping deduplication state bounded."
  (puthash anchor t qq-notifications--seen-anchors)
  (push anchor qq-notifications--seen-anchor-order)
  (when (> (length qq-notifications--seen-anchor-order)
           qq-notifications--seen-anchor-limit)
    (let ((expired (car (last qq-notifications--seen-anchor-order))))
      (setq qq-notifications--seen-anchor-order
            (butlast qq-notifications--seen-anchor-order))
      (remhash expired qq-notifications--seen-anchors))))

(defun qq-notifications--session-muted-p (session)
  "Return non-nil when SESSION is in QQ's quiet/muted mode."
  (eq (alist-get 'muted-p session) t))

(defun qq-notifications--mention-breaks-mute-p (message)
  "Return non-nil when MESSAGE has a high-priority native QQ mention."
  (or (qq-state-message-mentions-self-p message)
      (and qq-notifications-at-all-breaks-mute
           (qq-state-message-mentions-all-p message))))

(defun qq-notifications--chat-observable-p (session-key)
  "Return non-nil when SESSION-KEY is selected on a focused frame."
  (seq-some
   (lambda (frame)
     (and (frame-live-p frame)
          (eq (frame-focus-state frame) t)
          (let* ((window (frame-selected-window frame))
                 (buffer (and (window-live-p window) (window-buffer window))))
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'qq-chat-mode)
                        (equal qq-chat--session-key session-key)))))))
   (frame-list)))

(defun qq-notifications-message-notify-p (message)
  "Return non-nil when normalized MESSAGE should produce a notification."
  (let* ((session-key (alist-get 'session-key message))
         (session (and session-key (qq-state-session session-key)))
         (time (or (alist-get 'time message) 0)))
    (and session-key
         session
         (not (alist-get 'self-p message))
         (not (qq-state-message-recalled-p message))
         (or (not (numberp time))
             (<= (- (float-time) time) qq-notifications-max-message-age))
         (or (not (qq-notifications--session-muted-p session))
             (qq-notifications--mention-breaks-mute-p message))
         (not (qq-notifications--chat-observable-p session-key)))))

(defun qq-notifications--title (message session)
  "Return desktop notification title for MESSAGE in SESSION."
  (let ((sender (or (alist-get 'sender-name message)
                    (alist-get 'sender-id message)
                    "QQ"))
        (chat-title (or (alist-get 'title session)
                        (alist-get 'key session)
                        "QQ")))
    (if (eq (alist-get 'type session) 'group)
        (format "%s — %s" sender chat-title)
      sender)))

(defun qq-notifications--body (message)
  "Return compact desktop notification body for MESSAGE."
  (if (not qq-notifications-show-preview)
      "有新的未读消息"
    (let* ((preview (string-trim
                     (or (qq-state-message-preview message)
                         (alist-get 'raw-message message)
                         "[message]")))
           (prefix (cond
                    ((qq-state-message-mentions-self-p message) "@你  ")
                    ((qq-state-message-mentions-all-p message) "@全体成员  ")
                    (t ""))))
      (truncate-string-to-width
       (concat prefix preview)
       (max 1 qq-notifications-body-limit)
       nil nil "…"))))

(defun qq-notifications-open-message (session-key message-anchor)
  "Open SESSION-KEY and jump to snowflake MESSAGE-ANCHOR."
  (when (fboundp 'x-focus-frame)
    (ignore-errors (x-focus-frame (selected-frame))))
  (ignore-errors (raise-frame (selected-frame)))
  (qq-chat-open session-key)
  (when (and message-anchor (derived-mode-p 'qq-chat-mode))
    (qq-chat-goto-message message-anchor t)))

(defun qq-notifications--close (id)
  "Close desktop notification ID when it is still current."
  (when (equal id qq-notifications--last-id)
    (setq qq-notifications--last-id nil)
    (ignore-errors (notifications-close-notification id))))

(defun qq-notifications--show (message)
  "Show one desktop notification for normalized MESSAGE."
  (when (and qq-notifications-mode
             (qq-notifications-message-notify-p message))
    (let* ((session-key (alist-get 'session-key message))
           (session (qq-state-session session-key))
           (anchor (qq-state-message-anchor message))
           (args
            (append
             (list :app-name "emacs-qq"
                   :title (qq-notifications--title message session)
                   :body (qq-notifications--body message)
                   :urgency (if (qq-state-message-mentions-self-p message)
                                "critical"
                              "normal")
                   :timeout -1
                   :actions '("default" "Open")
                   :on-action
                   (lambda (&rest _)
                     (qq-notifications-open-message session-key anchor)))
             qq-notifications-extra-args)))
      (when qq-notifications--last-id
        (ignore-errors
          (notifications-close-notification qq-notifications--last-id)))
      (ring-insert (qq-notifications--history-ring) (copy-tree message))
      (condition-case err
          (setq qq-notifications--last-id
                (apply #'notifications-notify args))
        (error
         (message "qq: desktop notification failed: %s"
                  (error-message-string err))))
      (when (and qq-notifications-timeout qq-notifications--last-id)
        (run-with-timer qq-notifications-timeout nil
                        #'qq-notifications--close
                        qq-notifications--last-id)))))

(defun qq-notifications--handle-state-change (event)
  "Schedule a desktop notification for one incoming state EVENT."
  (when (eq (plist-get event :type) 'reset)
    (clrhash qq-notifications--seen-anchors)
    (setq qq-notifications--seen-anchor-order nil))
  (when (and (eq (plist-get event :type) 'message)
             (eq (plist-get event :mutation) 'create)
             (eq (plist-get event :source) 'event))
    (when-let* ((message (plist-get event :message))
                (anchor (qq-state-message-anchor message)))
      (unless (gethash anchor qq-notifications--seen-anchors)
        (qq-notifications--remember-anchor anchor)
        (if (> qq-notifications-delay 0)
            (run-with-timer qq-notifications-delay nil
                            #'qq-notifications--show
                            (copy-tree message))
          (qq-notifications--show message))))))

;;;###autoload
(define-minor-mode qq-notifications-mode
  "Toggle telega-style desktop notifications for emacs-qq."
  :global t
  :group 'qq-notifications
  (if qq-notifications-mode
      (add-hook 'qq-state-change-hook #'qq-notifications--handle-state-change)
    (remove-hook 'qq-state-change-hook #'qq-notifications--handle-state-change)))

;;;###autoload
(defun qq-notifications-history ()
  "Show recent emacs-qq desktop notifications."
  (interactive)
  (let ((buffer (get-buffer-create "*QQ Notifications*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (dolist (message (ring-elements (qq-notifications--history-ring)))
          (let* ((session-key (alist-get 'session-key message))
                 (session (qq-state-session session-key))
                 (anchor (qq-state-message-anchor message))
                 (title (qq-notifications--title message session))
                 (body (qq-notifications--body message)))
            (insert-text-button
             title
             'follow-link t
             'action (lambda (_button)
                       (qq-notifications-open-message session-key anchor)))
            (insert (format "  %s\n" body))))))
    (pop-to-buffer buffer)))

(provide 'qq-notifications)

;;; qq-notifications.el ends here
