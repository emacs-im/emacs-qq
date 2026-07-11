;;; qq-modes.el --- Global presentation modes for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style global mode-line status for unread QQ activity.

;;; Code:

(require 'cl-lib)
(require 'disco-mode-line)
(require 'qq-customize)
(require 'qq-state)
(require 'qq-root)

(defvar qq-mode-line-string ""
  "Cached emacs-qq mode-line string.")

(defcustom qq-mode-line-format
  '(qq-mode-line-mode ("" qq-mode-line-string))
  "Mode-line construct installed in `mode-line-misc-info'."
  :type 'sexp
  :group 'qq-modes
  :risky t)

(defun qq-mode-line--counts ()
  "Return (UNREAD . MENTIONS) counts for current QQ sessions.

UNREAD is the number of messages in unmuted sessions.  MENTIONS is the
number of sessions carrying an unread @self or @all marker, including muted
sessions because native QQ mentions are priority activity."
  (let ((unread 0)
        (mentions 0))
    (dolist (session (qq-state-sessions))
      (unless (qq-root--session-muted-p session)
        (cl-incf unread (max 0 (or (alist-get 'unread-count session) 0))))
      (when (qq-root--session-mention-kinds session)
        (cl-incf mentions)))
    (cons unread mentions)))

(defun qq-mode-line-open-root ()
  "Open the QQ root buffer from the mode line."
  (interactive)
  (qq-root-open))

(defun qq-mode-line-open-unread ()
  "Open QQ root and move to the next unread session."
  (interactive)
  (qq-root-open)
  (qq-root-next-unread))

(defun qq-mode-line-open-mention ()
  "Open QQ root and move to a session with an unread mention."
  (interactive)
  (qq-root-open)
  (unless (qq-root--move-linewise
           1
           (lambda ()
             (when-let* ((session (qq-root--session-at-point)))
               (qq-root--session-mention-kinds session)))
           t)
    (message "qq: no unread mentions")))

(defun qq-mode-line-icon ()
  "Return clickable QQ label for the mode line."
  (disco-mode-line-indicator
   "QQ" :face 'mode-line-emphasis
   :command #'qq-mode-line-open-root :help-echo "Open QQ"))

(defun qq-mode-line-unread-unmuted ()
  "Return mode-line text for unmuted unread messages."
  (let ((count (car (qq-mode-line--counts))))
    (unless (zerop count)
      (disco-mode-line-indicator
       (number-to-string count) :prefix " " :face 'qq-mode-line-unread
       :command #'qq-mode-line-open-unread
       :help-echo "Open unread QQ chats"))))

(defun qq-mode-line-mentions ()
  "Return mode-line text for sessions with unread native mentions."
  (let ((count (cdr (qq-mode-line--counts))))
    (unless (zerop count)
      (disco-mode-line-indicator
       (format "@%d" count) :prefix " " :face 'qq-mode-line-mention
       :command #'qq-mode-line-open-mention
       :help-echo "Open QQ chats with unread mentions"))))

(defun qq-mode-line-update (&rest _ignored)
  "Refresh the cached QQ mode-line status."
  (when qq-mode-line-mode
    (disco-mode-line-update-cache
     'qq-mode-line-string qq-mode-line-string-format)))

;;;###autoload
(define-minor-mode qq-mode-line-mode
  "Toggle global QQ unread and mention status in the mode line."
  :init-value nil
  :global t
  :group 'qq-modes
  (if qq-mode-line-mode
      (progn
        (disco-mode-line-install 'qq-mode-line-format)
        (add-hook 'qq-state-change-hook #'qq-mode-line-update)
        (qq-mode-line-update))
    (disco-mode-line-uninstall 'qq-mode-line-format)
    (setq qq-mode-line-string "")
    (remove-hook 'qq-state-change-hook #'qq-mode-line-update)
    (force-mode-line-update t)))

(provide 'qq-modes)

;;; qq-modes.el ends here
