;;; qq-transient.el --- Transient menus for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Transient command menus modeled after disco-room / telega:
;; - chat-level menu (`qq-chat-transient')
;; - message-at-point menu (`qq-chat-message-transient')
;; - attach menu (`qq-chat-attach-transient')
;; - root menu (`qq-root-transient')
;;
;; These replace discoverability that used to live in always-visible
;; action-button rows.  Single-key timeline bindings remain for power use.

;;; Code:

(require 'transient)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-root)
(require 'qq-state)

(declare-function qq-connect "qq")
(declare-function qq-disconnect "qq")
(declare-function qq-reset-session-state "qq")


;;; Availability helpers

(defun qq-transient--message-at-point ()
  "Return message at point, or nil without signaling."
  (ignore-errors (qq-chat--message-at-point)))

(defun qq-transient--no-message-at-point-p ()
  "Return non-nil when there is no message under point."
  (null (qq-transient--message-at-point)))

(defun qq-transient--reply-inapt-p ()
  "Return non-nil when reply is unavailable for the message at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'server-id message))
        (qq-state-message-recalled-p message))))

(defun qq-transient--recall-inapt-p ()
  "Return non-nil when recall is unavailable for the message at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'server-id message))
        (not (alist-get 'self-p message))
        (qq-state-message-recalled-p message))))

(defun qq-transient--resource-inapt-p ()
  "Return non-nil when open-resource is unavailable at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (not (qq-chat--message-openable-p message)))))

(defun qq-transient--avatar-inapt-p ()
  "Return non-nil when avatar open is unavailable at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'sender-id message)))))

(defun qq-transient--no-reply-context-p ()
  "Return non-nil when composer has no pending reply."
  (null (qq-chat--reply-message)))

(defun qq-transient--cancel-inapt-p ()
  "Return non-nil when cancel-dwim has nothing to clear."
  (and (qq-transient--no-reply-context-p)
       (let ((draft (ignore-errors (qq-chat--current-draft-string))))
         (or (null draft) (string-empty-p (string-trim draft))))))

(defun qq-transient--no-session-at-point-p ()
  "Return non-nil when root point is not on a session row."
  (null (ignore-errors (qq-root--session-key-at-point))))


;;; Attach helpers (typed)

(defun qq-chat-attach-as-image ()
  "Prompt for a file and attach it as an image segment."
  (interactive)
  (qq-chat-attach-file (read-file-name "Attach image: " nil nil t) "image"))

(defun qq-chat-attach-as-file ()
  "Prompt for a file and attach it as a generic file segment."
  (interactive)
  (qq-chat-attach-file (read-file-name "Attach file: " nil nil t) "file"))


;;; Chat / message transients
;;
;; Magit-style autoloads: do NOT put bare `;;;###autoload' above
;; `transient-define-prefix'.  loaddefs would copy the whole form into
;; *-autoloads.el, where `transient-define-prefix' is still undefined.
;; Use an explicit (autoload SYMBOL FILE nil t) cookie instead; the real
;; definition runs only after this file loads and (require 'transient).

;;;###autoload(autoload 'qq-chat-message-transient "qq-transient" nil t)
(transient-define-prefix qq-chat-message-transient ()
  "Message actions for the QQ chat message at point.

Prefer this over inline button rows (telega/disco style)."
  [["Message"
    ("r" "Reply" qq-chat-reply-to-message
     :inapt-if qq-transient--reply-inapt-p)
    ("d" "Recall" qq-chat-delete-message
     :inapt-if qq-transient--recall-inapt-p)
    ("o" "Open resource" qq-chat-open-resource-at-point
     :inapt-if qq-transient--resource-inapt-p)
    ("a" "Open avatar" qq-chat-open-avatar-at-point
     :inapt-if qq-transient--avatar-inapt-p)]
   ["Navigate"
    ("n" "Next message" qq-chat-next-message)
    ("p" "Previous message" qq-chat-previous-message)]])

;;;###autoload(autoload 'qq-chat-attach-transient "qq-transient" nil t)
(transient-define-prefix qq-chat-attach-transient ()
  "Attach local media / QQ faces into the QQ chat composer."
  [["Attach"
    ("e" "QQ face / emoji (C-c C-e)" qq-chat-attach-face)
    ("f" "File (auto type)" qq-chat-attach-file)
    ("i" "As image" qq-chat-attach-as-image)
    ("F" "As file" qq-chat-attach-as-file)
    ("v" "Clipboard (C-c C-v)" qq-chat-attach-clipboard)]])

;;;###autoload(autoload 'qq-chat-transient "qq-transient" nil t)
(transient-define-prefix qq-chat-transient ()
  "Chat command menu for emacs-qq."
  [["Timeline"
    ("g" "Refresh" qq-chat-refresh)
    ("o" "Load older" qq-chat-load-older-messages)
    ("/" "Search" qq-chat-search)
    ("n" "Search next" qq-chat-search-next)
    ("p" "Search prev" qq-chat-search-prev)
    ("R" "Mark read" qq-chat-read-all)
    ("m" "Message at point…" qq-chat-message-transient
     :inapt-if qq-transient--no-message-at-point-p)]
   ["Composer"
    ("c" "Send" qq-chat-send-message)
    ("a" "Attach…" qq-chat-attach-transient)
    ("E" "QQ face / emoji" qq-chat-attach-face)
    ("k" "Cancel reply/draft" qq-chat-cancel-dwim
     :inapt-if qq-transient--cancel-inapt-p)
    ("e" "Focus draft" qq-chat-edit-draft)
    ("r" "Reply at point" qq-chat-reply-to-message
     :inapt-if qq-transient--reply-inapt-p)
    ("d" "Recall at point" qq-chat-delete-message
     :inapt-if qq-transient--recall-inapt-p)]
   ["Session"
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]])


;;; Root transient

;;;###autoload(autoload 'qq-root-transient "qq-transient" nil t)
(transient-define-prefix qq-root-transient ()
  "Root command menu for emacs-qq."
  [["Sessions"
    ("g" "Refresh" qq-root-refresh)
    ("RET" "Open at point" qq-root-open-at-point
     :inapt-if qq-transient--no-session-at-point-p)
    ("a" "Open avatar" qq-root-open-avatar-at-point
     :inapt-if qq-transient--no-session-at-point-p)
    ("s" "Search session…" qq-root-search)
    ("u" "Next unread" qq-root-next-unread)]
   ["Connection"
    ("c" "Connect" qq-connect)
    ("C" "Disconnect" qq-disconnect)
    ("x" "Reset state" qq-reset-session-state)]
   ["Window"
    ("q" "Quit window" quit-window)
    ("?" "Describe mode" describe-mode)]])

(provide 'qq-transient)

;;; qq-transient.el ends here
