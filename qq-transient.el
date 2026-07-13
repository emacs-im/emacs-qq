;;; qq-transient.el --- Transient menus for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Transient command menus for the root, chat, and message contexts:
;; - chat-level menu (`qq-chat-transient')
;; - message-at-point menu (`qq-chat-message-transient')
;; - attach menu (`qq-chat-attach-transient')
;; - root menu (`qq-root-transient')
;;
;; These replace discoverability that used to live in always-visible
;; action-button rows.  Single-key timeline bindings remain for power use.

;;; Code:

(require 'transient)
(require 'appkit-media)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-protocol)
(require 'qq-root)
(require 'qq-state)
(require 'qq-user)
(require 'qq-group)

(declare-function qq-connect "qq")
(declare-function qq-disconnect "qq")
(declare-function qq-reset-session-state "qq")

(defvar qq-chat--forward-request-active-p)
(defvar qq-chat--marked-message-anchors)
(defvar qq-chat--session-key)


;;; Availability helpers

(defun qq-transient--message-at-point ()
  "Return message at point, or nil without signaling."
  (ignore-errors (qq-chat--message-at-point)))

(defun qq-transient--no-message-at-point-p ()
  "Return non-nil when there is no message under point."
  (null (qq-transient--message-at-point)))

(defun qq-transient--poke-session-inapt-p ()
  "Return non-nil when the current conversation cannot send pokes."
  (condition-case nil
      (progn
        (qq-chat--poke-session qq-chat--session-key)
        nil)
    (user-error t)))

(defun qq-transient--poke-sender-inapt-p ()
  "Return non-nil when the message sender at point cannot be poked."
  (or (qq-transient--poke-session-inapt-p)
      (condition-case nil
          (progn
            (qq-chat--poke-sender-at-point)
            nil)
        (user-error t))))

(defun qq-transient--reply-inapt-p ()
  "Return non-nil when reply is unavailable for the message at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'server-id message))
        (qq-state-message-recalled-p message))))

(defun qq-transient--goto-reply-inapt-p ()
  "Return non-nil when the message at point has no reply target to jump to."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (qq-chat--message-reply-id message)))))

(defun qq-transient--pop-ring-empty-p ()
  "Return non-nil when the messages pop ring is empty."
  (or (not (boundp 'qq-chat--messages-pop-ring))
      (null qq-chat--messages-pop-ring)
      (not (ring-p qq-chat--messages-pop-ring))
      (ring-empty-p qq-chat--messages-pop-ring)))

(defun qq-transient--recall-inapt-p ()
  "Return non-nil when recall is unavailable for the message at point."
  (let* ((message (qq-transient--message-at-point))
         (poke-p (and message (qq-state-poke-message-p message)))
         (recall-reference
          (and poke-p (qq-state-poke-recall-reference message))))
    (or (null message)
        (null (alist-get 'server-id message))
        (not (alist-get 'self-p message))
        (and poke-p
             (or (null recall-reference)
                 (qq-protocol-poke-recall-reference-expired-p
                  recall-reference)))
        (qq-state-message-recalled-p message))))

(defun qq-transient--forward-inapt-p ()
  "Return non-nil when forwarding the message at point is unavailable."
  (not (qq-chat--message-forwardable-p
        (qq-transient--message-at-point))))

(defun qq-transient--reaction-inapt-p ()
  "Return non-nil when reacting to the message at point is unavailable."
  (not (qq-chat--message-reactable-p
        (qq-transient--message-at-point))))

(defun qq-transient--no-forward-marks-p ()
  "Return non-nil when there are no raw forward selections to clear."
  (null qq-chat--marked-message-anchors))

(defun qq-transient--forward-marked-inapt-p ()
  "Return non-nil when a merged-forward submission is unavailable."
  (or qq-chat--forward-request-active-p
      (null (qq-chat-marked-messages))))

(defun qq-transient--resource-inapt-p ()
  "Return non-nil when open-resource is unavailable at point."
  (appkit-media-card-action-inapt-reason 'open))

(defun qq-transient--avatar-inapt-p ()
  "Return non-nil when avatar open is unavailable at point."
  (let ((message (qq-transient--message-at-point)))
    (or (null message)
        (null (alist-get 'sender-id message)))))

(defun qq-transient--user-inapt-p ()
  "Return non-nil when the message sender has no user page."
  (let* ((message (qq-transient--message-at-point))
         (user-id (and message (alist-get 'sender-id message))))
    (not (and (qq-api-user-id-p user-id) (not (equal user-id "0"))))))

(defun qq-transient--peer-user-inapt-p ()
  "Return non-nil when the current chat has no private peer user page."
  (let* ((session (and (boundp 'qq-chat--session-key)
                       (qq-state-session qq-chat--session-key)))
         (user-id (and session
                       (or (alist-get 'peer-uin session)
                           (alist-get 'target-id session)))))
    (not (and (eq (alist-get 'type session) 'private)
              (qq-api-user-id-p user-id)))))

(defun qq-transient--chat-info-inapt-p ()
  "Return non-nil when the current chat has no profile page."
  (let* ((session (and (boundp 'qq-chat--session-key)
                       (qq-state-session qq-chat--session-key)))
         (type (and session (alist-get 'type session)))
         (target-id (and session
                         (or (and (eq type 'private)
                                  (alist-get 'peer-uin session))
                             (alist-get 'target-id session)))))
    (pcase type
      ('private (not (qq-api-user-id-p target-id)))
      ('group (not (qq-api-group-id-p target-id)))
      (_ t))))

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

(defun qq-transient--root-user-inapt-p ()
  "Return non-nil when the root session has no user page."
  (let* ((session (ignore-errors (qq-root--session-at-point)))
         (user-id (and session
                       (or (alist-get 'peer-uin session)
                           (alist-get 'target-id session)))))
    (not (and (eq (alist-get 'type session) 'private)
              (qq-api-user-id-p user-id)))))

(defun qq-transient--root-info-inapt-p ()
  "Return non-nil when the root session has no profile page."
  (let* ((session (ignore-errors (qq-root--session-at-point)))
         (type (and session (alist-get 'type session)))
         (target-id (and session
                         (or (and (eq type 'private)
                                  (alist-get 'peer-uin session))
                             (alist-get 'target-id session)))))
    (pcase type
      ('private (not (qq-api-user-id-p target-id)))
      ('group (not (qq-api-group-id-p target-id)))
      (_ t))))


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

Prefer this over inline button rows."
  [["Message"
    ("r" "Reply" qq-chat-reply-to-message
     :inapt-if qq-transient--reply-inapt-p)
    ("f" "Forward" qq-chat-forward-message
     :inapt-if qq-transient--forward-inapt-p)
    ("M" "Mark / unmark" qq-chat-toggle-forward-mark
     :inapt-if qq-transient--forward-inapt-p)
    ("d" "Recall" qq-chat-delete-message
     :inapt-if qq-transient--recall-inapt-p)
    ("!" "React…" qq-chat-react-to-message
     :inapt-if qq-transient--reaction-inapt-p)
    ("P" "Poke sender" qq-chat-poke-sender
     :inapt-if qq-transient--poke-sender-inapt-p)
    ("a" "Open avatar" qq-chat-open-avatar-at-point
     :inapt-if qq-transient--avatar-inapt-p)
    ("i" "User page" qq-chat-open-user-at-point
     :inapt-if qq-transient--user-inapt-p)
    ("g" "Goto reply target" qq-chat-goto-reply
     :inapt-if qq-transient--goto-reply-inapt-p)]
   ["Media"
    ("o" "Open / play" appkit-media-card-open
     :inapt-if qq-transient--resource-inapt-p)
    ("D" "Download / retry" appkit-media-card-download
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'download)))
    ("s" "Save as" appkit-media-card-save-as
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'save-as)))
    ("y" "Copy media URL" appkit-media-card-copy-url
     :inapt-if (lambda ()
                 (appkit-media-card-action-inapt-reason 'copy-url)))]
   ["Navigate"
    ("n" "Next message" qq-chat-next-message)
    ("p" "Previous message" qq-chat-previous-message)
    ("x" "Pop jump" qq-chat-goto-pop-message
     :inapt-if qq-transient--pop-ring-empty-p)]])

;;;###autoload(autoload 'qq-chat-attach-transient "qq-transient" nil t)
(transient-define-prefix qq-chat-attach-transient ()
  "Attach local media / QQ faces into the QQ chat composer."
  [["Attach"
    ("e" "QQ face (C-c C-e)" qq-chat-attach-face)
    ("E" "Favorite face (C-u C-c C-e)" qq-chat-attach-custom-face)
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
    ("/" "Search timeline" qq-chat-search)
    ("M-/" "Search results" qq-chat-search-results)
    ("x" "Cancel search" qq-chat-search-cancel)
    ("s" "Search older" qq-chat-search)
    ("S" "Search newer" qq-chat-search-forward)
    ("n" "Search next" qq-chat-search-next)
    ("p" "Search prev" qq-chat-search-prev)
    ("P" "Poke user…" qq-chat-send-poke
     :inapt-if qq-transient--poke-session-inapt-p)
    ("R" "Mark read" qq-chat-read-all)
    ("v" "Forward marked" qq-chat-forward-marked-messages
     :inapt-if qq-transient--forward-marked-inapt-p)
    ("U" "Clear forward marks" qq-chat-clear-forward-marks
     :inapt-if qq-transient--no-forward-marks-p)
    ("m" "Message at point…" qq-chat-message-transient
     :inapt-if qq-transient--no-message-at-point-p)]
   ["Composer"
    ("c" "Send" qq-chat-send-message)
    ("a" "Attach…" qq-chat-attach-transient)
    ("E" "QQ face" qq-chat-attach-face)
    ("F" "Favorite face" qq-chat-attach-custom-face)
    ("k" "Cancel reply/draft" qq-chat-cancel-dwim
     :inapt-if qq-transient--cancel-inapt-p)
    ("e" "Focus draft" qq-chat-edit-draft)
    ("r" "Reply at point" qq-chat-reply-to-message
     :inapt-if qq-transient--reply-inapt-p)
    ("d" "Recall at point" qq-chat-delete-message
     :inapt-if qq-transient--recall-inapt-p)]
   ["Session"
    ("h" "Chat info" qq-chat-open-peer-info
     :inapt-if qq-transient--chat-info-inapt-p)
    ("i" "User page" qq-chat-open-peer-user
     :inapt-if qq-transient--peer-user-inapt-p)
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
    ("i" "Session info" qq-root-open-info-at-point
     :inapt-if qq-transient--root-info-inapt-p)
    ("I" "My profile" qq-root-open-self-user)
    ("/" "Find session…" qq-root-open-session)
    ("s" "Search messages…" qq-root-search)
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
