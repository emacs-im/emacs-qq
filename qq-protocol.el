;;; qq-protocol.el --- Wire value contracts for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Small, dependency-free decoders shared by the transport, API, and state
;; layers.  Identity values are intentionally not "normalized": accepting a
;; numeric NT message snowflake after JSON decoding would preserve an already
;; rounded value as if it were authoritative.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst qq-protocol--max-safe-integer 9007199254740991
  "Largest integer represented exactly by a JSON/JavaScript number.")

(defun qq-protocol-message-id-p (value)
  "Return non-nil when VALUE is an original NT message snowflake string."
  (and (stringp value)
       (not (string-empty-p value))
       (string-match-p "\\`[0-9]+\\'" value)))

(defun qq-protocol-optional-message-id (value &optional context)
  "Return optional message-id VALUE after validating its wire representation.

Nil remains nil.  Every non-nil value must be an original decimal string.
CONTEXT is included in protocol errors."
  (cond
   ((null value) nil)
   ((qq-protocol-message-id-p value) value)
   (t
    (error "qq: %s requires message_id as an original decimal string, got %S"
           (or context "protocol payload") value))))

(defun qq-protocol--closed-object-p (value keys)
  "Return non-nil when VALUE is an alist with exactly unique KEYS."
  (and (consp value)
       (proper-list-p value)
       (cl-every (lambda (cell)
                   (and (consp cell) (symbolp (car cell))))
                 value)
       (let ((actual (mapcar #'car value)))
         (and (= (length actual) (length keys))
              (= (length actual)
                 (length (delete-dups (copy-sequence actual))))
              (null (seq-difference actual keys))))))

(defun qq-protocol--positive-safe-integer-p (value)
  "Return non-nil when VALUE is a positive JSON-safe integer."
  (and (integerp value)
       (> value 0)
       (<= value qq-protocol--max-safe-integer)))

(defun qq-protocol-poke-recall-reference-p (value)
  "Return non-nil when VALUE is a closed native poke recall reference.

The native locator consists of the NT snowflake `message_id' and the exact
QQ `Peer' used by `recallNudge'.  Private `peer_uid' values are opaque NT
UIDs, not QQ numbers, and must therefore remain strings.  `valid_before' is
the positive, safe-integer epoch-second deadline supplied by the server."
  (and (qq-protocol--closed-object-p value
                                     '(message_id peer valid_before))
       (let ((message-id (alist-get 'message_id value))
             (peer (alist-get 'peer value))
             (valid-before (alist-get 'valid_before value)))
         (and (stringp message-id)
              (string-match-p "\\`[1-9][0-9]*\\'" message-id)
              (qq-protocol--positive-safe-integer-p valid-before)
              (qq-protocol--closed-object-p
               peer '(chat_type peer_uid guild_id))
              (memq (alist-get 'chat_type peer) '(1 2))
              (let ((peer-uid (alist-get 'peer_uid peer)))
                (and (stringp peer-uid)
                     (not (string-empty-p peer-uid))))
              (equal (alist-get 'guild_id peer) "")))))

(defun qq-protocol-validate-poke-recall-reference
    (value &optional context error-symbol)
  "Return a copy of native poke recall reference VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error';
callers validating interactive input may pass `user-error'."
  (unless (qq-protocol-poke-recall-reference-p value)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed native poke recall reference, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol-poke-recall-reference-expired-p (reference &optional now)
  "Return non-nil when poke recall REFERENCE has expired at NOW.

REFERENCE must be a valid closed native poke recall reference.  NOW is an
epoch-second number and defaults to the current time.  The deadline itself is
already expired, so NOW equal to `valid_before' returns non-nil."
  (unless (qq-protocol-poke-recall-reference-p reference)
    (error "qq: cannot check an invalid poke recall reference: %S" reference))
  (<= (alist-get 'valid_before reference)
      (or now (float-time))))

(defun qq-protocol-json-true-p (value)
  "Return non-nil only when wire VALUE explicitly represents JSON true."
  (or (eq value t)
      (and (numberp value) (not (zerop value)))
      (and (stringp value)
           (member (downcase value) '("true" "1" "yes")))))

(provide 'qq-protocol)

;;; qq-protocol.el ends here
