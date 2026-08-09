;;; qq-protocol.el --- Wire value contracts for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

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

(defun qq-protocol--decimal-string-p (value)
  "Return non-nil when VALUE is a non-empty decimal identity string."
  (and (stringp value)
       (not (string-empty-p value))
       (string-match-p "\\`[0-9]+\\'" value)))

(defun qq-protocol--nonzero-decimal-string-p (value)
  "Return non-nil when VALUE is a positive decimal identity string."
  (and (stringp value)
       (string-match-p "\\`[1-9][0-9]*\\'" value)))

(defun qq-protocol-decimal-string-compare (left right)
  "Compare nonzero decimal strings LEFT and RIGHT without number coercion.

Return -1, 0, or 1.  Length is compared before lexicographic order, which is
exact for canonical decimal strings and cannot lose NT sequence precision."
  (unless (qq-protocol--nonzero-decimal-string-p left)
    (error "qq: decimal comparison requires nonzero LEFT string, got %S" left))
  (unless (qq-protocol--nonzero-decimal-string-p right)
    (error "qq: decimal comparison requires nonzero RIGHT string, got %S" right))
  (cond
   ((< (length left) (length right)) -1)
   ((> (length left) (length right)) 1)
   ((string-lessp left right) -1)
   ((string-lessp right left) 1)
   (t 0)))

(defun qq-protocol-group-uin-p (value)
  "Return non-nil when VALUE is a canonical uint32 QQ group UIN string."
  (and (qq-protocol--nonzero-decimal-string-p value)
       (<= (qq-protocol-decimal-string-compare value "4294967295") 0)))

(defun qq-protocol-message-id-p (value)
  "Return non-nil when VALUE is a canonical NT message snowflake string.

The hard-cut wire identity is a nonzero decimal string with no leading zero."
  (qq-protocol--nonzero-decimal-string-p value))

(defun qq-protocol-message-sequence-p (value)
  "Return non-nil when VALUE is a canonical nonzero message sequence string.

Sequences are conversation-local uint64 identities.  They stay decimal
strings at the Elisp boundary just like message snowflakes, but this separate
predicate prevents the two domains from being used interchangeably."
  (qq-protocol--nonzero-decimal-string-p value))

(defun qq-protocol-optional-message-id (value &optional context)
  "Return optional message-id VALUE after validating its wire representation.

Nil remains nil.  Every non-nil value must be a canonical nonzero decimal
string with no leading zero.
CONTEXT is included in protocol errors."
  (cond
   ((null value) nil)
   ((qq-protocol-message-id-p value) value)
   (t
    (error "qq: %s requires message_id as a canonical nonzero decimal string, got %S"
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

(defun qq-protocol--nonnegative-safe-integer-p (value)
  "Return non-nil when VALUE is a non-negative JSON-safe integer."
  (and (integerp value)
       (>= value 0)
       (<= value qq-protocol--max-safe-integer)))

(defconst qq-protocol-account-presence-kinds
  '("online" "q_me" "away" "busy" "do_not_disturb" "invisible")
  "Closed standard account presence kinds.")

(defun qq-protocol-account-presence-p (value)
  "Return non-nil when VALUE is one closed account presence object."
  (let ((kind (and (listp value) (alist-get 'kind value))))
    (cond
     ((member kind qq-protocol-account-presence-kinds)
      (qq-protocol--closed-object-p value '(kind)))
     ((equal kind "custom")
      (and (qq-protocol--closed-object-p value '(kind face_id wording))
           (let ((face-id (alist-get 'face_id value)))
             (and (integerp face-id) (<= 0 face-id #xffffffff)))
           (stringp (alist-get 'wording value))))
     (t nil))))

(defun qq-protocol-validate-account-presence
    (value &optional context error-symbol)
  "Return a copy of closed account presence VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error'."
  (unless (qq-protocol-account-presence-p value)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed account presence, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol-emacs-session-locator-p (value)
  "Return non-nil when VALUE is a closed Emacs session locator."
  (pcase (and (consp value) (alist-get 'kind value))
    ("group"
     (and (qq-protocol--closed-object-p value '(kind group_id))
          (qq-protocol-group-uin-p (alist-get 'group_id value))))
    ("private"
     (and (qq-protocol--closed-object-p value '(kind user_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'user_id value))))
    ("guild-channel"
     (and (qq-protocol--closed-object-p
           value '(kind guild_id channel_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'guild_id value))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'channel_id value))))
    ("dataline"
     (and (qq-protocol--closed-object-p
           value '(kind peer_uid variant))
          (let ((peer-uid (alist-get 'peer_uid value)))
            (and (stringp peer-uid)
                 (not (string-empty-p peer-uid))))
          (member (alist-get 'variant value) '("desktop" "mobile"))))
    ("service"
     (and (qq-protocol--closed-object-p value '(kind peer_uid))
          (let ((peer-uid (alist-get 'peer_uid value)))
            (and (stringp peer-uid)
                 (not (string-empty-p peer-uid))))))
    (_ nil)))

(defun qq-protocol-emacs-chat-locator-p (value)
  "Return non-nil when VALUE is a searchable group/private locator."
  (pcase (and (consp value) (alist-get 'kind value))
    ("group"
     (and (qq-protocol--closed-object-p value '(kind group_id))
          (qq-protocol-group-uin-p (alist-get 'group_id value))))
    ("private"
     (and (qq-protocol--closed-object-p value '(kind user_id))
          (qq-protocol--nonzero-decimal-string-p
           (alist-get 'user_id value))))
    (_ nil)))

(defun qq-protocol-validate-emacs-session-locator
    (value &optional context error-symbol)
  "Return a copy of closed Emacs session locator VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error'."
  (unless (qq-protocol-emacs-session-locator-p value)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed Emacs session locator, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol--emacs-message-search-sender-p (value)
  "Return non-nil when VALUE is a closed message-search sender."
  (and (qq-protocol--closed-object-p value '(user_id name))
       (qq-protocol--nonzero-decimal-string-p (alist-get 'user_id value))
       (let ((name (alist-get 'name value)))
         (and (stringp name) (not (string-empty-p name))))))

(defun qq-protocol--emacs-message-search-summary-p (value)
  "Return non-nil when VALUE is one closed message-search summary."
  (and (qq-protocol--closed-object-p
        value '(chat message_id message_seq sent_at sender preview))
       (qq-protocol-emacs-chat-locator-p (alist-get 'chat value))
       (qq-protocol--nonzero-decimal-string-p
        (alist-get 'message_id value))
       (qq-protocol--nonzero-decimal-string-p
        (alist-get 'message_seq value))
       (qq-protocol--positive-safe-integer-p (alist-get 'sent_at value))
       (qq-protocol--emacs-message-search-sender-p
        (alist-get 'sender value))
       (let ((preview (alist-get 'preview value)))
         (and (stringp preview) (not (string-empty-p preview))))))

(defun qq-protocol--emacs-message-search-segment-p (value)
  "Return non-nil when VALUE has the closed native segment envelope.

The API layer validates each discriminated payload in detail.  This protocol
module only owns the common JSON object boundary so it stays independent from
the recursive media/forward decoder."
  (and (qq-protocol--closed-object-p value '(kind payload))
       (let ((kind (alist-get 'kind value)))
         (and (stringp kind) (not (string-empty-p kind))))
       (let ((payload (alist-get 'payload value)))
         (or (null payload)
             (and (proper-list-p payload)
                  (cl-every (lambda (cell)
                              (and (consp cell) (symbolp (car cell))))
                            payload))))))

(defun qq-protocol--emacs-message-search-reaction-p (value)
  "Return non-nil when VALUE is one closed normalized reaction snapshot."
  (and (qq-protocol--closed-object-p
        value '(emoji_id emoji_type count chosen))
       (qq-protocol--nonzero-decimal-string-p (alist-get 'emoji_id value))
       (let ((emoji-type (alist-get 'emoji_type value)))
         (and (stringp emoji-type) (not (string-empty-p emoji-type))))
       (qq-protocol--positive-safe-integer-p (alist-get 'count value))
       (memq (alist-get 'chosen value) '(t :false))))

(defun qq-protocol--emacs-message-search-message-p (value)
  "Return non-nil when VALUE is one flat closed rendering snapshot."
  (and (qq-protocol--closed-object-p
        value '(chat message_id message_seq sent_at sender outgoing state
                segments reactions))
       (qq-protocol-emacs-chat-locator-p (alist-get 'chat value))
       (qq-protocol--nonzero-decimal-string-p
        (alist-get 'message_id value))
       (qq-protocol--nonzero-decimal-string-p
        (alist-get 'message_seq value))
       (qq-protocol--positive-safe-integer-p (alist-get 'sent_at value))
       (qq-protocol--emacs-message-search-sender-p
        (alist-get 'sender value))
       (memq (alist-get 'outgoing value) '(t :false))
       (member (alist-get 'state value) '("live" "recalled"))
       (let ((segments (alist-get 'segments value)))
         (and (proper-list-p segments)
              (cl-every #'qq-protocol--emacs-message-search-segment-p
                        segments)
              (or (equal (alist-get 'state value) "live")
                  (null segments))))
       (let ((reactions (alist-get 'reactions value)))
         (and (proper-list-p reactions)
              (cl-every #'qq-protocol--emacs-message-search-reaction-p
                        reactions)))))

(defun qq-protocol-emacs-message-search-result-p (value &optional projection)
  "Return non-nil when VALUE is one closed result for PROJECTION.

PROJECTION is `summary' or `message'.  A message projection retains the exact
identity fields but is a flat, fork-native rendering snapshot rather than an
OB11 message wrapper."
  (pcase (or projection 'summary)
    ('summary
     (qq-protocol--emacs-message-search-summary-p value))
    ('message
     (qq-protocol--emacs-message-search-message-p value))
    (_ nil)))

(defun qq-protocol-emacs-message-search-page-p (value &optional projection)
  "Return non-nil when VALUE is one closed message-search page.

An empty result page may still carry `next_cursor'.  The cursor is an opaque
server-owned continuation and is therefore validated only as a non-empty
string; clients must never parse or synthesize it.  PROJECTION, when non-nil,
also requires the server to return that exact projection."
  (and (memq projection '(nil summary message))
       (qq-protocol--closed-object-p value '(projection results next_cursor))
       (let ((actual (alist-get 'projection value)))
         (and (member actual '("summary" "message"))
              (or (null projection)
                  (equal actual (symbol-name projection)))))
       (let* ((actual (intern (alist-get 'projection value)))
              (results (alist-get 'results value)))
         (and (proper-list-p results)
              (cl-every
               (lambda (result)
                 (qq-protocol-emacs-message-search-result-p result actual))
               results)))
       (let ((cursor (alist-get 'next_cursor value)))
         (or (null cursor)
             (and (stringp cursor) (not (string-empty-p cursor)))))))

(defun qq-protocol-validate-emacs-message-search-page
    (value &optional context error-symbol projection)
  "Return a copy of closed message-search page VALUE after validation.

CONTEXT is included in the diagnostic.  ERROR-SYMBOL defaults to `error'.
When PROJECTION is non-nil it must be `summary' or `message' and match the
wire discriminator exactly."
  (unless (memq projection '(nil summary message))
    (error "qq: invalid message-search projection %S" projection))
  (unless (qq-protocol-emacs-message-search-page-p value projection)
    (signal (or error-symbol 'error)
            (list
             (format "qq: %s requires a closed message-search page, got %S"
                     (or context "protocol payload") value))))
  (copy-tree value))

(defun qq-protocol-json-true-p (value)
  "Return non-nil only when wire VALUE explicitly represents JSON true."
  (or (eq value t)
      (and (numberp value) (not (zerop value)))
      (and (stringp value)
           (member (downcase value) '("true" "1" "yes")))))

(provide 'qq-protocol)

;;; qq-protocol.el ends here
