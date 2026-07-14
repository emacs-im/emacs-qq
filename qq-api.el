;;; qq-api.el --- OneBot actions and event handlers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; High-level NapCat actions, bootstrap, and event handling.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)
(require 'qq-state)
(require 'qq-transport)

(declare-function qq-transport-cancel "qq-transport" (echo))
(declare-function qq-state-apply-poke-notice "qq-state" (notice))

(defvar qq-api--read-operations (make-hash-table :test #'equal)
  "In-flight mark-read operations keyed by session key.

Each operation owns the exact NT message id currently in flight and at most
one newer cursor intent.  It never reconstructs unread counts; those come
from authoritative Linux QQ observations.")

(defvar qq-api--read-operation-counter 0
  "Monotonic token used to reject stale read callbacks.")

(defvar qq-api--read-observation-clock 0
  "Monotonic clock for read-state writers and observations.")

(defvar qq-api--session-read-observation-tokens
  (make-hash-table :test #'equal)
  "Last accepted read-state token keyed by session key.")

(defvar qq-api--request-finalizers (make-hash-table :test #'equal)
  "Cleanup callbacks keyed by live transport request token.")

(cl-defstruct (qq-api--snapshot-request
               (:constructor qq-api--snapshot-request-create))
  resource
  action
  validator
  apply-function
  subscribers
  transport-token
  settled-p)

(cl-defstruct (qq-api--snapshot-subscription
               (:constructor qq-api--snapshot-subscription-create))
  request
  callback
  errback
  active-p)

(defvar qq-api--snapshot-active (make-hash-table :test #'eq)
  "Active authoritative-directory request keyed by resource.")

(defvar qq-api--snapshot-queued (make-hash-table :test #'eq)
  "Next authoritative-directory request keyed by resource.")

(defun qq-api--next-read-observation-token ()
  "Allocate a token for one read-state request or observation."
  (cl-incf qq-api--read-observation-clock))

(defun qq-api--accept-read-observation-p (session-key token)
  "Accept TOKEN for SESSION-KEY unless a newer observation already won.

Accepted tokens are recorded before returning non-nil.  This makes a newer
request response or notice supersede responses from requests that started
earlier, without introducing a generic application-wide generation counter."
  (when (< (gethash session-key qq-api--session-read-observation-tokens 0)
           token)
    (puthash session-key token qq-api--session-read-observation-tokens)
    t))

(defun qq-api-read-observation-start ()
  "Return a freshness token for a caller-owned read-state request."
  (qq-api--next-read-observation-token))

(defun qq-api-read-observation-accept-p (session-key token)
  "Accept caller-owned read observation TOKEN for SESSION-KEY if current.

Callers which fetch read state without asking `qq-api' to apply it use this
barrier before mutating state or choosing a timeline position."
  (qq-api--accept-read-observation-p session-key token))

(defun qq-api--response-data (response)
  "Extract `data' payload from RESPONSE alist."
  (alist-get 'data response nil nil #'eq))

(defun qq-api--default-error (response reason)
  "Display default API error using RESPONSE and REASON."
  (let ((retcode (and response (alist-get 'retcode response))))
    (message "qq: %s%s"
             reason
             (if retcode
                 (format " (retcode %s)" retcode)
               ""))))

(defun qq-api-call (action params callback &optional errback)
  "Call OneBot ACTION with PARAMS and CALLBACK.

ERRBACK falls back to `qq-api--default-error'."
  (qq-transport-send action params callback (or errback #'qq-api--default-error)))

(defun qq-api--run-request-finalizer (request-token)
  "Run and forget cleanup registered for REQUEST-TOKEN."
  (when-let* ((finalizer (and request-token
                              (gethash request-token
                                       qq-api--request-finalizers))))
    (remhash request-token qq-api--request-finalizers)
    (funcall finalizer)
    t))

(defun qq-api--call-with-materialization-owner
    (session-key action params callback &optional errback)
  "Call ACTION while owning one SESSION-KEY materialization window.

CALLBACK receives RESPONSE and the active request owner.  ERRBACK keeps the
ordinary `(RESPONSE REASON)' signature.  Success, error, synchronous callback,
synchronous dispatch failure, and explicit cancellation all settle the owner
exactly once."
  (let ((owner (qq-state-materialization-request-begin session-key))
        (handle-error (or errback #'qq-api--default-error))
        request-token
        settled)
    (cl-labels
        ((finish
          ()
          (unless settled
            (setq settled t)
            (when request-token
              (remhash request-token qq-api--request-finalizers))
            (qq-state-materialization-request-end owner)))
         (succeed
          (response)
          (unless settled
            (unwind-protect
                (funcall callback response owner)
              (finish))))
         (fail
          (response reason)
          (unless settled
            (unwind-protect
                (funcall handle-error response reason)
              (finish)))))
      (condition-case err
          (progn
            (setq request-token
                  (qq-api-call action params #'succeed #'fail))
            (cond
             (settled nil)
             (request-token
              (puthash request-token #'finish qq-api--request-finalizers))
             (t
              ;; A transport should invoke ERRBACK when it cannot dispatch.
              ;; Still settle defensively when a test double or alternate
              ;; transport returns nil without doing so.
              (finish)))
            request-token)
        (error
         (finish)
         (signal (car err) (cdr err)))))))

(defun qq-api-cancel-request (request-token)
  "Cancel local callback ownership for REQUEST-TOKEN."
  (cond
   ((qq-api--snapshot-subscription-p request-token)
    (qq-api--snapshot-cancel-subscription request-token))
   (request-token
    (unwind-protect
        (qq-transport-cancel request-token)
      ;; Cancellation is also an API ownership boundary.  Settle a registered
      ;; materialization window even if the transport entry raced to absence.
      (qq-api--run-request-finalizer request-token)))))

(defun qq-api-message-id-p (value)
  "Return non-nil when VALUE is a canonical NT message snowflake.

The hard-cut protocol represents `message_id' as an original, non-empty
decimal string.  In particular, a number is never accepted and reformatted:
by the time Emacs or JavaScript has represented a snowflake numerically its
low bits may already have been lost."
  (qq-protocol-message-id-p value))

(defun qq-api-non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value)
       (> (length value) 0)))

(defun qq-api-resource-id-p (value)
  "Return non-nil when VALUE is an opaque native resource_id string."
  (qq-api-non-empty-string-p value))

(defun qq-api-user-id-p (value)
  "Return non-nil when VALUE is a canonical QQ sender UIN string."
  (and (stringp value)
       (string-match-p "\\`[0-9]+\\'" value)))

(defun qq-api-group-id-p (value)
  "Return non-nil when VALUE is a canonical QQ group-code string."
  (qq-protocol-group-uin-p value))

(defconst qq-api--uint32-max #xffffffff
  "Largest exact unsigned 32-bit integer accepted by native directory data.")

(defun qq-api--uint32-p (value)
  "Return non-nil when VALUE is an exact unsigned 32-bit integer."
  (and (integerp value)
       (<= 0 value qq-api--uint32-max)))

(defun qq-api-entry-id-p (value)
  "Return non-nil when VALUE is a canonical native snapshot entry id."
  (and (stringp value)
       (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)*\\'" value)))

(defun qq-api--validate-identity (predicate value field context protocol-p)
  "Validate VALUE with PREDICATE for FIELD used in CONTEXT.

Signal `error' for malformed protocol data when PROTOCOL-P is non-nil;
otherwise signal `user-error' for malformed caller input.  Return VALUE
unchanged so callers cannot accidentally canonicalize a lossy numeric id."
  (unless (funcall predicate value)
    (signal (if protocol-p 'error 'user-error)
            (list (format "qq: %s requires %s as an original string, got %S"
                          (or context "protocol input") field value))))
  value)

(defun qq-api-validate-message-id (value &optional context protocol-p)
  "Return canonical message-id VALUE or signal an identity error.

CONTEXT is included in the diagnostic.  PROTOCOL-P selects a protocol
`error'; otherwise invalid user/API input signals `user-error'."
  (qq-api--validate-identity #'qq-api-message-id-p value "decimal message_id"
                             context protocol-p))

(defun qq-api-validate-resource-id (value &optional context protocol-p)
  "Return canonical resource_id VALUE or signal an identity error.

CONTEXT is included in the diagnostic.  PROTOCOL-P selects a protocol
`error'; otherwise invalid user/API input signals `user-error'."
  (qq-api--validate-identity #'qq-api-resource-id-p value
                             "non-empty resource_id"
                             context protocol-p))

(defun qq-api--single-alist-p (value)
  "Return non-nil when VALUE is one symbol-keyed alist object."
  (and (consp value)
       (proper-list-p value)
       (cl-every (lambda (cell)
                   (and (consp cell) (symbolp (car cell))))
                 value)))

(defun qq-api--finite-number-p (value)
  "Return non-nil when VALUE is a finite JSON number."
  (and (numberp value)
       (or (integerp value)
           (and (floatp value)
                (not (isnan value))
                (not (string-match-p
                      "INF" (upcase (format "%S" value))))))))

(defun qq-api--validate-string-fields
    (object fields context protocol-p)
  "Require every present member of FIELDS in OBJECT to be a string."
  (dolist (key fields)
    (when (and (assq key object)
               (not (stringp (alist-get key object))))
      (qq-api--signal-schema-error
       protocol-p "qq: %s.%s must be a string" context key))))

(defun qq-api--validate-native-file-payload
    (payload context protocol-p &optional image-p)
  "Validate common native file PAYLOAD, with image fields when IMAGE-P."
  (let ((optional
         (append '(file_id path url name thumb file_size)
                 (when image-p '(summary sub_type)))))
    (unless (qq-api--exact-object-keys-p payload '(file) optional)
      (qq-api--signal-schema-error
       protocol-p "qq: %s file payload has invalid fields" context))
    (unless (stringp (alist-get 'file payload))
      (qq-api--signal-schema-error
       protocol-p "qq: %s.file must be a string" context))
    (qq-api--validate-string-fields
     payload '(file_id path url name thumb) context protocol-p)
    (when (assq 'file_size payload)
      (let ((size (alist-get 'file_size payload)))
        (unless (or (stringp size) (qq-api--finite-number-p size))
          (qq-api--signal-schema-error
           protocol-p "qq: %s.file_size must be a string or finite number"
           context))))
    (when image-p
      (qq-api--validate-string-fields payload '(summary) context protocol-p)
      (when (and (assq 'sub_type payload)
                 (not (qq-api--finite-number-p
                       (alist-get 'sub_type payload))))
        (qq-api--signal-schema-error
         protocol-p "qq: %s.sub_type must be a finite number" context)))))

(defun qq-api--exact-object-keys-p (object required &optional optional)
  "Return non-nil when OBJECT has exactly REQUIRED and OPTIONAL keys."
  ;; `json-parse-buffer' represents an empty JSON object as nil when objects
  ;; are requested as alists.  Accept that representation only when the
  ;; schema has no required fields (for example an empty presentation).
  (when (or (qq-api--single-alist-p object)
            (and (null object) (null required)))
    (let ((keys (mapcar #'car object))
          (allowed (append required optional)))
      (and (= (length keys)
              (length (delete-dups (copy-sequence keys))))
           (cl-every (lambda (key) (assq key object)) required)
           (null (seq-difference keys allowed))))))

(defun qq-api--json-safe-value-p (value &optional seen)
  "Return non-nil when VALUE is a finite JSON-safe decoded value.

SEEN guards against cyclic Lisp containers, which cannot originate in JSON
and must not be retained as an `unsupported.raw' diagnostic payload."
  (cond
   ((or (null value) (eq value t) (eq value :false)
        (stringp value) (qq-api--finite-number-p value))
    t)
   ((or (vectorp value) (and (consp value) (proper-list-p value)))
    (unless (memq value seen)
      (let ((seen (cons value seen)))
        (if (vectorp value)
            (cl-every (lambda (item)
                        (qq-api--json-safe-value-p item seen))
                      (append value nil))
          (if (and (consp (car value))
                   (cl-every (lambda (cell)
                               (and (consp cell)
                                    (or (symbolp (car cell))
                                        (stringp (car cell)))))
                             value))
              (cl-every (lambda (cell)
                          (qq-api--json-safe-value-p (cdr cell) seen))
                        value)
            (cl-every (lambda (item)
                        (qq-api--json-safe-value-p item seen))
                      value))))))
   (t nil)))

(defun qq-api--signal-schema-error (protocol-p format-string &rest args)
  "Signal a schema error described by FORMAT-STRING and ARGS."
  (signal (if protocol-p 'error 'user-error)
          (list (apply #'format format-string args))))

(defun qq-api-validate-chat-locator (chat &optional context protocol-p)
  "Validate and return a copied fork-native CHAT locator."
  (let ((context (or context "chat locator")))
    (unless (qq-api--single-alist-p chat)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be an object" context))
    (pcase (alist-get 'kind chat)
      ("group"
       (unless (qq-api--exact-object-keys-p chat '(kind group_id))
         (qq-api--signal-schema-error
          protocol-p "qq: %s group locator has invalid fields" context))
       (unless (qq-api-group-id-p (alist-get 'group_id chat))
         (qq-api--signal-schema-error
          protocol-p "qq: %s group_id must be a canonical uint32 group UIN"
          context)))
      ("private"
       (unless (qq-api--exact-object-keys-p chat '(kind user_id))
         (qq-api--signal-schema-error
          protocol-p "qq: %s private locator has invalid fields" context))
       (unless (qq-api-user-id-p (alist-get 'user_id chat))
         (qq-api--signal-schema-error
          protocol-p "qq: %s user_id must be a decimal string" context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind chat))))
    (copy-tree chat)))

(defun qq-api-validate-forward-source (source &optional context protocol-p)
  "Validate and return a copied fork-native query SOURCE union."
  (let ((context (or context "forward source")))
    (unless (qq-api--single-alist-p source)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be a source object" context))
    (pcase (alist-get 'kind source)
      ("message"
       (unless (qq-api--exact-object-keys-p
                source '(kind message_id chat))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s message source requires kind, message_id, and chat"
          context))
       (qq-api-validate-message-id
        (alist-get 'message_id source) context protocol-p)
       (qq-api-validate-chat-locator
        (alist-get 'chat source) (format "%s chat" context) protocol-p))
      ("resource"
       (unless (qq-api--exact-object-keys-p
                source '(kind resource_id))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s resource source requires only kind and resource_id"
          context))
       (qq-api-validate-resource-id
        (alist-get 'resource_id source) context protocol-p))
      ("context"
       (unless (qq-api--exact-object-keys-p
                source '(kind peer root_message_id parent_message_id))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s context source requires only kind, peer, "
                  "root_message_id, and parent_message_id")
          context))
       (let ((peer (alist-get 'peer source)))
         (unless (qq-api--exact-object-keys-p
                  peer '(chat_type peer_uid guild_id))
           (qq-api--signal-schema-error
            protocol-p
            (concat "qq: %s context peer requires only chat_type, "
                    "peer_uid, and guild_id")
            context))
         (let ((chat-type (alist-get 'chat_type peer)))
           (unless (and (qq-api--finite-number-p chat-type)
                        (> chat-type 0)
                        (= chat-type (truncate chat-type)))
             (qq-api--signal-schema-error
              protocol-p
              "qq: %s context peer.chat_type must be a positive integer"
              context)))
         (unless (qq-api-non-empty-string-p (alist-get 'peer_uid peer))
           (qq-api--signal-schema-error
            protocol-p
            "qq: %s context peer.peer_uid must be a non-empty string"
            context))
         (unless (stringp (alist-get 'guild_id peer))
           (qq-api--signal-schema-error
            protocol-p "qq: %s context peer.guild_id must be a string"
            context)))
       (dolist (key '(root_message_id parent_message_id))
         (let ((message-id (alist-get key source)))
           (unless (and (stringp message-id)
                        (string-match-p "\\`[1-9][0-9]*\\'" message-id))
             (qq-api--signal-schema-error
              protocol-p
              (concat "qq: %s context %s must be a positive decimal string "
                      "without leading zeros")
              context key)))))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind source))))
    (let ((copy (copy-tree source)))
      (when (equal (alist-get 'kind copy) "context")
        (setf (alist-get 'chat_type (alist-get 'peer copy))
              (truncate (alist-get 'chat_type (alist-get 'peer copy)))))
      copy)))

(defun qq-api-validate-message-reference
    (reference &optional context protocol-p)
  "Validate and copy a locator-qualified message REFERENCE."
  (let ((context (or context "message reference")))
    (unless (qq-api--exact-object-keys-p
             reference '(message_id chat))
      (qq-api--signal-schema-error
       protocol-p
       "qq: %s requires only message_id and chat" context))
    (qq-api-validate-message-id
     (alist-get 'message_id reference) context protocol-p)
    (qq-api-validate-chat-locator
     (alist-get 'chat reference) (format "%s chat" context) protocol-p)
    (copy-tree reference)))

(defun qq-api--validate-native-peer (peer context protocol-p)
  "Validate and copy exact Linux QQ PEER for CONTEXT."
  (unless (qq-api--exact-object-keys-p
           peer '(chat_type peer_uid guild_id))
    (qq-api--signal-schema-error
     protocol-p
     (concat "qq: %s peer requires only chat_type, peer_uid, and "
             "guild_id")
     context))
  (let ((chat-type (alist-get 'chat_type peer)))
    (unless (and (integerp chat-type) (> chat-type 0))
      (qq-api--signal-schema-error
       protocol-p "qq: %s peer.chat_type must be a positive integer"
       context)))
  (unless (qq-api-non-empty-string-p (alist-get 'peer_uid peer))
    (qq-api--signal-schema-error
     protocol-p "qq: %s peer.peer_uid must be a non-empty string"
     context))
  (unless (stringp (alist-get 'guild_id peer))
    (qq-api--signal-schema-error
     protocol-p "qq: %s peer.guild_id must be a string" context))
  (copy-tree peer))

(defun qq-api-validate-video-resolver
    (resolver &optional context protocol-p)
  "Validate and copy exact fork-native video RESOLVER.

The two locator domains are deliberately disjoint: live messages use their
own native peer/message/element identity, while forward snapshots use their
own peer/file UUID.  No parent id or generic file token is accepted."
  (let ((context (or context "video resolver")))
    (unless (qq-api--single-alist-p resolver)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be an object" context))
    (pcase (alist-get 'kind resolver)
      ("message"
       (unless (qq-api--exact-object-keys-p
                resolver '(kind peer message_id element_id))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s message resolver requires only kind, peer, "
                  "message_id, and element_id")
          context))
       (qq-api--validate-native-peer
        (alist-get 'peer resolver) context protocol-p)
       (dolist (key '(message_id element_id))
         (let ((value (alist-get key resolver)))
           (unless (and (stringp value)
                        (string-match-p "\\`[1-9][0-9]*\\'" value))
             (qq-api--signal-schema-error
              protocol-p
              "qq: %s.%s must be a positive decimal identity string"
              context key)))))
      ("snapshot"
       (unless (qq-api--exact-object-keys-p
                resolver '(kind peer file_uuid))
         (qq-api--signal-schema-error
          protocol-p
          (concat "qq: %s snapshot resolver requires only kind, peer, "
                  "and file_uuid")
          context))
       (qq-api--validate-native-peer
        (alist-get 'peer resolver) context protocol-p)
       (unless (qq-api-non-empty-string-p (alist-get 'file_uuid resolver))
         (qq-api--signal-schema-error
          protocol-p "qq: %s.file_uuid must be a non-empty string"
          context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind resolver))))
    (copy-tree resolver)))

(defun qq-api--validate-native-video-remote
    (remote context protocol-p &optional allow-resolvable-p)
  "Validate native video REMOTE discriminant for CONTEXT.

ALLOW-RESOLVABLE-P permits the wire capability carried by forward video
segments.  A resolve action result itself must be terminal or available."
  (unless (qq-api--single-alist-p remote)
    (qq-api--signal-schema-error
     protocol-p "qq: %s video remote must be an object" context))
  (pcase (alist-get 'state remote)
    ("available"
     (unless (and (qq-api--exact-object-keys-p remote '(state url))
                  (qq-api-non-empty-string-p (alist-get 'url remote)))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s available video remote requires only non-empty string url"
        context)))
    ("resolvable"
     (unless allow-resolvable-p
       (qq-api--signal-schema-error
        protocol-p "qq: %s resolve result cannot remain resolvable" context))
     (unless (qq-api--exact-object-keys-p remote '(state resolver))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s resolvable video remote requires only state and resolver"
        context))
     (qq-api-validate-video-resolver
      (alist-get 'resolver remote) (format "%s resolver" context) protocol-p))
    ((or "expired" "unavailable" "unresolved")
     (unless (qq-api--exact-object-keys-p remote '(state))
       (qq-api--signal-schema-error
        protocol-p
        "qq: %s non-available video remote may contain only state"
        context)))
    (_
     (qq-api--signal-schema-error
      protocol-p "qq: %s video remote has invalid state %S"
      context (alist-get 'state remote))))
  remote)

(defun qq-api--validate-native-forward-segment (segment context protocol-p)
  "Validate one fork-native forward SEGMENT."
  (unless (qq-api--exact-object-keys-p segment '(kind payload))
    (qq-api--signal-schema-error
     protocol-p "qq: %s must contain only kind and payload" context))
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (unless (and (stringp kind) (not (string-empty-p kind)))
      (qq-api--signal-schema-error
       protocol-p "qq: %s kind must be a non-empty string" context))
    (unless (member
             kind
             '("text" "at" "reply" "image" "file" "record" "face"
               "mface" "mail" "music" "poke" "dice" "rps" "contact"
               "location" "json" "card" "xml" "markdown" "miniapp"
               "onlinefile" "flashtransfer" "video" "forward"
               "forward-card" "unsupported"))
      (qq-api--signal-schema-error
       protocol-p "qq: %s has unsupported native kind %S" context kind))
    (pcase kind
      ("text"
       (unless (and (qq-api--exact-object-keys-p payload '(text))
                    (stringp (alist-get 'text payload)))
         (qq-api--signal-schema-error
          protocol-p "qq: %s text payload is invalid" context)))
      ("at"
       (unless (qq-api--exact-object-keys-p payload '(qq) '(name))
         (qq-api--signal-schema-error
          protocol-p "qq: %s at payload has invalid fields" context))
       (unless (stringp (alist-get 'qq payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s at.qq must be a string" context))
       (qq-api--validate-string-fields payload '(name) context protocol-p))
      ("image"
       (qq-api--validate-native-file-payload
        payload context protocol-p t))
      ((or "file" "record")
       (qq-api--validate-native-file-payload
        payload context protocol-p nil))
      ("face"
       (unless (qq-api--exact-object-keys-p
                payload '(id)
                '(description face_type sticker_id sticker_pack_id
                  sticker_type resultId chainCount))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face payload has invalid fields" context))
       (unless (stringp (alist-get 'id payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face.id must be a string" context))
       (qq-api--validate-string-fields
        payload '(description sticker_id sticker_pack_id resultId)
        context protocol-p)
       (dolist (key '(face_type sticker_type))
         (when (and (assq key payload)
                    (not (qq-api--finite-number-p (alist-get key payload))))
           (qq-api--signal-schema-error
            protocol-p "qq: %s face.%s must be a finite number"
            context key)))
       (when (and (assq 'chainCount payload)
                  (not (qq-api--finite-number-p
                        (alist-get 'chainCount payload))))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face.chainCount must be a finite number"
          context)))
      ("mface"
       (unless (qq-api--exact-object-keys-p
                payload '(emoji_package_id emoji_id key summary))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mface payload has invalid fields" context))
       (unless (qq-api--finite-number-p
                (alist-get 'emoji_package_id payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mface.emoji_package_id must be a finite number"
          context))
       (qq-api--validate-string-fields
        payload '(emoji_id key summary) context protocol-p))
      ("mail"
       (unless (qq-api--exact-object-keys-p
                payload nil '(sender subject content prompt detail url))
         (qq-api--signal-schema-error
          protocol-p "qq: %s mail payload has invalid fields" context))
       (qq-api--validate-string-fields
        payload '(sender subject content prompt detail url)
        context protocol-p))
      ("video"
       (unless (qq-api--exact-object-keys-p
                payload '(file remote)
                '(local_path size name thumb))
         (qq-api--signal-schema-error
          protocol-p "qq: %s video payload has invalid fields" context))
       (unless (qq-api-non-empty-string-p (alist-get 'file payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s video file must be a non-empty string" context))
       (when (assq 'local_path payload)
         (unless (stringp (alist-get 'local_path payload))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video local_path must be a string" context)))
       (when (assq 'size payload)
         (unless (and (integerp (alist-get 'size payload))
                      (>= (alist-get 'size payload) 0))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video size must be a non-negative integer"
            context)))
       (dolist (key '(name thumb))
         (when (and (assq key payload)
                    (not (stringp (alist-get key payload))))
           (qq-api--signal-schema-error
            protocol-p "qq: %s video %s must be a string" context key)))
       (qq-api--validate-native-video-remote
        (alist-get 'remote payload) context protocol-p t))
      ("reply"
       (unless (qq-api--exact-object-keys-p payload '(target))
         (qq-api--signal-schema-error
          protocol-p "qq: %s reply payload requires only target" context))
       (let ((target (alist-get 'target payload)))
         (pcase (and (qq-api--single-alist-p target)
                     (alist-get 'kind target))
           ("entry"
            (unless (and (qq-api--exact-object-keys-p
                          target '(kind entry_id))
                         (qq-api-entry-id-p
                          (alist-get 'entry_id target)))
              (qq-api--signal-schema-error
               protocol-p "qq: %s reply entry target is invalid" context)))
           ("unresolved"
            (unless (qq-api--exact-object-keys-p
                     target '(kind) '(message_id))
              (qq-api--signal-schema-error
               protocol-p "qq: %s unresolved reply target is invalid" context))
            (when (assq 'message_id target)
              (qq-api-validate-message-id
               (alist-get 'message_id target) context protocol-p)))
           (_
            (qq-api--signal-schema-error
             protocol-p "qq: %s reply target has invalid kind" context)))))
      ("forward"
       (unless (qq-api--exact-object-keys-p payload '(content))
         (qq-api--signal-schema-error
          protocol-p "qq: %s forward payload requires only content" context))
       (let ((content (alist-get 'content payload)))
         (pcase (and (qq-api--single-alist-p content)
                     (alist-get 'kind content))
           ("inline"
            (unless (qq-api--exact-object-keys-p
                     content '(kind messages))
              (qq-api--signal-schema-error
               protocol-p "qq: %s inline content has invalid fields" context))
            (qq-api-validate-native-forward-messages
             (alist-get 'messages content)
             (format "%s inline messages" context) protocol-p))
           ("remote"
            (unless (qq-api--exact-object-keys-p
                     content '(kind reference))
              (qq-api--signal-schema-error
               protocol-p "qq: %s remote content has invalid fields" context))
            (qq-api-validate-forward-source
             (alist-get 'reference content)
             (format "%s reference" context) protocol-p))
           (_
            (qq-api--signal-schema-error
             protocol-p "qq: %s forward content has invalid kind" context)))))
      ("forward-card"
       (unless (qq-api--exact-object-keys-p
                payload '(reference presentation))
         (qq-api--signal-schema-error
          protocol-p
          "qq: %s forward-card requires only reference and presentation"
          context))
       (let ((reference
              (qq-api-validate-forward-source
               (alist-get 'reference payload)
               (format "%s reference" context) protocol-p)))
         (when (equal (alist-get 'kind reference) "message")
           (qq-api--signal-schema-error
            protocol-p
            "qq: %s forward-card reference must be resource or context"
            context)))
       (let ((presentation (alist-get 'presentation payload)))
         (unless (qq-api--exact-object-keys-p
                  presentation nil
                  '(source title content summary prompt))
           (qq-api--signal-schema-error
            protocol-p "qq: %s presentation has invalid fields" context))
         (dolist (key '(source title content summary prompt))
           (when (and (assq key presentation)
                      (not (stringp (alist-get key presentation))))
             (qq-api--signal-schema-error
              protocol-p "qq: %s presentation.%s must be a string"
              context key)))))
      ("unsupported"
       (unless (qq-api--exact-object-keys-p
                payload '(native_keys summary) '(raw))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported payload has invalid fields" context))
       (let ((native-keys (alist-get 'native_keys payload)))
         (unless (and (or (listp native-keys) (vectorp native-keys))
                      (cl-every #'stringp
                                (if (vectorp native-keys)
                                    (append native-keys nil)
                                  native-keys)))
           (qq-api--signal-schema-error
            protocol-p "qq: %s native_keys must be a string array" context)))
       (unless (stringp (alist-get 'summary payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported summary must be a string" context))
       (when (and (assq 'raw payload)
                  (not (qq-api--json-safe-value-p
                        (alist-get 'raw payload))))
         (qq-api--signal-schema-error
          protocol-p "qq: %s unsupported raw must be JSON-safe" context)))
      ((or "music" "poke" "dice" "rps" "contact" "location"
           "json" "card" "xml" "markdown" "miniapp" "onlinefile"
           "flashtransfer")
       (unless (or (null payload) (qq-api--single-alist-p payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s passthrough payload must be an object" context))
       (unless (qq-api--json-safe-value-p payload)
         (qq-api--signal-schema-error
          protocol-p "qq: %s passthrough payload must be JSON-safe" context))))
    (copy-tree segment)))

(defun qq-api--validate-native-forward-message (message context protocol-p)
  "Validate one fork-native forward MESSAGE."
  (unless (qq-api--exact-object-keys-p
           message '(entry_id state sent_at sender origin segments)
           '(message_id))
    (qq-api--signal-schema-error
     protocol-p "qq: %s has invalid native message fields" context))
  (unless (qq-api-entry-id-p (alist-get 'entry_id message))
    (qq-api--signal-schema-error
     protocol-p "qq: %s entry_id must be a dotted decimal path" context))
  (when (assq 'message_id message)
    (qq-api-validate-message-id
     (alist-get 'message_id message) context protocol-p))
  (unless (and (integerp (alist-get 'sent_at message))
               (>= (alist-get 'sent_at message) 0))
    (qq-api--signal-schema-error
     protocol-p "qq: %s sent_at must be a non-negative integer" context))
  (unless (member (alist-get 'state message) '("live" "recalled"))
    (qq-api--signal-schema-error
     protocol-p "qq: %s state must be live or recalled" context))
  (let ((sender (alist-get 'sender message)))
    (pcase (and (qq-api--single-alist-p sender) (alist-get 'kind sender))
      ("user"
       (unless (qq-api--exact-object-keys-p sender '(kind user_id name))
         (qq-api--signal-schema-error
          protocol-p "qq: %s user sender has invalid fields" context))
       (unless (qq-api-user-id-p (alist-get 'user_id sender))
         (qq-api--signal-schema-error
          protocol-p "qq: %s sender.user_id must be decimal" context))
       (unless (stringp (alist-get 'name sender))
         (qq-api--signal-schema-error
          protocol-p "qq: %s sender.name must be a string" context)))
      ("anonymous"
       (unless (and (qq-api--exact-object-keys-p sender '(kind name))
                    (stringp (alist-get 'name sender)))
         (qq-api--signal-schema-error
          protocol-p "qq: %s anonymous sender is invalid" context)))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s sender has invalid kind" context))))
  (let ((origin (alist-get 'origin message)))
    (if (equal (and (qq-api--single-alist-p origin)
                    (alist-get 'kind origin))
               "unknown")
        (unless (qq-api--exact-object-keys-p origin '(kind))
          (qq-api--signal-schema-error
           protocol-p "qq: %s unknown origin has invalid fields" context))
      (qq-api-validate-chat-locator
       origin (format "%s origin" context) protocol-p)))
  (let ((segments (alist-get 'segments message)))
    (unless (or (listp segments) (vectorp segments))
      (qq-api--signal-schema-error
       protocol-p "qq: %s segments must be an array" context))
    (cl-loop for segment in (if (vectorp segments)
                                (append segments nil)
                              segments)
             for index from 0
             do (qq-api--validate-native-forward-segment
                 segment (format "%s.segments[%d]" context index)
                 protocol-p)))
  (copy-tree message))

(defun qq-api-validate-native-forward-messages
    (messages &optional context protocol-p)
  "Validate and copy a fork-native forward message array."
  (unless (or (listp messages) (vectorp messages))
    (qq-api--signal-schema-error
     protocol-p "qq: %s must be a native message array"
     (or context "forward messages")))
  (let ((items (if (vectorp messages) (append messages nil) messages))
        (context (or context "forward messages"))
        (entry-ids (make-hash-table :test #'equal)))
    (cl-loop for message in items
             for index from 0
             do (progn
                  (qq-api--validate-native-forward-message
                   message (format "%s[%d]" context index) protocol-p)
                  (let ((entry-id (alist-get 'entry_id message)))
                    (when (gethash entry-id entry-ids)
                      (qq-api--signal-schema-error
                       protocol-p "qq: %s has duplicate entry_id %S"
                       context entry-id))
                    (puthash entry-id t entry-ids))))
    (copy-tree items)))
(defun qq-api-refresh-status ()
  "Refresh runtime status from NapCat."
  (interactive)
  (qq-api-call
   "get_status"
   nil
   (lambda (response)
     (qq-state-set-status (qq-api--response-data response)))))

(defun qq-api-refresh-login-info (&optional callback errback)
  "Refresh login info from NapCat, then call CALLBACK with it."
  (interactive)
  (qq-api-call
   "get_login_info"
   nil
   (lambda (response)
     (let ((info (qq-api--response-data response)))
       (qq-state-set-self-info info)
       (when callback (funcall callback info))))
   errback))

(defun qq-api-refresh-recent-contacts (&optional callback errback)
  "Refresh recent contacts, then call CALLBACK with the contact list."
  (interactive)
  (let ((read-token (qq-api--next-read-observation-token))
        (summary-token (qq-state-session-summary-observation-start)))
    (qq-api-call
     "get_recent_contact"
     `((count . ,(max 1 qq-recent-contact-count)))
     (lambda (response)
       (let ((contacts (qq-api--response-data response)))
         (qq-state-apply-recent-contacts
         contacts
          (lambda (session-key)
            (qq-api--accept-read-observation-p session-key read-token))
          summary-token)
         (when callback (funcall callback contacts))))
     errback)))

(defun qq-api--validate-friend-categories-snapshot (data)
  "Validate and copy exact native friend-category snapshot DATA."
  (unless (qq-api--exact-object-keys-p data '(categories))
    (error "qq: emacs_get_friend_categories returned an invalid object"))
  (let ((categories (alist-get 'categories data))
        (seen-categories (make-hash-table :test #'eql))
        (seen-friends (make-hash-table :test #'equal)))
    (unless (listp categories)
      (error "qq: emacs_get_friend_categories.categories must be an array"))
    (cl-loop
     for category in categories
     for category-index from 0
     do
     (let ((context (format "emacs_get_friend_categories.categories[%d]"
                            category-index)))
       (unless (qq-api--exact-object-keys-p
                category '(category_id sort_id name online_count friends))
         (error "qq: %s has invalid fields" context))
       (dolist (key '(category_id sort_id online_count))
         (unless (qq-api--uint32-p (alist-get key category))
           (error "qq: %s.%s must be an unsigned 32-bit integer"
                  context key)))
       (unless (stringp (alist-get 'name category))
         (error "qq: %s.name must be a string" context))
       (let ((category-id (alist-get 'category_id category))
             (friends (alist-get 'friends category)))
         (when (gethash category-id seen-categories)
           (error "qq: friend snapshot duplicated category_id %s" category-id))
         (puthash category-id t seen-categories)
         (unless (listp friends)
           (error "qq: %s.friends must be an array" context))
         (cl-loop
          for friend in friends
          for friend-index from 0
          do
          (let ((friend-context (format "%s.friends[%d]" context friend-index)))
            (unless (qq-api--exact-object-keys-p
                     friend '(user_id nickname remark))
              (error "qq: %s has invalid fields" friend-context))
            (let ((user-id (alist-get 'user_id friend)))
              (unless (qq-protocol--nonzero-decimal-string-p user-id)
                (error "qq: %s.user_id must be an original nonzero decimal string"
                       friend-context))
              (when (gethash user-id seen-friends)
                (error "qq: friend snapshot duplicated user_id %s" user-id))
              (puthash user-id t seen-friends))
            (unless (stringp (alist-get 'nickname friend))
              (error "qq: %s.nickname must be a string" friend-context))
            (unless (or (null (alist-get 'remark friend))
                        (stringp (alist-get 'remark friend)))
              (error "qq: %s.remark must be a string or null"
                     friend-context)))))))
    (copy-tree categories)))

(defun qq-api--validate-joined-groups-snapshot (data)
  "Validate exact native joined-group snapshot DATA and normalize its names."
  (unless (qq-api--exact-object-keys-p data '(groups))
    (error "qq: emacs_get_joined_groups returned an invalid object"))
  (let ((groups (alist-get 'groups data))
        (seen-groups (make-hash-table :test #'equal)))
    (unless (listp groups)
      (error "qq: emacs_get_joined_groups.groups must be an array"))
    (cl-loop
     for group in groups
     for index from 0
     collect
     (let ((context (format "emacs_get_joined_groups.groups[%d]" index)))
       (unless (qq-api--exact-object-keys-p
                group '(group_id name remark member_count max_member_count
                                 pinned self_permission))
         (error "qq: %s has invalid fields" context))
       (let ((group-id (alist-get 'group_id group)))
         (unless (qq-protocol-group-uin-p group-id)
           (error "qq: %s.group_id must be a canonical uint32 group UIN"
                  context))
         (when (gethash group-id seen-groups)
           (error "qq: joined-group snapshot duplicated group_id %s" group-id))
         (puthash group-id t seen-groups))
       (unless (stringp (alist-get 'name group))
         (error "qq: %s.name must be a string" context))
       (unless (or (null (alist-get 'remark group))
                   (stringp (alist-get 'remark group)))
         (error "qq: %s.remark must be a string or null" context))
       (dolist (key '(member_count max_member_count))
         (unless (qq-api--uint32-p (alist-get key group))
           (error "qq: %s.%s must be an unsigned 32-bit integer" context key)))
       (unless (memq (alist-get 'pinned group) '(t :false))
         (error "qq: %s.pinned must be a boolean" context))
       (unless (member (alist-get 'self_permission group)
                       '("member" "admin" "owner"))
         (error "qq: %s.self_permission is invalid" context))
       `((group_id . ,(alist-get 'group_id group))
         (group_name . ,(alist-get 'name group))
         (group_remark . ,(alist-get 'remark group))
         (member_count . ,(alist-get 'member_count group))
         (max_member_count . ,(alist-get 'max_member_count group))
         (pinned . ,(alist-get 'pinned group))
         (self_permission . ,(alist-get 'self_permission group)))))))

(defun qq-api--snapshot-live-subscribers (request)
  "Return active subscribers owned by authoritative REQUEST."
  (seq-filter #'qq-api--snapshot-subscription-active-p
              (qq-api--snapshot-request-subscribers request)))

(defun qq-api--snapshot-call-subscriber (subscriber outcome value response reason)
  "Settle SUBSCRIBER once with authoritative snapshot OUTCOME."
  (when (qq-api--snapshot-subscription-active-p subscriber)
    (setf (qq-api--snapshot-subscription-active-p subscriber) nil)
    (condition-case error-data
        (if (eq outcome 'success)
            (when-let* ((callback
                         (qq-api--snapshot-subscription-callback subscriber)))
              (funcall callback value))
          (funcall (qq-api--snapshot-subscription-errback subscriber)
                   response reason))
      (error
       (message "qq: directory snapshot subscriber failed: %s"
                (error-message-string error-data))))))

(defun qq-api--snapshot-promote-queued (request)
  "Finish REQUEST ownership and start its resource's queued request."
  (let ((resource (qq-api--snapshot-request-resource request)))
    (when (eq (gethash resource qq-api--snapshot-active) request)
      (remhash resource qq-api--snapshot-active))
    (when-let* ((queued (gethash resource qq-api--snapshot-queued)))
      (remhash resource qq-api--snapshot-queued)
      (when (qq-api--snapshot-live-subscribers queued)
        (qq-api--snapshot-start queued)))))

(defun qq-api--snapshot-complete (request outcome value response reason)
  "Complete authoritative REQUEST with OUTCOME, VALUE, RESPONSE and REASON."
  (unless (qq-api--snapshot-request-settled-p request)
    (setf (qq-api--snapshot-request-settled-p request) t)
    (let ((subscribers
           (nreverse (qq-api--snapshot-request-subscribers request))))
      (setf (qq-api--snapshot-request-subscribers request) nil)
      (dolist (subscriber subscribers)
        (qq-api--snapshot-call-subscriber
         subscriber outcome value response reason)))
    (qq-api--snapshot-promote-queued request)))

(defun qq-api--snapshot-succeed (request response)
  "Validate, apply and publish successful authoritative REQUEST RESPONSE."
  (if (null (qq-api--snapshot-live-subscribers request))
      (qq-api--snapshot-complete request 'success nil response nil)
    (condition-case error-data
        (let ((value
               (funcall (qq-api--snapshot-request-validator request)
                        (qq-api--response-data response))))
          (funcall (qq-api--snapshot-request-apply-function request) value)
          (qq-api--snapshot-complete request 'success value response nil))
      (error
       (qq-api--snapshot-complete
        request 'error nil response (error-message-string error-data))))))

(defun qq-api--snapshot-fail (request response reason)
  "Publish transport failure REASON for authoritative REQUEST."
  (qq-api--snapshot-complete request 'error nil response reason))

(defun qq-api--snapshot-start (request)
  "Start authoritative directory REQUEST as its resource's sole flight."
  (let ((resource (qq-api--snapshot-request-resource request)))
    (puthash resource request qq-api--snapshot-active)
    (condition-case error-data
        (let ((token
               (qq-api-call
                (qq-api--snapshot-request-action request)
                '((refresh . t))
                (apply-partially #'qq-api--snapshot-succeed request)
                (apply-partially #'qq-api--snapshot-fail request))))
          (cond
           ((qq-api--snapshot-request-settled-p request))
           (token
            (setf (qq-api--snapshot-request-transport-token request) token))
           (t
            (qq-api--snapshot-complete
             request 'error nil nil
             "transport did not return a directory snapshot request token"))))
      (error
       (qq-api--snapshot-complete
        request 'error nil nil (error-message-string error-data))))))

(defun qq-api--snapshot-subscribe
    (resource action validator apply-function callback errback)
  "Subscribe to a serialized authoritative RESOURCE snapshot refresh."
  (let* ((active (gethash resource qq-api--snapshot-active))
         (request
          (if active
              (or (gethash resource qq-api--snapshot-queued)
                  (let ((queued
                         (qq-api--snapshot-request-create
                          :resource resource
                          :action action
                          :validator validator
                          :apply-function apply-function)))
                    (puthash resource queued qq-api--snapshot-queued)
                    queued))
            (qq-api--snapshot-request-create
             :resource resource
             :action action
             :validator validator
             :apply-function apply-function)))
         (subscriber
          (qq-api--snapshot-subscription-create
           :request request
           :callback callback
           :errback (or errback #'qq-api--default-error)
           :active-p t)))
    (push subscriber (qq-api--snapshot-request-subscribers request))
    (unless active
      (qq-api--snapshot-start request))
    subscriber))

(defun qq-api--snapshot-cancel-orphaned-active (resource)
  "Cancel RESOURCE's active request when no subscriber or queue owns it."
  (when-let* ((active (gethash resource qq-api--snapshot-active))
              ((null (qq-api--snapshot-live-subscribers active)))
              ((null (gethash resource qq-api--snapshot-queued))))
    (setf (qq-api--snapshot-request-settled-p active) t)
    (remhash resource qq-api--snapshot-active)
    (when-let* ((token (qq-api--snapshot-request-transport-token active)))
      (qq-transport-cancel token))))

(defun qq-api--snapshot-cancel-subscription (subscriber)
  "Cancel only authoritative snapshot SUBSCRIBER's callback ownership."
  (when (qq-api--snapshot-subscription-active-p subscriber)
    (setf (qq-api--snapshot-subscription-active-p subscriber) nil)
    (let* ((request (qq-api--snapshot-subscription-request subscriber))
           (resource (qq-api--snapshot-request-resource request))
           (live (qq-api--snapshot-live-subscribers request)))
      (when (and (eq (gethash resource qq-api--snapshot-queued) request)
                 (not live))
        (remhash resource qq-api--snapshot-queued))
      (qq-api--snapshot-cancel-orphaned-active resource)
      t)))

(defun qq-api-refresh-friend-categories (&optional callback errback)
  "Refresh exact native friend categories, then call CALLBACK with them."
  (interactive)
  (qq-api--snapshot-subscribe
   'friend-categories
   "emacs_get_friend_categories"
   #'qq-api--validate-friend-categories-snapshot
   #'qq-state-apply-friend-categories
   callback errback))

(defun qq-api-refresh-joined-groups (&optional callback errback)
  "Refresh exact native joined groups, then call CALLBACK with them."
  (interactive)
  (qq-api--snapshot-subscribe
   'joined-groups
   "emacs_get_joined_groups"
   #'qq-api--validate-joined-groups-snapshot
   #'qq-state-apply-groups
   callback errback))

(defun qq-api-bootstrap ()
  "Run initial bootstrap in dependency order.

Self identity and friend names must exist before recent-contact messages are
normalized; otherwise an arbitrary response order can permanently classify a
self message as incoming or store a weaker sender display name."
  (interactive)
  (qq-api-refresh-status)
  (cl-labels
      ((recent () (qq-api-refresh-recent-contacts))
       (groups ()
         (qq-api-refresh-joined-groups
          (lambda (_groups) (recent))
          (lambda (response reason)
            (qq-api--default-error response reason)
            (recent))))
       (friends ()
         (qq-api-refresh-friend-categories
          (lambda (_friends) (groups))
          (lambda (response reason)
            (qq-api--default-error response reason)
            (groups)))))
    (qq-api-refresh-login-info
     (lambda (_info) (friends))
     (lambda (response reason)
       ;; Friend/group/session data remains useful when login info fails, but
       ;; the deterministic chain is preserved for the next refresh.
       (qq-api--default-error response reason)
       (friends)))))

(defun qq-api-refresh ()
  "Refresh all primary runtime data from NapCat."
  (interactive)
  (qq-api-bootstrap))

(defun qq-api--session-request-params (session-key)
  "Return base request params for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (target-id (alist-get 'target-id identity)))
    (pcase type
      ('group
       `((group_id . ,target-id)))
      ('dataline
       `((chat_type . ,(alist-get 'chat-type identity))
         (peer_uid . ,(alist-get 'peer-uid identity))))
      ('service
       `((chat_type . ,(alist-get 'chat-type identity))
         (peer_uid . ,(alist-get 'peer-uid identity))))
      ('private
       `((user_id . ,target-id)))
      (_
       (user-error "qq: unsupported session key %S" session-key)))))

(defun qq-api--history-exhausted-error-p (response reason)
  "Return non-nil when RESPONSE/REASON means no older history page.

NapCat throws when `message_seq' is unknown or the page is empty
(\"消息…不存在\").  For load-older that is end-of-history, not a hard failure."
  (let ((text (format "%s%s"
                      (or reason "")
                      (or (and (listp response) (alist-get 'message response))
                          (and (listp response) (alist-get 'wording response))
                          ""))))
    (or (string-match-p "不存在" text)
        (string-match-p "not exist" text)
        (string-match-p "no message" text))))

(defun qq-api--session-emacs-locator (session-key)
  "Return the closed Emacs protocol locator for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (target-id (alist-get 'target-id identity)))
    (pcase type
      ('group
       `((kind . "group") (group_id . ,target-id)))
      ('private
       `((kind . "private") (user_id . ,target-id)))
      ('dataline
       `((kind . "dataline")
         (peer_uid . ,(alist-get 'peer-uid identity))
         (variant . ,(alist-get 'variant identity))))
      ('service
       `((kind . "service")
         (peer_uid . ,(alist-get 'peer-uid identity))))
      (_ (error "qq: unsupported Emacs session type %s" type)))))

(defun qq-api--session-emacs-params (session-key)
  "Return native Emacs action params for SESSION-KEY."
  `((chat . ,(qq-api--session-emacs-locator session-key))))

(defun qq-api-session-key-from-locator (locator)
  "Return the unique local session key represented by LOCATOR.

LOCATOR must satisfy the fork's closed `EmacsSessionLocator' union.  Opaque
peer UIDs stay strings and are never interpreted as QQ numbers."
  (setq locator
        (qq-protocol-validate-emacs-session-locator
         locator "Emacs session locator"))
  (pcase (alist-get 'kind locator)
    ("group"
     (qq-state-session-key 'group (alist-get 'group_id locator)))
    ("private"
     (qq-state-session-key 'private (alist-get 'user_id locator)))
    ("dataline"
     (qq-state-session-key 'dataline
                           (alist-get 'peer_uid locator)
                           (alist-get 'variant locator)))
    ("service"
     (qq-state-session-key 'service (alist-get 'peer_uid locator)))
    ;; The validator makes this unreachable.  Keep the branch explicit so a
    ;; future locator kind cannot silently map to the wrong session namespace.
    (_ (error "qq: unsupported Emacs session locator %S" locator))))

(defun qq-api-fetch-session-read-state (session-key &optional callback errback)
  "Fetch the official Linux QQ read position for SESSION-KEY.

NapCat resolves the kernel first-unread sequence to the hard-cut NT snowflake
in `first_unread.message_id'.  CALLBACK receives the validated raw read-state
payload.  Fetching alone does not mutate local unread state: callers that own a
freshness barrier decide whether the response may be applied."
  (let ((handle-error (or errback #'qq-api--default-error)))
    (qq-api-call
     "emacs_get_read_state"
     (qq-api--session-emacs-params session-key)
     (lambda (response)
       (condition-case err
           (let ((read-state
                  (qq-protocol-validate-emacs-read-state
                   (qq-api--response-data response)
                   "emacs_get_read_state response")))
             (when callback
               (funcall callback read-state)))
         (error
          (funcall handle-error response (error-message-string err)))))
     errback)))

(defun qq-api--refresh-session-read-state-after-failure (session-key)
  "Refresh SESSION-KEY after a failed mark-read without guessing counts."
  (let ((token (qq-api--next-read-observation-token)))
    (qq-api-fetch-session-read-state
     session-key
     (lambda (read-state)
       (when (qq-api--accept-read-observation-p session-key token)
         (qq-state-apply-session-read-state session-key read-state))))))

(defun qq-api-fetch-history-page (session-key cursor direction
                                              &optional callback errback count)
  "Fetch one history page for SESSION-KEY at CURSOR in DIRECTION.

DIRECTION is `older' or `newer'.  A nil CURSOR pulls the latest page.  On the
current Linux QQ kernel `reverse_order' true walks older and false walks newer.

COUNT overrides `qq-history-fetch-count' when non-nil (used by jump seek).

CALLBACK receives the merge-history plist
\(`:added-count', `:message-count', `:oldest-message-id', …).
ERRBACK receives (RESPONSE REASON)."
  (let* ((type (alist-get 'type
                          (qq-state-session-key-identity session-key)))
         (action (pcase type
                   ('group "get_group_msg_history")
                   ('dataline "get_peer_msg_history")
                   ('service "get_peer_msg_history")
                   (_ "get_friend_msg_history")))
         (n (max 1 (or count qq-history-fetch-count)))
         (params (append
                  (qq-api--session-request-params session-key)
                  `((count . ,n))
                  (when cursor
                    `((message_seq . ,(format "%s" cursor))
                      (reverse_order . ,(if (eq direction 'older) t :false)))))))
    (qq-api--call-with-materialization-owner
     session-key
     action
     params
     (lambda (response request-owner)
       (let* ((data (qq-api--response-data response))
              (messages (alist-get 'messages data nil nil #'eq))
              (meta (qq-state-merge-history
                     session-key messages request-owner)))
         (when callback
           (funcall callback meta))))
     errback)))

(defun qq-api-fetch-older-history (session-key &optional before-message-id callback errback count)
  "Fetch latest or older history for SESSION-KEY.

BEFORE-MESSAGE-ID is the optional older-page cursor."
  (interactive)
  (qq-api-fetch-history-page session-key before-message-id 'older
                             callback errback count))

(defun qq-api-chat-locator (session-key)
  "Return strict fork-native ChatLocator for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (target-id (alist-get 'target-id identity)))
    (unless (memq type '(group private))
      (user-error "qq: chat locators do not support %s sessions" type))
    (unless (qq-api-user-id-p target-id)
      (user-error
       "qq: session %s requires a decimal string target id"
       session-key))
    (if (eq type 'group)
        `((kind . "group") (group_id . ,target-id))
      `((kind . "private") (user_id . ,target-id)))))

(defun qq-api-get-forward (source callback &optional errback)
  "Fetch fork-native forward messages from explicit SOURCE."
  (setq source
        (qq-api-validate-forward-source source "emacs_get_forward source"))
  (qq-api-call
   "emacs_get_forward"
   `((source . ,source))
   (lambda (response)
     (condition-case error-data
         (let ((data (qq-api--response-data response)))
           (unless (qq-api--exact-object-keys-p data '(messages))
             (error
              "qq: emacs_get_forward response requires only data.messages"))
           (let ((messages
                  (qq-api-validate-native-forward-messages
                   (alist-get 'messages data)
                   "emacs_get_forward response messages" t)))
             (when callback
               (funcall callback messages))))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-resolve-video (resolver callback &optional errback)
  "Resolve an exact fork-native video RESOLVER on explicit user demand.

CALLBACK receives the validated remote result object.  The server owns the
two disjoint resolution routes; this client never substitutes a file token,
parent message id, or a different interface."
  (setq resolver
        (qq-api-validate-video-resolver
         resolver "emacs_resolve_video resolver"))
  (qq-api-call
   "emacs_resolve_video"
   `((resolver . ,resolver))
   (lambda (response)
     (condition-case error-data
         (let ((remote
                (qq-api--validate-native-video-remote
                 (qq-api--response-data response)
                 "emacs_resolve_video response" t)))
           (when callback
             (funcall callback (copy-tree remote))))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api--validate-send-forward-request
    (request &optional context protocol-p)
  "Validate and copy native send-forward REQUEST."
  (let ((context (or context "emacs_send_forward request")))
    (unless (qq-api--single-alist-p request)
      (qq-api--signal-schema-error
       protocol-p "qq: %s must be an object" context))
    (pcase (alist-get 'kind request)
      ("single"
       (unless (qq-api--exact-object-keys-p request '(kind message))
         (qq-api--signal-schema-error
          protocol-p "qq: %s single request has invalid fields" context))
       (qq-api-validate-message-reference
        (alist-get 'message request)
        (format "%s message" context) protocol-p))
      ("bundle"
       (unless (qq-api--exact-object-keys-p request '(kind messages))
         (qq-api--signal-schema-error
          protocol-p "qq: %s bundle request has invalid fields" context))
       (let ((messages (alist-get 'messages request)))
         (unless (or (listp messages) (vectorp messages))
           (qq-api--signal-schema-error
            protocol-p "qq: %s bundle messages must be an array" context))
         (let ((items (if (vectorp messages)
                          (append messages nil)
                        messages)))
           (unless (consp items)
             (qq-api--signal-schema-error
              protocol-p "qq: %s bundle messages must not be empty" context))
           (cl-loop for message in items
                    for index from 0
                    do (qq-api-validate-message-reference
                        message
                        (format "%s messages[%d]" context index)
                        protocol-p)))))
      (_
       (qq-api--signal-schema-error
        protocol-p "qq: %s has invalid kind %S"
        context (alist-get 'kind request))))
    (copy-tree request)))

(defun qq-api--send-forward-result (response)
  "Validate and return fork-native emacs_send_forward result."
  (let ((data (qq-api--response-data response)))
    (pcase (and (qq-api--single-alist-p data)
                (alist-get 'kind data))
      ("single"
       (unless (qq-api--exact-object-keys-p data '(kind))
         (error "qq: single forward result may contain only kind"))
       (copy-tree data))
      ("bundle"
       (unless (qq-api--exact-object-keys-p
                data '(kind message_id resource_id))
         (error
          "qq: bundle forward result requires kind, message_id, and resource_id"))
       (qq-api-validate-message-id
        (alist-get 'message_id data) "bundle forward result" t)
       (qq-api-validate-resource-id
        (alist-get 'resource_id data) "bundle forward result" t)
       (copy-tree data))
      (_
       (error "qq: emacs_send_forward result has invalid kind")))))

(defun qq-api-send-forward (destination request callback &optional errback)
  "Send a fork-native forward REQUEST to DESTINATION."
  (setq destination
        (qq-api-validate-chat-locator
         destination "emacs_send_forward destination")
        request
        (qq-api--validate-send-forward-request request))
  (qq-api-call
   "emacs_send_forward"
   `((destination . ,destination)
     (request . ,request))
   (lambda (response)
     (condition-case error-data
         (let ((result (qq-api--send-forward-result response)))
           (when callback
             (funcall callback result)))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-forward-message
    (message-id source-session-key target-session-key callback &optional errback)
  "Forward MESSAGE-ID once from SOURCE-SESSION-KEY to TARGET-SESSION-KEY."
  (let* ((chat (qq-api-chat-locator source-session-key))
         (message
          `((message_id
             . ,(qq-api-validate-message-id
                 message-id "single forward message"))
            (chat . ,chat))))
    (qq-api-send-forward
     (qq-api-chat-locator target-session-key)
     `((kind . "single") (message . ,message))
     callback errback)))

(defun qq-api-send-forward-bundle
    (source-session-key target-session-key message-ids callback
                        &optional errback)
  "Forward ordered MESSAGE-IDS as one native bundle."
  (unless (or (listp message-ids) (vectorp message-ids))
    (user-error "qq: bundle message ids must be an array"))
  (let* ((ids (if (vectorp message-ids)
                  (append message-ids nil)
                message-ids))
         (chat (qq-api-chat-locator source-session-key)))
    (unless (consp ids)
      (user-error "qq: bundle message ids must not be empty"))
    (qq-api-send-forward
     (qq-api-chat-locator target-session-key)
     `((kind . "bundle")
       (messages
        . ,(mapcar
            (lambda (message-id)
              `((message_id
                 . ,(qq-api-validate-message-id
                     message-id "bundle forward message"))
                (chat . ,(copy-tree chat))))
            ids)))
     callback errback)))
(defun qq-api--history-around-params (session-key message-id count)
  "Build `get_msg_history_around' params for SESSION-KEY centered on MESSAGE-ID."
  (setq message-id
        (qq-api-validate-message-id message-id "get_msg_history_around"))
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (target-id (alist-get 'target-id identity))
         (params `((message_id . ,message-id)
                   (count . ,(max 1 count)))))
    (pcase type
      ('group
       (append params `((group_id . ,(format "%s" target-id)))))
      ('dataline
       (append params
               `((chat_type . ,(alist-get 'chat-type identity))
                 (peer_uid . ,(alist-get 'peer-uid identity)))))
      ('service
       (append params
               `((chat_type . ,(alist-get 'chat-type identity))
                 (peer_uid . ,(alist-get 'peer-uid identity)))))
      (_
       (append params
               `((user_id . ,target-id)))))))

(defun qq-api-fetch-history-around (session-key message-id callback &optional errback count)
  "Fetch a history window around MESSAGE-ID (NapCat `get_msg_history_around').

Fork action: older+newer pages around the NT snowflake center (telega around).
CALLBACK receives the merge-history plist.  ERRBACK receives
`(RESPONSE REASON)' when the exact around request fails."
  (let ((n (max 1 (or count
                      (and (boundp 'qq-chat-jump-history-count)
                           qq-chat-jump-history-count)
                      qq-history-fetch-count))))
    (qq-api--call-with-materialization-owner
     session-key
     "get_msg_history_around"
     (qq-api--history-around-params session-key message-id n)
     (lambda (response request-owner)
       (let* ((data (qq-api--response-data response))
              (messages (or (alist-get 'messages data nil nil #'eq)
                            (and (listp data) data)))
              (meta (qq-state-merge-history
                     session-key messages request-owner)))
         (when callback
           (funcall callback meta))))
     errback)))

(defun qq-api--read-operation-current-p (session-key token)
  "Return non-nil when TOKEN still owns SESSION-KEY's read operation."
  (equal token
         (plist-get (gethash session-key qq-api--read-operations) :token)))

(defun qq-api--finish-read-operation (session-key token)
  "Settle TOKEN and start SESSION-KEY's newest coalesced read intent.

The operation remains registered while response state and synchronous hooks
run.  A reentrant `qq-api-mark-message-read' therefore coalesces into the
current owner instead of starting an overlapping request."
  (when (qq-api--read-operation-current-p session-key token)
    (let ((next-message-id
           (plist-get (gethash session-key qq-api--read-operations)
                      :next-message-id)))
      (remhash session-key qq-api--read-operations)
      (when next-message-id
        (qq-api--start-mark-message-read session-key next-message-id)))))

(defun qq-api--validate-mark-read-result
    (session-key message-id response)
  "Return RESPONSE's closed mark-read result for SESSION-KEY and MESSAGE-ID.

Service and DataLine sessions expose only a session-scoped native read
capability.  Private and group sessions must report an exact message-scoped
advance.  The tagged result must echo the requested NT message identity in
the field belonging to that scope."
  (let* ((result
          (qq-protocol-validate-emacs-mark-read-result
           (qq-api--response-data response)
           "emacs_mark_read response"))
         (type (qq-state-session-key-type session-key))
         (scope (alist-get 'scope result))
         (expected-scope
          (if (memq type '(service dataline)) "session" "message"))
         (reported-id
          (alist-get (if (equal scope "session")
                         'requested_message_id
                       'read_through_message_id)
                     result)))
    (unless (equal scope expected-scope)
      (error "qq: emacs_mark_read returned %s scope for %s session"
             scope type))
    (unless (equal reported-id message-id)
      (error "qq: emacs_mark_read response belongs to %s, requested %s"
             reported-id message-id))
    result))

(defun qq-api--start-mark-message-read (session-key message-id)
  "Start one read-through request for MESSAGE-ID in SESSION-KEY."
  (let* ((token (cl-incf qq-api--read-operation-counter))
         (observation-token (qq-api--next-read-observation-token))
         (operation (list :token token
                          :message-id message-id
                          :next-message-id nil)))
    (puthash session-key operation qq-api--read-operations)
    (qq-api-call
     "emacs_mark_read"
     (append (qq-api--session-emacs-params session-key)
             `((message_id . ,message-id)))
     (lambda (response)
       (when (qq-api--read-operation-current-p session-key token)
         (unwind-protect
             (condition-case err
                 (let* ((result
                         (qq-api--validate-mark-read-result
                          session-key message-id response))
                        (read-state (alist-get 'read_state result)))
                   (when (qq-api--accept-read-observation-p
                          session-key observation-token)
                     (qq-state-apply-session-read-state
                      session-key read-state)))
               (error
                ;; A malformed or contradictory success response is not proof
                ;; of read state.  Fail closed and request a fresh native state.
                (qq-api--default-error response (error-message-string err))
                (qq-api--refresh-session-read-state-after-failure session-key)))
           (qq-api--finish-read-operation session-key token))))
     (lambda (response reason)
       (when (qq-api--read-operation-current-p session-key token)
         (unwind-protect
             (progn
               (qq-api--default-error response reason)
               (qq-api--refresh-session-read-state-after-failure session-key))
           ;; Failure does not consume a later cursor intent.  The later target
           ;; starts clean, so persistent errors cannot self-loop.
           (qq-api--finish-read-operation session-key token)))))
    token))

(defun qq-api-mark-message-read (session-key message-id)
  "Advance SESSION-KEY's native read state using MESSAGE-ID as ownership.

MESSAGE-ID remains the original decimal NT snowflake string.  Private and
group sessions advance through that exact message.  Linux QQ exposes service
and DataLine reads only at session scope, so their tagged response explicitly
reports that wider operation and returns an authoritative post-state instead
of pretending to own a precise cursor.  Concurrent intents coalesce behind
one request and retain only the newest target.  Failure starts one
authoritative read-state refresh."
  (interactive)
  (setq message-id
        (qq-api-validate-message-id message-id "mark read target"))
  (let ((operation (gethash session-key qq-api--read-operations)))
    (cond
     ((null operation)
      (qq-api--start-mark-message-read session-key message-id))
     ((equal message-id (plist-get operation :message-id))
      (plist-get operation :token))
     (t
      (setq operation
            (plist-put operation :next-message-id message-id))
      (puthash session-key operation qq-api--read-operations)
      (plist-get operation :token)))))

(defun qq-api--send-text-segments (text &optional reply-to-message-id)
  "Return send_msg segment list for TEXT and optional REPLY-TO-MESSAGE-ID."
  (append
   (when reply-to-message-id
     (setq reply-to-message-id
           (qq-api-validate-message-id reply-to-message-id "reply segment"))
     `(((type . "reply")
        (data . ((id . ,reply-to-message-id))))))
   `(((type . "text")
      (data . ((text . ,text)))))))

(defun qq-api-send-message
    (session-key segments &optional raw-message callback errback)
  "Send SEGMENTS to SESSION-KEY.

Insert a local pending message immediately and update it after the response.
RAW-MESSAGE, when non-nil, overrides the optimistic raw-message field used for
local pending rendering.  CALLBACK receives the raw response after the pending
message is promoted.  ERRBACK receives RESPONSE and REASON after the optimistic
message actually transitions from pending to failed; it defaults to
`qq-api--default-error'.  A late failure for an already server-backed message is
ignored so callers cannot restore and resend an authoritative delivery.

The NapCat hard-cut returns `message_id' as the NT snowflake string; that value
is stored as the message `server-id' and becomes the timeline anchor."
  (unless (qq-state-session-sendable-p session-key)
    (user-error "qq: this session is read-only"))
  (let* ((segments (copy-tree (or segments '())))
         (params (append
                  (qq-api--session-request-params session-key)
                  `((message . ,segments))))
         ;; Build and validate request parameters before adding timeline state:
         ;; a synchronous caller error must not leave a permanent pending row.
         (pending (qq-state-insert-pending-message session-key segments raw-message))
         (local-id (alist-get 'local-id pending))
         (failure-callback (or errback #'qq-api--default-error)))
    (cl-labels
        ((fail-pending
          (response reason)
          (when (qq-state-mark-pending-message-failed
                 session-key local-id reason)
            (funcall failure-callback response reason))))
      (condition-case err
          (qq-api--call-with-materialization-owner
           session-key
           "send_msg"
           params
           (lambda (response request-owner)
             ;; Protocol decoding and state promotion belong to the send
             ;; transaction.  The caller callback does not: never reinterpret
             ;; a client callback error as a failed network delivery.
             (when
                 (condition-case promote-error
                     (let* ((data (qq-api--response-data response))
                            (message-id
                             (qq-api-validate-message-id
                              (alist-get 'message_id data nil nil #'eq)
                              "send_msg response" t)))
                       (qq-state-mark-pending-message-sent
                        session-key local-id message-id request-owner)
                       t)
                   (error
                    (fail-pending
                     response (error-message-string promote-error))
                    nil))
               (when callback
                 (funcall callback response))))
           #'fail-pending)
        (error
         ;; A synchronous transport failure happens after optimistic insertion.
         ;; Settle that row, then let the caller restore its rich draft and
         ;; retain the original error type/backtrace.
         (qq-state-mark-pending-message-failed
          session-key local-id (error-message-string err))
         (signal (car err) (cdr err)))))))

(defun qq-api-send-text (session-key text &optional reply-to-message-id)
  "Send TEXT to SESSION-KEY.

Insert a local pending message immediately and update it after the response.
When REPLY-TO-MESSAGE-ID is non-nil, send the text as a reply."
  (interactive)
  (qq-api-send-message
   session-key
   (qq-api--send-text-segments text reply-to-message-id)
   text))

(defun qq-api--poke-id-p (value)
  "Return non-nil when VALUE is a nonzero decimal poke identity."
  (and (qq-api-user-id-p value)
       (string-match-p "[1-9]" value)))

(defun qq-api-send-poke (session-key target-id &optional callback errback)
  "Send a poke in SESSION-KEY to TARGET-ID.

SESSION-KEY must name an existing private or group session.  Its stored peer
identity always determines the conversation; TARGET-ID only determines who is
poked.  A successful action is reflected locally as a poke notice; a matching
websocket notice is deduplicated by its local second-level anchor."
  (let ((session (qq-state-session session-key)))
    (unless session
      (user-error "qq: poke requires an existing session"))
    (unless (qq-api--poke-id-p target-id)
      (user-error "qq: poke target must be a nonzero decimal QQ string"))
    (let* ((identity (qq-state-session-key-identity session-key))
           (type (alist-get 'type identity))
           (peer-id (alist-get 'target-id identity))
           (params
            (pcase type
              ('group
               (unless (and (qq-api-group-id-p peer-id)
                            (qq-api--poke-id-p peer-id))
                 (user-error
                  "qq: group poke requires a nonzero decimal session peer"))
               `((group_id . ,peer-id)
                 (user_id . ,target-id)
                 (target_id . ,target-id)))
              ('private
               (unless (qq-api--poke-id-p peer-id)
                 (user-error
                  "qq: private poke requires a nonzero decimal session peer"))
               `((user_id . ,peer-id)
                 (target_id . ,target-id)))
              (_
               (user-error
                "qq: poke is unsupported for %s sessions" type)))))
      (qq-api-call
       "send_poke"
       params
       (lambda (response)
         (when-let* ((self-id (qq-state-self-user-id)))
           (qq-state-apply-poke-notice
            `((time . ,(truncate (float-time)))
              (emacs_local_p . t)
              (post_type . "notice")
              (notice_type . "notify")
              (sub_type . "poke")
              ,@(if (eq type 'group)
                    `((group_id . ,peer-id)
                      (user_id . ,self-id)
                      (target_id . ,target-id))
                  `((user_id . ,peer-id)
                    (sender_id . ,self-id)
                    (target_id . ,target-id))))))
         (when callback
           (funcall callback response)))
       (or errback
           (lambda (response reason)
             (qq-api--default-error response reason)))))))

(defun qq-api--message-mutation-context (reference context)
  "Return validated mutation context for closed REFERENCE and CONTEXT.

The message id and chat travel as one identity object.  A known canonical
index contradiction is rejected before any remote side effect."
  (let* ((reference
          (qq-api-validate-message-reference reference context))
         (message-id (alist-get 'message_id reference))
         (session-key
          (qq-api-session-key-from-locator (alist-get 'chat reference))))
    (qq-state-validate-message-session session-key message-id)
    (list :reference reference
          :message-id message-id
          :session-key session-key)))

(defun qq-api-delete-message (reference)
  "Recall the exact message in closed locator-qualified REFERENCE."
  (let* ((context
          (qq-api--message-mutation-context reference "delete_msg reference"))
         (message-id (plist-get context :message-id))
         (session-key (plist-get context :session-key)))
    (qq-api-call
     "delete_msg"
     `((message_id . ,message-id))
     (lambda (_response)
       (qq-state-apply-recall session-key message-id)))))

(defun qq-api-recall-poke
    (session-key recall-reference &optional callback errback)
  "Recall a poke in SESSION-KEY through native RECALL-REFERENCE.

Pokes are gray-tip records, so they must not be sent through `delete_msg'.
The closed reference carries the exact native Peer and msgId expected by
`recallNudge'; no session or message-cache lookup is permitted here."
  (let* ((reference
          (qq-protocol-validate-poke-recall-reference
           recall-reference "recall_poke" 'user-error))
         (reference
          (qq-state-validate-poke-recall-reference session-key reference))
         (message-id (alist-get 'message_id reference)))
    (when (qq-protocol-poke-recall-reference-expired-p reference)
      (user-error "qq: 戳一戳已超过 2 分钟撤回期限"))
    (qq-api-call
     "recall_poke"
     `((recall_reference . ,reference))
     (lambda (response)
       (qq-state-apply-recall session-key message-id)
       (when callback
         (funcall callback response)))
     (or errback
         (lambda (response reason)
           (qq-api--default-error response reason))))))

(defun qq-api-set-message-emoji-like
    (reference emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on the group message in closed REFERENCE.

REFERENCE's `message_id' remains the original NapCat NT snowflake string.  On
success, optimistically apply one local reaction delta; the subsequent NapCat
notice reconciles it with the authoritative aggregate count."
  (let* ((context
          (qq-api--message-mutation-context
           reference "set_msg_emoji_like reference"))
         (reference (plist-get context :reference))
         (message-id (plist-get context :message-id))
         (session-key (plist-get context :session-key))
         (chat (alist-get 'chat reference)))
    (unless (equal (alist-get 'kind chat) "group")
      (user-error "qq: reactions require a group message reference"))
    (setq emoji-id (format "%s" emoji-id))
    (unless (string-match-p "\\`[0-9]+\\'" emoji-id)
      (user-error "qq: reaction emoji_id must be a decimal string"))
    (let* ((set (and set t))
           (group-id (qq-state-session-key-target-id session-key)))
      (qq-api-call
       "set_msg_emoji_like"
       `((message_id . ,message-id)
         (emoji_id . ,emoji-id)
         (set . ,(if set t :false)))
       (lambda (response)
         (when-let* ((self-id (qq-state-self-user-id)))
           (qq-state-apply-emoji-like-notice
            session-key
            `((notice_type . "group_msg_emoji_like")
              (message_id . ,message-id)
              (group_id . ,group-id)
              (user_id . ,self-id)
              (is_add . ,(if set t :false))
              (likes . (((emoji_id . ,emoji-id)))))))
         (when callback
           (funcall callback response)))
       (or errback #'qq-api--default-error)))))

(defun qq-api-get-avatar (user-id callback &optional errback no-cache)
  "Fetch avatar resource for USER-ID and pass it to CALLBACK."
  (qq-api-call
   "get_avatar"
   `((user_id . ,(format "%s" user-id))
     (no_cache . ,(if no-cache t :false)))
   (lambda (response)
     (funcall callback (qq-api--response-data response)))
   errback))

(defun qq-api-get-user (user-id callback &optional errback)
  "Fetch native profile for USER-ID and pass it to CALLBACK."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: user profile requires a decimal string user id"))
  (qq-api-call
   "emacs_get_user"
   `((user_id . ,user-id))
   (lambda (response)
     (condition-case error-data
         (let ((profile (qq-api--response-data response)))
           (unless (equal (alist-get 'user_id profile) user-id)
             (error "qq: emacs_get_user returned a different user identity"))
           (funcall callback profile))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-get-user-like (user-id callback &optional errback)
  "Fetch verified profile-like count for USER-ID and call CALLBACK."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: user likes require a decimal string user id"))
  (qq-api-call
   "emacs_get_user_like"
   `((user_id . ,user-id))
   (lambda (response)
     (condition-case error-data
         (let* ((data (qq-api--response-data response))
                (total-count (alist-get 'total_count data)))
           (unless (equal (alist-get 'user_id data) user-id)
             (error "qq: emacs_get_user_like returned a different user identity"))
           (unless (and (integerp total-count) (>= total-count 0))
             (error "qq: emacs_get_user_like returned an invalid total count"))
           (funcall callback total-count))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-like-user (user-id callback &optional errback)
  "Add one profile like to USER-ID and call CALLBACK with its domain result."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: user likes require a decimal string user id"))
  (qq-api-call
   "emacs_like_user"
   `((user_id . ,user-id))
   (lambda (response)
     (condition-case error-data
         (let* ((data (qq-api--response-data response))
                (outcome (alist-get 'outcome data)))
           (unless (equal (alist-get 'user_id data) user-id)
             (error "qq: emacs_like_user returned a different user identity"))
           (pcase outcome
             ("liked"
              (unless (and (qq-api--exact-object-keys-p
                            data '(user_id outcome added_count))
                           (equal (alist-get 'added_count data) 1))
                (error "qq: emacs_like_user returned an invalid liked result")))
             ("daily_limit"
              (unless (qq-api--exact-object-keys-p data '(user_id outcome))
                (error "qq: emacs_like_user returned an invalid daily-limit result")))
             (_
              (error "qq: emacs_like_user returned an unknown outcome")))
           (funcall callback (copy-tree data)))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-get-user-photo-wall (user-id callback &optional errback)
  "Fetch native photo wall for USER-ID and pass its photos to CALLBACK."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: photo wall requires a decimal string user id"))
  (qq-api-call
   "emacs_get_user_photo_wall"
   `((user_id . ,user-id))
   (lambda (response)
     (condition-case error-data
         (let ((data (qq-api--response-data response)))
           (unless (equal (alist-get 'user_id data) user-id)
             (error "qq: emacs_get_user_photo_wall returned a different user identity"))
           (funcall callback (alist-get 'photos data)))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api-get-group-avatar (group-id callback &optional errback no-cache)
  "Fetch group avatar resource for GROUP-ID and pass it to CALLBACK."
  (qq-api-call
   "get_group_avatar"
   `((group_id . ,(format "%s" group-id))
     (no_cache . ,(if no-cache t :false)))
   (lambda (response)
     (funcall callback (qq-api--response-data response)))
   errback))

(defun qq-api-get-group (group-id callback &optional errback)
  "Fetch native group profile for GROUP-ID and pass it to CALLBACK."
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group profile requires a canonical uint32 group UIN"))
  (qq-api-call
   "emacs_get_group"
   `((group_id . ,group-id))
   (lambda (response)
     (condition-case error-data
         (let ((profile (qq-api--response-data response)))
           (unless (equal (alist-get 'group_id profile) group-id)
             (error "qq: emacs_get_group returned a different group identity"))
           (funcall callback profile))
       (error
        (funcall (or errback #'qq-api--default-error)
                 response (error-message-string error-data)))))
   errback))

(defun qq-api--message-search-session-locator (session-key)
  "Return a supported message-search locator for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity))
         (locator (qq-api--session-emacs-locator session-key)))
    (unless (memq type '(group private))
      (user-error "qq: message search is unsupported for %s sessions" type))
    (unless (qq-protocol-emacs-chat-locator-p locator)
      (user-error "qq: message search requires a nonzero decimal chat id"))
    locator))

(defun qq-api--message-search-page-callback
    (projection expected-chat callback errback response)
  "Validate search RESPONSE for PROJECTION and EXPECTED-CHAT.

Invoke CALLBACK only after every result proves the same closed chat identity;
otherwise route the protocol error to ERRBACK."
  (condition-case error-data
      (let* ((page
              (qq-protocol-validate-emacs-message-search-page
               (qq-api--response-data response)
               "emacs_search_messages response" nil projection))
             (results (alist-get 'results page)))
        (when (eq projection 'message)
          (qq-api--validate-message-search-segments page))
        (dolist (result results)
          (unless (equal (alist-get 'chat result) expected-chat)
            (error "qq: message search returned a different chat")))
        (funcall callback page))
    (error
     (funcall (or errback #'qq-api--default-error)
              response (error-message-string error-data)))))

(defun qq-api--validate-message-search-segments (page)
  "Validate every discriminated native segment in message-projection PAGE."
  (dolist (result (alist-get 'results page))
    (cl-loop
     for segment in (alist-get 'segments result)
     for index from 0
     do (qq-api--validate-native-forward-segment
         segment
         (format "message search result %s segment %d"
                 (alist-get 'message_id result) index)
         t)))
  page)

(defun qq-api-search-messages-start
    (session-key query callback &optional errback limit)
  "Start authoritative message search in SESSION-KEY for QUERY.

CALLBACK receives one validated closed page with `results' and an opaque
`next_cursor'.  LIMIT defaults to 50 and must be between 1 and 100.  Only
private and group chats are searchable; no loaded-buffer fallback exists."
  (setq query (and (stringp query) (string-trim query)))
  (unless (and query (not (string-empty-p query)))
    (user-error "qq: message search query must be a non-empty string"))
  (when (> (length query) 512)
    (user-error "qq: message search query must be at most 512 characters"))
  (setq limit (or limit 50))
  (unless (and (integerp limit) (<= 1 limit 100))
    (user-error "qq: message search limit must be between 1 and 100"))
  (let ((chat (qq-api--message-search-session-locator session-key)))
    (qq-api-call
     "emacs_search_messages"
     `((kind . "start")
       (projection . "summary")
       (chat . ,chat)
       (query . ,query)
       (limit . ,limit))
     (apply-partially
      #'qq-api--message-search-page-callback
      'summary chat callback errback)
     errback)))

(defun qq-api-search-messages-next
    (session-key cursor projection callback &optional errback)
  "Continue SESSION-KEY's authoritative search at opaque CURSOR.

PROJECTION must be the cursor's explicit `summary' or `message' result
projection.  CALLBACK receives a page validated against that discriminator."
  (let ((chat (qq-api--message-search-session-locator session-key)))
    (unless (and (stringp cursor) (not (string-empty-p cursor)))
      (user-error "qq: message search cursor must be a non-empty string"))
    (unless (memq projection '(summary message))
      (user-error "qq: invalid message search projection"))
    (qq-api-call
     "emacs_search_messages"
     `((kind . "next")
       (cursor . ,cursor)
       (chat . ,chat)
       (projection . ,(symbol-name projection)))
     (apply-partially
      #'qq-api--message-search-page-callback
      projection chat callback errback)
     errback)))

(defun qq-api-filter-messages-start
    (session-key query callback &optional errback limit)
  "Start a rendering-snapshot filter search in SESSION-KEY for QUERY.

Unlike summary search, CALLBACK receives a `message' projection whose exact
hits remain filter-owned closed snapshots.  The normal history cache and
continuous window remain untouched."
  (setq query (and (stringp query) (string-trim query)))
  (unless (and query (not (string-empty-p query)))
    (user-error "qq: message filter query must be a non-empty string"))
  (when (> (length query) 512)
    (user-error "qq: message filter query must be at most 512 characters"))
  (setq limit (or limit 50))
  (unless (and (integerp limit) (<= 1 limit 100))
    (user-error "qq: message filter limit must be between 1 and 100"))
  (let ((chat (qq-api--message-search-session-locator session-key)))
    (qq-api-call
     "emacs_search_messages"
     `((kind . "start")
       (projection . "message")
       (chat . ,chat)
       (query . ,query)
       (limit . ,limit))
     (apply-partially
      #'qq-api--message-search-page-callback
      'message chat callback errback)
     errback)))

(defun qq-api-filter-messages-next
    (session-key cursor callback &optional errback)
  "Continue SESSION-KEY's rendering-snapshot filter at opaque CURSOR."
  (let ((chat (qq-api--message-search-session-locator session-key)))
    (unless (and (stringp cursor) (not (string-empty-p cursor)))
      (user-error "qq: message filter cursor must be a non-empty string"))
    (qq-api-call
     "emacs_search_messages"
     `((kind . "next")
       (cursor . ,cursor)
       (chat . ,chat)
       (projection . "message"))
     (apply-partially
      #'qq-api--message-search-page-callback
      'message chat callback errback)
     errback)))

(defun qq-api--normalize-group-member-search-result (member index)
  "Validate and normalize native group MEMBER at INDEX."
  (let ((context (format "emacs_search_group_members[%d]" index))
        (keys '(user_id uid nickname card remark qid title role robot)))
    (unless (qq-api--exact-object-keys-p member keys)
      (error "qq: %s has invalid fields" context))
    (unless (qq-api-user-id-p (alist-get 'user_id member))
      (error "qq: %s.user_id must be an original decimal string" context))
    (unless (qq-api-non-empty-string-p (alist-get 'uid member))
      (error "qq: %s.uid must be a non-empty string" context))
    (unless (stringp (alist-get 'nickname member))
      (error "qq: %s.nickname must be a string" context))
    (dolist (key '(card remark qid title))
      (unless (or (null (alist-get key member))
                  (stringp (alist-get key member)))
        (error "qq: %s.%s must be a string or null" context key)))
    (unless (member (alist-get 'role member)
                    '("member" "admin" "owner" "stranger"))
      (error "qq: %s.role is invalid" context))
    (unless (memq (alist-get 'robot member) '(t :false))
      (error "qq: %s.robot must be a boolean" context))
    (let ((copy (copy-tree member)))
      (setf (alist-get 'robot copy nil nil #'eq)
            (eq (alist-get 'robot member) t))
      copy)))

(defun qq-api--validate-group-member-search-results (data)
  "Validate and copy the closed native group-member search array DATA."
  (unless (and (listp data) (proper-list-p data))
    (error "qq: emacs_search_group_members returned a non-array"))
  (let ((seen-user-ids (make-hash-table :test #'equal)))
    (cl-loop
     for member in data
     for index from 0
     for normalized = (qq-api--normalize-group-member-search-result member index)
     for user-id = (alist-get 'user_id normalized)
     do (when (gethash user-id seen-user-ids)
          (error
           "qq: emacs_search_group_members returned duplicate user_id %s"
           user-id))
     do (puthash user-id t seen-user-ids)
     collect normalized)))

(defun qq-api--group-member-search-callback (callback errback response)
  "Validate group-member search RESPONSE before invoking CALLBACK.

Only protocol validation failures reach ERRBACK.  Errors raised by the
consumer CALLBACK remain consumer errors and are never relabelled as wire
failures."
  (let (members valid-p)
    (condition-case error-data
        (setq members
              (qq-api--validate-group-member-search-results
               (qq-api--response-data response))
              valid-p t)
      (error
       (funcall (or errback #'qq-api--default-error)
                response (error-message-string error-data))))
    (when valid-p
      (funcall callback members))))

(defun qq-api-search-group-members
    (group-id query callback &optional errback limit)
  "Search native members in GROUP-ID for QUERY and call CALLBACK.

The fork-native action searches card, nickname, remark, QID and QQ number.
Every returned identity is validated as an original string; there is no
OneBot numeric member-list fallback.  LIMIT defaults server-side and may be
  between 1 and 200."
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group member search requires a canonical uint32 group UIN"))
  (unless (stringp query)
    (user-error "qq: group member search query must be a string"))
  (when (and limit (not (and (integerp limit) (<= 1 limit 200))))
    (user-error "qq: group member search limit must be between 1 and 200"))
  (qq-api-call
   "emacs_search_group_members"
   `((group_id . ,group-id)
     (query . ,query)
     ,@(when limit `((limit . ,limit))))
   (apply-partially #'qq-api--group-member-search-callback callback errback)
   errback))

(defun qq-api--validate-directory-search-hit (hit context)
  "Validate and copy one closed native directory HIT in CONTEXT."
  (unless (qq-api--exact-object-keys-p hit '(start end text))
    (error "qq: %s has invalid fields" context))
  (let ((start (alist-get 'start hit))
        (end (alist-get 'end hit))
        (text (alist-get 'text hit)))
    (unless (and (qq-protocol--nonnegative-safe-integer-p start)
                 (qq-protocol--positive-safe-integer-p end)
                 (< start end))
      (error "qq: %s has an invalid hit range" context))
    (unless (qq-api-non-empty-string-p text)
      (error "qq: %s.text must be a non-empty string" context))
    (copy-tree hit)))

(defun qq-api--validate-directory-hit-object (hits field-sources context)
  "Validate closed directory HITS against FIELD-SOURCES in CONTEXT.

FIELD-SOURCES maps every required hit field to its exact source string.
Offsets are JavaScript UTF-16 code units and every hit text must equal the
corresponding source slice."
  (let ((fields (mapcar #'car field-sources)))
    (unless (qq-api--exact-object-keys-p hits fields)
      (error "qq: %s has invalid hit fields" context))
    (dolist (spec field-sources)
      (let* ((field (car spec))
             (source (cdr spec))
             (items (alist-get field hits))
             (hit-context (format "%s.%s" context field)))
        (unless (and (listp items) (proper-list-p items))
          (error "qq: %s must be a hit array" hit-context))
        (cl-loop
         for hit in items
         for index from 0
         do
         (let ((item-context (format "%s[%d]" hit-context index)))
           (qq-api--validate-directory-search-hit hit item-context)
           (let* ((start (alist-get 'start hit))
                  (end (alist-get 'end hit))
                  (text (alist-get 'text hit))
                  (slice (qq-api--utf-16-unit-slice source start end)))
             (unless (and slice (string= text slice))
               (error "qq: %s does not match its source text"
                      item-context)))))))
    (copy-tree hits)))

(defun qq-api--validate-group-chat-search-group (group context)
  "Validate and copy a closed group-search GROUP in CONTEXT."
  (unless (qq-api--exact-object-keys-p
           group
           '(group_id name remark member_count is_conf
                      has_modify_conf_group_face has_modify_conf_group_name
                      no_code_finger_open_flag self_permission hits))
    (error "qq: %s has invalid fields" context))
  (unless (qq-protocol-group-uin-p (alist-get 'group_id group))
    (error "qq: %s.group_id must be a canonical uint32 group UIN"
           context))
  (dolist (field '(name remark))
    (unless (stringp (alist-get field group))
      (error "qq: %s.%s must be a string" context field)))
  (unless (qq-api--uint32-p (alist-get 'member_count group))
    (error "qq: %s.member_count must be an unsigned 32-bit integer" context))
  (dolist (field '(is_conf has_modify_conf_group_face
                          has_modify_conf_group_name
                          no_code_finger_open_flag))
    (unless (memq (alist-get field group) '(t :false))
      (error "qq: %s.%s must be a boolean" context field)))
  (unless (member (alist-get 'self_permission group)
                  '("member" "admin" "owner"))
    (error "qq: %s.self_permission is invalid" context))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits group)
   `((group_id . ,(alist-get 'group_id group))
     (name . ,(alist-get 'name group))
     (remark . ,(alist-get 'remark group)))
   (format "%s.hits" context))
  (copy-tree group))

(defun qq-api--validate-group-chat-search-discussion (discussion context)
  "Validate and copy one group-search DISCUSSION in CONTEXT."
  (unless (qq-api--exact-object-keys-p discussion '(discussion_id name hits))
    (error "qq: %s has invalid fields" context))
  (unless (qq-api-non-empty-string-p (alist-get 'discussion_id discussion))
    (error "qq: %s.discussion_id must be a non-empty native string" context))
  (unless (stringp (alist-get 'name discussion))
    (error "qq: %s.name must be a string" context))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits discussion)
   `((name . ,(alist-get 'name discussion)))
   (format "%s.hits" context))
  (copy-tree discussion))

(defun qq-api--validate-group-chat-search-member-profile (profile context)
  "Validate and copy one group-search member PROFILE in CONTEXT."
  (unless (qq-api--exact-object-keys-p
           profile '(uid user_id nickname remark card hits))
    (error "qq: %s has invalid fields" context))
  (unless (qq-api-non-empty-string-p (alist-get 'uid profile))
    (error "qq: %s.uid must be a non-empty native string" context))
  (unless (qq-protocol--nonzero-decimal-string-p (alist-get 'user_id profile))
    (error "qq: %s.user_id must be an original nonzero decimal string" context))
  (dolist (field '(nickname remark card))
    (unless (stringp (alist-get field profile))
      (error "qq: %s.%s must be a string" context field)))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits profile)
   `((user_id . ,(alist-get 'user_id profile))
     (nickname . ,(alist-get 'nickname profile))
     (remark . ,(alist-get 'remark profile))
     (card . ,(alist-get 'card profile)))
   (format "%s.hits" context))
  (copy-tree profile))

(defun qq-api--validate-group-chat-search-member-card (card context)
  "Validate and copy one group-search member CARD in CONTEXT."
  (unless (qq-api--exact-object-keys-p card '(uid card hits))
    (error "qq: %s has invalid fields" context))
  (unless (qq-api-non-empty-string-p (alist-get 'uid card))
    (error "qq: %s.uid must be a non-empty native string" context))
  (unless (stringp (alist-get 'card card))
    (error "qq: %s.card must be a string" context))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits card)
   `((card . ,(alist-get 'card card)))
   (format "%s.hits" context))
  (copy-tree card))

(defun qq-api--validate-group-chat-search-result (result index)
  "Validate and copy group-chat search RESULT at INDEX."
  (let ((context (format "emacs_search_group_chats.results[%d]" index)))
    (unless (qq-api--exact-object-keys-p
             result '(group discussions member_profiles member_cards
                            recall_reason))
      (error "qq: %s has invalid fields" context))
    (qq-api--validate-group-chat-search-group
     (alist-get 'group result) (format "%s.group" context))
    (dolist (spec `((discussions . ,#'qq-api--validate-group-chat-search-discussion)
                    (member_profiles
                     . ,#'qq-api--validate-group-chat-search-member-profile)
                    (member_cards
                     . ,#'qq-api--validate-group-chat-search-member-card)))
      (let ((items (alist-get (car spec) result)))
        (unless (listp items)
          (error "qq: %s.%s must be an array" context (car spec)))
        (cl-loop for item in items
                 for item-index from 0
                 do (funcall (cdr spec) item
                             (format "%s.%s[%d]"
                                     context (car spec) item-index)))))
    (unless (stringp (alist-get 'recall_reason result))
      (error "qq: %s.recall_reason must be a string" context))
    (copy-tree result)))

(defun qq-api--validate-group-chat-search-page (data)
  "Validate and copy a closed native group-chat search page DATA."
  (unless (qq-api--exact-object-keys-p
           data '(results multi_user_keywords next_cursor))
    (error "qq: emacs_search_group_chats returned an invalid page"))
  (let ((results (alist-get 'results data))
        (keywords (alist-get 'multi_user_keywords data))
        (cursor (alist-get 'next_cursor data))
        (seen-groups (make-hash-table :test #'equal)))
    (unless (listp results)
      (error "qq: emacs_search_group_chats.results must be an array"))
    (cl-loop
     for result in results
     for index from 0
     do
     (let* ((validated (qq-api--validate-group-chat-search-result result index))
            (group-id (alist-get 'group_id (alist-get 'group validated))))
       (when (gethash group-id seen-groups)
         (error "qq: group search page duplicated group_id %s" group-id))
       (puthash group-id t seen-groups)))
    (unless (and (listp keywords) (cl-every #'stringp keywords))
      (error "qq: emacs_search_group_chats.multi_user_keywords must be a string array"))
    (unless (or (null cursor) (qq-api--uuid-v4-string-p cursor))
      (error "qq: emacs_search_group_chats.next_cursor must be UUIDv4 or null"))
    (copy-tree data)))

(defun qq-api--group-chat-search-sort-wire (sort)
  "Return wire value for semantic group-chat search SORT."
  (pcase (or sort 'default)
    ('default "default")
    ('latest-created "latest_created")
    ('few-members "few_members")
    (_ (user-error "qq: invalid group-chat search sort %S" sort))))

(defun qq-api--group-chat-search-owner-params
    (query sort limit filter-member-uids)
  "Validate and return group-search owner params.

QUERY, SORT, LIMIT, and FILTER-MEMBER-UIDS are repeated unchanged for cursor
owner proof on continuation calls."
  (unless (stringp query)
    (user-error "qq: group-chat search query must be a string"))
  (setq query (string-trim query))
  (when (or (string-empty-p query) (> (length query) 512))
    (user-error "qq: group-chat search query must contain 1 to 512 characters"))
  (setq limit (or limit 50))
  (unless (and (integerp limit) (<= 1 limit 100))
    (user-error "qq: group-chat search limit must be between 1 and 100"))
  (setq filter-member-uids (or filter-member-uids '()))
  (unless (and (listp filter-member-uids)
               (<= (length filter-member-uids) 100)
               (cl-every #'qq-api-non-empty-string-p filter-member-uids)
               (= (length filter-member-uids)
                  (length (delete-dups (copy-sequence filter-member-uids)))))
    (user-error "qq: group-chat member filters must be unique native UID strings"))
  `((query . ,query)
    (sort . ,(qq-api--group-chat-search-sort-wire sort))
    (limit . ,limit)
    ,@(when filter-member-uids
        `((filter_member_uids . ,(copy-sequence filter-member-uids))))))

(defun qq-api--group-chat-search-callback (callback errback response)
  "Validate group-chat search RESPONSE before invoking CALLBACK."
  (let (page valid-p)
    (condition-case error-data
        (setq page (qq-api--validate-group-chat-search-page
                    (qq-api--response-data response))
              valid-p t)
      (error
       (funcall (or errback #'qq-api--default-error)
                response (error-message-string error-data))))
    (when valid-p (funcall callback page))))

(defun qq-api-search-group-chats-start
    (query callback &optional errback sort limit filter-member-uids)
  "Start native group-chat search for QUERY and invoke CALLBACK with its page."
  (let ((owner (qq-api--group-chat-search-owner-params
                query sort limit filter-member-uids)))
    (qq-api-call
     "emacs_search_group_chats"
     (cons '(kind . "start") owner)
     (apply-partially #'qq-api--group-chat-search-callback callback errback)
     errback)))

(defun qq-api-search-group-chats-next
    (cursor query callback &optional errback sort limit filter-member-uids)
  "Continue opaque group-chat search CURSOR owned by QUERY and its options."
  (unless (qq-api--uuid-v4-string-p cursor)
    (user-error "qq: group-chat search cursor must be a canonical UUIDv4"))
  (let ((owner (qq-api--group-chat-search-owner-params
                query sort limit filter-member-uids)))
    (qq-api-call
     "emacs_search_group_chats"
     (append `((kind . "next") (cursor . ,cursor)) owner)
     (apply-partially #'qq-api--group-chat-search-callback callback errback)
     errback)))

(defun qq-api--uuid-v4-string-p (value)
  "Return non-nil when VALUE is a lowercase canonical UUIDv4 string."
  (let ((case-fold-search nil))
    (and
     (stringp value)
     (string-match-p
      (concat
       "\\`[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-"
       "[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}\\'")
      value))))

(defun qq-api--utf-16-unit-slice (source start end)
  "Return SOURCE slice between UTF-16 code-unit offsets START and END.

Return nil when either offset is out of bounds or splits a surrogate pair.
NapCat validates directory hits with JavaScript string offsets, so using
ordinary Emacs character indexes here would accept the wrong ranges around
astral Unicode characters."
  (let* ((encoded (encode-coding-string source 'utf-16le t))
         (byte-start (* 2 start))
         (byte-end (* 2 end)))
    (when (and (<= 0 byte-start byte-end)
               (<= byte-end (length encoded)))
      (let* ((bytes (substring encoded byte-start byte-end))
             (decoded (decode-coding-string bytes 'utf-16le t)))
        ;; Decoding a lone UTF-16 surrogate may silently produce an empty
        ;; string.  Round-trip length proves that both offsets were code-point
        ;; boundaries rather than accepting that lossy decode.
        (when (= (length (encode-coding-string decoded 'utf-16le t))
                 (length bytes))
          decoded)))))

(defun qq-api--validate-contact-search-friend (result context)
  "Validate and copy closed friend search RESULT in CONTEXT."
  (unless (qq-api--exact-object-keys-p
           result
           '(kind user_id uid qid nickname remark category_name hits
                  recall_reason))
    (error "qq: %s has invalid friend fields" context))
  (unless (equal (alist-get 'kind result) "friend")
    (error "qq: %s.kind must be friend" context))
  (unless (qq-protocol--nonzero-decimal-string-p
           (alist-get 'user_id result))
    (error "qq: %s.user_id must be an original nonzero decimal string"
           context))
  (unless (qq-api-non-empty-string-p (alist-get 'uid result))
    (error "qq: %s.uid must be a non-empty native string" context))
  (dolist (field '(qid nickname remark category_name recall_reason))
    (unless (stringp (alist-get field result))
      (error "qq: %s.%s must be a string" context field)))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits result)
   `((qid . ,(alist-get 'qid result))
     (user_id . ,(alist-get 'user_id result))
     (nickname . ,(alist-get 'nickname result))
     (remark . ,(alist-get 'remark result)))
   (format "%s.hits" context))
  (copy-tree result))

(defun qq-api--validate-contact-search-group-member (result context)
  "Validate and copy closed group-member search RESULT in CONTEXT."
  (unless (qq-api--exact-object-keys-p
           result
           '(kind group_id group_name group_remark user_id uid nickname
                  remark card is_friend hits recall_reason))
    (error "qq: %s has invalid group-member fields" context))
  (unless (equal (alist-get 'kind result) "group_member")
    (error "qq: %s.kind must be group_member" context))
  (unless (qq-protocol-group-uin-p (alist-get 'group_id result))
    (error "qq: %s.group_id must be a canonical uint32 group UIN" context))
  (unless (qq-protocol--nonzero-decimal-string-p (alist-get 'user_id result))
    (error "qq: %s.user_id must be an original nonzero decimal string" context))
  (unless (qq-api-non-empty-string-p (alist-get 'uid result))
    (error "qq: %s.uid must be a non-empty native string" context))
  (dolist (field '(group_name group_remark nickname remark card recall_reason))
    (unless (stringp (alist-get field result))
      (error "qq: %s.%s must be a string" context field)))
  (unless (memq (alist-get 'is_friend result) '(t :false))
    (error "qq: %s.is_friend must be a boolean" context))
  (qq-api--validate-directory-hit-object
   (alist-get 'hits result)
   `((user_id . ,(alist-get 'user_id result))
     (nickname . ,(alist-get 'nickname result))
     (remark . ,(alist-get 'remark result))
     (card . ,(alist-get 'card result)))
   (format "%s.hits" context))
  (copy-tree result))

(defun qq-api--validate-contact-search-result (result index)
  "Validate and copy contact-search RESULT at INDEX."
  (let ((context (format "emacs_search_contacts.results[%d]" index)))
    (pcase (and (consp result) (alist-get 'kind result))
      ("friend"
       (qq-api--validate-contact-search-friend result context))
      ("group_member"
       (qq-api--validate-contact-search-group-member result context))
      (_
       (error "qq: %s has an invalid kind" context)))))

(defun qq-api--validate-contact-search-owner-result (result owner context)
  "Require validated RESULT to remain inside contact-search OWNER in CONTEXT."
  (let ((scope (alist-get 'scope owner))
        (kind (alist-get 'kind result))
        (group-id (alist-get 'group_id owner)))
    (pcase scope
      ("friends"
       (unless (equal kind "friend")
         (error "qq: %s escaped friends scope" context)))
      ("group_members"
       (unless (and (equal kind "group_member")
                    (equal (alist-get 'group_id result) group-id))
         (error "qq: %s escaped group_members owner" context)))
      ("friends_and_group_members"
       (unless (or (equal kind "friend")
                   (and (equal kind "group_member")
                        (equal (alist-get 'group_id result) group-id)))
         (error "qq: %s escaped combined contact owner" context)))
      (_
       (error "qq: %s has an invalid internal owner scope" context)))))

(defun qq-api--validate-contact-search-page (data &optional owner)
  "Validate and copy one closed native contact-search page DATA.

When OWNER is non-nil, also prove that every branch and group identity stays
inside the semantic request scope."
  (unless (qq-api--exact-object-keys-p data '(results next_cursor))
    (error "qq: emacs_search_contacts returned an invalid page"))
  (let ((results (alist-get 'results data))
        (cursor (alist-get 'next_cursor data)))
    (unless (and (listp results) (proper-list-p results))
      (error "qq: emacs_search_contacts.results must be an array"))
    (let ((seen (make-hash-table :test #'equal))
          validated)
      (setq validated
            (cl-loop
             for result in results
             for index from 0
             for context = (format "emacs_search_contacts.results[%d]" index)
             for item = (qq-api--validate-contact-search-result result index)
             for identity = (cons (alist-get 'kind item)
                                  (alist-get 'user_id item))
             do (when (gethash identity seen)
                  (error "qq: %s duplicates contact identity %s:%s"
                         context (car identity) (cdr identity)))
             do (puthash identity t seen)
             do (when owner
                  (qq-api--validate-contact-search-owner-result
                   item owner context))
             collect item))
      (unless (or (null cursor) (qq-api--uuid-v4-string-p cursor))
        (error "qq: emacs_search_contacts.next_cursor must be UUIDv4 or null"))
      `((results . ,validated)
        (next_cursor . ,cursor)))))

(defun qq-api--contact-search-owner-params (scope query group-id limit)
  "Validate and return complete contact-search owner parameters.

SCOPE is one of `friends', `group-members', or
`friends-and-group-members'.  QUERY, semantic SCOPE, GROUP-ID when required,
and LIMIT are repeated for server-side cursor owner proof."
  (unless (stringp query)
    (user-error "qq: contact search query must be a string"))
  (setq query (string-trim query))
  (when (or (string-empty-p query) (> (length query) 512))
    (user-error "qq: contact search query must contain 1 to 512 characters"))
  (setq limit (or limit 50))
  (unless (and (integerp limit) (<= 1 limit 100))
    (user-error "qq: contact search limit must be between 1 and 100"))
  (pcase scope
    ('friends
     (when group-id
       (user-error "qq: friends contact search forbids group-id"))
     `((scope . "friends")
       (query . ,query)
       (limit . ,limit)))
    ('group-members
     (unless (qq-protocol-group-uin-p group-id)
       (user-error
        "qq: group-members contact search requires a canonical uint32 group UIN"))
     `((scope . "group_members")
       (group_id . ,group-id)
       (query . ,query)
       (limit . ,limit)))
    ('friends-and-group-members
     (unless (qq-protocol-group-uin-p group-id)
       (user-error
        "qq: combined contact search requires a canonical uint32 group UIN"))
     `((scope . "friends_and_group_members")
       (group_id . ,group-id)
       (query . ,query)
       (limit . ,limit)))
    (_
     (user-error "qq: invalid contact search scope %S" scope))))

(defun qq-api--contact-search-callback (owner callback errback response)
  "Validate contact-search RESPONSE before invoking CALLBACK."
  (let (page valid-p)
    (condition-case error-data
        (setq page (qq-api--validate-contact-search-page
                    (qq-api--response-data response) owner)
              valid-p t)
      (error
       (funcall (or errback #'qq-api--default-error)
                response (error-message-string error-data))))
    (when valid-p (funcall callback page))))

(defun qq-api-search-contacts-start
    (scope query callback &optional errback group-id limit)
  "Start native contact search in semantic SCOPE for QUERY.

CALLBACK receives a validated closed page.  GROUP-ID is forbidden for
`friends' and required for `group-members' and
`friends-and-group-members'.  LIMIT defaults to 50."
  (let ((owner (qq-api--contact-search-owner-params
                scope query group-id limit)))
    (qq-api-call
     "emacs_search_contacts"
     (cons '(kind . "start") owner)
     (apply-partially
      #'qq-api--contact-search-callback owner callback errback)
     errback)))

(defun qq-api-search-contacts-next
    (scope cursor query callback &optional errback group-id limit)
  "Continue UUID CURSOR using its complete semantic owner proof.

SCOPE, QUERY, GROUP-ID, and LIMIT must describe the original start request."
  (unless (qq-api--uuid-v4-string-p cursor)
    (user-error "qq: contact search cursor must be a canonical UUIDv4 string"))
  (let ((owner (qq-api--contact-search-owner-params
                scope query group-id limit)))
    (qq-api-call
     "emacs_search_contacts"
     (append `((kind . "next") (cursor . ,cursor)) owner)
     (apply-partially
      #'qq-api--contact-search-callback owner callback errback)
     errback)))

(defun qq-api-get-base-emoji
    (emoji-id callback &optional errback emoji-type download hints)
  "Fetch QQ base emoji resource for EMOJI-ID and pass it to CALLBACK.

HINTS may carry native `sticker_id', `sticker_pack_id' and `description'
needed to resolve newer animated faces absent from static face_config."
  (qq-api-call
   "get_base_emoji"
   `((emoji_id . ,(format "%s" emoji-id))
     ,@(when emoji-type `((emoji_type . ,emoji-type)))
     ,@(when-let* ((value (alist-get 'sticker_id hints)))
         `((sticker_id . ,value)))
     ,@(when-let* ((value (alist-get 'sticker_pack_id hints)))
         `((sticker_pack_id . ,value)))
     ,@(when-let* ((value (alist-get 'description hints)))
         `((description . ,value)))
     (download . ,(if (or (null download) download) t :false)))
   (lambda (response)
     (funcall callback (qq-api--response-data response)))
   errback))

(defun qq-api-fetch-custom-face-info (callback &optional errback count)
  "Fetch detailed favorite custom-face resources and pass them to CALLBACK."
  (qq-api-call
   "fetch_custom_face_info"
   `((count . ,(max 1 (or count 48))))
   (lambda (response)
     (funcall callback (qq-api--response-data response)))
   errback))

(defun qq-api--refresh-for-notice (notice)
  "Run light refresh actions that correspond to NOTICE."
  (pcase (alist-get 'notice_type notice)
    ("friend_add" (qq-api-refresh-friend-categories))
    (_ nil)))

(defun qq-api--handle-meta-event (event)
  "Handle websocket meta EVENT."
  (pcase (alist-get 'meta_event_type event)
    ("lifecycle"
     (when (equal (alist-get 'sub_type event) "connect")
       (qq-state-set-connection-status 'ready)
       (qq-api-bootstrap)))
    ("heartbeat"
     (qq-state-set-last-heartbeat)
     (when-let* ((status (alist-get 'status event nil nil #'eq)))
       (qq-state-set-status status)))))

(defun qq-api-set-input-status (user-id event-type &optional callback errback)
  "Tell USER-ID our input status (OneBot `set_input_status').

EVENT-TYPE 1 = typing (show 正在输入 on peer); 0 = cancel.
CALLBACK / ERRBACK optional; default errors are silent (ephemeral signal)."
  (qq-api-call
   "set_input_status"
   `((user_id . ,(format "%s" user-id))
     (event_type . ,(or event-type 1)))
   (or callback #'ignore)
   (or errback #'ignore)))

(defun qq-api--handle-notice (notice)
  "Handle websocket NOTICE event."
  (pcase (alist-get 'notice_type notice)
    ("emacs_read_state"
     (let* ((event
             (qq-protocol-validate-emacs-read-state-notice
              notice "websocket event"))
            (chat (alist-get 'chat event))
            (read-state (alist-get 'read_state event))
            (session-key (qq-api-session-key-from-locator chat)))
       (when (qq-api--accept-read-observation-p
              session-key (qq-api--next-read-observation-token))
         (qq-state-apply-session-read-state session-key read-state))))
    ("friend_recall"
     (qq-state-apply-recall
      (qq-state-session-key 'private (alist-get 'user_id notice))
      (alist-get 'message_id notice)))
    ("group_recall"
     (qq-state-apply-recall
      (qq-state-session-key 'group (alist-get 'group_id notice))
      (alist-get 'message_id notice)))
    ("group_msg_emoji_like"
     (qq-state-apply-emoji-like-notice
      (qq-state-session-key 'group (alist-get 'group_id notice))
      notice))
    ("notify"
     (pcase (alist-get 'sub_type notice)
       ("input_status"
        (qq-state-apply-input-status notice))
       ("poke"
        (qq-state-apply-poke-notice notice))
       ("gray_tip"
        (qq-state-apply-gray-tip-notice notice))
       (_ (qq-api--refresh-for-notice notice))))
    (_ (qq-api--refresh-for-notice notice))))

(defun qq-api--handle-request (request)
  "Handle websocket REQUEST event."
  (qq-state-add-request request))

(defun qq-api-handle-event (event)
  "Handle websocket EVENT emitted by transport."
  (pcase (alist-get 'post_type event)
    ((or "message" "message_sent")
     (qq-state-merge-live-message event))
    ("meta_event"
     (qq-api--handle-meta-event event))
    ("notice"
     (qq-api--handle-notice event))
    ("request"
     (qq-api--handle-request event))
    (_ nil)))

(add-hook 'qq-transport-event-hook #'qq-api-handle-event)

(provide 'qq-api)

;;; qq-api.el ends here
