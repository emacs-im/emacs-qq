;;; qq-api.el --- OneBot actions and event handlers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; High-level NapCat actions, bootstrap, and event handling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)
(require 'qq-state)
(require 'qq-transport)

(declare-function qq-transport-cancel "qq-transport" (echo))
(declare-function qq-state-apply-poke-notice "qq-state" (notice))

(defvar qq-api--read-operations (make-hash-table :test #'equal)
  "In-flight optimistic read operations keyed by session key.")

(defvar qq-api--read-operation-counter 0
  "Monotonic token used to reject stale read callbacks.")

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

(defun qq-api-cancel-request (request-token)
  "Cancel local callback ownership for REQUEST-TOKEN."
  (and request-token (qq-transport-cancel request-token)))

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
  (and (stringp value)
       (string-match-p "\\`[0-9]+\\'" value)))

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
       (unless (qq-api-user-id-p (alist-get 'group_id chat))
         (qq-api--signal-schema-error
          protocol-p "qq: %s group_id must be a decimal string" context)))
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

(defun qq-api--validate-native-video-remote (remote context protocol-p)
  "Validate native video REMOTE discriminant for CONTEXT."
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
                payload '(id) '(resultId chainCount))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face payload has invalid fields" context))
       (unless (stringp (alist-get 'id payload))
         (qq-api--signal-schema-error
          protocol-p "qq: %s face.id must be a string" context))
       (qq-api--validate-string-fields
        payload '(resultId) context protocol-p)
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
        (alist-get 'remote payload) context protocol-p))
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
  (qq-api-call
   "get_recent_contact"
   `((count . ,(max 1 qq-recent-contact-count)))
   (lambda (response)
     (let ((contacts (qq-api--response-data response)))
       (qq-state-apply-recent-contacts contacts)
       (when callback (funcall callback contacts))))
   errback))

(defun qq-api-refresh-friend-list (&optional callback errback)
  "Refresh friend list, then call CALLBACK with it."
  (interactive)
  (qq-api-call
   "get_friend_list"
   nil
   (lambda (response)
     (let ((friends (qq-api--response-data response)))
       (qq-state-apply-friends friends)
       (when callback (funcall callback friends))))
   errback))

(defun qq-api-refresh-group-list (&optional callback errback)
  "Refresh group list, then call CALLBACK with it."
  (interactive)
  (qq-api-call
   "get_group_list"
   nil
   (lambda (response)
     (let ((groups (qq-api--response-data response)))
       (qq-state-apply-groups groups)
       (when callback (funcall callback groups))))
   errback))

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
         (qq-api-refresh-group-list
          (lambda (_groups) (recent))
          (lambda (response reason)
            (qq-api--default-error response reason)
            (recent))))
       (friends ()
         (qq-api-refresh-friend-list
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
  (let* ((session (qq-state-session session-key))
         (type (or (alist-get 'type session)
                   (qq-state-session-key-type session-key)))
         (target-id (or (alist-get 'target-id session)
                        (qq-state-session-key-target-id session-key))))
    (pcase type
      ('group
       `((group_id . ,target-id)))
      ('dataline
       `((chat_type . ,(or (alist-get 'chat-type session) 8))
         (peer_uid . ,(or (alist-get 'peer-uid session)
                          target-id))))
      ('service
       `((chat_type . ,(or (alist-get 'chat-type session) 103))
         (peer_uid . ,(or (alist-get 'peer-uid session)
                          target-id))))
      ('private
       `((user_id . ,(or (alist-get 'peer-uin session)
                         target-id))))
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
  (let* ((session (qq-state-session session-key))
         (raw-type (or (alist-get 'type session)
                       (qq-state-session-key-type session-key)))
         (type (pcase raw-type
                 ((or 'group "group") 'group)
                 ((or 'private "private") 'private)
                 ((or 'dataline "dataline") 'dataline)
                 ((or 'service "service") 'service)
                 (_ raw-type)))
         (target-id (or (alist-get 'target-id session)
                        (qq-state-session-key-target-id session-key))))
    (pcase type
      ((or 'group 'private)
       (qq-api-chat-locator session-key))
      ('dataline
       (let ((peer-uid (or (alist-get 'peer-uid session) target-id))
             (chat-type (alist-get 'chat-type session)))
         (unless peer-uid
           (error "qq: dataline session %s has no peer uid" session-key))
         `((kind . "dataline")
           (peer_uid . ,(format "%s" peer-uid))
           (variant . ,(pcase (and chat-type (format "%s" chat-type))
                         ("8" "desktop")
                         ("134" "mobile")
                         (_ (error "qq: unsupported dataline chat type %s"
                                   chat-type)))))))
      ('service
       (let ((peer-uid (or (alist-get 'peer-uid session) target-id)))
         (unless peer-uid
           (error "qq: service session %s has no peer uid" session-key))
         `((kind . "service")
           (peer_uid . ,(format "%s" peer-uid)))))
      (_ (error "qq: unsupported Emacs session type %s" type)))))

(defun qq-api--session-emacs-params (session-key)
  "Return native Emacs action params for SESSION-KEY."
  `((chat . ,(qq-api--session-emacs-locator session-key))))

(defun qq-api-fetch-session-read-state (session-key &optional callback errback)
  "Fetch the official Linux QQ read position for SESSION-KEY.

NapCat resolves the kernel first-unread sequence to the hard-cut NT snowflake
in `first_unread.message_id'.  CALLBACK receives the raw read-state
payload after it has been applied to `qq-state'."
  (qq-api-call
   "emacs_get_read_state"
   (qq-api--session-emacs-params session-key)
   (lambda (response)
     (let ((read-state (qq-api--response-data response)))
       (qq-state-apply-session-read-state session-key read-state)
       (when callback
         (funcall callback read-state))))
   errback))

(defun qq-api-fetch-history-page (session-key cursor direction
                                              &optional callback errback count)
  "Fetch one history page for SESSION-KEY at CURSOR in DIRECTION.

DIRECTION is `older' or `newer'.  A nil CURSOR pulls the latest page.  On the
current Linux QQ kernel `reverse_order' true walks older and false walks newer.

COUNT overrides `qq-history-fetch-count' when non-nil (used by jump seek).

CALLBACK receives the merge-history plist
\(`:added-count', `:message-count', `:oldest-message-id', …).
ERRBACK receives (RESPONSE REASON)."
  (let* ((session (qq-state-session session-key))
         (type (or (alist-get 'type session)
                   (qq-state-session-key-type session-key)))
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
    (qq-api-call
     action
     params
     (lambda (response)
       (let* ((data (qq-api--response-data response))
              (messages (alist-get 'messages data nil nil #'eq))
              (meta (qq-state-merge-history session-key messages)))
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
  (let* ((session (qq-state-session session-key))
         (raw-type (or (alist-get 'type session)
                       (qq-state-session-key-type session-key)))
         (type (pcase raw-type
                 ((or 'group "group") "group")
                 ((or 'private "private") "private")
                 ((or 'dataline "dataline") "dataline")
                 ((or 'service "service") "service")
                 (_ raw-type)))
         (target-id
          (if (equal type "private")
              (or (alist-get 'peer-uin session)
                  (alist-get 'target-id session)
                  (qq-state-session-key-target-id session-key))
            (or (alist-get 'target-id session)
                (qq-state-session-key-target-id session-key)))))
    (unless (member type '("group" "private"))
      (user-error "qq: forwarding to %s sessions is not supported" type))
    (unless (qq-api-user-id-p target-id)
      (user-error
       "qq: forward session %s requires a decimal string target id"
       session-key))
    (if (equal type "group")
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
  (let* ((session (qq-state-session session-key))
         (type (or (alist-get 'type session)
                   (qq-state-session-key-type session-key)))
         (target-id (or (alist-get 'target-id session)
                        (qq-state-session-key-target-id session-key)))
         (params `((message_id . ,message-id)
                   (count . ,(max 1 count)))))
    (pcase type
      ('group
       (append params `((group_id . ,(format "%s" target-id)))))
      ('dataline
       (append params
               `((chat_type . ,(or (alist-get 'chat-type session) 8))
                 (peer_uid . ,(or (alist-get 'peer-uid session)
                                  target-id)))))
      ('service
       (append params
               `((chat_type . ,(or (alist-get 'chat-type session) 103))
                 (peer_uid . ,(or (alist-get 'peer-uid session)
                                  target-id)))))
      (_
       (append params
               `((user_id . ,(format "%s"
                                     (or (alist-get 'peer-uin session)
                                         target-id)))))))))

(defun qq-api-fetch-history-around (session-key message-id callback &optional errback count)
  "Fetch a history window around MESSAGE-ID (NapCat `get_msg_history_around').

Fork action: older+newer pages around the NT snowflake center (telega around).
CALLBACK receives the merge-history plist.  On action-missing/network error,
ERRBACK is called so the client can fall back to single-side seek."
  (let ((n (max 1 (or count
                      (and (boundp 'qq-chat-jump-history-count)
                           qq-chat-jump-history-count)
                      qq-history-fetch-count))))
    (qq-api-call
     "get_msg_history_around"
     (qq-api--history-around-params session-key message-id n)
     (lambda (response)
       (let* ((data (qq-api--response-data response))
              (messages (or (alist-get 'messages data nil nil #'eq)
                            (and (listp data) data)))
              (meta (qq-state-merge-history session-key messages)))
         (when callback
           (funcall callback meta))))
     errback)))

(defun qq-api--read-operation-current-p (session-key token)
  "Return non-nil when TOKEN still owns SESSION-KEY's read operation."
  (equal token
         (plist-get (gethash session-key qq-api--read-operations) :token)))

(defun qq-api--start-mark-session-read (session-key cleared-count)
  "Start one read request for SESSION-KEY owning CLEARED-COUNT unread rows."
  (let* ((token (cl-incf qq-api--read-operation-counter))
         (operation (list :token token
                          :cleared (max 0 cleared-count)
                          :queued 0)))
    (puthash session-key operation qq-api--read-operations)
    (qq-api-call
     "emacs_mark_read"
     (qq-api--session-emacs-params session-key)
     (lambda (_response)
       (when (qq-api--read-operation-current-p session-key token)
         (let* ((current (gethash session-key qq-api--read-operations))
                (queued (or (plist-get current :queued) 0)))
           (remhash session-key qq-api--read-operations)
           ;; Rows cleared while the first request was already in flight need
           ;; their own server acknowledgement.  They stay locally clear, but
           ;; remain owned by the follow-up operation for rollback purposes.
           (when (> queued 0)
             (qq-api--start-mark-session-read session-key queued)))))
     (lambda (response reason)
       (when (qq-api--read-operation-current-p session-key token)
         (let* ((current-operation
                 (gethash session-key qq-api--read-operations))
                (owned (+ (or (plist-get current-operation :cleared) 0)
                          (or (plist-get current-operation :queued) 0)))
                (session (qq-state-session session-key))
                (current-unread
                 (or (and session (alist-get 'unread-count session)) 0)))
           (remhash session-key qq-api--read-operations)
           (when (and session (> owned 0))
             (qq-state-set-session-unread
              session-key (+ current-unread owned)))
           (qq-api--default-error response reason)))))
    token))

(defun qq-api-mark-session-read (session-key)
  "Mark SESSION-KEY as read locally and in NapCat without losing races.

Concurrent calls coalesce behind one request.  Unread rows that arrive while
that request is in flight are cleared optimistically, then acknowledged by a
follow-up request.  A failure restores exactly the rows owned by the failed
operation in addition to any newer unread rows."
  (interactive)
  (let* ((session (qq-state-session session-key))
         (unread (or (and session (alist-get 'unread-count session)) 0))
         (operation (gethash session-key qq-api--read-operations)))
    (when (> unread 0)
      (qq-state-clear-session-unread session-key))
    (if operation
        (progn
          (when (> unread 0)
            (setq operation
                  (plist-put operation :queued
                             (+ (or (plist-get operation :queued) 0)
                                unread)))
            (puthash session-key operation qq-api--read-operations))
          (plist-get operation :token))
      (qq-api--start-mark-session-read session-key unread))))

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

(defun qq-api-send-message (session-key segments &optional raw-message)
  "Send SEGMENTS to SESSION-KEY.

Insert a local pending message immediately and update it after the response.
RAW-MESSAGE, when non-nil, overrides the optimistic raw-message field used for
local pending rendering.

The NapCat hard-cut returns `message_id' as the NT snowflake string; that value
is stored as the message `server-id' and becomes the timeline anchor."
  (unless (qq-state-session-sendable-p session-key)
    (user-error "qq: this session is read-only"))
  (let* ((segments (copy-tree (or segments '())))
         (pending (qq-state-insert-pending-message session-key segments raw-message))
         (local-id (alist-get 'local-id pending))
         (params (append
                  (qq-api--session-request-params session-key)
                  `((message . ,segments)))))
    (qq-api-call
     "send_msg"
     params
     (lambda (response)
       (let* ((data (qq-api--response-data response))
              (message-id (alist-get 'message_id data nil nil #'eq)))
         (qq-state-mark-pending-message-sent session-key local-id message-id)))
     (lambda (response reason)
       (qq-state-mark-pending-message-failed session-key local-id reason)
       (qq-api--default-error response reason)))))

(defun qq-api-send-text (session-key text &optional reply-to-message-id)
  "Send TEXT to SESSION-KEY.

Insert a local pending message immediately and update it after the response.
When REPLY-TO-MESSAGE-ID is non-nil, send the text as a reply."
  (interactive)
  (qq-api-send-message
   session-key
   (qq-api--send-text-segments text reply-to-message-id)
   text))

(defun qq-api-send-poke (session-key &optional target-id callback errback)
  "Send a poke to SESSION-KEY, optionally targeting TARGET-ID.

Private chats poke the peer.  Group chats require TARGET-ID, normally the
sender of the message at point.  A successful action is reflected locally as
a poke notice; a matching websocket notice is deduplicated by its local
second-level anchor."
  (let* ((session (qq-state-session session-key))
         (type (or (and session (alist-get 'type session))
                   (qq-state-session-key-type session-key)))
         (peer-id (qq-state-session-key-target-id session-key))
         (target (if (eq type 'group)
                     target-id
                   (or target-id
                       (and session (alist-get 'target-id session))
                       peer-id)))
         (params
          (pcase type
            ('group
             (unless (qq-api-user-id-p target)
               (user-error "qq: group poke requires a decimal target QQ"))
             `((group_id . ,peer-id) (user_id . ,target)))
            ('private
             (unless (qq-api-user-id-p target)
               (user-error "qq: private poke requires a decimal peer QQ"))
             `((user_id . ,target)))
            (_
             (user-error "qq: poke is unsupported for %s sessions" type)))))
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
                    (target_id . ,target))
                `((user_id . ,target)
                  (sender_id . ,self-id)
                  (target_id . ,target))))))
       (when callback
         (funcall callback response)))
     (or errback
         (lambda (response reason)
           (qq-api--default-error response reason))))))

(defun qq-api-delete-message (message-id)
  "Recall MESSAGE-ID (NT snowflake string) via NapCat and mark it recalled."
  (interactive)
  (setq message-id
        (qq-api-validate-message-id message-id "delete_msg"))
  (qq-api-call
   "delete_msg"
   `((message_id . ,message-id))
   (lambda (_response)
     (qq-state-apply-recall message-id))))

(defun qq-api-recall-poke (message-id &optional callback errback)
  "Recall poke MESSAGE-ID via QQ's native recallNudge path.

Pokes are gray-tip records, so they must not be sent through `delete_msg'."
  (setq message-id
        (qq-api-validate-message-id message-id "recall_poke"))
  (qq-api-call
   "recall_poke"
   `((message_id . ,message-id))
   (lambda (response)
     (qq-state-apply-recall message-id)
     (when callback
       (funcall callback response)))
   (or errback
       (lambda (response reason)
         (qq-api--default-error response reason)))))

(defun qq-api-set-message-emoji-like
    (message-id emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on MESSAGE-ID according to SET.

MESSAGE-ID remains the original NapCat NT snowflake string.  On success,
optimistically apply one local reaction delta; the subsequent NapCat notice
reconciles it with the authoritative aggregate count."
  (setq message-id
        (qq-api-validate-message-id message-id "set_msg_emoji_like"))
  (setq emoji-id (format "%s" emoji-id))
  (unless (string-match-p "\\`[0-9]+\\'" emoji-id)
    (user-error "qq: reaction emoji_id must be a decimal string"))
  (let ((set (and set t)))
    (qq-api-call
     "set_msg_emoji_like"
     `((message_id . ,message-id)
       (emoji_id . ,emoji-id)
       (set . ,(if set t :false)))
     (lambda (response)
       (when-let* ((self-id (qq-state-self-user-id)))
         (qq-state-apply-emoji-like-notice
          `((notice_type . "group_msg_emoji_like")
            (message_id . ,message-id)
            (user_id . ,self-id)
            (is_add . ,(if set t :false))
            (likes . (((emoji_id . ,emoji-id)))))))
       (when callback
         (funcall callback response)))
     (or errback #'qq-api--default-error))))

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
         (let ((data (qq-api--response-data response)))
           (unless (equal (alist-get 'user_id data) user-id)
             (error "qq: emacs_get_user_like returned a different user identity"))
           (funcall callback (alist-get 'total_count data)))
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
    (user-error "qq: group profile requires a decimal string group id"))
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

(defun qq-api-get-base-emoji (emoji-id callback &optional errback emoji-type download)
  "Fetch QQ base emoji resource for EMOJI-ID and pass it to CALLBACK."
  (qq-api-call
   "get_base_emoji"
   `((emoji_id . ,(format "%s" emoji-id))
     ,@(when emoji-type `((emoji_type . ,emoji-type)))
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
    ("friend_add" (qq-api-refresh-friend-list))
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
    ((or "friend_recall" "group_recall")
     (qq-state-apply-recall (alist-get 'message_id notice)))
    ("group_msg_emoji_like"
     (qq-state-apply-emoji-like-notice notice))
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
