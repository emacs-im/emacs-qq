;;; qq-gateway-message.el --- Native Gateway message projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Closed protocol decoder and selected-account projection for native Gateway
;; message and recall events.  Events for every managed account remain
;; observable on `qq-gateway-message-event-hook'; only the exact selected
;; `(account_id . generation)' owner may mutate the legacy single-account QQ
;; timeline.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-protocol)
(require 'qq-state)

(defvar qq-gateway-message-event-hook nil
  "Hook called with EVENT and validated DATA for every native message event.")

(defvar qq-gateway-message-projection-error-hook nil
  "Hook called with EVENT, DATA, and REASON when a valid event cannot project.")

(defvar qq-gateway-message--projection-owner nil
  "Exact `(ACCOUNT-ID . GENERATION)' currently stored in `qq-state'.")

(defvar qq-gateway-message--peer-uin-by-uid
  (make-hash-table :test #'equal)
  "Private UID to UIN map owned by the current projection generation.")

(defvar qq-gateway-message--pending-recalls
  (make-hash-table :test #'equal)
  "Sequence-scoped recalls awaiting a message in the current projection.")

(defvar qq-gateway-message--pending-reactions
  (make-hash-table :test #'equal)
  "Sequence-scoped reaction events awaiting a projected message.")

(defvar qq-gateway-message--pending-essences
  (make-hash-table :test #'equal)
  "Native target-scoped essence events awaiting a projected message.")

(defvar qq-gateway-message--essence-revisions
  (make-hash-table :test #'equal)
  "Authoritative essence-event revision observed per native target.")

(defvar qq-gateway-message--pending-sends
  (make-hash-table :test #'equal)
  "Client-sequence receipts awaiting an authoritative self message event.")

(defvar qq-gateway-message--live-frontiers
  (make-hash-table :test #'equal)
  "Newest live message identity and sequence observed per projected session.")

(defconst qq-gateway-message--max-uint64-decimal
  "18446744073709551615"
  "Largest sequence accepted by the native Gateway protocol.")

(defun qq-gateway-message--uint32-p (value)
  "Return non-nil when VALUE is an unsigned 32-bit integer."
  (and (integerp value) (<= 0 value) (<= value #xffffffff)))

(defun qq-gateway-message--int32-p (value)
  "Return non-nil when VALUE is a signed 32-bit integer."
  (and (integerp value) (<= (- #x80000000) value) (< value #x80000000)))

(defun qq-gateway-message--closed-object-p (object required optional)
  "Return non-nil when OBJECT has exactly REQUIRED plus optional OPTIONAL keys."
  (and (listp object)
       (let ((seen nil)
             valid)
         (setq valid
               (cl-every
                (lambda (entry)
                  (let ((key (and (consp entry) (car entry))))
                    (and (symbolp key)
                         (memq key (append required optional))
                         (not (memq key seen))
                         (progn (push key seen) t))))
                object))
         (and valid (cl-every (lambda (key) (assq key object)) required)))))

(defun qq-gateway-message--validate-owner-data (data payload-key)
  "Validate event DATA containing account owner and PAYLOAD-KEY."
  (unless (qq-gateway--exact-object-keys-p
           data (list 'account_id 'generation payload-key))
    (error "qq: Gateway message event has invalid outer fields"))
  (unless (qq-gateway--non-empty-string-p (alist-get 'account_id data))
    (error "qq: Gateway message event account_id must be opaque string"))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'generation data) t)
    (error "qq: Gateway message event generation must be decimal string"))
  data)

(defun qq-gateway-message--validate-endpoint (endpoint context)
  "Validate and copy message ENDPOINT for CONTEXT."
  (unless (qq-gateway-message--closed-object-p endpoint nil '(uin uid))
    (error "qq: Gateway %s endpoint has invalid fields" context))
  (when (assq 'uin endpoint)
    (unless (qq-gateway--canonical-decimal-p (alist-get 'uin endpoint))
      (error "qq: Gateway %s endpoint UIN must be decimal string" context)))
  (when (assq 'uid endpoint)
    (unless (qq-gateway--non-empty-string-p (alist-get 'uid endpoint))
      (error "qq: Gateway %s endpoint UID must be opaque string" context)))
  (copy-tree endpoint))

(defun qq-gateway-message--validate-conversation (conversation)
  "Validate and copy native message CONVERSATION."
  (unless (listp conversation)
    (error "qq: Gateway message conversation must be an object"))
  (pcase (alist-get 'kind conversation)
    ("private"
     (unless (qq-gateway-message--closed-object-p
              conversation '(kind) '(name))
       (error "qq: Gateway private conversation has invalid fields"))
     (when (assq 'name conversation)
       (unless (stringp (alist-get 'name conversation))
         (error "qq: Gateway private conversation name must be string"))))
    ("group"
     (unless (qq-gateway-message--closed-object-p
              conversation '(kind group_uin) '(group_name sender_card))
       (error "qq: Gateway group conversation has invalid fields"))
     (unless (qq-gateway--canonical-decimal-p
              (alist-get 'group_uin conversation))
       (error "qq: Gateway group UIN must be decimal string"))
     (dolist (key '(group_name sender_card))
       (when (assq key conversation)
         (unless (stringp (alist-get key conversation))
           (error "qq: Gateway group %s must be string" key)))))
    ("temp"
     (unless (qq-gateway-message--closed-object-p
              conversation '(kind) '(name from_tiny_id to_tiny_id))
       (error "qq: Gateway temp conversation has invalid fields"))
     (dolist (key '(name from_tiny_id to_tiny_id))
       (when (assq key conversation)
         (unless (stringp (alist-get key conversation))
           (error "qq: Gateway temp %s must be string" key)))))
    (_ (error "qq: Gateway message conversation has unknown kind")))
  (copy-tree conversation))

(defun qq-gateway-message--validate-segment (segment)
  "Validate and copy one native message SEGMENT."
  (unless (qq-gateway--exact-object-keys-p segment '(kind payload))
    (error "qq: Gateway message segment has invalid fields"))
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (pcase kind
      ("text"
       (unless (and (qq-gateway--exact-object-keys-p payload '(text))
                    (stringp (alist-get 'text payload)))
         (error "qq: Gateway text segment is malformed")))
      ("face"
       (let ((id (alist-get 'id payload)))
         (unless (and (qq-gateway--exact-object-keys-p payload '(id))
                      (qq-gateway--canonical-decimal-p id t)
                      (qq-gateway--decimal-less-p id "260"))
           (error "qq: Gateway base face segment is malformed"))))
      ("at"
       (unless (qq-gateway-message--closed-object-p payload '(qq) '(name))
         (error "qq: Gateway at segment has invalid fields"))
       (let ((qq (alist-get 'qq payload)))
         (unless (or (equal qq "all")
                     (qq-gateway--canonical-decimal-p qq))
           (error "qq: Gateway at target must be exact UIN or all")))
       (when (assq 'name payload)
         (unless (stringp (alist-get 'name payload))
           (error "qq: Gateway at display name must be string"))))
      ("reply"
       (unless (qq-gateway--exact-object-keys-p payload '(target))
         (error "qq: Gateway reply segment has invalid fields"))
       (let ((target (alist-get 'target payload)))
         (unless (and (qq-gateway--exact-object-keys-p
                       target '(kind message_id))
                      (equal (alist-get 'kind target) "unresolved")
                      (qq-gateway--canonical-decimal-p
                       (alist-get 'message_id target)))
           (error "qq: Gateway reply target is malformed"))))
      ("unsupported"
       (unless (qq-gateway-message--closed-object-p
                payload '(native_keys summary) '(raw))
         (error "qq: Gateway unsupported segment has invalid fields"))
       (unless (and (listp (alist-get 'native_keys payload))
                    (cl-every #'stringp (alist-get 'native_keys payload))
                    (stringp (alist-get 'summary payload)))
         (error "qq: Gateway unsupported segment metadata is malformed"))
       (when (assq 'raw payload)
         (let ((raw (alist-get 'raw payload)))
           (unless (and (qq-gateway--exact-object-keys-p raw '(fallback_text))
                        (stringp (alist-get 'fallback_text raw)))
             (error "qq: Gateway unsupported segment raw data is malformed")))))
      (_ (error "qq: Gateway message segment has unknown kind %S" kind))))
  (copy-tree segment))

(defun qq-gateway-message--validate-message (message)
  "Validate and copy a closed native MESSAGE snapshot."
  (unless (qq-gateway--exact-object-keys-p
           message
           '(message_id sent_at sender recipient conversation sequence
             client_sequence random message_type sub_type segments))
    (error "qq: Gateway message snapshot has invalid fields"))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'message_id message))
    (error "qq: Gateway message_id must be an exact decimal string"))
  (unless (and (integerp (alist-get 'sent_at message))
               (>= (alist-get 'sent_at message) 0))
    (error "qq: Gateway message sent_at must be non-negative integer"))
  (qq-gateway-message--validate-endpoint
   (alist-get 'sender message) "sender")
  (qq-gateway-message--validate-endpoint
   (alist-get 'recipient message) "recipient")
  (qq-gateway-message--validate-conversation
   (alist-get 'conversation message))
  (dolist (key '(sequence client_sequence))
    (unless (qq-gateway--canonical-decimal-p (alist-get key message) t)
      (error "qq: Gateway message %s must be decimal string" key)))
  (unless (qq-gateway-message--uint32-p (alist-get 'random message))
    (error "qq: Gateway message random must be uint32"))
  (dolist (key '(message_type sub_type))
    (unless (qq-gateway-message--int32-p (alist-get key message))
      (error "qq: Gateway message %s must be int32" key)))
  (unless (listp (alist-get 'segments message))
    (error "qq: Gateway message segments must be an array"))
  (dolist (segment (alist-get 'segments message))
    (qq-gateway-message--validate-segment segment))
  (copy-tree message))

(defun qq-gateway-message--validate-recall-conversation (conversation)
  "Validate and copy recall CONVERSATION."
  (pcase (alist-get 'kind conversation)
    ("private"
     (unless (and (qq-gateway--exact-object-keys-p
                   conversation '(kind peer_uid))
                  (qq-gateway--non-empty-string-p
                   (alist-get 'peer_uid conversation)))
       (error "qq: Gateway private recall conversation is malformed")))
    ("group"
     (unless (and (qq-gateway--exact-object-keys-p
                   conversation '(kind group_uin))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'group_uin conversation)))
       (error "qq: Gateway group recall conversation is malformed")))
    (_ (error "qq: Gateway recall conversation has unknown kind")))
  (copy-tree conversation))

(defun qq-gateway-message--validate-recall-target (target)
  "Validate and copy recall TARGET."
  (pcase (alist-get 'kind target)
    ("message"
     (unless (and (qq-gateway--exact-object-keys-p
                   target '(kind message_id sequence))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'message_id target))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'sequence target)))
       (error "qq: Gateway message recall target is malformed")))
    ("sequence"
     (unless (and (qq-gateway--exact-object-keys-p target '(kind sequence))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'sequence target)))
       (error "qq: Gateway sequence recall target is malformed")))
    (_ (error "qq: Gateway recall target has unknown kind")))
  (copy-tree target))

(defun qq-gateway-message--validate-recall (recall)
  "Validate and copy one closed native RECALL snapshot."
  (unless (qq-gateway-message--closed-object-p
           recall '(conversation target) '(author_uid operator_uid tip))
    (error "qq: Gateway recall snapshot has invalid fields"))
  (qq-gateway-message--validate-recall-conversation
   (alist-get 'conversation recall))
  (qq-gateway-message--validate-recall-target (alist-get 'target recall))
  (dolist (key '(author_uid operator_uid tip))
    (when (assq key recall)
      (unless (qq-gateway--non-empty-string-p (alist-get key recall))
        (error "qq: Gateway recall %s must be non-empty string" key))))
  (copy-tree recall))

(defun qq-gateway-message--validate-message-data (data)
  "Validate and copy outer message event DATA."
  (qq-gateway-message--validate-owner-data data 'message)
  (qq-gateway-message--validate-message (alist-get 'message data))
  (copy-tree data))

(defun qq-gateway-message--validate-recall-data (data)
  "Validate and copy outer recall event DATA."
  (qq-gateway-message--validate-owner-data data 'recall)
  (qq-gateway-message--validate-recall (alist-get 'recall data))
  (copy-tree data))

(defun qq-gateway-message--validate-poke (poke)
  "Validate and copy one authoritative group POKE snapshot."
  (unless (qq-gateway-message--closed-object-p
           poke
           '(message_id sent_at sequence conversation actor_uin target_uin
             recall)
           '(action action_image_url suffix))
    (error "qq: Gateway poke snapshot has invalid fields"))
  (dolist (key '(message_id sequence actor_uin target_uin))
    (unless (qq-gateway--canonical-decimal-p (alist-get key poke))
      (error "qq: Gateway poke %s must be exact decimal string" key)))
  (unless (and (integerp (alist-get 'sent_at poke))
               (> (alist-get 'sent_at poke) 0))
    (error "qq: Gateway poke sent_at must be positive integer"))
  (let ((conversation (alist-get 'conversation poke)))
    (unless (and (qq-gateway--exact-object-keys-p
                  conversation '(kind group_uin))
                 (equal (alist-get 'kind conversation) "group")
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'group_uin conversation)))
      (error "qq: Gateway poke conversation must identify an exact group")))
  (dolist (key '(action action_image_url suffix))
    (when (assq key poke)
      (unless (qq-gateway--non-empty-string-p (alist-get key poke))
        (error "qq: Gateway poke %s must be non-empty string" key))))
  (let ((recall (alist-get 'recall poke)))
    (unless (and (qq-gateway--exact-object-keys-p
                  recall '(tips_sequence valid_before))
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'tips_sequence recall))
                 (integerp (alist-get 'valid_before recall))
                 (> (alist-get 'valid_before recall)
                    (alist-get 'sent_at poke)))
      (error "qq: Gateway poke recall capability is malformed")))
  (copy-tree poke))

(defun qq-gateway-message--validate-poke-data (data)
  "Validate and copy outer authoritative poke event DATA."
  (qq-gateway-message--validate-owner-data data 'poke)
  (qq-gateway-message--validate-poke (alist-get 'poke data))
  (copy-tree data))

(defun qq-gateway-message--validate-reaction (reaction)
  "Validate and copy one authoritative group REACTION event."
  (unless (qq-gateway-message--closed-object-p
           reaction
           '(conversation sequence operator_uid emoji_id emoji_type is_add count)
           '(operator_uin))
    (error "qq: Gateway reaction snapshot has invalid fields"))
  (let ((conversation (alist-get 'conversation reaction)))
    (unless (and (qq-gateway--exact-object-keys-p
                  conversation '(kind group_uin))
                 (equal (alist-get 'kind conversation) "group")
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'group_uin conversation)))
      (error "qq: Gateway reaction conversation must identify an exact group")))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'sequence reaction))
    (error "qq: Gateway reaction sequence must be exact decimal string"))
  (unless (qq-gateway--non-empty-string-p (alist-get 'operator_uid reaction))
    (error "qq: Gateway reaction operator UID must be opaque string"))
  (when (assq 'operator_uin reaction)
    (unless (qq-gateway--canonical-decimal-p
             (alist-get 'operator_uin reaction))
      (error "qq: Gateway reaction operator UIN must be exact decimal string")))
  (let* ((emoji-id (alist-get 'emoji_id reaction))
         (emoji-type (alist-get 'emoji_type reaction))
         (expected-type (and (stringp emoji-id)
                             (if (<= (length emoji-id) 3) "1" "2"))))
    (unless (and (qq-gateway--canonical-decimal-p emoji-id t)
                 (member emoji-type '("1" "2"))
                 (equal emoji-type expected-type))
      (error "qq: Gateway reaction emoji identity is malformed")))
  (unless (memq (alist-get 'is_add reaction) '(t :false))
    (error "qq: Gateway reaction direction must be JSON boolean"))
  (unless (qq-gateway-message--uint32-p (alist-get 'count reaction))
    (error "qq: Gateway reaction count must be uint32"))
  (copy-tree reaction))

(defun qq-gateway-message--validate-reaction-data (data)
  "Validate and copy outer authoritative reaction event DATA."
  (qq-gateway-message--validate-owner-data data 'reaction)
  (qq-gateway-message--validate-reaction (alist-get 'reaction data))
  (copy-tree data))

(defun qq-gateway-message--validate-essence (essence)
  "Validate and copy one authoritative group ESSENCE event."
  (unless (qq-gateway-message--closed-object-p
           essence
           '(conversation sequence random is_set sender_uin operator_uin
             changed_at)
           '(operator_nickname sender_nickname))
    (error "qq: Gateway essence snapshot has invalid fields"))
  (let ((conversation (alist-get 'conversation essence)))
    (unless (and (qq-gateway--exact-object-keys-p
                  conversation '(kind group_uin))
                 (equal (alist-get 'kind conversation) "group")
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'group_uin conversation)))
      (error "qq: Gateway essence conversation must identify an exact group")))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'sequence essence))
    (error "qq: Gateway essence sequence must be exact decimal string"))
  (unless (qq-gateway-message--uint32-p (alist-get 'random essence))
    (error "qq: Gateway essence random must be uint32"))
  (unless (memq (alist-get 'is_set essence) '(t :false))
    (error "qq: Gateway essence direction must be JSON boolean"))
  (dolist (key '(sender_uin operator_uin))
    (unless (qq-gateway--canonical-decimal-p (alist-get key essence))
      (error "qq: Gateway essence %s must be exact decimal string" key)))
  (unless (and (qq-gateway-message--uint32-p
                (alist-get 'changed_at essence))
               (> (alist-get 'changed_at essence) 0))
    (error "qq: Gateway essence changed_at must be positive uint32"))
  (dolist (key '(operator_nickname sender_nickname))
    (when (assq key essence)
      (unless (qq-gateway--non-empty-string-p (alist-get key essence))
        (error "qq: Gateway essence %s must be non-empty string" key))))
  (copy-tree essence))

(defun qq-gateway-message--validate-essence-data (data)
  "Validate and copy outer authoritative essence event DATA."
  (qq-gateway-message--validate-owner-data data 'essence)
  (qq-gateway-message--validate-essence (alist-get 'essence data))
  (copy-tree data))

(defun qq-gateway-message--event-owner (data)
  "Return `(ACCOUNT-ID . GENERATION)' carried by event DATA."
  (cons (alist-get 'account_id data) (alist-get 'generation data)))

(defun qq-gateway-message--selected-owner-p (data)
  "Return non-nil when event DATA belongs to the exact selected generation."
  (and (eq qq-backend 'gateway)
       (equal (qq-gateway-message--event-owner data)
              (qq-gateway-current-account-owner))))

(defun qq-gateway-message-revoke-projection ()
  "Revoke native projection ownership and all private correlation caches.

Shared `qq-state' is left to the caller's backend-switch or reset transaction."
  (setq qq-gateway-message--projection-owner nil)
  (clrhash qq-gateway-message--peer-uin-by-uid)
  (clrhash qq-gateway-message--pending-recalls)
  (clrhash qq-gateway-message--pending-reactions)
  (clrhash qq-gateway-message--pending-essences)
  (clrhash qq-gateway-message--essence-revisions)
  (clrhash qq-gateway-message--pending-sends)
  (clrhash qq-gateway-message--live-frontiers)
  nil)

(defun qq-gateway-message--clear-projection-state ()
  "Clear timeline state and all correlation owned by the old projection."
  (qq-gateway-message-revoke-projection)
  (qq-state-reset))

(defun qq-gateway-message-deactivate-projection ()
  "Release native projection state without disturbing another backend.

Shared `qq-state' is reset only when the Gateway currently owns it.  Private
correlation caches are always revoked."
  (if qq-gateway-message--projection-owner
      (qq-gateway-message--clear-projection-state)
    (qq-gateway-message-revoke-projection)))

(defun qq-gateway-message--sync-self-info (account)
  "Project selected native ACCOUNT identity into shared QQ state."
  (let ((info
         `((user_id . ,(alist-get 'uin account))
           (uid . ,(alist-get 'uid account))
           (nickname . ,(or (alist-get 'label account)
                            (alist-get 'uin account)
                            "QQ")))))
    (unless (equal info (qq-state-self-info))
      (qq-state-set-self-info info))))

(defun qq-gateway-message--sync-connection-status (account)
  "Project native ACCOUNT and transport state into shared connection status."
  (qq-state-set-connection-status
   (pcase (qq-gateway-transport-state)
     ('ready
      (pcase (alist-get 'phase account)
        ("online" 'ready)
        ("reconnecting" 'reconnecting)
        ((or "starting" "login_required" "logging_in") 'connecting)
        (_ 'disconnected)))
     ((or 'connecting 'authenticating) 'connecting)
     ('reconnecting 'reconnecting)
     (_ 'disconnected))))

(defun qq-gateway-message--ensure-projection-owner (owner)
  "Ensure shared QQ state is exclusively owned by native OWNER."
  (unless (eq qq-backend 'gateway)
    (error "qq: Native Gateway does not own the active client backend"))
  (unless (equal owner (qq-gateway-current-account-owner))
    (error "qq: Gateway event owner is not the selected account generation"))
  (unless (equal owner qq-gateway-message--projection-owner)
    (qq-gateway-message--clear-projection-state)
    (setq qq-gateway-message--projection-owner (copy-tree owner)))
  (let ((account (qq-gateway-current-account)))
    (qq-gateway-message--sync-self-info account)
    (qq-gateway-message--sync-connection-status account))
  owner)

(defun qq-gateway-message-activate-projection ()
  "Claim the selected Gateway account for shared state when available."
  (unless (eq qq-backend 'gateway)
    (user-error "qq: Select the native Gateway backend first"))
  (when-let* ((owner (qq-gateway-current-account-owner)))
    (qq-gateway-message--ensure-projection-owner owner)))

(defun qq-gateway-message--endpoint-self-p (endpoint account)
  "Return non-nil when ENDPOINT identifies selected ACCOUNT."
  (let ((uin (alist-get 'uin endpoint))
        (uid (alist-get 'uid endpoint))
        (self-uin (alist-get 'uin account))
        (self-uid (alist-get 'uid account)))
    (when (or (and uin self-uin (equal uin self-uin))
              (and uid self-uid (equal uid self-uid)))
      (when (and uin self-uin (not (equal uin self-uin)))
        (error "qq: Gateway endpoint UIN contradicts selected account"))
      (when (and uid self-uid (not (equal uid self-uid)))
        (error "qq: Gateway endpoint UID contradicts selected account"))
      t)))

(defun qq-gateway-message--private-context (message account)
  "Return private projection context for MESSAGE and selected ACCOUNT."
  (let* ((sender (alist-get 'sender message))
         (recipient (alist-get 'recipient message))
         (sender-self (qq-gateway-message--endpoint-self-p sender account))
         (recipient-self
          (qq-gateway-message--endpoint-self-p recipient account))
         outgoing peer)
    (pcase (list (and sender-self t) (and recipient-self t))
      (`(t nil) (setq outgoing t peer recipient))
      (`(nil t) (setq outgoing nil peer sender))
      (`(t t) (setq outgoing t peer recipient))
      (_ (error "qq: Private message endpoints do not identify selected account")))
    (unless (qq-gateway--canonical-decimal-p (alist-get 'uin peer))
      (error "qq: Private message peer lacks an exact UIN"))
    (list :outgoing outgoing :peer peer)))

(defun qq-gateway-message--segment-to-internal (segment)
  "Convert validated native SEGMENT to shared timeline shape."
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (pcase kind
      ("reply"
       `((type . "reply")
         (data . ((message_id
                   . ,(alist-get 'message_id (alist-get 'target payload)))))))
      ("unsupported"
       `((type . "__unsupported")
         (data . ((native_keys . ,(copy-tree (alist-get 'native_keys payload)))
                  (summary . ,(alist-get 'summary payload))
                  ,@(when-let* ((raw (alist-get 'raw payload)))
                      `((fallback_text . ,(alist-get 'fallback_text raw))))))))
      (_ `((type . ,kind) (data . ,(copy-tree payload)))))))

(defun qq-gateway-message--pending-send-key (owner client-sequence)
  "Return exact correlation key for OWNER and CLIENT-SEQUENCE."
  (list (car owner) (cdr owner) client-sequence))

(defun qq-gateway-message--validate-peer-identity (owner uid uin)
  "Validate exact UID/UIN mapping for projected account OWNER."
  (unless (equal owner qq-gateway-message--projection-owner)
    (error "qq: Peer identity does not belong to the projected Gateway owner"))
  (unless (and (qq-gateway--non-empty-string-p uid)
               (qq-gateway--canonical-decimal-p uin))
    (error "qq: Peer identity requires opaque UID and exact UIN"))
  (when-let* ((known (gethash uid qq-gateway-message--peer-uin-by-uid)))
    (unless (equal known uin)
      (error "qq: Gateway peer UID contradicts its known UIN")))
  (cons uid uin))

(defun qq-gateway-message--remember-peer-identity (owner uid uin)
  "Remember validated UID/UIN identity for projected account OWNER."
  (qq-gateway-message--validate-peer-identity owner uid uin)
  (puthash uid uin qq-gateway-message--peer-uin-by-uid)
  (cons uid uin))

(defun qq-gateway-message--attach-pending-local-id
    (normalized owner message session-key)
  "Attach OWNER's pending local ID to NORMALIZED when MESSAGE metadata matches.

SESSION-KEY must equal the conversation recorded with the send receipt."
  (let* ((key (qq-gateway-message--pending-send-key
               owner (alist-get 'client_sequence message)))
         (pending (gethash key qq-gateway-message--pending-sends)))
    ;; Consume the receipt only after the authoritative message has merged.
    ;; This keeps normalization free of correlation side effects, which is
    ;; required when a complete history page is preflighted before commit.
    (when pending
      (unless (and (equal session-key (plist-get pending :session-key))
                   (equal (alist-get 'sequence message)
                          (plist-get pending :server-sequence))
                   (= (alist-get 'random message)
                      (plist-get pending :random)))
        (error "qq: Self message contradicts its exact send receipt"))
      (setf (alist-get 'local-id normalized nil nil #'eq)
            (plist-get pending :local-id)))
    normalized))

(defun qq-gateway-message--normalize-message (data)
  "Normalize validated native message event DATA for shared state."
  (let* ((owner (qq-gateway-message--event-owner data))
         (message (alist-get 'message data))
         (account (qq-gateway-current-account))
         (conversation (alist-get 'conversation message))
         (kind (alist-get 'kind conversation))
         (sender (alist-get 'sender message))
         (sender-id (or (alist-get 'uin sender) (alist-get 'uid sender)))
         session-key peer peer-name outgoing group-id)
    (pcase kind
      ("private"
       (let ((context (qq-gateway-message--private-context message account)))
         (setq outgoing (plist-get context :outgoing)
               peer (plist-get context :peer)
               peer-name (alist-get 'name conversation)
               session-key (qq-state-session-key
                            'private (alist-get 'uin peer)))))
      ("group"
       (setq outgoing (and (qq-gateway-message--endpoint-self-p sender account) t)
             group-id (alist-get 'group_uin conversation)
             peer-name (alist-get 'group_name conversation)
             session-key (qq-state-session-key 'group group-id)))
      ("temp" (error "qq: Temp conversations are not projected yet")))
    (let* ((segments
            (mapcar #'qq-gateway-message--segment-to-internal
                    (alist-get 'segments message)))
           (mention-kinds (qq-state--mention-kinds-from-segments segments))
           (preview (qq-state-message-preview-from-segments segments))
           (sender-name
            (if outgoing
                (or (alist-get 'label account) (alist-get 'uin account) "me")
              (or (and (equal kind "group")
                       (alist-get 'sender_card conversation))
                  peer-name sender-id "unknown")))
           (normalized
            `((id . ,(alist-get 'message_id message))
              (server-id . ,(alist-get 'message_id message))
              (session-key . ,session-key)
              (time . ,(alist-get 'sent_at message))
              (message-seq . ,(alist-get 'sequence message))
              (native-client-sequence . ,(alist-get 'client_sequence message))
              (native-random . ,(alist-get 'random message))
              (native-sent-at . ,(alist-get 'sent_at message))
              (gateway-account-id . ,(car owner))
              (gateway-generation . ,(cdr owner))
              (sender-id . ,sender-id)
              (sender-native-id . ,(alist-get 'uid sender))
              (sender-name . ,sender-name)
              (sender-secondary-name . nil)
              (sender-card . ,(and (equal kind "group")
                                   (alist-get 'sender_card conversation)))
              (sender-nickname . nil)
              (sender-remark . nil)
              (self-p . ,outgoing)
              (status . ,(if outgoing 'sent 'received))
              (segments . ,segments)
              (mention-kinds . ,mention-kinds)
              (contains-mention-p . ,(and mention-kinds t))
              (raw-message . ,preview)
              (preview . ,preview)
              (message-type . ,kind)
              (chat-type . ,(if (equal kind "group") "2" "1"))
              (peer-uid . ,(and peer (alist-get 'uid peer)))
              (peer-uin . ,(and peer (alist-get 'uin peer)))
              (peer-name . ,peer-name)
              (group-id . ,group-id)
              (user-id . ,(alist-get 'uin sender))
              (target-id . ,(if group-id group-id (alist-get 'uin peer)))
              (order . ,(qq-state--next-message-order))
              (raw-event . ,(copy-tree data)))))
      (when (and peer (alist-get 'uid peer) (alist-get 'uin peer))
        (qq-gateway-message--validate-peer-identity
         owner (alist-get 'uid peer) (alist-get 'uin peer)))
      (qq-gateway-message--attach-pending-local-id
       normalized owner message session-key))))

(defun qq-gateway-message--finalize-message-context (owner message normalized)
  "Commit correlation context for OWNER's merged MESSAGE and NORMALIZED row."
  (when-let* ((peer-uid (alist-get 'peer-uid normalized))
              (peer-uin (alist-get 'peer-uin normalized)))
    (qq-gateway-message--remember-peer-identity owner peer-uid peer-uin))
  (let* ((key (qq-gateway-message--pending-send-key
               owner (alist-get 'client_sequence message)))
         (pending (gethash key qq-gateway-message--pending-sends)))
    (when (and pending
               (equal (alist-get 'local-id normalized)
                      (plist-get pending :local-id)))
      (remhash key qq-gateway-message--pending-sends))))

(defun qq-gateway-message--merge-normalized (normalized &optional source)
  "Merge native NORMALIZED message and publish a state event from SOURCE."
  (let ((session-key (alist-get 'session-key normalized)))
    (when-let* ((title (alist-get 'peer-name normalized)))
      (unless (string-empty-p title)
        (qq-state-upsert-session session-key `((title . ,title)) nil)))
    (cl-multiple-value-bind (merged mutation previous-anchor)
        (qq-state--merge-normalized-message session-key normalized)
      (when merged
        (apply #'qq-state--emit
               'message
               :session-key session-key
               :message (copy-tree merged)
               :message-anchor (qq-state-message-anchor merged)
               :mutation mutation
               :source (or source 'event)
               (when previous-anchor
                 (list :previous-anchor previous-anchor))))
      merged)))

(defun qq-gateway-message--recall-conversation-key (conversation)
  "Return stable native identity key for recall CONVERSATION."
  (pcase (alist-get 'kind conversation)
    ("private" (list "private" (alist-get 'peer_uid conversation)))
    ("group" (list "group" (alist-get 'group_uin conversation)))))

(defun qq-gateway-message--message-conversation-key (normalized)
  "Return recall identity key for NORMALIZED message."
  (pcase (alist-get 'message-type normalized)
    ("private"
     (when-let* ((uid (alist-get 'peer-uid normalized)))
       (list "private" uid)))
    ("group" (list "group" (alist-get 'group-id normalized)))))

(defun qq-gateway-message--pending-recall-key (owner conversation-key sequence)
  "Return sequence recall key for OWNER, CONVERSATION-KEY, and SEQUENCE."
  (list (car owner) (cdr owner) conversation-key sequence))

(defun qq-gateway-message--pending-reaction-key (owner group-uin sequence)
  "Return reaction key for OWNER, GROUP-UIN, and exact SEQUENCE."
  (list (car owner) (cdr owner) group-uin sequence))

(defun qq-gateway-message--pending-essence-key
    (owner group-uin sequence random)
  "Return essence key for OWNER, GROUP-UIN, SEQUENCE, and RANDOM."
  (list (car owner) (cdr owner) group-uin sequence random))

(defun qq-gateway-message--validate-pending-recall (owner normalized)
  "Reject a pending recall that contradicts OWNER's NORMALIZED message."
  (when-let* ((conversation-key
               (qq-gateway-message--message-conversation-key normalized))
              (sequence (alist-get 'message-seq normalized))
              (key (qq-gateway-message--pending-recall-key
                    owner conversation-key sequence))
              (recall (gethash key qq-gateway-message--pending-recalls))
              (target (alist-get 'target recall))
              ((equal (alist-get 'kind target) "message"))
              (expected-id (alist-get 'message_id target)))
    (unless (equal expected-id (alist-get 'server-id normalized))
      (error "qq: Sequence recall message_id contradicts received message")))
  normalized)

(defun qq-gateway-message--apply-pending-recall (owner normalized merged)
  "Apply a pending recall for OWNER to MERGED NORMALIZED message when present."
  (qq-gateway-message--validate-pending-recall owner normalized)
  (when-let* ((conversation-key
               (qq-gateway-message--message-conversation-key normalized))
              (sequence (alist-get 'message-seq normalized))
              (key (qq-gateway-message--pending-recall-key
                    owner conversation-key sequence))
              (recall (gethash key qq-gateway-message--pending-recalls)))
    (let ((message-id (alist-get 'server-id merged)))
      (remhash key qq-gateway-message--pending-recalls)
      (qq-state-apply-recall (alist-get 'session-key merged) message-id))))

(defun qq-gateway-message--reaction-notice (reaction message)
  "Return legacy state notice for authoritative REACTION on MESSAGE."
  (let* ((conversation (alist-get 'conversation reaction))
         (group-uin (alist-get 'group_uin conversation))
         (account (qq-gateway-current-account))
         (operator-uin
          (or (alist-get 'operator_uin reaction)
              (and (equal (alist-get 'operator_uid reaction)
                          (alist-get 'uid account))
                   (alist-get 'uin account)))))
    `((notice_type . "group_msg_emoji_like")
      (group_id . ,group-uin)
      (message_id . ,(alist-get 'server-id message))
      ,@(when operator-uin `((user_id . ,operator-uin)))
      (is_add . ,(alist-get 'is_add reaction))
      (likes . (((emoji_id . ,(alist-get 'emoji_id reaction))
                 (emoji_type . ,(alist-get 'emoji_type reaction))
                 (count . ,(alist-get 'count reaction))))))))

(defun qq-gateway-message--apply-reaction (reaction message)
  "Apply authoritative REACTION to projected MESSAGE."
  (qq-state-apply-emoji-like-notice
   (alist-get 'session-key message)
   (qq-gateway-message--reaction-notice reaction message)))

(defun qq-gateway-message--apply-pending-reactions (owner normalized merged)
  "Apply reaction events awaiting OWNER's MERGED NORMALIZED message."
  (when-let* ((group-uin (alist-get 'group-id normalized))
              (sequence (alist-get 'message-seq normalized))
              (key (qq-gateway-message--pending-reaction-key
                    owner group-uin sequence))
              (reactions (gethash key qq-gateway-message--pending-reactions)))
    (dolist (reaction reactions)
      (qq-gateway-message--apply-reaction reaction merged))
    (remhash key qq-gateway-message--pending-reactions)))

(defun qq-gateway-message--apply-essence-state
    (message set &optional essence source)
  "Apply essence SET state to MESSAGE and publish it from SOURCE.

When ESSENCE is non-nil, retain its authoritative actor and timestamp
metadata.  A synchronous action receipt intentionally changes only the
optimistic boolean; the later push remains authoritative."
  (let ((patched (copy-tree message)))
    (setf (alist-get 'essence-p patched nil nil #'eq) (and set t))
    (when essence
      (setf (alist-get 'essence-sender-id patched nil nil #'eq)
            (alist-get 'sender_uin essence)
            (alist-get 'essence-operator-id patched nil nil #'eq)
            (alist-get 'operator_uin essence)
            (alist-get 'essence-changed-at patched nil nil #'eq)
            (alist-get 'changed_at essence)
            (alist-get 'essence-operator-nickname patched nil nil #'eq)
            (alist-get 'operator_nickname essence)
            (alist-get 'essence-sender-nickname patched nil nil #'eq)
            (alist-get 'sender_nickname essence)))
    (qq-gateway-message--merge-normalized patched (or source 'event))))

(defun qq-gateway-message--apply-pending-essence (owner normalized merged)
  "Apply the latest essence event awaiting OWNER's MERGED message."
  (when-let* ((group-uin (alist-get 'group-id normalized))
              (sequence (alist-get 'message-seq normalized))
              (random (alist-get 'native-random normalized))
              (key (qq-gateway-message--pending-essence-key
                    owner group-uin sequence random))
              (essence (gethash key qq-gateway-message--pending-essences)))
    (remhash key qq-gateway-message--pending-essences)
    (qq-gateway-message--apply-essence-state
     merged (eq (alist-get 'is_set essence) t) essence 'event)))

(defun qq-gateway-message--project-message (data)
  "Project selected-account native message event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (normalized (qq-gateway-message--normalize-message data))
         (frontier (qq-gateway-message--plan-live-frontier normalized))
         (merged (qq-gateway-message--merge-normalized normalized 'event)))
    (qq-gateway-message--finalize-message-context
     owner (alist-get 'message data) normalized)
    (qq-gateway-message--apply-pending-recall owner normalized merged)
    (qq-gateway-message--apply-pending-reactions owner normalized merged)
    (qq-gateway-message--apply-pending-essence owner normalized merged)
    (when frontier
      (puthash (alist-get 'session-key normalized) frontier
               qq-gateway-message--live-frontiers))
    merged))

(defun qq-gateway-message--plan-live-frontier (normalized)
  "Return a new live frontier for NORMALIZED, or nil when it cannot advance.

An equal sequence carrying a different message id is a protocol contradiction.
The caller commits the returned value only after the timeline projection has
completed, so a failed projection cannot advance this side index."
  (let* ((session-key (alist-get 'session-key normalized))
         (message-id (alist-get 'server-id normalized))
         (sequence (alist-get 'message-seq normalized))
         (current (gethash session-key qq-gateway-message--live-frontiers))
         (current-sequence (alist-get 'sequence current)))
    (cond
     ((null current)
      `((message_id . ,message-id) (sequence . ,sequence)))
     ((equal current-sequence sequence)
      (unless (equal (alist-get 'message_id current) message-id)
        (error "qq: Gateway live sequence maps to different message ids"))
      nil)
     ((qq-gateway--decimal-less-p current-sequence sequence)
      `((message_id . ,message-id) (sequence . ,sequence)))
     (t nil))))

(defun qq-gateway-message-live-frontier (session-key)
  "Return selected-generation live frontier for SESSION-KEY, or nil.

The result contains exact string `message_id' and `sequence' fields.  History
responses never advance this observation; only `message.received' events do."
  (when (and (eq qq-backend 'gateway)
             qq-gateway-message--projection-owner
             (equal qq-gateway-message--projection-owner
                    (qq-gateway-current-account-owner)))
    (copy-tree (gethash session-key qq-gateway-message--live-frontiers))))

(defun qq-gateway-message--private-session-by-uid (peer-uid)
  "Return current private session key for exact PEER-UID, or nil."
  (or (when-let* ((uin (gethash peer-uid
                                qq-gateway-message--peer-uin-by-uid)))
        (qq-state-session-key 'private uin))
      (when-let* ((session
                   (seq-find
                    (lambda (candidate)
                      (and (eq (alist-get 'type candidate) 'private)
                           (equal (alist-get 'peer-uid candidate) peer-uid)))
                    (qq-state-sessions))))
        (alist-get 'key session))))

(defun qq-gateway-message--recall-session-key (conversation)
  "Return projected session key for recall CONVERSATION, or nil."
  (pcase (alist-get 'kind conversation)
    ("group" (qq-state-session-key
              'group (alist-get 'group_uin conversation)))
    ("private" (qq-gateway-message--private-session-by-uid
                (alist-get 'peer_uid conversation)))))

(defun qq-gateway-message--message-by-sequence (session-key sequence)
  "Return cached message in SESSION-KEY matching exact SEQUENCE."
  (seq-find (lambda (message)
              (equal (alist-get 'message-seq message) sequence))
            (qq-state-session-messages session-key)))

(defun qq-gateway-message--message-by-native-target
    (session-key sequence random)
  "Return cached message in SESSION-KEY matching SEQUENCE and RANDOM."
  (seq-find
   (lambda (message)
     (and (equal (alist-get 'message-seq message) sequence)
          (equal (alist-get 'native-random message) random)))
   (qq-state-session-messages session-key)))

(defun qq-gateway-message--project-recall (data)
  "Project selected-account native recall event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (recall (alist-get 'recall data))
         (conversation (alist-get 'conversation recall))
         (target (alist-get 'target recall))
         (sequence (alist-get 'sequence target))
         (session-key (qq-gateway-message--recall-session-key conversation))
         (message
          (and session-key
               (qq-gateway-message--message-by-sequence session-key sequence)))
         (message-id
          (if (equal (alist-get 'kind target) "message")
              (alist-get 'message_id target)
            (and message (alist-get 'server-id message)))))
    (when (and message
               (equal (alist-get 'kind target) "message")
               (not (equal (alist-get 'server-id message) message-id)))
      (error "qq: Recall target message_id contradicts cached sequence"))
    (if (and session-key message-id)
        (qq-state-apply-recall session-key message-id)
      (puthash
       (qq-gateway-message--pending-recall-key
        owner
        (qq-gateway-message--recall-conversation-key conversation)
        sequence)
       (copy-tree recall)
       qq-gateway-message--pending-recalls))))

(defun qq-gateway-message--poke-raw-info (poke)
  "Return shared renderer decoration items for validated POKE."
  (delq
   nil
   (list
    (when-let* ((action (alist-get 'action poke)))
      `((type . "text") (txt . ,action)))
    (when-let* ((image-url (alist-get 'action_image_url poke)))
      `((type . "img") (src . ,image-url)))
    (when-let* ((suffix (alist-get 'suffix poke)))
      `((type . "text") (txt . ,suffix))))))

(defun qq-gateway-message--project-poke (data)
  "Project selected-account authoritative group poke event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (poke (alist-get 'poke data))
         (conversation (alist-get 'conversation poke))
         (group-uin (alist-get 'group_uin conversation))
         (recall (alist-get 'recall poke))
         (message-id (alist-get 'message_id poke))
         (notice
          `((time . ,(alist-get 'sent_at poke))
            (post_type . "notice")
            (notice_type . "notify")
            (sub_type . "poke")
            (group_id . ,group-uin)
            (user_id . ,(alist-get 'actor_uin poke))
            (target_id . ,(alist-get 'target_uin poke))
            (recall_reference
             . ((message_id . ,message-id)
                (peer . ((chat_type . 2)
                         (peer_uid . ,group-uin)
                         (guild_id . "")))
                (valid_before . ,(alist-get 'valid_before recall))))
            (gateway_recall
             . ((account_id . ,(car owner))
                (generation . ,(cdr owner))
                (conversation . ,(copy-tree conversation))
                (message_id . ,message-id)
                (sequence . ,(alist-get 'sequence poke))
                (sent_at . ,(alist-get 'sent_at poke))
                (tips_sequence . ,(alist-get 'tips_sequence recall))))
            (raw_info . ,(qq-gateway-message--poke-raw-info poke)))))
    (qq-state-apply-poke-notice notice)))

(defun qq-gateway-message--project-reaction (data)
  "Project selected-account authoritative group reaction event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (reaction (alist-get 'reaction data))
         (conversation (alist-get 'conversation reaction))
         (group-uin (alist-get 'group_uin conversation))
         (sequence (alist-get 'sequence reaction))
         (session-key (qq-state-session-key 'group group-uin))
         (message (qq-gateway-message--message-by-sequence
                   session-key sequence)))
    (if message
        (qq-gateway-message--apply-reaction reaction message)
      (let* ((key (qq-gateway-message--pending-reaction-key
                   owner group-uin sequence))
             (pending (gethash key qq-gateway-message--pending-reactions)))
        (puthash key (append pending (list (copy-tree reaction)))
                 qq-gateway-message--pending-reactions)))))

(defun qq-gateway-message--project-essence (data)
  "Project selected-account authoritative group essence event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (essence (alist-get 'essence data))
         (conversation (alist-get 'conversation essence))
         (group-uin (alist-get 'group_uin conversation))
         (sequence (alist-get 'sequence essence))
         (random (alist-get 'random essence))
         (session-key (qq-state-session-key 'group group-uin))
         (key (qq-gateway-message--pending-essence-key
               owner group-uin sequence random))
         (message (qq-gateway-message--message-by-native-target
                   session-key sequence random)))
    (puthash key (1+ (or (gethash key qq-gateway-message--essence-revisions)
                         0))
             qq-gateway-message--essence-revisions)
    (if message
        (qq-gateway-message--apply-essence-state
         message (eq (alist-get 'is_set essence) t) essence 'event)
      (puthash
       key
       (copy-tree essence)
       qq-gateway-message--pending-essences))))

(defun qq-gateway-message--projection-error (event data error-data)
  "Publish projection ERROR-DATA for validated EVENT and DATA."
  (let ((reason (error-message-string error-data)))
    (qq-gateway--run-hook
     'qq-gateway-message-projection-error-hook event (copy-tree data) reason)
    (message "qq: Gateway %s projection skipped: %s" event reason)))

(defun qq-gateway-message--handle-event (event data)
  "Validate native message EVENT with DATA and project the selected owner."
  (when (member event '("message.received" "message.recalled" "message.poked"
                        "message.reaction_changed" "message.essence_changed"))
    (condition-case error-data
        (let ((validated
               (pcase event
                 ("message.received"
                  (qq-gateway-message--validate-message-data data))
                 ("message.recalled"
                  (qq-gateway-message--validate-recall-data data))
                 ("message.poked"
                  (qq-gateway-message--validate-poke-data data))
                 ("message.reaction_changed"
                  (qq-gateway-message--validate-reaction-data data))
                 ("message.essence_changed"
                  (qq-gateway-message--validate-essence-data data)))))
          (qq-gateway--run-hook
           'qq-gateway-message-event-hook event (copy-tree validated))
          (when (qq-gateway-message--selected-owner-p validated)
            (condition-case projection-error
                (pcase event
                  ("message.received"
                   (qq-gateway-message--project-message validated))
                  ("message.recalled"
                   (qq-gateway-message--project-recall validated))
                  ("message.poked"
                   (qq-gateway-message--project-poke validated))
                  ("message.reaction_changed"
                   (qq-gateway-message--project-reaction validated))
                  ("message.essence_changed"
                   (qq-gateway-message--project-essence validated)))
              (error
               (qq-gateway-message--projection-error
                event validated projection-error)))))
      (error
       (qq-gateway-transport--protocol-violation
        "Malformed %s event: %s" event
        (error-message-string error-data))))))

(defun qq-gateway-message--handle-account-change (reason account-id)
  "Keep projection ownership aligned after account change REASON/ACCOUNT-ID."
  (ignore reason)
  (if (not (eq qq-backend 'gateway))
      (qq-gateway-message-revoke-projection)
    (let ((selected (qq-gateway-current-account-owner)))
      (cond
       ((null selected)
        (when qq-gateway-message--projection-owner
          (qq-gateway-message--clear-projection-state)))
       ((not (equal selected qq-gateway-message--projection-owner))
        (qq-gateway-message--ensure-projection-owner selected))
       ((or (null account-id)
            (equal account-id (car selected)))
        (let ((account (qq-gateway-current-account)))
          (qq-gateway-message--sync-self-info account)
          (qq-gateway-message--sync-connection-status account)))))))

(defun qq-gateway-message--handle-selection-change (_old-account-id new-account-id)
  "Move projected state ownership to selected NEW-ACCOUNT-ID."
  (cond
   ((not (eq qq-backend 'gateway))
   (qq-gateway-message-revoke-projection))
   (new-account-id
    (let ((owner (qq-gateway-current-account-owner)))
      (unless (and owner (equal (car owner) new-account-id))
        (error "qq: Gateway selection has no matching account snapshot"))
      (qq-gateway-message--ensure-projection-owner owner)))
   (qq-gateway-message--projection-owner
    (qq-gateway-message--clear-projection-state))))

(defun qq-gateway-message--handle-transport-state (_state)
  "Refresh shared status after a native transport state transition."
  (when (and (eq qq-backend 'gateway)
             qq-gateway-message--projection-owner
             (equal qq-gateway-message--projection-owner
                    (qq-gateway-current-account-owner)))
    (qq-gateway-message--sync-connection-status
     (qq-gateway-current-account))))

(defun qq-gateway-message--conversation-params (session-key)
  "Return native Gateway conversation params for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target (alist-get 'target-id identity)))
    (pcase kind
      ('private `((kind . "private") (peer_uin . ,target)))
      ('group `((kind . "group") (group_uin . ,target)))
      (_ (user-error "qq: Native Gateway only sends private or group messages")))))

(defun qq-gateway-message--decimal-add-small (value addend)
  "Return canonical decimal VALUE plus non-negative small integer ADDEND.

VALUE remains a string throughout; only individual decimal digits and ADDEND
are represented as Emacs integers."
  (unless (and (qq-gateway--canonical-decimal-p value t)
               (integerp addend) (>= addend 0))
    (error "qq: Invalid decimal addition operands"))
  (let ((carry addend)
        result)
    (dolist (digit (nreverse (string-to-list value)))
      (let ((sum (+ (- digit ?0) carry)))
        (push (+ ?0 (% sum 10)) result)
        (setq carry (/ sum 10))))
    (while (> carry 0)
      (push (+ ?0 (% carry 10)) result)
      (setq carry (/ carry 10)))
    (apply #'string result)))

(defun qq-gateway-message--decimal-subtract-small (value subtrahend)
  "Return canonical decimal VALUE minus non-negative small SUBTRAHEND.

The result saturates at zero.  VALUE is never coerced to an Emacs number."
  (unless (and (qq-gateway--canonical-decimal-p value t)
               (integerp subtrahend) (>= subtrahend 0))
    (error "qq: Invalid decimal subtraction operands"))
  (let ((small (number-to-string subtrahend)))
    (if (or (qq-gateway--decimal-less-p value small)
            (equal value small))
        "0"
      (let ((borrow subtrahend)
            result)
        (dolist (digit (nreverse (string-to-list value)))
          (let* ((subdigit (% borrow 10))
                 (next-borrow (/ borrow 10))
                 (difference (- (- digit ?0) subdigit)))
            (when (< difference 0)
              (setq difference (+ difference 10)
                    next-borrow (1+ next-borrow)))
            (push (+ ?0 difference) result)
            (setq borrow next-borrow)))
        (setq result (string-trim-left (apply #'string result) "0+"))
        (if (string-empty-p result) "0" result)))))

(defun qq-gateway-message--validate-sequence (value context)
  "Return exact sequence VALUE after validation for CONTEXT."
  (unless (and (qq-gateway--canonical-decimal-p value t)
               (not (qq-gateway--decimal-less-p
                     qq-gateway-message--max-uint64-decimal value)))
    (user-error "qq: %s must be a canonical uint64 decimal string" context))
  value)

(defun qq-gateway-message--validate-history-range
    (start-sequence end-sequence)
  "Validate inclusive START-SEQUENCE and END-SEQUENCE without coercion."
  (qq-gateway-message--validate-sequence start-sequence "History start sequence")
  (qq-gateway-message--validate-sequence end-sequence "History end sequence")
  (when (qq-gateway--decimal-less-p end-sequence start-sequence)
    (user-error "qq: History start sequence must not exceed end sequence"))
  (when (qq-gateway--decimal-less-p
         (qq-gateway-message--decimal-add-small start-sequence 99)
         end-sequence)
    (user-error "qq: Native Gateway history range is limited to 100 messages"))
  (cons start-sequence end-sequence))

(defun qq-gateway-message--validate-history-count (count)
  "Return history page COUNT after native range validation."
  (unless (and (integerp count) (<= 1 count 100))
    (user-error "qq: Native Gateway history count must be between 1 and 100"))
  count)

(defun qq-gateway-message-history-range-ending-at (end-sequence count)
  "Return COUNT messages in an inclusive range ending at exact END-SEQUENCE.

The start saturates at zero, so a sequence near the beginning may yield a
shorter range."
  (qq-gateway-message--validate-sequence end-sequence "History page end")
  (qq-gateway-message--validate-history-count count)
  (let ((start-sequence
         (qq-gateway-message--decimal-subtract-small
          end-sequence (1- count))))
    (qq-gateway-message--validate-history-range
     start-sequence end-sequence)))

(defun qq-gateway-message-history-range-around (sequence count)
  "Return COUNT messages in an inclusive range containing exact SEQUENCE.

The target is centered when possible.  Close to zero, the whole range shifts
right instead of becoming shorter."
  (qq-gateway-message--validate-sequence sequence "History center sequence")
  (qq-gateway-message--validate-history-count count)
  (let* ((start-sequence
          (qq-gateway-message--decimal-subtract-small
           sequence (/ (1- count) 2)))
         (end-sequence
          (qq-gateway-message--decimal-add-small start-sequence (1- count))))
    (qq-gateway-message--validate-history-range
     start-sequence end-sequence)))

(defun qq-gateway-message--validate-history-result
    (result owner start-sequence end-sequence)
  "Validate history RESULT for OWNER and START-SEQUENCE through END-SEQUENCE."
  (unless (qq-gateway--exact-object-keys-p
           result
           '(account_id generation requested_start_sequence
             requested_end_sequence response_start_sequence
             response_end_sequence unsupported_message_count messages))
    (error "qq: Gateway history result has invalid fields"))
  (unless (and (equal (alist-get 'account_id result) (car owner))
               (equal (alist-get 'generation result) (cdr owner)))
    (error "qq: Gateway history result owner contradicts request"))
  (unless (and (equal (alist-get 'requested_start_sequence result)
                      start-sequence)
               (equal (alist-get 'requested_end_sequence result)
                      end-sequence))
    (error "qq: Gateway history result range contradicts request"))
  (dolist (key '(requested_start_sequence requested_end_sequence
                 response_start_sequence response_end_sequence))
    (condition-case error-data
        (qq-gateway-message--validate-sequence
         (alist-get key result) (format "History result %s" key))
      (user-error
       (error "%s" (error-message-string error-data)))))
  (when (qq-gateway--decimal-less-p
         (alist-get 'response_end_sequence result)
         (alist-get 'response_start_sequence result))
    (error "qq: Gateway history response range is reversed"))
  (unless (and (integerp (alist-get 'unsupported_message_count result))
               (>= (alist-get 'unsupported_message_count result) 0))
    (error "qq: Gateway history unsupported count must be non-negative integer"))
  (let ((messages (alist-get 'messages result)))
    (unless (listp messages)
      (error "qq: Gateway history messages must be an array"))
    (dolist (message messages)
      (qq-gateway-message--validate-message message)
      (let ((sequence (alist-get 'sequence message)))
        (when (or (qq-gateway--decimal-less-p sequence start-sequence)
                  (qq-gateway--decimal-less-p end-sequence sequence))
          (error "qq: Gateway history message falls outside requested range")))))
  (copy-tree result))

(defun qq-gateway-message--history-message-data (owner message)
  "Wrap one history MESSAGE with exact account OWNER context."
  `((account_id . ,(car owner))
    (generation . ,(cdr owner))
    (message . ,(copy-tree message))))

(defun qq-gateway-message--normalize-history
    (result owner session-key)
  "Preflight RESULT messages for OWNER and exact SESSION-KEY.

Return a list of `(NATIVE-MESSAGE . NORMALIZED-MESSAGE)' pairs."
  (let ((initial-order qq-state--message-order-counter)
        (pending-local-ids (make-hash-table :test #'equal))
        rows)
    (condition-case error-data
        (progn
          (dolist (message (alist-get 'messages result))
            (let* ((data (qq-gateway-message--history-message-data
                          owner message))
                   (normalized (qq-gateway-message--normalize-message data))
                   (local-id (alist-get 'local-id normalized)))
              (unless (equal (alist-get 'session-key normalized) session-key)
                (error "qq: Gateway history message contradicts requested conversation"))
              (qq-state-validate-message-session
               session-key (alist-get 'server-id normalized))
              (qq-gateway-message--validate-pending-recall owner normalized)
              (when local-id
                (when (gethash local-id pending-local-ids)
                  (error "qq: Gateway history reuses one pending send receipt"))
                (puthash local-id t pending-local-ids))
              (push (cons message normalized) rows)))
          (nreverse rows))
      (error
       (setq qq-state--message-order-counter initial-order)
       (signal (car error-data) (cdr error-data))))))

(defun qq-gateway-message--merge-history (session-key result owner)
  "Merge validated native history RESULT into SESSION-KEY for OWNER."
  (let ((rows (qq-gateway-message--normalize-history
               result owner session-key))
        (known-ids (make-hash-table :test #'equal))
        (added 0)
        batch-ids)
    (dolist (message (qq-state-session-messages session-key))
      (when-let* ((message-id (alist-get 'server-id message)))
        (puthash message-id t known-ids)))
    (dolist (row rows)
      (let* ((native (car row))
             (normalized (cdr row))
             (message-id (alist-get 'server-id normalized)))
        (when-let* ((title (alist-get 'peer-name normalized)))
          (unless (string-empty-p title)
            (qq-state-upsert-session session-key `((title . ,title)) nil)))
        (cl-multiple-value-bind (merged _mutation _previous-anchor)
            (qq-state--merge-normalized-message session-key normalized)
          (qq-gateway-message--finalize-message-context
           owner native normalized)
          (qq-gateway-message--apply-pending-recall owner normalized merged)
          (qq-gateway-message--apply-pending-reactions owner normalized merged)
          (qq-gateway-message--apply-pending-essence owner normalized merged))
        (unless (gethash message-id known-ids)
          (cl-incf added)
          (puthash message-id t known-ids))
        (push message-id batch-ids)))
    (setq batch-ids (delete-dups (nreverse batch-ids)))
    (let* ((oldest (qq-state-session-oldest-message-id session-key))
           (meta
            (list
             :session-key session-key
             :account-id (car owner)
             :generation (cdr owner)
             :message-count (length rows)
             :added-count added
             :oldest-message-id oldest
             :batch-message-ids batch-ids
             :batch-oldest-message-id (car batch-ids)
             :batch-newest-message-id (car (last batch-ids))
             :requested-start-sequence
             (alist-get 'requested_start_sequence result)
             :requested-end-sequence
             (alist-get 'requested_end_sequence result)
             :response-start-sequence
             (alist-get 'response_start_sequence result)
             :response-end-sequence
             (alist-get 'response_end_sequence result)
             :unsupported-message-count
             (alist-get 'unsupported_message_count result))))
      (apply #'qq-state--emit 'history
             :mutation 'history :source 'response meta)
      meta)))

(defun qq-gateway-message-get-history
    (session-key start-sequence end-sequence &optional callback errback)
  "Fetch an inclusive native history range for SESSION-KEY.

START-SEQUENCE and END-SEQUENCE are exact canonical decimal strings and the
inclusive range may contain at most 100 sequence values.  CALLBACK receives a
merge metadata plist; ERRBACK receives a Gateway error body and reason."
  (qq-gateway-message--validate-history-range start-sequence end-sequence)
  (let* ((conversation (qq-gateway-message--conversation-params session-key))
         (owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first"))))
    (qq-gateway-message--ensure-projection-owner owner)
    (qq-gateway--send
     "message.get_history"
     `((account_id . ,(car owner))
       (conversation . ,conversation)
       (start_sequence . ,start-sequence)
       (end_sequence . ,end-sequence))
     (lambda (raw-result)
       (condition-case error-data
           (progn
             (unless (equal owner (qq-gateway-current-account-owner))
               (error "qq: Gateway account generation changed during history request"))
             (let* ((result
                     (qq-gateway-message--validate-history-result
                      raw-result owner start-sequence end-sequence))
                    (meta (qq-gateway-message--merge-history
                           session-key result owner)))
               (qq-gateway--invoke callback meta)))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

(defun qq-gateway-message--validate-send-receipt (receipt owner)
  "Validate and copy message-send RECEIPT for exact OWNER."
  (unless (qq-gateway--exact-object-keys-p
           receipt
           '(account_id generation sent_at server_sequence client_sequence random))
    (error "qq: Gateway message-send receipt has invalid fields"))
  (unless (and (equal (alist-get 'account_id receipt) (car owner))
               (equal (alist-get 'generation receipt) (cdr owner)))
    (error "qq: Gateway message-send receipt owner contradicts request"))
  (unless (and (integerp (alist-get 'sent_at receipt))
               (>= (alist-get 'sent_at receipt) 0))
    (error "qq: Gateway message-send receipt timestamp is invalid"))
  (dolist (key '(server_sequence client_sequence))
    (unless (qq-gateway--canonical-decimal-p (alist-get key receipt) t)
      (error "qq: Gateway message-send receipt %s is invalid" key)))
  (unless (qq-gateway-message--uint32-p (alist-get 'random receipt))
    (error "qq: Gateway message-send receipt random is invalid"))
  (copy-tree receipt))

(defun qq-gateway-message--outgoing-reply-target
    (session-key message-id owner)
  "Return native reply target for MESSAGE-ID in SESSION-KEY owned by OWNER."
  (unless (qq-gateway--canonical-decimal-p message-id)
    (user-error "qq: Native reply requires an exact snowflake message ID"))
  (let ((message
         (seq-find
          (lambda (candidate)
            (equal (alist-get 'server-id candidate) message-id))
          (qq-state-session-messages session-key))))
    (unless message
      (user-error "qq: Native reply target %s is not loaded" message-id))
    (unless (and (equal (alist-get 'gateway-account-id message) (car owner))
                 (equal (alist-get 'gateway-generation message) (cdr owner)))
      (user-error "qq: Native reply target belongs to another Gateway generation"))
    (let ((sequence (alist-get 'message-seq message))
          (sender-uin (alist-get 'user-id message))
          (sender-uid (alist-get 'sender-native-id message))
          (sent-at (alist-get 'native-sent-at message)))
      (unless (qq-gateway--canonical-decimal-p sequence)
        (user-error "qq: Native reply target lacks an exact message sequence"))
      (unless (qq-gateway--canonical-decimal-p sender-uin)
        (user-error "qq: Native reply target lacks an exact sender UIN"))
      (unless (qq-gateway--non-empty-string-p sender-uid)
        (user-error "qq: Native reply target lacks an exact sender UID"))
      (unless (qq-gateway-message--uint32-p sent-at)
        (user-error "qq: Native reply target has an invalid timestamp"))
      `((message_id . ,message-id)
        (sequence . ,sequence)
        (sender_uin . ,sender-uin)
        (sender_uid . ,sender-uid)
        (sent_at . ,sent-at)))))

(defun qq-gateway-message--outgoing-segments (session-key segments owner)
  "Validate SEGMENTS for SESSION-KEY and return native elements for OWNER."
  (unless (and (proper-list-p segments)
               segments
               (<= (length segments) 128))
    (user-error "qq: Native Gateway requires between 1 and 128 segments"))
  (let ((group-p (eq (qq-state-session-key-type session-key) 'group))
        (reply-count 0))
    (mapcar
     (lambda (segment)
       (unless (qq-gateway--exact-object-keys-p segment '(type data))
         (user-error "qq: Native Gateway segment has invalid fields"))
       (let ((type (alist-get 'type segment))
             (data (alist-get 'data segment)))
         (pcase type
           ("text"
            (unless (and (qq-gateway--exact-object-keys-p data '(text))
                         (qq-gateway--non-empty-string-p
                          (alist-get 'text data)))
              (user-error "qq: Native Gateway text segment is malformed"))
           `((kind . "text")
              (payload . ((text . ,(alist-get 'text data))))))
           ("face"
            (let ((id (alist-get 'id data)))
              (unless (and (qq-gateway--exact-object-keys-p data '(id))
                           (qq-gateway--canonical-decimal-p id t)
                           (qq-gateway--decimal-less-p id "260"))
                (user-error
                 "qq: Native Gateway base face ID must be between 0 and 259"))
              `((kind . "face") (payload . ((id . ,id))))))
           ("at"
            (unless group-p
              (user-error "qq: Native Gateway mentions require a group chat"))
            (unless (qq-gateway-message--closed-object-p data '(qq) '(name))
              (user-error "qq: Native Gateway mention has invalid fields"))
            (let ((qq (alist-get 'qq data))
                  (name (alist-get 'name data)))
              (unless (or (equal qq "all")
                          (qq-gateway--canonical-decimal-p qq))
                (user-error "qq: Native Gateway mention target is invalid"))
              (when (and (assq 'name data)
                         (not (qq-gateway--non-empty-string-p name)))
                (user-error "qq: Native Gateway mention name is invalid"))
              `((kind . "mention")
                (payload
                 . ((target
                     . ((kind . ,(if (equal qq "all") "all" "user"))
                        ,@(unless (equal qq "all") `((uin . ,qq)))))
                    ,@(when name `((display . ,name))))))))
           ("reply"
            (cl-incf reply-count)
            (when (> reply-count 1)
              (user-error "qq: Native Gateway accepts at most one reply"))
            (unless (and (qq-gateway--exact-object-keys-p data '(id))
                         (stringp (alist-get 'id data)))
              (user-error "qq: Native Gateway reply segment is malformed"))
            `((kind . "reply")
              (payload
               . ((target
                   . ,(qq-gateway-message--outgoing-reply-target
                       session-key (alist-get 'id data) owner))))))
           (_
            (user-error
             "qq: Native Gateway cannot send segment type %S yet" type)))))
     segments)))

(defun qq-gateway-message--send-request
    (session-key segments raw-message method params callback errback)
  "Send validated SEGMENTS through METHOD with PARAMS for SESSION-KEY."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (pending (qq-state-insert-pending-message
                   session-key segments raw-message))
         (local-id (alist-get 'local-id pending)))
    (cl-labels
        ((fail
          (body reason)
          (qq-state-mark-pending-message-failed session-key local-id reason)
          (qq-gateway--invoke errback body reason)))
      (condition-case error-data
          (qq-gateway--send
           method
           (append `((account_id . ,(car owner))) params)
           (lambda (result)
             (condition-case result-error
                 (let ((receipt
                        (qq-gateway-message--validate-send-receipt
                         result owner)))
                   (if (not (equal owner (qq-gateway-current-account-owner)))
                       (fail nil "Gateway account generation changed during send")
                     (unless
                         (seq-find
                          (lambda (message)
                            (and (equal (alist-get 'local-id message) local-id)
                                 (alist-get 'server-id message)))
                          (qq-state-session-messages session-key))
                       (puthash
                        (qq-gateway-message--pending-send-key
                         owner (alist-get 'client_sequence receipt))
                        (list :session-key session-key
                              :local-id local-id
                              :server-sequence
                              (alist-get 'server_sequence receipt)
                              :random (alist-get 'random receipt))
                        qq-gateway-message--pending-sends))
                     (qq-gateway--invoke callback receipt)))
               (error
                (fail nil (error-message-string result-error)))))
           #'fail)
        (error
         (qq-state-mark-pending-message-failed
          session-key local-id (error-message-string error-data))
         (signal (car error-data) (cdr error-data)))))))

(defun qq-gateway-message-send
    (session-key segments &optional raw-message callback errback)
  "Send closed SEGMENTS to native private/group SESSION-KEY.

Supported elements are text, base face (ID 0 through 259), group mention, and
reply.  Reply metadata is resolved only from an exact message owned by the
selected Gateway generation.  RAW-MESSAGE is an optional optimistic rendering
override."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (native-segments
          (qq-gateway-message--outgoing-segments session-key segments owner))
         (conversation (qq-gateway-message--conversation-params session-key)))
    (qq-gateway-message--send-request
     session-key segments raw-message "message.send"
     `((conversation . ,conversation)
       (segments . ,native-segments))
     callback errback)))

(defun qq-gateway-message-send-text
    (session-key text &optional callback errback)
  "Send TEXT to native private/group SESSION-KEY on the selected account.

The local pending row remains pending after the synchronous Gateway receipt,
because that receipt intentionally has no message ID.  CALLBACK receives the
validated receipt.  ERRBACK receives an error body and reason after the row is
marked failed.  The later exact self `message.received' event promotes it."
  (unless (and (stringp text) (not (string-empty-p text)))
    (user-error "qq: Text message must not be empty"))
  (qq-gateway-message--send-request
   session-key
   `(((type . "text") (data . ((text . ,text)))))
   text
   "message.send_text"
   `((conversation . ,(qq-gateway-message--conversation-params session-key))
     (text . ,text))
   callback errback))

(defun qq-gateway-message--validate-poke-receipt (receipt owner target-uin)
  "Validate poke RECEIPT for exact OWNER and TARGET-UIN."
  (unless (qq-gateway--exact-object-keys-p
           receipt '(account_id generation target_uin))
    (error "qq: Gateway poke receipt has invalid fields"))
  (unless (and (equal (alist-get 'account_id receipt) (car owner))
               (equal (alist-get 'generation receipt) (cdr owner))
               (equal (alist-get 'target_uin receipt) target-uin)
               (qq-gateway--canonical-decimal-p
                (alist-get 'target_uin receipt)))
    (error "qq: Gateway poke receipt contradicts request"))
  (copy-tree receipt))

(defun qq-gateway-message-send-poke
    (session-key target-uin &optional callback errback)
  "Poke exact TARGET-UIN in native private/group SESSION-KEY.

CALLBACK receives an account/generation-scoped acknowledgement.  QQ's empty
OIDB response carries no message identity or server timestamp, so the local
gray-tip row remains explicitly optimistic until a later native event can
replace it."
  (let* ((session (or (qq-state-session session-key)
                      (user-error "qq: Poke requires an existing session")))
         (kind (alist-get 'type session))
         (peer-uin (alist-get 'target-id session))
         (owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner)))
    (unless (and (memq kind '(private group))
                 (qq-gateway--canonical-decimal-p peer-uin))
      (user-error "qq: Native Gateway poke requires a private/group UIN"))
    (unless (qq-gateway--canonical-decimal-p target-uin)
      (user-error "qq: Native Gateway poke target must be an exact UIN"))
    (qq-gateway--send
     "message.poke"
     `((account_id . ,(car owner))
       (conversation
        . ((kind . ,(symbol-name kind))
           (,(if (eq kind 'group) 'group_uin 'peer_uin) . ,peer-uin)))
       (target_uin . ,target-uin))
     (lambda (raw-result)
       (condition-case error-data
           (let ((receipt
                  (qq-gateway-message--validate-poke-receipt
                   raw-result owner target-uin)))
             (unless (equal owner (qq-gateway-current-account-owner))
               (error "qq: Gateway account generation changed during poke"))
             (when-let* ((self-id (qq-state-self-user-id)))
               (qq-state-apply-poke-notice
                `((time . ,(truncate (float-time)))
                  (emacs_local_p . t)
                  (post_type . "notice")
                  (notice_type . "notify")
                  (sub_type . "poke")
                  ,@(if (eq kind 'group)
                        `((group_id . ,peer-uin)
                          (user_id . ,self-id)
                          (target_id . ,target-uin))
                      `((user_id . ,peer-uin)
                        (sender_id . ,self-id)
                        (target_id . ,target-uin))))))
             (qq-gateway--invoke callback receipt))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

(defun qq-gateway-message--validate-reaction-receipt
    (receipt owner message-id sequence emoji-id set)
  "Validate RECEIPT for OWNER, MESSAGE-ID, SEQUENCE, EMOJI-ID, and SET."
  (unless (qq-gateway--exact-object-keys-p
           receipt
           '(account_id generation message_id sequence emoji_id set))
    (error "qq: Gateway reaction receipt has invalid fields"))
  (unless (and (equal (alist-get 'account_id receipt) (car owner))
               (equal (alist-get 'generation receipt) (cdr owner))
               (equal (alist-get 'message_id receipt) message-id)
               (equal (alist-get 'sequence receipt) sequence)
               (equal (alist-get 'emoji_id receipt) emoji-id)
               (eq (alist-get 'set receipt) (if set t :false)))
    (error "qq: Gateway reaction receipt contradicts request"))
  (copy-tree receipt))

(defun qq-gateway-message-set-reaction
    (message emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on native group MESSAGE.

SET non-nil adds the reaction.  CALLBACK receives the validated synchronous
receipt; the later `message.reaction_changed' event reconciles the optimistic
local delta with QQ's authoritative aggregate count."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (sequence (alist-get 'message-seq message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-gateway--canonical-decimal-p group-uin)
                 (qq-gateway--canonical-decimal-p message-id)
                 (qq-gateway--canonical-decimal-p sequence))
      (user-error "qq: Native reaction requires exact group message identity"))
    (unless (and (equal (alist-get 'gateway-account-id message) (car owner))
                 (equal (alist-get 'gateway-generation message) (cdr owner)))
      (user-error "qq: Reaction message belongs to another Gateway generation"))
    (setq emoji-id (format "%s" emoji-id))
    (qq-gateway-message--validate-sequence emoji-id "Reaction emoji ID")
    (qq-gateway--send
     "message.set_reaction"
     `((account_id . ,(car owner))
       (conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id) (sequence . ,sequence)))
       (emoji_id . ,emoji-id)
       (set . ,(if set t :false)))
     (lambda (raw-result)
       (condition-case error-data
           (let ((receipt
                  (qq-gateway-message--validate-reaction-receipt
                   raw-result owner message-id sequence emoji-id set)))
             (unless (equal owner (qq-gateway-current-account-owner))
               (error "qq: Gateway account generation changed during reaction"))
             (when-let* ((self-id (qq-state-self-user-id)))
               (qq-state-apply-emoji-like-notice
                session-key
                `((notice_type . "group_msg_emoji_like")
                  (group_id . ,group-uin)
                  (message_id . ,message-id)
                  (user_id . ,self-id)
                  (is_add . ,(if set t :false))
                  (likes . (((emoji_id . ,emoji-id)
                             (emoji_type
                              . ,(if (<= (length emoji-id) 3) "1" "2"))))))))
             (qq-gateway--invoke callback receipt))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

(defun qq-gateway-message--validate-essence-receipt
    (receipt owner message-id sequence random set)
  "Validate essence RECEIPT for OWNER and the exact native target."
  (unless (qq-gateway--exact-object-keys-p
           receipt
           '(account_id generation message_id sequence random set))
    (error "qq: Gateway essence receipt has invalid fields"))
  (unless (and (equal (alist-get 'account_id receipt) (car owner))
               (equal (alist-get 'generation receipt) (cdr owner))
               (equal (alist-get 'message_id receipt) message-id)
               (equal (alist-get 'sequence receipt) sequence)
               (equal (alist-get 'random receipt) random)
               (eq (alist-get 'set receipt) (if set t :false)))
    (error "qq: Gateway essence receipt contradicts request"))
  (copy-tree receipt))

(defun qq-gateway-message-set-essence
    (message set &optional callback errback)
  "Set or remove native group MESSAGE as an essence message.

SET non-nil sets the essence flag.  CALLBACK receives the validated
synchronous receipt.  The later `message.essence_changed' event is
authoritative and suppresses a racing optimistic receipt update."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (sequence (alist-get 'message-seq message))
         (random (alist-get 'native-random message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-gateway--canonical-decimal-p group-uin)
                 (qq-gateway--canonical-decimal-p message-id)
                 (qq-gateway--canonical-decimal-p sequence)
                 (qq-gateway-message--uint32-p random))
      (user-error "qq: Native essence requires exact group message identity"))
    (unless (and (equal (alist-get 'gateway-account-id message) (car owner))
                 (equal (alist-get 'gateway-generation message) (cdr owner)))
      (user-error "qq: Essence message belongs to another Gateway generation"))
    (let* ((target-key (qq-gateway-message--pending-essence-key
                        owner group-uin sequence random))
           (start-revision
            (gethash target-key qq-gateway-message--essence-revisions 0)))
      (qq-gateway--send
       "message.set_essence"
       `((account_id . ,(car owner))
         (conversation . ((kind . "group") (group_uin . ,group-uin)))
         (message . ((message_id . ,message-id)
                     (sequence . ,sequence)
                     (random . ,random)))
         (set . ,(if set t :false)))
       (lambda (raw-result)
         (condition-case error-data
             (let ((receipt
                    (qq-gateway-message--validate-essence-receipt
                     raw-result owner message-id sequence random set)))
               (unless (equal owner (qq-gateway-current-account-owner))
                 (error "qq: Gateway account generation changed during essence action"))
               (when (= start-revision
                        (gethash target-key
                                 qq-gateway-message--essence-revisions 0))
                 (qq-gateway-message--apply-essence-state
                  message set nil 'request))
               (qq-gateway--invoke callback receipt))
           (error
            (qq-gateway--client-error
             errback "invalid_gateway_result" "%s"
             (error-message-string error-data)))))
       errback))))

(defun qq-gateway-message--validate-recall-receipt
    (receipt owner message-id sequence)
  "Validate recall RECEIPT for OWNER, MESSAGE-ID, and SEQUENCE."
  (unless (qq-gateway--exact-object-keys-p
           receipt '(account_id generation message_id sequence))
    (error "qq: Gateway recall receipt has invalid fields"))
  (unless (and (equal (alist-get 'account_id receipt) (car owner))
               (equal (alist-get 'generation receipt) (cdr owner))
               (equal (alist-get 'message_id receipt) message-id)
               (equal (alist-get 'sequence receipt) sequence))
    (error "qq: Gateway recall receipt contradicts request"))
  (copy-tree receipt))

(defun qq-gateway-message-recall
    (session-key message &optional callback errback)
  "Recall native MESSAGE in SESSION-KEY for the selected Gateway account.

CALLBACK receives the validated SSO receipt; ERRBACK receives an error body
and reason.  State changes only after `message.recalled', which is the
authoritative recall fact."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (message-id (alist-get 'server-id message))
         (sequence (alist-get 'message-seq message))
         (kind (qq-state-session-key-type session-key))
         (conversation (qq-gateway-message--conversation-params session-key)))
    (unless (and (equal (alist-get 'session-key message) session-key)
                 (qq-gateway--canonical-decimal-p message-id)
                 (qq-gateway--canonical-decimal-p sequence))
      (user-error "qq: Native recall requires exact message and sequence identity"))
    (unless (and (equal (alist-get 'gateway-account-id message) (car owner))
                 (equal (alist-get 'gateway-generation message) (cdr owner)))
      (user-error "qq: Message is not owned by selected Gateway generation"))
    (let ((message-params
           `((message_id . ,message-id)
             (sequence . ,sequence))))
      (when (eq kind 'private)
        (let ((client-sequence (alist-get 'native-client-sequence message))
              (random (alist-get 'native-random message))
              (sent-at (alist-get 'native-sent-at message)))
          (unless (and (qq-gateway--canonical-decimal-p client-sequence)
                       (qq-gateway-message--uint32-p random)
                       (qq-gateway-message--uint32-p sent-at)
                       (> sent-at 0))
            (user-error "qq: Private recall requires native correlation metadata"))
          (setq message-params
                (append message-params
                        `((client_sequence . ,client-sequence)
                          (random . ,random)
                          (sent_at . ,sent-at))))))
      (qq-gateway--send
       "message.recall"
       `((account_id . ,(car owner))
         (conversation . ,conversation)
         (message . ,message-params))
       (lambda (result)
         (condition-case result-error
             (qq-gateway--invoke
              callback
              (qq-gateway-message--validate-recall-receipt
               result owner message-id sequence))
           (error
            (qq-gateway--client-error
             errback "invalid_gateway_result" "%s"
             (error-message-string result-error)))))
       errback))))

(defun qq-gateway-message--validate-poke-recall-metadata (message owner)
  "Return MESSAGE's native group poke recall metadata for exact OWNER."
  (let* ((raw-event (alist-get 'raw-event message))
         (metadata (and (listp raw-event)
                        (alist-get 'gateway_recall raw-event))))
    (unless (and (qq-gateway--exact-object-keys-p
                  metadata
                  '(account_id generation conversation message_id sequence
                    sent_at tips_sequence))
                 (equal (alist-get 'account_id metadata) (car owner))
                 (equal (alist-get 'generation metadata) (cdr owner))
                 (equal (alist-get 'message_id metadata)
                        (alist-get 'server-id message))
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'sequence metadata))
                 (integerp (alist-get 'sent_at metadata))
                 (> (alist-get 'sent_at metadata) 0)
                 (qq-gateway--canonical-decimal-p
                  (alist-get 'tips_sequence metadata)))
      (user-error "qq: Poke lacks exact native Gateway recall metadata"))
    (let ((conversation (alist-get 'conversation metadata)))
      (unless (and (qq-gateway--exact-object-keys-p
                    conversation '(kind group_uin))
                   (equal (alist-get 'kind conversation) "group")
                   (equal (qq-state-session-key
                           'group (alist-get 'group_uin conversation))
                          (alist-get 'session-key message)))
        (user-error "qq: Poke recall metadata contradicts its group session")))
    (copy-tree metadata)))

(defun qq-gateway-message-recall-poke
    (message &optional callback errback)
  "Recall authoritative group poke MESSAGE through the selected Gateway.

CALLBACK receives the validated receipt; ERRBACK receives failure details."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_owner (qq-gateway-message--ensure-projection-owner owner))
         (reference (qq-state-poke-recall-reference message))
         (metadata
          (qq-gateway-message--validate-poke-recall-metadata message owner))
         (message-id (alist-get 'message_id metadata))
         (sequence (alist-get 'sequence metadata)))
    (unless reference
      (user-error "qq: Poke has no native recall capability"))
    (when (qq-protocol-poke-recall-reference-expired-p reference)
      (user-error "qq: 戳一戳已超过 2 分钟撤回期限"))
    (qq-gateway--send
     "message.recall_poke"
     `((account_id . ,(car owner))
       (conversation . ,(copy-tree (alist-get 'conversation metadata)))
       (poke . ((message_id . ,message-id)
                (sequence . ,sequence)
                (sent_at . ,(alist-get 'sent_at metadata))
                (tips_sequence . ,(alist-get 'tips_sequence metadata)))))
     (lambda (result)
       (condition-case result-error
           (let ((receipt
                  (qq-gateway-message--validate-recall-receipt
                   result owner message-id sequence)))
             (unless (equal owner (qq-gateway-current-account-owner))
               (error "qq: Gateway account generation changed during poke recall"))
             (qq-state-apply-recall
              (alist-get 'session-key message) message-id)
             (qq-gateway--invoke callback receipt))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string result-error)))))
     errback)))

(add-hook 'qq-gateway-transport-event-hook
          #'qq-gateway-message--handle-event t)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-gateway-message--handle-account-change)
(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-gateway-message--handle-selection-change)
(add-hook 'qq-gateway-transport-state-hook
          #'qq-gateway-message--handle-transport-state)

(provide 'qq-gateway-message)

;;; qq-gateway-message.el ends here
