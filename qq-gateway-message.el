;;; qq-gateway-message.el --- Native Gateway message projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Domain adapter and account-partitioned projection for native service
;; message and recall events.  Every managed account is projected even while
;; another account is selected in the management UI.  The Gateway filters
;; obsolete Native Session work before publishing events; exact session
;; identity is never part of the Emacs wire or projection model.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-gateway-dispatch)
(require 'qq-gateway)
(require 'qq-gateway-attachment)
(require 'qq-gateway-media)
(require 'qq-gateway-rpc)
(require 'qq-gateway-wire)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-gateway-message-event-hook nil
  "Hook called with EVENT and domain DATA for every native message event.")

(defvar qq-gateway-message-projection-error-hook nil
  "Hook called with EVENT, DATA, and REASON when a valid event cannot project.")

(defvar qq-gateway-message--peer-uin-by-uid
  (make-hash-table :test #'equal)
  "Private UID to UIN map keyed by stable account ID and peer UID.")

(defvar qq-gateway-message--pending-recalls
  (make-hash-table :test #'equal)
  "Sequence-scoped recalls awaiting a message in the current projection.")

(defvar qq-gateway-message--pending-reactions
  (make-hash-table :test #'equal)
  "Sequence-scoped reaction events awaiting a projected message.")

(defvar qq-gateway-message--pending-essences
  (make-hash-table :test #'equal)
  "Native target-scoped essence events awaiting a projected message.")

(defvar qq-gateway-message--pending-sends
  (make-hash-table :test #'equal)
  "Client-sequence receipts awaiting an authoritative self message event.")

(defvar qq-gateway-message--live-frontiers
  (make-hash-table :test #'equal)
  "Newest live identity keyed by stable account ID and session.")

(defun qq-gateway-message--message-id-p (value)
  "Return non-nil when VALUE is one canonical, nonzero uint64 Message ID."
  (qq-gateway--uint64-decimal-p value))

(defun qq-gateway-message--event-owner (data)
  "Return the stable ACCOUNT-ID carried by event DATA."
  (alist-get 'account_id data))

(defun qq-gateway-message--current-owner ()
  "Return the account owning the current product/UI context."
  (or (qq-runtime-current-account-id)
      (user-error "qq: Select a QQ account first")))

(defun qq-gateway-message--peer-key (owner uid)
  "Return correlation key for OWNER and private peer UID."
  (list owner uid))

(defun qq-gateway-message--frontier-key (owner session-key)
  "Return live-frontier key for OWNER and SESSION-KEY."
  (list owner session-key))

(defun qq-gateway-message-reset-correlations ()
  "Reset all native message correlation caches.

Account state partitions remain available for simultaneous account views."
  (clrhash qq-gateway-message--peer-uin-by-uid)
  (clrhash qq-gateway-message--pending-recalls)
  (clrhash qq-gateway-message--pending-reactions)
  (clrhash qq-gateway-message--pending-essences)
  (clrhash qq-gateway-message--pending-sends)
  (clrhash qq-gateway-message--live-frontiers)
  nil)

(defun qq-gateway-message--sync-self-info (account)
  "Project native ACCOUNT identity into its active QQ state partition."
  (let ((info
         `((user_id . ,(alist-get 'uin account))
           (uid . ,(alist-get 'uid account))
           (nickname . ,(or (alist-get 'label account)
                            (alist-get 'uin account)
                            "QQ")))))
    (unless (equal info (qq-state-self-info))
      (qq-state-set-self-info info))))

(defun qq-gateway-message--sync-connection-status (account)
  "Project native ACCOUNT and transport state into the active partition."
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

(defun qq-gateway-message--sync-account (owner)
  "Validate OWNER and synchronize its active account-state partition."
  (let ((account (qq-gateway-account owner)))
    (unless account
      (error "qq: Gateway event owner has no managed account snapshot"))
    (qq-gateway-message--sync-self-info account)
    (qq-gateway-message--sync-connection-status account))
  owner)

(defun qq-gateway-message-sync-accounts ()
  "Synchronize state metadata for every managed QQ account."
  (dolist (account (qq-gateway-accounts))
    (let ((owner (alist-get 'account_id account)))
      (qq-runtime-with-account owner
        (qq-gateway-message--sync-account owner)))))

(defun qq-gateway-message--endpoint-self-p (endpoint account)
  "Return non-nil when ENDPOINT identifies owning ACCOUNT."
  (let ((uin (alist-get 'uin endpoint))
        (uid (alist-get 'uid endpoint))
        (self-uin (alist-get 'uin account))
        (self-uid (alist-get 'uid account)))
    (when (or (and uin self-uin (equal uin self-uin))
              (and uid self-uid (equal uid self-uid)))
      (when (and uin self-uin (not (equal uin self-uin)))
        (error "qq: Gateway endpoint UIN contradicts owning account"))
      (when (and uid self-uid (not (equal uid self-uid)))
        (error "qq: Gateway endpoint UID contradicts owning account"))
      t)))

(defun qq-gateway-message--private-context (message account)
  "Return private projection context for MESSAGE and owning ACCOUNT."
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
      (_ (error "qq: Private message endpoints do not identify owning account")))
    (unless (qq-gateway--uint64-decimal-p (alist-get 'uin peer))
      (error "qq: Private message peer lacks an exact UIN"))
    (list :outgoing outgoing :peer peer)))

(defun qq-gateway-message--segment-to-internal (segment)
  "Convert native SEGMENT to shared timeline shape."
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
  (list owner client-sequence))

(defun qq-gateway-message--validate-peer-identity (owner uid uin)
  "Validate exact UID/UIN mapping for projected account OWNER."
  (unless (and (qq-gateway--non-empty-string-p uid)
               (qq-gateway--uint64-decimal-p uin))
    (error "qq: Peer identity requires opaque UID and exact UIN"))
  (when-let* ((known
               (gethash (qq-gateway-message--peer-key owner uid)
                        qq-gateway-message--peer-uin-by-uid)))
    (unless (equal known uin)
      (error "qq: Gateway peer UID contradicts its known UIN")))
  (cons uid uin))

(defun qq-gateway-message--remember-peer-identity (owner uid uin)
  "Remember UID/UIN identity for projected account OWNER."
  (qq-gateway-message--validate-peer-identity owner uid uin)
  (puthash (qq-gateway-message--peer-key owner uid) uin
           qq-gateway-message--peer-uin-by-uid)
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

(defun qq-gateway-message-normalize-snapshot
    (message owner account &optional recalled-p)
  "Purely normalize one Gateway MESSAGE for OWNER and ACCOUNT.

OWNER is the stable opaque account-id.  ACCOUNT supplies the QQ identity needed
to resolve private endpoints.  When RECALLED-P is non-nil, return a recalled
root-summary row.

MESSAGE is already in the domain representation owned by the RPC or event
boundary.  This function checks only client-owned relationships and does not
inspect or mutate pending send, recall, reaction, essence, peer-correlation,
live-frontier, or timeline state.  It also does not allocate a local message
order."
  (unless (and (listp account)
               (equal (alist-get 'account_id account) owner))
    (error "qq: Native message snapshot account contradicts observation owner"))
  (let* ((conversation (alist-get 'conversation message))
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
            (unless recalled-p
              (mapcar #'qq-gateway-message--segment-to-internal
                      (alist-get 'segments message))))
           (mention-kinds (qq-state--mention-kinds-from-segments segments))
           (preview (if recalled-p
                        "[message recalled]"
                      (qq-state-message-preview-from-segments segments)))
           (sender-name
            (if outgoing
                (or (alist-get 'label account) (alist-get 'uin account) "me")
              (or (and (equal kind "group")
                       (alist-get 'sender_card conversation))
                  peer-name sender-id "unknown"))))
      `((id . ,(alist-get 'message_id message))
        (server-id . ,(alist-get 'message_id message))
        (session-key . ,session-key)
        (time . ,(alist-get 'sent_at message))
        (message-seq . ,(alist-get 'sequence message))
        (native-client-sequence . ,(alist-get 'client_sequence message))
        (native-random . ,(alist-get 'random message))
        (gateway-account-id . ,owner)
        (sender-id . ,sender-id)
        (sender-native-id . ,(alist-get 'uid sender))
        (sender-name . ,sender-name)
        (sender-secondary-name . nil)
        (sender-card . ,(and (equal kind "group")
                             (alist-get 'sender_card conversation)))
        (sender-nickname . nil)
        (sender-remark . nil)
        (self-p . ,outgoing)
        (status . ,(cond (recalled-p 'recalled)
                         (outgoing 'sent)
                         (t 'received)))
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
        (target-id . ,(if group-id group-id (alist-get 'uin peer)))))))

(defun qq-gateway-message--normalize-message (data)
  "Normalize native message event DATA for projection.

Unlike `qq-gateway-message-normalize-snapshot', this wrapper attaches local
ordering and pending-send correlation owned by the selected live projection."
  (let* ((owner (qq-gateway-message--event-owner data))
         (message (alist-get 'message data))
         (account (qq-gateway-account owner))
         (normalized
          (qq-gateway-message-normalize-snapshot message owner account)))
    (setf (alist-get 'order normalized nil nil #'eq)
          (qq-state--next-message-order)
          (alist-get 'raw-event normalized nil nil #'eq)
          (copy-tree data))
    (when-let* ((peer-uid (alist-get 'peer-uid normalized))
                (peer-uin (alist-get 'peer-uin normalized)))
      (qq-gateway-message--validate-peer-identity owner peer-uid peer-uin))
    (qq-gateway-message--attach-pending-local-id
     normalized owner message (alist-get 'session-key normalized))))

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
  (list owner conversation-key sequence))

(defun qq-gateway-message--pending-reaction-key (owner group-uin sequence)
  "Return reaction key for OWNER, GROUP-UIN, and exact SEQUENCE."
  (list owner group-uin sequence))

(defun qq-gateway-message--pending-essence-key
    (owner group-uin sequence random)
  "Return essence key for OWNER, GROUP-UIN, SEQUENCE, and RANDOM."
  (list owner group-uin sequence random))

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
         (account
          (qq-gateway-account (alist-get 'gateway-account-id message)))
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
metadata.  Only native push/history projection calls this function; an action
acknowledgement never impersonates a state update."
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
  "Project one account-scoped native message event DATA."
  (let* ((owner (qq-gateway-message--event-owner data))
         (_owner (qq-gateway-message--sync-account owner))
         (normalized (qq-gateway-message--normalize-message data))
         (frontier (qq-gateway-message--plan-live-frontier owner normalized))
         (merged (qq-gateway-message--merge-normalized normalized 'event)))
    (qq-gateway-message--finalize-message-context
     owner (alist-get 'message data) normalized)
    (qq-gateway-message--apply-pending-recall owner normalized merged)
    (qq-gateway-message--apply-pending-reactions owner normalized merged)
    (qq-gateway-message--apply-pending-essence owner normalized merged)
    (when frontier
      (puthash (qq-gateway-message--frontier-key
                owner (alist-get 'session-key normalized))
               frontier
               qq-gateway-message--live-frontiers))
    merged))

(defun qq-gateway-message--plan-live-frontier (owner normalized)
  "Return OWNER's new live frontier for NORMALIZED, or nil if unchanged.

An equal sequence carrying a different message id is a protocol contradiction.
The caller commits the returned value only after the timeline projection has
completed, so a failed projection cannot advance this side index."
  (let* ((session-key (alist-get 'session-key normalized))
         (message-id (alist-get 'server-id normalized))
         (sequence (alist-get 'message-seq normalized))
         (current
          (gethash (qq-gateway-message--frontier-key owner session-key)
                   qq-gateway-message--live-frontiers))
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
  "Return the current UI account's live frontier for SESSION-KEY, or nil.

The result contains exact string `message_id' and `sequence' fields.  History
responses never advance this observation; only `message.received' events do."
  (when-let* ((owner (qq-runtime-current-account-id)))
    (qq-gateway-value-copy
     (gethash (qq-gateway-message--frontier-key owner session-key)
              qq-gateway-message--live-frontiers))))

(defun qq-gateway-message--private-session-by-uid (owner peer-uid)
  "Return OWNER's current private session key for exact PEER-UID, or nil."
  (or (when-let* ((uin
                   (gethash (qq-gateway-message--peer-key owner peer-uid)
                            qq-gateway-message--peer-uin-by-uid)))
        (qq-state-session-key 'private uin))
      (when-let* ((session
                   (seq-find
                    (lambda (candidate)
                      (and (eq (alist-get 'type candidate) 'private)
                           (equal (alist-get 'peer-uid candidate) peer-uid)))
                    (qq-state-sessions))))
        (alist-get 'key session))))

(defun qq-gateway-message--recall-session-key (owner conversation)
  "Return OWNER's projected session key for recall CONVERSATION, or nil."
  (pcase (alist-get 'kind conversation)
    ("group" (qq-state-session-key
              'group (alist-get 'group_uin conversation)))
    ("private" (qq-gateway-message--private-session-by-uid
                owner (alist-get 'peer_uid conversation)))))

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
         (_owner (qq-gateway-message--sync-account owner))
         (recall (alist-get 'recall data))
         (conversation (alist-get 'conversation recall))
         (target (alist-get 'target recall))
         (sequence (alist-get 'sequence target))
         (session-key
          (qq-gateway-message--recall-session-key owner conversation))
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
  "Return shared renderer decoration items for POKE."
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
         (_owner (qq-gateway-message--sync-account owner))
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
             . ((account_id . ,owner)
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
         (_owner (qq-gateway-message--sync-account owner))
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
         (_owner (qq-gateway-message--sync-account owner))
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
    (if message
        (qq-gateway-message--apply-essence-state
         message (eq (alist-get 'is_set essence) t) essence 'event)
      (puthash
       key
       (copy-tree essence)
       qq-gateway-message--pending-essences))))

(defun qq-gateway-message--projection-error (event data error-data)
  "Publish projection ERROR-DATA for Gateway EVENT and domain DATA."
  (let ((reason (error-message-string error-data)))
    (qq-gateway--run-hook
     'qq-gateway-message-projection-error-hook event (copy-tree data) reason)
    (message "qq: Gateway %s projection skipped: %s" event reason)))

(defun qq-gateway-message--handle-event (event data)
  "Observe domainized message EVENT DATA and project its stable owner."
  (qq-gateway--run-hook
   'qq-gateway-message-event-hook event (copy-tree data))
  (when-let* ((owner (qq-gateway-message--event-owner data)))
    (condition-case projection-error
        (qq-runtime-with-account owner
          (pcase event
            ("message.received"
             (qq-gateway-message--project-message data))
            ("message.recalled"
             (qq-gateway-message--project-recall data))
            ("message.poked"
             (qq-gateway-message--project-poke data))
            ("message.reaction_changed"
             (qq-gateway-message--project-reaction data))
            ("message.essence_changed"
             (qq-gateway-message--project-essence data))))
      (error
       (qq-gateway-message--projection-error
        event data projection-error)))))

(defun qq-gateway-message--handle-account-change (_reason account-id)
  "Synchronize account partitions after registry REASON/ACCOUNT-ID."
  (if account-id
      (if (qq-gateway-account account-id)
          (qq-runtime-with-account account-id
            (qq-gateway-message--sync-account account-id))
        (qq-runtime-stop-account account-id t))
    (progn
      (dolist (known-account-id (qq-state-partition-account-ids))
        (unless (qq-gateway-account known-account-id)
          (qq-runtime-stop-account known-account-id t)))
      (qq-gateway-message-sync-accounts))))

(defun qq-gateway-message--handle-transport-state (_state)
  "Refresh every managed account after a transport state transition."
  (dolist (account (qq-gateway-accounts))
    (let ((owner (alist-get 'account_id account)))
      (qq-runtime-with-account owner
        (qq-gateway-message--sync-connection-status account)))))

(defun qq-gateway-message--conversation-params (session-key)
  "Return native service conversation params for SESSION-KEY."
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
  (unless (qq-gateway--uint64-decimal-p value t)
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

(defun qq-gateway-message--history-message-data (owner message)
  "Wrap one history MESSAGE with stable account OWNER context."
  `((account_id . ,owner)
    (message . ,(qq-gateway-value-copy message))))

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

(defun qq-gateway-message--merge-history
    (session-key result owner &optional properties)
  "Merge native history RESULT into SESSION-KEY for OWNER.

Optional PROPERTIES are prefixed to the returned and emitted metadata.  This
lets private roaming history retain its time/random continuation cursor
without pretending that it covered a sequence range."
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
            (append
             properties
             (list
             :session-key session-key
             :account-id owner
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
             (alist-get 'unsupported_message_count result)))))
      (apply #'qq-state--emit 'history
             :mutation 'history :source 'response meta)
      meta)))

(cl-defun qq-gateway-message--call
    (method owner params &key projector callback errback stale-message)
  "Call message METHOD for stable account OWNER through the typed RPC boundary.

PARAMS deliberately exclude account ownership: this helper adds only the
stable `account_id' slot to the public request.  Projection and leaf callbacks
run in OWNER's state partition.  Freshness follows the stable managed slot and
Gateway instance, never UI selection or Native Session identity."
  (when (assq 'account_id params)
    (error "qq: Message RPC params must not duplicate account ownership"))
  (let ((instance-id (qq-gateway-transport-gateway-instance-id)))
    (qq-gateway-rpc-call
     method (append `((account_id . ,owner)) params)
     :current-p
     (lambda ()
       (and (qq-gateway-account owner)
            (equal instance-id
                   (qq-gateway-transport-gateway-instance-id))))
     :stale-message
     (or stale-message "QQ account or Gateway connection changed during request")
     :projector
     (and projector
          (lambda (value)
            (qq-runtime-with-account owner
              (funcall projector value))))
     :callback
     (and callback
          (lambda (value)
            (qq-runtime-with-account owner
              (funcall callback value))))
     :errback
     (and errback
          (lambda (body reason)
            (qq-runtime-with-account owner
              (funcall errback body reason)))))))

(defun qq-gateway-message-get-history
    (session-key start-sequence end-sequence &optional callback errback)
  "Fetch an inclusive native history range for SESSION-KEY.

START-SEQUENCE and END-SEQUENCE are exact canonical decimal strings and the
inclusive range may contain at most 100 sequence values.  CALLBACK receives a
merge metadata plist; ERRBACK receives a Gateway error body and reason."
  (qq-gateway-message--validate-history-range start-sequence end-sequence)
  (let* ((conversation (qq-gateway-message--conversation-params session-key))
         (owner (qq-gateway-message--current-owner)))
    (qq-gateway-message--sync-account owner)
    (qq-gateway-message--call
     "message.get_history" owner
     `((conversation . ,conversation)
       (start_sequence . ,start-sequence)
       (end_sequence . ,end-sequence))
     :projector
     (lambda (result)
       (qq-gateway-message--merge-history session-key result owner))
     :callback callback
     :errback errback)))

(defun qq-gateway-message--private-history-cursor (cursor context)
  "Return a closed copy of private history CURSOR for CONTEXT."
  (unless
      (and (qq-gateway--exact-object-keys-p cursor '(timestamp random))
           (seq-every-p
            (lambda (field)
              (let ((value (alist-get field cursor)))
                (and (integerp value)
                     (<= 0 value 4294967295))))
            '(timestamp random)))
    (error "qq: %s must contain uint32 timestamp and random fields" context))
  `((timestamp . ,(alist-get 'timestamp cursor))
    (random . ,(alist-get 'random cursor))))

(defun qq-gateway-message--merge-private-history
    (session-key result owner requested-cursor)
  "Validate and merge private roaming history RESULT.

REQUESTED-CURSOR is nil for the server-clock bootstrap request."
  (unless
      (and
       (qq-gateway--exact-object-keys-p
        result
        '(account_id requested_cursor response_cursor complete
          unsupported_message_count messages))
       (equal (alist-get 'account_id result) owner)
       (memq (alist-get 'complete result) '(t :false))
       (integerp (alist-get 'unsupported_message_count result))
       (>= (alist-get 'unsupported_message_count result) 0)
       (listp (alist-get 'messages result)))
    (error "qq: Gateway returned an invalid private history page"))
  (let* ((requested
          (qq-gateway-message--private-history-cursor
           (alist-get 'requested_cursor result)
           "Gateway private history requested cursor"))
         (response
          (qq-gateway-message--private-history-cursor
           (alist-get 'response_cursor result)
           "Gateway private history response cursor"))
         (complete (eq (alist-get 'complete result) t))
         (messages (alist-get 'messages result)))
    (when (and requested-cursor
               (not (equal requested requested-cursor)))
      (error "qq: Gateway private history echoed another request cursor"))
    (when (and (not complete)
               (null messages)
               (equal requested response))
      (error "qq: Gateway private history cursor did not advance"))
    (qq-gateway-message--merge-history
     session-key result owner
     (list :private-history-p t
           :requested-private-cursor requested
           :response-private-cursor response
           :history-at-oldest-p complete))))

(defun qq-gateway-message-get-private-history
    (session-key cursor &optional callback errback limit)
  "Fetch one backwards private roaming page for SESSION-KEY.

CURSOR is nil for the server-clock bootstrap request.  Otherwise it is the
exact `response_cursor' returned by the previous page.  CALLBACK receives
merge metadata containing `:response-private-cursor' and
`:history-at-oldest-p'."
  (unless (eq (qq-state-session-key-type session-key) 'private)
    (user-error "qq: Private roaming history requires a private session"))
  (setq limit (or limit qq-history-fetch-count))
  (qq-gateway-message--validate-history-count limit)
  (when cursor
    (setq cursor
          (qq-gateway-message--private-history-cursor
           cursor "Private history cursor")))
  (let* ((conversation (qq-gateway-message--conversation-params session-key))
         (owner (qq-gateway-message--current-owner)))
    (qq-gateway-message--sync-account owner)
    (qq-gateway-message--call
     "message.get_private_history" owner
     `((conversation . ,conversation)
       ,@(when cursor `((cursor . ,cursor)))
       (limit . ,limit))
     :projector
     (lambda (result)
       (qq-gateway-message--merge-private-history
        session-key result owner cursor))
     :callback callback
     :errback errback)))

(defun qq-gateway-message--prepare-outbound (session-key segments owner)
  "Translate UI SEGMENTS into one native outbound message.

Return a plist with `:reply-to', either nil or an exact message-reference
object, and `:segments', the ordered native content elements.  Reply is a
request modifier rather than content.  This adapter checks only facts needed
to translate the local shape or protect its attachment projection; Gateway
owns the outbound domain schema and segment limits."
  (let ((group-p (eq (qq-state-session-key-type session-key) 'group))
        reply-to
        native-segments)
    (dolist (segment segments)
      (let* ((type (alist-get 'type segment))
             (data (alist-get 'data segment))
             (native
              (pcase type
                ("text"
                 (unless (qq-gateway--non-empty-string-p
                          (alist-get 'text data))
                   (user-error "qq: Message text must not be empty"))
                 `((kind . "text")
                   (payload . ((text . ,(alist-get 'text data))))))
                ("face"
                 `((kind . "face")
                   (payload . ((id . ,(alist-get 'id data))))))
                ("at"
                 (unless group-p
                   (user-error "qq: Mentions require a group chat"))
                 (let ((qq (alist-get 'qq data))
                       (name (alist-get 'name data)))
                   `((kind . "mention")
                     (payload
                      . ((target
                          . ((kind
                              . ,(if (equal qq "all") "all" "user"))
                             ,@(unless (equal qq "all")
                                 `((uin . ,qq)))))
                         ,@(when name `((display . ,name))))))))
                ("image"
                 (let ((attachment-id (alist-get 'attachment_id data)))
                   (qq-gateway-attachment-assert-sendable
                    attachment-id session-key owner)
                   `((kind . "image")
                     (payload . ((attachment_id . ,attachment-id))))))
                ("record"
                 (let ((attachment-id (alist-get 'attachment_id data)))
                   (qq-gateway-attachment-assert-sendable
                    attachment-id session-key owner "record")
                   `((kind . "record")
                     (payload . ((attachment_id . ,attachment-id))))))
                ("reply"
                 (unless (qq-gateway-message--message-id-p
                          (alist-get 'id data))
                   (user-error "qq: Reply target has no exact Message ID"))
                 (when reply-to
                   (user-error "qq: A message has only one reply target"))
                 (setq reply-to
                       `((message_id . ,(alist-get 'id data))))
                 nil)
                (_
                 (user-error
                  "qq: Native Gateway cannot send segment type %S yet"
                  type)))))
        (when native
          (push native native-segments))))
    (setq native-segments (nreverse native-segments))
    (unless native-segments
      (user-error "qq: Enter message content before sending"))
    (list :reply-to reply-to :segments native-segments)))

(defun qq-gateway-message--send-request
    (session-key segments raw-message method params callback errback)
  "Send prepared SEGMENTS through METHOD with PARAMS for SESSION-KEY."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (pending (qq-state-insert-pending-message
                   session-key segments raw-message))
         (local-id (alist-get 'local-id pending)))
    (cl-labels
        ((fail
           (body reason)
           (qq-state-mark-pending-message-failed session-key local-id reason)
           (qq-gateway--invoke errback body reason)))
      (condition-case error-data
          (qq-gateway-message--call
           method owner params
           :projector
           (lambda (receipt)
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
             receipt)
           :callback callback
           :errback #'fail
           :stale-message "QQ account or Gateway connection changed during send")
        (error
         (qq-state-mark-pending-message-failed
          session-key local-id (error-message-string error-data))
         (signal (car error-data) (cdr error-data)))))))

(defun qq-gateway-message-send
    (session-key segments &optional raw-message callback errback
                 optimistic-segments)
  "Send SEGMENTS to native private/group SESSION-KEY.

Supported elements are text, base face (ID 0 through 259), group mention,
reply, and already prepared image/record attachments.  One optional reply is
lifted into the request envelope and carries only the exact target message ID;
the enclosing conversation completes its stable Message Reference and Gateway
resolves native metadata.  At least one and at most 128 non-reply content
elements are required.  RAW-MESSAGE is an optional optimistic rendering
override.
OPTIMISTIC-SEGMENTS, when non-nil, are stored in the pending row instead of
protocol-ready SEGMENTS so local media previews never enter the wire request.
The original reply element remains part of this local rendering shape."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (outbound
          (qq-gateway-message--prepare-outbound session-key segments owner))
         (reply-to (plist-get outbound :reply-to))
         (native-segments (plist-get outbound :segments))
         (conversation (qq-gateway-message--conversation-params session-key)))
    (qq-gateway-message--send-request
     session-key (or optimistic-segments segments) raw-message "message.send"
     `((conversation . ,conversation)
       ,@(when reply-to `((reply_to . ,reply-to)))
       (segments . ,native-segments))
     callback errback)))

(defun qq-gateway-message-send-poke
    (session-key target-uin &optional callback errback)
  "Poke exact TARGET-UIN in native private/group SESSION-KEY.

CALLBACK receives an account-scoped acknowledgement.  QQ's empty
OIDB response carries no message identity or server timestamp, so the local
gray-tip row remains explicitly optimistic until a later native event can
replace it."
  (let* ((session (or (qq-state-session session-key)
                      (user-error "qq: Poke requires an existing session")))
         (kind (alist-get 'type session))
         (peer-uin (alist-get 'target-id session))
         (owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner)))
    (unless (and (memq kind '(private group))
                 (qq-gateway--uint64-decimal-p peer-uin))
      (user-error "qq: Native Gateway poke requires a private/group UIN"))
    (unless (qq-gateway--uint64-decimal-p target-uin)
      (user-error "qq: Native Gateway poke target must be an exact UIN"))
    (qq-gateway-message--call
     "message.poke" owner
     `((conversation
       . ((kind . ,(symbol-name kind))
           (,(if (eq kind 'group) 'group_uin 'peer_uin) . ,peer-uin)))
       (target_uin . ,target-uin))
     :projector
     (lambda (receipt)
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
       receipt)
     :callback callback
     :errback errback
     :stale-message "QQ account or Gateway connection changed during poke")))

(defun qq-gateway-message-set-reaction
    (message emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on native group MESSAGE.

SET non-nil adds the reaction.  CALLBACK receives the receipt.  Only the
authoritative `message.reaction_changed' update changes local reaction state."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-gateway--uint64-decimal-p group-uin)
                 (qq-gateway-message--message-id-p message-id))
      (user-error "qq: Native reaction requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Reaction message belongs to another Gateway account"))
    (setq emoji-id (format "%s" emoji-id))
    (qq-gateway-message--validate-sequence emoji-id "Reaction emoji ID")
    (qq-gateway-message--call
     "message.set_reaction" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (emoji_id . ,emoji-id)
       (set . ,(if set t :false)))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during reaction")))

(defun qq-gateway-message-set-essence
    (message set &optional callback errback)
  "Set or remove native group MESSAGE as an essence message.

SET non-nil sets the essence flag.  CALLBACK receives the receipt.  Only the
authoritative `message.essence_changed' update changes local essence state."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-gateway--uint64-decimal-p group-uin)
                 (qq-gateway-message--message-id-p message-id))
      (user-error "qq: Native essence requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Essence message belongs to another Gateway account"))
    (qq-gateway-message--call
     "message.set_essence" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (set . ,(if set t :false)))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during essence action")))

(defun qq-gateway-message-set-todo
    (message operation &optional callback errback)
  "Apply todo OPERATION to native group MESSAGE.

OPERATION is one of `set', `complete', or `cancel'.  CALLBACK receives the
receipt.  No local todo state is invented because native query/event semantics
are not yet part of the Gateway protocol."
  (unless (memq operation '(set complete cancel))
    (user-error "qq: Unknown native todo operation %S" operation))
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (operation-name (symbol-name operation)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-gateway--uint64-decimal-p group-uin)
                 (qq-gateway-message--message-id-p message-id))
      (user-error "qq: Native todo requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Todo message belongs to another Gateway account"))
    (qq-gateway-message--call
     "message.set_todo" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (operation . ,operation-name))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during todo action")))

(defun qq-gateway-message--read-request (message owner)
  "Return an exact reference-only read request for MESSAGE and OWNER.

The result is a plist containing `:message-id' and `:params'.  This is the
single capability check and request builder; it has no projection or transport
side effects."
  (let* ((session-key (and (listp message)
                           (alist-get 'session-key message)))
         (kind (and session-key (qq-state-session-key-type session-key)))
         (message-id (and (listp message) (alist-get 'server-id message)))
         conversation)
    (unless (and owner
                 (memq kind '(private group))
                 (qq-gateway-message--message-id-p message-id))
      (user-error "qq: Native read report requires an exact Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Read target belongs to another Gateway account"))
    (setq conversation
          (qq-gateway-message--conversation-params session-key))
    (list :message-id message-id
          :params
          `((conversation . ,conversation)
            (message . ((message_id . ,message-id)))))))

(defun qq-gateway-message-read-capable-p (message)
  "Return non-nil when MESSAGE is an exact reference for the current owner."
  (when-let* ((owner (qq-runtime-current-account-id)))
    (condition-case nil
        (progn
          (qq-gateway-message--read-request message owner)
          t)
      (error nil))))

(defun qq-gateway-message-mark-read
    (message &optional callback errback)
  "Mark the selected account's conversation read through MESSAGE.

MESSAGE supplies only its stable public conversation locator and exact
message ID.  Gateway resolves the corresponding Native read boundary.
CALLBACK receives the acknowledgement; ERRBACK receives an error
body and reason."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (request (qq-gateway-message--read-request message owner)))
    (qq-gateway-message--call
     "message.mark_read" owner
     (plist-get request :params)
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during read report")))

(defun qq-gateway-message-recall
    (session-key message &optional callback errback)
  "Recall native MESSAGE in SESSION-KEY for the selected QQ account.

CALLBACK receives the SSO receipt; ERRBACK receives an error body
and reason.  A successful typed acknowledgement marks the local message
recalled; a later `message.recalled' event is an idempotent reconciliation."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (message-id (alist-get 'server-id message))
         (conversation (qq-gateway-message--conversation-params session-key)))
    (unless (and (equal (alist-get 'session-key message) session-key)
                 (qq-gateway-message--message-id-p message-id))
      (user-error "qq: Native recall requires an exact Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Message is not owned by selected Gateway account"))
    (qq-gateway-message--call
     "message.recall" owner
     `((conversation . ,conversation)
       (message . ((message_id . ,message-id))))
     :projector
     (lambda (receipt)
       (qq-state-apply-recall session-key message-id)
       receipt)
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during recall")))

(defun qq-gateway-message--validate-poke-recall-metadata (message owner)
  "Return MESSAGE's native poke recall metadata for OWNER's account slot."
  (let* ((raw-event (alist-get 'raw-event message))
         (metadata (and (listp raw-event)
                        (alist-get 'gateway_recall raw-event))))
    (unless (and (qq-gateway--exact-object-keys-p
                  metadata
                  '(account_id conversation message_id sequence sent_at
                    tips_sequence))
                 (equal (alist-get 'account_id metadata) owner)
                 (equal (alist-get 'message_id metadata)
                        (alist-get 'server-id message))
                 (qq-gateway-message--message-id-p
                  (alist-get 'message_id metadata))
                 (qq-gateway--uint64-decimal-p
                  (alist-get 'sequence metadata) t)
                 (integerp (alist-get 'sent_at metadata))
                 (> (alist-get 'sent_at metadata) 0)
                 (qq-gateway--uint64-decimal-p
                  (alist-get 'tips_sequence metadata) t))
      (user-error "qq: Poke lacks exact native service recall metadata"))
    (let ((conversation (alist-get 'conversation metadata)))
      (unless (and (qq-gateway--exact-object-keys-p
                    conversation '(kind group_uin))
                   (equal (alist-get 'kind conversation) "group")
                   (qq-gateway--uint64-decimal-p
                    (alist-get 'group_uin conversation))
                   (equal (qq-state-session-key
                           'group (alist-get 'group_uin conversation))
                          (alist-get 'session-key message)))
        (user-error "qq: Poke recall metadata contradicts its group session")))
    (copy-tree metadata)))

(defun qq-gateway-message-recall-poke
    (message &optional callback errback)
  "Recall authoritative group poke MESSAGE through the selected Gateway.

CALLBACK receives the receipt; ERRBACK receives failure details."
  (let* ((owner (qq-gateway-message--current-owner))
         (_owner (qq-gateway-message--sync-account owner))
         (reference (qq-state-poke-recall-reference message))
         (metadata
          (qq-gateway-message--validate-poke-recall-metadata message owner))
         (message-id (alist-get 'message_id metadata))
         (sequence (alist-get 'sequence metadata)))
    (unless reference
      (user-error "qq: Poke has no native recall capability"))
    (when (qq-protocol-poke-recall-reference-expired-p reference)
      (user-error "qq: 戳一戳已超过 2 分钟撤回期限"))
    (qq-gateway-message--call
     "message.recall_poke" owner
     `((conversation . ,(copy-tree (alist-get 'conversation metadata)))
       (poke . ((message_id . ,message-id)
                (sequence . ,sequence)
                (sent_at . ,(alist-get 'sent_at metadata))
                (tips_sequence . ,(alist-get 'tips_sequence metadata)))))
     :projector
     (lambda (receipt)
       (qq-state-apply-recall
        (alist-get 'session-key message) message-id)
       receipt)
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during poke recall")))

(dolist (event '("message.received" "message.recalled" "message.poked"
                 "message.reaction_changed" "message.essence_changed"))
  (qq-gateway-dispatch-register-event
   event #'qq-gateway-message--handle-event))
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-gateway-message--handle-account-change)
(add-hook 'qq-gateway-transport-state-hook
          #'qq-gateway-message--handle-transport-state)

(provide 'qq-gateway-message)

;;; qq-gateway-message.el ends here
