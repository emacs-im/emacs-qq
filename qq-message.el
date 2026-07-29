;;; qq-message.el --- QQ service message projection -*- lexical-binding: t; -*-

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
(require 'qq-rpc)
(require 'qq-account)
(require 'qq-attachment)
(require 'qq-remote-media)
(require 'qq-resource)
(require 'qq-server)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)

(defvar qq-message-event-hook nil
  "Hook called with EVENT and domain DATA for every native message event.")

(defvar qq-message-projection-error-hook nil
  "Hook called with EVENT, DATA, and REASON when a valid event cannot project.")

(defvar qq-message--peer-uin-by-uid
  (make-hash-table :test #'equal)
  "Private UID to UIN map keyed by stable account ID and peer UID.")

(defvar qq-message--pending-recalls
  (make-hash-table :test #'equal)
  "Sequence-scoped recalls awaiting a message in the current projection.")

(defvar qq-message--pending-reactions
  (make-hash-table :test #'equal)
  "Sequence-scoped reaction events awaiting a projected message.")

(defvar qq-message--pending-essences
  (make-hash-table :test #'equal)
  "Native target-scoped essence events awaiting a projected message.")

(defvar qq-message--pending-sends
  (make-hash-table :test #'equal)
  "Client-sequence receipts awaiting an authoritative self message event.")

(defvar qq-message--live-frontiers
  (make-hash-table :test #'equal)
  "Newest live identity keyed by stable account ID and session.")

(defconst qq-message-max-merged-forward-messages 500
  "Maximum source messages accepted by one native merged forward.")

(defconst qq-message-max-dataline-text-bytes (* 1024 1024)
  "Maximum reassembled DataLine text accepted from the Gateway timeline.")

(defun qq-message--message-id-p (value)
  "Return non-nil when VALUE is one canonical, nonzero uint64 Message ID."
  (qq-account--uint64-decimal-p value))

(defun qq-message--event-owner (data)
  "Return the stable ACCOUNT-ID carried by event DATA."
  (alist-get 'account_id data))

(defun qq-message--current-owner ()
  "Return the account owning the current product/UI context."
  (or (qq-runtime-current-account-id)
      (user-error "qq: Select a QQ account first")))

(defun qq-message--peer-key (owner uid)
  "Return correlation key for OWNER and private peer UID."
  (list owner uid))

(defun qq-message--frontier-key (owner session-key)
  "Return live-frontier key for OWNER and SESSION-KEY."
  (list owner session-key))

(defun qq-message-reset-correlations ()
  "Reset all native message correlation caches.

Account state partitions remain available for simultaneous account views."
  (clrhash qq-message--peer-uin-by-uid)
  (clrhash qq-message--pending-recalls)
  (clrhash qq-message--pending-reactions)
  (clrhash qq-message--pending-essences)
  (clrhash qq-message--pending-sends)
  (clrhash qq-message--live-frontiers)
  nil)

(defun qq-message--sync-self-info (account)
  "Project native ACCOUNT identity into its active QQ state partition."
  (let ((info
         `((user_id . ,(alist-get 'uin account))
           (uid . ,(alist-get 'uid account))
           (nickname . ,(or (alist-get 'label account)
                            (alist-get 'uin account)
                            "QQ")))))
    (unless (equal info (qq-state-self-info))
      (qq-state-set-self-info info))))

(defun qq-message--sync-connection-status (account)
  "Project native ACCOUNT and transport state into the active partition."
  (qq-state-set-connection-status
   (pcase (qq-server-state)
     ('ready
      (pcase (alist-get 'phase account)
        ("online" 'ready)
        ("reconnecting" 'reconnecting)
        ((or "starting" "login_required" "logging_in") 'connecting)
        (_ 'disconnected)))
     ((or 'connecting 'authenticating) 'connecting)
     ('reconnecting 'reconnecting)
     (_ 'disconnected))))

(defun qq-message--sync-account (owner)
  "Validate OWNER and synchronize its active account-state partition."
  (let ((account (qq-account-get owner)))
    (unless account
      (error "qq: Gateway event owner has no managed account snapshot"))
    (qq-message--sync-self-info account)
    (qq-message--sync-connection-status account))
  owner)

(defun qq-message-sync-accounts ()
  "Synchronize state metadata for every managed QQ account."
  (dolist (account (qq-account-list))
    (let ((owner (alist-get 'account_id account)))
      (qq-runtime-with-account owner
        (qq-message--sync-account owner)))))

(defun qq-message--endpoint-self-p (endpoint account)
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

(defun qq-message--resolve-private-peer-uin (owner peer)
  "Return PEER with a decimal UIN, resolving from OWNER's known UID map.

Live private self-echoes sometimes carry only the peer UID.  Dropping those
pushes left optimistic sends stuck without a snowflake."
  (let ((uin (alist-get 'uin peer))
        (uid (alist-get 'uid peer)))
    (cond
     ((qq-account--uint64-decimal-p uin) peer)
     ((qq-account--non-empty-string-p uid)
      (let ((resolved
             (or (gethash (qq-message--peer-key owner uid)
                          qq-message--peer-uin-by-uid)
                 (when-let* ((session
                              (seq-find
                               (lambda (candidate)
                                 (and (eq (alist-get 'type candidate)
                                          'private)
                                      (equal (alist-get 'peer-uid candidate)
                                             uid)))
                               (qq-state-sessions))))
                   (alist-get
                    'target-id
                    (qq-state-session-key-identity
                     (alist-get 'key session)))))))
        (unless (qq-account--uint64-decimal-p resolved)
          (error "qq: Private message peer lacks an exact UIN"))
        (if (equal (alist-get 'uin peer) resolved)
            peer
          (append `((uin . ,resolved)) peer))))
     (t (error "qq: Private message peer lacks an exact UIN")))))

(defun qq-message--private-context (message account)
  "Return private projection context for MESSAGE and owning ACCOUNT."
  (let* ((owner (alist-get 'account_id account))
         (sender (alist-get 'sender message))
         (recipient (alist-get 'recipient message))
         (sender-self (qq-message--endpoint-self-p sender account))
         (recipient-self
          (qq-message--endpoint-self-p recipient account))
         outgoing peer)
    (pcase (list (and sender-self t) (and recipient-self t))
      (`(t nil) (setq outgoing t peer recipient))
      (`(nil t) (setq outgoing nil peer sender))
      (`(t t) (setq outgoing t peer recipient))
      (_ (error "qq: Private message endpoints do not identify owning account")))
    (setq peer (qq-message--resolve-private-peer-uin owner peer))
    (list :outgoing outgoing :peer peer)))

(defun qq-message--segment-to-internal (segment)
  "Convert native SEGMENT to shared timeline shape."
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (pcase kind
      ("forward_card"
       `((type . "card")
         (data . ((kind . "forward")
                  (reference . ,(copy-tree
                                 (alist-get 'reference payload)))
                  (presentation . ,(copy-tree
                                    (alist-get 'presentation payload)))))))
      ("light_app"
       (let* ((preview (alist-get 'preview payload))
              (preview (cond
                        ((vectorp preview) (append preview nil))
                        ((listp preview) preview)))
              (content (and preview (string-join preview "\n")))
              (app (alist-get 'app payload))
              (prompt (alist-get 'prompt payload))
              (source (or (alist-get 'source payload) app)))
         `((type . "card")
           (data . ((kind . "app")
                    (app . ,app)
                    (title . ,prompt)
                    (content . ,content)
                    (source . ,source)
                    (summary . ,(alist-get 'summary payload))
                    (prompt . ,prompt))))))
      ("market_face"
       `((type . "mface")
         (data . ((emoji_package_id . ,(alist-get 'package_id payload))
                  (emoji_id . ,(alist-get 'emoji_id payload))
                  (summary . ,(alist-get 'summary payload))
                  (url . ,(alist-get 'url payload))
                  (width . ,(alist-get 'width payload))
                  (height . ,(alist-get 'height payload))))))
      ("reply"
       `((type . "reply")
         (data . ,(qq-state--native-reply-data
                   (alist-get 'target payload)))))
      ("unsupported"
       `((type . "__unsupported")
         (data . ((native_keys . ,(copy-tree (alist-get 'native_keys payload)))
                  (summary . ,(alist-get 'summary payload))
                  ,@(when-let* ((raw (alist-get 'raw payload)))
                      `((fallback_text . ,(alist-get 'fallback_text raw))))))))
      (_ `((type . ,kind) (data . ,(copy-tree payload)))))))

(defun qq-message-get-forward
    (resource-id scene &optional callback errback)
  "Fetch merged-forward entries behind opaque RESOURCE-ID in SCENE.

The returned entries are transient viewer data.  They are not merged into the
ordinary chat timeline or message store.  CALLBACK receives a plist containing
`:messages' and `:unsupported-message-count'."
  (unless (qq-account--non-empty-string-p resource-id)
    (user-error "qq: Merged-forward resource id must be non-empty"))
  (unless (member scene '("group" "private" "group_temp"))
    (user-error "qq: Merged-forward scene is invalid"))
  (let ((owner (qq-message--current-owner)))
    (qq-message--sync-account owner)
    (qq-message--call
     "message.get_forward" owner
     `((resource_id . ,resource-id)
       (scene . ,scene))
     :projector
     (lambda (result)
       (unless
           (and (qq-account--exact-object-keys-p
                 result '(account_id unsupported_message_count messages))
                (equal (alist-get 'account_id result) owner)
                (integerp (alist-get 'unsupported_message_count result))
                (>= (alist-get 'unsupported_message_count result) 0)
                (listp (alist-get 'messages result)))
         (error "qq: Gateway returned invalid merged-forward messages"))
       (list
        :messages
        (qq-server-value-copy (alist-get 'messages result))
        :unsupported-message-count
        (alist-get 'unsupported_message_count result)))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during merged-forward fetch")))

(defun qq-message-send-merged-forward
    (source-session-key destination-session-key message-ids
                        &optional callback errback)
  "Send MESSAGE-IDS from SOURCE-SESSION-KEY as one native merged forward.

The Gateway resolves every stable message ID from its durable Message Store,
preserves caller order (including intentional duplicates), uploads the native
long-message bag, and publishes its card to DESTINATION-SESSION-KEY."
  (unless (and (listp message-ids)
               (<= 1 (length message-ids)
                   qq-message-max-merged-forward-messages))
    (user-error "qq: Merged forward requires between 1 and %d messages"
                qq-message-max-merged-forward-messages))
  (dolist (message-id message-ids)
    (unless (qq-message--message-id-p message-id)
      (user-error "qq: Merged forward message IDs must be canonical uint64 strings")))
  (let ((owner (qq-message--current-owner)))
    (qq-message--sync-account owner)
    (qq-message--call
     "message.send_merged_forward" owner
     `((destination
        . ,(qq-message--conversation-params destination-session-key))
       (source . ,(qq-message--conversation-params source-session-key))
       (message_ids . ,(copy-sequence message-ids)))
     :projector
     (lambda (receipt)
       (unless
           (and
            (qq-account--exact-object-keys-p
             receipt
             '(account_id resource_id sent_at server_sequence
               client_sequence random))
            (equal (alist-get 'account_id receipt) owner)
            (qq-account--non-empty-string-p
             (alist-get 'resource_id receipt))
            (integerp (alist-get 'sent_at receipt))
            (qq-account--uint64-decimal-p
             (alist-get 'server_sequence receipt) t)
            (qq-account--uint64-decimal-p
             (alist-get 'client_sequence receipt) t)
            (integerp (alist-get 'random receipt))
            (<= 0 (alist-get 'random receipt) #xffffffff))
         (error "qq: Gateway returned an invalid merged-forward receipt"))
       (qq-server-value-copy receipt))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during merged-forward send")))

(defun qq-message--pending-send-key (owner client-sequence)
  "Return exact correlation key for OWNER and CLIENT-SEQUENCE."
  (list owner client-sequence))

(defun qq-message--validate-peer-identity (owner uid uin)
  "Validate exact UID/UIN mapping for projected account OWNER."
  (unless (and (qq-account--non-empty-string-p uid)
               (qq-account--uint64-decimal-p uin))
    (error "qq: Peer identity requires opaque UID and exact UIN"))
  (when-let* ((known
               (gethash (qq-message--peer-key owner uid)
                        qq-message--peer-uin-by-uid)))
    (unless (equal known uin)
      (error "qq: Gateway peer UID contradicts its known UIN")))
  (cons uid uin))

(defun qq-message--remember-peer-identity (owner uid uin)
  "Remember UID/UIN identity for projected account OWNER."
  (qq-message--validate-peer-identity owner uid uin)
  (puthash (qq-message--peer-key owner uid) uin
           qq-message--peer-uin-by-uid)
  (cons uid uin))

(defun qq-message--decimal-equal-p (left right)
  "Return non-nil when LEFT and RIGHT are the same decimal identity text."
  (equal (format "%s" (or left ""))
         (format "%s" (or right ""))))

(defun qq-message--random-compatible-p (message-random pending-random)
  "Return non-nil when MESSAGE-RANDOM is absent or equals PENDING-RANDOM.

Private self-echoes often omit ContentHead.random; requiring it blocked rekey."
  (or (null message-random)
      (and (numberp message-random)
           (numberp pending-random)
           (= message-random pending-random))))

(defun qq-message--pending-send-matches-p
    (pending message session-key &optional client-sequence-hit)
  "Return non-nil when PENDING receipt metadata matches MESSAGE in SESSION-KEY.

When CLIENT-SEQUENCE-HIT is non-nil, MESSAGE already keyed the same outbound
client_sequence as the send receipt (via ContentHead field 5 or 11).  In that
case only session + random (if present) are required.

Live C2C evidence: after PbSendMsg, receipt.client_sequence is the outbound
request field 4, while receipt.server_sequence is PbSendMsgResp field 11.  The
later CommonMessage often stores those values inverted relative to group
pushes — ContentHead.Sequence carries the outbound client_sequence and
ContentHead.client_sequence carries the conversation sequence — so equality
must accept either field against either receipt value."
  (and (equal session-key (plist-get pending :session-key))
       (qq-message--random-compatible-p
        (alist-get 'random message)
        (plist-get pending :random))
       (or client-sequence-hit
           (let ((server-sequence (plist-get pending :server-sequence))
                 (pending-client (plist-get pending :client-sequence))
                 (push-sequence (alist-get 'sequence message))
                 (push-client (alist-get 'client_sequence message)))
             (or (qq-message--decimal-equal-p push-sequence server-sequence)
                 (qq-message--decimal-equal-p push-client server-sequence)
                 (qq-message--decimal-equal-p push-sequence pending-client)
                 (qq-message--decimal-equal-p push-client pending-client))))))

(defun qq-message--pending-lookup-key (owner sequence)
  "Return pending-send hash key for OWNER and SEQUENCE when SEQUENCE is usable."
  (and sequence
       (not (member (format "%s" sequence) '("" "0" "nil")))
       (qq-message--pending-send-key owner (format "%s" sequence))))

(defun qq-message--find-pending-send (owner message session-key)
  "Return (KEY . PENDING) for OWNER's MESSAGE, preferring client_sequence.

Group self-echoes sometimes omit ContentHead.client_sequence (wire 0 / absent).
Private self-echoes often keep the outbound client_sequence on ContentHead
field 5 instead of field 11.  Try both before falling back to a full scan."
  (let* ((client-sequence (alist-get 'client_sequence message))
         (push-sequence (alist-get 'sequence message))
         (candidate-keys
          (delq nil
                (delete-dups
                 (list (qq-message--pending-lookup-key owner client-sequence)
                       (qq-message--pending-lookup-key owner push-sequence))))))
    (or
     (seq-some
      (lambda (client-key)
        (when-let* ((pending (gethash client-key qq-message--pending-sends))
                    ((qq-message--pending-send-matches-p
                      pending message session-key t)))
          (cons client-key pending)))
      candidate-keys)
     (let (found)
       (maphash
        (lambda (candidate-key candidate)
          (when (and (null found)
                     (equal (car candidate-key) owner)
                     (qq-message--pending-send-matches-p
                      candidate message session-key nil))
            (setq found (cons candidate-key candidate))))
        qq-message--pending-sends)
       found))))
(defun qq-message--attach-pending-local-id
    (normalized owner message session-key)
  "Attach OWNER's pending local ID to NORMALIZED when MESSAGE metadata matches.

SESSION-KEY must equal the conversation recorded with the send receipt."
  (when-let* ((match (qq-message--find-pending-send owner message session-key))
              (pending (cdr match)))
    ;; Consume the receipt only after the authoritative message has merged.
    ;; This keeps normalization free of correlation side effects, which is
    ;; required when a complete history page is preflighted before commit.
    (setf (alist-get 'local-id normalized nil nil #'eq)
          (plist-get pending :local-id)))
  normalized)

(defun qq-message--history-observation-anchor
    (owner session-key sequence random)
  "Return an opaque local anchor for one sequence-only history observation.

OWNER and SESSION-KEY scope QQ's conversation-local SEQUENCE.  RANDOM is
retained when present so the anchor remains descriptive, but it is not exposed
as a server message identity."
  (unless (and (stringp owner) (not (string-empty-p owner)))
    (error "qq: Sequence-only history requires an account owner"))
  (unless (qq-protocol--nonzero-decimal-string-p sequence)
    (error "qq: Sequence-only history requires a nonzero native sequence"))
  (format "history:%s:%s:%s:%s"
          owner session-key sequence
          (if (integerp random) random "none")))

(defun qq-message--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-message-normalize-snapshot
    (message owner account &optional recalled-p history-p)
  "Purely normalize one Gateway MESSAGE for OWNER and ACCOUNT.

OWNER is the stable opaque account-id.  ACCOUNT supplies the QQ identity needed
to resolve private endpoints.  When RECALLED-P is non-nil, return a recalled
root-summary row.  When HISTORY-P is non-nil, a group-history observation may
lack an exact NT snowflake and receives an opaque client-only timeline anchor.

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
         (sender-presentation (alist-get 'sender_presentation message))
         (sender-nickname
          (qq-message--present-string
           (alist-get 'nickname sender-presentation)))
         (sender-remark
          (qq-message--present-string
           (alist-get 'remark sender-presentation)))
         (sender-member-name
          (qq-message--present-string
           (alist-get 'member_name sender-presentation)))
         (sender-id (or (alist-get 'uin sender) (alist-get 'uid sender)))
         session-key peer peer-name outgoing group-id)
    (pcase kind
      ("private"
       (let ((context (qq-message--private-context message account)))
         (setq outgoing (plist-get context :outgoing)
               peer (plist-get context :peer)
               session-key (qq-state-session-key
                            'private (alist-get 'uin peer)))
         (let* ((friend (qq-state-friend (alist-get 'uin peer)))
                (session (qq-state-session session-key)))
           (setq peer-name
                 (or (qq-message--present-string
                      (alist-get 'remark friend))
                     (qq-message--present-string
                      (alist-get 'nickname friend))
                     (qq-message--present-string
                      (alist-get 'title session))
                     (alist-get 'uin peer))))))
      ("group"
       (setq outgoing (and (qq-message--endpoint-self-p sender account) t)
             group-id (alist-get 'group_uin conversation)
             session-key (qq-state-session-key 'group group-id))
       (let ((group (qq-state-group group-id)))
         (setq peer-name
               (or (qq-message--present-string
                    (alist-get 'group_name conversation))
                   (qq-message--present-string
                    (alist-get 'group_name group))
                   group-id))))
      ("temp" (error "qq: Temp conversations are not projected yet")))
    (let* ((server-id
            (qq-protocol-optional-message-id
             (alist-get 'message_id message) "Native message snapshot"))
           (history-anchor
            (when (null server-id)
              (unless history-p
                (error "qq: Live Native message snapshot requires message_id"))
              (unless (equal kind "group")
                (error "qq: Only group history may omit message_id"))
              (qq-message--history-observation-anchor
               owner session-key
               (alist-get 'sequence message)
               (alist-get 'random message))))
           (segments
            (unless recalled-p
              (mapcar #'qq-message--segment-to-internal
                      (alist-get 'segments message))))
           (mention-kinds (qq-state--mention-kinds-from-segments segments))
           (preview (if recalled-p
                        "[message recalled]"
                      (qq-state-message-preview-from-segments segments)))
           (presentation-name
            ;; LinuxQQ's display order over exact sender snapshot fields.
            ;; Missing presentation is a Rust/Gateway contract violation; a
            ;; QQ number or chat title must not be substituted here.
            (or sender-member-name sender-remark sender-nickname
                (error "qq: Native message omitted sender presentation")))
           (sender-name presentation-name)
           (sender-secondary-name
            (and sender-nickname
                 (not (equal sender-name sender-nickname))
                 sender-nickname)))
      `((id . ,(or server-id history-anchor))
        (server-id . ,server-id)
        (session-key . ,session-key)
        (time . ,(alist-get 'sent_at message))
        (message-seq . ,(alist-get 'sequence message))
        (native-client-sequence . ,(alist-get 'client_sequence message))
        (native-random . ,(alist-get 'random message))
        (gateway-account-id . ,owner)
        (sender-id . ,sender-id)
        (sender-native-id . ,(alist-get 'uid sender))
        (sender-name . ,sender-name)
        (sender-secondary-name . ,sender-secondary-name)
        (sender-card . ,sender-member-name)
        (sender-nickname . ,sender-nickname)
        (sender-remark . ,sender-remark)
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

(defun qq-message--normalize-message (data &optional history-p)
  "Normalize native message event DATA for projection.

Unlike `qq-message-normalize-snapshot', this wrapper attaches local
ordering and pending-send correlation owned by the selected projection.
HISTORY-P allows the explicitly sequence-only group-history shape."
  (let* ((owner (qq-message--event-owner data))
         (message (alist-get 'message data))
         (account (qq-account-get owner))
         (normalized
          (qq-message-normalize-snapshot
           message owner account nil history-p)))
    (setf (alist-get 'order normalized nil nil #'eq)
          (qq-state--next-message-order)
          (alist-get 'raw-event normalized nil nil #'eq)
          (copy-tree data))
    (when-let* ((peer-uid (alist-get 'peer-uid normalized))
                (peer-uin (alist-get 'peer-uin normalized)))
      (qq-message--validate-peer-identity owner peer-uid peer-uin))
    (qq-message--attach-pending-local-id
     normalized owner message (alist-get 'session-key normalized))))

(defun qq-message--finalize-message-context (owner message normalized)
  "Commit correlation context for OWNER's merged MESSAGE and NORMALIZED row."
  (when-let* ((peer-uid (alist-get 'peer-uid normalized))
              (peer-uin (alist-get 'peer-uin normalized)))
    (qq-message--remember-peer-identity owner peer-uid peer-uin))
  (when-let* ((match (qq-message--find-pending-send owner message
                                                    (alist-get 'session-key normalized)))
              (key (car match))
              (pending (cdr match))
              ((equal (alist-get 'local-id normalized)
                      (plist-get pending :local-id)))
              ;; A sequence-only group-history row correlates the optimistic
              ;; send but cannot complete its identity promotion.  Keep the
              ;; receipt until an observation carrying the real snowflake
              ;; arrives.
              ((alist-get 'server-id normalized)))
    (remhash key qq-message--pending-sends)))

(defun qq-message--merge-normalized (normalized &optional source)
  "Merge native NORMALIZED message and publish a state event from SOURCE."
  (let ((session-key (alist-get 'session-key normalized)))
    (when-let* ((title (alist-get 'peer-name normalized)))
      (unless (string-empty-p title)
        (qq-state-upsert-session session-key `((title . ,title)) nil)))
    (cl-multiple-value-bind (merged mutation previous-anchor)
        (qq-state--merge-normalized-message
         session-key normalized nil source)
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

(defun qq-message--recall-conversation-key (conversation)
  "Return stable native identity key for recall CONVERSATION."
  (pcase (alist-get 'kind conversation)
    ("private" (list "private" (alist-get 'peer_uid conversation)))
    ("group" (list "group" (alist-get 'group_uin conversation)))))

(defun qq-message--message-conversation-key (normalized)
  "Return recall identity key for NORMALIZED message."
  (pcase (alist-get 'message-type normalized)
    ("private"
     (when-let* ((uid (alist-get 'peer-uid normalized)))
       (list "private" uid)))
    ("group" (list "group" (alist-get 'group-id normalized)))))

(defun qq-message--pending-recall-key (owner conversation-key sequence)
  "Return sequence recall key for OWNER, CONVERSATION-KEY, and SEQUENCE."
  (list owner conversation-key sequence))

(defun qq-message--pending-reaction-key (owner group-uin sequence)
  "Return reaction key for OWNER, GROUP-UIN, and exact SEQUENCE."
  (list owner group-uin sequence))

(defun qq-message--pending-essence-key
    (owner group-uin sequence random)
  "Return essence key for OWNER, GROUP-UIN, SEQUENCE, and RANDOM."
  (list owner group-uin sequence random))

(defun qq-message--apply-recall-target (session-key target)
  "Apply closed native recall TARGET in SESSION-KEY."
  (pcase (alist-get 'kind target)
    ("message"
     (qq-state-apply-recall
      session-key (alist-get 'message_id target)))
    ("sequence"
     (qq-state-apply-group-sequence-recall
      session-key (alist-get 'sequence target)))))

(defun qq-message--apply-pending-recall (owner normalized merged)
  "Apply a pending recall for OWNER to MERGED NORMALIZED message when present."
  (when-let* ((conversation-key
               (qq-message--message-conversation-key normalized))
              (sequence (alist-get 'message-seq normalized))
              (key (qq-message--pending-recall-key
                    owner conversation-key sequence))
              (recall (gethash key qq-message--pending-recalls))
              (target (alist-get 'target recall))
              (session-key (alist-get 'session-key merged)))
    (remhash key qq-message--pending-recalls)
    (qq-message--apply-recall-target session-key target)))

(defun qq-message--reaction-notice (reaction message)
  "Return legacy state notice for authoritative REACTION on MESSAGE."
  (let* ((conversation (alist-get 'conversation reaction))
         (group-uin (alist-get 'group_uin conversation))
         (account
          (qq-account-get (alist-get 'gateway-account-id message)))
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

(defun qq-message--apply-reaction (reaction message)
  "Apply authoritative REACTION to projected MESSAGE."
  (qq-state-apply-emoji-like-notice
   (alist-get 'session-key message)
   (qq-message--reaction-notice reaction message)))

(defun qq-message--apply-pending-reactions (owner normalized merged)
  "Apply reaction events awaiting OWNER's MERGED NORMALIZED message."
  (when-let* ((group-uin (alist-get 'group-id normalized))
              (sequence (alist-get 'message-seq normalized))
              (key (qq-message--pending-reaction-key
                    owner group-uin sequence))
              (reactions (gethash key qq-message--pending-reactions)))
    (dolist (reaction reactions)
      (qq-message--apply-reaction reaction merged))
    (remhash key qq-message--pending-reactions)))

(defun qq-message--apply-essence-state
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
    (qq-message--merge-normalized patched (or source 'event))))

(defun qq-message--apply-pending-essence (owner normalized merged)
  "Apply the latest essence event awaiting OWNER's MERGED message."
  (when-let* ((group-uin (alist-get 'group-id normalized))
              (sequence (alist-get 'message-seq normalized))
              (random (alist-get 'native-random normalized))
              (key (qq-message--pending-essence-key
                    owner group-uin sequence random))
              (essence (gethash key qq-message--pending-essences)))
    (remhash key qq-message--pending-essences)
    (qq-message--apply-essence-state
     merged (eq (alist-get 'is_set essence) t) essence 'event)))

(defun qq-message--project-message (data)
  "Project one account-scoped native message event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
         (normalized (qq-message--normalize-message data))
         (frontier (qq-message--plan-live-frontier owner normalized))
         (merged (qq-message--merge-normalized normalized 'event)))
    (qq-message--finalize-message-context
     owner (alist-get 'message data) normalized)
    (qq-message--apply-pending-recall owner normalized merged)
    (qq-message--apply-pending-reactions owner normalized merged)
    (qq-message--apply-pending-essence owner normalized merged)
    (when frontier
      (puthash (qq-message--frontier-key
                owner (alist-get 'session-key normalized))
               frontier
               qq-message--live-frontiers))
    merged))

(defun qq-message--normalize-dataline-message (owner message &optional raw-data)
  "Return normalized DataLine MESSAGE owned by OWNER.

RAW-DATA, when non-nil, is retained only as the event diagnostic envelope."
  (let* ((chat (alist-get 'chat message))
         (peer-uid (alist-get 'peer_uid chat))
         (variant (alist-get 'variant chat))
         (direction (alist-get 'direction message))
         (message-id (alist-get 'message_id message))
         (sent-at (alist-get 'sent_at message))
         (client-sequence (alist-get 'client_sequence message))
         (message-sequence (alist-get 'message_sequence message))
         (random (alist-get 'random message))
         (batch-id (alist-get 'batch_id message))
         (text (alist-get 'text message))
         (message-keys (mapcar #'car message))
         (allowed-message-keys
          '(message_id chat direction sent_at client_sequence
                       message_sequence random batch_id text)))
    (unless (and
             (cl-every (lambda (key) (memq key allowed-message-keys))
                       message-keys)
             (= (length message-keys)
                (length (delete-dups (copy-sequence message-keys))))
             (cl-every (lambda (key) (assq key message))
                       '(message_id chat direction sent_at batch_id text))
             (qq-account--exact-object-keys-p chat '(peer_uid variant))
             (member variant '("desktop" "mobile"))
             (member peer-uid '("u_Wcc5rknRRqRO8y5gxMD6sA"
                                "u_l7jpPIZxQo0mzJwoEt-SKw"))
             (member direction '("sent" "received"))
             (qq-message--message-id-p message-id)
             (qq-account--uint32-p sent-at)
             (> sent-at 0)
             (or (null client-sequence)
                 (qq-account--uint64-decimal-p client-sequence t))
             (or (null message-sequence)
                 (qq-account--uint64-decimal-p message-sequence t))
             (or (null random) (qq-account--uint32-p random))
             (qq-account--uint64-decimal-p batch-id)
             (qq-account--non-empty-string-p text)
             (<= (string-bytes text)
                 qq-message-max-dataline-text-bytes))
      (error "qq: Gateway returned an invalid DataLine text message"))
    (let* ((session-key
            (qq-state-session-key 'dataline peer-uid variant))
           (outgoing (equal direction "sent"))
           (wire-message
            `((client_sequence . ,client-sequence)
              (sequence . ,message-sequence)
              (random . ,random)))
           (normalized
            `((id . ,message-id)
              (server-id . ,message-id)
              (session-key . ,session-key)
              (time . ,sent-at)
              (message-seq
               . ,(and message-sequence
                       (not (equal message-sequence "0"))
                       message-sequence))
              (native-client-sequence . ,client-sequence)
              (native-random . ,random)
              (gateway-account-id . ,owner)
              (sender-id . nil)
              (sender-native-id . nil)
              (sender-name . ,(if outgoing "Me" "My device"))
              (self-p . ,outgoing)
              (status . ,(if outgoing 'sent 'received))
              (segments . (((type . "text")
                            (data . ((text . ,text))))))
              (raw-message . ,text)
              (preview . ,text)
              (message-type . "dataline")
              (chat-type . ,(if (equal variant "mobile") "134" "8"))
              (peer-uid . ,peer-uid)
              (peer-uin . nil)
              (peer-name . ,(if (equal peer-uid
                                       "u_l7jpPIZxQo0mzJwoEt-SKw")
                                "My pad"
                              "My phone"))
              (group-id . nil)
              (user-id . nil)
              (target-id . ,peer-uid)
              (dataline-batch-id . ,batch-id)
              (order . ,(qq-state--next-message-order))
              ,@(when raw-data
                  `((raw-event . ,(copy-tree raw-data)))))))
      (setq normalized
            (qq-message--attach-pending-local-id
             normalized owner wire-message session-key))
      (cons normalized wire-message))))

(defun qq-message--project-dataline-message (data)
  "Project one account-scoped DataLine text event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
         (message (alist-get 'message data)))
    (unless (qq-account--exact-object-keys-p data '(account_id message))
      (error "qq: Gateway returned an invalid DataLine event envelope"))
    (pcase-let* ((`(,normalized . ,wire-message)
                  (qq-message--normalize-dataline-message
                   owner message data))
                 (merged
                  (qq-message--merge-normalized normalized 'event)))
      (qq-message--finalize-message-context
       owner wire-message normalized)
      merged)))

(defun qq-message--plan-live-frontier (owner normalized)
  "Return OWNER's new live frontier for NORMALIZED, or nil if unchanged.

An equal sequence carrying a different message id is a protocol contradiction.
The caller commits the returned value only after the timeline projection has
completed, so a failed projection cannot advance this side index."
  (let* ((session-key (alist-get 'session-key normalized))
         (message-id (alist-get 'server-id normalized))
         (sequence (alist-get 'message-seq normalized))
         (current
          (gethash (qq-message--frontier-key owner session-key)
                   qq-message--live-frontiers))
         (current-sequence (alist-get 'sequence current)))
    (cond
     ((null current)
      `((message_id . ,message-id) (sequence . ,sequence)))
     ((equal current-sequence sequence)
      (unless (equal (alist-get 'message_id current) message-id)
        (error "qq: Gateway live sequence maps to different message ids"))
      nil)
     ((qq-account--decimal-less-p current-sequence sequence)
      `((message_id . ,message-id) (sequence . ,sequence)))
     (t nil))))

(defun qq-message-live-frontier (session-key)
  "Return the current UI account's live frontier for SESSION-KEY, or nil.

The result contains exact string `message_id' and `sequence' fields.  History
responses never advance this observation; only `message.received' events do."
  (when-let* ((owner (qq-runtime-current-account-id)))
    (qq-server-value-copy
     (gethash (qq-message--frontier-key owner session-key)
              qq-message--live-frontiers))))

(defun qq-message--private-session-by-uid (owner peer-uid)
  "Return OWNER's current private session key for exact PEER-UID, or nil."
  (or (when-let* ((uin
                   (gethash (qq-message--peer-key owner peer-uid)
                            qq-message--peer-uin-by-uid)))
        (qq-state-session-key 'private uin))
      (when-let* ((session
                   (seq-find
                    (lambda (candidate)
                      (and (eq (alist-get 'type candidate) 'private)
                           (equal (alist-get 'peer-uid candidate) peer-uid)))
                    (qq-state-sessions))))
        (alist-get 'key session))))

(defun qq-message--recall-session-key (owner conversation)
  "Return OWNER's projected session key for recall CONVERSATION, or nil."
  (pcase (alist-get 'kind conversation)
    ("group" (qq-state-session-key
              'group (alist-get 'group_uin conversation)))
    ("private" (qq-message--private-session-by-uid
                owner (alist-get 'peer_uid conversation)))))

(defun qq-message--message-by-sequence (session-key sequence)
  "Return cached message in SESSION-KEY matching exact SEQUENCE."
  (seq-find (lambda (message)
              (equal (alist-get 'message-seq message) sequence))
            (qq-state-session-messages session-key)))

(defun qq-message--message-by-native-target
    (session-key sequence random)
  "Return cached message in SESSION-KEY matching SEQUENCE and RANDOM."
  (seq-find
   (lambda (message)
     (and (equal (alist-get 'message-seq message) sequence)
          (equal (alist-get 'native-random message) random)))
   (qq-state-session-messages session-key)))

(defun qq-message--project-recall (data)
  "Project selected-account native recall event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
         (recall (alist-get 'recall data))
         (conversation (alist-get 'conversation recall))
         (target (alist-get 'target recall))
         (sequence (alist-get 'sequence target))
         (session-key
          (qq-message--recall-session-key owner conversation))
         (message (and session-key
                       (qq-message--message-by-sequence
                        session-key sequence))))
    (if (and session-key
             (or (equal (alist-get 'kind target) "message")
                 message))
        (qq-message--apply-recall-target session-key target)
      (puthash
       (qq-message--pending-recall-key
        owner
        (qq-message--recall-conversation-key conversation)
        sequence)
       (copy-tree recall)
       qq-message--pending-recalls))))

(defun qq-message--poke-raw-info (poke)
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

(defun qq-message--project-poke (data)
  "Project selected-account authoritative group poke event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
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
            (raw_info . ,(qq-message--poke-raw-info poke)))))
    (qq-state-apply-poke-notice notice)))

(defun qq-message--project-reaction (data)
  "Project selected-account authoritative group reaction event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
         (reaction (alist-get 'reaction data))
         (conversation (alist-get 'conversation reaction))
         (group-uin (alist-get 'group_uin conversation))
         (sequence (alist-get 'sequence reaction))
         (session-key (qq-state-session-key 'group group-uin))
         (message (qq-message--message-by-sequence
                   session-key sequence)))
    (if message
        (qq-message--apply-reaction reaction message)
      (let* ((key (qq-message--pending-reaction-key
                   owner group-uin sequence))
             (pending (gethash key qq-message--pending-reactions)))
        (puthash key (append pending (list (copy-tree reaction)))
                 qq-message--pending-reactions)))))

(defun qq-message--project-essence (data)
  "Project selected-account authoritative group essence event DATA."
  (let* ((owner (qq-message--event-owner data))
         (_owner (qq-message--sync-account owner))
         (essence (alist-get 'essence data))
         (conversation (alist-get 'conversation essence))
         (group-uin (alist-get 'group_uin conversation))
         (sequence (alist-get 'sequence essence))
         (random (alist-get 'random essence))
         (session-key (qq-state-session-key 'group group-uin))
         (key (qq-message--pending-essence-key
               owner group-uin sequence random))
         (message (qq-message--message-by-native-target
                   session-key sequence random)))
    (if message
        (qq-message--apply-essence-state
         message (eq (alist-get 'is_set essence) t) essence 'event)
      (puthash
       key
       (copy-tree essence)
       qq-message--pending-essences))))

(defun qq-message--projection-error (event data error-data)
  "Publish projection ERROR-DATA for Gateway EVENT and domain DATA."
  (let ((reason (error-message-string error-data)))
    (qq-account--run-hook
     'qq-message-projection-error-hook event (copy-tree data) reason)
    (message "qq: Gateway %s projection skipped: %s" event reason)))

(defun qq-message--handle-event (event data)
  "Observe domainized message EVENT DATA and project its stable owner."
  (qq-account--run-hook
   'qq-message-event-hook event (copy-tree data))
  (when-let* ((owner (qq-message--event-owner data)))
    (condition-case projection-error
        (qq-runtime-with-account owner
          (pcase event
            ("message.received"
             (qq-message--project-message data))
            ("dataline.message_received"
             (qq-message--project-dataline-message data))
            ("message.recalled"
             (qq-message--project-recall data))
            ("message.poked"
             (qq-message--project-poke data))
            ("message.reaction_changed"
             (qq-message--project-reaction data))
            ("message.essence_changed"
             (qq-message--project-essence data))))
      (error
       (qq-message--projection-error
        event data projection-error)))))

(defun qq-message--handle-account-change (_reason account-id)
  "Synchronize account partitions after registry REASON/ACCOUNT-ID."
  (if account-id
      (if (qq-account-get account-id)
          (qq-runtime-with-account account-id
            (qq-message--sync-account account-id))
        (qq-runtime-stop-account account-id t))
    (progn
      (dolist (known-account-id (qq-state-partition-account-ids))
        (unless (qq-account-get known-account-id)
          (qq-runtime-stop-account known-account-id t)))
      (qq-message-sync-accounts))))

(defun qq-message--handle-transport-state (_state)
  "Refresh every managed account after a transport state transition."
  (dolist (account (qq-account-list))
    (let ((owner (alist-get 'account_id account)))
      (qq-runtime-with-account owner
        (qq-message--sync-connection-status account)))))

(defun qq-message--conversation-params (session-key)
  "Return native service conversation params for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target (alist-get 'target-id identity)))
    (pcase kind
      ('private `((kind . "private") (peer_uin . ,target)))
      ('group `((kind . "group") (group_uin . ,target)))
      (_ (user-error "qq: Native Gateway only sends private or group messages")))))

(defun qq-message--decimal-add-small (value addend)
  "Return canonical decimal VALUE plus non-negative small integer ADDEND.

VALUE remains a string throughout; only individual decimal digits and ADDEND
are represented as Emacs integers."
  (unless (and (qq-account--canonical-decimal-p value t)
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

(defun qq-message--validate-sequence (value context)
  "Return exact sequence VALUE after validation for CONTEXT."
  (unless (qq-account--uint64-decimal-p value t)
    (user-error "qq: %s must be a canonical uint64 decimal string" context))
  value)

(defun qq-message--validate-history-range
    (start-sequence end-sequence)
  "Validate inclusive START-SEQUENCE and END-SEQUENCE without coercion."
  (qq-message--validate-sequence start-sequence "History start sequence")
  (qq-message--validate-sequence end-sequence "History end sequence")
  (when (qq-account--decimal-less-p end-sequence start-sequence)
    (user-error "qq: History start sequence must not exceed end sequence"))
  (when (qq-account--decimal-less-p
         (qq-message--decimal-add-small start-sequence 99)
         end-sequence)
    (user-error "qq: Native Gateway history range is limited to 100 messages"))
  (cons start-sequence end-sequence))

(defun qq-message--validate-history-count (count)
  "Return history page COUNT after native range validation."
  (unless (and (integerp count) (<= 1 count 100))
    (user-error "qq: Native Gateway history count must be between 1 and 100"))
  count)

(defun qq-message--history-message-data (owner message)
  "Wrap one history MESSAGE with stable account OWNER context."
  `((account_id . ,owner)
    (message . ,(qq-server-value-copy message))))

(defun qq-message--normalize-history
    (result owner session-key)
  "Preflight RESULT messages for OWNER and exact SESSION-KEY.

Return a list of `(NATIVE-MESSAGE . NORMALIZED-MESSAGE)' pairs."
  (let ((initial-order qq-state--message-order-counter)
        (pending-local-ids (make-hash-table :test #'equal))
        rows)
    (condition-case error-data
        (progn
          (dolist (message (alist-get 'messages result))
            (let* ((data (qq-message--history-message-data
                          owner message))
                   (normalized (qq-message--normalize-message data t))
                   (local-id (alist-get 'local-id normalized)))
              (unless (equal (alist-get 'session-key normalized) session-key)
                (error "qq: Gateway history message contradicts requested conversation"))
              (when-let* ((server-id (alist-get 'server-id normalized)))
                (qq-state-validate-message-session session-key server-id))
              (when local-id
                (when (gethash local-id pending-local-ids)
                  (error "qq: Gateway history reuses one pending send receipt"))
                (puthash local-id t pending-local-ids))
              (push (cons message normalized) rows)))
          (nreverse rows))
      (error
       (setq qq-state--message-order-counter initial-order)
       (signal (car error-data) (cdr error-data))))))

(defun qq-message--merge-history
    (session-key result owner &optional properties)
  "Merge native history RESULT into SESSION-KEY for OWNER.

Optional PROPERTIES are prefixed to the returned and emitted metadata.  This
lets private roaming history retain its time/random continuation cursor
without pretending that it covered a sequence range."
  (let ((rows (qq-message--normalize-history
               result owner session-key))
        (known-anchors (make-hash-table :test #'equal))
        (added 0)
        batch-ids)
    (dolist (message (qq-state-session-messages session-key))
      (when-let* ((anchor (qq-state-message-anchor message)))
        (puthash anchor t known-anchors)))
    (dolist (row rows)
      (let* ((native (car row))
             (normalized (cdr row))
             (message-anchor (qq-state-message-anchor normalized)))
        (when-let* ((title (alist-get 'peer-name normalized)))
          (unless (string-empty-p title)
            (qq-state-upsert-session session-key `((title . ,title)) nil)))
        (cl-multiple-value-bind (merged _mutation _previous-anchor)
            (qq-state--merge-normalized-message
             session-key normalized nil 'history)
          (qq-message--finalize-message-context
           owner native normalized)
          (qq-message--apply-pending-recall owner normalized merged)
          (qq-message--apply-pending-reactions owner normalized merged)
          (qq-message--apply-pending-essence owner normalized merged)
          (setq message-anchor (qq-state-message-anchor merged)))
        (unless (gethash message-anchor known-anchors)
          (cl-incf added)
          (puthash message-anchor t known-anchors))
        (push message-anchor batch-ids)))
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

(cl-defun qq-message--call
    (method owner params &key projector callback errback stale-message)
  "Call message METHOD for stable account OWNER through the typed RPC boundary.

PARAMS deliberately exclude account ownership: this helper adds only the
stable `account_id' slot to the public request.  Projection and leaf callbacks
run in OWNER's state partition.  Freshness follows the stable managed slot and
Gateway instance, never UI selection or Native Session identity."
  (when (assq 'account_id params)
    (error "qq: Message RPC params must not duplicate account ownership"))
  (let ((instance-id (qq-server-gateway-instance-id)))
    (qq-rpc-call
     method (append `((account_id . ,owner)) params)
     :current-p
     (lambda ()
       (and (qq-account-get owner)
            (equal instance-id
                   (qq-server-gateway-instance-id))))
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

(defun qq-message--request-native-history-range
    (session-key start-sequence end-sequence &optional callback errback)
  "Fetch an inclusive native history range for SESSION-KEY.

START-SEQUENCE and END-SEQUENCE are exact canonical decimal strings and the
inclusive range may contain at most 100 sequence values.  CALLBACK receives a
merge metadata plist; ERRBACK receives a Gateway error body and reason."
  (qq-message--validate-history-range start-sequence end-sequence)
  (let* ((conversation (qq-message--conversation-params session-key))
         (owner (qq-message--current-owner)))
    (qq-message--sync-account owner)
    (qq-message--call
     "message.get_history" owner
     `((conversation . ,conversation)
       (start_sequence . ,start-sequence)
       (end_sequence . ,end-sequence))
     :projector
     (lambda (result)
       (qq-message--merge-history session-key result owner))
     :callback callback
     :errback errback)))

(defun qq-message--private-history-cursor (cursor context)
  "Return a closed copy of private history CURSOR for CONTEXT."
  (unless
      (and (qq-account--exact-object-keys-p cursor '(timestamp random))
           (seq-every-p
            (lambda (field)
              (let ((value (alist-get field cursor)))
                (and (integerp value)
                     (<= 0 value 4294967295))))
            '(timestamp random)))
    (error "qq: %s must contain uint32 timestamp and random fields" context))
  `((timestamp . ,(alist-get 'timestamp cursor))
    (random . ,(alist-get 'random cursor))))

(defconst qq-message-history-port-version 1
  "Gateway's conversation-neutral history façade version.")

(defun qq-message--history-conversation-params (session-key)
  "Return the closed unified-history locator for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity)))
    (pcase kind
      ((or 'private 'group)
       (qq-message--conversation-params session-key))
      ('dataline
       `((kind . "dataline")
         (peer_uid . ,(alist-get 'peer-uid identity))
         (variant . ,(alist-get 'variant identity))))
      (_
       (user-error
        "qq: Unified history supports private, group, and DataLine chats")))))

(defun qq-message--history-conversation-equal-p (left right)
  "Return non-nil when closed history locators LEFT and RIGHT are equal.

JSON object member order is not semantic.  Compare the closed discriminator
and identity fields instead of raw alist order."
  (let ((left-kind (alist-get 'kind left))
        (right-kind (alist-get 'kind right)))
    (and (equal left-kind right-kind)
         (pcase left-kind
           ("private"
            (and (qq-account--exact-object-keys-p left '(kind peer_uin))
                 (qq-account--exact-object-keys-p right '(kind peer_uin))
                 (equal (alist-get 'peer_uin left)
                        (alist-get 'peer_uin right))))
           ("group"
            (and (qq-account--exact-object-keys-p left '(kind group_uin))
                 (qq-account--exact-object-keys-p right '(kind group_uin))
                 (equal (alist-get 'group_uin left)
                        (alist-get 'group_uin right))))
           ("dataline"
            (and (qq-account--exact-object-keys-p
                  left '(kind peer_uid variant))
                 (qq-account--exact-object-keys-p
                  right '(kind peer_uid variant))
                 (equal (alist-get 'peer_uid left)
                        (alist-get 'peer_uid right))
                 (equal (alist-get 'variant left)
                        (alist-get 'variant right))))
           (_ nil)))))

(defun qq-message--history-position-equal-p (left right)
  "Return non-nil when closed cursor positions LEFT and RIGHT are equal."
  (let ((left-kind (alist-get 'kind left))
        (right-kind (alist-get 'kind right)))
    (and (equal left-kind right-kind)
         (pcase left-kind
           ("native_sequence"
            (and (qq-message--exact-history-object-keys-p
                  left '(kind sequence) '(maximum_sequence))
                 (qq-message--exact-history-object-keys-p
                  right '(kind sequence) '(maximum_sequence))
                 (equal (alist-get 'sequence left)
                        (alist-get 'sequence right))
                 (equal (alist-get 'maximum_sequence left)
                        (alist-get 'maximum_sequence right))))
           ("group_window"
            (and (qq-account--exact-object-keys-p left '(kind sequence))
                 (qq-account--exact-object-keys-p right '(kind sequence))
                 (equal (alist-get 'sequence left)
                        (alist-get 'sequence right))))
           ("private_roam"
            (let ((left-cursor (alist-get 'cursor left))
                  (right-cursor (alist-get 'cursor right)))
              (and (qq-account--exact-object-keys-p left '(kind cursor))
                   (qq-account--exact-object-keys-p right '(kind cursor))
                   (qq-account--exact-object-keys-p
                    left-cursor '(timestamp random))
                   (qq-account--exact-object-keys-p
                    right-cursor '(timestamp random))
                   (equal (alist-get 'timestamp left-cursor)
                          (alist-get 'timestamp right-cursor))
                   (equal (alist-get 'random left-cursor)
                          (alist-get 'random right-cursor)))))
           ("dataline"
            (and (qq-account--exact-object-keys-p left '(kind message_id))
                 (qq-account--exact-object-keys-p right '(kind message_id))
                 (equal (alist-get 'message_id left)
                        (alist-get 'message_id right))))
           (_ nil)))))

(defun qq-message--history-cursor-equal-p (left right)
  "Return non-nil when scoped history cursors LEFT and RIGHT are equal."
  (and (qq-account--exact-object-keys-p
        left '(version account_id conversation position))
       (qq-account--exact-object-keys-p
        right '(version account_id conversation position))
       (eql (alist-get 'version left) (alist-get 'version right))
       (equal (alist-get 'account_id left) (alist-get 'account_id right))
       (qq-message--history-conversation-equal-p
        (alist-get 'conversation left) (alist-get 'conversation right))
       (qq-message--history-position-equal-p
        (alist-get 'position left) (alist-get 'position right))))

(defun qq-message--normalize-dataline-history
    (messages owner session-key)
  "Preflight DataLine MESSAGES for OWNER and SESSION-KEY."
  (let ((initial-order qq-state--message-order-counter)
        rows)
    (condition-case error-data
        (progn
          (dolist (message messages)
            (pcase-let ((`(,normalized . ,wire-message)
                         (qq-message--normalize-dataline-message
                          owner message)))
              (unless (equal (alist-get 'session-key normalized)
                             session-key)
                (error
                 "qq: Gateway DataLine history contradicts requested conversation"))
              (qq-state-validate-message-session
               session-key (alist-get 'server-id normalized))
              (push (cons normalized wire-message) rows)))
          (nreverse rows))
      (error
       (setq qq-state--message-order-counter initial-order)
       (signal (car error-data) (cdr error-data))))))

(defun qq-message--merge-dataline-history
    (session-key messages owner properties)
  "Merge DataLine MESSAGES into SESSION-KEY and return PROPERTIES metadata."
  (let ((rows (qq-message--normalize-dataline-history
               messages owner session-key))
        (known-anchors (make-hash-table :test #'equal))
        (added 0)
        batch-ids)
    (dolist (message (qq-state-session-messages session-key))
      (when-let* ((anchor (qq-state-message-anchor message)))
        (puthash anchor t known-anchors)))
    (dolist (row rows)
      (let* ((normalized (car row))
             (wire-message (cdr row))
             (anchor (qq-state-message-anchor normalized)))
        (when-let* ((title (alist-get 'peer-name normalized)))
          (qq-state-upsert-session session-key `((title . ,title)) nil))
        (cl-multiple-value-bind (merged _mutation _previous-anchor)
            (qq-state--merge-normalized-message
             session-key normalized nil 'history)
          (qq-message--finalize-message-context
           owner wire-message normalized)
          (setq anchor (qq-state-message-anchor merged)))
        (unless (gethash anchor known-anchors)
          (cl-incf added)
          (puthash anchor t known-anchors))
        (push anchor batch-ids)))
    (setq batch-ids (delete-dups (nreverse batch-ids)))
    (let ((meta
           (append
            properties
            (list :session-key session-key
                  :account-id owner
                  :message-count (length rows)
                  :added-count added
                  :oldest-message-id
                  (qq-state-session-oldest-message-id session-key)
                  :batch-message-ids batch-ids
                  :batch-oldest-message-id (car batch-ids)
                  :batch-newest-message-id (car (last batch-ids))))))
      (apply #'qq-state--emit 'history
             :mutation 'history :source 'response meta)
      meta)))

(defun qq-message--exact-history-object-keys-p
    (object required &optional optional)
  "Return non-nil when history OBJECT has only REQUIRED/OPTIONAL keys."
  (when (and (listp object)
             (cl-every (lambda (entry)
                         (and (consp entry) (symbolp (car entry))))
                       object))
    (let ((keys (mapcar #'car object))
          (allowed (append required optional)))
      (and (= (length keys)
              (length (delete-dups (copy-sequence keys))))
           (cl-every (lambda (key) (assq key object)) required)
           (cl-every (lambda (key) (memq key allowed)) keys)))))

(defun qq-message--history-cursor
    (cursor owner conversation direction context)
  "Validate and copy opaque history CURSOR for CONTEXT.

OWNER and CONVERSATION bind the capability to one managed account locator;
DIRECTION closes which operation may consume its private position."
  (unless
      (and
       (qq-account--exact-object-keys-p
        cursor '(version account_id conversation position))
       (= (alist-get 'version cursor) qq-message-history-port-version)
       (equal (alist-get 'account_id cursor) owner)
       (qq-message--history-conversation-equal-p
        (alist-get 'conversation cursor) conversation)
       (listp (alist-get 'position cursor)))
    (error "qq: %s is not scoped to this account conversation" context))
  (let* ((position (alist-get 'position cursor))
         (kind (alist-get 'kind position))
         (conversation-kind (alist-get 'kind conversation)))
    (pcase kind
      ("native_sequence"
       (unless
           (and
            (member conversation-kind '("private" "group"))
            (qq-message--exact-history-object-keys-p
             position '(kind sequence) '(maximum_sequence))
            (qq-message--validate-sequence
             (alist-get 'sequence position) context)
            (let ((maximum (alist-get 'maximum_sequence position)))
              (and
               (or (null maximum)
                   (progn
                     (qq-message--validate-sequence maximum context)
                     (not (qq-account--decimal-less-p
                           maximum (alist-get 'sequence position)))))
               (pcase direction
                 ('older
                  (not (equal (alist-get 'sequence position) "0")))
                 ('newer
                  (and (equal conversation-kind "private")
                       maximum
                       (qq-account--decimal-less-p
                        (alist-get 'sequence position) maximum)))))))
         (error "qq: %s has an invalid native sequence position" context)))
      ("group_window"
       (unless
           (and
            (equal conversation-kind "group")
            (eq direction 'newer)
            (qq-account--exact-object-keys-p position '(kind sequence))
            (qq-message--validate-sequence
             (alist-get 'sequence position) context))
         (error "qq: %s has an invalid group continuation" context)))
      ("private_roam"
       (unless
           (and
            (equal conversation-kind "private")
            (eq direction 'older)
            (qq-account--exact-object-keys-p position '(kind cursor)))
         (error "qq: %s has an invalid private continuation" context))
       (qq-message--private-history-cursor
        (alist-get 'cursor position) context))
      ("dataline"
       (unless
           (and
            (equal conversation-kind "dataline")
            (qq-account--exact-object-keys-p position '(kind message_id))
            (qq-message--message-id-p (alist-get 'message_id position)))
         (error "qq: %s has an invalid DataLine continuation" context)))
      (_ (error "qq: %s contains an unknown position" context))))
  (copy-tree cursor))

(defun qq-message--history-items (items expected-kind context)
  "Return message payloads from closed history ITEMS of EXPECTED-KIND."
  (unless (listp items)
    (error "qq: %s messages must be a list" context))
  (mapcar
   (lambda (item)
     (unless
         (and (qq-account--exact-object-keys-p item '(kind message))
              (equal (alist-get 'kind item) expected-kind)
              (listp (alist-get 'message item)))
       (error "qq: %s contains a mismatched history message" context))
     (copy-tree (alist-get 'message item)))
   items))

(defun qq-message--history-common-meta
    (result owner session-key conversation context)
  "Validate RESULT's common unified-history envelope for CONTEXT."
  (unless
      (and
       (= (alist-get 'history_version result -1)
          qq-message-history-port-version)
       (equal (alist-get 'account_id result) owner)
       (qq-message--history-conversation-equal-p
        (alist-get 'conversation result) conversation)
       (integerp (alist-get 'unsupported_message_count result))
       (>= (alist-get 'unsupported_message_count result) 0)
       (memq (alist-get 'at_oldest result) '(t :false))
       (memq (alist-get 'at_latest result) '(t :false)))
    (error "qq: Gateway returned an invalid %s envelope" context))
  (let ((older (and (alist-get 'older_cursor result)
                    (qq-message--history-cursor
                     (alist-get 'older_cursor result) owner conversation
                     'older (format "%s older cursor" context))))
        (newer (and (alist-get 'newer_cursor result)
                    (qq-message--history-cursor
                     (alist-get 'newer_cursor result) owner conversation
                     'newer (format "%s newer cursor" context))))
        (at-oldest (eq (alist-get 'at_oldest result) t))
        (at-latest (eq (alist-get 'at_latest result) t)))
    (when (and at-oldest older)
      (error "qq: %s supplies an older cursor at the oldest edge" context))
    (list :history-port-version qq-message-history-port-version
          :history-account-id owner
          :history-session-key session-key
          :history-older-cursor older
          :history-newer-cursor newer
          :history-at-oldest-p at-oldest
          :history-at-latest-p at-latest)))

(defun qq-message--merge-unified-history
    (session-key result owner conversation properties context
                 &optional center-message-id)
  "Merge one validated unified-history RESULT using PROPERTIES metadata."
  (let* ((dataline-p (equal (alist-get 'kind conversation) "dataline"))
         (messages
          (qq-message--history-items
           (alist-get 'messages result)
           (if dataline-p "dataline" "native") context)))
    (if dataline-p
        (let ((message-ids
               (mapcar (lambda (message)
                         (alist-get 'message_id message))
                       messages)))
          (unless
              (= (length message-ids)
                 (length (delete-dups (copy-sequence message-ids))))
            (error "qq: %s repeats a DataLine Message ID" context))
          (when (and center-message-id
                     (not (member center-message-id message-ids)))
            (error "qq: %s omitted its DataLine center" context))
          (qq-message--merge-dataline-history
           session-key messages owner properties))
      (qq-message--merge-history
       session-key
       `((messages . ,messages)
         (unsupported_message_count
          . ,(alist-get 'unsupported_message_count result)))
       owner properties))))

(defun qq-message--merge-unified-history-page
    (session-key result owner conversation cursor direction)
  "Validate and merge one unified page RESULT for its exact request."
  (unless
      (qq-message--exact-history-object-keys-p
       result
       '(history_version account_id conversation direction messages
                         unsupported_message_count requested_cursor
                         older_cursor newer_cursor at_oldest at_latest))
    (error "qq: Gateway returned a non-closed history page"))
  (let ((wire-direction (symbol-name direction))
        (response-cursor (alist-get 'requested_cursor result)))
    (when response-cursor
      (qq-message--history-cursor
       response-cursor owner conversation direction
       "History page requested cursor"))
    (unless (and (equal (alist-get 'direction result) wire-direction)
                 (if cursor
                     (and response-cursor
                          (qq-message--history-cursor-equal-p
                           response-cursor cursor))
                   (null response-cursor)))
      (error "qq: Gateway history page contradicts its request")))
  (let ((properties
         (qq-message--history-common-meta
          result owner session-key conversation "history page")))
    (qq-message--merge-unified-history
     session-key result owner conversation properties "history page")))

(defun qq-message--request-history-page
    (session-key cursor direction &optional callback errback limit)
  "Request one page from the Gateway's unified history façade."
  (unless (memq direction '(older newer))
    (user-error "qq: History direction must be older or newer"))
  (when (and (null cursor) (eq direction 'newer))
    (user-error "qq: Nil history cursor identifies only the latest page"))
  (setq limit (or limit qq-history-fetch-count))
  (qq-message--validate-history-count limit)
  (let* ((conversation
          (qq-message--history-conversation-params session-key))
         (owner (qq-message--current-owner))
         (cursor
          (and cursor
               (qq-message--history-cursor
                cursor owner conversation direction "History cursor"))))
    (qq-message--sync-account owner)
    (qq-message--call
     "message.get_history_page" owner
     `((conversation . ,conversation)
       ,@(when cursor `((cursor . ,cursor)))
       (direction . ,(symbol-name direction))
       (count . ,limit))
     :projector
     (lambda (result)
       (qq-message--merge-unified-history-page
        session-key result owner conversation cursor direction))
     :callback callback
     :errback errback)))

(defun qq-message--merge-unified-history-around
    (session-key result owner conversation center-message-id)
  "Validate and merge one unified around-history RESULT."
  (unless
      (and
       (qq-message--exact-history-object-keys-p
        result
        '(history_version account_id conversation center_message_id messages
                          unsupported_message_count older_cursor newer_cursor
                          at_oldest at_latest))
       (equal (alist-get 'center_message_id result) center-message-id))
    (error "qq: Gateway returned a non-closed history around page"))
  (let ((properties
         (qq-message--history-common-meta
          result owner session-key conversation "history around")))
    (qq-message--merge-unified-history
     session-key result owner conversation properties "history around"
     center-message-id)))

(defun qq-message--request-history-around
    (session-key center-message-id sequence-hint latest-sequence-hint
                 &optional callback errback limit)
  "Request a unified history window around CENTER-MESSAGE-ID.

SEQUENCE-HINT and LATEST-SEQUENCE-HINT are adapter-only native locators;
DataLine resolves its durable Message ID directly."
  (unless (qq-message--message-id-p center-message-id)
    (user-error "qq: History center must be a Message ID"))
  (setq limit (or limit qq-history-fetch-count))
  (qq-message--validate-history-count limit)
  (let* ((conversation
          (qq-message--history-conversation-params session-key))
         (dataline-p (equal (alist-get 'kind conversation) "dataline"))
         (owner (qq-message--current-owner)))
    (if dataline-p
        (when (or sequence-hint latest-sequence-hint)
          (error "qq: DataLine around-history must not carry sequence hints"))
      (progn
        (qq-message--validate-sequence
         sequence-hint "History around sequence hint")
        (when (equal sequence-hint "0")
          (user-error "qq: History around sequence hint must be nonzero"))
        (when latest-sequence-hint
          (qq-message--validate-sequence
           latest-sequence-hint "History latest sequence hint")
          (when (qq-account--decimal-less-p
                 latest-sequence-hint sequence-hint)
            (user-error
             "qq: History latest sequence hint precedes its center")))))
    (qq-message--sync-account owner)
    (qq-message--call
     "message.get_history_around" owner
     `((conversation . ,conversation)
       (center_message_id . ,center-message-id)
       ,@(when sequence-hint `((sequence_hint . ,sequence-hint)))
       ,@(when latest-sequence-hint
           `((latest_sequence_hint . ,latest-sequence-hint)))
       (count . ,limit))
     :projector
     (lambda (result)
       (qq-message--merge-unified-history-around
        session-key result owner conversation center-message-id))
     :callback callback
     :errback errback)))

(defun qq-message--prepare-outbound (session-key segments owner)
  "Translate UI SEGMENTS into one native outbound message.

Return a plist with `:reply-to', either nil or a closed reply target, and
`:segments', the ordered native content elements.  Reply is a request modifier
rather than content.  This adapter checks only facts needed to translate the
local shape or protect its attachment projection; Gateway owns the outbound
domain schema, observed source metadata, and segment limits."
  (let ((group-p (eq (qq-state-session-key-type session-key) 'group))
        reply-to
        native-segments)
    (dolist (segment segments)
      (let* ((type (alist-get 'type segment))
             (data (alist-get 'data segment))
             (native
              (pcase type
                ("text"
                 (unless (qq-account--non-empty-string-p
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
                   (qq-attachment-assert-sendable
                    attachment-id session-key owner)
                   `((kind . "image")
                     (payload . ((attachment_id . ,attachment-id))))))
                ("record"
                 (let ((attachment-id (alist-get 'attachment_id data)))
                   (qq-attachment-assert-sendable
                    attachment-id session-key owner "record")
                   `((kind . "record")
                     (payload . ((attachment_id . ,attachment-id))))))
                ("video"
                 (let ((attachment-id (alist-get 'attachment_id data)))
                   (qq-attachment-assert-sendable
                    attachment-id session-key owner "video")
                   `((kind . "video")
                     (payload . ((attachment_id . ,attachment-id))))))
                ("reply"
                 (let ((target (alist-get 'target data)))
                   (pcase (alist-get 'kind target)
                     ("message"
                      (unless (qq-message--message-id-p
                               (alist-get 'message_id target))
                        (user-error "qq: Reply target has an invalid Message ID")))
                     ("sequence"
                      (unless (and group-p
                                   (qq-protocol-message-sequence-p
                                    (alist-get 'sequence target)))
                        (user-error
                         "qq: Sequence reply target requires a valid group sequence")))
                     (_
                      (user-error "qq: Reply target is malformed")))
                   (when reply-to
                     (user-error "qq: A message has only one reply target"))
                   (setq reply-to target))
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

(defun qq-message--recover-send-from-receipt
    (session-key owner receipt)
  "Recover the authoritative snowflake for a just-sent message.

The `message.send' protocol receipt carries the conversation `server_sequence'
(PbSendMsgResp field 11) but not the NT snowflake `message_id'.  The snowflake
arrives later, unreliably, as a self-echo `message.received' push — a push
that frequently omits ContentHead fields, never reaches the sending session,
or reaches it before its snowflake is assigned.  Relying on that push left
outgoing rows stuck on their optimistic `local-*' id for both group and
private chats.

This deterministic fallback fetches the single conversation sequence from
history (`SsoGetGroupMsg' / `SsoGetC2cMsg', both routed through
`message.get_history') right after the receipt.  The returned message carries
the snowflake as `server-id' and reuses the normal pending-rekey merge, so the
optimistic row is promoted even when the self-echo push never arrives.

The fetch is skipped when the receipt has no nonzero conversation sequence,
when Gateway is not ready, when the account is gone, or when a racing
self-echo push already consumed the pending receipt."
  (when (and (memq (qq-state-session-key-type session-key) '(private group))
             (qq-account-get owner)
             (fboundp 'qq-server-ready-p)
             (qq-server-ready-p))
    (let* ((client-sequence
            (format "%s" (or (alist-get 'client_sequence receipt) "")))
           (server-sequence
            (format "%s" (or (alist-get 'server_sequence receipt) "")))
           (pending-key
            (qq-message--pending-lookup-key owner client-sequence)))
      (when (and pending-key
                 (gethash pending-key qq-message--pending-sends)
                 (qq-account--uint64-decimal-p server-sequence t)
                 (not (equal server-sequence "0")))
        (condition-case err
            (qq-runtime-with-account owner
              (qq-message--request-native-history-range
               session-key server-sequence server-sequence
               nil
               (lambda (_body reason)
                 (message "qq: send history recovery failed: %s" reason))))
          (error
           (message "qq: send history recovery failed: %s"
                    (error-message-string err))))))))

(defun qq-message--send-request
    (session-key segments raw-message method params callback errback)
  "Send prepared SEGMENTS through METHOD with PARAMS for SESSION-KEY."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (pending (qq-state-insert-pending-message
                   session-key segments raw-message))
         (local-id (alist-get 'local-id pending)))
    (cl-labels
        ((fail
           (body reason)
           (qq-state-mark-pending-message-failed session-key local-id reason)
           (qq-account--invoke errback body reason)))
      (condition-case error-data
          (qq-message--call
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
                (qq-message--pending-send-key
                 owner
                 (format "%s" (alist-get 'client_sequence receipt)))
                (list :session-key session-key
                      :local-id local-id
                      :server-sequence
                      (alist-get 'server_sequence receipt)
                      :client-sequence
                      (format "%s" (alist-get 'client_sequence receipt))
                      :random (alist-get 'random receipt))
                qq-message--pending-sends))
             receipt)
           :callback
           (lambda (receipt)
             (let ((value
                    (if callback
                        (funcall callback receipt)
                      receipt)))
               ;; The receipt carries the conversation sequence but not the
               ;; NT snowflake, and the self-echo push that would deliver it
               ;; is unreliable.  Deterministically recover the authoritative
               ;; snowflake from history so the optimistic pending row rekeys.
               (qq-message--recover-send-from-receipt
                session-key owner receipt)
               value))
           :errback #'fail
           :stale-message "QQ account or Gateway connection changed during send")
        (error
         (qq-state-mark-pending-message-failed
          session-key local-id (error-message-string error-data))
         (signal (car error-data) (cdr error-data)))))))

(defun qq-message-send
    (session-key segments &optional raw-message callback errback
                 optimistic-segments)
  "Send SEGMENTS to a native sendable SESSION-KEY.

Supported elements are text, base face (ID 0 through 259), group mention,
reply, and already prepared image/record/video attachments.  One optional
reply is lifted into the request envelope.  It carries either an exact target
Message ID or, for id-less group history, the authoritative group sequence.
Gateway resolves the observed native source metadata.  At least one and at
most 128 non-reply content elements are required.  RAW-MESSAGE is an optional
optimistic rendering override.
OPTIMISTIC-SEGMENTS, when non-nil, are stored in the pending row instead of
protocol-ready SEGMENTS so local media previews never enter the wire request.
The original reply element remains part of this local rendering shape.
The currently negotiated `dataline.send_text' operation accepts one nonempty
desktop text segment of at most 160 UTF-8 bytes.  That method boundary does not
describe stock DataLine file, forwarding, face projection, or multipart
capabilities."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner)))
    (if (eq (qq-state-session-key-type session-key) 'dataline)
        (let* ((identity (qq-state-session-key-identity session-key))
               (peer-uid (alist-get 'peer-uid identity))
               (variant (alist-get 'variant identity))
               (segment (and (= (length segments) 1) (car segments)))
               (data (and segment (alist-get 'data segment)))
               (text (and (equal (alist-get 'type segment) "text")
                          (alist-get 'text data))))
          (unless (and (equal variant "desktop")
                       (member peer-uid '("u_Wcc5rknRRqRO8y5gxMD6sA"
                                          "u_l7jpPIZxQo0mzJwoEt-SKw"))
                       (qq-account--non-empty-string-p text)
                       (<= (string-bytes text) 160))
             (user-error
              "qq: current dataline.send_text requires one 1–160 byte desktop text segment for a pinned phone/pad class"))
          (qq-message--send-request
           session-key (or optimistic-segments segments) raw-message
           "dataline.send_text"
           `((chat . ((peer_uid . ,peer-uid) (variant . ,variant)))
             (text . ,text))
           callback errback))
      (let* ((outbound
              (qq-message--prepare-outbound session-key segments owner))
             (reply-to (plist-get outbound :reply-to))
             (native-segments (plist-get outbound :segments))
             (conversation (qq-message--conversation-params session-key)))
        (qq-message--send-request
         session-key (or optimistic-segments segments) raw-message "message.send"
         `((conversation . ,conversation)
           ,@(when reply-to `((reply_to . ,reply-to)))
           (segments . ,native-segments))
         callback errback)))))

(defun qq-message-send-file
    (session-key resource-id &optional callback errback)
  "Publish staged RESOURCE-ID as a standalone group file.

Group files use QQ's dedicated file feed rather than `message.send'.  The
successful receipt acknowledges publication but does not invent a message ID;
the authoritative file message is projected when QQ returns it through push
or history."
  (unless (eq (qq-state-session-key-type session-key) 'group)
    (user-error "qq: Native private file upload is not implemented yet"))
  (unless (qq-resource-id-p resource-id)
    (user-error "qq: File send requires an opaque staged resource"))
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner)))
    (qq-message--call
     "file.send" owner
     `((conversation . ,(qq-message--conversation-params session-key))
       (resource_id . ,resource-id))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during file send")))

(defun qq-message-send-poke
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
         (owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner)))
    (unless (and (memq kind '(private group))
                 (qq-account--uint64-decimal-p peer-uin))
      (user-error "qq: Native Gateway poke requires a private/group UIN"))
    (unless (qq-account--uint64-decimal-p target-uin)
      (user-error "qq: Native Gateway poke target must be an exact UIN"))
    (qq-message--call
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

(defun qq-message-set-reaction
    (message emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on native group MESSAGE.

SET non-nil adds the reaction.  CALLBACK receives the receipt.  Only the
authoritative `message.reaction_changed' update changes local reaction state."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-account--uint64-decimal-p group-uin)
                 (qq-message--message-id-p message-id))
      (user-error "qq: Native reaction requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Reaction message belongs to another Gateway account"))
    (setq emoji-id (format "%s" emoji-id))
    (qq-message--validate-sequence emoji-id "Reaction emoji ID")
    (qq-message--call
     "message.set_reaction" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (emoji_id . ,emoji-id)
       (set . ,(if set t :false)))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during reaction")))

(defun qq-message-set-essence
    (message set &optional callback errback)
  "Set or remove native group MESSAGE as an essence message.

SET non-nil sets the essence flag.  CALLBACK receives the receipt.  Only the
authoritative `message.essence_changed' update changes local essence state."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (set (and set t)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-account--uint64-decimal-p group-uin)
                 (qq-message--message-id-p message-id))
      (user-error "qq: Native essence requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Essence message belongs to another Gateway account"))
    (qq-message--call
     "message.set_essence" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (set . ,(if set t :false)))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during essence action")))

(defun qq-message-set-todo
    (message operation &optional callback errback)
  "Apply todo OPERATION to native group MESSAGE.

OPERATION is one of `set', `complete', or `cancel'.  CALLBACK receives the
receipt.  No local todo state is invented because native query/event semantics
are not yet part of the Gateway protocol."
  (unless (memq operation '(set complete cancel))
    (user-error "qq: Unknown native todo operation %S" operation))
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (session-key (alist-get 'session-key message))
         (message-id (alist-get 'server-id message))
         (group-uin (and session-key
                         (qq-state-session-key-target-id session-key)))
         (operation-name (symbol-name operation)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-account--uint64-decimal-p group-uin)
                 (qq-message--message-id-p message-id))
      (user-error "qq: Native todo requires an exact group Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Todo message belongs to another Gateway account"))
    (qq-message--call
     "message.set_todo" owner
     `((conversation . ((kind . "group") (group_uin . ,group-uin)))
       (message . ((message_id . ,message-id)))
       (operation . ,operation-name))
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during todo action")))

(defun qq-message--read-request (message owner)
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
                 (qq-message--message-id-p message-id))
      (user-error "qq: Native read report requires an exact Message Reference"))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Read target belongs to another Gateway account"))
    (setq conversation
          (qq-message--conversation-params session-key))
    (list :message-id message-id
          :params
          `((conversation . ,conversation)
            (message . ((message_id . ,message-id)))))))

(defun qq-message-read-capable-p (message)
  "Return non-nil when MESSAGE is an exact reference for the current owner."
  (when-let* ((owner (qq-runtime-current-account-id)))
    (condition-case nil
        (progn
          (qq-message--read-request message owner)
          t)
      (error nil))))

(defun qq-message-mark-read
    (message &optional callback errback)
  "Mark the selected account's conversation read through MESSAGE.

MESSAGE supplies only its stable public conversation locator and exact
message ID.  Gateway resolves the corresponding Native read boundary.
CALLBACK receives the acknowledgement; ERRBACK receives an error
body and reason."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (request (qq-message--read-request message owner)))
    (qq-message--call
     "message.mark_read" owner
     (plist-get request :params)
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during read report")))

(defun qq-message-exact-id (message)
  "Return MESSAGE's observed exact NT message ID, or nil.

This accessor is deliberately narrower than `qq-state-message-anchor'.
Sequence-only group history has a valid local timeline anchor and a native
group locator, but neither is an exact NT message ID."
  (when (listp message)
    (let ((message-id (alist-get 'server-id message)))
      (and (qq-protocol-message-id-p message-id) message-id))))

(defun qq-message-group-sequence-target (session-key message)
  "Return MESSAGE's group-local sequence target in SESSION-KEY, or nil.

The returned tagged object is a QQ protocol locator scoped by SESSION-KEY.
It is not a substitute for an exact NT message ID."
  (when (and (listp message)
             (equal (alist-get 'session-key message) session-key)
             (eq (qq-state-session-key-type session-key) 'group))
    (let ((sequence (alist-get 'message-seq message)))
      (when (qq-protocol-message-sequence-p sequence)
        `((kind . "sequence") (sequence . ,sequence))))))

(defun qq-message-recall-target (session-key message)
  "Return MESSAGE's closed native recall target in SESSION-KEY, or nil.

An exact NT snowflake works in private and group chats.  A group message may
instead use its conversation-local native sequence; private sequence values
alone are not a complete native recall capability."
  (when (and (listp message)
             (equal (alist-get 'session-key message) session-key))
    (if-let* ((message-id (qq-message-exact-id message)))
        `((kind . "message") (message_id . ,message-id))
      (qq-message-group-sequence-target session-key message))))

(defun qq-message-reply-target (session-key message)
  "Return MESSAGE's closed native reply target in SESSION-KEY, or nil.

An exact NT snowflake works in private and group chats.  Group history may
instead use its authoritative conversation-local sequence; Gateway retains
the corresponding sender identity and timestamp inside the account actor."
  (when (and (listp message)
             (equal (alist-get 'session-key message) session-key))
    (if-let* ((message-id (qq-message-exact-id message)))
        `((kind . "message") (message_id . ,message-id))
      (qq-message-group-sequence-target session-key message))))

(defun qq-message-recall
    (session-key message &optional callback errback)
  "Recall native MESSAGE in SESSION-KEY for the selected QQ account.

CALLBACK receives the SSO receipt; ERRBACK receives an error body
and reason.  A successful typed acknowledgement marks the local message
recalled; a later `message.recalled' event is an idempotent reconciliation."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (conversation (qq-message--conversation-params session-key))
         (target (or (qq-message-recall-target session-key message)
                     (user-error
                      "qq: Message has no native recall target"))))
    (unless (equal (alist-get 'gateway-account-id message) owner)
      (user-error "qq: Message is not owned by selected Gateway account"))
    (qq-message--call
     "message.recall" owner
     `((conversation . ,conversation)
       (target . ,target))
     :projector
     (lambda (receipt)
       (qq-message--apply-recall-target session-key target)
       receipt)
     :callback callback
     :errback errback
     :stale-message
     "QQ account or Gateway connection changed during recall")))

(defun qq-message--validate-poke-recall-metadata (message owner)
  "Return MESSAGE's native poke recall metadata for OWNER's account slot."
  (let* ((raw-event (alist-get 'raw-event message))
         (metadata (and (listp raw-event)
                        (alist-get 'gateway_recall raw-event))))
    (unless (and (qq-account--exact-object-keys-p
                  metadata
                  '(account_id conversation message_id sequence sent_at
                    tips_sequence))
                 (equal (alist-get 'account_id metadata) owner)
                 (equal (alist-get 'message_id metadata)
                        (alist-get 'server-id message))
                 (qq-message--message-id-p
                  (alist-get 'message_id metadata))
                 (qq-account--uint64-decimal-p
                  (alist-get 'sequence metadata) t)
                 (integerp (alist-get 'sent_at metadata))
                 (> (alist-get 'sent_at metadata) 0)
                 (qq-account--uint64-decimal-p
                  (alist-get 'tips_sequence metadata) t))
      (user-error "qq: Poke lacks exact native service recall metadata"))
    (let ((conversation (alist-get 'conversation metadata)))
      (unless (and (qq-account--exact-object-keys-p
                    conversation '(kind group_uin))
                   (equal (alist-get 'kind conversation) "group")
                   (qq-account--uint64-decimal-p
                    (alist-get 'group_uin conversation))
                   (equal (qq-state-session-key
                           'group (alist-get 'group_uin conversation))
                          (alist-get 'session-key message)))
        (user-error "qq: Poke recall metadata contradicts its group session")))
    (copy-tree metadata)))

(defun qq-message-recall-poke
    (message &optional callback errback)
  "Recall authoritative group poke MESSAGE through the selected Gateway.

CALLBACK receives the receipt; ERRBACK receives failure details."
  (let* ((owner (qq-message--current-owner))
         (_owner (qq-message--sync-account owner))
         (reference (qq-state-poke-recall-reference message))
         (metadata
          (qq-message--validate-poke-recall-metadata message owner))
         (message-id (alist-get 'message_id metadata))
         (sequence (alist-get 'sequence metadata)))
    (unless reference
      (user-error "qq: Poke has no native recall capability"))
    (when (qq-protocol-poke-recall-reference-expired-p reference)
      (user-error "qq: 戳一戳已超过 2 分钟撤回期限"))
    (qq-message--call
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

(defconst qq-message-max-recent-limit 500
  "Largest page accepted by `conversation.list_recent'.")

(defun qq-message--recent-assert-identity-message
    (identity message context)
  "Reject a recent IDENTITY that contradicts its latest MESSAGE in CONTEXT."
  (let ((identity-kind (alist-get 'kind identity))
        (conversation (alist-get 'conversation message)))
    (pcase identity-kind
      ("private"
       (unless (equal (alist-get 'kind conversation) "private")
         (error "qq: recent %s private identity contradicts latest message"
                context)))
      ("group"
       (unless (and (equal (alist-get 'kind conversation) "group")
                    (equal (alist-get 'group_uin identity)
                           (alist-get 'group_uin conversation)))
         (error "qq: recent %s group identity contradicts latest message"
                context)))
      ("temporary"
       (unless (equal (alist-get 'kind conversation) "temp")
         (error "qq: recent %s temporary identity contradicts latest message"
                context))
       (dolist (key '(from_tiny_id to_tiny_id))
         (when (and (assq key identity) (assq key conversation)
                    (not (equal (alist-get key identity)
                                (alist-get key conversation))))
           (error "qq: recent %s temporary route contradicts latest message"
                  context))))))
  message)

(defun qq-message--recent-identity-key (identity message)
  "Return a projection duplicate key for IDENTITY and latest MESSAGE."
  (pcase (alist-get 'kind identity)
    ("private"
     (list "private" (alist-get 'peer_uid identity)
           (alist-get 'peer_uin identity)))
    ("group" (list "group" (alist-get 'group_uin identity)))
    ("temporary"
     (list "temporary"
           (alist-get 'peer_uid identity)
           (alist-get 'peer_uin identity)
           (alist-get 'from_tiny_id identity)
           (alist-get 'to_tiny_id identity)
           (alist-get 'sub_type message)))))

(defun qq-message--recent-check-row (row index seen-identities)
  "Check projection relationships for recent ROW at INDEX.

SEEN-IDENTITIES rejects two rows that would own the same local session."
  (let* ((context (format "conversations[%d]" index))
         (identity (alist-get 'conversation row))
         (message (alist-get 'latest_message row))
         (identity-key
          (qq-message--recent-identity-key identity message)))
    (qq-message--recent-assert-identity-message identity message context)
    (when (gethash identity-key seen-identities)
      (error "qq: recent page duplicates conversation identity"))
    (puthash identity-key t seen-identities)
    row))

(defun qq-message--recent-check-page (page)
  "Check client projection relationships in recent-conversation PAGE."
  (let ((seen-identities (make-hash-table :test #'equal)))
    (cl-loop
     for row in (alist-get 'conversations page)
     for index from 0
     do (qq-message--recent-check-row row index seen-identities)))
  page)

(defun qq-message--recent-normalize-limit (limit)
  "Return normalized recent-conversation LIMIT."
  (setq limit (or limit qq-recent-contact-count))
  (unless (and (integerp limit)
               (<= 1 limit qq-message-max-recent-limit))
    (user-error "qq: Recent conversation limit must be between 1 and %d"
                qq-message-max-recent-limit))
  limit)

(cl-defun qq-message-list-recent
    (account-id &key callback errback limit)
  "Request ACCOUNT-ID's recent-conversation domain page.

CALLBACK receives a checked domain page.  ERRBACK receives the standard
service body and reason.  LIMIT defaults to `qq-recent-contact-count'."
  (unless (qq-account--non-empty-string-p account-id)
    (user-error "qq: Recent conversations require an account slot"))
  (setq limit (qq-message--recent-normalize-limit limit))
  (qq-rpc-call
   "conversation.list_recent"
   `((account_id . ,account-id) (limit . ,limit))
   :projector #'qq-message--recent-check-page
   :callback callback
   :errback errback))

(dolist (event '("message.received" "dataline.message_received"
                  "message.recalled" "message.poked"
                 "message.reaction_changed" "message.essence_changed"))
  (qq-rpc-register-event
   event #'qq-message--handle-event))
(add-hook 'qq-account-registry-changed-hook
          #'qq-message--handle-account-change)
(add-hook 'qq-server-state-hook
          #'qq-message--handle-transport-state)

(provide 'qq-message)

;;; qq-message.el ends here
