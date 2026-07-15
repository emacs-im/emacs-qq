;;; qq-state.el --- In-memory store for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Central state for sessions, messages, contacts, and connection metadata.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-protocol)

(defvar qq-state-change-hook nil
  "Hook called with one event plist argument after state mutations.

Event plists always include `:type'.  Message-related emitters should also
include:

- `:session-key' — affected QQ session key
- `:mutation' — one of `create', `update', `delete', `read', `history',
  `session' (coarse kind; see `qq-state--emit')
- `:source' — `local', `event', `response', `notice', when known
- `:message' / `:message-anchor' / `:previous-anchor' — when a single
  message is the subject (anchor prefers NT snowflake `server-id')
- `:message-patch' — a pure ID-scoped patch when the subject is not cached
- `:observation-token' — message-patch observation clock for request windows

Chat views project canonical state after every relevant mutation; anchors and
resource identities let the shared timeline redraw only affected rows.")

(defvar qq-state--connection-status 'disconnected)
(defvar qq-state--last-heartbeat nil)
(defvar qq-state--self-info nil)
(defvar qq-state--status nil)
(defvar qq-state--sessions (make-hash-table :test #'equal))
(defvar qq-state--recent-session-keys nil
  "Canonical keys in the latest authoritative recent-contact snapshot.")
(defvar qq-state--recent-session-key-set (make-hash-table :test #'equal)
  "Set form of `qq-state--recent-session-keys' for constant-time membership.")
(defvar qq-state--messages-by-session (make-hash-table :test #'equal))
(defvar qq-state--message-patch-journal (make-hash-table :test #'equal)
  "Notice state keyed by (SESSION-KEY . MESSAGE-ID).

Recall tombstones remain authoritative for the lifetime of the state store.
Reaction patches are retained only while an older materialization request is
active.  Such a request may replay patches observed after it started, but a
future request must accept its own authoritative reaction snapshot unchanged.")
(defvar qq-state--message-observation-clock 0
  "Monotonic token for ID-scoped message patch observations.")
(defvar qq-state--materialization-request-counter 0
  "Monotonic identity counter for materialization request owners.")
(defvar qq-state--materialization-request-owners
  (make-hash-table :test #'eql)
  "Active materialization request owners keyed by their numeric identity.")
(defvar qq-state--friends-by-id (make-hash-table :test #'equal))
(defvar qq-state--friend-order nil
  "Friend UINs in the authoritative snapshot order.")
(defvar qq-state--friend-categories nil
  "Authoritative ordered friend categories from Linux QQ.")
(defvar qq-state--friend-categories-loaded-p nil
  "Non-nil after an authoritative friend-category snapshot was applied.")
(defvar qq-state--groups-by-id (make-hash-table :test #'equal))
(defvar qq-state--group-order nil
  "Joined group codes in the authoritative snapshot order.")
(defvar qq-state--groups-loaded-p nil
  "Non-nil after an authoritative joined-group snapshot was applied.")
(defvar qq-state--guilds-by-id (make-hash-table :test #'equal))
(defvar qq-state--guild-order nil
  "Guild IDs in the authoritative Linux QQ message-list order.")
(defvar qq-state--guild-channels-by-key (make-hash-table :test #'equal))
(defvar qq-state--guild-channel-order nil
  "Composite Guild channel keys in authoritative Linux QQ order.")
(defvar qq-state--guild-directory-loaded-p nil
  "Non-nil after an authoritative QQ Guild directory snapshot was applied.")
(defvar qq-state--requests nil)
(defvar qq-state--message-session-index (make-hash-table :test #'equal))
(defvar qq-state--local-message-session-index (make-hash-table :test #'equal))
(defvar qq-state--message-order-counter 0)
(defvar qq-state--local-message-counter 0)
(defvar qq-state--session-summary-observation-clock 0
  "Monotonic token for root latest-message summary observations.")
(defvar qq-state--actions (make-hash-table :test #'equal)
  "Peer chat-actions by session-key (telega telega--actions counterpart).

Value is an alist of (SENDER-ID . ACTION), where SENDER-ID is a UIN string
and ACTION is an alist:

- type: symbol, currently only typing (maps from NapCat input_status)
- text: display string (kernel status_text or a local fallback)
- event-type: raw OneBot/kernel event_type number
- expires-at: float-time auto-clear deadline
- timer: Emacs timer that clears this sender's action")

(defun qq-state--emit (type &rest plist)
  "Emit state TYPE event with extra PLIST fields.

Preferred keys (callers should populate when applicable):

`:type' (always)  event class: `message', `history', `session', `reset', …
`:mutation'       coarse change kind for views: `create', `update', `delete',
                  `read', `history', or `session'
`:session-key'    QQ session key string
`:source'         provenance: `local' | `event' | `response' | `notice'
`:message'        normalized message alist (copy)
`:message-anchor' stable timeline key (server-id or local-id)
`:previous-anchor' prior key when rekeying (pending local-id → snowflake)"
  (run-hook-with-args 'qq-state-change-hook (append (list :type type) plist)))

(defun qq-state-message-anchor (message)
  "Return stable timeline anchor for MESSAGE.

Prefer NapCat hard-cut NT snowflake `server-id', then `local-id', then `id'."
  (and message
       (or (alist-get 'server-id message)
           (alist-get 'local-id message)
           (alist-get 'id message))))

(defun qq-state--normalize-id (value)
  "Return VALUE normalized as a string ID, or nil."
  (and value (format "%s" value)))

(defun qq-state--normalize-time (value)
  "Return VALUE normalized as an integer UNIX timestamp."
  (cond
   ((integerp value) value)
   ((floatp value) (truncate value))
   ((stringp value)
    (truncate (string-to-number value)))
   (t 0)))

(defun qq-state--normalize-reaction-count (value)
  "Return reaction count VALUE as a non-negative integer."
  (max 0
       (cond
        ((integerp value) value)
        ((numberp value) (truncate value))
        ((and (stringp value)
              (string-match-p "\\`[0-9]+\\'" value))
         (string-to-number value))
        (t 0))))

(defun qq-state--infer-reaction-emoji-type (emoji-id)
  "Infer QQ reaction type from string EMOJI-ID."
  (if (> (length (or emoji-id "")) 3) "2" "1"))

(defun qq-state--normalize-reactions (raw-reactions)
  "Normalize RAW-REACTIONS from OneBot `emoji_likes_list'."
  (seq-keep
   (lambda (raw)
     (when-let* ((emoji-id (qq-state--normalize-id
                            (or (alist-get 'emoji_id raw)
                                (alist-get 'emojiId raw)))))
       (let ((count (qq-state--normalize-reaction-count
                     (or (alist-get 'likes_cnt raw)
                         (alist-get 'count raw)))))
         (when (> count 0)
           `((emoji-id . ,emoji-id)
             (emoji-type . ,(qq-state--normalize-id
                              (or (alist-get 'emoji_type raw)
                                  (alist-get 'emojiType raw)
                                  (qq-state--infer-reaction-emoji-type emoji-id))))
             (count . ,count)
             (chosen-p . ,(and
                            (qq-protocol-json-true-p
                             (or (alist-get 'is_clicked raw)
                                 (alist-get 'isClicked raw)
                                 (alist-get 'is_chosen raw)
                                 (alist-get 'me raw)))
                            t)))))))
   (or raw-reactions '())))

(defun qq-state-message-reactions (message)
  "Return normalized reactions stored on MESSAGE."
  (or (and (listp message) (alist-get 'reactions message)) '()))

(defun qq-state--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-state--first-present-string (&rest values)
  "Return the first non-empty string in VALUES, or nil."
  (seq-find #'qq-state--present-string values))

(defun qq-state--distinct-present-string (reference &rest values)
  "Return first non-empty string in VALUES distinct from REFERENCE."
  (seq-find (lambda (value)
              (and (qq-state--present-string value)
                   (not (equal value reference))))
            values))

(defun qq-state--sender-display-fields (session-key sender sender-id)
  "Return normalized display fields for message SENDER in SESSION-KEY."
  (let* ((session-type (and session-key (qq-state-session-key-type session-key)))
         (card (qq-state--present-string (alist-get 'card sender)))
         (nickname (qq-state--present-string (alist-get 'nickname sender)))
         (friend (and (eq session-type 'private)
                      sender-id
                      (gethash sender-id qq-state--friends-by-id)))
         (remark (and friend
                      (qq-state--present-string (alist-get 'remark friend))))
         (primary (if (eq session-type 'group)
                      (or card nickname sender-id "unknown")
                    (or remark nickname sender-id "unknown")))
         (secondary (if (eq session-type 'group)
                        (qq-state--distinct-present-string primary nickname)
                      (and remark
                           (qq-state--distinct-present-string primary nickname)))))
    `((sender-name . ,primary)
      (sender-secondary-name . ,secondary)
      (sender-card . ,card)
      (sender-nickname . ,nickname)
      (sender-remark . ,remark))))

(defun qq-state--next-message-order ()
  "Return the next local message ordering number."
  (cl-incf qq-state--message-order-counter))

(defun qq-state--cancel-action-timer (action)
  "Cancel any expire timer stored on ACTION alist."
  (when-let* ((timer (and (listp action) (alist-get 'timer action))))
    (when (timerp timer)
      (cancel-timer timer))))

(defun qq-state--cancel-session-action-timers (actions)
  "Cancel timers for every ACTION in the session ACTIONS alist."
  (dolist (cell actions)
    (when (consp cell)
      (qq-state--cancel-action-timer (cdr cell)))))

(defun qq-state-reset ()
  "Reset all in-memory emacs-qq state."
  (setq qq-state--connection-status 'disconnected)
  (setq qq-state--last-heartbeat nil)
  (setq qq-state--self-info nil)
  (setq qq-state--status nil)
  (setq qq-state--requests nil)
  (setq qq-state--recent-session-keys nil)
  (clrhash qq-state--recent-session-key-set)
  (setq qq-state--friend-order nil)
  (setq qq-state--friend-categories nil)
  (setq qq-state--friend-categories-loaded-p nil)
  (setq qq-state--group-order nil)
  (setq qq-state--groups-loaded-p nil)
  (setq qq-state--guild-order nil)
  (setq qq-state--guild-channel-order nil)
  (setq qq-state--guild-directory-loaded-p nil)
  (setq qq-state--message-order-counter 0)
  (setq qq-state--local-message-counter 0)
  (setq qq-state--session-summary-observation-clock 0)
  (maphash (lambda (_key actions)
             (qq-state--cancel-session-action-timers actions))
           qq-state--actions)
  (clrhash qq-state--actions)
  (clrhash qq-state--sessions)
  (clrhash qq-state--messages-by-session)
  (clrhash qq-state--message-patch-journal)
  (clrhash qq-state--materialization-request-owners)
  (clrhash qq-state--friends-by-id)
  (clrhash qq-state--groups-by-id)
  (clrhash qq-state--guilds-by-id)
  (clrhash qq-state--guild-channels-by-key)
  (clrhash qq-state--message-session-index)
  (clrhash qq-state--local-message-session-index)
  (qq-state--emit 'reset))

(defun qq-state-connection-status ()
  "Return current transport connection status symbol."
  qq-state--connection-status)

(defun qq-state-session-summary-observation-start ()
  "Return a freshness token for a latest-message summary observation.

Asynchronous summary callers capture this before dispatch.  Live events
allocate at receipt, so a response that began earlier cannot replace a later
observation of the same or a newer message.  This clock is deliberately
independent of unread state ownership."
  (cl-incf qq-state--session-summary-observation-clock))

(defun qq-state-set-connection-status (status)
  "Set current transport STATUS symbol."
  (unless (eq qq-state--connection-status status)
    (setq qq-state--connection-status status)
    (qq-state--emit 'connection :status status))
  qq-state--connection-status)

(defun qq-state-last-heartbeat ()
  "Return the last heartbeat timestamp."
  qq-state--last-heartbeat)

(defun qq-state-set-last-heartbeat (&optional timestamp)
  "Store heartbeat TIMESTAMP or current time when nil."
  (setq qq-state--last-heartbeat (or timestamp (float-time)))
  (qq-state--emit 'heartbeat :timestamp qq-state--last-heartbeat)
  qq-state--last-heartbeat)

(defun qq-state-self-info ()
  "Return current self info object."
  (copy-tree qq-state--self-info))

(defun qq-state-set-self-info (info)
  "Store self INFO object."
  (setq qq-state--self-info (copy-tree info))
  (qq-state--emit 'self-info :self-info (qq-state-self-info))
  qq-state--self-info)

(defun qq-state-status ()
  "Return current status object."
  (copy-tree qq-state--status))

(defun qq-state-set-status (status)
  "Store STATUS object."
  (unless (equal qq-state--status status)
    (setq qq-state--status (copy-tree status))
    (qq-state--emit 'status :status (qq-state-status)))
  qq-state--status)

(defun qq-state-self-user-id ()
  "Return current self QQ number as a normalized string, or nil."
  (qq-state--normalize-id (alist-get 'user_id qq-state--self-info)))

(defun qq-state--dataline-chat-type-p (chat-type)
  "Return non-nil when CHAT-TYPE denotes a 移动设备 / DataLine 会话."
  (member (qq-state--normalize-id chat-type) '("8" "134")))

(defun qq-state--dataline-variant-from-chat-type (chat-type)
  "Return the canonical DataLine variant for exact CHAT-TYPE, or nil."
  (pcase (qq-state--normalize-id chat-type)
    ("8" "desktop")
    ("134" "mobile")
    (_ nil)))

(defun qq-state--service-chat-type-p (chat-type)
  "Return non-nil when CHAT-TYPE is a public/service-account peer session."
  (equal (qq-state--normalize-id chat-type) "103"))

(defun qq-state--canonical-decimal-id (value context)
  "Return VALUE as a canonical decimal string for CONTEXT."
  (let ((id (qq-state--normalize-id value)))
    (unless (qq-protocol--decimal-string-p id)
      (error "qq: %s requires a decimal identity, got %S" context value))
    id))

(defun qq-state--canonical-peer-uid (value context)
  "Return opaque peer UID VALUE unchanged after validating CONTEXT."
  (unless (and (stringp value) (not (string-empty-p value)))
    (error "qq: %s requires a non-empty native peer UID, got %S"
           context value))
  value)

(defun qq-state--canonical-dataline-variant (variant)
  "Return canonical DataLine VARIANT string, or signal an error."
  (pcase variant
    ((or 'desktop "desktop") "desktop")
    ((or 'mobile "mobile") "mobile")
    (_ (error "qq: dataline session requires desktop or mobile variant, got %S"
              variant))))

(defun qq-state-guild-channel-session-key (guild-id channel-id)
  "Build a canonical channel session key from GUILD-ID and CHANNEL-ID."
  (unless (qq-protocol--nonzero-decimal-string-p guild-id)
    (error "qq: Guild identity requires a canonical nonzero decimal string"))
  (unless (qq-protocol--nonzero-decimal-string-p channel-id)
    (error "qq: Guild channel identity requires a canonical nonzero decimal string"))
  (format "guild:%s:channel:%s"
          guild-id channel-id))

(defun qq-state-session-key (type target-id &optional variant)
  "Build a canonical session key from TYPE, TARGET-ID, and VARIANT.

DataLine keys require VARIANT to be `desktop' or `mobile'.  TARGET-ID is an
opaque native peer UID for DataLine and service sessions; it is preserved
byte-for-byte, including any colon characters."
  (pcase type
    ((or 'private "private")
     (when variant
       (error "qq: private session does not accept a variant"))
     (format "private:%s"
             (qq-state--canonical-decimal-id target-id "private session")))
    ((or 'group "group")
     (when variant
       (error "qq: group session does not accept a variant"))
     (format "group:%s"
             (qq-state--canonical-decimal-id target-id "group session")))
    ((or 'dataline "dataline")
     (format "dataline:%s:%s"
             (qq-state--canonical-dataline-variant variant)
             (qq-state--canonical-peer-uid target-id "dataline session")))
    ((or 'service "service")
     (when variant
       (error "qq: service session does not accept a variant"))
     (format "service:%s"
             (qq-state--canonical-peer-uid target-id "service session")))
    (_ (error "qq: unsupported session type %S" type))))

(defun qq-state-session-key-identity (session-key)
  "Decode canonical SESSION-KEY into its complete immutable identity.

The result contains `type', `target-id', `chat-type', `peer-uid', and
`variant'.  Opaque peer UIDs are decoded only by removing their fixed prefix;
they are never split, normalized, escaped, or reconstructed from metadata."
  (unless (stringp session-key)
    (error "qq: session key must be a string, got %S" session-key))
  (let (type target-id chat-type peer-uid variant guild-id)
    (cond
     ((string-prefix-p "private:" session-key)
      (setq type 'private
            target-id (substring session-key (length "private:"))
            chat-type "1"))
     ((string-prefix-p "group:" session-key)
      (setq type 'group
            target-id (substring session-key (length "group:"))
            chat-type "2"))
     ((string-match
       "\\`guild:\\([1-9][0-9]*\\):channel:\\([1-9][0-9]*\\)\\'"
       session-key)
      (setq type 'guild-channel
            guild-id (match-string 1 session-key)
            target-id (match-string 2 session-key)
            chat-type "4"
            peer-uid target-id
            variant nil))
     ((string-prefix-p "dataline:desktop:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:desktop:"))
            chat-type "8"
            peer-uid target-id
            variant "desktop"))
     ((string-prefix-p "dataline:mobile:" session-key)
      (setq type 'dataline
            target-id (substring session-key (length "dataline:mobile:"))
            chat-type "134"
            peer-uid target-id
            variant "mobile"))
     ((string-prefix-p "service:" session-key)
      (setq type 'service
            target-id (substring session-key (length "service:"))
            chat-type "103"
            peer-uid target-id))
     (t
      (error "qq: unsupported canonical session key %S" session-key)))
    (pcase type
      ((or 'private 'group)
       (unless (qq-protocol--decimal-string-p target-id)
         (error "qq: malformed canonical %s session key %S"
                type session-key)))
      ('guild-channel
       (unless (and (qq-protocol--nonzero-decimal-string-p guild-id)
                    (qq-protocol--nonzero-decimal-string-p target-id))
         (error "qq: malformed canonical Guild channel session key %S"
                session-key)))
      ((or 'dataline 'service)
       (unless (and (stringp peer-uid) (not (string-empty-p peer-uid)))
         (error "qq: malformed canonical %s session key %S"
                type session-key))))
    `((type . ,type)
      (target-id . ,target-id)
      (chat-type . ,chat-type)
      (peer-uid . ,peer-uid)
      (variant . ,variant)
      ,@(when (eq type 'guild-channel)
          `((guild-id . ,guild-id)
            (channel-id . ,target-id))))))

(defun qq-state-session-key-type (session-key)
  "Return session type symbol extracted from SESSION-KEY."
  (alist-get 'type (qq-state-session-key-identity session-key)))

(defun qq-state-session-sendable-p (session-key)
  "Return non-nil when SESSION-KEY supports outbound messages."
  (condition-case nil
      (let* ((identity (qq-state-session-key-identity session-key))
             (type (alist-get 'type identity)))
        (pcase type
          ((or 'private 'group 'dataline) t)
          ('guild-channel
           (equal
            (alist-get
             'kind
             (gethash session-key qq-state--guild-channels-by-key))
            "text"))
          (_ nil)))
    (error nil)))

(defun qq-state-session-key-target-id (session-key)
  "Return target id extracted from SESSION-KEY."
  (alist-get 'target-id (qq-state-session-key-identity session-key)))

(defun qq-state--cached-private-title (target-id)
  "Return best cached title for private TARGET-ID."
  (let ((friend (gethash (qq-state--normalize-id target-id) qq-state--friends-by-id)))
    (or (and (listp friend)
             (let ((remark (alist-get 'remark friend))
                   (nickname (alist-get 'nickname friend)))
               (cond
                ((and (stringp remark) (not (string-empty-p remark))) remark)
                ((and (stringp nickname) (not (string-empty-p nickname))) nickname)
                (t nil))))
        (qq-state--normalize-id target-id))))

(defun qq-state--cached-group-title (target-id)
  "Return best cached title for group TARGET-ID."
  (let ((group (gethash (qq-state--normalize-id target-id) qq-state--groups-by-id)))
    (or (and (listp group)
             (let ((name (alist-get 'group_name group)))
               (and (stringp name)
                    (not (string-empty-p name))
                    name)))
        (qq-state--normalize-id target-id))))

(defun qq-state--cached-guild-channel-title (guild-id channel-id)
  "Return the cached display title for GUILD-ID and CHANNEL-ID."
  (let* ((key (qq-state-guild-channel-session-key guild-id channel-id))
         (channel (gethash key qq-state--guild-channels-by-key))
         (guild-name (and channel (alist-get 'guild_name channel)))
         (channel-name (and channel (alist-get 'name channel))))
    (if (and (stringp guild-name) (not (string-empty-p guild-name))
             (stringp channel-name) (not (string-empty-p channel-name)))
        (format "%s · #%s" guild-name channel-name)
      (or channel-name channel-id))))

(defun qq-state--default-session-title (session)
  "Return default title for SESSION using local caches."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group
       (qq-state--cached-group-title target-id))
      ('guild-channel
       (qq-state--cached-guild-channel-title
        (alist-get 'guild-id session) target-id))
      ('dataline
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           "我的手机"))
      ('service
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           (qq-state--cached-private-title
            (or (alist-get 'peer-uin session) target-id))))
      (_
       (qq-state--cached-private-title target-id)))))

(defun qq-state--session-template (session-key)
  "Return base session object for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (target-id (alist-get 'target-id identity)))
    `((key . ,session-key)
      ,@identity
      (title . ,target-id)
      (unread-count . 0)
      (unread-at-me-message-id . nil)
      (unread-at-me-message-seq . nil)
      (unread-at-all-message-id . nil)
      (unread-at-all-message-seq . nil)
      (muted-p . nil)
      (message-notify-mode . unspecified)
      (first-unread-message-id . nil)
      (first-unread-message-seq . nil)
      (read-position-available . nil)
      (read-latest-message-id . nil)
      (last-message-time . 0)
      (last-message-preview . "")
      (last-message-sender-name . nil)
      (last-message-self-p . nil)
      (last-message-summary-token . 0)
      (last-message-local-id . nil)
      (last-message-order . nil)
      (last-message-seq . nil)
      (last-message-id . nil)
      (oldest-message-id . nil))))

(defun qq-state--merge-alists (old new)
  "Return OLD merged with NEW by symbol key.

Values from NEW replace values in OLD."
  (let ((merged (copy-tree old)))
    (dolist (pair new)
      (setf (alist-get (car pair) merged nil nil #'eq) (cdr pair)))
    merged))

(defun qq-state-message-recalled-p (message)
  "Return non-nil when local MESSAGE has status `recalled'."
  (eq (alist-get 'status message) 'recalled))

(defun qq-state-poke-message-p (message)
  "Return non-nil when MESSAGE is a NapCat poke notice row."
  (or (qq-state--poke-notice-p message)
      (let ((raw-event (alist-get 'raw-event message)))
        (and (listp raw-event)
             (qq-state--poke-notice-p raw-event)))
      (seq-some
       (lambda (segment)
         (equal (alist-get 'type segment) "poke"))
       (alist-get 'segments message))))

(defun qq-state-gray-tip-message-p (message)
  "Return non-nil when MESSAGE is a QQ gray-tip notice row."
  (or (and (listp message) (alist-get 'gray-tip-p message))
      (let ((raw-event (and (listp message) (alist-get 'raw-event message))))
        (and (listp raw-event)
             (qq-state--gray-tip-notice-p raw-event)))
      (seq-some
       (lambda (segment)
         (equal (alist-get 'type segment) "gray-tip"))
       (and (listp message) (alist-get 'segments message)))))

(defun qq-state-gray-tip-message-data (message)
  "Return normalized visual data for gray-tip MESSAGE."
  (when (qq-state-gray-tip-message-p message)
    (let* ((segment
            (seq-find
             (lambda (candidate)
               (equal (alist-get 'type candidate) "gray-tip"))
             (alist-get 'segments message)))
           (segment-data (and segment (alist-get 'data segment))))
      (or segment-data
          (let ((notice (if (qq-state--gray-tip-notice-p message)
                            message
                          (alist-get 'raw-event message))))
            (and notice (qq-state--gray-tip-data notice)))))))

(defun qq-state-poke-message-data (message)
  "Return normalized visual data for poke MESSAGE.

Older rows may only have `raw-event', so derive the same data lazily when
their cached segment predates the richer poke renderer."
  (when (qq-state-poke-message-p message)
    (let* ((segment
            (seq-find
             (lambda (candidate)
               (equal (alist-get 'type candidate) "poke"))
             (alist-get 'segments message)))
           (segment-data (and segment (alist-get 'data segment))))
      (or segment-data
          (let* ((notice (if (qq-state--poke-notice-p message)
                             message
                           (alist-get 'raw-event message)))
                 (actor-id (qq-state--normalize-id
                            (or (alist-get 'sender-id message)
                                (alist-get 'sender_id notice)
                                (alist-get 'user_id notice))))
                 (target-id (qq-state--normalize-id
                             (or (alist-get 'target-id message)
                                 (alist-get 'target_id notice))))
                 (actor-name (or (qq-state--present-string
                                  (alist-get 'sender-name message))
                                 (qq-state--poke-user-name actor-id nil)))
                 (target-name (or (qq-state--present-string
                                   (alist-get 'target-name message))
                                  (qq-state--poke-user-name target-id nil))))
            (qq-state--normalize-poke-info
             notice actor-id target-id actor-name target-name))))))

(defun qq-state-poke-recall-reference (message)
  "Return a copy of MESSAGE's native poke recall reference, or nil."
  (when (qq-state-poke-message-p message)
    (copy-tree (alist-get 'poke-recall-reference message))))

(defun qq-state--validate-poke-recall-context (reference session-key)
  "Return REFERENCE when its native Peer belongs to SESSION-KEY.

Group peers are their decimal group IDs.  Private peers are opaque NT UIDs;
the session must already own that exact UID before a recall can be sent."
  (when reference
    (unless session-key
      (error "qq: poke recall reference has no conversation"))
    (let* ((peer (alist-get 'peer reference))
           (chat-type (alist-get 'chat_type peer))
           (peer-uid (alist-get 'peer_uid peer))
           (session-type (qq-state-session-key-type session-key))
           (target-id (qq-state-session-key-target-id session-key))
           (known-peer-uid
            (alist-get 'peer-uid (gethash session-key qq-state--sessions))))
      (pcase session-type
        ('group
         (unless (and (= chat-type 2)
                      (equal peer-uid target-id))
           (error "qq: group poke recall reference does not match %s"
                  session-key)))
        ('private
         (unless (= chat-type 1)
           (error "qq: private poke recall reference has non-private peer"))
         (unless (and (stringp known-peer-uid)
                      (not (string-empty-p known-peer-uid)))
           (error "qq: private poke recall requires the session's exact peer UID"))
         (unless (equal peer-uid known-peer-uid)
           (error "qq: private poke recall reference does not match %s"
                  session-key)))
        (_
         (error "qq: poke recall reference has unsupported conversation %s"
                session-key)))))
  reference)

(defun qq-state-validate-poke-recall-reference (session-key reference)
  "Return a copy of REFERENCE after validating explicit SESSION-KEY."
  (qq-state-session-key-identity session-key)
  (copy-tree (qq-state--validate-poke-recall-context reference session-key)))

(defun qq-state--raw-message-recalled-p (message)
  "Return non-nil when raw OneBot MESSAGE is explicitly recalled.

Only protocol fields from the NapCat fork (`recalled', `recall_time').
Empty bodies alone are not treated as recalled."
  (let ((recalled (alist-get 'recalled message))
        (recall-time (alist-get 'recall_time message)))
    (or (eq recalled t)
        (eq recalled 'true)
        (and (numberp recalled) (not (zerop recalled)))
        (and (stringp recalled)
             (member (downcase recalled) '("true" "1" "yes")))
        (and recall-time
             (not (member (format "%s" recall-time) '("" "0" "nil")))))))

(defun qq-state--as-recalled-message (message)
  "Return MESSAGE with recalled stub fields (kept in store for optional display)."
  (qq-state--merge-alists
   message
   '((status . recalled)
     (segments . nil)
     (raw-message . "[message recalled]")
     (preview . "[message recalled]"))))

(defun qq-state-session (session-key)
  "Return session object for SESSION-KEY."
  (copy-tree (gethash session-key qq-state--sessions)))

(defun qq-state--hydrate-session (session)
  "Return SESSION with title hydrated from contact caches when possible."
  (let* ((target-id (alist-get 'target-id session))
         (title (alist-get 'title session))
         (default-title (qq-state--default-session-title session)))
    (when (and default-title
               (or (null title)
                   (equal title target-id)
                   (string-empty-p title)))
      (setf (alist-get 'title session nil nil #'eq) default-title))
    session))

(defun qq-state-upsert-session (session-key fields &optional emit)
  "Insert or update SESSION-KEY with FIELDS.

When EMIT is non-nil, fire one session mutation event (`:mutation' `session')."
  (let* ((identity (qq-state-session-key-identity session-key))
         (canonical-fields
          (seq-filter
           (lambda (field)
             (or (memq (car field) '(type target-id chat-type variant))
                 (cdr field)))
           identity))
         (existing (or (copy-tree (gethash session-key qq-state--sessions))
                       (qq-state--session-template session-key)))
         (session (qq-state--merge-alists existing fields)))
    ;; The key is the sole source of routing identity.  Metadata refreshes and
    ;; message payloads may enrich display fields, but may never retarget an
    ;; existing session.
    ;; A private/group key cannot derive the kernel peer UID; retain that UID
    ;; when a native payload later supplies it as capability metadata.  All
    ;; encoded routing fields (including a nil non-DataLine variant) still
    ;; overwrite mutable payload metadata.
    (setq session (qq-state--merge-alists session canonical-fields))
    (setf (alist-get 'key session nil nil #'eq) session-key)
    (setq session (qq-state--hydrate-session session))
    (puthash session-key session qq-state--sessions)
    (when emit
      (qq-state--emit 'session
                      :session-key session-key
                      :session (copy-tree session)
                      :mutation 'session))
    session))

(defun qq-state-sessions ()
  "Return all known sessions sorted by last activity."
  (let (sessions)
    (maphash (lambda (_key session)
               (push (copy-tree session) sessions))
             qq-state--sessions)
    (sort sessions
          (lambda (left right)
            (let ((left-time (qq-state--normalize-time (alist-get 'last-message-time left)))
                  (right-time (qq-state--normalize-time (alist-get 'last-message-time right))))
              (if (/= left-time right-time)
                  (> left-time right-time)
                (string-lessp (or (alist-get 'title left) "")
                              (or (alist-get 'title right) ""))))))))

(defun qq-state--short-media-label (value &optional fallback)
  "Return a short label for media VALUE, never a full URL or CQ blob.

Prefer basename of local paths; drop http(s) URLs entirely."
  (let ((fallback (or fallback "media")))
    (cond
     ((not (stringp value)) fallback)
     ((string-empty-p (string-trim value)) fallback)
     ((string-match-p "\\`https?://" value) fallback)
     ((string-match-p "\\`\\[CQ:" value) fallback)
     ((or (file-name-absolute-p value)
          (string-match-p "/" value)
          (string-match-p "\\\\" value))
      (let ((base (file-name-nondirectory value)))
        (if (or (string-empty-p base)
                (string-match-p "\\`https?:" base))
            fallback
          (truncate-string-to-width base 28 nil nil t))))
     (t (truncate-string-to-width (string-trim value) 28 nil nil t)))))

(defun qq-state-preview-one-line (value)
  "Return VALUE as a compact single-line message preview.

Like telega's `telega-ins--one-lined', line breaks and repeated horizontal
whitespace are presentation details rather than part of a root-row preview."
  (if (stringp value)
      (string-trim
       (replace-regexp-in-string "[[:space:]\u00a0]+" " " value))
    ""))

(defun qq-state-message-preview-from-segments (segments)
  "Return a human-readable plain-text preview for message SEGMENTS.

Never emit OneBot CQ strings.  Reply segments are omitted (shown via
reply chrome elsewhere).  Media becomes short placeholders like
`[image]' / `[face:178]'."
  (qq-state-preview-one-line
   (mapconcat
    (lambda (segment)
      (let ((type (alist-get 'type segment))
            (data (alist-get 'data segment)))
        (pcase type
          ("text" (or (alist-get 'text data) ""))
          ("at" (concat "@" (or (alist-get 'name data)
                                (alist-get 'qq data)
                                "mention")))
          ;; Reply chrome is rendered separately in chatbuf / composer.
          ("reply" "")
          ("__unsupported"
           (let* ((summary (alist-get 'summary data))
                  (summary (if (stringp summary)
                               (replace-regexp-in-string
                                "[\n\r\t ]+" " " summary)
                             "unknown element")))
             (format "[unsupported QQ element: %s]"
                     (truncate-string-to-width summary 80 nil nil t))))
          ("face"
           (let* ((raw (alist-get 'raw data))
                  (text (or (alist-get 'description data)
                            (alist-get 'faceText data)
                            (alist-get 'face_text data)
                            (and (listp raw) (alist-get 'faceText raw))
                            (and (listp raw) (alist-get 'face_text raw))))
                  (id (or (alist-get 'id data)
                          (alist-get 'faceIndex data)
                          (and (listp raw) (alist-get 'faceIndex raw))))
                  (named (and id
                              (fboundp 'qq-media-face-text-fallback)
                              (qq-media-face-text-fallback id))))
             (cond
              ((and (stringp text) (not (string-empty-p text))) text)
              ((and (stringp named) (not (string-empty-p named))) named)
              (id (format "[face:%s]" id))
              (t "[face]"))))
          ;; Compact previews stay telega-short; chat body uses media cards.
          ("image" "[image]")
          ("mface" "[sticker]")
          ("file"
           (format "[file:%s]"
                   (qq-state--short-media-label
                    (or (alist-get 'name data)
                        (alist-get 'file data))
                    "file")))
          ("record" "[voice]")
          ("video" "[video]")
          ("mail"
           (or (qq-state--present-string (alist-get 'prompt data))
               (let ((parts (delq nil
                                  (list (qq-state--present-string
                                         (alist-get 'sender data))
                                        (qq-state--present-string
                                         (alist-get 'subject data))))))
                 (and parts (string-join parts ": ")))
               "[mail]"))
          ("wallet"
           (let* ((receiver (alist-get 'receiver data))
                  (sender (alist-get 'sender data))
                  (kind (alist-get 'wallet_kind data)))
             (or (qq-state--present-string (alist-get 'notice receiver))
                 (qq-state--present-string (alist-get 'title receiver))
                 (qq-state--present-string (alist-get 'notice sender))
                 (qq-state--present-string (alist-get 'title sender))
                 (pcase kind
                   ("transfer" "[转账]")
                   ("red-packet" "[QQ红包]")
                   ("password-red-packet" "[口令红包]")
                   (_ "[QQ钱包]")))))
          ("card"
           (or (qq-state--present-string (alist-get 'prompt data))
               (qq-state--present-string (alist-get 'title data))
               (qq-state--present-string (alist-get 'content data))
               "[card]"))
          ("json" "[card]")
          ("xml" "[xml]")
          ("poke"
           (let ((action (qq-state--present-string (alist-get 'action data)))
                 (target (qq-state--present-string (alist-get 'target-name data)))
                 (detail (qq-state--present-string (alist-get 'detail data))))
             (or (and action target (concat action " " target detail))
                 (and action (concat action detail))
                 (and target (concat "戳了戳 " target))
                 "[poke]")))
          ("gray-tip"
           (or (qq-state--present-string (alist-get 'text data))
               "QQ system notice"))
          ("dice" "[dice]")
          ("rps" "[rps]")
          ("share" "[share]")
          ("location" "[location]")
          ("music" "[music]")
          ("forward" "[forward]")
          ("node" "[forward]")
          ("markdown" (or (alist-get 'content data)
                          (alist-get 'text data)
                          "[markdown]"))
          (_ (format "[%s]" (or type "message"))))))
    (or segments '())
    " ")))

(defun qq-state--mention-kinds-from-segments (segments)
  "Return native QQ mention kinds found in SEGMENTS.

`at-me' denotes a direct mention of the current account and `at-all' denotes
QQ's @全体成员.  Ordinary mentions of another member are deliberately ignored."
  (let ((self-id (qq-state-self-user-id))
        kinds)
    (dolist (segment (or segments '()))
      (when (equal (alist-get 'type segment) "at")
        (let* ((data (alist-get 'data segment))
               (target (qq-state--normalize-id (alist-get 'qq data))))
          (cond
           ((equal target "all") (cl-pushnew 'at-all kinds))
           ((and self-id (equal target self-id))
            (cl-pushnew 'at-me kinds))))))
    (nreverse kinds)))

(defun qq-state-message-mention-kinds (message)
  "Return normalized native mention kinds for MESSAGE."
  (copy-sequence (or (alist-get 'mention-kinds message) '())))

(defun qq-state-message-mentions-self-p (message)
  "Return non-nil when MESSAGE directly mentions the current account."
  (and (memq 'at-me (qq-state-message-mention-kinds message)) t))

(defun qq-state-message-mentions-all-p (message)
  "Return non-nil when MESSAGE contains QQ's @全体成员."
  (and (memq 'at-all (qq-state-message-mention-kinds message)) t))

(defun qq-state--cq-looks-p (string)
  "Return non-nil when STRING looks like OneBot CQ `raw_message'."
  (and (stringp string)
       (string-match-p "\\[CQ:" string)))

(defun qq-state--decode-cq-entities (string)
  "Decode common CQ / HTML entities in STRING."
  (let ((s (or string "")))
    (dolist (pair '(("&amp;" . "&")
                    ("&#44;" . ",")
                    ("&#91;" . "[")
                    ("&#93;" . "]")
                    ("&lt;" . "<")
                    ("&gt;" . ">")
                    ("&quot;" . "\"")))
      (setq s (replace-regexp-in-string (regexp-quote (car pair))
                                        (cdr pair) s t t)))
    s))

(defun qq-state-message-preview-from-cq (raw)
  "Convert OneBot CQ RAW string into a short human-readable preview.

Used only as a fallback when structured `message' segments are missing.
Strips reply tags and collapses media/face codes."
  (let* ((s (qq-state--decode-cq-entities (or raw "")))
         (parts nil)
         (pos 0)
         (len (length s)))
    (while (< pos len)
      (if (string-match "\\[CQ:\\([a-zA-Z0-9_-]+\\)\\(,[^]]*\\)?\\]" s pos)
          (let* ((start (match-beginning 0))
                 (end (match-end 0))
                 (type (match-string 1 s))
                 (params (or (match-string 2 s) ""))
                 (plain (substring s pos start)))
            (when (and plain (not (string-empty-p plain)))
              (push plain parts))
            (pcase type
              ("reply" nil)
              ("text"
               (when (string-match "text=\\([^,]+\\)" params)
                 (push (match-string 1 params) parts)))
              ("at"
               (push (concat "@"
                             (or (and (string-match "name=\\([^,]+\\)" params)
                                      (match-string 1 params))
                                 (and (string-match "qq=\\([^,]+\\)" params)
                                      (match-string 1 params))
                                 "mention"))
                     parts))
              ("face"
               (push (if (string-match "id=\\([^,]+\\)" params)
                         (let ((id (match-string 1 params)))
                           (if (fboundp 'qq-media-face-text-fallback)
                               (qq-media-face-text-fallback id)
                             (format "[face:%s]" id)))
                       "[face]")
                     parts))
              ("image" (push "[image]" parts))
              ("mface" (push "[sticker]" parts))
              ("record" (push "[voice]" parts))
              ("video" (push "[video]" parts))
              ("file"
               (push (if (string-match "name=\\([^,]+\\)" params)
                         (format "[file:%s]"
                                 (qq-state--short-media-label
                                  (match-string 1 params) "file"))
                       "[file]")
                     parts))
              ("json" (push "[card]" parts))
              (_ (push (format "[%s]" type) parts)))
            (setq pos end))
        (push (substring s pos) parts)
        (setq pos len)))
    (qq-state-preview-one-line
     (mapconcat #'identity (nreverse parts) ""))))

(defun qq-state-message-preview (message)
  "Return human-readable preview text for normalized MESSAGE.

Prefer structured segment previews.  Never surface OneBot CQ `raw_message'
in the UI — that is wire format, not display text."
  (qq-state-preview-one-line
   (or (let ((from-segments
              (qq-state-message-preview-from-segments
               (alist-get 'segments message))))
         (and (stringp from-segments)
              (not (string-empty-p from-segments))
              from-segments))
       (let ((stored (alist-get 'preview message)))
         (and (stringp stored)
              (not (string-empty-p (string-trim stored)))
              (not (qq-state--cq-looks-p stored))
              stored))
       (let ((raw (alist-get 'raw-message message)))
         (cond
          ((not (stringp raw)) nil)
          ((string-empty-p (string-trim raw)) nil)
          ((qq-state--cq-looks-p raw)
           (let ((converted (qq-state-message-preview-from-cq raw)))
             (and (not (string-empty-p converted)) converted)))
          (t raw)))
       "")))

(defun qq-state--message-chat-type (message)
  "Return raw backend chat type extracted from raw MESSAGE, or nil."
  (alist-get 'chat_type message))

(defun qq-state--message-peer-uid (message)
  "Return raw backend peer uid extracted from raw MESSAGE, or nil."
  (let ((peer-uid (alist-get 'peer_uid message)))
    (and (stringp peer-uid)
         (not (string-empty-p peer-uid))
         peer-uid)))

(defun qq-state--message-peer-uin (message)
  "Return the native decimal peer UIN carried by raw MESSAGE, or nil.

Unlike display and participant fields, `peer_uin' is the conversation
identity for ordinary private and group messages.  The fork exposes it as a
string so accepting another representation here would reintroduce an
ambiguous identity source."
  (let ((peer-uin (alist-get 'peer_uin message)))
    (and (stringp peer-uin)
         (not (string-empty-p peer-uin))
         peer-uin)))

(defun qq-state--message-self-p (message)
  "Return non-nil when raw MESSAGE belongs to the logged in account."
  (let ((sender-id (qq-state--normalize-id
                    (or (alist-get 'user_id (alist-get 'sender message))
                        (alist-get 'user_id message))))
        (self-id (qq-state--normalize-id
                  (or (alist-get 'self_id message)
                      (alist-get 'user_id qq-state--self-info)))))
    (or (equal (alist-get 'post_type message) "message_sent")
        (and sender-id self-id (equal sender-id self-id)))))

(defun qq-state--raw-message-session-key (message &optional expected-session-key)
  "Return the exact canonical session key carried by raw MESSAGE.

When EXPECTED-SESSION-KEY is non-nil, require MESSAGE to carry that same
identity.  The request context is an assertion, never a substitute for
missing or contradictory wire identity."
  (let* ((raw-chat-type (qq-state--message-chat-type message))
         (chat-type (qq-state--normalize-id raw-chat-type))
         (peer-uin (qq-state--message-peer-uin message))
         (peer-uid (qq-state--message-peer-uid message))
         (derived
          (pcase chat-type
            ("1"
             (unless peer-uin
               (error "qq: private message requires native peer_uin string"))
             (qq-state-session-key 'private peer-uin))
            ("2"
             (unless peer-uin
               (error "qq: group message requires native peer_uin string"))
             (qq-state-session-key 'group peer-uin))
            ((or "8" "134")
             (when peer-uid
               (qq-state-session-key
                'dataline peer-uid
                (qq-state--dataline-variant-from-chat-type chat-type))))
            ("103"
             (when peer-uid
               (qq-state-session-key 'service peer-uid)))
            (_ nil))))
    (when expected-session-key
      (qq-state-session-key-identity expected-session-key)
      (unless derived
        (error "qq: message is missing a supported native session identity"))
      (unless (equal derived expected-session-key)
        (error "qq: message session %S contradicts expected session %S"
               derived expected-session-key)))
    derived))

(defun qq-state--poke-notice-p (message)
  "Return non-nil when MESSAGE is a NapCat poke notice."
  (and (equal (alist-get 'post_type message) "notice")
       (equal (alist-get 'notice_type message) "notify")
       (equal (alist-get 'sub_type message) "poke")))

(defun qq-state--gray-tip-notice-p (message)
  "Return non-nil when MESSAGE is a NapCat gray-tip notice."
  (and (equal (alist-get 'post_type message) "notice")
       (equal (alist-get 'notice_type message) "notify")
       (equal (alist-get 'sub_type message) "gray_tip")))

(defun qq-state--gray-tip-json (notice)
  "Return decoded JSON payload from gray-tip NOTICE, or nil."
  (or (let ((raw-info (alist-get 'raw_info notice)))
        (and (listp raw-info) (alist-get 'json raw-info)))
      (when-let* ((content (alist-get 'content notice))
                  ((stringp content)))
        (condition-case nil
            (json-parse-string content
                               :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (json-parse-error nil)))))

(defun qq-state--normalize-gray-tip-parts (notice)
  "Return strict interactive presentation parts carried by NOTICE."
  (mapcar
   (lambda (part)
     (unless (listp part)
       (error "qq: gray-tip presentation part is not an object"))
     (pcase (alist-get 'type part)
       ("text"
        (let ((text (qq-state--present-string (alist-get 'text part))))
          (unless text
            (error "qq: gray-tip text part is empty"))
          `((type . "text") (text . ,text))))
       ("user"
        (let ((user-id (qq-state--normalize-id (alist-get 'user_id part)))
              (name (qq-state--present-string (alist-get 'name part)))
              (role (alist-get 'role part)))
          (unless (qq-protocol--nonzero-decimal-string-p user-id)
            (error "qq: gray-tip user part requires an exact QQ number"))
          (unless name
            (error "qq: gray-tip user part requires a display name"))
          (unless (member role '("member" "inviter"))
            (error "qq: gray-tip user part has invalid role %S" role))
          `((type . "user")
            (role . ,role)
            (user-id . ,user-id)
            (name . ,name))))
       (_
        (error "qq: unsupported gray-tip presentation part %S"
               (alist-get 'type part)))))
   (or (alist-get 'gray_tip_parts notice) '())))

(defun qq-state--gray-tip-data (notice)
  "Return stable visual data decoded from gray-tip NOTICE."
  (let* ((direct-text
          (qq-state--present-string (alist-get 'text notice)))
         (json (qq-state--gray-tip-json notice))
         (items (and (listp json) (alist-get 'items json)))
         (texts
          (delq nil
                (mapcar
                 (lambda (item)
                   (and (listp item)
                        (qq-state--present-string (alist-get 'txt item))))
                 (if (listp items) items '()))))
         (text (or direct-text
                   (and texts (string-join texts ""))
                   "QQ system notice")))
    `((text . ,text)
      (kind . ,(qq-state--present-string
                (alist-get 'gray_tip_kind notice)))
      (busi-id . ,(qq-state--normalize-id (alist-get 'busi_id notice)))
      (parts . ,(qq-state--normalize-gray-tip-parts notice))
      (items . ,(copy-tree items)))))

(defun qq-state--poke-user-name (user-id explicit-name)
  "Return a display name for POKE USER-ID, preferring EXPLICIT-NAME."
  (let* ((user-id (qq-state--normalize-id user-id))
         (friend (and user-id (gethash user-id qq-state--friends-by-id)))
         (self-p (and user-id
                      (equal user-id (qq-state-self-user-id))))
         (name (or explicit-name
                   (and self-p
                        (qq-state--present-string
                         (alist-get 'nickname qq-state--self-info)))
                   (and friend
                        (qq-state--present-string
                         (alist-get 'remark friend)))
                   (and friend
                        (qq-state--present-string
                         (alist-get 'nickname friend)))
                   user-id)))
    (or name "某人")))

(defun qq-state--normalize-poke-info
    (notice actor-id target-id actor-name target-name)
  "Return normalized visual metadata from POKE NOTICE.

`raw_info' is NapCat's QQ-native gray-tip decoration list.  Keep only the
small stable subset needed by the Emacs renderer: the action image URL and
the natural-language fragments.  The complete original notice remains in
`raw-event' for protocol/debugging purposes."
  (let ((raw-info (alist-get 'raw_info notice))
        image-url
        texts)
    (dolist (item (if (listp raw-info) raw-info '()))
      (when (listp item)
        (let ((text (qq-state--present-string (alist-get 'txt item)))
              (type (qq-state--normalize-id (alist-get 'type item))))
          (when (and (equal type "img") (null image-url))
            (setq image-url
                  (qq-state--present-string
                   (or (alist-get 'src item)
                       (alist-get 'url item)
                       (alist-get 'jp item)))))
          (when text
            (push text texts)))))
    (setq texts (nreverse texts))
    `((actor-id . ,actor-id)
      (target-id . ,target-id)
      (actor-name . ,actor-name)
      (target-name . ,target-name)
      (image-url . ,image-url)
      (action . ,(car texts))
      (detail . ,(and (cdr texts) (string-join (cdr texts) "")))
      (texts . ,texts))))

(defun qq-state--normalize-poke-notice (notice &optional expected-session-key)
  "Normalize a live or historical POKE NOTICE into a timeline message."
  (let* ((group-id (qq-state--normalize-id (alist-get 'group_id notice)))
         (local-marker-cell (assq 'emacs_local_p notice))
         (recall-reference-cell (assq 'recall_reference notice))
         (legacy-message-id-cell (assq 'message_id notice))
         (local-poke-p
          (and local-marker-cell
               (eq (cdr local-marker-cell) t)))
         (unscoped-recall-reference
          (cond
           (legacy-message-id-cell
            (error "qq: poke notice must not carry top-level message_id"))
           ((and local-poke-p (null recall-reference-cell)) nil)
           ((and (null local-marker-cell) recall-reference-cell)
            (qq-protocol-validate-poke-recall-reference
             (cdr recall-reference-cell) "poke notice"))
           (t
            (error
             "qq: poke notice must be either local or carry recall_reference"))))
         ;; In a private poke, user_id is the peer and sender_id is the actor
         ;; (the fork emits sender_id for this case).  Group pokes use user_id
         ;; as the actor, matching the OneBot notice contract.
         (peer-id (and (null group-id)
                       (qq-state--normalize-id (alist-get 'user_id notice))))
         (derived-session-key
          (cond
           (group-id (qq-state-session-key 'group group-id))
           (peer-id (qq-state-session-key 'private peer-id))))
         (session-key
          (progn
            (when expected-session-key
              (qq-state-session-key-identity expected-session-key)
              (unless derived-session-key
                (error "qq: poke notice is missing its session identity"))
              (unless (equal derived-session-key expected-session-key)
                (error "qq: poke session %S contradicts expected session %S"
                       derived-session-key expected-session-key)))
            derived-session-key))
         (recall-reference
          (qq-state--validate-poke-recall-context
           unscoped-recall-reference session-key))
         (actor-id (qq-state--normalize-id
                    (or (alist-get 'sender_id notice)
                        (alist-get 'user_id notice))))
         (target-id (qq-state--normalize-id (alist-get 'target_id notice)))
         (actor-name
          (qq-state--poke-user-name actor-id (alist-get 'sender_name notice)))
         (target-name
          (qq-state--poke-user-name target-id (alist-get 'target_name notice)))
         (poke-info
          (qq-state--normalize-poke-info
           notice actor-id target-id actor-name target-name))
         (body (if target-id
                   (format "戳了戳 %s" target-name)
                 "戳了戳"))
         (time (qq-state--normalize-time
                (or (alist-get 'time notice) (float-time))))
         ;; The authoritative fork exposes one closed native recall reference;
         ;; its msgId is also the timeline identity.  Optimistic local notices
         ;; remain local-only until the matching authoritative event arrives.
         (server-id (alist-get 'message_id recall-reference))
         (local-id
          (and local-poke-p
               (format "local-poke-%d"
                       (cl-incf qq-state--local-message-counter))))
         (anchor (or server-id local-id))
         (self-p (and actor-id
                      (equal actor-id (qq-state-self-user-id))))
         (message
          `((id . ,anchor)
            (server-id . ,server-id)
            (local-id . ,local-id)
            (session-key . ,session-key)
            (time . ,time)
            (sender-id . ,actor-id)
            (sender-name . ,actor-name)
            (sender-secondary-name . nil)
            (sender-card . nil)
            (sender-nickname . ,actor-name)
            (sender-remark . nil)
            (self-p . ,self-p)
            (local-poke-p . ,local-poke-p)
            (poke-recall-reference . ,recall-reference)
            (status . ,(if self-p 'sent 'received))
            ;; Poke notices are gray-tip records, not ordinary text messages.
            ;; Keep their visual metadata as a dedicated segment so chat can
            ;; render the QQ-style action without using the normal message
            ;; header/body layout.
            (segments . ,(list `((type . "poke")
                                 (data . ,poke-info))))
            (raw-message . ,body)
            (preview . ,body)
            (message-type . ,(if group-id "group" "private"))
            (group-id . ,group-id)
            (user-id . ,actor-id)
            (target-id . ,target-id)
            (order . ,(qq-state--next-message-order))
            (raw-event . ,(copy-tree notice)))))
    message))

(defun qq-state--normalize-gray-tip-notice
    (notice &optional expected-session-key)
  "Normalize fork-native gray-tip NOTICE into a timeline message.

When EXPECTED-SESSION-KEY is non-nil, require NOTICE's native `group_id' to
identify that exact group session."
  (let* ((group-id (qq-state--normalize-id (alist-get 'group_id notice)))
         (derived-session-key
          (and group-id (qq-state-session-key 'group group-id)))
         (session-key
          (progn
            (when expected-session-key
              (qq-state-session-key-identity expected-session-key)
              (unless derived-session-key
                (error "qq: gray-tip notice is missing its group identity"))
              (unless (equal derived-session-key expected-session-key)
                (error "qq: gray-tip session %S contradicts expected session %S"
                       derived-session-key expected-session-key)))
            derived-session-key))
         (server-id
          (qq-protocol-optional-message-id
           (alist-get 'message_id notice)
           "gray-tip notice"))
         (data (qq-state--gray-tip-data notice))
         (text (alist-get 'text data))
         (raw-info (alist-get 'raw_info notice))
         (time
          (qq-state--normalize-time
           (or (alist-get 'time notice)
               (and (listp raw-info) (alist-get 'msgTime raw-info)))))
         (sender-id
          (let ((id (qq-state--normalize-id (alist-get 'user_id notice))))
            (and id (not (equal id "0")) id))))
    (unless session-key
      (error "qq: gray-tip notice requires group_id"))
    (unless server-id
      (error "qq: gray-tip notice requires NT message_id"))
    `((id . ,server-id)
      (server-id . ,server-id)
      (session-key . ,session-key)
      (time . ,time)
      (sender-id . ,sender-id)
      (sender-name . "QQ")
      (self-p . nil)
      (status . received)
      (gray-tip-p . t)
      (segments . (((type . "gray-tip") (data . ,data))))
      (raw-message . ,text)
      (preview . ,text)
      (message-type . "group")
      (group-id . ,group-id)
      (user-id . ,sender-id)
      (order . ,(qq-state--next-message-order))
      (raw-event . ,(copy-tree notice)))))

(defun qq-state--normalize-raw-message (message &optional expected-session-key)
  "Normalize raw OneBot MESSAGE into local store shape."
  (cond
   ((qq-state--poke-notice-p message)
    (qq-state--normalize-poke-notice message expected-session-key))
   ((qq-state--gray-tip-notice-p message)
    (qq-state--normalize-gray-tip-notice message expected-session-key))
   (t
    (let* ((chat-type (qq-state--normalize-id (qq-state--message-chat-type message)))
         (peer-uid (qq-state--message-peer-uid message))
         (peer-uin (qq-state--message-peer-uin message))
         (session-key (qq-state--raw-message-session-key
                       message expected-session-key))
         (session-identity
          (and session-key (qq-state-session-key-identity session-key)))
         (sender (alist-get 'sender message))
         (sender-id (qq-state--normalize-id
                     (or (alist-get 'user_id sender)
                         (alist-get 'user_id message))))
         (sender-id (if (and (qq-state--dataline-chat-type-p chat-type)
                             (equal sender-id "0"))
                        nil
                      sender-id))
         (sender-fields (qq-state--sender-display-fields session-key sender sender-id))
         (recalled-p (qq-state--raw-message-recalled-p message))
         (segments (if recalled-p '() (or (alist-get 'message message) '())))
         (mention-kinds (qq-state--mention-kinds-from-segments segments))
         (raw-message (if recalled-p
                          "[message recalled]"
                        (or (alist-get 'raw_message message)
                            (qq-state-message-preview-from-segments segments)
                            "")))
         ;; NapCat hard-cut: message_id is the NT snowflake string (never coerce
         ;; with string-to-number — snowflakes exceed fixnum precision).
         (server-id
          (qq-protocol-optional-message-id
           (or (alist-get 'message_id message)
               (alist-get 'id message))
           "message event"))
         (self-p (qq-state--message-self-p message))
         (status (cond
                  (recalled-p 'recalled)
                  (self-p 'sent)
                  (t 'received)))
         (time (qq-state--normalize-time (alist-get 'time message)))
         (target-id (alist-get 'target-id session-identity)))
    `((id . ,server-id)
      (server-id . ,server-id)
      (session-key . ,session-key)
      (time . ,time)
      (message-seq . ,(let ((sequence (alist-get 'message_seq message)))
                        (and (qq-protocol--nonzero-decimal-string-p sequence)
                             sequence)))
      (sender-id . ,sender-id)
      (sender-name . ,(alist-get 'sender-name sender-fields))
      (sender-secondary-name . ,(alist-get 'sender-secondary-name sender-fields))
      (sender-card . ,(alist-get 'sender-card sender-fields))
      (sender-nickname . ,(alist-get 'sender-nickname sender-fields))
      (sender-remark . ,(alist-get 'sender-remark sender-fields))
      (self-p . ,self-p)
      (status . ,status)
      (segments . ,segments)
      (mention-kinds . ,mention-kinds)
      (contains-mention-p . ,(and mention-kinds t))
      (raw-message . ,raw-message)
      (preview . ,(if recalled-p
                     "[message recalled]"
                   (qq-state-message-preview-from-segments segments)))
      (message-type . ,(alist-get 'message_type message))
      (chat-type . ,chat-type)
      (peer-uid . ,peer-uid)
      (peer-uin . ,peer-uin)
      (peer-name . ,(qq-state--present-string (alist-get 'peer_name message)))
      (group-id . ,(qq-state--normalize-id (alist-get 'group_id message)))
      (user-id . ,(qq-state--normalize-id (alist-get 'user_id message)))
      (target-id . ,target-id)
      ,@(when (assq 'emoji_likes_list message)
          `((reactions . ,(qq-state--normalize-reactions
                           (alist-get 'emoji_likes_list message)))))
      (order . ,(qq-state--next-message-order))
      (raw-event . ,(copy-tree message)))))))

(defun qq-state--emacs-search-chat-session-key (chat)
  "Return canonical group/private session key represented by closed CHAT."
  (unless (qq-protocol-emacs-chat-locator-p chat)
    (error "qq: message snapshot has invalid chat locator"))
  (pcase (alist-get 'kind chat)
    ("group" (qq-state-session-key 'group (alist-get 'group_id chat)))
    ("private" (qq-state-session-key 'private (alist-get 'user_id chat)))
    (_ (error "qq: message snapshot has unsupported chat locator"))))

(defun qq-state--emacs-video-segment-data (payload)
  "Map closed fork-native video PAYLOAD to the internal media shape."
  (let* ((remote (alist-get 'remote payload))
         (state (alist-get 'state remote)))
    `((file . ,(alist-get 'file payload))
      ,@(when (assq 'local_path payload)
          `((path . ,(alist-get 'local_path payload))))
      ,@(when (assq 'size payload)
          `((file_size . ,(alist-get 'size payload))))
      ,@(when (assq 'name payload)
          `((name . ,(alist-get 'name payload))))
      ,@(when (assq 'thumb payload)
          `((thumb . ,(alist-get 'thumb payload))))
      (remote_status . ,state)
      ,@(when (equal state "available")
          `((url . ,(alist-get 'url remote))))
      ,@(when (equal state "resolvable")
          `((resolver . ,(copy-tree (alist-get 'resolver remote))))))))

(defun qq-state--emacs-search-segment-to-internal (segment)
  "Map one validated fork-native search SEGMENT to the timeline model."
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (pcase kind
      ("video"
       `((type . "video")
         (data . ,(qq-state--emacs-video-segment-data payload))))
      ("reply"
       (let ((target (alist-get 'target payload)))
         (unless (equal (alist-get 'kind target) "unresolved")
           (error "qq: search snapshot reply must use an unresolved target"))
         `((type . "reply")
           (data
            . ,(if-let* ((message-id (alist-get 'message_id target)))
                   `((message_id . ,message-id))
                 nil)))))
      ("forward"
       `((type . "forward")
         (data . ((content . ,(copy-tree (alist-get 'content payload)))))))
      ("forward-card"
       `((type . "card")
         (data . ((kind . "forward")
                  (reference . ,(copy-tree (alist-get 'reference payload)))
                  (presentation . ,(copy-tree
                                     (alist-get 'presentation payload)))))))
      ("wallet"
       `((type . "wallet")
         (data . ,(copy-tree payload))))
      ("gray-tip"
       `((type . "gray-tip")
         (data . ((text . ,(alist-get 'text payload))
                  (kind . ,(alist-get 'gray_tip_kind payload))
                  (native-id . ,(alist-get 'native_id payload))))))
      ("unsupported"
       `((type . "__unsupported")
         (data . ((native_keys . ,(copy-tree
                                    (alist-get 'native_keys payload)))
                  (summary . ,(alist-get 'summary payload))))))
      (_
       `((type . ,kind) (data . ,(copy-tree payload)))))))

(defun qq-state--emacs-search-reaction-to-internal (reaction)
  "Map one validated fork-native search REACTION to the local model."
  `((emoji-id . ,(alist-get 'emoji_id reaction))
    (emoji-type . ,(alist-get 'emoji_type reaction))
    (count . ,(alist-get 'count reaction))
    (chosen-p . ,(eq (alist-get 'chosen reaction) t))))

(defun qq-state-normalize-message-snapshot (session-key snapshot)
  "Return normalized flat search SNAPSHOT for SESSION-KEY without storing it.

SNAPSHOT is the fork-native rendering projection, not an OB11 event.  This
decoder has one exact shape, emits no state event, and cannot widen the
canonical history cache."
  (qq-state-session-key-identity session-key)
  (unless (qq-protocol-emacs-message-search-result-p snapshot 'message)
    (error "qq: invalid closed message snapshot"))
  (let ((derived-session
         (qq-state--emacs-search-chat-session-key
          (alist-get 'chat snapshot))))
    (unless (equal derived-session session-key)
      (error "qq: message snapshot session %S contradicts expected session %S"
             derived-session session-key)))
  (let* ((server-id (alist-get 'message_id snapshot))
         (sequence (alist-get 'message_seq snapshot))
         (time (alist-get 'sent_at snapshot))
         (sender (alist-get 'sender snapshot))
         (sender-id (alist-get 'user_id sender))
         (sender-name (alist-get 'name sender))
         (self-p (eq (alist-get 'outgoing snapshot) t))
         (recalled-p (equal (alist-get 'state snapshot) "recalled"))
         (segments
          (if recalled-p
              nil
            (mapcar #'qq-state--emacs-search-segment-to-internal
                    (alist-get 'segments snapshot))))
         (mention-kinds (qq-state--mention-kinds-from-segments segments))
         (preview (if recalled-p
                      "[message recalled]"
                    (qq-state-message-preview-from-segments segments)))
         (identity (qq-state-session-key-identity session-key))
         (session-type (alist-get 'type identity)))
    ;; `order' only breaks ties inside the canonical cache.  Run the ordinary
    ;; allocator under a dynamic copy so private snapshots cannot consume it.
    (let ((qq-state--message-order-counter qq-state--message-order-counter))
      `((id . ,server-id)
        (server-id . ,server-id)
        (session-key . ,session-key)
        (time . ,time)
        (message-seq . ,sequence)
        (sender-id . ,sender-id)
        (sender-name . ,sender-name)
        (sender-secondary-name . nil)
        (sender-card . nil)
        (sender-nickname . ,sender-name)
        (sender-remark . nil)
        (self-p . ,self-p)
        (status . ,(cond (recalled-p 'recalled)
                         (self-p 'sent)
                         (t 'received)))
        (segments . ,segments)
        (mention-kinds . ,mention-kinds)
        (contains-mention-p . ,(and mention-kinds t))
        (raw-message . ,preview)
        (preview . ,preview)
        (message-type . ,(symbol-name session-type))
        (group-id . ,(and (eq session-type 'group)
                          (alist-get 'target-id identity)))
        (user-id . ,sender-id)
        (target-id . ,(alist-get 'target-id identity))
        (reactions
         . ,(mapcar #'qq-state--emacs-search-reaction-to-internal
                    (alist-get 'reactions snapshot)))
        (order . ,(qq-state--next-message-order))))))

(defun qq-state--pending-message (session-key segments &optional raw-message)
  "Return a local pending message for SESSION-KEY with SEGMENTS.

RAW-MESSAGE, when non-nil, becomes the optimistic raw-message field shown in the
chat timeline and used by weak pending-message matching."
  (let* ((local-id (format "local-%d" (cl-incf qq-state--local-message-counter)))
         (time (truncate (float-time)))
         (self-name (or (qq-state--first-present-string
                         (alist-get 'nickname qq-state--self-info))
                        (qq-state-self-user-id)
                        "me"))
         (segments (copy-tree (or segments '())))
         (preview (qq-state-message-preview-from-segments segments))
         (raw-message (or raw-message preview "")))
    `((id . ,local-id)
      (local-id . ,local-id)
      (session-key . ,session-key)
      (time . ,time)
      (sender-id . ,(qq-state-self-user-id))
      (sender-name . ,self-name)
      (sender-secondary-name . nil)
      (sender-card . nil)
      (sender-nickname . ,self-name)
      (sender-remark . nil)
      (self-p . t)
      (status . pending)
      (segments . ,segments)
      (raw-message . ,raw-message)
      (preview . ,preview)
      (order . ,(qq-state--next-message-order)))))

(defun qq-state--pending-text-message (session-key text &optional reply-to-message-id)
  "Return a local pending text message for SESSION-KEY with TEXT.

When REPLY-TO-MESSAGE-ID is non-nil, prepend a reply segment to the local
pending message model."
  (qq-state--pending-message
   session-key
   (append
    (when reply-to-message-id
      `(((type . "reply")
         (data . ((id . ,(format "%s" reply-to-message-id)))))))
    `(((type . "text")
       (data . ((text . ,text))))))
   text))

(defun qq-state--message-sort< (left right)
  "Return non-nil when LEFT should sort before RIGHT."
  (let ((left-time (qq-state--normalize-time (alist-get 'time left)))
        (right-time (qq-state--normalize-time (alist-get 'time right))))
    (if (/= left-time right-time)
        (< left-time right-time)
      (< (or (alist-get 'order left) 0)
         (or (alist-get 'order right) 0)))))

(defun qq-state--sort-messages (messages)
  "Return MESSAGES sorted from oldest to newest."
  (sort messages #'qq-state--message-sort<))

(defun qq-state--find-message (messages predicate)
  "Return first element in MESSAGES matching PREDICATE."
  (seq-find predicate messages))

(defun qq-state--direct-message-match (messages message)
  "Return direct match in MESSAGES for normalized MESSAGE."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (id (alist-get 'id message)))
    (qq-state--find-message
     messages
     (lambda (it)
       (or (and server-id (equal (alist-get 'server-id it) server-id))
           (and local-id (equal (alist-get 'local-id it) local-id))
           ;; Some protocol domains, notably QQ Guild forum feeds, own
           ;; stable opaque row identities which are neither message
           ;; snowflakes nor optimistic local ids.
           (and id
                (null server-id)
                (null local-id)
                (null (alist-get 'server-id it))
                (null (alist-get 'local-id it))
                (equal (alist-get 'id it) id)))))))

(defun qq-state--pending-segment-signature (segment)
  "Return stable optimistic reconciliation signature for one SEGMENT.

Only fields preserved across send payloads and self websocket events are used.
Media resource ids and paths are deliberately excluded because the kernel
rewrites them during upload; equal media-only sends are reconciled FIFO by the
surrounding message matcher."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment)))
    (pcase type
      ("text"
       (let ((text (alist-get 'text data)))
         (and (stringp text) (list type text))))
      ("at"
       (when-let* ((target (qq-state--normalize-id (alist-get 'qq data))))
         (list type target)))
      ("reply"
       (when-let* ((message-id (alist-get 'id data)))
         (list type message-id)))
      ("face"
       (when-let* ((face-id (qq-state--normalize-id (alist-get 'id data))))
         (list type face-id)))
      ("image"
       (list type (and (member (alist-get 'sub_type data) '(1 "1")) t)))
      ((or "mface" "video" "record" "file")
       (list type))
      (_ nil))))

(defun qq-state--pending-message-signature (message)
  "Return stable segment signature for optimistic reconciliation of MESSAGE."
  (when-let* ((segments (alist-get 'segments message))
              ((listp segments)))
    (let ((signature (mapcar #'qq-state--pending-segment-signature segments)))
      (and (not (memq nil signature)) signature))))

(defun qq-state--weak-pending-match (messages message)
  "Return weak pending match in MESSAGES for self-sent MESSAGE."
  (let ((self-p (alist-get 'self-p message))
        (raw-message (alist-get 'raw-message message))
        (signature (qq-state--pending-message-signature message))
        (time (qq-state--normalize-time (alist-get 'time message))))
    (when self-p
      (qq-state--find-message
       messages
       (lambda (it)
         (and (alist-get 'self-p it)
              (alist-get 'local-id it)
              ;; Weak matching is FIFO ownership among unresolved local rows.
              ;; A settled row is matched directly by server-id; admitting it
              ;; here lets the next equal media event overwrite the first.
              (eq (alist-get 'status it) 'pending)
              (null (alist-get 'server-id it))
              (let ((pending-signature
                     (qq-state--pending-message-signature it)))
                ;; Stable structure wins whenever both sides expose it.  Raw
                ;; preview text is only a compatibility path for unsupported
                ;; segment kinds; equal display names must not merge distinct
                ;; @targets, replies, or base faces.
                (if (and signature pending-signature)
                    (equal pending-signature signature)
                  (equal (alist-get 'raw-message it) raw-message)))
              (<= (abs (- time (qq-state--normalize-time (alist-get 'time it))))
                  qq-self-message-dedupe-window)))))))

(defun qq-state--poke-echo-match (messages message)
  "Return the nearest opposite-provenance poke echo for MESSAGE.

NapCat may deliver the websocket notice before or after the `send_poke'
response.  Match exactly one local/remote pair by actor, target and time while
leaving repeated pokes as distinct timeline records."
  (when (qq-state-poke-message-p message)
    (let* ((sender-id (alist-get 'sender-id message))
           (target-id (alist-get 'target-id message))
           (time (qq-state--normalize-time (alist-get 'time message)))
           (local-p (alist-get 'local-poke-p message))
           (candidates
            (seq-filter
             (lambda (it)
               (and (qq-state-poke-message-p it)
                    (not (alist-get 'poke-echo-reconciled-p it))
                    (not (eq (and (alist-get 'local-poke-p it) t)
                             (and local-p t)))
                    (equal (alist-get 'sender-id it) sender-id)
                    (equal (alist-get 'target-id it) target-id)
                    (<= (abs (- time
                                (qq-state--normalize-time
                                 (alist-get 'time it))))
                        qq-self-message-dedupe-window)))
             messages)))
      (car
       (sort candidates
             (lambda (left right)
               (< (abs (- time
                          (qq-state--normalize-time (alist-get 'time left))))
                  (abs (- time
                          (qq-state--normalize-time
                           (alist-get 'time right)))))))))))

(defun qq-state--index-message (message)
  "Refresh lookup indexes for MESSAGE."
  (let ((server-id (alist-get 'server-id message))
        (local-id (alist-get 'local-id message))
        (session-key (alist-get 'session-key message)))
    (when (and server-id session-key)
      (puthash server-id session-key qq-state--message-session-index))
    (when (and local-id session-key)
      (puthash local-id session-key qq-state--local-message-session-index))))

(defun qq-state--reindex-session-messages (session-key messages)
  "Rebuild indexes for SESSION-KEY using MESSAGES."
  (dolist (message messages)
    (when (equal (alist-get 'session-key message) session-key)
      (qq-state--index-message message))))

(defun qq-state--session-summary-position-compare
    (fields session &optional current-local-resolved-p)
  "Compare candidate summary FIELDS with current SESSION position.

Return -1, 0, or 1.  Compare server timestamps first and exact per-session
message sequences inside the same second.  NT message ids are identities, not
ordering keys: self and incoming messages can occupy incomparable id ranges.
Local insertion order and freshness tokens break otherwise unknown ties."
  (let ((candidate-id (alist-get 'last-message-id fields))
        (candidate-local-id (alist-get 'last-message-local-id fields))
        (candidate-seq (alist-get 'last-message-seq fields))
        (candidate-order (or (alist-get 'last-message-order fields) 0))
        (current-id (alist-get 'last-message-id session))
        (current-seq (alist-get 'last-message-seq session))
        (current-order (or (alist-get 'last-message-order session) 0))
        (candidate-time
         (qq-state--normalize-time (alist-get 'last-message-time fields)))
        (current-time
         (qq-state--normalize-time (alist-get 'last-message-time session))))
    (cond
     ((< candidate-time current-time) -1)
     ((> candidate-time current-time) 1)
     ((and (qq-protocol--nonzero-decimal-string-p candidate-seq)
           (qq-protocol--nonzero-decimal-string-p current-seq))
      (qq-protocol-decimal-string-compare candidate-seq current-seq))
     ;; The same canonical identity denotes the same frontier even when one
     ;; observation carries less sequence metadata than the other.
     ((and candidate-id (equal candidate-id current-id)) 0)
     ;; A promotion is the same optimistic row acquiring its server id.
     ((and (stringp current-id)
           (string-prefix-p "local-" current-id)
           (equal candidate-local-id current-id))
      0)
     ;; Once canonical storage proves that the current optimistic row acquired
     ;; a server id, its local frontier no longer shields equal-second server
     ;; messages.  The caller supplies the latest sequence/time candidate from
     ;; a cache which includes that promoted row.
     ((and current-local-resolved-p
           (qq-protocol--nonzero-decimal-string-p candidate-id)
           (stringp current-id)
           (string-prefix-p "local-" current-id))
      1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)
           (stringp current-id) (string-prefix-p "local-" current-id))
      (cond ((< candidate-order current-order) -1)
            ((> candidate-order current-order) 1)
            (t 0)))
     ;; Equal-second server snapshots cannot displace an incomparable local
     ;; optimistic row.  Conversely, a newly inserted local row follows the
     ;; last known server message even when their integer timestamps tie.
     ((and (stringp current-id) (string-prefix-p "local-" current-id)) -1)
     ((and (stringp candidate-id) (string-prefix-p "local-" candidate-id)) 1)
     ((< candidate-order current-order) -1)
     ((> candidate-order current-order) 1)
     (t 0))))

(defun qq-state--apply-session-summary
    (session-key fields &optional observation-token low-information-p
                 current-local-resolved-p)
  "Apply latest-message summary FIELDS to SESSION-KEY when still fresh.

OBSERVATION-TOKEN belongs to the request that observed FIELDS.  When omitted,
allocate a token for a synchronous local/live observation.  A candidate must
both be no older than the accepted message frontier and own a strictly newer
observation token.  LOW-INFORMATION-P preserves structured sender facts when a
fallback describes the already accepted exact message."
  (let* ((token (or observation-token
                    (qq-state-session-summary-observation-start)))
         (session (or (gethash session-key qq-state--sessions)
                      (qq-state--session-template session-key)))
         (current-token (or (alist-get 'last-message-summary-token session) 0))
         (position
          (qq-state--session-summary-position-compare
           fields session current-local-resolved-p)))
    (unless (and (integerp token) (> token 0))
      (error "qq: session summary observation token must be positive"))
    (when (and (> token current-token) (>= position 0))
      ;; A same-position fallback can improve preview text, but it has no
      ;; authority to erase sender facts learned from a structured message.
      ;; Clearing belongs only to a proven advance to a different frontier.
      (when (and low-information-p
                 (= position 0)
                 (equal (alist-get 'last-message-id fields)
                        (alist-get 'last-message-id session)))
        (setq fields
              (assq-delete-all
               'last-message-local-id
               (assq-delete-all
                'last-message-order
                (assq-delete-all
                 'last-message-self-p
                 (assq-delete-all 'last-message-sender-name
                                  (copy-tree fields)))))))
      (qq-state-upsert-session
       session-key
       (append (copy-tree fields)
               `((last-message-summary-token . ,token)))
       nil)
      t)))

(defun qq-state--message-summary-fields (message)
  "Return root latest-message summary fields for normalized MESSAGE."
  (let ((special-p (or (qq-state-poke-message-p message)
                       (qq-state-gray-tip-message-p message))))
    `((last-message-time
       . ,(qq-state--normalize-time (alist-get 'time message)))
      (last-message-id . ,(or (alist-get 'server-id message)
                              (alist-get 'id message)))
      (last-message-seq . ,(alist-get 'message-seq message))
      (last-message-local-id . ,(alist-get 'local-id message))
      (last-message-order . ,(alist-get 'order message))
      (last-message-preview . ,(qq-state-message-preview message))
      (last-message-sender-name
       . ,(unless special-p (alist-get 'sender-name message)))
      (last-message-self-p
       . ,(and (not special-p) (alist-get 'self-p message) t)))))

(defun qq-state--latest-summary-message (messages)
  "Return the newest summary candidate in normalized MESSAGES.

Do not inherit timeline list order here: canonical rendering deliberately uses
arrival order to break equal-second ties.  Root summary ownership uses server
time, then the exact per-session message sequence.  Local rows and messages
without sequence metadata fall back to explicit local insertion order."
  (let (latest)
    (dolist (message messages latest)
      (if (null latest)
          (setq latest message)
        (let* ((candidate-fields (qq-state--message-summary-fields message))
               (latest-fields (qq-state--message-summary-fields latest))
               (position
                (qq-state--session-summary-position-compare
                 candidate-fields latest-fields)))
          (when (or (> position 0)
                    (and (= position 0)
                         (> (or (alist-get 'order message) 0)
                            (or (alist-get 'order latest) 0))))
            (setq latest message)))))))

(defun qq-state--sync-session-summary (session-key &optional observation-token)
  "Sync timeline bounds and fresh latest summary for SESSION-KEY.

Keep sender metadata separate from the content preview.  Pokes and gray tips
already describe their actor or service meaning in their content, so they do
not project a root-row sender prefix.  OBSERVATION-TOKEN is captured before an
asynchronous materialization request; nil denotes a live/local observation."
  (let* ((messages (or (gethash session-key qq-state--messages-by-session) '()))
         (oldest (seq-find (lambda (it) (alist-get 'server-id it)) messages))
         (latest (qq-state--latest-summary-message messages))
         (session (gethash session-key qq-state--sessions))
         (current-id (and session (alist-get 'last-message-id session)))
         (current-local-resolved-p
          (and (stringp current-id)
               (string-prefix-p "local-" current-id)
               (seq-some
                (lambda (message)
                  (and (equal (alist-get 'local-id message) current-id)
                       (qq-protocol--nonzero-decimal-string-p
                        (alist-get 'server-id message))))
                messages))))
    (qq-state-upsert-session
     session-key
     `((oldest-message-id . ,(alist-get 'server-id oldest)))
     nil)
    (when latest
      (qq-state--apply-session-summary
       session-key (qq-state--message-summary-fields latest)
       observation-token nil current-local-resolved-p))))

(defun qq-state-session-messages (session-key)
  "Return cached messages for SESSION-KEY."
  (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))

(defun qq-state-session-oldest-message-id (session-key)
  "Return oldest server-backed message id for SESSION-KEY."
  (alist-get 'oldest-message-id (gethash session-key qq-state--sessions)))

(defun qq-state-insert-pending-message (session-key segments &optional raw-message)
  "Insert local pending SEGMENTS message into SESSION-KEY.

RAW-MESSAGE overrides the optimistic raw-message field when non-nil.  Return the
local message object."
  (let* ((message (qq-state--pending-message session-key segments raw-message))
         (messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '()))))
    (push message messages)
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (qq-state-upsert-session session-key nil nil)
    (qq-state--index-message message)
    (qq-state--sync-session-summary session-key)
    (qq-state--emit 'message
                    :session-key session-key
                    :message (copy-tree message)
                    :message-anchor (qq-state-message-anchor message)
                    :mutation 'create
                    :source 'local)
    message))

(defun qq-state-insert-pending-text-message (session-key text &optional reply-to-message-id)
  "Insert local pending TEXT message into SESSION-KEY.

When REPLY-TO-MESSAGE-ID is non-nil, include a local reply segment.
Return the local message object."
  (qq-state-insert-pending-message
   session-key
   (append
    (when reply-to-message-id
      `(((type . "reply")
         (data . ((id . ,(format "%s" reply-to-message-id)))))))
    `(((type . "text")
       (data . ((text . ,text))))))
   text))

(defun qq-state--replace-message (messages existing replacement)
  "Return MESSAGES with EXISTING replaced by REPLACEMENT."
  (mapcar (lambda (it) (if (eq it existing) replacement it)) messages))

(defun qq-state--message-patch-journal-key (session-key message-id)
  "Return the journal key for exact MESSAGE-ID in SESSION-KEY."
  (cons session-key message-id))

(defun qq-state-message-observation-token ()
  "Return the current ID-scoped message observation clock.

Callers which own a buffer-local request may capture this value before
dispatch and accept only later reaction patches.  Recall patches are
tombstones and do not require that freshness comparison."
  qq-state--message-observation-clock)

(defun qq-state--next-message-observation-token ()
  "Allocate the next ID-scoped message observation token."
  (cl-incf qq-state--message-observation-clock))

(defun qq-state-materialization-request-begin (session-key)
  "Begin a materialization request for SESSION-KEY and return its owner.

The returned opaque plist captures the current message observation clock.
Only reaction patches observed strictly after that clock may repair the
request's eventual snapshot."
  (qq-state-session-key-identity session-key)
  (let* ((id (cl-incf qq-state--materialization-request-counter))
         (owner (list :id id
                      :session-key session-key
                      :start-token qq-state--message-observation-clock)))
    (puthash id owner qq-state--materialization-request-owners)
    (copy-tree owner)))

(defun qq-state--materialization-request-current (owner &optional session-key)
  "Return registered OWNER when it is active and matches SESSION-KEY."
  (when (listp owner)
    (let* ((id (plist-get owner :id))
           (current (and (integerp id)
                         (gethash id
                                  qq-state--materialization-request-owners))))
      (when (and current
                 (equal current owner)
                 (or (null session-key)
                     (equal session-key (plist-get current :session-key))))
        current))))

(defun qq-state--reaction-patch-needed-p (session-key patch)
  "Return non-nil when an active SESSION-KEY owner may need PATCH."
  (let ((token (plist-get patch :observation-token))
        needed)
    (when (integerp token)
      (maphash
       (lambda (_id owner)
         (when (and (equal session-key (plist-get owner :session-key))
                    (< (plist-get owner :start-token) token))
           (setq needed t)))
       qq-state--materialization-request-owners))
    needed))

(defun qq-state--prune-reaction-patch-journal (&optional only-session-key)
  "Discard reaction patches no active request can observe.

When ONLY-SESSION-KEY is non-nil, inspect only entries in that session.
Recall tombstones are retained permanently."
  (let (updates removals)
    (maphash
     (lambda (key entry)
       (when (or (null only-session-key)
                 (equal only-session-key (car key)))
         (let* ((session-key (car key))
                (retained
                 (seq-filter
                  (lambda (patch)
                    (qq-state--reaction-patch-needed-p session-key patch))
                  (plist-get entry :reaction-patches)))
                (next (plist-put (copy-tree entry)
                                 :reaction-patches retained)))
           (if (or (plist-get next :recalled-p) retained)
               (push (cons key next) updates)
             (push key removals)))))
     qq-state--message-patch-journal)
    (dolist (update updates)
      (puthash (car update) (cdr update) qq-state--message-patch-journal))
    (dolist (key removals)
      (remhash key qq-state--message-patch-journal))))

(defun qq-state-materialization-request-end (owner)
  "End active materialization request OWNER.

Return non-nil only when OWNER was active.  Reaction patches are pruned once
no older request in the same session can still consume them."
  (when-let* ((current (qq-state--materialization-request-current owner))
              (id (plist-get current :id))
              (session-key (plist-get current :session-key)))
    (remhash id qq-state--materialization-request-owners)
    (qq-state--prune-reaction-patch-journal session-key)
    t))

(defun qq-state--journal-message-patch
    (session-key message-id patch)
  "Journal closed PATCH for MESSAGE-ID in SESSION-KEY when required.

Recall is stored as a tombstone.  A reaction patch is retained only when an
active request started before its observation token."
  (let* ((key (qq-state--message-patch-journal-key session-key message-id))
         (entry (copy-tree (gethash key qq-state--message-patch-journal))))
    (pcase (plist-get patch :kind)
      ('recall
       (setq entry (plist-put entry :recalled-p t)))
      ('emoji-like
       (when (qq-state--reaction-patch-needed-p session-key patch)
         (setq entry
               (plist-put
                entry :reaction-patches
                (append (plist-get entry :reaction-patches)
                        (list (copy-tree patch)))))))
      (kind (error "qq: unsupported message patch kind %S" kind)))
    (when (or (plist-get entry :recalled-p)
              (plist-get entry :reaction-patches))
      (puthash key entry qq-state--message-patch-journal))))

(defun qq-state--message-reaction-observation-token (message)
  "Return MESSAGE's latest materialized reaction observation token."
  (let ((token (alist-get 'reaction-observation-token message)))
    (and (integerp token) token)))

(defun qq-state--apply-observed-reaction-patch (message patch)
  "Apply reaction PATCH to MESSAGE and record its observation token.

The token is a per-canonical-message watermark.  It lets a later request
response replay only deltas that have not already reached that row."
  (let ((token (plist-get patch :observation-token))
        (updated (qq-state-message-apply-patch message patch)))
    (unless (integerp token)
      (error "qq: reaction patch requires an observation token"))
    (setf (alist-get 'reaction-observation-token updated nil nil #'eq) token)
    updated))

(defun qq-state--materialize-message-patches
    (session-key message &optional owner replay-retained-reactions-p)
  "Apply journaled notice state to normalized MESSAGE in SESSION-KEY.

Recall tombstones always apply.  Reaction patches apply only when OWNER is an
active request for SESSION-KEY and they were observed after OWNER started.
When REPLAY-RETAINED-REACTIONS-P is non-nil without an owner, catch a
canonical row up through every retained delta before applying a newer live
notice.  In both cases the row's reaction observation watermark prevents a
delta from being applied twice."
  (let* ((message-id (alist-get 'server-id message))
         (key (and message-id
                   (qq-state--message-patch-journal-key
                    session-key message-id)))
         (entry (and key
                     (copy-tree
                      (gethash key qq-state--message-patch-journal)))))
    (if (null entry)
        message
      (let* ((updated (copy-tree message))
             (current-owner
              (qq-state--materialization-request-current owner session-key))
             (start-token (and current-owner
                               (plist-get current-owner :start-token)))
             (replay-floor
              (cond (start-token start-token)
                    (replay-retained-reactions-p -1)))
             (applied-token
              (or (qq-state--message-reaction-observation-token updated) -1)))
        (when replay-floor
          (dolist (patch (plist-get entry :reaction-patches))
            (let ((token (plist-get patch :observation-token)))
              (when (and (integerp token)
                         (< replay-floor token)
                         (< applied-token token))
                (setq updated
                      (qq-state--apply-observed-reaction-patch updated patch))
                (setq applied-token token)))))
        (when (plist-get entry :recalled-p)
          (setq updated (qq-state--as-recalled-message updated)))
        updated))))

(defun qq-state--merge-normalized-message
    (session-key message &optional summary-observation-token)
  "Merge normalized MESSAGE into SESSION-KEY.

Unread state is deliberately not inferred from message delivery.  The Linux QQ
kernel's authoritative read-state snapshot is the only source of unread count
and position, including updates caused by another logged-in client.

Return three values via `cl-values':
1. merged local message object
2. mutation symbol `create' or `update'
3. previous timeline anchor when a pending local-id is promoted to server-id,
   else nil"
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (direct (qq-state--direct-message-match messages message))
         (poke-echo (and (null direct)
                         (qq-state--poke-echo-match messages message)))
         (existing (or direct
                       poke-echo
                       (qq-state--weak-pending-match messages message)))
         ;; If the real notice won the race, the later synthetic callback must
         ;; not replace its snowflake/raw_info with a local anchor.  In the
         ;; opposite order, merge normally so the local row is promoted.
         (merged (cond
                  ((and poke-echo
                        (alist-get 'local-poke-p message)
                        (not (alist-get 'local-poke-p existing)))
                   (copy-tree existing))
                  (existing
                   (qq-state--merge-alists existing message))
                  (t message)))
         (old-order (and existing (alist-get 'order existing)))
         (previous-anchor
          (and existing
               (let ((old-local (alist-get 'local-id existing))
                     (new-server (alist-get 'server-id merged)))
                 (and old-local
                      new-server
                      (not (equal old-local new-server))
                      (not (alist-get 'server-id existing))
                      old-local))))
         (mutation (if existing 'update 'create)))
    (when poke-echo
      (setf (alist-get 'poke-echo-reconciled-p merged nil nil #'eq) t))
    (when old-order
      (setf (alist-get 'order merged nil nil #'eq) old-order))
    ;; Keep recalled once known unless the incoming payload is itself recalled
    ;; (NapCat history may omit the flag if using stock; fork should set it).
    (when (and existing
               (qq-state-message-recalled-p existing)
               (not (qq-state-message-recalled-p merged)))
      (setq merged (qq-state--as-recalled-message merged)))
    (setq merged (qq-state--materialize-message-patches session-key merged))
    (if existing
        (setq messages (qq-state--replace-message messages existing merged))
      (push merged messages))
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (qq-state-upsert-session
     session-key
     (delq nil
           (list (and (alist-get 'peer-name merged)
                      (cons 'peer-name (alist-get 'peer-name merged)))
                 (and (alist-get 'peer-uid merged)
                      (cons 'peer-uid (alist-get 'peer-uid merged)))
                 (and (alist-get 'peer-uin merged)
                      (cons 'peer-uin (alist-get 'peer-uin merged)))))
     nil)
    (qq-state--index-message merged)
    (qq-state--sync-session-summary session-key summary-observation-token)
    (cl-values merged mutation previous-anchor)))

(defun qq-state-merge-live-message (message)
  "Merge live websocket MESSAGE into local state and return its session key."
  (let* ((normalized (qq-state--normalize-raw-message message))
         (session-key (alist-get 'session-key normalized)))
    (when session-key
      (cl-multiple-value-bind (merged mutation previous-anchor)
          (qq-state--merge-normalized-message
           session-key
           normalized)
        (when merged
          (apply #'qq-state--emit
                 'message
                 :session-key session-key
                 :message (copy-tree merged)
                 :message-anchor (qq-state-message-anchor merged)
                 :mutation mutation
                 :source 'event
                 (when previous-anchor
                   (list :previous-anchor previous-anchor))))))
    session-key))

(defun qq-state--normalize-guild-message (event)
  "Normalize one validated closed QQ channel message EVENT."
  (let* ((chat (alist-get 'chat event))
         (guild-id (alist-get 'guild_id chat))
         (channel-id (alist-get 'channel_id chat))
         (session-key
          (qq-state-guild-channel-session-key guild-id channel-id))
         (sender (alist-get 'sender event))
         (sender-id (or (alist-get 'user_id sender)
                        (alist-get 'native_id sender)))
         (sender-name (alist-get 'display_name sender))
         (recalled-p (equal (alist-get 'state event) "recalled"))
         (segments
          (unless recalled-p
            (mapcar #'qq-state--emacs-search-segment-to-internal
                    (alist-get 'segments event))))
         (mention-kinds (qq-state--mention-kinds-from-segments segments))
         (self-p (eq (alist-get 'outgoing event) t))
         (preview (if recalled-p
                      "[message recalled]"
                    (qq-state-message-preview-from-segments segments))))
    `((id . ,(alist-get 'message_id event))
      (server-id . ,(alist-get 'message_id event))
      (session-key . ,session-key)
      (time . ,(alist-get 'sent_at event))
      (message-seq . ,(alist-get 'message_sequence event))
      (sender-id . ,sender-id)
      (sender-native-id . ,(alist-get 'native_id sender))
      (sender-name . ,sender-name)
      (sender-secondary-name . nil)
      (sender-card . ,(qq-state--present-string
                       (alist-get 'member_name sender)))
      (sender-nickname . ,(qq-state--present-string
                           (alist-get 'nickname sender)))
      (sender-remark . nil)
      (self-p . ,self-p)
      (status . ,(cond (recalled-p 'recalled)
                       (self-p 'sent)
                       (t 'received)))
      (segments . ,segments)
      (mention-kinds . ,mention-kinds)
      (contains-mention-p . ,(and mention-kinds t))
      (raw-message . ,preview)
      (preview . ,preview)
      (message-type . "guild-channel")
      (chat-type . "4")
      (peer-uid . ,channel-id)
      (peer-name . ,(qq-state--present-string
                     (alist-get 'channel_name event)))
      (guild-id . ,guild-id)
      (channel-id . ,channel-id)
      (user-id . ,(alist-get 'user_id sender))
      (target-id . ,channel-id)
      (order . ,(qq-state--next-message-order))
      (raw-event . ,(copy-tree event)))))

(defun qq-state-merge-guild-message (event)
  "Merge validated closed QQ channel message EVENT into local state."
  (let* ((normalized (qq-state--normalize-guild-message event))
         (session-key (alist-get 'session-key normalized)))
    (cl-multiple-value-bind (merged mutation previous-anchor)
        (qq-state--merge-normalized-message session-key normalized)
      (when merged
        (apply #'qq-state--emit
               'message
               :session-key session-key
               :message (copy-tree merged)
               :message-anchor (qq-state-message-anchor merged)
               :mutation mutation
               :source 'event
               (when previous-anchor
                 (list :previous-anchor previous-anchor)))))
    session-key))

(defun qq-state--normalize-guild-forum-post (post)
  "Normalize one validated closed QQ Guild forum POST."
  (let* ((chat (alist-get 'chat post))
         (guild-id (alist-get 'guild_id chat))
         (channel-id (alist-get 'channel_id chat))
         (session-key
          (qq-state-guild-channel-session-key guild-id channel-id))
         (sender (alist-get 'sender post))
         (deleted-p (equal (alist-get 'state post) "deleted"))
         (segments
          (if deleted-p
              '(((type . "text") (data . ((text . "[post deleted]")))))
            (mapcar #'qq-state--emacs-search-segment-to-internal
                    (alist-get 'segments post))))
         (segment-preview (qq-state-message-preview-from-segments segments))
         (title (alist-get 'title post))
         (preview (if (string-empty-p title) segment-preview title)))
    `((id . ,(alist-get 'post_id post))
      ;; `server-id' is intentionally absent.  Forum post ids are opaque
      ;; Feed identities, not NT message snowflakes.
      (session-key . ,session-key)
      (time . ,(alist-get 'created_at post))
      (message-seq . nil)
      (sender-id . ,(alist-get 'native_id sender))
      (sender-native-id . ,(alist-get 'native_id sender))
      (sender-name . ,(alist-get 'display_name sender))
      (sender-secondary-name . nil)
      (sender-card . nil)
      (sender-nickname . nil)
      (sender-remark . nil)
      (sender-avatar-url . ,(alist-get 'avatar_url sender))
      (self-p . nil)
      (status . received)
      (segments . ,segments)
      (mention-kinds . nil)
      (contains-mention-p . nil)
      (raw-message . ,preview)
      (preview . ,preview)
      (message-type . "guild-forum-post")
      (chat-type . "4")
      (peer-uid . ,channel-id)
      (peer-name . ,(qq-state--present-string
                     (alist-get 'channel_name post)))
      (guild-id . ,guild-id)
      (channel-id . ,channel-id)
      (user-id . nil)
      (target-id . ,channel-id)
      (forum-title . ,title)
      (forum-comment-count . ,(alist-get 'comment_count post))
      (forum-updated-at . ,(alist-get 'updated_at post))
      (order . ,(qq-state--next-message-order))
      (raw-event . ,(copy-tree post)))))

(defun qq-state-merge-guild-forum-post (post)
  "Merge validated closed QQ Guild forum POST into local state."
  (let* ((normalized (qq-state--normalize-guild-forum-post post))
         (session-key (alist-get 'session-key normalized)))
    (cl-multiple-value-bind (merged mutation previous-anchor)
        (qq-state--merge-normalized-message session-key normalized)
      (when merged
        (apply #'qq-state--emit
               'message
               :session-key session-key
               :message (copy-tree merged)
               :message-anchor (qq-state-message-anchor merged)
               :mutation mutation
               :source 'response
               (when previous-anchor
                 (list :previous-anchor previous-anchor)))))
    session-key))

(defun qq-state-replace-guild-forum-posts (session-key posts)
  "Replace SESSION-KEY history with validated first-page forum POSTS.

The first native Feed page is an authoritative snapshot.  Replacing the
cache also removes legacy sequence-range rows which represented forum
activity notifications rather than posts."
  (let ((old-messages (gethash session-key qq-state--messages-by-session))
        (normalized
         (mapcar #'qq-state--normalize-guild-forum-post posts)))
    (dolist (message old-messages)
      (when-let* ((server-id (alist-get 'server-id message)))
        (remhash server-id qq-state--message-session-index))
      (when-let* ((local-id (alist-get 'local-id message)))
        (remhash local-id qq-state--local-message-session-index)))
    (dolist (message normalized)
      (unless (equal (alist-get 'session-key message) session-key)
        (error "qq: forum first page contains a contradictory session")))
    (setq normalized (qq-state--sort-messages normalized))
    (puthash session-key normalized qq-state--messages-by-session)
    (qq-state--sync-session-summary session-key)
    (qq-state--emit 'history
                    :session-key session-key
                    :messages (copy-tree normalized)
                    :mutation 'history
                    :source 'response)
    session-key))

(defun qq-state-apply-guild-navigation (navigation)
  "Apply validated authoritative Guild NAVIGATION to its channel session."
  (let* ((chat (alist-get 'chat navigation))
         (session-key
          (qq-state-guild-channel-session-key
           (alist-get 'guild_id chat)
           (alist-get 'channel_id chat)))
         (unread (alist-get 'unread_count navigation))
         (begin (alist-get 'begin_sequence navigation))
         (first-sequence (and (> unread 0)
                              (not (equal begin "0"))
                              begin)))
    (qq-state-upsert-session
     session-key
     `((unread-count . ,unread)
       (first-unread-message-id . nil)
       (first-unread-message-seq . ,first-sequence)
       (read-position-available . nil)
       (guild-navigation-sequences
        . ,(copy-tree (alist-get 'navigation_sequences navigation))))
     nil)
    (qq-state--emit 'session
                    :session-key session-key
                    :session (qq-state-session session-key)
                    :mutation 'read)
    session-key))

(defun qq-state-apply-poke-notice (notice)
  "Append a local timeline message for OneBot NOTIFY/POKE NOTICE.

NapCat reports pokes as notices rather than ordinary messages.  The notice has
no sender display object; use the fork's closed native recall reference for
authoritative identity, and a local event anchor only for an optimistic local
echo.  Numeric participant IDs remain display labels of last resort."
  (let* ((message (qq-state--normalize-poke-notice notice))
         (session-key (alist-get 'session-key message)))
    (when session-key
      (cl-multiple-value-bind (merged mutation previous-anchor)
          (qq-state--merge-normalized-message session-key message)
        (when merged
          (apply #'qq-state--emit
                 'message
                 :session-key session-key
                 :message (copy-tree merged)
                 :message-anchor (qq-state-message-anchor merged)
                 :mutation mutation
                 :source 'notice
                 (when previous-anchor
                   (list :previous-anchor previous-anchor)))
          merged)))))

(defun qq-state-apply-gray-tip-notice (notice)
  "Merge fork-native JSON gray-tip NOTICE into its group timeline."
  (let* ((message (qq-state--normalize-gray-tip-notice notice))
         (session-key (alist-get 'session-key message)))
    (cl-multiple-value-bind (merged mutation previous-anchor)
        (qq-state--merge-normalized-message session-key message)
      (when merged
        (apply #'qq-state--emit
               'message
               :session-key session-key
               :message (copy-tree merged)
               :message-anchor (qq-state-message-anchor merged)
               :mutation mutation
               :source 'notice
               (when previous-anchor
                 (list :previous-anchor previous-anchor)))
        merged))))

(defun qq-state-merge-history (session-key raw-messages &optional request-owner)
  "Merge RAW-MESSAGES history batch into SESSION-KEY.

Recalled rows from NapCat (`recalled'/`recall_time') are stored as stubs so
`qq-chat-show-recalled-messages' can optionally show them.

REQUEST-OWNER, when non-nil, is the active owner returned before this history
request was dispatched.  Only reaction patches observed after that owner
started may repair an explicit reaction snapshot in the response.

Return a plist:
  :session-key, :message-count (batch size), :added-count (new server ids),
  :oldest-message-id (after merge).  Chat uses `:added-count' to detect
  beginning-of-history when NapCat returns only already-cached rows."
  (qq-state-upsert-session session-key nil nil)
  (let* ((messages
          (copy-tree
           (or (gethash session-key qq-state--messages-by-session) '())))
         (server-cells (make-hash-table :test #'equal))
         (local-cells (make-hash-table :test #'equal))
         (added 0)
         batch-ids
         session-fields
         (batch (or raw-messages '())))
    ;; Keep cons cells as O(1) replacement handles.  A history page is one
    ;; store transaction: normalize and merge every row, then sort/index/sync
    ;; the session exactly once.
    (let ((tail messages))
      (while tail
        (let* ((message (car tail))
               (server-id (alist-get 'server-id message))
               (local-id (alist-get 'local-id message)))
          (when server-id (puthash server-id tail server-cells))
          (when local-id (puthash local-id tail local-cells)))
        (setq tail (cdr tail))))
    (dolist (raw-message batch)
      (let* ((normalized
              (qq-state--normalize-raw-message raw-message session-key))
             (server-id (alist-get 'server-id normalized))
             (direct-cell (and server-id (gethash server-id server-cells)))
             (pending
              (and (null direct-cell)
                   (qq-state--weak-pending-match messages normalized)))
             (cell (or direct-cell
                       (and pending
                            (gethash (alist-get 'local-id pending)
                                     local-cells))))
             (existing (and cell (car cell)))
             (explicit-reactions-p (and (assq 'emoji_likes_list raw-message)
                                        t))
             (merged (if existing
                         (qq-state--merge-alists existing normalized)
                       normalized))
             (old-order (and existing (alist-get 'order existing))))
        (when server-id
          (push server-id batch-ids))
        (when old-order
          (setf (alist-get 'order merged nil nil #'eq) old-order))
        (when (and existing
                   (qq-state-message-recalled-p existing)
                   (not (qq-state-message-recalled-p merged)))
          (setq merged (qq-state--as-recalled-message merged)))
        ;; An explicit reaction snapshot replaces the previous canonical base.
        ;; Reset its watermark to the request's observation boundary so only
        ;; later journaled deltas are replayed.  Without a valid owner there is
        ;; no freshness proof, so do not preserve a watermark from the row that
        ;; the snapshot just replaced.
        (when explicit-reactions-p
          (setq merged
                (assq-delete-all 'reaction-observation-token merged))
          (when-let* ((current-owner
                       (qq-state--materialization-request-current
                        request-owner session-key))
                      (start-token (plist-get current-owner :start-token)))
            (setf (alist-get 'reaction-observation-token merged nil nil #'eq)
                  start-token)))
        (setq merged
              (qq-state--materialize-message-patches
               session-key merged request-owner))
        (if cell
            (setcar cell merged)
          (push merged messages)
          (setq cell messages))
        (when (and server-id (null direct-cell))
          (cl-incf added)
          (puthash server-id cell server-cells))
        (when-let* ((local-id (alist-get 'local-id merged)))
          (puthash local-id cell local-cells))
        (dolist (key '(peer-name peer-uid peer-uin))
          (when-let* ((value (alist-get key merged)))
            (setf (alist-get key session-fields nil nil #'eq) value)))))
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (when session-fields
      (qq-state-upsert-session session-key session-fields nil))
    (qq-state--reindex-session-messages session-key messages)
    (qq-state--sync-session-summary session-key)
    (setq batch-ids (delete-dups (nreverse batch-ids)))
    (let ((oldest (qq-state-session-oldest-message-id session-key))
          (count (length batch)))
      (qq-state--emit 'history
                      :session-key session-key
                      :message-count count
                      :added-count added
                      :oldest-message-id oldest
                      :batch-message-ids batch-ids
                      :batch-oldest-message-id (car batch-ids)
                      :batch-newest-message-id (car (last batch-ids))
                      :mutation 'history)
      (list :session-key session-key
            :message-count count
            :added-count added
            :oldest-message-id oldest
            :batch-message-ids batch-ids
            :batch-oldest-message-id (car batch-ids)
            :batch-newest-message-id (car (last batch-ids))))))

(defun qq-state-mark-pending-message-sent
    (session-key local-id message-id &optional request-owner)
  "Mark local pending message LOCAL-ID as sent with MESSAGE-ID in SESSION-KEY.

MESSAGE-ID is the NapCat NT snowflake string (`message_id' in the protocol
hard-cut).  It is stored as `server-id' and becomes the chat timeline anchor.
REQUEST-OWNER is the owner captured before dispatching the send request."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id))))
         (normalized-id
          (qq-protocol-optional-message-id message-id "send_msg response")))
    (when (and existing normalized-id)
      (let ((updated (qq-state--merge-alists
                      existing
                      `((id . ,normalized-id)
                        (server-id . ,normalized-id)
                        (status . sent)
                        (error . nil)))))
        (setq updated
              (qq-state--materialize-message-patches
               session-key updated request-owner))
        (setq messages (qq-state--replace-message messages existing updated))
        (setq messages (qq-state--sort-messages messages))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :previous-anchor local-id
                        :mutation 'update
                        :source 'response)
        updated))))

(defun qq-state-mark-pending-message-failed (session-key local-id reason)
  "Mark a still-pending LOCAL-ID as failed with REASON in SESSION-KEY.

Return the updated message only when a transition happened.  A server-backed
or otherwise settled row is authoritative and must not be downgraded by a late
transport timeout."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id)))))
    (when (and existing
               (eq (alist-get 'status existing) 'pending)
               (null (alist-get 'server-id existing)))
      (let ((updated (qq-state--merge-alists
                      existing
                      `((status . failed)
                        (error . ,reason)))))
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :mutation 'update
                        :source 'response)
        updated))))

(defun qq-state-set-session-unread (session-key count)
  "Set SESSION-KEY unread-count to COUNT and emit `:mutation' `read'.

COUNT is clamped to a non-negative integer.  Views treat this mutation as a
read-state change (header-line + optional unread divider), not a full
timeline rebuild."
  (let ((n (max 0 (if (integerp count) count (truncate (or count 0))))))
    (qq-state-upsert-session
     session-key
     `((unread-count . ,n)
       ,@(when (zerop n)
           '((unread-at-me-message-id . nil)
             (unread-at-me-message-seq . nil)
             (unread-at-all-message-id . nil)
             (unread-at-all-message-seq . nil))))
     nil)
    (qq-state--emit 'session
                    :session-key session-key
                    :session (qq-state-session session-key)
                    :mutation 'read)
    n))

(defun qq-state-clear-session-unread (session-key)
  "Reset unread count for SESSION-KEY."
  (qq-state-set-session-unread session-key 0))

(defconst qq-state--session-read-projection-keys
  '(unread-count
    first-unread-message-id first-unread-message-seq
    unread-at-me-message-id unread-at-me-message-seq
    unread-at-all-message-id unread-at-all-message-seq
    read-position-available read-latest-message-id)
  "Session fields written by an authoritative kernel read state.")

(defun qq-state--session-read-projection (session)
  "Return the authoritative read projection of SESSION."
  (mapcar (lambda (key) (cons key (alist-get key session)))
          qq-state--session-read-projection-keys))

(defun qq-state-apply-session-read-state (session-key read-state)
  "Apply kernel READ-STATE to SESSION-KEY and emit a read mutation.

READ-STATE is the payload from NapCat `emacs_get_read_state'.  Message ids and
sequences remain strings; in particular, the NT snowflake message id is never
coerced to an Emacs number."
  (let* ((raw-count (alist-get 'unread_count read-state))
         (count (max 0 (if (numberp raw-count)
                           (truncate raw-count)
                         (string-to-number (format "%s" (or raw-count 0))))))
         (first (alist-get 'first_unread read-state))
         (latest (alist-get 'latest read-state))
         (first-id
          (qq-protocol-optional-message-id
           (and (listp first) (alist-get 'message_id first))
           "read state"))
         (first-seq (qq-state--normalize-id
                     (and (listp first) (alist-get 'sequence first))))
         (latest-id
          (qq-protocol-optional-message-id
           (and (listp latest) (alist-get 'message_id latest))
           "read state"))
         (mentions (alist-get 'mentions read-state))
         (at-me (and (listp mentions) (alist-get 'at_me mentions)))
         (at-all (and (listp mentions) (alist-get 'at_all mentions)))
         (has-unread (> count 0))
         (available (and has-unread first-id))
         (projection
          `((unread-count . ,count)
            (first-unread-message-id . ,(and has-unread first-id))
            (first-unread-message-seq . ,(and has-unread first-seq))
            (unread-at-me-message-id
             . ,(and has-unread (alist-get 'message_id at-me)))
            (unread-at-me-message-seq
             . ,(and has-unread (alist-get 'sequence at-me)))
            (unread-at-all-message-id
             . ,(and has-unread (alist-get 'message_id at-all)))
            (unread-at-all-message-seq
             . ,(and has-unread (alist-get 'sequence at-all)))
            (read-position-available . ,(and available t))
            (read-latest-message-id . ,latest-id)))
         (existing (qq-state-session session-key)))
    (unless (and existing
                 (equal (qq-state--session-read-projection existing)
                        projection))
      (qq-state-upsert-session session-key projection nil)
      (qq-state--emit 'session
                      :session-key session-key
                      :session (qq-state-session session-key)
                      :mutation 'read))
    (qq-state-session session-key)))

(defun qq-state-validate-message-session (session-key message-id)
  "Reject known SESSION-KEY contradictions for exact MESSAGE-ID.

Return the validated NT snowflake string.  This cache-index check is not an
ownership proof for an unknown id; outbound mutations must carry a closed
locator-qualified message reference."
  (qq-state-session-key-identity session-key)
  (setq message-id
        (or (qq-protocol-optional-message-id
             message-id "message/session validation")
            (error "qq: message/session validation requires message_id")))
  (let ((indexed (gethash message-id qq-state--message-session-index)))
    (when (and indexed (not (equal indexed session-key)))
      (error "qq: message patch session %s contradicts indexed session %s"
             session-key indexed)))
  message-id)

(defun qq-state-apply-recall (session-key message-id)
  "Mark exact MESSAGE-ID in explicit SESSION-KEY as recalled.

Keeps the row so the chat view can hide it (default) or show a stub when
`qq-chat-show-recalled-messages' is non-nil.  The explicit session lets an
ID-only patch also update a materialized snapshot outside canonical history.
Return the updated canonical message, if one was present."
  (let* ((normalized-id
          (qq-state-validate-message-session session-key message-id))
         (messages (and session-key
                        (copy-tree (or (gethash session-key qq-state--messages-by-session) '()))))
         (existing (and messages
                        (qq-state--find-message
                         messages
                         (lambda (it)
                           (equal (alist-get 'server-id it) normalized-id)))))
         (observation-token (qq-state--next-message-observation-token))
         (patch (list :kind 'recall
                      :observation-token observation-token)))
    (cond
     ((and session-key existing)
      (let* ((materialized
              (qq-state--materialize-message-patches
               session-key existing))
             (updated (qq-state-message-apply-patch materialized patch)))
        (qq-state--journal-message-patch
         session-key normalized-id patch)
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :mutation 'update
                        :source 'notice
                        :observation-token observation-token
                        :message-patch patch)
        updated))
     ((and session-key normalized-id)
      ;; Filter projections deliberately do not register in the canonical
      ;; message index.  Journal the patch for later canonical materialization
      ;; and publish it so the private projection updates immediately.
      (qq-state--journal-message-patch
       session-key normalized-id patch)
      (qq-state--emit 'message
                      :session-key session-key
                      :message-anchor normalized-id
                      :mutation 'update
                      :source 'notice
                      :observation-token observation-token
                      :message-patch patch)
      nil))))

(defun qq-state--reaction-with-notice (reactions like is-add own-operation-p)
  "Return REACTIONS after applying one emoji LIKE notice.

IS-ADD identifies add versus remove.  OWN-OPERATION-P updates the local
`chosen-p' flag; reactions by other users leave that flag unchanged."
  (let* ((emoji-id (qq-state--normalize-id
                    (or (alist-get 'emoji_id like)
                        (alist-get 'emojiId like))))
         (existing (and emoji-id
                        (seq-find
                         (lambda (reaction)
                           (equal (alist-get 'emoji-id reaction) emoji-id))
                         reactions))))
    (if (null emoji-id)
        reactions
      (let* ((has-count (or (assq 'count like) (assq 'likes_cnt like)))
             (old-count (qq-state--normalize-reaction-count
                         (alist-get 'count existing)))
             (already-applied-p
              (and own-operation-p
                   existing
                   (eq (and (alist-get 'chosen-p existing) t)
                       (and is-add t))))
             (next-count
              (if has-count
                  (qq-state--normalize-reaction-count
                   (or (alist-get 'count like) (alist-get 'likes_cnt like)))
                (if already-applied-p
                    old-count
                  (max 0 (+ old-count (if is-add 1 -1))))))
             (emoji-type
              (qq-state--normalize-id
               (or (alist-get 'emoji_type like)
                   (alist-get 'emojiType like)
                   (alist-get 'emoji-type existing)
                   (qq-state--infer-reaction-emoji-type emoji-id))))
             (chosen-p (if own-operation-p
                           is-add
                         (and existing (alist-get 'chosen-p existing))))
             (next-item `((emoji-id . ,emoji-id)
                          (emoji-type . ,emoji-type)
                          (count . ,next-count)
                          (chosen-p . ,(and chosen-p t))))
             (next nil)
             (replaced nil))
        (dolist (reaction reactions)
          (if (equal (alist-get 'emoji-id reaction) emoji-id)
              (progn
                (setq replaced t)
                (when (> next-count 0)
                  (push next-item next)))
            (push reaction next)))
        (when (and (not replaced) (> next-count 0))
          (push next-item next))
        (nreverse next)))))

(defun qq-state--message-with-emoji-like-notice (message notice)
  "Return a copy of MESSAGE after applying emoji-like NOTICE."
  (let* ((is-add (qq-protocol-json-true-p (alist-get 'is_add notice)))
         (operator-id (qq-state--normalize-id (alist-get 'user_id notice)))
         (own-operation-p
          (and operator-id
               (equal operator-id (qq-state-self-user-id))))
         (reactions (copy-tree (qq-state-message-reactions message))))
    (dolist (like (or (alist-get 'likes notice) '()))
      (setq reactions
            (qq-state--reaction-with-notice
             reactions like is-add own-operation-p)))
    (let ((updated (copy-tree message)))
      (setf (alist-get 'reactions updated nil nil #'eq) reactions)
      updated)))

(defun qq-state-message-apply-patch (message patch)
  "Return a copy of normalized MESSAGE after applying closed PATCH.

This pure boundary is shared by canonical storage and buffer-owned filtered
snapshots.  PATCH is a plist whose `:kind' is `recall' or `emoji-like'."
  (unless (listp message)
    (error "qq: message patch requires a normalized message"))
  (pcase (plist-get patch :kind)
    ('recall (qq-state--as-recalled-message message))
    ('emoji-like
     (let ((notice (plist-get patch :notice)))
       (unless (listp notice)
         (error "qq: emoji-like message patch requires a notice"))
       (qq-state--message-with-emoji-like-notice message notice)))
    (kind (error "qq: unsupported message patch kind %S" kind))))

(defun qq-state-apply-emoji-like-notice (session-key notice)
  "Apply group emoji-like NOTICE in explicit SESSION-KEY.

The notice `count' is treated as the authoritative aggregate when present;
notices without an aggregate count are applied as a one-step delta.  The
explicit session scopes filter-owned snapshots when the message is absent from
canonical history."
  (let* ((message-id
          (or (qq-protocol-optional-message-id
               (alist-get 'message_id notice)
               "group_msg_emoji_like notice")
              (error "qq: emoji-like notice requires an exact message_id")))
         (group-id (qq-state--normalize-id (alist-get 'group_id notice)))
         (identity (qq-state-session-key-identity session-key))
         (_group-session
          (unless (eq (alist-get 'type identity) 'group)
            (error "qq: emoji-like patch requires a group session")))
         (_group-id
          (unless (and group-id
                       (equal group-id (alist-get 'target-id identity)))
            (error "qq: emoji-like notice requires the explicit session group")))
         (message-id
          (qq-state-validate-message-session session-key message-id))
         (messages (and session-key
                        (copy-tree
                         (or (gethash session-key qq-state--messages-by-session)
                             '()))))
         (existing
          (and message-id messages
               (qq-state--find-message
                messages
                (lambda (message)
                  (equal (alist-get 'server-id message) message-id)))))
         (observation-token (qq-state--next-message-observation-token))
         (patch (list :kind 'emoji-like
                      :observation-token observation-token
                      :notice (copy-tree notice))))
    (cond
     ((and session-key existing)
      (let* ((materialized
              (qq-state--materialize-message-patches
               ;; Fold any older retained deltas first.  This keeps the
               ;; per-message watermark contiguous when a first notice arrived
               ;; before the live message and this newer notice arrived after.
               session-key existing nil t))
             (updated
              (qq-state--apply-observed-reaction-patch materialized patch)))
        (qq-state--journal-message-patch
         session-key message-id patch)
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--index-message updated)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor message-id
                        :mutation 'update
                        :source 'notice
                        :observation-token observation-token
                        :message-patch patch)
        updated))
     ((and session-key message-id)
      (qq-state--journal-message-patch
       session-key message-id patch)
      (qq-state--emit 'message
                      :session-key session-key
                      :message-anchor message-id
                      :mutation 'update
                      :source 'notice
                      :observation-token observation-token
                      :message-patch patch)
      nil))))

(defun qq-state--recent-contact-title (contact session-key)
  "Return display title for recent CONTACT in SESSION-KEY."
  (let ((peer-name (alist-get 'peerName contact))
        (remark (alist-get 'remark contact))
        (chat-type (alist-get 'chatType contact)))
    (or (and (stringp remark) (not (string-empty-p remark)) remark)
        (and (stringp peer-name) (not (string-empty-p peer-name)) peer-name)
        (and (qq-state--dataline-chat-type-p chat-type) "我的手机")
        (qq-state--default-session-title (qq-state--session-template session-key))
        (qq-state-session-key-target-id session-key))))

(defun qq-state--recent-contact-session-key (contact)
  "Return CONTACT's canonical session key, or nil when unsupported.

Only exact native chat types are accepted.  DataLine and service identities
require the original non-empty `peerUid'; `peerUin' is never a substitute."
  (let ((chat-type (qq-state--normalize-id (alist-get 'chatType contact))))
    (pcase chat-type
      ("1"
       (when-let* ((peer-uin (alist-get 'peerUin contact)))
         (qq-state-session-key 'private peer-uin)))
      ("2"
       (when-let* ((peer-uin (alist-get 'peerUin contact)))
         (qq-state-session-key 'group peer-uin)))
      ((or "8" "134")
       (when-let* ((peer-uid (alist-get 'peerUid contact))
                   ((stringp peer-uid))
                   ((not (string-empty-p peer-uid))))
         (qq-state-session-key
          'dataline peer-uid
          (qq-state--dataline-variant-from-chat-type chat-type))))
      ("103"
       (when-let* ((peer-uid (alist-get 'peerUid contact))
                   ((stringp peer-uid))
                   ((not (string-empty-p peer-uid))))
         (qq-state-session-key 'service peer-uid)))
      (_ nil))))

(defun qq-state--prepare-recent-contact (contact)
  "Normalize CONTACT without mutating session or timeline stores.

Return a closed prepared plist, or nil for an unsupported native chat type.
In particular, a structured `lastestMsg' is fully normalized before any of
its session metadata can be committed."
  (when-let* ((session-key (qq-state--recent-contact-session-key contact)))
    (let* ((identity (qq-state-session-key-identity session-key))
           (chat-type (alist-get 'chat-type identity))
           (peer-uid (alist-get 'peer-uid identity))
           (peer-uin (qq-state--normalize-id (alist-get 'peerUin contact)))
           (target-id (alist-get 'target-id identity))
           (msg-time (qq-state--normalize-time (alist-get 'msgTime contact)))
           (msg-id (qq-state--normalize-id (alist-get 'msgId contact)))
           (msg-seq (alist-get 'msgSeq contact))
           (last-message (alist-get 'lastestMsg contact))
           (message-copy (and (consp last-message) (copy-tree last-message)))
           (unread-entry (assq 'unreadCount contact))
           (disturb-entry (assq 'isMsgDisturb contact))
           (notify-mode-entry (assq 'messageNotifyMode contact)))
      (unless (qq-protocol--decimal-string-p msg-seq)
        (error "qq: recent contact requires exact decimal msgSeq"))
      (when message-copy
        (when-let* ((embedded-seq
                     (alist-get 'message_seq message-copy nil nil #'eq)))
          (unless (and (stringp embedded-seq)
                       (equal embedded-seq msg-seq))
            (error "qq: recent contact msgSeq disagrees with latest message")))
        (when (and msg-id
                   (not (alist-get 'message_id message-copy nil nil #'eq))
                   (not (alist-get 'id message-copy nil nil #'eq)))
          (push (cons 'message_id msg-id) message-copy))
        (when (and msg-seq
                   (not (alist-get 'message_seq message-copy nil nil #'eq)))
          (push (cons 'message_seq msg-seq) message-copy))
        (when (and (> msg-time 0)
                   (not (alist-get 'time message-copy nil nil #'eq)))
          (push (cons 'time msg-time) message-copy)))
      (list
       :session-key session-key
       :metadata-fields
       `((title . ,(qq-state--recent-contact-title contact session-key))
         (target-id . ,target-id)
         (chat-type . ,(qq-state--normalize-id chat-type))
         (peer-uid . ,peer-uid)
         (peer-uin . ,peer-uin)
         (peer-name . ,(qq-state--present-string (alist-get 'peerName contact)))
         (remark . ,(alist-get 'remark contact))
         ,@(when disturb-entry
             `((muted-p
                . ,(and (qq-protocol-json-true-p (cdr disturb-entry)) t))))
         ,@(when notify-mode-entry
             `((message-notify-mode
                . ,(intern (format "%s" (cdr notify-mode-entry)))))))
       :summary-fields
       `((last-message-time . ,msg-time)
         (last-message-id . ,msg-id)
         (last-message-seq . ,msg-seq)
         (last-message-local-id . nil)
         (last-message-order . nil)
         (last-message-preview
          . ,(qq-state-preview-one-line
              (or (alist-get 'lastMessagePreview contact)
                  (alist-get 'last_message_preview contact)
                  "")))
         (last-message-sender-name . nil)
         (last-message-self-p . nil))
       :normalized-message
       (and message-copy
            (qq-state--normalize-raw-message message-copy session-key))
       :unread-entry-p (and unread-entry t)
       :unread-count (and unread-entry (cdr unread-entry))
       :at-me-seq
       (qq-state--normalize-id (alist-get 'firstUnreadAtMeSeq contact))
       :at-all-seq
       (qq-state--normalize-id (alist-get 'firstUnreadAtAllSeq contact))))))

(defun qq-state-apply-recent-contacts
    (contacts &optional read-state-writable-p summary-observation-token)
  "Apply recent CONTACTS snapshot to local session store.

When READ-STATE-WRITABLE-P is non-nil, call it with each session key and apply
that contact's unread projection only when it returns non-nil.  This unread
gate is independent of SUMMARY-OBSERVATION-TOKEN, which owns only root latest
message summaries.  Callers dispatching asynchronous refreshes must capture
that token with `qq-state-session-summary-observation-start'.  Other contact
metadata is always refreshed.  A missing or null `unreadCount' is not an
authoritative zero and therefore never writes the unread projection.

All structured latest messages are normalized before the first session store
write, so malformed payloads cannot leave a half-committed session snapshot."
  (let* ((token (or summary-observation-token
                    (qq-state-session-summary-observation-start)))
         ;; Prepare the complete response before the first store mutation.
         (prepared (delq nil (mapcar #'qq-state--prepare-recent-contact
                                     (or contacts '())))))
    (setq qq-state--recent-session-keys
          (delete-dups
           (mapcar (lambda (entry) (plist-get entry :session-key)) prepared)))
    (clrhash qq-state--recent-session-key-set)
    (dolist (session-key qq-state--recent-session-keys)
      (puthash session-key t qq-state--recent-session-key-set))
    (dolist (entry prepared)
      (let* ((session-key (plist-get entry :session-key))
             (unread-count (plist-get entry :unread-count))
             (valid-unread-count-p
              (and (plist-get entry :unread-entry-p)
                   (qq-protocol--nonnegative-safe-integer-p unread-count)))
             (write-read-state
              (and valid-unread-count-p
                   (or (null read-state-writable-p)
                       (funcall read-state-writable-p session-key))))
             (at-me-seq (plist-get entry :at-me-seq))
             (at-all-seq (plist-get entry :at-all-seq)))
        (qq-state-upsert-session
         session-key
         (append
          (plist-get entry :metadata-fields)
          (when write-read-state
            `((unread-count . ,unread-count)
              (first-unread-message-id . nil)
              (first-unread-message-seq . nil)
              (read-position-available . nil)
              (read-latest-message-id . nil)
              (unread-at-me-message-id . nil)
              (unread-at-me-message-seq . ,(and (> unread-count 0) at-me-seq))
              (unread-at-all-message-id . nil)
              (unread-at-all-message-seq . ,(and (> unread-count 0)
                                                 at-all-seq)))))
         nil)
        (if-let* ((message (plist-get entry :normalized-message)))
            (qq-state--merge-normalized-message session-key message token)
          (qq-state--apply-session-summary
           session-key (plist-get entry :summary-fields) token t))))
    (qq-state--emit 'sessions-refreshed :count (length contacts))
    (qq-state-sessions)))

(defun qq-state-recent-session-keys ()
  "Return keys from the latest authoritative recent-contact snapshot."
  (copy-sequence qq-state--recent-session-keys))

(defun qq-state-session-recent-p (session-key)
  "Return non-nil when SESSION-KEY belongs to the latest recent snapshot."
  (and (gethash session-key qq-state--recent-session-key-set) t))

(defun qq-state--refresh-session-titles ()
  "Refresh hydrated session titles from current contact caches."
  (maphash
   (lambda (session-key session)
     (let ((updated (qq-state--hydrate-session (copy-tree session))))
       (puthash session-key updated qq-state--sessions)))
   qq-state--sessions))

(defun qq-state-apply-friend-categories (categories)
  "Replace cached friends with ordered authoritative CATEGORIES.

Each category contains a `friends' list.  Both category order and friend
order are retained exactly as supplied by the native snapshot."
  (setq qq-state--friend-categories (copy-tree (or categories '())))
  (setq qq-state--friend-categories-loaded-p t)
  (setq qq-state--friend-order nil)
  (clrhash qq-state--friends-by-id)
  (dolist (category qq-state--friend-categories)
    (dolist (friend (alist-get 'friends category))
      (let ((user-id (alist-get 'user_id friend)))
        (push user-id qq-state--friend-order)
        (puthash user-id (copy-tree friend) qq-state--friends-by-id))))
  (setq qq-state--friend-order (nreverse qq-state--friend-order))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'friends-refreshed
                  :count (length qq-state--friend-order)
                  :category-count (length qq-state--friend-categories))
  (qq-state-friend-categories))

(defun qq-state-apply-groups (groups)
  "Replace cached group list with GROUPS."
  (setq qq-state--groups-loaded-p t)
  (setq qq-state--group-order nil)
  (clrhash qq-state--groups-by-id)
  (dolist (group (or groups '()))
    (let ((group-id (qq-state--normalize-id (alist-get 'group_id group))))
      (push group-id qq-state--group-order)
      (puthash group-id (copy-tree group) qq-state--groups-by-id)))
  (setq qq-state--group-order (nreverse qq-state--group-order))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'groups-refreshed :count (length groups))
  groups)

(defun qq-state-friend (user-id)
  "Return cached friend object for USER-ID."
  (copy-tree (gethash (qq-state--normalize-id user-id) qq-state--friends-by-id)))

(defun qq-state-friends ()
  "Return all cached friend objects."
  (mapcar (lambda (user-id)
            (copy-tree (gethash user-id qq-state--friends-by-id)))
          qq-state--friend-order))

(defun qq-state-friend-categories ()
  "Return ordered authoritative friend categories."
  (copy-tree qq-state--friend-categories))

(defun qq-state-friend-categories-loaded-p ()
  "Return non-nil after the native friend-category snapshot has loaded."
  qq-state--friend-categories-loaded-p)

(defun qq-state-friend-count ()
  "Return the number of friends in the authoritative snapshot."
  (length qq-state--friend-order))

(defun qq-state-group (group-id)
  "Return cached group object for GROUP-ID."
  (copy-tree (gethash (qq-state--normalize-id group-id) qq-state--groups-by-id)))

(defun qq-state-groups ()
  "Return all cached group objects."
  (mapcar (lambda (group-id)
            (copy-tree (gethash group-id qq-state--groups-by-id)))
          qq-state--group-order))

(defun qq-state-groups-loaded-p ()
  "Return non-nil after the native joined-group snapshot has loaded."
  qq-state--groups-loaded-p)

(defun qq-state-group-count ()
  "Return the number of joined groups in the authoritative snapshot."
  (length qq-state--group-order))

(defun qq-state-apply-guild-directory (directory)
  "Replace the authoritative Guild DIRECTORY caches.

DIRECTORY is an alist containing ordered `guilds' and `channels' lists."
  (let ((guilds (alist-get 'guilds directory))
        (channels (alist-get 'channels directory)))
    (setq qq-state--guild-directory-loaded-p t
          qq-state--guild-order nil
          qq-state--guild-channel-order nil)
    (clrhash qq-state--guilds-by-id)
    (clrhash qq-state--guild-channels-by-key)
    (dolist (guild guilds)
      (let ((guild-id (alist-get 'guild_id guild)))
        (push guild-id qq-state--guild-order)
        (puthash guild-id (copy-tree guild) qq-state--guilds-by-id)))
    (setq qq-state--guild-order (nreverse qq-state--guild-order))
    (dolist (channel channels)
      (let* ((guild-id (alist-get 'guild_id channel))
             (channel-id (alist-get 'channel_id channel))
             (key (qq-state-guild-channel-session-key guild-id channel-id)))
        (push key qq-state--guild-channel-order)
        (puthash key (copy-tree channel) qq-state--guild-channels-by-key)
        (when (gethash key qq-state--sessions)
          (qq-state-upsert-session
           key `((title . ,(qq-state--cached-guild-channel-title
                            guild-id channel-id))
                 (guild-name . ,(alist-get 'guild_name channel))
                 (channel-name . ,(alist-get 'name channel))
                 (channel-kind . ,(alist-get 'kind channel)))
           nil))))
    (setq qq-state--guild-channel-order
          (nreverse qq-state--guild-channel-order))
    (qq-state--emit 'guild-directory-refreshed
                    :guild-count (length guilds)
                    :channel-count (length channels))
    (qq-state-guild-directory)))

(defun qq-state-guild-directory ()
  "Return a copy of the ordered authoritative Guild directory."
  `((guilds . ,(mapcar
                (lambda (guild-id)
                  (copy-tree (gethash guild-id qq-state--guilds-by-id)))
                qq-state--guild-order))
    (channels . ,(mapcar
                  (lambda (key)
                    (copy-tree (gethash key qq-state--guild-channels-by-key)))
                  qq-state--guild-channel-order))))

(defun qq-state-guild-directory-loaded-p ()
  "Return non-nil once the authoritative Guild directory has loaded."
  qq-state--guild-directory-loaded-p)

(defun qq-state-guild (guild-id)
  "Return cached Guild metadata for GUILD-ID."
  (copy-tree (gethash guild-id qq-state--guilds-by-id)))

(defun qq-state-guild-channel (guild-id channel-id)
  "Return cached channel metadata for GUILD-ID and CHANNEL-ID."
  (copy-tree
   (gethash (qq-state-guild-channel-session-key guild-id channel-id)
            qq-state--guild-channels-by-key)))

(defun qq-state-add-request (request)
  "Append REQUEST event to local request list."
  (push (copy-tree request) qq-state--requests)
  (setq qq-state--requests (sort qq-state--requests
                                 (lambda (left right)
                                   (> (qq-state--normalize-time (alist-get 'time left))
                                      (qq-state--normalize-time (alist-get 'time right))))))
  (qq-state--emit 'request :request (copy-tree request))
  request)

(defun qq-state-requests ()
  "Return pending request events tracked in memory."
  (copy-tree qq-state--requests))

(defun qq-state--action-live-p (action &optional now)
  "Return non-nil when ACTION has not expired relative to NOW."
  (and (listp action)
       (let ((expires-at (alist-get 'expires-at action)))
         (or (null expires-at)
             (> expires-at (or now (float-time)))))))

(defun qq-state--actions-without-timers (actions)
  "Return a copy of ACTIONS alist without internal timer objects."
  (mapcar (lambda (cell)
            (cons (car cell)
                  (assq-delete-all 'timer (copy-tree (cdr cell)))))
          actions))

(defun qq-state--prune-session-actions (session-key &optional now)
  "Drop expired actions for SESSION-KEY.

Return non-nil when anything was removed."
  (let* ((now (or now (float-time)))
         (actions (gethash session-key qq-state--actions))
         (kept nil)
         (removed nil))
    (dolist (cell actions)
      (if (qq-state--action-live-p (cdr cell) now)
          (push cell kept)
        (qq-state--cancel-action-timer (cdr cell))
        (setq removed t)))
    (cond
     ((null kept)
      (when actions
        (remhash session-key qq-state--actions)
        t))
     (removed
      (puthash session-key (nreverse kept) qq-state--actions)
      t)
     (t nil))))

(defun qq-state-session-actions (session-key)
  "Return live chat-actions alist for SESSION-KEY (telega-style).

Each element is (SENDER-ID . ACTION-ALIST).  Timers are stripped."
  (qq-state--prune-session-actions session-key)
  (qq-state--actions-without-timers
   (copy-tree (gethash session-key qq-state--actions))))

(defun qq-state-action-text (session-key)
  "Return one-line action display text for SESSION-KEY, or nil.

Mirrors telega `telega-ins--actions' first-action preference: show the
first live action's `text' (kernel status_text for QQ typing)."
  (when-let* ((actions (qq-state-session-actions session-key))
              (first (car actions))
              (text (alist-get 'text (cdr first))))
    (and (stringp text)
         (not (string-empty-p text))
         text)))

(defun qq-state-clear-session-actions (session-key &optional silent)
  "Clear all chat-actions for SESSION-KEY.

When SILENT is non-nil, do not emit a state-change event."
  (when-let* ((actions (gethash session-key qq-state--actions)))
    (qq-state--cancel-session-action-timers actions)
    (remhash session-key qq-state--actions)
    (unless silent
      (qq-state--emit 'action
                      :session-key session-key
                      :mutation 'delete
                      :source 'notice
                      :actions nil))
    t))

(defun qq-state-clear-sender-action (session-key sender-id &optional silent)
  "Clear SENDER-ID's action in SESSION-KEY (telega chatActionCancel)."
  (let* ((sender-id (qq-state--normalize-id sender-id))
         (actions (gethash session-key qq-state--actions))
         (cell (and sender-id (assoc sender-id actions))))
    (when cell
      (qq-state--cancel-action-timer (cdr cell))
      (setq actions (assoc-delete-all sender-id actions))
      (if actions
          (puthash session-key actions qq-state--actions)
        (remhash session-key qq-state--actions))
      (unless silent
        (qq-state--emit 'action
                        :session-key session-key
                        :mutation 'delete
                        :source 'notice
                        :actions (qq-state-session-actions session-key)))
      t)))

(defun qq-state-apply-input-status (notice)
  "Apply OneBot `input_status' NOTICE as a telega-like chat action.

Maps NapCat:
  notice_type=notify, sub_type=input_status
  user_id / event_type / status_text
to `qq-state--actions' entry of type `typing'.

Semantics (aligned with telega updateChatAction):
- event_type = 0 → cancel that sender's action
- event_type missing or > 0 → typing (status_text optional; default
  \"对方正在输入...\")
- missing cancel packets are handled by `qq-input-status-ttl' auto-expire

Note: kernel/NAPI sometimes omits statusText; JSON then has no status_text
field.  Requiring a non-empty status_text previously dropped every event."
  (let* ((user-id (qq-state--normalize-id (alist-get 'user_id notice)))
         (status-text (alist-get 'status_text notice))
         (event-type (alist-get 'event_type notice))
         ;; nil when field absent — do NOT default to 0 (0 means cancel).
         (event-num (cond
                     ((numberp event-type) event-type)
                     ((and (stringp event-type) (not (string-empty-p event-type)))
                      (string-to-number event-type))
                     (t nil)))
         (explicit-cancel-p (and (numberp event-num) (= event-num 0)))
         (text
          (let ((trimmed (and (stringp status-text) (string-trim status-text))))
            (cond
             ((and trimmed (not (string-empty-p trimmed))) trimmed)
             (explicit-cancel-p nil)
             (t "对方正在输入..."))))
         (active-p (and user-id (not explicit-cancel-p) text))
         ;; Private peer is the typer; session key follows private:<uin>.
         (session-key (and user-id (qq-state-session-key 'private user-id))))
    (cond
     ((null session-key) nil)
     ((not active-p)
      (qq-state-clear-sender-action session-key user-id)
      nil)
     (t
      (let* ((ttl (max 1 (or qq-input-status-ttl 6)))
             (expires-at (+ (float-time) ttl))
             (actions (copy-sequence (gethash session-key qq-state--actions)))
             (old (assoc user-id actions))
             (action `((type . typing)
                       (text . ,text)
                       (event-type . ,(or event-num 1))
                       (expires-at . ,expires-at)
                       (timer . nil)))
             (timer (run-at-time
                     ttl nil
                     (lambda ()
                       (when-let* ((cur (assoc user-id
                                               (gethash session-key
                                                        qq-state--actions))))
                         (when (equal (alist-get 'expires-at (cdr cur))
                                      expires-at)
                           (qq-state-clear-sender-action session-key user-id)))))))
        (when old
          (qq-state--cancel-action-timer (cdr old))
          (setq actions (assoc-delete-all user-id actions)))
        (setf (alist-get 'timer action) timer)
        (puthash session-key
                 (cons (cons user-id action) actions)
                 qq-state--actions)
        (qq-state--emit 'action
                        :session-key session-key
                        :mutation (if old 'update 'create)
                        :source 'notice
                        :actions (qq-state-session-actions session-key))
        (qq-state-session-actions session-key))))))

(defun qq-state-message-apply-tombstones (session-key message)
  "Apply permanent SESSION-KEY tombstones to normalized MESSAGE.

This is the public projection boundary for buffer-owned snapshots such as
message-search results.  It does not store MESSAGE or replay request-scoped
reaction observations; recall is the only permanent message patch."
  (qq-state-session-key-identity session-key)
  (unless (listp message)
    (error "qq: message tombstones require a normalized message"))
  (qq-state--materialize-message-patches session-key message))

(provide 'qq-state)

;;; qq-state.el ends here
