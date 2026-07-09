;;; qq-state.el --- In-memory store for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Central state for sessions, messages, contacts, and connection metadata.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)

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

Chat views should prefer `:mutation' + anchors for incremental EWOC patches
and fall back to full render only for `history'/`reset' or failed patches.")

(defvar qq-state--connection-status 'disconnected)
(defvar qq-state--last-heartbeat nil)
(defvar qq-state--self-info nil)
(defvar qq-state--status nil)
(defvar qq-state--sessions (make-hash-table :test #'equal))
(defvar qq-state--messages-by-session (make-hash-table :test #'equal))
(defvar qq-state--friends-by-id (make-hash-table :test #'equal))
(defvar qq-state--groups-by-id (make-hash-table :test #'equal))
(defvar qq-state--requests nil)
(defvar qq-state--message-session-index (make-hash-table :test #'equal))
(defvar qq-state--local-message-session-index (make-hash-table :test #'equal))
(defvar qq-state--message-order-counter 0)
(defvar qq-state--local-message-counter 0)

(defun qq-state--emit (type &rest plist)
  "Emit state TYPE event with extra PLIST fields.

Preferred keys (callers should populate when applicable):

`:type' (always)  event class: `message', `history', `session', `reset', …
`:mutation'       coarse change kind for views:
                  `create' | `update' | `delete' | `read' | `history' | `session'
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

(defun qq-state-reset ()
  "Reset all in-memory emacs-qq state."
  (setq qq-state--connection-status 'disconnected)
  (setq qq-state--last-heartbeat nil)
  (setq qq-state--self-info nil)
  (setq qq-state--status nil)
  (setq qq-state--requests nil)
  (setq qq-state--message-order-counter 0)
  (setq qq-state--local-message-counter 0)
  (clrhash qq-state--sessions)
  (clrhash qq-state--messages-by-session)
  (clrhash qq-state--friends-by-id)
  (clrhash qq-state--groups-by-id)
  (clrhash qq-state--message-session-index)
  (clrhash qq-state--local-message-session-index)
  (qq-state--emit 'reset))

(defun qq-state-connection-status ()
  "Return current transport connection status symbol."
  qq-state--connection-status)

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

(defun qq-state--session-type-from-chat-type (chat-type)
  "Return session type symbol inferred from raw CHAT-TYPE."
  (cond
   ((equal (qq-state--normalize-id chat-type) "2") 'group)
   ((qq-state--dataline-chat-type-p chat-type) 'dataline)
   (t 'private)))

(defun qq-state-session-key (type target-id)
  "Build a stable session key from TYPE and TARGET-ID."
  (format "%s:%s"
          (pcase type
            ((or 'group "group") "group")
            ((or 'dataline "dataline") "dataline")
            (_ "private"))
          (qq-state--normalize-id target-id)))

(defun qq-state-session-key-type (session-key)
  "Return session type symbol extracted from SESSION-KEY."
  (cond
   ((string-prefix-p "group:" session-key) 'group)
   ((string-prefix-p "dataline:" session-key) 'dataline)
   (t 'private)))

(defun qq-state-session-key-target-id (session-key)
  "Return target id extracted from SESSION-KEY."
  (cadr (split-string session-key ":" t)))

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

(defun qq-state--default-session-title (session)
  "Return default title for SESSION using local caches."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group
       (qq-state--cached-group-title target-id))
      ('dataline
       (or (qq-state--first-present-string
            (alist-get 'peer-name session)
            (alist-get 'remark session))
           "我的手机"))
      (_
       (qq-state--cached-private-title target-id)))))

(defun qq-state--session-template (session-key)
  "Return base session object for SESSION-KEY."
  `((key . ,session-key)
    (type . ,(qq-state-session-key-type session-key))
    (target-id . ,(qq-state-session-key-target-id session-key))
    (title . ,(qq-state-session-key-target-id session-key))
    (unread-count . 0)
    (last-message-time . 0)
    (last-message-preview . "")
    (last-message-id . nil)
    (oldest-message-id . nil)))

(defun qq-state--merge-alists (old new)
  "Return OLD merged with NEW by symbol key.

Values from NEW replace values in OLD."
  (let ((merged (copy-tree old)))
    (dolist (pair new)
      (setf (alist-get (car pair) merged nil nil #'eq) (cdr pair)))
    merged))

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
  (let* ((existing (or (copy-tree (gethash session-key qq-state--sessions))
                       (qq-state--session-template session-key)))
         (session (qq-state--merge-alists existing fields)))
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

(defun qq-state-message-preview-from-segments (segments)
  "Return a plain text preview string for message SEGMENTS."
  (string-trim
   (mapconcat
    (lambda (segment)
      (let ((type (alist-get 'type segment))
            (data (alist-get 'data segment)))
        (pcase type
          ("text" (or (alist-get 'text data) ""))
          ("at" (concat "@" (or (alist-get 'name data)
                                (alist-get 'qq data)
                                "mention")))
          ("reply" "[reply]")
          ("image" "[image]")
          ("file" (format "[file:%s]" (or (alist-get 'name data)
                                          (alist-get 'file data)
                                          "file")))
          ("record" "[voice]")
          ("video" "[video]")
          (_ (format "[%s]" (or type "message"))))))
    (or segments '())
    "")))

(defun qq-state-message-preview (message)
  "Return preview text for normalized MESSAGE object."
  (or (let ((raw (alist-get 'raw-message message)))
        (and (stringp raw)
             (not (string-empty-p raw))
             raw))
      (qq-state-message-preview-from-segments (alist-get 'segments message))
      ""))

(defun qq-state--message-chat-type (message)
  "Return raw backend chat type extracted from raw MESSAGE, or nil."
  (alist-get 'chat_type message))

(defun qq-state--message-peer-uid (message)
  "Return raw backend peer uid extracted from raw MESSAGE, or nil."
  (qq-state--normalize-id (alist-get 'peer_uid message)))

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

(defun qq-state--raw-message-session-key (message &optional fallback-session-key)
  "Return session key for raw MESSAGE, or FALLBACK-SESSION-KEY when given."
  (or fallback-session-key
      (when-let* ((chat-type (qq-state--message-chat-type message))
                  ((qq-state--dataline-chat-type-p chat-type))
                  (peer-uid (qq-state--message-peer-uid message)))
        (qq-state-session-key 'dataline peer-uid))
      (pcase (alist-get 'message_type message)
        ("group"
         (qq-state-session-key 'group (alist-get 'group_id message)))
        ("private"
         (qq-state-session-key
          'private
          (or (alist-get 'target_id message)
              (let ((user-id (qq-state--normalize-id (alist-get 'user_id message)))
                    (self-id (qq-state--normalize-id
                              (or (alist-get 'self_id message)
                                  (alist-get 'user_id qq-state--self-info)))))
                (and user-id
                     (not (equal user-id self-id))
                     user-id))
              (alist-get 'user_id message))))
        (_ nil))))

(defun qq-state--normalize-raw-message (message &optional fallback-session-key)
  "Normalize raw OneBot MESSAGE into local store shape."
  (let* ((chat-type (qq-state--normalize-id (qq-state--message-chat-type message)))
         (peer-uid (qq-state--message-peer-uid message))
         (peer-uin (qq-state--normalize-id
                    (or (alist-get 'peer_uin message)
                        (alist-get 'peerUin message))))
         (session-key (qq-state--raw-message-session-key message fallback-session-key))
         (sender (alist-get 'sender message))
         (sender-id (qq-state--normalize-id
                     (or (alist-get 'user_id sender)
                         (alist-get 'user_id message))))
         (sender-id (if (and (qq-state--dataline-chat-type-p chat-type)
                             (equal sender-id "0"))
                        nil
                      sender-id))
         (sender-fields (qq-state--sender-display-fields session-key sender sender-id))
         (segments (or (alist-get 'message message) '()))
         (raw-message (or (alist-get 'raw_message message)
                          (qq-state-message-preview-from-segments segments)
                          ""))
         ;; NapCat hard-cut: message_id is the NT snowflake string (never coerce
         ;; with string-to-number — snowflakes exceed fixnum precision).
         (server-id (qq-state--normalize-id
                     (or (alist-get 'message_id message)
                         (alist-get 'id message))))
         (self-p (qq-state--message-self-p message))
         (status (if self-p 'sent 'received))
         (time (qq-state--normalize-time (alist-get 'time message)))
         (target-id (or (and (qq-state--dataline-chat-type-p chat-type) peer-uid)
                        (qq-state--normalize-id (alist-get 'target_id message)))))
    `((id . ,server-id)
      (server-id . ,server-id)
      (session-key . ,session-key)
      (time . ,time)
      (sender-id . ,sender-id)
      (sender-name . ,(alist-get 'sender-name sender-fields))
      (sender-secondary-name . ,(alist-get 'sender-secondary-name sender-fields))
      (sender-card . ,(alist-get 'sender-card sender-fields))
      (sender-nickname . ,(alist-get 'sender-nickname sender-fields))
      (sender-remark . ,(alist-get 'sender-remark sender-fields))
      (self-p . ,self-p)
      (status . ,status)
      (segments . ,segments)
      (raw-message . ,raw-message)
      (preview . ,(qq-state-message-preview-from-segments segments))
      (message-type . ,(alist-get 'message_type message))
      (chat-type . ,chat-type)
      (peer-uid . ,peer-uid)
      (peer-uin . ,peer-uin)
      (peer-name . ,(qq-state--present-string (alist-get 'peer_name message)))
      (group-id . ,(qq-state--normalize-id (alist-get 'group_id message)))
      (user-id . ,(qq-state--normalize-id (alist-get 'user_id message)))
      (target-id . ,target-id)
      (order . ,(qq-state--next-message-order))
      (raw-event . ,(copy-tree message)))))

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
        (local-id (alist-get 'local-id message)))
    (qq-state--find-message
     messages
     (lambda (it)
       (or (and server-id (equal (alist-get 'server-id it) server-id))
           (and local-id (equal (alist-get 'local-id it) local-id)))))))

(defun qq-state--weak-pending-match (messages message)
  "Return weak pending match in MESSAGES for self-sent MESSAGE."
  (let ((self-p (alist-get 'self-p message))
        (raw-message (alist-get 'raw-message message))
        (time (qq-state--normalize-time (alist-get 'time message))))
    (when self-p
      (qq-state--find-message
       messages
       (lambda (it)
         (and (alist-get 'self-p it)
              (alist-get 'local-id it)
              (memq (alist-get 'status it) '(pending sent))
              (equal (alist-get 'raw-message it) raw-message)
              (<= (abs (- time (qq-state--normalize-time (alist-get 'time it))))
                  qq-self-message-dedupe-window)))))))

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

(defun qq-state--sync-session-summary (session-key)
  "Sync last and oldest message summary for SESSION-KEY."
  (let* ((messages (or (gethash session-key qq-state--messages-by-session) '()))
         (oldest (seq-find (lambda (it) (alist-get 'server-id it)) messages))
         (latest (car (last messages)))
         fields)
    (when latest
      (setq fields
            `((last-message-time . ,(qq-state--normalize-time (alist-get 'time latest)))
              (last-message-id . ,(or (alist-get 'server-id latest)
                                      (alist-get 'id latest)))
              (last-message-preview . ,(qq-state-message-preview latest)))))
    (push `(oldest-message-id . ,(alist-get 'server-id oldest)) fields)
    (qq-state-upsert-session session-key (nreverse fields) nil)))

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

(defun qq-state--merge-normalized-message (session-key message &optional count-unread)
  "Merge normalized MESSAGE into SESSION-KEY.

When COUNT-UNREAD is non-nil and MESSAGE is not self-sent, increment unread.

Return three values via `cl-values':
1. merged local message object
2. mutation symbol `create' or `update'
3. previous timeline anchor when a pending local-id is promoted to server-id,
   else nil"
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (or (qq-state--direct-message-match messages message)
                       (qq-state--weak-pending-match messages message)))
         (merged (if existing
                     (qq-state--merge-alists existing message)
                   message))
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
    (when old-order
      (setf (alist-get 'order merged nil nil #'eq) old-order))
    (if existing
        (setq messages (qq-state--replace-message messages existing merged))
      (push merged messages))
    (setq messages (qq-state--sort-messages messages))
    (puthash session-key messages qq-state--messages-by-session)
    (qq-state-upsert-session session-key nil nil)
    (qq-state--index-message merged)
    (qq-state--sync-session-summary session-key)
    (when (and count-unread (not (alist-get 'self-p merged)))
      (let* ((session (or (gethash session-key qq-state--sessions)
                          (qq-state--session-template session-key)))
             (current (or (alist-get 'unread-count session) 0)))
        (qq-state-upsert-session session-key `((unread-count . ,(1+ current))) nil)))
    (cl-values merged mutation previous-anchor)))

(defun qq-state-merge-live-message (message)
  "Merge live websocket MESSAGE into local state and return its session key."
  (let* ((normalized (qq-state--normalize-raw-message message))
         (session-key (alist-get 'session-key normalized)))
    (when session-key
      (cl-multiple-value-bind (merged mutation previous-anchor)
          (qq-state--merge-normalized-message
           session-key
           normalized
           (not (alist-get 'self-p normalized)))
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

(defun qq-state-merge-history (session-key raw-messages)
  "Merge RAW-MESSAGES history batch into SESSION-KEY."
  (qq-state-upsert-session session-key nil nil)
  (dolist (raw-message (or raw-messages '()))
    (qq-state--merge-normalized-message
     session-key
     (qq-state--normalize-raw-message raw-message session-key)
     nil))
  (qq-state--emit 'history
                  :session-key session-key
                  :message-count (length raw-messages)
                  :mutation 'history)
  session-key)

(defun qq-state-mark-pending-message-sent (session-key local-id message-id)
  "Mark local pending message LOCAL-ID as sent with MESSAGE-ID in SESSION-KEY.

MESSAGE-ID is the NapCat NT snowflake string (`message_id' in the protocol
hard-cut).  It is stored as `server-id' and becomes the chat timeline anchor."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id))))
         (normalized-id (qq-state--normalize-id message-id)))
    (when (and existing normalized-id)
      (let ((updated (qq-state--merge-alists
                      existing
                      `((id . ,normalized-id)
                        (server-id . ,normalized-id)
                        (status . sent)
                        (error . nil)))))
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
  "Mark local pending message LOCAL-ID as failed with REASON in SESSION-KEY."
  (let* ((messages (copy-tree (or (gethash session-key qq-state--messages-by-session) '())))
         (existing (qq-state--find-message
                    messages
                    (lambda (it)
                      (equal (alist-get 'local-id it) local-id)))))
    (when existing
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

(defun qq-state-clear-session-unread (session-key)
  "Reset unread count for SESSION-KEY."
  (qq-state-upsert-session session-key '((unread-count . 0)) nil)
  (qq-state--emit 'session
                  :session-key session-key
                  :session (qq-state-session session-key)
                  :mutation 'read)
  0)

(defun qq-state-apply-recall (message-id)
  "Mark MESSAGE-ID as recalled when it exists locally."
  (let* ((normalized-id (qq-state--normalize-id message-id))
         (session-key (gethash normalized-id qq-state--message-session-index))
         (messages (and session-key
                        (copy-tree (or (gethash session-key qq-state--messages-by-session) '()))))
         (existing (and messages
                        (qq-state--find-message
                         messages
                         (lambda (it)
                           (equal (alist-get 'server-id it) normalized-id))))))
    (when (and session-key existing)
      (let ((updated (qq-state--merge-alists
                      existing
                      '((status . recalled)
                        (raw-message . "[message recalled]")
                        (preview . "[message recalled]")))))
        (setq messages (qq-state--replace-message messages existing updated))
        (puthash session-key messages qq-state--messages-by-session)
        (qq-state--sync-session-summary session-key)
        (qq-state--emit 'message
                        :session-key session-key
                        :message (copy-tree updated)
                        :message-anchor (qq-state-message-anchor updated)
                        :mutation 'update
                        :source 'notice)
        updated))))

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

(defun qq-state-apply-recent-contacts (contacts)
  "Apply recent CONTACTS snapshot to local session store."
  (dolist (contact (or contacts '()))
    (let* ((chat-type (alist-get 'chatType contact))
           (type (qq-state--session-type-from-chat-type chat-type))
           (peer-uid (qq-state--normalize-id (alist-get 'peerUid contact)))
           (peer-uin (qq-state--normalize-id (alist-get 'peerUin contact)))
           (target-id (if (eq type 'dataline)
                          (or peer-uid peer-uin)
                        peer-uin))
           (session-key (qq-state-session-key type target-id))
           (title (qq-state--recent-contact-title contact session-key))
           (msg-time (qq-state--normalize-time (alist-get 'msgTime contact)))
           (msg-id (qq-state--normalize-id (alist-get 'msgId contact)))
           (last-message (alist-get 'lastestMsg contact))
           (preview (cond
                     ((and (listp last-message)
                           (stringp (alist-get 'raw_message last-message))
                           (not (string-empty-p (alist-get 'raw_message last-message))))
                      (alist-get 'raw_message last-message))
                     ((listp last-message)
                      (qq-state-message-preview-from-segments (alist-get 'message last-message)))
                     (t ""))))
      (qq-state-upsert-session
       session-key
       `((title . ,title)
         (target-id . ,target-id)
         (chat-type . ,(qq-state--normalize-id chat-type))
         (peer-uid . ,peer-uid)
         (peer-uin . ,peer-uin)
         (peer-name . ,(qq-state--present-string (alist-get 'peerName contact)))
         (remark . ,(alist-get 'remark contact))
         (last-message-time . ,msg-time)
         (last-message-id . ,msg-id)
         (last-message-preview . ,preview))
       nil)
      (when (listp last-message)
        (let ((message-copy (copy-tree last-message)))
          (when (and msg-id
                     (not (alist-get 'message_id message-copy nil nil #'eq))
                     (not (alist-get 'id message-copy nil nil #'eq)))
            (push (cons 'message_id msg-id) message-copy))
          (when (and (> msg-time 0)
                     (not (alist-get 'time message-copy nil nil #'eq)))
            (push (cons 'time msg-time) message-copy))
          (qq-state--merge-normalized-message
           session-key
           (qq-state--normalize-raw-message message-copy session-key)
           nil)))))
  (qq-state--emit 'sessions-refreshed :count (length contacts))
  (qq-state-sessions))

(defun qq-state--refresh-session-titles ()
  "Refresh hydrated session titles from current contact caches."
  (maphash
   (lambda (session-key session)
     (let ((updated (qq-state--hydrate-session (copy-tree session))))
       (puthash session-key updated qq-state--sessions)))
   qq-state--sessions))

(defun qq-state-apply-friends (friends)
  "Replace cached friend list with FRIENDS."
  (clrhash qq-state--friends-by-id)
  (dolist (friend (or friends '()))
    (puthash (qq-state--normalize-id (alist-get 'user_id friend))
             (copy-tree friend)
             qq-state--friends-by-id))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'friends-refreshed :count (length friends))
  friends)

(defun qq-state-apply-groups (groups)
  "Replace cached group list with GROUPS."
  (clrhash qq-state--groups-by-id)
  (dolist (group (or groups '()))
    (puthash (qq-state--normalize-id (alist-get 'group_id group))
             (copy-tree group)
             qq-state--groups-by-id))
  (qq-state--refresh-session-titles)
  (qq-state--emit 'groups-refreshed :count (length groups))
  groups)

(defun qq-state-friend (user-id)
  "Return cached friend object for USER-ID."
  (copy-tree (gethash (qq-state--normalize-id user-id) qq-state--friends-by-id)))

(defun qq-state-group (group-id)
  "Return cached group object for GROUP-ID."
  (copy-tree (gethash (qq-state--normalize-id group-id) qq-state--groups-by-id)))

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

(provide 'qq-state)

;;; qq-state.el ends here
