;;; qq-api.el --- OneBot actions and event handlers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; High-level NapCat actions, bootstrap, and event handling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-state)
(require 'qq-transport)

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

(defun qq-api-refresh-status ()
  "Refresh runtime status from NapCat."
  (interactive)
  (qq-api-call
   "get_status"
   nil
   (lambda (response)
     (qq-state-set-status (qq-api--response-data response)))))

(defun qq-api-refresh-login-info ()
  "Refresh login info from NapCat."
  (interactive)
  (qq-api-call
   "get_login_info"
   nil
   (lambda (response)
     (qq-state-set-self-info (qq-api--response-data response)))))

(defun qq-api-refresh-recent-contacts ()
  "Refresh recent contact snapshot from NapCat."
  (interactive)
  (qq-api-call
   "get_recent_contact"
   `((count . ,(max 1 qq-recent-contact-count)))
   (lambda (response)
     (qq-state-apply-recent-contacts (qq-api--response-data response)))))

(defun qq-api-refresh-friend-list ()
  "Refresh friend list from NapCat."
  (interactive)
  (qq-api-call
   "get_friend_list"
   nil
   (lambda (response)
     (qq-state-apply-friends (qq-api--response-data response)))))

(defun qq-api-refresh-group-list ()
  "Refresh group list from NapCat."
  (interactive)
  (qq-api-call
   "get_group_list"
   nil
   (lambda (response)
     (qq-state-apply-groups (qq-api--response-data response)))))

(defun qq-api-bootstrap ()
  "Run the standard initial data bootstrap sequence."
  (interactive)
  (qq-api-refresh-status)
  (qq-api-refresh-login-info)
  (qq-api-refresh-recent-contacts)
  (qq-api-refresh-friend-list)
  (qq-api-refresh-group-list))

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
      (_
       `((user_id . ,(or (alist-get 'peer-uin session)
                         target-id)))))))

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

(defun qq-api-fetch-history (session-key &optional before-message-id callback errback)
  "Fetch history for SESSION-KEY before BEFORE-MESSAGE-ID when provided.

BEFORE-MESSAGE-ID is the NapCat hard-cut snowflake `message_id' of the oldest
already-loaded row (legacy param name `message_seq' on the wire).

CALLBACK is called with the merge-history plist
\(`:added-count', `:message-count', `:oldest-message-id', …) after a successful
merge.  ERRBACK receives (RESPONSE REASON) like other `qq-api-call' errors."
  (interactive)
  (let* ((session (qq-state-session session-key))
         (type (or (alist-get 'type session)
                   (qq-state-session-key-type session-key)))
         (action (pcase type
                   ('group "get_group_msg_history")
                   ('dataline "get_peer_msg_history")
                   (_ "get_friend_msg_history")))
         (params (append
                  (qq-api--session-request-params session-key)
                  `((count . ,(max 1 qq-history-fetch-count)))
                  (when before-message-id
                    `((message_seq . ,(format "%s" before-message-id)))))))
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

(defun qq-api-mark-session-read (session-key)
  "Mark SESSION-KEY as read both locally and in NapCat.

Clear unread optimistically before the network roundtrip (telega/disco
style).  On API failure, restore the previous unread count when the
session is still at zero."
  (interactive)
  (let* ((session (qq-state-session session-key))
         (previous-unread (or (and session (alist-get 'unread-count session)) 0)))
    (when (> previous-unread 0)
      (qq-state-clear-session-unread session-key))
    (qq-api-call
     "mark_msg_as_read"
     (qq-api--session-request-params session-key)
     (lambda (_response)
       ;; Idempotent: success after optimistic clear is a no-op for state.
       (qq-state-clear-session-unread session-key))
     (lambda (response reason)
       (let* ((current (qq-state-session session-key))
              (current-unread (or (and current (alist-get 'unread-count current)) 0)))
         (when (and (> previous-unread 0)
                    current
                    (= current-unread 0))
           (qq-state-set-session-unread session-key previous-unread)))
       (qq-api--default-error response reason)))))

(defun qq-api--send-text-segments (text &optional reply-to-message-id)
  "Return send_msg segment list for TEXT and optional REPLY-TO-MESSAGE-ID."
  (append
   (when reply-to-message-id
     `(((type . "reply")
        (data . ((id . ,(format "%s" reply-to-message-id)))))))
   `(((type . "text")
      (data . ((text . ,text)))))))

(defun qq-api-send-message (session-key segments &optional raw-message)
  "Send SEGMENTS to SESSION-KEY.

Insert a local pending message immediately and update it after the response.
RAW-MESSAGE, when non-nil, overrides the optimistic raw-message field used for
local pending rendering.

The NapCat hard-cut returns `message_id' as the NT snowflake string; that value
is stored as the message `server-id' and becomes the timeline anchor."
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

(defun qq-api-delete-message (message-id)
  "Recall MESSAGE-ID (NT snowflake string) via NapCat and mark it recalled."
  (interactive)
  (qq-api-call
   "delete_msg"
   ;; Always stringify: hard-cut snowflake ids must not be JSON numbers.
   `((message_id . ,(format "%s" message-id)))
   (lambda (_response)
     (qq-state-apply-recall message-id))))

(defun qq-api-get-avatar (user-id callback &optional errback no-cache)
  "Fetch avatar resource for USER-ID and pass it to CALLBACK."
  (qq-api-call
   "get_avatar"
   `((user_id . ,(format "%s" user-id))
     (no_cache . ,(if no-cache t :false)))
   (lambda (response)
     (funcall callback (qq-api--response-data response)))
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

(defun qq-api--handle-notice (notice)
  "Handle websocket NOTICE event."
  (pcase (alist-get 'notice_type notice)
    ((or "friend_recall" "group_recall")
     (qq-state-apply-recall (alist-get 'message_id notice)))
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
