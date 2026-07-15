;;; qq-api-test.el --- Tests for qq-api -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-api)

(defmacro qq-api-test-with-reset (&rest body)
  "Run BODY with isolated QQ state."
  (declare (indent 0) (debug t))
  `(let ((qq-state-change-hook nil))
     (unwind-protect
         (progn (qq-state-reset) ,@body)
       (qq-state-reset))))

(defun qq-api-test--native-message
    (entry-id &optional message-id segments)
  "Return one strict fork-native forward snapshot."
  (copy-tree
   `((entry_id . ,entry-id)
     ,@(when message-id `((message_id . ,message-id)))
     (state . "live")
     (sent_at . 1710000000)
     (sender . ((kind . "user")
                (user_id . "10001")
                (name . "Alice")))
     (origin . ((kind . "group") (group_id . "20001")))
     (segments
      . ,(or segments
             '(((kind . "text") (payload . ((text . "hello"))))))))))

(defun qq-api-test--context-source ()
  "Return one strict fork-native context forward source."
  '((kind . "context")
    (peer . ((chat_type . 2)
             (peer_uid . "u_group-peer")
             (guild_id . "")))
    (root_message_id . "9007199254742007001")
    (parent_message_id . "9007199254742007002")))

(defun qq-api-test--read-state ()
  "Return one complete fork-native authoritative read state."
  '((unread_count . 5)
    (first_unread
     . ((sequence . "30001")
        (message_id . "9007199254742007089")))
    (mentions
     . ((at_me
         . ((sequence . "30003")
            (message_id . "9007199254742007091")))
        (at_all . nil)))
    (latest
     . ((message_id . "9007199254742007094")
        (sequence . "30005")))))

(defun qq-api-test--read-state-notice (chat)
  "Return one strict read-state notice for CHAT."
  `((time . 1710000200)
    (self_id . 90001)
    (post_type . "notice")
    (notice_type . "emacs_read_state")
    (chat . ,chat)
    (read_state . ,(qq-api-test--read-state))))

(defun qq-api-test--guild-message-event ()
  "Return one strict synthetic QQ channel message event."
  (copy-tree
   '((post_type . "emacs_guild_message")
    (chat . ((kind . "guild-channel")
             (guild_id . "9007199254740993")
             (channel_id . "9007199254741999")))
    (message_id . "9007199254742999")
    (message_sequence . "17")
    (sent_at . 1784000000)
    (channel_name . "Synthetic channel")
    (sender . ((native_id . "u_synthetic_sender")
               (user_id . nil)
               (nickname . "Synthetic member")
               (member_name . "")
               (display_name . "Synthetic member")))
    (outgoing . :false)
    (state . "live")
     (segments . (((kind . "text")
                   (payload . ((text . "hello")))))))))

(defun qq-api-test--guild-message-record ()
  "Return the action-response form of the synthetic Guild message."
  (assq-delete-all 'post_type (qq-api-test--guild-message-event)))

(defun qq-api-test--guild-forum-post (&optional post-id created-at)
  "Return one strict synthetic QQ Guild forum post."
  `((chat . ((kind . "guild-channel")
             (guild_id . "9007199254740993")
             (channel_id . "9007199254741999")))
    (post_id . ,(or post-id "B_synthetic_opaque_post"))
    (created_at . ,(or created-at 1784000000))
    (updated_at . nil)
    (channel_name . "Synthetic forum")
    (sender . ((native_id . "144115219000000001")
               (display_name . "Synthetic member")
               (avatar_url . "https://example.invalid/avatar.png")))
    (state . "live")
    (title . "Synthetic title")
    (comment_count . 3)
    (segments . (((kind . "text")
                  (payload . ((text . "Synthetic body"))))))))

(defun qq-api-test--mark-read-response (scope message-id unread-count)
  "Return a strict mark-read response for SCOPE and MESSAGE-ID.

The authoritative post-state reports UNREAD-COUNT."
  (let ((state (copy-tree (qq-api-test--read-state))))
    (setf (alist-get 'unread_count state) unread-count)
    (when (= unread-count 0)
      (setf (alist-get 'first_unread state) nil))
    `((status . "ok")
      (data
       . ((scope . ,scope)
          (,(if (equal scope "session")
                'requested_message_id
              'read_through_message_id)
           . ,message-id)
          (read_state . ,state))))))

(defun qq-api-test--search-result (&optional message-id sent-at preview)
  "Return one strict fork-native message-search result."
  (copy-tree
   `((chat . ((kind . "group") (group_id . "20001")))
     (message_id . ,(or message-id "9007199254742007089"))
     (message_seq . "30001")
     (sent_at . ,(or sent-at 1710000000))
     (sender . ((user_id . "10001") (name . "Alice")))
     (preview . ,(or preview "hello")))))

(defun qq-api-test--search-message-result (&optional message-id preview)
  "Return one strict flat rendering snapshot search result."
  (let* ((id (or message-id "9007199254742007089"))
         (text (or preview "hello")))
    `((chat . ((kind . "group") (group_id . "20001")))
      (message_id . ,id)
      (message_seq . "30001")
      (sent_at . 1710000000)
      (sender . ((user_id . "10001") (name . "Alice")))
      (outgoing . :false)
      (state . "live")
      (segments . (((kind . "text")
                    (payload . ((text . ,text))))))
      (reactions))))

(defun qq-api-test--group-chat-search-result (&optional group-id)
  "Return one strict native group-chat search result for GROUP-ID."
  (copy-tree
   `((group
     . ((group_id . ,(or group-id "20001"))
        (name . "Search Group")
        (remark . "")
        (member_count . 42)
        (is_conf . :false)
        (has_modify_conf_group_face . :false)
        (has_modify_conf_group_name . t)
        (no_code_finger_open_flag . :false)
        (self_permission . "member")
        (hits
         . ((group_id)
            (name . (((start . 0) (end . 6) (text . "Search"))))
            (remark)))))
    (discussions)
    (member_profiles
     . (((uid . "u_native_alice")
         (user_id . "10001")
         (nickname . "Alice")
         (remark . "")
         (card . "Alice Card")
         (hits . ((user_id) (nickname) (remark) (card))))))
     (member_cards)
     (recall_reason . ""))))

(defun qq-api-test--contact-search-friend-result ()
  "Return one strict native friend-search result."
  (copy-tree
   '((kind . "friend")
     (user_id . "9007199254740993")
     (uid . "uid-alice")
     (qid . "alice")
     (nickname . "Alice")
     (remark . "Maintainer")
     (category_name . "Friends")
     (hits
      . ((qid . (((start . 0) (end . 5) (text . "alice"))))
         (user_id . (((start . 0) (end . 4) (text . "9007"))))
         (nickname . (((start . 0) (end . 5) (text . "Alice"))))
         (remark . (((start . 0) (end . 4) (text . "Main"))))))
     (recall_reason . "friend recall"))))

(defun qq-api-test--contact-search-group-member-result (&optional group-id)
  "Return one strict native group-member result for GROUP-ID."
  (copy-tree
   `((kind . "group_member")
     (group_id . ,(or group-id "4294967295"))
     (group_name . "Emacs Group")
     (group_remark . "Lisp users")
     (user_id . "18446744073709551615")
     (uid . "uid-bob")
     (nickname . "Bob")
     (remark . "Contributor")
     (card . "Bob Card")
     (is_friend . :false)
     (hits
      . ((user_id . (((start . 0) (end . 4) (text . "1844"))))
         (nickname . (((start . 0) (end . 3) (text . "Bob"))))
         (remark . (((start . 0) (end . 4) (text . "Cont"))))
         (card . (((start . 0) (end . 3) (text . "Bob"))))))
     (recall_reason . "member recall"))))

(defun qq-api-test--uuid-v4 ()
  "Return one canonical UUIDv4 cursor for API tests."
  "123e4567-e89b-42d3-a456-426614174000")

(defun qq-api-test--capability-token (&optional character)
  "Return one synthetic exact capability token filled with CHARACTER."
  (make-string 43 (or character ?A)))

(defun qq-api-test--message-reference (kind target-id message-id)
  "Return one closed mutation reference for KIND, TARGET-ID and MESSAGE-ID."
  `((message_id . ,message-id)
    (chat . ,(if (eq kind 'group)
                 `((kind . "group") (group_id . ,target-id))
               `((kind . "private") (user_id . ,target-id))))))

(ert-deftest qq-api-chat-locator-group-id-is-canonical-uint32 ()
  (should
   (equal
    (qq-api-validate-chat-locator
     '((kind . "group") (group_id . "4294967295")))
    '((kind . "group") (group_id . "4294967295"))))
  (dolist (group-id '("0" "4294967296" 4294967295))
    (should-error
     (qq-api-validate-chat-locator
     `((kind . "group") (group_id . ,group-id)))
     :type 'user-error)))

(ert-deftest qq-api-chat-locator-private-id-is-canonical-nonzero-decimal ()
  (should
   (equal
    (qq-api-validate-chat-locator
     '((kind . "private") (user_id . "10001")))
    '((kind . "private") (user_id . "10001"))))
  (dolist (user-id '("0" "010001" 10001))
    (should-error
     (qq-api-validate-chat-locator
      `((kind . "private") (user_id . ,user-id)))
     :type 'user-error)))

(ert-deftest qq-api-materialization-owner-settles-synchronous-success ()
  (let ((qq-state-change-hook nil)
        seen-owner)
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (_action _params callback &optional _errback)
                     (funcall callback '((status . "ok")))
                     'sync-request)))
          (should
           (eq 'sync-request
               (qq-api--call-with-materialization-owner
                "private:10001" "action" nil
                (lambda (_response owner)
                  (setq seen-owner owner)))))
          (should seen-owner)
          (should-not (qq-state-materialization-request-end seen-owner))
          (should-not (gethash 'sync-request qq-api--request-finalizers))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners))))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-materialization-owner-settles-synchronous-error ()
  (let ((qq-state-change-hook nil)
        reason-seen)
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (_action _params _callback &optional errback)
                     (funcall errback nil "synchronous failure")
                     nil)))
          (should-not
           (qq-api--call-with-materialization-owner
            "private:10001" "action" nil #'ignore
            (lambda (_response reason)
              (setq reason-seen reason))))
          (should (equal reason-seen "synchronous failure"))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners))))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-materialization-owner-settles-asynchronous-error ()
  (let ((qq-state-change-hook nil)
        error-callback
        reason-seen)
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (_action _params _callback &optional errback)
                     (setq error-callback errback)
                     'async-error-request)))
          (should
           (eq 'async-error-request
               (qq-api--call-with-materialization-owner
                "private:10001" "action" nil #'ignore
                (lambda (_response reason)
                  (setq reason-seen reason)))))
          (should (= 1 (hash-table-count
                        qq-state--materialization-request-owners)))
          (funcall error-callback nil "late failure")
          (should (equal reason-seen "late failure"))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should-not
           (gethash 'async-error-request qq-api--request-finalizers)))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-cancel-request-settles-materialization-owner ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _args) 'async-request))
                  ((symbol-function 'qq-transport-cancel)
                   (lambda (token) (eq token 'async-request))))
          (should
           (eq 'async-request
               (qq-api--call-with-materialization-owner
                "group:20001" "action" nil #'ignore)))
          (should (= 1 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should (gethash 'async-request qq-api--request-finalizers))
          (should (qq-api-cancel-request 'async-request))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should-not (gethash 'async-request qq-api--request-finalizers)))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-cancel-request-settles-owner-after-transport-race ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _args) 'raced-request))
                  ((symbol-function 'qq-transport-cancel)
                   (lambda (_token) nil)))
          (should
           (eq 'raced-request
               (qq-api--call-with-materialization-owner
                "group:20001" "action" nil #'ignore)))
          (should (= 1 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should-not (qq-api-cancel-request 'raced-request))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should-not (gethash 'raced-request qq-api--request-finalizers)))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-cancel-request-settles-owner-when-transport-signals ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _args) 'signalling-request))
                  ((symbol-function 'qq-transport-cancel)
                   (lambda (_token) (error "transport cancel failed"))))
          (qq-api--call-with-materialization-owner
           "group:20001" "action" nil #'ignore)
          (should (= 1 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should
           (gethash 'signalling-request qq-api--request-finalizers))
          (should
           (equal '(error "transport cancel failed")
                  (condition-case err
                      (qq-api-cancel-request 'signalling-request)
                    (error err))))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners)))
          (should-not
           (gethash 'signalling-request qq-api--request-finalizers)))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-cancel-request-rejects-late-materialization-callbacks ()
  (let ((qq-state-change-hook nil)
        success-callback
        error-callback
        calls)
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (_action _params success &optional error)
                     (setq success-callback success
                           error-callback error)
                     'late-request))
                  ((symbol-function 'qq-transport-cancel)
                   (lambda (_token) t)))
          (qq-api--call-with-materialization-owner
           "group:20001" "action" nil
           (lambda (&rest _args) (push 'success calls))
           (lambda (&rest _args) (push 'error calls)))
          (should (qq-api-cancel-request 'late-request))
          ;; An alternate transport may still deliver stale closures after it
          ;; reported cancellation.  API ownership rejects both outcomes.
          (funcall success-callback '((status . "ok")))
          (funcall error-callback nil "late failure")
          (should-not calls)
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners))))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-materialization-owner-settles-dispatch-signal ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (clrhash qq-api--request-finalizers)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _args) (error "dispatch failed"))))
          (should-error
           (qq-api--call-with-materialization-owner
            "private:10001" "action" nil #'ignore))
          (should (= 0 (hash-table-count
                        qq-state--materialization-request-owners))))
      (clrhash qq-api--request-finalizers)
      (qq-state-reset))))

(ert-deftest qq-api-send-message-builds-send-msg-request ()
  (let (captured-action captured-params pending-call)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest args)
                 (setq pending-call args)
                 '((local-id . "local-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (action params _callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 nil)))
      (qq-api-send-message
       "private:10001"
       '(((type . "reply")
          (data . ((id . "42"))))
         ((type . "text")
          (data . ((text . "hello")))))
       "hello")
      (should (equal pending-call
                     '("private:10001"
                       (((type . "reply")
                         (data . ((id . "42"))))
                        ((type . "text")
                         (data . ((text . "hello")))))
                       "hello")))
      (should (equal captured-action "send_msg"))
      (should (equal (alist-get 'user_id captured-params) "10001"))
      (should (equal (alist-get 'message captured-params)
                     '(((type . "reply")
                        (data . ((id . "42"))))
                       ((type . "text")
                         (data . ((text . "hello"))))))))))

(ert-deftest qq-api-send-message-calls-owner-after-pending-success ()
  (let (success-fn events promotion-args)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-state-session-sendable-p)
               (lambda (_session-key) t))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (setq success-fn callback)))
              ((symbol-function 'qq-state-mark-pending-message-sent)
               (lambda (&rest args)
                 (setq promotion-args args)
                 (push 'promoted events))))
      (qq-api-send-message
       "private:10001"
       '(((type . "text") (data . ((text . "hello")))))
       "hello"
       (lambda (_response) (push 'callback events)))
      (funcall success-fn
               '((data . ((message_id . "9007199254742007094")))))
      (should (equal '(promoted callback) (nreverse events)))
      (should (equal (seq-take promotion-args 3)
                     '("private:10001" "local-1"
                       "9007199254742007094")))
      (should (equal (plist-get (nth 3 promotion-args) :session-key)
                     "private:10001"))
      (should-not
       (qq-state-materialization-request-end (nth 3 promotion-args))))))

(ert-deftest qq-api-send-message-calls-owner-after-pending-failure ()
  (let (error-fn events)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params _callback &optional errback)
                 (setq error-fn errback)))
              ((symbol-function 'qq-state-mark-pending-message-failed)
               (lambda (&rest _args) (push 'failed events))))
      (qq-api-send-message
       "private:10001"
       '(((type . "text") (data . ((text . "hello")))))
       "hello" nil
       (lambda (_response _reason) (push 'errback events)))
      (funcall error-fn nil "network failed")
      (should (equal '(failed errback) (nreverse events))))))

(ert-deftest qq-api-send-message-ignores-late-failure-after-authoritative-event ()
  (let (error-fn called)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params _callback &optional errback)
                 (setq error-fn errback)))
              ((symbol-function 'qq-state-mark-pending-message-failed)
               (lambda (&rest _args) nil)))
      (qq-api-send-message
       "private:10001"
       '(((type . "text") (data . ((text . "hello")))))
       "hello" nil
       (lambda (&rest _args) (setq called t)))
      (funcall error-fn nil "late timeout")
      (should-not called))))

(ert-deftest qq-api-send-message-malformed-success-enters-failure-path ()
  (let (success-fn failed-reason error-reason callback-called)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (setq success-fn callback)))
              ((symbol-function 'qq-state-mark-pending-message-sent)
               (lambda (&rest _args)
                 (ert-fail "malformed response must not promote pending")))
              ((symbol-function 'qq-state-mark-pending-message-failed)
               (lambda (_session _local-id reason)
                 (setq failed-reason reason)
                 '((status . failed)))))
      (qq-api-send-message
       "private:10001"
       '(((type . "text") (data . ((text . "hello")))))
       "hello"
       (lambda (&rest _args) (setq callback-called t))
       (lambda (_response reason) (setq error-reason reason)))
      (funcall success-fn '((data . ((message_id . 42)))))
      (should-not callback-called)
      (should (string-match-p "original string" failed-reason))
      (should (equal failed-reason error-reason)))))

(ert-deftest qq-api-send-message-builds-dataline-send-msg-request ()
  (let (captured-action captured-params pending-call)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest args)
                 (setq pending-call args)
                 '((local-id . "local-1"))))
              ((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "dataline:desktop:wrong")
                   (type . dataline)
                   (target-id . "wrong")
                   (chat-type . "8")
                   (peer-uid . "wrong"))))
              ((symbol-function 'qq-api-call)
               (lambda (action params _callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 nil)))
      (qq-api-send-message
       "dataline:mobile:dev:a"
       '(((type . "text")
          (data . ((text . "hello phone")))))
       "hello phone")
      (should (equal pending-call
                     '("dataline:mobile:dev:a"
                       (((type . "text")
                         (data . ((text . "hello phone")))))
                       "hello phone")))
      (should (equal captured-action "send_msg"))
      (should (equal (alist-get 'chat_type captured-params) "134"))
      (should (equal (alist-get 'peer_uid captured-params) "dev:a"))
      (should-not (alist-get 'user_id captured-params)))))

(ert-deftest qq-api-send-message-uses-closed-guild-action-and-locator ()
  (let (captured-action captured-params)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-state-session-sendable-p)
               (lambda (_session-key) t))
              ((symbol-function 'qq-api-call)
               (lambda (action params _callback &optional _errback)
                 (setq captured-action action
                       captured-params params))))
      (qq-api-send-message
       "guild:9007199254740993:channel:9007199254741999"
       '(((type . "text") (data . ((text . "hello channel")))))
       "hello channel")
      (should (equal captured-action "emacs_send_guild_message"))
      (should (equal (alist-get 'chat captured-params)
                     '((kind . "guild-channel")
                       (guild_id . "9007199254740993")
                       (channel_id . "9007199254741999"))))
      (should (equal (alist-get 'message captured-params)
                     '(((type . "text")
                        (data . ((text . "hello channel")))))))
      (should-not (assq 'chat_type captured-params)))))

(ert-deftest qq-api-guild-message-event-validates-and-dispatches-closed-shape ()
  (let (delivered)
    (cl-letf (((symbol-function 'qq-state-merge-guild-message)
               (lambda (event) (setq delivered event))))
      (let ((source (qq-api-test--guild-message-event)))
        (qq-api-handle-event source)
        (should (equal delivered source))
        (setf (alist-get 'message_sequence source) 17)
        (should-error (qq-api-handle-event source))))))

(ert-deftest qq-api-guild-message-event-rejects-lossy-or-open-identities ()
  (dolist (mutator
           (list
            (lambda (event)
              (setf (alist-get 'guild_id (alist-get 'chat event))
                    9007199254740992))
            (lambda (event)
              (setf (alist-get 'user_id (alist-get 'sender event)) "0"))
            (lambda (event)
              (nconc event '((extra . t))))))
    (let ((event (qq-api-test--guild-message-event)))
      (funcall mutator event)
      (should-error (qq-api--validate-guild-message-event event)))))

(ert-deftest qq-api-guild-forum-page-validates-opaque-cursor-and-post-identity ()
  (let* ((chat '((kind . "guild-channel")
                 (guild_id . "9007199254740993")
                 (channel_id . "9007199254741999")))
         (page `((posts . (,(qq-api-test--guild-forum-post)))
                 (next_cursor . "page=synthetic-next")
                 (finished . :false))))
    (should (equal (qq-api--validate-guild-forum-page page chat) page))
    (let ((bad (copy-tree page)))
      (setf (alist-get 'post_id (car (alist-get 'posts bad))) 9007199254740992)
      (should-error (qq-api--validate-guild-forum-page bad chat)))
    (let ((bad (copy-tree page)))
      (setf (alist-get 'next_cursor bad) nil)
      (should-error (qq-api--validate-guild-forum-page bad chat)))))

(ert-deftest qq-api-fetch-guild-forum-page-replaces-first-page-and-merges-older-pages ()
  (qq-api-test-with-reset
   (let* ((session-key
           (qq-state-guild-channel-session-key
            "9007199254740993" "9007199254741999"))
          (post (qq-api-test--guild-forum-post))
          action params replaced merged delivered)
     (qq-state-upsert-session
      session-key '((type . guild-channel) (channel-kind . "forum")) nil)
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (candidate-action candidate-params callback
                         &optional _errback)
                  (setq action candidate-action
                        params candidate-params)
                  (funcall callback
                           `((data . ((posts . (,post))
                                      (next_cursor . "page=synthetic-next")
                                      (finished . :false)))))))
               ((symbol-function 'qq-state-replace-guild-forum-posts)
                (lambda (candidate posts)
                  (setq replaced (list candidate posts))))
               ((symbol-function 'qq-state-merge-guild-forum-post)
                (lambda (candidate) (push candidate merged))))
       (qq-api-fetch-guild-forum-page
        session-key "" (lambda (page) (setq delivered page)))
       (should (equal action "emacs_get_guild_forum_page"))
       (should (equal (alist-get 'cursor params) ""))
       (should (equal replaced (list session-key (list post))))
       (should-not merged)
       (should (equal (alist-get 'next_cursor delivered)
                      "page=synthetic-next"))
       (setq replaced nil delivered nil)
       (qq-api-fetch-guild-forum-page
        session-key "page=synthetic-next"
        (lambda (page) (setq delivered page)))
       (should-not replaced)
       (should (equal merged (list post)))
       (should (equal (alist-get 'cursor params) "page=synthetic-next"))))))

(ert-deftest qq-api-fetch-guild-navigation-applies-exact-channel-state ()
  (let (captured-action captured-params applied delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback
                          `((data
                             . ((chat . ,(alist-get 'chat params))
                                (unread_count . 6)
                                (begin_sequence . "18")
                                (navigation_sequences
                                 . (((sequence . "19")
                                     (native_kind . 7))))))))))
              ((symbol-function 'qq-state-apply-guild-navigation)
               (lambda (navigation) (setq applied navigation))))
      (qq-api-fetch-guild-navigation
       "guild:9007199254740993:channel:9007199254741999"
       (lambda (navigation) (setq delivered navigation)))
      (should (equal captured-action "emacs_get_guild_navigation"))
      (should (equal (alist-get 'chat captured-params)
                     '((kind . "guild-channel")
                       (guild_id . "9007199254740993")
                       (channel_id . "9007199254741999"))))
      (should (equal applied delivered))
      (should (= (alist-get 'unread_count delivered) 6)))))

(ert-deftest qq-api-mark-guild-read-requires-authoritative-zero-or-nonzero-result ()
  (let (captured-action applied)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action)
                 (funcall callback
                          `((data
                             . ((chat . ,(alist-get 'chat params))
                                (unread_count . 0)
                                (begin_sequence . "0")
                                (navigation_sequences)))))))
              ((symbol-function 'qq-state-apply-guild-navigation)
               (lambda (navigation) (setq applied navigation))))
      (qq-api-mark-guild-read
       "guild:9007199254740993:channel:9007199254741999")
      (should (equal captured-action "emacs_mark_guild_read"))
      (should (= (alist-get 'unread_count applied) 0)))))

(ert-deftest qq-api-fetch-guild-message-range-merges-only-matching-events ()
  (let ((expected (qq-api-test--guild-message-record))
        captured-action captured-params merged delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback
                          `((data
                             . ((messages
                                 . (,expected))))))))
              ((symbol-function 'qq-state-merge-guild-message)
               (lambda (event) (push event merged)))
              ((symbol-function 'qq-state-session)
               (lambda (_session-key) '((channel-kind . "text")))))
      (qq-api-fetch-guild-message-range
       "guild:9007199254740993:channel:9007199254741999"
       "10" "20" (lambda (messages) (setq delivered messages)))
      (should (equal captured-action "emacs_get_guild_messages_by_range"))
      (should (equal (alist-get 'channel_kind captured-params) "text"))
      (should (equal (alist-get 'start_sequence captured-params) "10"))
      (should (equal (alist-get 'end_sequence captured-params) "20"))
      (should (= (length merged) 1))
      (should (equal delivered (list expected)))
      (should (equal (alist-get 'post_type (car merged))
                     "emacs_guild_message")))))

(ert-deftest qq-api-send-message-rejects-read-only-service-session ()
  (let (pending-called api-called)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args)
                 (setq pending-called t)))
              ((symbol-function 'qq-api-call)
               (lambda (&rest _args)
                 (setq api-called t))))
      (should-error
       (qq-api-send-message
        "service:u_mail"
        '(((type . "text") (data . ((text . "hello"))))))
       :type 'user-error)
      (should-not pending-called)
      (should-not api-called))))

(ert-deftest qq-api-mark-message-read-applies-authoritative-post-state ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        captured-action
        captured-params
        success-fn)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((title . "Alice")
       (target-id . "10001")
       (unread-count . 3))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (setq success-fn callback)
                 'sent)))
      (qq-api-mark-message-read "private:10001" "9007199254741004645")
      (should (equal captured-action "emacs_mark_read"))
      (should (equal (alist-get 'chat captured-params)
                     '((kind . "private") (user_id . "10001"))))
      (should (equal (alist-get 'message_id captured-params)
                     "9007199254741004645"))
      (should (= 3 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (funcall success-fn
               (qq-api-test--mark-read-response
                "message" "9007199254741004645" 0))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (should-not (gethash "private:10001" qq-api--read-operations)))))

(ert-deftest qq-api-mark-read-response-cannot-overwrite-newer-observation ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        success-fn)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001" '((unread-count . 3)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (setq success-fn callback)
                 'sent)))
      (qq-api-mark-message-read
       "private:10001" "9007199254741004645")
      (let ((newer-token (qq-api--next-read-observation-token))
            (newer-state (copy-tree (qq-api-test--read-state))))
        (setf (alist-get 'unread_count newer-state) 7)
        (should
         (qq-api--accept-read-observation-p
          "private:10001" newer-token))
        (qq-state-apply-session-read-state
         "private:10001" newer-state))
      (funcall success-fn
               (qq-api-test--mark-read-response
                "message" "9007199254741004645" 0))
      (should (= 7 (alist-get 'unread-count
                              (qq-state-session "private:10001")))))))

(ert-deftest qq-api-mark-read-coalesces-reentrant-state-hook-intent ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        calls
        reentered)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001" '((unread-count . 3)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq calls
                       (append calls
                               (list (cons (alist-get 'message_id params)
                                           callback))))
                 (length calls))))
      (add-hook
       'qq-state-change-hook
       (lambda (event)
         (when (and (eq (plist-get event :type) 'session)
                    (eq (plist-get event :mutation) 'read)
                    (not reentered))
           (setq reentered t)
           (qq-api-mark-message-read
            "private:10001" "9007199254741004647"))))
      (qq-api-mark-message-read
       "private:10001" "9007199254741004645")
      ;; This intent is superseded by the still newer one issued reentrantly
      ;; from the synchronous read-state hook.
      (qq-api-mark-message-read
       "private:10001" "9007199254741004646")
      (funcall (cdar calls)
               (qq-api-test--mark-read-response
                "message" "9007199254741004645" 1))
      (should
       (equal (mapcar #'car calls)
              '("9007199254741004645" "9007199254741004647")))
      (let ((operation (gethash "private:10001" qq-api--read-operations)))
        (should (equal (plist-get operation :message-id)
                       "9007199254741004647"))
        (should-not (plist-get operation :next-message-id))))))

(ert-deftest qq-api-mark-service-read-requires-session-scoped-post-state ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        captured-params
        success-fn)
    (qq-state-reset)
    (qq-state-upsert-session
     "service:u_mail"
     '((title . "QQ Mail") (unread-count . 1))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params
                       success-fn callback)
                 'sent)))
      (qq-api-mark-message-read
       "service:u_mail" "9007199254741004645")
      (should
       (equal (alist-get 'chat captured-params)
              '((kind . "service") (peer_uid . "u_mail"))))
      (funcall success-fn
               (qq-api-test--mark-read-response
                "session" "9007199254741004645" 0))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "service:u_mail"))))
      (should-not (gethash "service:u_mail" qq-api--read-operations)))))

(ert-deftest qq-api-mark-dataline-read-requires-session-scoped-post-state ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        captured-params
        success-fn)
    (qq-state-reset)
    (qq-state-upsert-session
     "dataline:desktop:dev:a"
     '((title . "我的手机") (unread-count . 1))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params
                       success-fn callback)
                 'sent)))
      (qq-api-mark-message-read
       "dataline:desktop:dev:a" "9007199254741004645")
      (should
       (equal (alist-get 'chat captured-params)
              '((kind . "dataline")
                (peer_uid . "dev:a")
                (variant . "desktop"))))
      (funcall success-fn
               (qq-api-test--mark-read-response
                "session" "9007199254741004645" 0))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session
                               "dataline:desktop:dev:a"))))
      (should-not
       (gethash "dataline:desktop:dev:a" qq-api--read-operations)))))

(ert-deftest qq-api-mark-read-rejects-contradictory-success-scope ()
  (dolist (case '(("service:u_mail" . service)
                  ("dataline:desktop:dev:a" . dataline)))
    (let ((qq-state-change-hook nil)
          (qq-api--read-operations (make-hash-table :test #'equal))
          (qq-api--read-operation-counter 0)
          (qq-api--read-observation-clock 0)
          (qq-api--session-read-observation-tokens
           (make-hash-table :test #'equal))
          (session-key (car case))
          (session-type (cdr case))
          actions
          mark-success
          reported-error)
      (qq-state-reset)
      (qq-state-upsert-session
       session-key '((unread-count . 1)) nil)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (action _params callback &optional _errback)
                   (setq actions (append actions (list action)))
                   (when (equal action "emacs_mark_read")
                     (setq mark-success callback))
                   'sent))
                ((symbol-function 'qq-api--default-error)
                 (lambda (_response reason) (setq reported-error reason))))
        (qq-api-mark-message-read
         session-key "9007199254741004645")
        (funcall mark-success
                 (qq-api-test--mark-read-response
                  "message" "9007199254741004645" 0))
        (should (equal actions
                       '("emacs_mark_read" "emacs_get_read_state")))
        (should
         (string-match-p
          (format "message scope for %s" session-type) reported-error))
        (should (= 1 (alist-get 'unread-count
                                (qq-state-session session-key))))
        (should-not (gethash session-key qq-api--read-operations))))))

(ert-deftest qq-api-session-emacs-locator-tags-kernel-only-session-kinds ()
  (should
   (equal (qq-api--session-emacs-locator "dataline:mobile:dev:a")
          '((kind . "dataline")
            (peer_uid . "dev:a")
            (variant . "mobile"))))
  (should
   (equal (qq-api--session-emacs-locator "service:u:mail:x")
          '((kind . "service")
            (peer_uid . "u:mail:x")))))

(ert-deftest qq-api-session-key-from-locator-is-closed-and-lossless ()
  (dolist
      (case
       '((((kind . "group") (group_id . "20001")) . "group:20001")
         (((kind . "private") (user_id . "10001")) . "private:10001")
         (((kind . "guild-channel")
           (guild_id . "9007199254740993")
           (channel_id . "9007199254741999"))
          . "guild:9007199254740993:channel:9007199254741999")
         (((kind . "dataline")
           (peer_uid . "dev:a") (variant . "desktop"))
          . "dataline:desktop:dev:a")
         (((kind . "dataline")
           (peer_uid . "dev:a") (variant . "mobile"))
          . "dataline:mobile:dev:a")
         (((kind . "service") (peer_uid . "u:mail:x"))
          . "service:u:mail:x")))
    (should (equal (qq-api-session-key-from-locator (car case))
                   (cdr case))))
  (should-error
   (qq-api-session-key-from-locator
    '((kind . "private") (user_id . 10001))))
  (should-error
   (qq-api-session-key-from-locator
    '((kind . "private") (user_id . "0"))))
  (should-error
   (qq-api-session-key-from-locator
    '((kind . "private") (user_id . "010001"))))
  (should-error
   (qq-api-session-key-from-locator
    '((kind . "group") (group_id . "20001") (extra . t)))))

(ert-deftest qq-protocol-message-search-page-is-closed-and-chat-scoped ()
  (let ((page `((projection . "summary")
                (results . (,(qq-api-test--search-result)))
                (next_cursor . "opaque-1"))))
    (should (qq-protocol-emacs-message-search-page-p page))
    (should (equal (qq-protocol-validate-emacs-message-search-page page)
                   page))
    (should
     (qq-protocol-emacs-message-search-page-p
      '((projection . "summary")
        (results)
        (next_cursor . "opaque-empty"))))
    (let ((message-page
           `((projection . "message")
             (results . (,(qq-api-test--search-message-result)))
             (next_cursor))))
      (should (qq-protocol-emacs-message-search-page-p
               message-page 'message))
      (should-not (qq-protocol-emacs-message-search-page-p
                   message-page 'summary)))
    (dolist
        (invalid
         (list
          '((results) (next_cursor))
          '((projection . "unknown") (results) (next_cursor))
          (let ((result (qq-api-test--search-result)))
            (setf (alist-get 'message_id result) 9007199254742007089)
            `((projection . "summary") (results . (,result)) (next_cursor)))
          (let ((result (qq-api-test--search-result "09007199254742007089")))
            `((projection . "summary") (results . (,result)) (next_cursor)))
          (let ((result (qq-api-test--search-result)))
            (setf (alist-get 'message_seq result) 30001)
            `((projection . "summary") (results . (,result)) (next_cursor)))
          (let ((result (qq-api-test--search-result)))
            (setf (alist-get 'message_seq result) "030001")
            `((projection . "summary") (results . (,result)) (next_cursor)))
          `((projection . "summary")
            (results . (,(append (qq-api-test--search-result)
                                 '((extra . t)))))
            (next_cursor))
          '((projection . "summary")
            (results . (((chat . ((kind . "service")
                                  (peer_uid . "u_mail")))
                         (message_id . "1") (message_seq . "1")
                         (sent_at . 1)
                         (sender . ((user_id . "1") (name . "QQ")))
                         (preview . "mail"))))
            (next_cursor))
          '((projection . "summary")
            (results . (((chat . ((kind . "private") (user_id . "1")))
                         (message_id . "0") (message_seq . "1")
                         (sent_at . 1)
                         (sender . ((user_id . "1") (name . "QQ")))
                         (preview . "x"))))
            (next_cursor))
          '((projection . "summary")
            (results . (((chat . ((kind . "private") (user_id . "1")))
                         (message_id . "1") (message_seq . "1")
                         (sent_at . 1)
                         (sender . ((user_id . "1") (name . "")))
                         (preview . "x"))))
            (next_cursor))
          '((projection . "summary")
            (results . (((chat . ((kind . "private") (user_id . "1")))
                         (message_id . "1") (message_seq . "1")
                         (sent_at . 1)
                         (sender . ((user_id . "1") (name . "QQ")))
                         (preview . ""))))
            (next_cursor))
          (let ((result (qq-api-test--search-message-result)))
            (setq result (append result '((preview . "legacy wrapper"))))
            `((projection . "message")
              (results . (,result))
              (next_cursor)))))
      (should-not (qq-protocol-emacs-message-search-page-p invalid)))
    (should-not
     (qq-protocol-emacs-message-search-page-p page "summary"))
    (should-not
     (qq-protocol-emacs-message-search-page-p
      `((projection . "summary")
        (results . (,(qq-api-test--search-result) . malformed-tail))
        (next_cursor))))))

(ert-deftest qq-protocol-message-search-snapshot-is-strict-and-closed ()
  (let ((result (qq-api-test--search-message-result)))
    (should (qq-protocol-emacs-message-search-result-p result 'message))

    ;; Payload variants are decoded by the API layer, while the dependency-free
    ;; protocol boundary still requires their closed kind/payload envelope.
    (let ((candidate (copy-tree result)))
      (setf (alist-get 'segments candidate)
            '(((kind . "future") (payload . ((opaque . t))))))
      (should (qq-protocol-emacs-message-search-result-p candidate 'message)))

    (dolist (key '(chat message_id message_seq sent_at sender outgoing state
                       segments reactions))
      (let ((candidate (copy-tree result)))
        (setq candidate (assq-delete-all key candidate))
        (should-not
         (qq-protocol-emacs-message-search-result-p candidate 'message))))

    (let ((candidate (append (copy-tree result)
                             '((real_id . "9007199254742007089")))))
      (should-not
       (qq-protocol-emacs-message-search-result-p candidate 'message)))

    (dolist (case '((message_id . 9007199254742007089)
                    (message_id . "0")
                    (message_seq . 30001)
                    (message_seq . "0")))
      (let ((candidate (copy-tree result)))
        (setf (alist-get (car case) candidate) (cdr case))
        (should-not
         (qq-protocol-emacs-message-search-result-p candidate 'message))))

    (let ((candidate (copy-tree result)))
      (setf (alist-get 'segments candidate)
            '(((kind . "text") (payload . ((text . "x"))) (extra . t))))
      (should-not
       (qq-protocol-emacs-message-search-result-p candidate 'message)))

    (let ((candidate (copy-tree result)))
      (setf (alist-get 'segments candidate)
            (cons '((kind . "text") (payload . ((text . "x"))))
                  'improper-tail))
      (should-not
       (qq-protocol-emacs-message-search-result-p candidate 'message)))

    (let ((candidate (copy-tree result)))
      (setf (alist-get 'state candidate) "recalled"
            (alist-get 'segments candidate)
            '(((kind . "text") (payload . ((text . "x"))))))
      (should-not
       (qq-protocol-emacs-message-search-result-p candidate 'message)))

    (let ((candidate (copy-tree result)))
      (setf (alist-get 'reactions candidate)
            '(((emoji_id . "178") (emoji_type . "1")
               (count . 2) (chosen . :false) (extra . t))))
      (should-not
       (qq-protocol-emacs-message-search-result-p candidate 'message)))))

(ert-deftest qq-protocol-decimal-string-compare-never-coerces-precision ()
  (should (= -1 (qq-protocol-decimal-string-compare
                 "90071992547409931234" "90071992547409931235")))
  (should (= 1 (qq-protocol-decimal-string-compare
                "100000000000000000000" "99999999999999999999")))
  (should (= 0 (qq-protocol-decimal-string-compare "30001" "30001")))
  (should-error (qq-protocol-decimal-string-compare 30001 "30002")))

(ert-deftest qq-api-search-messages-start-trims-and-validates-closed-page ()
  (let ((result (qq-api-test--search-result
                 "900719925474099312345" 1710000000 "hello"))
        action params value)
    (setf (alist-get 'message_seq result) "900719925474099312346")
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall callback
                          `((data . ((projection . "summary")
                                     (results . (,result))
                                     (next_cursor . "cursor-1")))))
                 'request-1)))
      (should (eq (qq-api-search-messages-start
                   "group:20001" "  hello  "
                   (lambda (page) (setq value page)))
                  'request-1))
      (should (equal action "emacs_search_messages"))
      (should
       (equal params
              '((kind . "start")
                (projection . "summary")
                (chat . ((kind . "group") (group_id . "20001")))
                (query . "hello")
                (limit . 50))))
      (should (equal (alist-get 'next_cursor value) "cursor-1"))
      (let ((returned (car (alist-get 'results value))))
        (should (equal (alist-get 'message_id returned)
                       "900719925474099312345"))
        (should (equal (alist-get 'message_seq returned)
                       "900719925474099312346"))))))

(ert-deftest qq-api-search-messages-next-sends-opaque-cursor-directly ()
  (let (params value)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action candidate-params callback &optional _errback)
                 (setq params candidate-params)
                 (funcall callback
                          '((data . ((projection . "summary")
                                    (results)
                                    (next_cursor)))))
                 'request-2)))
      (qq-api-search-messages-next
       "group:20001" "opaque:cursor" 'summary
       (lambda (page) (setq value page)))
      (should
       (equal params
              '((kind . "next")
                (cursor . "opaque:cursor")
                (chat . ((kind . "group") (group_id . "20001")))
                (projection . "summary"))))
      (should (equal value '((projection . "summary")
                             (results)
                             (next_cursor)))))))

(ert-deftest qq-api-search-group-chats-start-sends-semantic-owner-and-validates-page ()
  (let (action params page)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall
                  callback
                  `((data
                     . ((results . (,(qq-api-test--group-chat-search-result)))
                        (multi_user_keywords . ("Alice"))
                        (next_cursor
                         . "00000000-0000-4000-8000-000000000000")))))
                 'group-search-request)))
      (should
       (eq (qq-api-search-group-chats-start
            "  Search Group  " (lambda (value) (setq page value))
            nil 'latest-created 25 '("u_native_alice"))
           'group-search-request))
      (should (equal action "emacs_search_group_chats"))
      (should
       (equal params
              '((kind . "start")
                (query . "Search Group")
                (sort . "latest_created")
                (limit . 25)
                (filter_member_uids . ("u_native_alice")))))
      (should (equal (alist-get 'next_cursor page)
                     "00000000-0000-4000-8000-000000000000"))
      (should
       (equal (alist-get 'group_id
                         (alist-get 'group (car (alist-get 'results page))))
              "20001")))))

(ert-deftest qq-api-search-group-chats-next-keeps-cursor-and-owner-exact ()
  (let (params page)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action candidate-params callback &optional _errback)
                 (setq params candidate-params)
                 (funcall callback
                          '((data . ((results)
                                     (multi_user_keywords)
                                     (next_cursor)))))
                 'next-request)))
      (qq-api-search-group-chats-next
       "00000000-0000-4000-8000-000000000000"
       "needle" (lambda (value) (setq page value))
       nil 'few-members 10 nil)
      (should
       (equal params
              '((kind . "next")
                (cursor . "00000000-0000-4000-8000-000000000000")
                (query . "needle")
                (sort . "few_members")
                (limit . 10))))
      (should (equal page '((results)
                            (multi_user_keywords)
                            (next_cursor)))))))

(ert-deftest qq-api-search-group-chats-rejects-lossy-and-open-results ()
  (dolist (mutation
           (list
            (lambda (result)
              (setf (alist-get 'group_id (alist-get 'group result)) 20001))
            (lambda (result)
              (setf (alist-get 'self_permission (alist-get 'group result))
                    "code-20"))
            (lambda (result)
              (setf (alist-get 'extra (alist-get 'group result)) t))))
    (let ((result (qq-api-test--group-chat-search-result))
          callback-called reason)
      (funcall mutation result)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (_action _params callback &optional _errback)
                   (funcall
                    callback
                    `((data . ((results . (,result))
                               (multi_user_keywords)
                               (next_cursor)))))
                   'request)))
        (qq-api-search-group-chats-start
         "query" (lambda (_page) (setq callback-called t))
         (lambda (_response error-reason) (setq reason error-reason)))
        (should-not callback-called)
        (should (stringp reason))))))

(ert-deftest qq-api-search-group-chats-member-count-is-exact-uint32 ()
  (let ((result
         (copy-tree
          '((group
            . ((group_id . "4294967295")
               (name . "Synthetic Group")
               (remark . "")
               (member_count . 4294967295)
               (is_conf . :false)
               (has_modify_conf_group_face . :false)
               (has_modify_conf_group_name . :false)
               (no_code_finger_open_flag . :false)
               (self_permission . "member")
               (hits . ((group_id) (name) (remark)))))
           (discussions)
           (member_profiles)
           (member_cards)
           (recall_reason . "synthetic")))))
    (should
     (qq-api--validate-group-chat-search-page
      `((results . (,result)) (multi_user_keywords) (next_cursor))))
    (setf (alist-get 'member_count (alist-get 'group result)) 4294967296)
    (should-error
     (qq-api--validate-group-chat-search-page
      `((results . (,result)) (multi_user_keywords) (next_cursor))))))

(ert-deftest qq-api-search-group-chats-rejects-inexact-hits-and-cursors ()
  (let ((result (qq-api-test--group-chat-search-result)))
    (setf (alist-get 'text
                     (car (alist-get
                           'name
                           (alist-get 'hits (alist-get 'group result)))))
          "Wrong")
    (should-error
     (qq-api--validate-group-chat-search-page
      `((results . (,result)) (multi_user_keywords) (next_cursor)))))
  (should-error
   (qq-api--validate-group-chat-search-page
    '((results) (multi_user_keywords) (next_cursor . "opaque"))))
  (should-error
   (qq-api-search-group-chats-next "opaque" "x" #'ignore)
   :type 'user-error))

(ert-deftest qq-api-search-contacts-friends-start-is-semantic-and-closed ()
  (let (action params page)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall
                  callback
                  `((data
                     . ((results
                         . (,(qq-api-test--contact-search-friend-result)))
                        (next_cursor . ,(qq-api-test--uuid-v4))))))
                 'contact-request)))
      (should
       (eq (qq-api-search-contacts-start
            'friends "  Alice   Maintainer "
            (lambda (value) (setq page value)))
           'contact-request))
      (should (equal action "emacs_search_contacts"))
      (should
       (equal params
              '((kind . "start")
                (scope . "friends")
                (query . "Alice   Maintainer")
                (limit . 50))))
      (should (equal (alist-get 'next_cursor page)
                     (qq-api-test--uuid-v4)))
      (let ((friend (car (alist-get 'results page))))
        (should (equal (alist-get 'kind friend) "friend"))
        (should (equal (alist-get 'user_id friend) "9007199254740993"))
        (should (stringp (alist-get 'user_id friend)))))))

(ert-deftest qq-api-search-contacts-group-scopes-send-required-owner ()
  (dolist
      (case
       `((group-members "group_members"
                        (,(qq-api-test--contact-search-group-member-result)))
         (friends-and-group-members
          "friends_and_group_members"
          (,(qq-api-test--contact-search-friend-result)
           ,(qq-api-test--contact-search-group-member-result)))))
    (let ((scope (nth 0 case))
          (wire-scope (nth 1 case))
          (results (nth 2 case))
          params page)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (_action candidate-params callback &optional _errback)
                   (setq params candidate-params)
                   (funcall callback
                            `((data . ((results . ,results)
                                       (next_cursor)))))
                   'contact-request)))
        (qq-api-search-contacts-start
         scope "Bob" (lambda (value) (setq page value))
         nil "4294967295" 25)
        (should
         (equal params
                `((kind . "start")
                  (scope . ,wire-scope)
                  (group_id . "4294967295")
                  (query . "Bob")
                  (limit . 25))))
        (should (= (length (alist-get 'results page)) (length results)))))))

(ert-deftest qq-api-search-contacts-next-repeats-complete-owner-proof ()
  (let (params page)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action candidate-params callback &optional _errback)
                 (should (equal action "emacs_search_contacts"))
                 (setq params candidate-params)
                 (funcall callback '((data . ((results) (next_cursor)))))
                 'contact-next-request)))
      (should
       (eq
        (qq-api-search-contacts-next
         'group-members (qq-api-test--uuid-v4) "  Bob Card "
         (lambda (value) (setq page value))
         nil "4294967295" 10)
        'contact-next-request))
      (should
       (equal params
              `((kind . "next")
                (cursor . ,(qq-api-test--uuid-v4))
                (scope . "group_members")
                (group_id . "4294967295")
                (query . "Bob Card")
                (limit . 10))))
      (should (equal page '((results) (next_cursor)))))))

(ert-deftest qq-api-search-contacts-rejects-invalid-owner-before-transport ()
  (let (transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) (setq transport-called t))))
      (dolist
          (form
           `((qq-api-search-contacts-start
              'friends "x" #'ignore nil "20001")
             (qq-api-search-contacts-start
              'group-members "x" #'ignore)
             (qq-api-search-contacts-start
              'friends-and-group-members "x" #'ignore nil "0")
             (qq-api-search-contacts-start
              'friends-and-group-members "x" #'ignore nil 20001)
             (qq-api-search-contacts-start 'unknown "x" #'ignore)
             (qq-api-search-contacts-start 'friends "   " #'ignore)
             (qq-api-search-contacts-start
              'friends ,(make-string 513 ?x) #'ignore)
             (qq-api-search-contacts-start 'friends "x" #'ignore nil nil 0)
             (qq-api-search-contacts-start 'friends "x" #'ignore nil nil 101)
             (qq-api-search-contacts-next
              'friends "not-a-uuid" "x" #'ignore)))
        (should-error (eval form t) :type 'user-error))
      (should-not transport-called))))

(ert-deftest qq-api-search-contacts-validates-exact-result-branch ()
  (dolist
      (result
       (list
        (let ((value (qq-api-test--contact-search-friend-result)))
          (setf (alist-get 'kind value) "unknown")
          value)
        (let ((value (qq-api-test--contact-search-friend-result)))
          (setf (alist-get 'group_id value) "20001")
          value)
        (assq-delete-all 'category_name
                         (qq-api-test--contact-search-friend-result))
        (let ((value (qq-api-test--contact-search-group-member-result)))
          (setf (alist-get 'kind value) "friend")
          value)
        (let ((value (qq-api-test--contact-search-group-member-result)))
          (setf (alist-get 'extra value) t)
          value)))
    (should-error
     (qq-api--validate-contact-search-page
      `((results . (,result)) (next_cursor))))))

(ert-deftest qq-api-search-contacts-rejects-duplicate-kind-and-identity ()
  (dolist (result (list (qq-api-test--contact-search-friend-result)
                        (qq-api-test--contact-search-group-member-result)))
    (should-error
     (qq-api--validate-contact-search-page
      `((results . (,result ,(copy-tree result))) (next_cursor)))))
  (let ((friend (qq-api-test--contact-search-friend-result))
        (member (qq-api-test--contact-search-group-member-result)))
    (setf (alist-get 'user_id member) (alist-get 'user_id friend)
          (alist-get 'user_id (alist-get 'hits member)) nil)
    (should
     (equal
      (length
       (alist-get
        'results
        (qq-api--validate-contact-search-page
         `((results . (,friend ,member)) (next_cursor)))))
      2))))

(ert-deftest qq-api-search-contacts-rejects-results-outside-request-owner ()
  (dolist
      (case
       `((friends nil ,(qq-api-test--contact-search-group-member-result))
         (group-members "4294967295"
                        ,(qq-api-test--contact-search-friend-result))
         (friends-and-group-members
          "4294967295"
          ,(qq-api-test--contact-search-group-member-result "42"))))
    (let ((scope (nth 0 case))
          (group-id (nth 1 case))
          (result (nth 2 case))
          callback-called reason)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (_action _params callback &optional _errback)
                   (funcall callback
                            `((data . ((results . (,result))
                                       (next_cursor)))))
                   'contact-request)))
        (qq-api-search-contacts-start
         scope "x" (lambda (_page) (setq callback-called t))
         (lambda (_response error-reason) (setq reason error-reason))
         group-id)
        (should-not callback-called)
        (should (string-match-p "owner\\|scope" reason))))))

(ert-deftest qq-api-search-contacts-rejects-lossy-identities-and-values ()
  (dolist
      (case
       (list
        (cons #'qq-api-test--contact-search-friend-result
              (lambda (value) (setf (alist-get 'user_id value) 10001)))
        (cons #'qq-api-test--contact-search-friend-result
              (lambda (value) (setf (alist-get 'user_id value) "010001")))
        (cons #'qq-api-test--contact-search-friend-result
              (lambda (value) (setf (alist-get 'uid value) "")))
        (cons #'qq-api-test--contact-search-friend-result
              (lambda (value) (setf (alist-get 'qid value) nil)))
        (cons #'qq-api-test--contact-search-group-member-result
              (lambda (value) (setf (alist-get 'group_id value) 20001)))
        (cons #'qq-api-test--contact-search-group-member-result
              (lambda (value) (setf (alist-get 'user_id value) "0")))
        (cons #'qq-api-test--contact-search-group-member-result
              (lambda (value) (setf (alist-get 'uid value) "")))
        (cons #'qq-api-test--contact-search-group-member-result
              (lambda (value) (setf (alist-get 'is_friend value) nil)))))
    (let ((result (funcall (car case))))
      (funcall (cdr case) result)
      (should-error
       (qq-api--validate-contact-search-page
        `((results . (,result)) (next_cursor)))))))

(ert-deftest qq-api-search-contacts-hits-must-match-exact-source-slice ()
  (dolist
      (hit
       '(((start . -1) (end . 5) (text . "Alice"))
         ((start . 0) (end . 0) (text . "Alice"))
         ((start . 0) (end . 99) (text . "Alice"))
         ((start . 0) (end . 5) (text . "Wrong"))
         ((start . 0) (end . 5) (text . ""))
         ((start . 0) (end . 5) (text . "Alice") (extra . t))))
    (let* ((result (qq-api-test--contact-search-friend-result))
           (hits (alist-get 'hits result)))
      (setf (alist-get 'nickname hits) (list hit))
      (should-error
       (qq-api--validate-contact-search-page
        `((results . (,result)) (next_cursor)))))))

(ert-deftest qq-api-search-contacts-hit-offsets-use-utf-16-code-units ()
  (let* ((result (qq-api-test--contact-search-friend-result))
         (hits (alist-get 'hits result)))
    (setf (alist-get 'nickname result) "A😀B"
          (alist-get 'nickname hits)
          '(((start . 1) (end . 3) (text . "😀"))))
    (should
     (equal
      (qq-api--validate-contact-search-page
       `((results . (,result)) (next_cursor)))
      `((results . (,result)) (next_cursor))))
    (setf (alist-get 'nickname hits)
          '(((start . 1) (end . 2) (text . "😀"))))
    (should-error
     (qq-api--validate-contact-search-page
      `((results . (,result)) (next_cursor))))))

(ert-deftest qq-api-search-contacts-page-and-cursor-are-strictly-closed ()
  (dolist
      (page
       (list
        `((results) (next_cursor . "opaque"))
        `((results) (next_cursor . "123E4567-E89B-42D3-A456-426614174000"))
        `((results) (next_cursor . ,(qq-api-test--uuid-v4)) (extra . t))
        `((results . (,(qq-api-test--contact-search-friend-result)
                       . malformed-tail))
          (next_cursor))))
    (should-error (qq-api--validate-contact-search-page page)))
  (should
   (equal
    (qq-api--validate-contact-search-page
     `((results) (next_cursor . ,(qq-api-test--uuid-v4))))
    `((results) (next_cursor . ,(qq-api-test--uuid-v4))))))

(ert-deftest qq-api-search-contacts-routes-schema-errors-to-errback ()
  (let (callback-called reason)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . ((results
                              . (((kind . "friend") (user_id . 10001))))
                             (next_cursor)))))
                 'contact-request)))
      (qq-api-search-contacts-start
       'friends "Alice" (lambda (_page) (setq callback-called t))
       (lambda (_response error-reason) (setq reason error-reason)))
      (should-not callback-called)
      (should (string-match-p "emacs_search_contacts" reason)))))

(ert-deftest qq-api-directory-consumer-errors-do-not-enter-protocol-errbacks ()
  (let* ((synthetic-group
          '((group
             . ((group_id . "4294967295")
                (name . "Synthetic Group")
                (remark . "")
                (member_count . 0)
                (is_conf . :false)
                (has_modify_conf_group_face . :false)
                (has_modify_conf_group_name . :false)
                (no_code_finger_open_flag . :false)
                (self_permission . "member")
                (hits . ((group_id) (name) (remark)))))
            (discussions)
            (member_profiles)
            (member_cards)
            (recall_reason . "synthetic")))
         (synthetic-friend
          '((kind . "friend")
            (user_id . "999999999999999999999999")
            (uid . "uid-synthetic-friend")
            (qid . "")
            (nickname . "Synthetic Friend")
            (remark . "")
            (category_name . "Synthetic Category")
            (hits . ((qid) (user_id) (nickname) (remark)))
            (recall_reason . "synthetic")))
         (synthetic-member
          '((user_id . "999999999999999999999998")
            (uid . "uid-synthetic-member")
            (nickname . "Synthetic Member")
            (card)
            (remark)
            (qid)
            (title)
            (role . "member")
            (robot . :false)))
         (consumer (lambda (_value) (error "synthetic consumer failure")))
         errback-calls
         (errback (lambda (&rest _args) (cl-incf errback-calls))))
    (should-error
     (qq-api--group-chat-search-callback
      consumer errback
      `((data . ((results . (,synthetic-group))
                  (multi_user_keywords)
                  (next_cursor))))))
    (should-error
     (qq-api--contact-search-callback
      '((scope . "friends") (query . "Synthetic") (limit . 50))
      consumer errback
      `((data . ((results . (,synthetic-friend)) (next_cursor))))))
    (should-error
     (qq-api--group-member-search-callback
      consumer errback `((data . (,synthetic-member)))))
    (should-not errback-calls)))

(ert-deftest qq-api-search-messages-rejects-cross-chat-results ()
  (let ((result (qq-api-test--search-result))
        callback-called
        reason)
    (setf (alist-get 'chat result)
          '((kind . "private") (user_id . "10001")))
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          `((data . ((projection . "summary")
                                     (results . (,result))
                                     (next_cursor)))))
                 'request)))
      (qq-api-search-messages-next
       "group:20001" "opaque:cursor" 'summary
       (lambda (_page) (setq callback-called t))
       (lambda (_response error-reason) (setq reason error-reason)))
      (should-not callback-called)
      (should (string-match-p "different chat" reason)))))

(ert-deftest qq-api-filter-messages-keeps-snapshots-out-of-history-cache ()
  (let ((qq-state-change-hook nil)
        (result (qq-api-test--search-message-result
                 "9007199254742007099" "filtered body"))
        action params value)
    (qq-state-reset)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (candidate-action candidate-params callback
                            &optional _errback)
                     (setq action candidate-action
                           params candidate-params)
                     (funcall
                      callback
                      `((data . ((projection . "message")
                                 (results . (,result))
                                 (next_cursor . "filter-next")))))
                     'filter-request)))
          (should
           (eq (qq-api-filter-messages-start
                "group:20001" " filtered "
                (lambda (page) (setq value page)))
               'filter-request))
          (should (equal action "emacs_search_messages"))
          (should
           (equal params
                  '((kind . "start")
                    (projection . "message")
                    (chat . ((kind . "group") (group_id . "20001")))
                    (query . "filtered")
                    (limit . 50))))
          (should (equal (alist-get 'projection value) "message"))
          (should-not (qq-state-session-messages "group:20001")))
      (qq-state-reset))))

(ert-deftest qq-api-filter-messages-next-keeps-cursor-opaque-and-message-shaped ()
  (let (params value)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action candidate-params callback &optional _errback)
                 (setq params candidate-params)
                 (funcall callback
                          '((data . ((projection . "message")
                                    (results)
                                    (next_cursor)))))
                 'filter-next-request)))
      (qq-api-filter-messages-next
       "private:10001" "opaque:filter" (lambda (page) (setq value page)))
      (should
       (equal params
              '((kind . "next")
                (cursor . "opaque:filter")
                (chat . ((kind . "private") (user_id . "10001")))
                (projection . "message"))))
      (should (equal (alist-get 'projection value) "message")))))

(ert-deftest qq-api-filter-next-validates-session-before-consuming-cursor ()
  (let (transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) (setq transport-called t))))
      (should-error
       (qq-api-filter-messages-next
        "service:u_mail" "single-use" #'ignore)
       :type 'user-error)
      (should-not transport-called))))

(ert-deftest qq-api-search-next-rejects-session-and-projection-before-transport ()
  (let (transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) (setq transport-called t))))
      (should-error
       (qq-api-search-messages-next
        "service:u_mail" "single-use" 'summary #'ignore)
       :type 'user-error)
      (should-error
       (qq-api-search-messages-next
        "group:20001" "single-use" "summary" #'ignore)
       :type 'user-error)
      (should-not transport-called))))

(ert-deftest qq-api-search-messages-rejects-unsupported-or-invalid-input ()
  (should-error
   (qq-api-search-messages-start "service:u_mail" "mail" #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-search-messages-start "group:20001" "   " #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-search-messages-start
    "group:20001" (make-string 513 ?x) #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-search-messages-next "group:20001" "" 'summary #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-search-messages-next
    "group:20001" "opaque:cursor" nil #'ignore)
   :type 'user-error))

(ert-deftest qq-api-search-messages-routes-schema-errors-to-errback ()
  (let (reason)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          '((data . ((projection . "summary")
                                     (results)
                                     (next_cursor)
                                     (extra . t)))))
                 'request)))
      (qq-api-search-messages-start
       "private:10001" "x" #'ignore
       (lambda (_response error-reason) (setq reason error-reason)))
      (should (string-match-p "closed message-search page" reason)))))

(ert-deftest qq-api-mark-message-read-failure-refreshes-authoritative-state ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        mark-errback
        fetch-success
        actions
        reported-error)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((type . private) (target-id . "10001") (unread-count . 5)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action _params callback &optional errback)
                 (setq actions (append actions (list action)))
                 (pcase action
                   ("emacs_mark_read" (setq mark-errback errback))
                   ("emacs_get_read_state" (setq fetch-success callback)))
                 'sent))
              ((symbol-function 'qq-api--default-error)
               (lambda (_response reason) (setq reported-error reason))))
      (qq-api-mark-message-read "private:10001" "9007199254741004645")
      (funcall mark-errback nil "network down")
      (should (equal actions '("emacs_mark_read" "emacs_get_read_state")))
      (should (equal reported-error "network down"))
      ;; A failure never reconstructs unread with local arithmetic.
      (should (= 5 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (let ((state (copy-tree (qq-api-test--read-state))))
        (setf (alist-get 'unread_count state) 7)
        (funcall fetch-success `((data . ,state))))
      (should (= 7 (alist-get 'unread-count
                              (qq-state-session "private:10001")))))))

(ert-deftest qq-api-mark-message-read-coalesces-to-newest-exact-target ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        calls
        params)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((type . private) (target-id . "10001") (unread-count . 2)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action call-params callback &optional errback)
                 (setq calls (append calls (list (cons callback errback)))
                       params (append params (list call-params)))
                 'sent)))
      (qq-api-mark-message-read "private:10001" "9007199254741004645")
      (qq-api-mark-message-read "private:10001" "9007199254741004646")
      (should (= (length calls) 1))
      (funcall (caar calls)
               (qq-api-test--mark-read-response
                "message" "9007199254741004645" 1))
      (should (= (length calls) 2))
      (should (equal (mapcar (lambda (it) (alist-get 'message_id it)) params)
                     '("9007199254741004645" "9007199254741004646")))
      (should (= 1 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (funcall (car (nth 1 calls))
               (qq-api-test--mark-read-response
                "message" "9007199254741004646" 0))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (should-not (gethash "private:10001" qq-api--read-operations)))))

(ert-deftest qq-api-mark-message-read-failure-preserves-later-intent-once ()
  (let ((qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        callbacks
        actions)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action _params callback &optional errback)
                 (setq actions (append actions (list action))
                       callbacks (append callbacks (list (cons callback errback))))
                 'sent))
              ((symbol-function 'qq-api--default-error) #'ignore))
      (qq-api-mark-message-read "private:10001" "9007199254741004645")
      (qq-api-mark-message-read "private:10001" "9007199254741004646")
      (funcall (cdr (nth 0 callbacks)) nil "network down")
      (should (equal actions
                     '("emacs_mark_read"
                       "emacs_get_read_state"
                       "emacs_mark_read")))
      ;; The later target begins with no inherited next target.  Its failure only
      ;; refreshes authoritative state and cannot recurse forever.
      (should-not (plist-get (gethash "private:10001" qq-api--read-operations)
                             :next-message-id))
      (funcall (cdr (nth 2 callbacks)) nil "still down")
      (should (= 4 (length actions)))
      (should (equal (car (last actions)) "emacs_get_read_state"))
      (should-not (gethash "private:10001" qq-api--read-operations)))))

(ert-deftest qq-api-send-text-builds-reply-segments ()
  (let (sent-session sent-segments sent-raw)
    (cl-letf (((symbol-function 'qq-api-send-message)
               (lambda (session-key segments &optional raw-message)
                 (setq sent-session session-key)
                 (setq sent-segments segments)
                 (setq sent-raw raw-message)
                 'sent)))
      (qq-api-send-text "private:10001" "hello" "42")
      (should (equal sent-session "private:10001"))
      (should (equal sent-raw "hello"))
      (should (equal sent-segments
                     '(((type . "reply")
                        (data . ((id . "42"))))
                       ((type . "text")
                        (data . ((text . "hello"))))))))))

(ert-deftest qq-api-fetch-older-history-uses-peer-history-for-dataline-sessions ()
  (let (captured-action captured-params merged done-meta)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "dataline:desktop:wrong")
                   (type . dataline)
                   (target-id . "wrong")
                   (chat-type . "8")
                   (peer-uid . "wrong"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (session-key messages &optional request-owner)
                 (setq merged (list session-key messages request-owner))
                 (list :session-key session-key
                       :message-count (length messages)
                       :added-count (length messages))))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 (funcall callback '((data . ((messages . (((message_id . 1))))))) )
                 'sent)))
      (qq-api-fetch-older-history "dataline:mobile:dev:a" nil
                            (lambda (meta) (setq done-meta meta)))
      (should (equal captured-action "get_peer_msg_history"))
      (should (equal (alist-get 'chat_type captured-params) "134"))
      (should (equal (alist-get 'peer_uid captured-params) "dev:a"))
      (should (equal (seq-take merged 2)
                     '("dataline:mobile:dev:a" (((message_id . 1))))))
      (should (equal (plist-get (nth 2 merged) :session-key)
                     "dataline:mobile:dev:a"))
      (should-not
       (qq-state-materialization-request-end (nth 2 merged)))
      (should (= (plist-get done-meta :added-count) 1)))))

(ert-deftest qq-api-history-around-preserves-mobile-colon-peer-identity ()
  (let ((params
         (qq-api--history-around-params
          "dataline:mobile:dev:a" "9007199254742007089" 17)))
    (should
     (equal params
            '((message_id . "9007199254742007089")
              (count . 17)
              (chat_type . "134")
              (peer_uid . "dev:a"))))))

(ert-deftest qq-api-fetch-session-read-state-returns-validated-raw-result ()
  (let (captured-action captured-params applied callback-value)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "group:20001")
                   (type . group)
                   (target-id . "20001")
                   (chat-type . "2")
                   (peer-uid . "20001"))))
              ((symbol-function 'qq-state-apply-session-read-state)
               (lambda (session-key read-state)
                 (setq applied (list session-key read-state))))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback
                          '((data . ((unread_count . 5)
                                     (first_unread
                                      . ((sequence . "30001")
                                         (message_id . "9007199254742007089")))
                                     (mentions
                                      . ((at_me . nil) (at_all . nil)))
                                     (latest . nil)))))
                 'sent)))
      (qq-api-fetch-session-read-state
       "group:20001"
       (lambda (read-state) (setq callback-value read-state)))
      (should (equal "emacs_get_read_state" captured-action))
      (should (equal '((kind . "group") (group_id . "20001"))
                     (alist-get 'chat captured-params)))
      (should-not applied)
      (should (= 5 (alist-get 'unread_count callback-value)))
      (should (equal "9007199254742007089"
                     (alist-get 'message_id
                                (alist-get 'first_unread callback-value)))))))

(ert-deftest qq-api-fetch-session-read-state-routes-lossy-wire-id-to-errback ()
  (let (applied callback-called error-response error-reason)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((type . group) (target-id . "20001"))))
              ((symbol-function 'qq-state-apply-session-read-state)
               (lambda (&rest _args) (setq applied t)))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data
                     . ((unread_count . 1)
                        (first_unread
                         . ((sequence . "30001")
                            (message_id . 9007199254742007089)))
                        (mentions . ((at_me . nil) (at_all . nil)))
                        (latest . nil)))))
                 'sent)))
      (qq-api-fetch-session-read-state
       "group:20001"
       (lambda (_state) (setq callback-called t))
       (lambda (response reason)
         (setq error-response response
               error-reason reason)))
      (should-not applied)
      (should-not callback-called)
      (should error-response)
      (should (string-match-p "authoritative read state" error-reason)))))

(ert-deftest qq-api-fetch-session-read-state-routes-consumer-error-to-errback ()
  (let (error-response error-reason)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((type . group) (target-id . "20001"))))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback `((data . ,(qq-api-test--read-state))))
                 'sent)))
      (qq-api-fetch-session-read-state
       "group:20001"
       (lambda (_state) (error "consumer exploded"))
       (lambda (response reason)
         (setq error-response response
               error-reason reason)))
      (should error-response)
      (should (equal error-reason "consumer exploded")))))

(ert-deftest qq-api-read-observation-newer-fetch-supersedes-older-recent ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        recent-success
        fetch-success)
    (qq-state-reset)
    (qq-state-upsert-session
     "group:20001"
     '((type . group) (target-id . "20001")
       (title . "Old") (unread-count . 4))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action _params callback &optional _errback)
                 (pcase action
                   ("get_recent_contact" (setq recent-success callback))
                   ("emacs_get_read_state" (setq fetch-success callback)))
                 'sent)))
      (qq-api-refresh-recent-contacts)
      (qq-api--refresh-session-read-state-after-failure "group:20001")
      (let ((state (copy-tree (qq-api-test--read-state))))
        (setf (alist-get 'unread_count state) 2)
        (funcall fetch-success `((data . ,state))))
      ;; The older recent request may still refresh metadata, but its unread
      ;; snapshot cannot overwrite the newer accepted read observation.
      (funcall recent-success
               '((data . (((chatType . 2)
                            (peerUid . "20001")
                            (peerUin . "20001")
                            (peerName . "Fresh title")
                            (msgTime . "1710000000")
                            (msgId . "9007199254741004991")
                            (msgSeq . "10001")
                            (lastMessagePreview . "latest")
                            (unreadCount . 9))))))
      (let ((session (qq-state-session "group:20001")))
        (should (= 2 (alist-get 'unread-count session)))
        (should (equal "Fresh title" (alist-get 'title session))))
      (should (= 2 (gethash "group:20001"
                            qq-api--session-read-observation-tokens))))))

(ert-deftest qq-api-read-observation-notice-supersedes-older-recent ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        recent-success)
    (qq-state-reset)
    (qq-state-upsert-session
     "group:20001"
     '((type . group) (target-id . "20001")
       (title . "Old") (unread-count . 4))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action _params callback &optional _errback)
                 (when (equal action "get_recent_contact")
                   (setq recent-success callback))
                 'sent)))
      (qq-api-refresh-recent-contacts)
      (let* ((notice
              (qq-api-test--read-state-notice
               '((kind . "group") (group_id . "20001"))))
             (state (alist-get 'read_state notice)))
        (setf (alist-get 'unread_count state) 0
              (alist-get 'first_unread state) nil
              (alist-get 'mentions state) '((at_me . nil) (at_all . nil)))
        (qq-api-handle-event notice))
      (funcall recent-success
               '((data . (((chatType . 2)
                            (peerUid . "20001")
                            (peerUin . "20001")
                            (peerName . "Fresh title")
                            (msgTime . "1710000000")
                            (msgId . "9007199254741004991")
                            (msgSeq . "10001")
                            (lastMessagePreview . "latest")
                            (unreadCount . 9))))))
      (let ((session (qq-state-session "group:20001")))
        (should (= 0 (alist-get 'unread-count session)))
        (should (equal "Fresh title" (alist-get 'title session))))
      (should (= 2 (gethash "group:20001"
                            qq-api--session-read-observation-tokens))))))

(ert-deftest qq-api-read-observation-newer-recent-supersedes-older-recent ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        callbacks)
    (qq-state-reset)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (setq callbacks (append callbacks (list callback)))
                 'sent)))
      (qq-api-refresh-recent-contacts)
      (qq-api-refresh-recent-contacts)
      (funcall (nth 1 callbacks)
               '((data . (((chatType . 2) (peerUid . "20001")
                            (peerUin . "20001") (unreadCount . 2)
                            (msgTime . "1710000002")
                            (msgId . "9007199254741004646")
                            (msgSeq . "10002")
                            (lastMessagePreview . "newer"))))))
      (funcall (nth 0 callbacks)
               '((data . (((chatType . 2) (peerUid . "20001")
                            (peerUin . "20001") (unreadCount . 9)
                            (msgTime . "1710000001")
                            (msgId . "9007199254741004645")
                            (msgSeq . "10001")
                            (lastMessagePreview . "older"))))))
      (should (= 2 (alist-get 'unread-count
                              (qq-state-session "group:20001"))))
      (should (equal "9007199254741004646"
                     (alist-get 'last-message-id
                                (qq-state-session "group:20001"))))
      (should (equal "newer"
                     (alist-get 'last-message-preview
                                (qq-state-session "group:20001")))))))

(ert-deftest qq-api-fetch-older-history-uses-peer-history-for-service-sessions ()
  (let (captured-action captured-params)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "service:wrong")
                   (type . service)
                   (target-id . "wrong")
                   (chat-type . "103")
                   (peer-uid . "wrong"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (&rest _) (list :added-count 0 :message-count 0)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((data . ((messages . nil)))))
                 'sent)))
      (qq-api-fetch-older-history "service:u:mail:x")
      (should (equal captured-action "get_peer_msg_history"))
      (should (equal (alist-get 'chat_type captured-params) "103"))
      (should (equal (alist-get 'peer_uid captured-params) "u:mail:x"))
      (should-not (alist-get 'user_id captured-params)))))

(ert-deftest qq-api-refresh-friend-categories-keeps-exact-ids-and-order ()
  (let ((qq-state-change-hook nil)
        action params callback-value)
    (qq-state-reset)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (candidate-action candidate-params callback
                            &optional _errback)
                     (setq action candidate-action
                           params candidate-params)
                     (funcall
                      callback
                      '((data
                         . ((categories
                             . (((category_id . 7)
                                 (sort_id . 20)
                                 (name . "工作")
                                 (online_count . 1)
                                 (friends
                                  . (((user_id . "90071992547409931")
                                      (nickname . "Alice")
                                      (remark . "A")))))
                                ((category_id . 3)
                                 (sort_id . 21)
                                 (name . "空分组")
                                 (online_count . 0)
                                 (friends))))))))
                     'friend-request)))
          (should
           (qq-api--snapshot-subscription-p
            (qq-api-refresh-friend-categories
             (lambda (categories) (setq callback-value categories)))))
          (should (equal action "emacs_get_friend_categories"))
          (should (equal params '((refresh . t))))
          (should (equal (mapcar (lambda (category)
                                   (alist-get 'category_id category))
                                 callback-value)
                         '(7 3)))
          (should (equal (alist-get 'user_id (car (qq-state-friends)))
                         "90071992547409931"))
          (should (equal (mapcar (lambda (category)
                                   (alist-get 'name category))
                                 (qq-state-friend-categories))
                         '("工作" "空分组"))))
      (qq-state-reset))))

(ert-deftest qq-api-directory-snapshots-serialize-overlapping-refreshes ()
  (let ((qq-state-change-hook nil)
        (qq-api--snapshot-active (make-hash-table :test #'eq))
        (qq-api--snapshot-queued (make-hash-table :test #'eq))
        calls callbacks)
    (qq-state-reset)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action _params callback &optional errback)
                 (setq calls
                       (append calls (list (list action callback errback))))
                 (intern (format "request-%d" (length calls))))))
      (qq-api-refresh-friend-categories
       (lambda (value) (push (list 'first value) callbacks)))
      (qq-api-refresh-friend-categories
       (lambda (value) (push (list 'second value) callbacks)))
      (qq-api-refresh-friend-categories
       (lambda (value) (push (list 'third value) callbacks)))
      (should (= (length calls) 1))
      (funcall
       (nth 1 (car calls))
       '((data
          . ((categories
              . (((category_id . 1) (sort_id . 1) (name . "旧")
                   (online_count . 0)
                   (friends . (((user_id . "10001")
                                (nickname . "Old") (remark)))))))))))
      (should (= (length calls) 2))
      (should (equal (mapcar #'car callbacks) '(first)))
      (should (equal (alist-get 'nickname (qq-state-friend "10001"))
                     "Old"))
      (funcall
       (nth 1 (nth 1 calls))
       '((data
          . ((categories
              . (((category_id . 1) (sort_id . 1) (name . "新")
                   (online_count . 1)
                   (friends . (((user_id . "10001")
                                (nickname . "New") (remark)))))))))))
      (should (equal (sort (mapcar #'car callbacks)
                           (lambda (left right)
                             (string< (symbol-name left) (symbol-name right))))
                     '(first second third)))
      (should (equal (alist-get 'nickname (qq-state-friend "10001"))
                     "New"))
      (should-not (gethash 'friend-categories qq-api--snapshot-active))
      (should-not (gethash 'friend-categories qq-api--snapshot-queued)))))

(ert-deftest qq-api-directory-snapshot-cancel-removes-only-one-subscriber ()
  (let ((qq-state-change-hook nil)
        (qq-api--snapshot-active (make-hash-table :test #'eq))
        (qq-api--snapshot-queued (make-hash-table :test #'eq))
        calls first-called second-called third-called)
    (qq-state-reset)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional errback)
                 (setq calls (append calls (list (cons callback errback))))
                 (format "request-%d" (length calls)))))
      (qq-api-refresh-friend-categories
       (lambda (_value) (setq first-called t)))
      (let ((cancelled
             (qq-api-refresh-friend-categories
              (lambda (_value) (setq second-called t)))))
        (qq-api-refresh-friend-categories
         (lambda (_value) (setq third-called t)))
        (qq-api-cancel-request cancelled))
      (funcall
       (caar calls)
       '((data . ((categories . (((category_id . 1) (sort_id . 1)
                                   (name . "A") (online_count . 0)
                                   (friends))))))))
      (should first-called)
      (should (= (length calls) 2))
      (funcall
       (car (nth 1 calls))
       '((data . ((categories . (((category_id . 1) (sort_id . 1)
                                   (name . "B") (online_count . 0)
                                   (friends))))))))
      (should-not second-called)
      (should third-called))))

(ert-deftest qq-api-directory-snapshot-cancel-cleans-orphaned-active-request ()
  (let ((qq-api--snapshot-active (make-hash-table :test #'eq))
        (qq-api--snapshot-queued (make-hash-table :test #'eq))
        cancelled)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) "wire-request"))
              ((symbol-function 'qq-transport-cancel)
               (lambda (token) (setq cancelled token))))
      (let ((active (qq-api-refresh-friend-categories #'ignore))
            queued)
        (setq queued (qq-api-refresh-friend-categories #'ignore))
        (qq-api-cancel-request active)
        (should (gethash 'friend-categories qq-api--snapshot-active))
        (qq-api-cancel-request queued)
        (should (equal cancelled "wire-request"))
        (should-not (gethash 'friend-categories qq-api--snapshot-active))
        (should-not (gethash 'friend-categories qq-api--snapshot-queued))))))

(ert-deftest qq-api-directory-snapshot-nil-dispatch-cannot-stick-active ()
  (let ((qq-api--snapshot-active (make-hash-table :test #'eq))
        (qq-api--snapshot-queued (make-hash-table :test #'eq))
        reason)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) nil)))
      (qq-api-refresh-friend-categories
       #'ignore (lambda (_response text) (setq reason text)))
      (should (string-match-p "did not return" reason))
      (should-not (gethash 'friend-categories qq-api--snapshot-active))
      (should-not (gethash 'friend-categories qq-api--snapshot-queued)))))

(ert-deftest qq-api-directory-snapshot-subscriber-error-does-not-block-queue ()
  (let ((qq-state-change-hook nil)
        (qq-api--snapshot-active (make-hash-table :test #'eq))
        (qq-api--snapshot-queued (make-hash-table :test #'eq))
        calls second-called)
    (qq-state-reset)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional errback)
                 (setq calls (append calls (list (cons callback errback))))
                 (format "wire-%d" (length calls))))
              ((symbol-function 'message) #'ignore))
      (qq-api-refresh-friend-categories
       (lambda (_value) (error "synthetic subscriber failure")))
      (qq-api-refresh-friend-categories
       (lambda (_value) (setq second-called t)))
      (funcall
       (caar calls)
       '((data . ((categories . (((category_id . 1) (sort_id . 1)
                                   (name . "First") (online_count . 0)
                                   (friends))))))))
      (should (= (length calls) 2))
      (funcall
       (car (nth 1 calls))
       '((data . ((categories . (((category_id . 2) (sort_id . 2)
                                   (name . "Second") (online_count . 0)
                                   (friends))))))))
      (should second-called)
      (should-not (gethash 'friend-categories qq-api--snapshot-active)))))

(ert-deftest qq-api-refresh-friend-categories-rejects-lossy-or-duplicate-ids ()
  (dolist
      (categories
       '((((category_id . 1) (sort_id . 1) (name . "A")
           (online_count . 0)
           (friends . (((user_id . 90071992547409931)
                        (nickname . "Alice") (remark))))) )
         (((category_id . 1) (sort_id . 1) (name . "A")
           (online_count . 0)
           (friends . (((user_id . "10001")
                        (nickname . "Alice") (remark)))))
          ((category_id . 2) (sort_id . 2) (name . "B")
           (online_count . 0)
           (friends . (((user_id . "10001")
                        (nickname . "Alice") (remark))))))))
    (let (callback-called reason)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (_action _params callback &optional _errback)
                   (funcall callback `((data . ((categories . ,categories)))))
                   'request)))
        (qq-api-refresh-friend-categories
         (lambda (_categories) (setq callback-called t))
         (lambda (_response error-reason) (setq reason error-reason)))
        (should-not callback-called)
        (should (stringp reason))))))

(ert-deftest qq-api-directory-snapshot-numbers-are-exact-uint32-values ()
  (let ((friend-data
         '((categories
            . (((category_id . 4294967295)
                (sort_id . 4294967295)
                (name . "Synthetic Category")
                (online_count . 4294967295)
                (friends))))))
        (group-data
         '((groups
            . (((group_id . "4294967295")
                (name . "Synthetic Group")
                (remark)
                (member_count . 4294967295)
                (max_member_count . 4294967295)
                (pinned . :false)
                (self_permission . "member")))))))
    (should (qq-api--validate-friend-categories-snapshot friend-data))
    (should (qq-api--validate-joined-groups-snapshot group-data))
    (dolist (field '(category_id sort_id online_count))
      (let* ((candidate (copy-tree friend-data))
             (category (car (alist-get 'categories candidate))))
        (setf (alist-get field category) 4294967296)
        (should-error
         (qq-api--validate-friend-categories-snapshot candidate))))
    (dolist (field '(member_count max_member_count))
      (let* ((candidate (copy-tree group-data))
             (group (car (alist-get 'groups candidate))))
        (setf (alist-get field group) 4294967296)
        (should-error (qq-api--validate-joined-groups-snapshot candidate))))))

(ert-deftest qq-api-refresh-joined-groups-normalizes-native-field-names ()
  (let ((qq-state-change-hook nil)
        action params callback-value)
    (qq-state-reset)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (candidate-action candidate-params callback
                            &optional _errback)
                     (setq action candidate-action
                           params candidate-params)
                     (funcall
                      callback
                      '((data
                         . ((groups
                             . (((group_id . "4294967295")
                                 (name . "Native name")
                                 (remark . "My remark")
                                 (member_count . 42)
                                 (max_member_count . 500)
                                 (pinned . :false)
                                 (self_permission . "admin"))))))))
                     'group-request)))
          (qq-api-refresh-joined-groups
           (lambda (groups) (setq callback-value groups)))
          (should (equal action "emacs_get_joined_groups"))
          (should (equal params '((refresh . t))))
          (should
           (equal callback-value
                  '(((group_id . "4294967295")
                     (group_name . "Native name")
                     (group_remark . "My remark")
                     (member_count . 42)
                     (max_member_count . 500)
                     (pinned . :false)
                     (self_permission . "admin")))))
          (should (equal (alist-get 'group_id (car (qq-state-groups)))
                         "4294967295")))
      (qq-state-reset))))

(ert-deftest qq-api-refresh-joined-groups-rejects-unknown-permission ()
  (let (callback-called reason)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data
                     . ((groups
                         . (((group_id . "20001")
                             (name . "Group") (remark)
                             (member_count . 1) (max_member_count . 500)
                             (pinned . :false)
                             (self_permission . "code-20"))))))))
                 'request)))
      (qq-api-refresh-joined-groups
       (lambda (_groups) (setq callback-called t))
       (lambda (_response error-reason) (setq reason error-reason)))
      (should-not callback-called)
      (should (string-match-p "self_permission" reason)))))

(ert-deftest qq-api-refresh-guild-directory-validates-and-applies-exact-identities ()
  (let ((qq-state-change-hook nil)
        action params callback-value)
    (qq-state-reset)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (candidate-action candidate-params callback
                            &optional _errback)
                     (setq action candidate-action
                           params candidate-params)
                     (funcall
                      callback
                      '((data
                         . ((guilds
                             . (((guild_id . "9007199254740993")
                                 (name . "Synthetic guild")
                                 (avatar_seq . "3")
                                 (pinned_at))))
                            (categories
                             . (((guild_id . "9007199254740993")
                                 (category_id . "0")
                                 (name . "")
                                 (uncategorized . t)
                                 (channel_ids
                                  . ("9007199254741999")))))
                            (channels
                             . (((guild_id . "9007199254740993")
                                 (channel_id . "9007199254741999")
                                 (guild_name . "Synthetic guild")
                                 (name . "General")
                                 (kind . "text")
                                 (avatar_seq . "4")
                                 (pinned_at . "1784000000")
                                 (latest_sequence . "23"))))))))
                     'guild-request)))
          (should
           (eq (qq-api-refresh-guild-directory
                (lambda (directory) (setq callback-value directory)))
               'guild-request))
          (should (equal action "emacs_get_guild_directory"))
          (should-not params)
          (should (equal callback-value (qq-state-guild-directory)))
          (should
           (equal (alist-get 'name
                             (qq-state-guild-channel
                              "9007199254740993" "9007199254741999"))
                  "General"))
          (should
           (equal (alist-get 'channel_ids
                             (car (qq-state-guild-categories
                                   "9007199254740993")))
                  '("9007199254741999"))))
      (qq-state-reset))))

(ert-deftest qq-api-get-guild-member-profile-preserves-native-identities ()
  (let (action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall callback
                          '((data
                             . ((guild_id . "9007199254740993")
                                (native_id . "144115219000000001")
                                (display_name . "Synthetic member")
                                (nickname . "Synthetic nickname")
                                (member_name . "Synthetic member")
                                (avatar_url
                                 . "https://example.invalid/avatar.jpg"))))))))
      (qq-api-get-guild-member-profile
       "9007199254740993"
       "144115219000000001"
       (lambda (profile) (setq delivered profile)))
      (should (equal action "emacs_get_guild_member_profile"))
      (should (equal params
                     '((guild_id . "9007199254740993")
                       (native_id . "144115219000000001"))))
      (should (equal (alist-get 'avatar_url delivered)
                     "https://example.invalid/avatar.jpg")))))

(ert-deftest qq-api-guild-gray-tip-segment-is-closed-and-semantic ()
  (let* ((event (qq-api-test--guild-message-event))
         (segments
          '(((kind . "gray-tip")
             (payload . ((gray_tip_kind . "revoke")
                         (text . "Synthetic member 撤回了一条消息")
                         (native_id . "144115219000000001")))))))
    (setf (alist-get 'segments event) segments)
    (should (equal (alist-get 'segments
                              (qq-api--validate-guild-message-event event))
                   segments))
    (setf (alist-get 'gray_tip_kind
                     (alist-get 'payload (car (alist-get 'segments event))))
          "unknown")
    (should-error (qq-api--validate-guild-message-event event))))

(ert-deftest qq-api-guild-directory-rejects-lossy-and-extra-fields ()
  (dolist
      (data
       '(((guilds . (((guild_id . 9007199254740992)
                      (name . "Guild") (avatar_seq . "1") (pinned_at))))
          (channels))
         ((guilds . (((guild_id . "9007199254740993")
                      (name . "Guild") (avatar_seq . "1") (pinned_at)
                      (extra . t))))
          (channels))
         ((guilds)
          (channels . (((guild_id . "9007199254740993")
                        (channel_id . "01") (guild_name . "Guild")
                        (name . "General") (kind . "text") (avatar_seq . "1")
                        (pinned_at) (latest_sequence . "0")))))))
    (should-error (qq-api--validate-guild-directory-snapshot data))))

(ert-deftest qq-api-guild-directory-rejects-unknown-channel-kind ()
  (should-error
   (qq-api--validate-guild-directory-snapshot
    '((guilds . (((guild_id . "9007199254740993")
                  (name . "Guild") (avatar_seq . "1") (pinned_at))))
      (categories . (((guild_id . "9007199254740993")
                      (category_id . "0") (name . "")
                      (uncategorized . t)
                      (channel_ids . ("9007199254741999")))))
      (channels . (((guild_id . "9007199254740993")
                    (channel_id . "9007199254741999")
                    (guild_name . "Guild") (name . "Forum")
                    (kind . "unknown") (avatar_seq . "1")
                    (pinned_at) (latest_sequence . "0"))))))))

(ert-deftest qq-api-guild-directory-requires-exact-category-coverage ()
  (let ((directory
         '((guilds . (((guild_id . "9007199254740993")
                       (name . "Guild") (avatar_seq . "1") (pinned_at))))
           (categories . (((guild_id . "9007199254740993")
                           (category_id . "0") (name . "")
                           (uncategorized . t)
                           (channel_ids . ("9007199254741999")))))
           (channels . (((guild_id . "9007199254740993")
                         (channel_id . "9007199254741999")
                         (guild_name . "Guild") (name . "General")
                         (kind . "text") (avatar_seq . "1")
                         (pinned_at) (latest_sequence . "0")))))))
    (should (equal (qq-api--validate-guild-directory-snapshot directory)
                   directory))
    (let ((bad (copy-tree directory)))
      (setf (alist-get 'channel_ids (car (alist-get 'categories bad)))
            '("9007199254742999"))
      (should-error (qq-api--validate-guild-directory-snapshot bad)))
    (let ((bad (copy-tree directory)))
      (setf (alist-get 'uncategorized (car (alist-get 'categories bad)))
            :false)
      (should-error (qq-api--validate-guild-directory-snapshot bad)))))

(ert-deftest qq-api-bootstrap-normalizes-contacts-after-identity-and-names ()
  (let (order)
    (cl-letf (((symbol-function 'qq-api-refresh-status)
               (lambda () (push 'status order)))
              ((symbol-function 'qq-api-refresh-login-info)
               (lambda (&optional callback _errback)
                 (push 'login order)
                 (funcall callback 'info)))
              ((symbol-function 'qq-api-refresh-friend-categories)
               (lambda (&optional callback _errback)
                 (push 'friends order)
                 (funcall callback 'friends)))
              ((symbol-function 'qq-api-refresh-joined-groups)
               (lambda (&optional callback _errback)
                 (push 'groups order)
                 (funcall callback 'groups)))
              ((symbol-function 'qq-api-refresh-guild-directory)
               (lambda (&optional callback _errback)
                 (push 'guilds order)
                 (funcall callback 'guilds)))
              ((symbol-function 'qq-api-refresh-recent-contacts)
               (lambda (&optional _callback _errback)
                 (push 'contacts order))))
      (qq-api-bootstrap)
      (should (equal (nreverse order)
                     '(status login friends groups guilds contacts))))))

(ert-deftest qq-api-handle-notice-dispatches-poke ()
  (let (received)
    (cl-letf (((symbol-function 'qq-state-apply-poke-notice)
               (lambda (notice)
                 (setq received notice))))
      (qq-api--handle-notice
       '((notice_type . "notify")
         (sub_type . "poke")
         (group_id . "20001")
         (user_id . "10001")
         (target_id . "90001")))
      (should (equal (alist-get 'sub_type received) "poke")))))

(ert-deftest qq-api-handle-notice-dispatches-gray-tip ()
  (let (received)
    (cl-letf (((symbol-function 'qq-state-apply-gray-tip-notice)
               (lambda (notice) (setq received notice))))
      (qq-api--handle-notice
       '((notice_type . "notify")
         (sub_type . "gray_tip")
         (group_id . 20001)
         (message_id . "9007199254750003456")
         (busi_id . "19366")))
      (should (equal (alist-get 'busi_id received) "19366")))))

(ert-deftest qq-api-handle-read-state-notice-applies-authoritative-state ()
  (let ((qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        applications)
    (cl-letf (((symbol-function 'qq-state-apply-session-read-state)
               (lambda (session-key read-state)
                 (push (list session-key read-state) applications))))
      (dolist
          (case
           '((((kind . "group") (group_id . "20001")) . "group:20001")
             (((kind . "private") (user_id . "10001")) . "private:10001")
             (((kind . "dataline")
               (peer_uid . "dev:a") (variant . "mobile"))
              . "dataline:mobile:dev:a")
             (((kind . "service") (peer_uid . "u:mail:x"))
              . "service:u:mail:x")))
        (qq-api-handle-event
         (qq-api-test--read-state-notice (car case)))
        (let ((application (car applications)))
          (should (equal (car application) (cdr case)))
          (should (equal (cadr application) (qq-api-test--read-state)))
          (should (equal
                   "9007199254742007089"
                   (alist-get 'message_id
                              (alist-get 'first_unread (cadr application)))))))
      (should (= 4 (length applications)))
      (should (= 4 qq-api--read-observation-clock)))))

(ert-deftest qq-api-read-notice-first-creates-lossless-mobile-session ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-observation-clock 0)
        (qq-api--session-read-observation-tokens
         (make-hash-table :test #'equal))
        (locator '((kind . "dataline")
                   (peer_uid . "dev:a")
                   (variant . "mobile"))))
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-api-handle-event (qq-api-test--read-state-notice locator))
          (let ((session (qq-state-session "dataline:mobile:dev:a")))
            (should session)
            (should (eq (alist-get 'type session) 'dataline))
            (should (equal (alist-get 'target-id session) "dev:a"))
            (should (equal (alist-get 'chat-type session) "134"))
            (should (equal (alist-get 'peer-uid session) "dev:a"))
            (should (equal (alist-get 'variant session) "mobile"))
            (should (= (alist-get 'unread-count session) 5))
            (should (equal
                     (qq-api--session-emacs-locator
                      "dataline:mobile:dev:a")
                     locator))))
      (qq-state-reset))))

(ert-deftest qq-api-handle-read-state-notice-rejects-old-or-lossy-shapes ()
  (let ((base
         (qq-api-test--read-state-notice
          '((kind . "private") (user_id . "10001"))))
        applied)
    (cl-letf (((symbol-function 'qq-state-apply-session-read-state)
               (lambda (&rest _args) (setq applied t))))
      (let* ((numeric (copy-tree base))
             (read-state (alist-get 'read_state numeric))
             (first (alist-get 'first_unread read-state)))
        (setf (alist-get 'message_id first) 9007199254742007089)
        (dolist
            (invalid
             (list
              numeric
              (append (copy-tree base) '((extra . t)))
              (assq-delete-all 'read_state (copy-tree base))
              `((post_type . "notice")
                (notice_type . "emacs_read_state")
                (chat . ((kind . "private") (user_id . "10001")))
                (read_state . ,(qq-api-test--read-state)))))
          (should-error (qq-api-handle-event invalid))
          (should-not applied))))))

(ert-deftest qq-api-handle-notice-dispatches-emoji-like ()
  (let (received-session received)
    (cl-letf (((symbol-function 'qq-state-apply-emoji-like-notice)
               (lambda (session-key notice)
                 (setq received-session session-key
                       received notice))))
      (qq-api--handle-notice
       '((notice_type . "group_msg_emoji_like")
         (group_id . "20001")
         (message_id . "9007199254741004001")
         (likes . (((emoji_id . "178") (count . 2))))))
      (should (equal received-session "group:20001"))
      (should (equal (alist-get 'message_id received)
                     "9007199254741004001")))))

(ert-deftest qq-api-set-message-emoji-like-preserves-snowflake-and-optimistically-applies ()
  (let (captured-action captured-params applied-session applied callback-called)
    (cl-letf (((symbol-function 'qq-state-self-user-id)
               (lambda () "90001"))
              ((symbol-function 'qq-state-apply-emoji-like-notice)
               (lambda (session-key notice)
                 (setq applied-session session-key
                       applied notice)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent)))
      (qq-api-set-message-emoji-like
       (qq-api-test--message-reference
        'group "20001" "9007199254741004001")
       "178" nil
       (lambda (_response) (setq callback-called t)))
      (should (equal captured-action "set_msg_emoji_like"))
      (should (equal (alist-get 'message_id captured-params)
                     "9007199254741004001"))
      (should (equal (alist-get 'emoji_id captured-params) "178"))
      (should (eq (alist-get 'set captured-params) :false))
      (should (equal applied-session "group:20001"))
      (should (equal (alist-get 'user_id applied) "90001"))
      (should (eq (alist-get 'is_add applied) :false))
      (should callback-called))))

(ert-deftest qq-api-send-poke-builds-group-request-and-local-notice ()
  (let (captured-action captured-params applied)
    (qq-state-reset)
    (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
    (qq-state-upsert-session
     "group:20001" '((type . group) (target-id . "20001")))
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent))
              ((symbol-function 'qq-state-apply-poke-notice)
               (lambda (notice)
                 (setq applied notice))))
      (qq-api-send-poke "group:20001" "10002")
      (should (equal captured-action "send_poke"))
      (should (equal captured-params
                     '((group_id . "20001")
                       (user_id . "10002")
                       (target_id . "10002"))))
      (should (equal (alist-get 'target_id applied) "10002"))
      (should (equal (alist-get 'user_id applied) "90001")))))

(ert-deftest qq-api-send-poke-builds-private-peer-request-and-local-notice ()
  (let (captured-params applied)
    (qq-state-reset)
    (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
    (qq-state-upsert-session
     "private:10002" '((type . private) (target-id . "10002")))
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent))
              ((symbol-function 'qq-state-apply-poke-notice)
               (lambda (notice)
                 (setq applied notice))))
      (qq-api-send-poke "private:10002" "10002")
      (should (equal captured-params
                     '((user_id . "10002") (target_id . "10002"))))
      (should (equal (alist-get 'user_id applied) "10002"))
      (should (equal (alist-get 'sender_id applied) "90001"))
      (should (equal (alist-get 'target_id applied) "10002")))))

(ert-deftest qq-api-send-poke-builds-private-self-request-and-local-notice ()
  (let (captured-params applied)
    (qq-state-reset)
    (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
    (qq-state-upsert-session
     "private:10002" '((type . private) (target-id . "10002")))
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent))
              ((symbol-function 'qq-state-apply-poke-notice)
               (lambda (notice)
                 (setq applied notice))))
      (qq-api-send-poke "private:10002" "90001")
      (should (equal captured-params
                     '((user_id . "10002") (target_id . "90001"))))
      (should (equal (alist-get 'user_id applied) "10002"))
      (should (equal (alist-get 'sender_id applied) "90001"))
      (should (equal (alist-get 'target_id applied) "90001")))))

(ert-deftest qq-api-send-poke-rejects-invalid-targets-before-transport ()
  (let ((transport-called nil))
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10002" '((type . private) (target-id . "10002")))
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args)
                 (setq transport-called t))))
      (should-error (apply #'qq-api-send-poke '("private:10002"))
                    :type 'wrong-number-of-arguments)
      (dolist (target '(nil "" "0" "00" "not-a-uin" 90001))
        (should-error (qq-api-send-poke "private:10002" target)
                      :type 'user-error))
      (should-not transport-called))))

(ert-deftest qq-api-send-poke-requires-a-valid-stored-session-peer ()
  (qq-state-reset)
  (should-error (qq-api-send-poke "private:10002" "10002")
                :type 'user-error)
  (should-error
   (qq-state-upsert-session
    "private:invalid" '((type . private) (target-id . "invalid"))))
  (qq-state-upsert-session
   "group:0" '((type . group) (target-id . "0")))
  (should-error (qq-api-send-poke "group:0" "10002")
                :type 'user-error))

(ert-deftest qq-api-recall-poke-uses-dedicated-action ()
  (let* ((reference
          '((message_id . "9007199254741007777")
            (peer . ((chat_type . 2)
                     (peer_uid . "20001")
                     (guild_id . "")))
            (valid_before . 4102444800)))
         captured-action captured-params recalled-session recalled-id)
    (cl-letf (((symbol-function 'qq-state-apply-recall)
               (lambda (session-key message-id)
                 (setq recalled-session session-key
                       recalled-id message-id)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent)))
      (qq-api-recall-poke "group:20001" reference)
      (should (equal captured-action "recall_poke"))
      (should (equal captured-params
                     `((recall_reference . ,reference))))
      (should (equal recalled-session "group:20001"))
      (should (equal recalled-id "9007199254741007777"))
      (should-error (qq-api-recall-poke
                     "group:20001" "9007199254741007777")
                    :type 'user-error))))

(ert-deftest qq-api-recall-poke-rejects-expiry-before-transport ()
  (let ((reference
         '((message_id . "9007199254741007777")
           (peer . ((chat_type . 2)
                    (peer_uid . "20001")
                    (guild_id . "")))
           (valid_before . 200)))
        transport-called)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 200))
              ((symbol-function 'qq-api-call)
               (lambda (&rest _args)
                 (setq transport-called t))))
      (should-error (qq-api-recall-poke "group:20001" reference)
                    :type 'user-error))
    (should-not transport-called)))

(ert-deftest qq-api-fetch-older-history-passes-message-seq-for-older-page ()
  (let (captured-params)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "private:10001")
                   (type . private)
                   (target-id . "10001")
                   (peer-uin . "10001"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (&rest _)
                 (list :added-count 0 :message-count 0)))
              ((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params)
                 (funcall callback '((data . ((messages . nil)))))
                 'sent)))
      (qq-api-fetch-older-history "private:10001" "9007199254741004999")
      (should (equal (alist-get 'message_seq captured-params)
                     "9007199254741004999"))
      (should (eq (alist-get 'reverse_order captured-params) t))
      (should (equal (alist-get 'user_id captured-params) "10001")))))

(ert-deftest qq-api-fetch-history-page-encodes-newer-direction-as-json-false ()
  (let (captured-params)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "group:20001")
                   (type . group)
                   (target-id . "20001"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (&rest _) (list :added-count 0 :message-count 0)))
              ((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (setq captured-params params)
                 (funcall callback '((data . ((messages . nil))))))))
      (qq-api-fetch-history-page "group:20001" "9007199254742007089" 'newer)
      (should (equal "9007199254742007089"
                     (alist-get 'message_seq captured-params)))
      (should (assq 'reverse_order captured-params))
      (should (eq :false (alist-get 'reverse_order captured-params))))))

(ert-deftest qq-api-delete-message-calls-delete-msg-and-applies-recall ()
  (let (captured-action captured-params recalled-session recalled-id)
    (cl-letf (((symbol-function 'qq-state-apply-recall)
               (lambda (session-key message-id)
                 (setq recalled-session session-key
                       recalled-id message-id)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent)))
      (qq-api-delete-message
       (qq-api-test--message-reference 'private "10001" "77"))
      (should (equal captured-action "delete_msg"))
      (should (equal (alist-get 'message_id captured-params) "77"))
      (should (equal recalled-session "private:10001"))
      (should (equal recalled-id "77"))
      (should-error
       (qq-api-delete-message
        (qq-api-test--message-reference 'private "10001" 77))
                    :type 'user-error))))

(ert-deftest qq-api-message-mutations-reject-invalid-reference-before-transport ()
  (let (transport-called)
    (qq-state-reset)
    (unwind-protect
        (progn
          (puthash "77" "private:10001" qq-state--message-session-index)
          (cl-letf (((symbol-function 'qq-api-call)
                     (lambda (&rest _args) (setq transport-called t))))
            (should-error
             (qq-api-delete-message
              (qq-api-test--message-reference 'group "20001" "77")))
            (should-error
             (qq-api-set-message-emoji-like
              (qq-api-test--message-reference 'group "20001" "77")
              "178" t))
            (should-error
             (qq-api-delete-message
              (append
               (qq-api-test--message-reference 'private "10001" "78")
               '((extra . t))))
             :type 'user-error)
            (should-error
             (qq-api-set-message-emoji-like
              (qq-api-test--message-reference 'private "10001" "78")
              "178" t)
             :type 'user-error)
            (should-not transport-called)))
      (qq-state-reset))))

(ert-deftest qq-api-native-get-forward-sends-explicit-source-union ()
  (let* ((messages
          (list (qq-api-test--native-message
                 "1.1" "9007199254742007089")))
         calls values)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (push (list action params) calls)
                 (funcall callback
                          `((data . ((messages . ,messages))))))))
      (qq-api-get-forward
       '((kind . "message")
         (message_id . "9007199254742007089")
         (chat . ((kind . "group") (group_id . "20001"))))
       (lambda (result) (push result values)))
      (qq-api-get-forward
       '((kind . "resource") (resource_id . "resource-a"))
       (lambda (result) (push result values))))
    (setq calls (nreverse calls))
    (should (equal (mapcar #'car calls)
                   '("emacs_get_forward" "emacs_get_forward")))
    (should
     (equal
      (alist-get 'source (cadr (car calls)))
      '((kind . "message")
        (message_id . "9007199254742007089")
        (chat . ((kind . "group") (group_id . "20001"))))))
    (should
     (equal (alist-get 'source (cadr (cadr calls)))
            '((kind . "resource") (resource_id . "resource-a"))))
    (should (equal values (list messages messages)))))

(ert-deftest qq-api-native-get-forward-preserves-context-source-exactly ()
  (let ((source (qq-api-test--context-source))
        captured-action captured-params callback-called delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((data . ((messages . nil)))))
                 'sent)))
      (qq-api-get-forward
       source
       (lambda (messages)
         (setq callback-called t
               delivered messages))))
    (should (equal captured-action "emacs_get_forward"))
    (should (equal captured-params `((source . ,source))))
    (should callback-called)
    (should-not delivered)))

(ert-deftest qq-api-native-get-forward-keeps-both-dataline-message-sources ()
  (let (captured-sources)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params callback &optional _errback)
                 (push (alist-get 'source params) captured-sources)
                 (funcall callback '((data . ((messages . nil))))))))
      (dolist (variant '("desktop" "mobile"))
        (qq-api-get-forward
         `((kind . "message")
           (message_id . "9007199254742007089")
           (chat . ((kind . "dataline")
                    (peer_uid . "dev:a")
                    (variant . ,variant))))
         #'ignore)))
    (should
     (equal
      (mapcar
       (lambda (source)
         (alist-get 'variant (alist-get 'chat source)))
       (nreverse captured-sources))
      '("desktop" "mobile")))))

(ert-deftest qq-api-resolve-video-sends-only-the-exact-native-resolver ()
  (let* ((resolver
          '((kind . "message")
            (peer . ((chat_type . 2)
                     (peer_uid . "20001")
                     (guild_id . "")))
            (message_id . "9007199254745006083")
            (element_id . "9007199254745006082")))
         action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (called-action called-params callback &optional _errback)
                 (setq action called-action params called-params)
                 (funcall callback
                          '((data . ((state . "available")
                                     (url . "https://video.example/manual")))))
                 'sent)))
      (should
       (eq 'sent
           (qq-api-resolve-video
            resolver (lambda (remote) (setq delivered remote))))))
    (should (equal action "emacs_resolve_video"))
    (should (equal params `((resolver . ,resolver))))
    (should
     (equal delivered
            '((state . "available")
              (url . "https://video.example/manual"))))))

(ert-deftest qq-api-video-resolver-and-result-unions-are-closed ()
  (let ((snapshot
         '((kind . "snapshot")
           (peer . ((chat_type . 2)
                    (peer_uid . "20001")
                    (guild_id . "")))
           (file_uuid . "native-file-uuid"))))
    (should (equal (qq-api-validate-video-resolver snapshot) snapshot))
    (dolist (invalid
             (list
              (append (copy-tree snapshot) '((message_id . "1")))
              '((kind . "message")
                (peer . ((chat_type . 2)
                         (peer_uid . "20001")
                         (guild_id . "")))
                (message_id . 9007199254745006083)
                (element_id . "9007199254745006082"))
              '((kind . "message")
                (peer . ((chat_type . 2)
                         (peer_uid . "20001")
                         (guild_id . "")))
                (message_id . "9007199254745006083")
                (element_id . "0"))
              '((kind . "snapshot")
                (peer . ((chat_type . 2)
                         (peer_uid . "")
                         (guild_id . "")))
                (file_uuid . "native-file-uuid"))))
      (should-error
       (qq-api-validate-video-resolver invalid)
       :type 'user-error)))
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          '((data . ((state . "resolvable")
                                     (resolver . nil))))))))
      (qq-api-resolve-video
       '((kind . "snapshot")
         (peer . ((chat_type . 2)
                  (peer_uid . "20001")
                  (guild_id . "")))
         (file_uuid . "native-file-uuid"))
       (lambda (remote) (setq delivered remote))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (string-match-p "cannot remain resolvable" failure))))

(ert-deftest qq-api-forward-context-source-schema-is-closed ()
  (let ((source (qq-api-test--context-source)))
    (should (equal (qq-api-validate-forward-source source) source))
    (dolist (invalid
             (list
              '((kind . "context")
                (peer . ((chat_type . 2)
                         (peer_uid . "u_group-peer")
                         (guild_id . "")))
                (root_message_id . "9007199254742007001"))
              (append (copy-tree source) '((message_id . "legacy")))
              (let ((value (copy-tree source)))
                (setf (alist-get 'peer value)
                      '((chat_type . 2) (peer_uid . "u_group-peer")))
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'legacy (alist-get 'peer value)) t)
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'chat_type (alist-get 'peer value)) 0)
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'chat_type (alist-get 'peer value)) 2.5)
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'peer_uid (alist-get 'peer value)) "")
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'guild_id (alist-get 'peer value)) nil)
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'root_message_id value) "000")
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'root_message_id value) "0001")
                value)
              (let ((value (copy-tree source)))
                (setf (alist-get 'parent_message_id value)
                      9007199254742007002)
                value)))
      (should-error
       (qq-api-validate-forward-source invalid)
       :type 'user-error))
    (let ((integral-float (copy-tree source)))
      (setf (alist-get 'chat_type (alist-get 'peer integral-float)) 2.0)
      (should (equal (qq-api-validate-forward-source integral-float) source)))))

(ert-deftest qq-api-native-get-forward-rejects-numeric-alias-and-bad-result ()
  (dolist (source
           '(((kind . "message")
              (message_id . 9007199254742007089)
              (chat . ((kind . "group") (group_id . "20001"))))
             ((kind . "message")
              (message_id . "9007199254742007089")
              (group_id . "20001"))
             ((kind . "resource") (res_id . "legacy"))))
    (should-error (qq-api-get-forward source #'ignore) :type 'user-error))
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          '((data . ((messages . "not-an-array"))))))))
      (qq-api-get-forward
       '((kind . "resource") (resource_id . "resource-a"))
       (lambda (_) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (stringp failure))))

(ert-deftest qq-api-message-id-requires-canonical-nonzero-decimal-string ()
  (should
   (equal (qq-api-validate-message-id "9007199254742007089")
          "9007199254742007089"))
  (dolist (value '("" "0" "00" "01" "1.0" "-1"
                   9007199254742007089))
    (should-error (qq-api-validate-message-id value) :type 'user-error)))

(ert-deftest qq-api-native-message-validator-separates-entry-and-message-id ()
  (should
   (qq-api-validate-native-forward-messages
    (list (qq-api-test--native-message "1.2" nil))))
  (dolist (messages
           (list
            (list (qq-api-test--native-message "not-entry" nil))
            (list (qq-api-test--native-message
                   "1.2" 9007199254742007089))
            (list (qq-api-test--native-message "1.2" nil)
                  (qq-api-test--native-message "1.2" nil))))
    (should-error
     (qq-api-validate-native-forward-messages messages)
     :type 'user-error)))

(ert-deftest qq-api-native-message-state-requires-matching-segment-cardinality ()
  (let ((live (qq-api-test--native-message "1" nil))
        (recalled (qq-api-test--native-message "2" nil)))
    (setf (alist-get 'state recalled) "recalled"
          (alist-get 'segments recalled) nil)
    (should
     (equal (qq-api-validate-native-forward-messages (list live recalled))
            (list live recalled))))
  (let ((live-empty (qq-api-test--native-message "1" nil))
        (recalled-nonempty (qq-api-test--native-message "2" nil)))
    (setf (alist-get 'segments live-empty) nil
          (alist-get 'state recalled-nonempty) "recalled")
    (should-error
     (qq-api-validate-native-forward-messages (list live-empty))
     :type 'user-error)
    (should-error
     (qq-api-validate-native-forward-messages (list recalled-nonempty))
     :type 'user-error)))

(ert-deftest qq-api-native-forward-card-presentation-is-closed-and-may-be-empty ()
  (let ((message
         (qq-api-test--native-message
          "1" nil
          '(((kind . "forward-card")
             (payload
              . ((reference . ((kind . "resource")
                                (resource_id . "resource-a")))
                 (presentation . nil))))))))
    (should (qq-api-validate-native-forward-messages (list message)))
    (setf (alist-get 'presentation
                     (alist-get 'payload
                                (car (alist-get 'segments message))))
          '((title . "History") (url . "legacy-extra")))
    (should-error
     (qq-api-validate-native-forward-messages (list message))
     :type 'user-error)))

(ert-deftest qq-api-native-forward-card-accepts-only-card-source-union ()
  (dolist (source
           (list
            '((kind . "resource") (resource_id . "resource-a"))
            (qq-api-test--context-source)))
    (should
     (qq-api-validate-native-forward-messages
      (list
       (qq-api-test--native-message
        "1" nil
        `(((kind . "forward-card")
           (payload . ((reference . ,source)
                       (presentation . nil))))))))))
  (should-error
   (qq-api-validate-native-forward-messages
    (list
     (qq-api-test--native-message
      "1" nil
      '(((kind . "forward-card")
         (payload
          . ((reference
              . ((kind . "message")
                 (message_id . "9007199254742007089")
                 (chat . ((kind . "group") (group_id . "20001")))))
             (presentation . nil))))))))
   :type 'user-error))

(ert-deftest qq-api-native-segment-payloads-match-closed-typebox-schemas ()
  (let ((valid
         '(((kind . "text") (payload . ((text . "hello"))))
           ((kind . "at") (payload . ((qq . "all") (name . "everyone"))))
           ((kind . "image")
            (payload . ((file . "image.png") (file_id . "download-token")
                        (file_size . 1.5) (summary . "photo")
                        (sub_type . 0))))
           ((kind . "file")
            (payload . ((file . "report.pdf") (file_size . "42"))))
           ((kind . "record")
            (payload . ((file . "voice.amr") (file_id . "voice-token"))))
           ((kind . "face")
            (payload . ((id . "178") (description . "/斜眼笑")
                        (face_type . 3) (sticker_id . "80")
                        (sticker_pack_id . "4") (sticker_type . 1)
                        (resultId . "ok")
                        (chainCount . 2))))
           ((kind . "mface")
            (payload . ((emoji_package_id . 1) (emoji_id . "2")
                        (key . "key") (summary . "sticker"))))
           ((kind . "mail") (payload . nil))
           ((kind . "video")
            (payload
             . ((file . "video.mp4")
                (remote
                 . ((state . "resolvable")
                    (resolver
                     . ((kind . "snapshot")
                        (peer . ((chat_type . 2)
                                 (peer_uid . "20001")
                                 (guild_id . "")))
                        (file_uuid . "native-file-uuid"))))))))
           ((kind . "music")
            (payload . ((provider . "qq") (nested . [1 "two"])))))))
    (dolist (segment valid)
      (should
       (qq-api-validate-native-forward-messages
        (list (qq-api-test--native-message "1" nil (list segment)))))))
  (let ((infinity (read "1.0e+INF")))
    (dolist
        (segment
         `(((kind . "text")
            (payload . ((text . "hello") (extra . t))))
           ((kind . "at") (payload . ((qq . 42))))
           ((kind . "image")
            (payload . ((file . "image.png") (sub_type . ,infinity))))
           ((kind . "file") (payload . ((file . "x") (extra . t))))
           ((kind . "record")
            (payload . ((file . "x") (file_id . 42))))
           ((kind . "face")
            (payload . ((id . "178") (chainCount . "two"))))
           ((kind . "mface")
            (payload . ((emoji_package_id . "1") (emoji_id . "2")
                        (key . "key") (summary . "sticker"))))
           ((kind . "mail") (payload . ((extra . "not allowed"))))
           ((kind . "video")
            (payload . ((file . "")
                        (remote . ((state . "unresolved"))))))
           ((kind . "unsupported")
            (payload . ((native_keys . ["bad-number"])
                        (summary . "bad") (raw . ,infinity))))
           ((kind . "music") (payload . ((value . ,infinity))))
           ((kind . "music") (payload . "not an object"))))
      (should-error
       (qq-api-validate-native-forward-messages
        (list (qq-api-test--native-message "1" nil (list segment))))
       :type 'user-error))))

(ert-deftest qq-api-native-forward-individual-builds-ordered-references ()
  (let (action params result)
    (cl-letf (((symbol-function 'qq-state-session) (lambda (_) nil))
              ((symbol-function 'qq-api-call)
               (lambda (called-action called-params callback &optional _errback)
                 (setq action called-action params called-params)
                 (funcall callback '((data . ((kind . "individual"))))))))
      (qq-api-forward-messages-individually
       "group:20001" "private:10001"
       '("9007199254742007089" "9007199254742007090")
       (lambda (value) (setq result value))))
    (should (equal action "emacs_send_forward"))
    (should
     (equal (alist-get 'destination params)
            '((kind . "private") (user_id . "10001"))))
    (should
     (equal
      (alist-get 'request params)
      '((kind . "individual")
        (messages
         . (((message_id . "9007199254742007089")
             (chat . ((kind . "group") (group_id . "20001"))))
            ((message_id . "9007199254742007090")
             (chat . ((kind . "group") (group_id . "20001")))))))))
    (should (equal result '((kind . "individual"))))))

(ert-deftest qq-api-native-forward-merged-preserves-order-and-group-target ()
  (let (params result)
    (cl-letf (((symbol-function 'qq-state-session) (lambda (_) nil))
              ((symbol-function 'qq-api-call)
               (lambda (_action called-params callback &optional _errback)
                 (setq params called-params)
                 (funcall
                  callback
                  '((data . ((kind . "merged")
                             (message_id . "9007199254742007999")
                             (resource_id . "resource-b"))))))))
      (qq-api-forward-messages-merged
       "private:10001" "group:20001"
       '("9007199254742007001"
         "9007199254742007001"
         "9007199254742007002")
       (lambda (value) (setq result value))))
    (should
     (equal (alist-get 'destination params)
            '((kind . "group") (group_id . "20001"))))
    (let ((messages
           (alist-get 'messages (alist-get 'request params))))
      (should (equal (alist-get 'kind (alist-get 'request params)) "merged"))
      (should
       (equal (mapcar (lambda (message) (alist-get 'message_id message))
                      messages)
              '("9007199254742007001"
                "9007199254742007001"
                "9007199254742007002")))
      (should
       (cl-every
        (lambda (message)
          (equal (alist-get 'chat message)
                 '((kind . "private") (user_id . "10001"))))
        messages)))
    (should
     (equal result
            '((kind . "merged")
              (message_id . "9007199254742007999")
              (resource_id . "resource-b"))))))

(ert-deftest qq-api-native-forward-rejects-dataline-destination ()
  (let ((request
         '((kind . "individual")
           (messages
            . (((message_id . "9007199254742007089")
                (chat . ((kind . "private") (user_id . "10001"))))))))
        transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _arguments) (setq transport-called t))))
      (should-error
       (qq-api-send-forward
        '((kind . "dataline")
          (peer_uid . "dev:a")
          (variant . "mobile"))
        request #'ignore)
       :type 'user-error)
      (should-error
       (qq-api-forward-messages-individually
        "private:10001" "dataline:mobile:dev:a"
        '("9007199254742007089") #'ignore)
       :type 'user-error))
    (should-not transport-called)))

(ert-deftest qq-api-native-send-forward-request-enforces-dataline-matrix ()
  (let (transport-calls)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params _callback &optional _errback)
                 (push (list action params) transport-calls)
                 'forward-request)))
      (dolist
          (case
           '(("individual" "desktop" t)
             ("individual" "mobile" nil)
             ("merged" "desktop" nil)
             ("merged" "mobile" nil)))
        (let* ((kind (nth 0 case))
               (variant (nth 1 case))
               (allowed-p (nth 2 case))
               (request
                `((kind . ,kind)
                  (messages
                   . (((message_id . "9007199254742007089")
                       (chat . ((kind . "dataline")
                                (peer_uid . "dev:a")
                                (variant . ,variant)))))))))
          (if allowed-p
              (should
               (eq
                (qq-api-send-forward
                 '((kind . "group") (group_id . "20001"))
                 request #'ignore)
                'forward-request))
            (should-error
             (qq-api-send-forward
              '((kind . "group") (group_id . "20001"))
              request #'ignore)
             :type 'user-error)))))
    (should (= (length transport-calls) 1))
    (should
     (equal
      (alist-get
       'chat
       (car
        (alist-get
         'messages
         (alist-get 'request (cadar transport-calls)))))
      '((kind . "dataline")
        (peer_uid . "dev:a")
        (variant . "desktop"))))))

(ert-deftest qq-api-native-forward-builders-enforce-dataline-matrix ()
  (let (transport-calls)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action params _callback &optional _errback)
                 (push params transport-calls)
                 'forward-request)))
      (should
       (eq
        (qq-api-forward-messages-individually
         "dataline:desktop:dev:a" "group:20001"
         '("9007199254742007089") #'ignore)
        'forward-request))
      (dolist
          (case
           '((qq-api-forward-messages-individually
              "dataline:mobile:dev:a")
             (qq-api-forward-messages-merged
              "dataline:desktop:dev:a")
             (qq-api-forward-messages-merged
              "dataline:mobile:dev:a")))
        (should-error
         (funcall (car case) (cadr case) "group:20001"
                  '("9007199254742007089") #'ignore)
         :type 'user-error)))
    (should (= (length transport-calls) 1))
    (should
     (equal
      (alist-get
       'chat
       (car
        (alist-get
         'messages
         (alist-get 'request (car transport-calls)))))
      '((kind . "dataline")
        (peer_uid . "dev:a")
        (variant . "desktop"))))))

(ert-deftest qq-api-native-forward-individual-accepts-service-source ()
  (let (params)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action called-params callback &optional _errback)
                 (setq params called-params)
                 (funcall callback '((data . ((kind . "individual"))))))))
      (qq-api-forward-messages-individually
       "service:u:mail:x" "group:20001"
       '("9007199254742007089") #'ignore))
    (should
     (equal
      (alist-get 'chat
                 (car (alist-get 'messages (alist-get 'request params))))
      '((kind . "service") (peer_uid . "u:mail:x"))))))

(ert-deftest qq-api-native-forward-rejects-mixed-source-chats-for-each-kind ()
  (let (transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _arguments) (setq transport-called t))))
      (dolist (kind '("individual" "merged"))
        (should-error
         (qq-api-send-forward
          '((kind . "group") (group_id . "20001"))
          `((kind . ,kind)
            (messages
             . (((message_id . "9007199254742007001")
                 (chat . ((kind . "group") (group_id . "20001"))))
                ((message_id . "9007199254742007002")
                 (chat . ((kind . "private") (user_id . "10001")))))))
          #'ignore)
         :type 'user-error)))
    (should-not transport-called)))

(ert-deftest qq-api-native-forward-compares-semantic-source-for-each-kind ()
  (let (transport-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _arguments) (setq transport-called t))))
      (dolist (kind '("individual" "merged"))
        (setq transport-called nil)
        (qq-api-send-forward
         '((kind . "group") (group_id . "20001"))
         `((kind . ,kind)
           (messages
            . (((message_id . "9007199254742007001")
                (chat . ((kind . "group") (group_id . "20001"))))
               ((message_id . "9007199254742007002")
                (chat . ((group_id . "20001") (kind . "group")))))))
         #'ignore)
        (should transport-called)))
    (should transport-called)))

(ert-deftest qq-api-native-forward-response-kind-matches-request-kind ()
  (dolist
      (case
       '((((kind . "individual"))
          ((kind . "merged")
           (message_id . "9007199254742007999")
           (resource_id . "resource-b")))
         (((kind . "merged"))
          ((kind . "individual")))))
    (let (delivered failure)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (_action _params callback &optional _errback)
                   (funcall callback `((data . ,(cadr case)))))))
        (qq-api-send-forward
         '((kind . "group") (group_id . "20001"))
         `((kind . ,(alist-get 'kind (car case)))
           (messages
            . (((message_id . "9007199254742007001")
                (chat . ((kind . "group") (group_id . "20001")))))))
         (lambda (_) (setq delivered t))
         (lambda (_response reason) (setq failure reason))))
      (should-not delivered)
      (should (string-match-p "does not match request kind" failure)))))

(ert-deftest qq-api-native-send-rejects-bad-request-and-result-aliases ()
  ;; The private protocol is a hard cut: old single/bundle shapes never enter
  ;; the transport, even when all of their identities are otherwise valid.
  (should-error
   (qq-api-send-forward
    '((kind . "group") (group_id . "20001"))
    '((kind . "single")
      (message . ((message_id . "9007199254742007001")
                  (chat . ((kind . "group") (group_id . "20001"))))))
   #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-send-forward
    '((kind . "group") (group_id . "20001"))
    '((kind . "bundle")
      (messages
       . (((message_id . "9007199254742007001")
           (chat . ((kind . "group") (group_id . "20001")))))))
    #'ignore)
   :type 'user-error)
  (should-error
   (qq-api-send-forward
    '((kind . "service") (peer_uid . "service-mail"))
    '((kind . "individual")
      (messages
       . (((message_id . "9007199254742007001")
           (chat . ((kind . "group") (group_id . "20001")))))))
    #'ignore)
   :type 'user-error)
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . ((kind . "merged")
                             (message_id . "9007199254742007999")
                             (res_id . "legacy"))))))))
      (qq-api-send-forward
       '((kind . "group") (group_id . "20001"))
       '((kind . "individual")
         (messages
          . (((message_id . "9007199254742007001")
              (chat . ((kind . "group") (group_id . "20001")))))))
       (lambda (_) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (stringp failure)))
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . ((kind . "bundle")
                             (message_id . "9007199254742007999")
                             (resource_id . "legacy-resource"))))))))
      (qq-api-send-forward
       '((kind . "group") (group_id . "20001"))
       '((kind . "merged")
         (messages
          . (((message_id . "9007199254742007001")
              (chat . ((kind . "group") (group_id . "20001")))))))
       (lambda (_) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (stringp failure))))

(ert-deftest qq-api-search-group-members-preserves-native-string-identities ()
  (let (action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (actual-action actual-params callback &optional _errback)
                 (setq action actual-action
                       params actual-params)
                 (funcall callback
                          '((data . (((user_id . "10001")
                                     (uid . "uid-alice")
                                     (nickname . "Alice")
                                     (card . "Alice Card")
                                     (remark . nil)
                                     (qid . "alice")
                                     (title . "管理员")
                                     (role . "admin")
                                     (robot . :false))
                                    ((user_id . "10002")
                                     (uid . "u-alice")
                                     (nickname . "Alice")
                                     (card . nil)
                                     (remark . nil)
                                     (qid . nil)
                                     (title . nil)
                                     (role . "member")
                                     (robot . :false)))))))))
      (qq-api-search-group-members
       "20001" "Alice" (lambda (members) (setq delivered members)) nil 80))
    (should (equal "emacs_search_group_members" action))
    (should (equal '((group_id . "20001") (query . "Alice") (limit . 80))
                   params))
    (should (equal '("10001" "10002")
                   (mapcar (lambda (member) (alist-get 'user_id member))
                           delivered)))
    (should (cl-every (lambda (member) (not (alist-get 'robot member)))
                      delivered))))

(ert-deftest qq-api-search-group-members-rejects-lossy-or-open-results ()
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          '((data . (((user_id . 10001)
                                     (uid . "uid-alice")
                                     (nickname . "Alice")
                                     (card . nil)
                                     (remark . nil)
                                     (qid . nil)
                                     (title . nil)
                                     (role . "member")
                                     (robot . :false)))))))))
      (qq-api-search-group-members
       "20001" "" (lambda (_) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (string-match-p "original decimal string" failure)))
  (should-error
   (qq-api-search-group-members "20001" "" #'ignore nil 0)
   :type 'user-error))

(ert-deftest qq-api-search-group-members-rejects-duplicate-user-ids ()
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . (((user_id . "10001")
                              (uid . "uid-alice")
                              (nickname . "Alice")
                              (card . nil)
                              (remark . nil)
                              (qid . nil)
                              (title . nil)
                              (role . "member")
                              (robot . :false))
                             ((user_id . "10001")
                              (uid . "uid-alice-duplicate")
                              (nickname . "Duplicate")
                              (card . nil)
                              (remark . nil)
                              (qid . nil)
                              (title . nil)
                              (role . "member")
                              (robot . :false)))))))))
      (qq-api-search-group-members
       "20001" "" (lambda (_) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (string-match-p "duplicate user_id 10001" failure))))

(ert-deftest qq-api-search-strangers-start-trims-owner-and-validates-copy ()
  (let* ((candidate (qq-api-test--capability-token ?B))
         (cursor (qq-api-test--capability-token ?C))
         (native-page
          `((results
             . (((user_id . "9007199254740993")
                 (uid . "u_synthetic_candidate")
                 (nickname . "Synthetic User")
                 (avatar_url . "https://example.invalid/avatar")
                 (candidate . ,candidate))))
            (next_cursor . ,cursor)))
         action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (wire-action wire-params callback &optional _errback)
                 (setq action wire-action params wire-params)
                 (funcall callback `((data . ,native-page)))
                 'stranger-search-request)))
      (should
       (eq (qq-api-search-strangers-start
            "  Synthetic User  " (lambda (page) (setq delivered page))
            nil 17)
           'stranger-search-request)))
    (should (equal action "emacs_search_strangers"))
    (should (equal params
                   '((kind . "start")
                     (query . "Synthetic User")
                     (limit . 17))))
    (should (equal delivered native-page))
    (setf (alist-get 'nickname (car (alist-get 'results native-page)))
          "Mutated")
    (should (equal (alist-get 'nickname
                              (car (alist-get 'results delivered)))
                   "Synthetic User"))))

(ert-deftest qq-api-search-strangers-next-repeats-exact-owner ()
  (let ((cursor (qq-api-test--capability-token ?D)) action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (wire-action wire-params callback &optional _errback)
                 (setq action wire-action params wire-params)
                 (funcall callback '((data . ((results) (next_cursor)))))
                 'stranger-next-request)))
      (should
       (eq (qq-api-search-strangers-next
            cursor "  Exact Owner " (lambda (page) (setq delivered page))
            nil 50)
           'stranger-next-request)))
    (should (equal action "emacs_search_strangers"))
    (should (equal params
                   `((kind . "next")
                     (cursor . ,cursor)
                     (query . "Exact Owner")
                     (limit . 50))))
    (should (equal delivered '((results) (next_cursor))))))

(ert-deftest qq-api-search-strangers-rejects-invalid-input-before-transport ()
  (let (called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) (setq called t))))
      (dolist (form
               `((qq-api-search-strangers-start "   " #'ignore)
                 (qq-api-search-strangers-start "x" #'ignore nil 0)
                 (qq-api-search-strangers-start "x" #'ignore nil 51)
                 (qq-api-search-strangers-next
                  "short" "x" #'ignore nil 10)))
        (should-error (eval form) :type 'user-error)))
    (should-not called)))

(ert-deftest qq-api-stranger-and-friend-add-limits-use-utf-16-code-units ()
  (let ((astral #x1f600))
    (should (= (qq-api--utf-16-code-unit-length
                (concat "a" (string astral)))
               3))
    (should
     (equal (alist-get 'query
                       (qq-api--stranger-search-owner-params
                        (make-string 64 astral) 20))
            (make-string 64 astral)))
    (should-error
     (qq-api--stranger-search-owner-params (make-string 65 astral) 20)
     :type 'user-error)
    (let ((boundary
           `((verification_message . ,(make-string 150 astral))
             (answers . (,(make-string 150 astral)))
             (remark . ,(make-string 50 astral))
             (friend_group_id . 0)
             (only_chat . nil)
             (qzone_not_watch . nil)
             (qzone_not_watched . nil))))
      (should (equal (qq-api--normalize-friend-add-options boundary)
                     boundary))
      (dolist (field '(verification_message answers remark))
        (let ((too-long (copy-tree boundary)))
          (setf (alist-get field too-long)
                (if (eq field 'answers)
                    (list (make-string 151 astral))
                  (make-string (if (eq field 'remark) 51 151) astral)))
          (should-error (qq-api--normalize-friend-add-options too-long)
                        :type 'user-error))))))

(ert-deftest qq-api-search-strangers-page-is-closed-and-lossless ()
  (let ((candidate (qq-api-test--capability-token ?E)))
    (should-error
     (qq-api--validate-stranger-search-page
      `((results
         . (((user_id . 9007199254740993)
             (uid)
             (nickname . "Synthetic")
             (avatar_url . "https://example.invalid/avatar")
             (candidate . ,candidate))))
        (next_cursor))))
    (should-error
     (qq-api--validate-stranger-search-page
      `((results
         . (((user_id . "9007199254740993")
             (uid)
             (nickname . "Synthetic")
             (avatar_url . "https://example.invalid/avatar")
             (candidate . ,candidate)
             (native . "leak"))))
        (next_cursor))))
    (should-error
     (qq-api--validate-stranger-search-page
      `((results
         . (((user_id . "9007199254740993")
             (uid)
             (nickname . "Synthetic")
             (avatar_url . "")
             (candidate . ,candidate))))
        (next_cursor))))
    (should-error
     (qq-api--validate-stranger-search-page
      '((results) (next_cursor . "not-a-capability"))))))

(ert-deftest qq-api-search-strangers-page-requires-unique-nonnull-uids ()
  (let ((candidate (qq-api-test--capability-token ?U)))
    (should-error
     (qq-api--validate-stranger-search-page
      `((results
         . (((user_id . "9007199254740993")
             (uid . "native-shared") (nickname . "First")
             (avatar_url . "https://example.invalid/first")
             (candidate . ,candidate))
            ((user_id . "9007199254740994")
             (uid . "native-shared") (nickname . "Second")
             (avatar_url . "https://example.invalid/second")
             (candidate . ,candidate))))
        (next_cursor))))
    (let ((validated
           (qq-api--validate-stranger-search-page
            `((results
               . (((user_id . "9007199254740993")
                   (uid) (nickname . "First")
                   (avatar_url . "https://example.invalid/first")
                   (candidate . ,candidate))
                  ((user_id . "9007199254740994")
                   (uid) (nickname . "Second")
                   (avatar_url . "https://example.invalid/second")
                   (candidate . ,candidate))))
              (next_cursor)))))
      (should (= (length (alist-get 'results validated)) 2)))))

(ert-deftest qq-api-search-strangers-does-not-reinterpret-consumer-errors ()
  (let (errback-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback '((data . ((results) (next_cursor))))))))
      (should-error
       (qq-api-search-strangers-start
        "Synthetic"
        (lambda (_page) (error "consumer failure"))
        (lambda (&rest _args) (setq errback-called t)))
       :type 'error))
    (should-not errback-called)))

(ert-deftest qq-api-friend-add-prepare-validates-multi-question-result ()
  (let* ((candidate (qq-api-test--capability-token ?F))
         (preparation (qq-api-test--capability-token ?G))
         (native-result
          `((kind . "prepared")
            (user_id . "9007199254740993")
            (verification . "question_and_audit")
            (questions . ("Synthetic question one?"
                          "Synthetic question two?"))
            (preparation . ,preparation)))
         action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (wire-action wire-params callback &optional _errback)
                 (setq action wire-action params wire-params)
                 (funcall callback `((data . ,native-result)))
                 'friend-prepare-request)))
      (should
       (eq (qq-api-friend-add-prepare
            "9007199254740993" candidate
            (lambda (result) (setq delivered result)))
           'friend-prepare-request)))
    (should (equal action "emacs_friend_add"))
    (should (equal params
                   `((kind . "prepare")
                     (user_id . "9007199254740993")
                     (candidate . ,candidate))))
    (should (equal delivered native-result))
    (setf (car (alist-get 'questions native-result)) "Mutated")
    (should (equal (alist-get 'questions delivered)
                   '("Synthetic question one?"
                     "Synthetic question two?")))))

(ert-deftest qq-api-friend-add-prepared-verification-modes-are-exact ()
  (let ((preparation (qq-api-test--capability-token ?M))
        (user-id "9007199254740993"))
    (dolist (result
             `(((kind . "prepared") (user_id . ,user-id)
                (verification . "message")
                (questions . ("Unexpected question"))
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "question_answer") (questions . (""))
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "question_answer") (questions)
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "question_and_audit") (questions)
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "question_and_audit")
                (questions . ("Question one" "   "))
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "single_question")
                (questions . ("Legacy question"))
                (preparation . ,preparation))
               ((kind . "prepared") (user_id . ,user-id)
                (verification . "question")
                (questions . ("Legacy question"))
                (preparation . ,preparation))))
      (should-error (qq-api--validate-friend-add-result result user-id)))
    (should
     (equal
      (alist-get
       'verification
       (qq-api--validate-friend-add-result
        `((kind . "prepared") (user_id . ,user-id)
          (verification . "question_answer")
          (questions . ("Synthetic question one" "Synthetic question two"))
          (preparation . ,preparation))
        user-id))
      "question_answer"))))

(ert-deftest qq-api-friend-add-submit-encodes-json-false-and-validates-result ()
  (let ((preparation (qq-api-test--capability-token ?H))
        action params delivered)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (wire-action wire-params callback &optional _errback)
                 (setq action wire-action params wire-params)
                 (funcall
                  callback
                  '((data . ((kind . "submitted")
                             (user_id . "9007199254740993")))))
                 'friend-submit-request)))
      (should
       (eq (qq-api-friend-add-submit
            "9007199254740993" preparation
            '((verification_message . "Synthetic hello")
              (answers)
              (remark . "")
              (friend_group_id . 0)
              (only_chat . nil)
              (qzone_not_watch . t)
              (qzone_not_watched . nil))
            (lambda (result) (setq delivered result)))
           'friend-submit-request)))
    (should (equal action "emacs_friend_add"))
    (should (equal params
                   `((kind . "submit")
                     (user_id . "9007199254740993")
                     (preparation . ,preparation)
                     (verification_message . "Synthetic hello")
                     (answers . [])
                     (remark . "")
                     (friend_group_id . 0)
                     (only_chat . :false)
                     (qzone_not_watch . t)
                     (qzone_not_watched . :false))))
    (should (equal delivered
                   '((kind . "submitted")
                     (user_id . "9007199254740993"))))))

(ert-deftest qq-api-friend-add-rejects-invalid-input-before-transport ()
  (let ((token (qq-api-test--capability-token ?I)) called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (&rest _args) (setq called t))))
      (should-error
       (qq-api-friend-add-prepare "0" token #'ignore) :type 'user-error)
      (should-error
       (qq-api-friend-add-prepare "9007199254740993" "short" #'ignore)
       :type 'user-error)
      (dolist
          (options
           (list
            `((verification_message . ,(make-string 301 ?x)) (answers)
              (remark . "") (friend_group_id . 0)
              (only_chat . nil) (qzone_not_watch . nil)
              (qzone_not_watched . nil))
            '((message . "Legacy mixed verification")
              (remark . "") (friend_group_id . 0)
              (only_chat . nil) (qzone_not_watch . nil)
              (qzone_not_watched . nil))
            `((verification_message . "")
              (answers . (,(make-string 301 ?x)))
              (remark . "") (friend_group_id . 0)
              (only_chat . nil) (qzone_not_watch . nil)
              (qzone_not_watched . nil))
            '((verification_message . "") (answers)
              (remark . "") (friend_group_id . 4294967296)
              (only_chat . nil) (qzone_not_watch . nil)
              (qzone_not_watched . nil))
            '((verification_message . "") (answers)
              (remark . "") (friend_group_id . 0)
              (only_chat . :false) (qzone_not_watch . nil)
              (qzone_not_watched . nil))
            '((verification_message . "") (answers)
              (remark . "") (friend_group_id . 0)
              (only_chat . nil) (qzone_not_watch . nil))))
        (should-error
         (qq-api-friend-add-submit
          "9007199254740993" token options #'ignore)
         :type 'user-error)))
    (should-not called)))

(ert-deftest qq-api-friend-add-routes-closed-phase-errors-to-errback ()
  (let ((candidate (qq-api-test--capability-token ?J)) delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . ((kind . "submitted")
                             (user_id . "9007199254740993")
                             (extra . :false))))))))
      (qq-api-friend-add-prepare
       "9007199254740993" candidate
       (lambda (_result) (setq delivered t))
       (lambda (_response reason) (setq failure reason))))
    (should-not delivered)
    (should (string-match-p "invalid submitted result" failure))))

(ert-deftest qq-api-friend-add-does-not-reinterpret-consumer-errors ()
  (let ((candidate (qq-api-test--capability-token ?K)) errback-called)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  `((data . ((kind . "prepared")
                             (user_id . "9007199254740993")
                             (verification . "none")
                             (questions)
                             (preparation
                              . ,(qq-api-test--capability-token ?L)))))))))
      (should-error
       (qq-api-friend-add-prepare
        "9007199254740993" candidate
        (lambda (_result) (error "consumer failure"))
        (lambda (&rest _args) (setq errback-called t)))
       :type 'error))
    (should-not errback-called)))

(provide 'qq-api-test)

;;; qq-api-test.el ends here
