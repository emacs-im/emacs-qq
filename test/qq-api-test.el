;;; qq-api-test.el --- Tests for qq-api -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-api)

(defun qq-api-test--native-message
    (entry-id &optional message-id segments)
  "Return one strict fork-native forward snapshot."
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
            '(((kind . "text") (payload . ((text . "hello")))))))))

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

(defun qq-api-test--message-reference (kind target-id message-id)
  "Return one closed mutation reference for KIND, TARGET-ID and MESSAGE-ID."
  `((message_id . ,message-id)
    (chat . ,(if (eq kind 'group)
                 `((kind . "group") (group_id . ,target-id))
               `((kind . "private") (user_id . ,target-id))))))

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
                 'sent)))
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
  (let (success-fn events)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest _args) '((local-id . "local-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (setq success-fn callback)))
              ((symbol-function 'qq-state-mark-pending-message-sent)
               (lambda (&rest _args) (push 'promoted events))))
      (qq-api-send-message
       "private:10001"
       '(((type . "text") (data . ((text . "hello")))))
       "hello"
       (lambda (_response) (push 'callback events)))
      (funcall success-fn
               '((data . ((message_id . "9007199254742007094")))))
      (should (equal '(promoted callback) (nreverse events))))))

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
                 'sent)))
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

(ert-deftest qq-api-mark-message-read-sends-exact-id-without-guessing-state ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
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
      (funcall success-fn '((status . ok)))
      (should (= 3 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (should-not (gethash "private:10001" qq-api--read-operations)))))

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
      (funcall (caar calls) '((status . "ok")))
      (should (= (length calls) 2))
      (should (equal (mapcar (lambda (it) (alist-get 'message_id it)) params)
                     '("9007199254741004645" "9007199254741004646")))
      (should (= 2 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (funcall (car (nth 1 calls)) '((status . "ok")))
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
               (lambda (session-key messages)
                 (setq merged (list session-key messages))
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
      (should (equal merged
                     '("dataline:mobile:dev:a" (((message_id . 1))))))
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
                            (lastMessagePreview . "newer"))))))
      (funcall (nth 0 callbacks)
               '((data . (((chatType . 2) (peerUid . "20001")
                            (peerUin . "20001") (unreadCount . 9)
                            (msgTime . "1710000001")
                            (msgId . "9007199254741004645")
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

(ert-deftest qq-api-bootstrap-normalizes-contacts-after-identity-and-names ()
  (let (order)
    (cl-letf (((symbol-function 'qq-api-refresh-status)
               (lambda () (push 'status order)))
              ((symbol-function 'qq-api-refresh-login-info)
               (lambda (&optional callback _errback)
                 (push 'login order)
                 (funcall callback 'info)))
              ((symbol-function 'qq-api-refresh-friend-list)
               (lambda (&optional callback _errback)
                 (push 'friends order)
                 (funcall callback 'friends)))
              ((symbol-function 'qq-api-refresh-group-list)
               (lambda (&optional callback _errback)
                 (push 'groups order)
                 (funcall callback 'groups)))
              ((symbol-function 'qq-api-refresh-recent-contacts)
               (lambda (&optional _callback _errback)
                 (push 'contacts order))))
      (qq-api-bootstrap)
      (should (equal (nreverse order)
                     '(status login friends groups contacts))))))

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

(ert-deftest qq-api-native-forward-message-builds-locator-qualified-single ()
  (let (action params result)
    (cl-letf (((symbol-function 'qq-state-session) (lambda (_) nil))
              ((symbol-function 'qq-api-call)
               (lambda (called-action called-params callback &optional _errback)
                 (setq action called-action params called-params)
                 (funcall callback '((data . ((kind . "single"))))))))
      (qq-api-forward-message
       "9007199254742007089" "group:20001" "private:10001"
       (lambda (value) (setq result value))))
    (should (equal action "emacs_send_forward"))
    (should
     (equal (alist-get 'destination params)
            '((kind . "private") (user_id . "10001"))))
    (should
     (equal
      (alist-get 'request params)
      '((kind . "single")
        (message
         . ((message_id . "9007199254742007089")
            (chat . ((kind . "group") (group_id . "20001"))))))))
    (should (equal result '((kind . "single"))))))

(ert-deftest qq-api-native-forward-bundle-preserves-order-and-duplicates ()
  (let (params result)
    (cl-letf (((symbol-function 'qq-state-session) (lambda (_) nil))
              ((symbol-function 'qq-api-call)
               (lambda (_action called-params callback &optional _errback)
                 (setq params called-params)
                 (funcall
                  callback
                  '((data . ((kind . "bundle")
                             (message_id . "9007199254742007999")
                             (resource_id . "resource-b"))))))))
      (qq-api-send-forward-bundle
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
            '((kind . "bundle")
              (message_id . "9007199254742007999")
              (resource_id . "resource-b"))))))

(ert-deftest qq-api-native-send-rejects-bad-request-and-result-aliases ()
  (should-error
   (qq-api-send-forward
    '((kind . "group") (group_id . "20001"))
    '((kind . "single")
      (message . ((message_id . 1)
                  (chat . ((kind . "group") (group_id . "20001"))))))
    #'ignore)
   :type 'user-error)
  (let (delivered failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall
                  callback
                  '((data . ((kind . "bundle")
                             (message_id . "9007199254742007999")
                             (res_id . "legacy"))))))))
      (qq-api-send-forward
       '((kind . "group") (group_id . "20001"))
       '((kind . "single")
         (message
          . ((message_id . "9007199254742007001")
             (chat . ((kind . "group") (group_id . "20001"))))))
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

(provide 'qq-api-test)

;;; qq-api-test.el ends here
