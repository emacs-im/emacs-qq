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

(ert-deftest qq-api-send-message-builds-dataline-send-msg-request ()
  (let (captured-action captured-params pending-call)
    (cl-letf (((symbol-function 'qq-state-insert-pending-message)
               (lambda (&rest args)
                 (setq pending-call args)
                 '((local-id . "local-1"))))
              ((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "dataline:device-1")
                   (type . dataline)
                   (target-id . "device-1")
                   (chat-type . "8")
                   (peer-uid . "device-1"))))
              ((symbol-function 'qq-api-call)
               (lambda (action params _callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 'sent)))
      (qq-api-send-message
       "dataline:device-1"
       '(((type . "text")
          (data . ((text . "hello phone")))))
       "hello phone")
      (should (equal pending-call
                     '("dataline:device-1"
                       (((type . "text")
                         (data . ((text . "hello phone")))))
                       "hello phone")))
      (should (equal captured-action "send_msg"))
      (should (equal (alist-get 'chat_type captured-params) "8"))
      (should (equal (alist-get 'peer_uid captured-params) "device-1"))
      (should-not (alist-get 'user_id captured-params)))))

(ert-deftest qq-api-mark-session-read-clears-unread-optimistically ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        cleared-before-call
        errback-fn
        success-fn)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((title . "Alice")
       (target-id . "10001")
       (unread-count . 3))
     nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional errback)
                 (setq cleared-before-call
                       (= 0 (alist-get 'unread-count
                                       (qq-state-session "private:10001"))))
                 (setq success-fn callback)
                 (setq errback-fn errback)
                 'sent)))
      (qq-api-mark-session-read "private:10001")
      (should cleared-before-call)
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      ;; Failure restores previous unread.
      (funcall errback-fn nil "network down")
      (should (= 3 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      ;; Success path after another optimistic clear stays at 0.
      (qq-api-mark-session-read "private:10001")
      (funcall success-fn '((status . ok)))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "private:10001")))))))

(ert-deftest qq-api-mark-session-read-coalesces-and-restores-new-arrivals ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        calls)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((type . private) (target-id . "10001") (unread-count . 5)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional errback)
                 (push (cons callback errback) calls)
                 'sent))
              ((symbol-function 'qq-api--default-error) #'ignore))
      (qq-api-mark-session-read "private:10001")
      ;; A live message arrives after the request was sent and is immediately
      ;; cleared by the visible chat's second mark-read call.
      (qq-state-set-session-unread "private:10001" 1)
      (qq-api-mark-session-read "private:10001")
      (should (= (length calls) 1))
      (funcall (cdar calls) nil "network down")
      (should (= (alist-get 'unread-count
                            (qq-state-session "private:10001"))
                 6)))))

(ert-deftest qq-api-mark-session-read-follows-up-for-coalesced-arrivals ()
  (let ((qq-state-change-hook nil)
        (qq-api--read-operations (make-hash-table :test #'equal))
        (qq-api--read-operation-counter 0)
        calls)
    (qq-state-reset)
    (qq-state-upsert-session
     "private:10001"
     '((type . private) (target-id . "10001") (unread-count . 2)) nil)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional errback)
                 (setq calls (append calls (list (cons callback errback))))
                 'sent)))
      (qq-api-mark-session-read "private:10001")
      (qq-state-set-session-unread "private:10001" 1)
      (qq-api-mark-session-read "private:10001")
      (should (= (length calls) 1))
      (funcall (caar calls) '((status . "ok")))
      (should (= (length calls) 2))
      (should (= 0 (alist-get 'unread-count
                              (qq-state-session "private:10001"))))
      (funcall (car (nth 1 calls)) '((status . "ok")))
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

(ert-deftest qq-api-fetch-history-uses-peer-history-for-dataline-sessions ()
  (let (captured-action captured-params merged done-meta)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "dataline:device-1")
                   (type . dataline)
                   (target-id . "device-1")
                   (chat-type . "8")
                   (peer-uid . "device-1"))))
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
      (qq-api-fetch-history "dataline:device-1" nil
                            (lambda (meta) (setq done-meta meta)))
      (should (equal captured-action "get_peer_msg_history"))
      (should (equal (alist-get 'chat_type captured-params) "8"))
      (should (equal (alist-get 'peer_uid captured-params) "device-1"))
      (should (equal merged '("dataline:device-1" (((message_id . 1))))))
      (should (= (plist-get done-meta :added-count) 1)))))

(ert-deftest qq-api-fetch-session-read-state-uses-raw-peer-and-applies-result ()
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
                                     (first_unread_message_id . "9007199254742007089")
                                     (position_available . t)))))
                 'sent)))
      (qq-api-fetch-session-read-state
       "group:20001"
       (lambda (read-state) (setq callback-value read-state)))
      (should (equal "get_peer_read_state" captured-action))
      (should (equal "2" (alist-get 'chat_type captured-params)))
      (should (equal "20001" (alist-get 'peer_uid captured-params)))
      (should (equal (car applied) "group:20001"))
      (should (equal callback-value (cadr applied))))))

(ert-deftest qq-api-fetch-history-uses-peer-history-for-service-sessions ()
  (let (captured-action captured-params)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "service:u_mail")
                   (type . service)
                   (target-id . "u_mail")
                   (chat-type . "103")
                   (peer-uid . "u_mail"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (&rest _) (list :added-count 0 :message-count 0)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback '((data . ((messages . nil)))))
                 'sent)))
      (qq-api-fetch-history "service:u_mail")
      (should (equal captured-action "get_peer_msg_history"))
      (should (equal (alist-get 'chat_type captured-params) "103"))
      (should (equal (alist-get 'peer_uid captured-params) "u_mail"))
      (should-not (alist-get 'user_id captured-params)))))

(ert-deftest qq-api-fetch-history-passes-message-seq-for-older-page ()
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
      (qq-api-fetch-history "private:10001" "9007199254741004999")
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
  (let (captured-action captured-params recalled-id)
    (cl-letf (((symbol-function 'qq-state-apply-recall)
               (lambda (message-id)
                 (setq recalled-id message-id)))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 (funcall callback '((status . "ok")))
                 'sent)))
      (qq-api-delete-message "77")
      (should (equal captured-action "delete_msg"))
      (should (equal (alist-get 'message_id captured-params) "77"))
      (should (equal recalled-id "77"))
      (should-error (qq-api-delete-message 77) :type 'user-error))))

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
            (payload . ((id . "178") (resultId . "ok")
                        (chainCount . 2))))
           ((kind . "mface")
            (payload . ((emoji_package_id . 1) (emoji_id . "2")
                        (key . "key") (summary . "sticker"))))
           ((kind . "mail") (payload . nil))
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

(provide 'qq-api-test)

;;; qq-api-test.el ends here
