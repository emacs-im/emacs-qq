;;; qq-api-test.el --- Tests for qq-api -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-api)

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

(ert-deftest qq-api-send-text-builds-reply-segments ()
  (let (sent-session sent-segments sent-raw)
    (cl-letf (((symbol-function 'qq-api-send-message)
               (lambda (session-key segments &optional raw-message)
                 (setq sent-session session-key)
                 (setq sent-segments segments)
                 (setq sent-raw raw-message)
                 'sent)))
      (qq-api-send-text "private:10001" "hello" 42)
      (should (equal sent-session "private:10001"))
      (should (equal sent-raw "hello"))
      (should (equal sent-segments
                     '(((type . "reply")
                        (data . ((id . "42"))))
                       ((type . "text")
                        (data . ((text . "hello"))))))))))

(ert-deftest qq-api-fetch-history-uses-peer-history-for-dataline-sessions ()
  (let (captured-action captured-params merged)
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((key . "dataline:device-1")
                   (type . dataline)
                   (target-id . "device-1")
                   (chat-type . "8")
                   (peer-uid . "device-1"))))
              ((symbol-function 'qq-state-merge-history)
               (lambda (session-key messages)
                 (setq merged (list session-key messages))))
              ((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action)
                 (setq captured-params params)
                 (funcall callback '((data . ((messages . (((message_id . 1))))))) )
                 'sent)))
      (qq-api-fetch-history "dataline:device-1")
      (should (equal captured-action "get_peer_msg_history"))
      (should (equal (alist-get 'chat_type captured-params) "8"))
      (should (equal (alist-get 'peer_uid captured-params) "device-1"))
      (should (equal merged '("dataline:device-1" (((message_id . 1)))))))))

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
      (qq-api-delete-message 77)
      (should (equal captured-action "delete_msg"))
      (should (equal (alist-get 'message_id captured-params) "77"))
      (should (equal recalled-id 77)))))

(provide 'qq-api-test)

;;; qq-api-test.el ends here
