;;; qq-gateway-message-test.el --- Tests for native Gateway messages -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-message)

(defconst qq-gateway-message-test-capabilities
  '("message.send" "message.send_text" "message.poke"
    "message.recall_poke" "message.recall" "message.get_history")
  "Native Gateway capabilities exercised by message tests.")

(defun qq-gateway-message-test-account
    (&optional account-id generation uin uid)
  "Return an online account for ACCOUNT-ID, GENERATION, UIN, and UID."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . "online")
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (generation . ,(or generation "7"))
    (challenge)
    (problem)))

(defun qq-gateway-message-test-text-segment (text)
  "Return a native text segment containing TEXT."
  `((kind . "text") (payload . ((text . ,text)))))

(cl-defun qq-gateway-message-test-event
    (&key
     (account-id "slot-a") (generation "7")
     (message-id "7348923749823749823")
     (sent-at 1784700000)
     (sender '((uin . "10001") (uid . "u_peer")))
     (recipient '((uin . "10002") (uid . "u_self")))
     (conversation '((kind . "private") (name . "Peer")))
     (sequence "9007199254740999")
     (client-sequence "9007199254741001")
     (random 7) (message-type 166) (sub-type 0)
     (segments '(((kind . "text") (payload . ((text . "hello")))))))
  "Return one closed native Gateway message event payload."
  `((account_id . ,account-id)
    (generation . ,generation)
    (message
     . ((message_id . ,message-id)
        (sent_at . ,sent-at)
        (sender . ,(copy-tree sender))
        (recipient . ,(copy-tree recipient))
        (conversation . ,(copy-tree conversation))
        (sequence . ,sequence)
        (client_sequence . ,client-sequence)
        (random . ,random)
        (message_type . ,message-type)
        (sub_type . ,sub-type)
        (segments . ,(copy-tree segments))))))

(cl-defun qq-gateway-message-test-recall
    (&key
     (account-id "slot-a") (generation "7")
     (conversation '((kind . "group") (group_uin . "8209413637")))
     (target '((kind . "sequence") (sequence . "9007199254740999")))
     (author-uid "u_peer") (operator-uid "u_admin")
     (tip "message recalled"))
  "Return one closed native Gateway recall event payload."
  `((account_id . ,account-id)
    (generation . ,generation)
    (recall
     . ((conversation . ,(copy-tree conversation))
        (target . ,(copy-tree target))
        (author_uid . ,author-uid)
        (operator_uid . ,operator-uid)
        (tip . ,tip)))))

(cl-defun qq-gateway-message-test-poke
    (&key
     (account-id "slot-a") (generation "7")
     (message-id "7348923749823749823") (sent-at 1784700000)
     (sequence "9007199254740999") (group-uin "8209413637")
     (actor-uin "10002") (target-uin "9007199254741001")
     (tips-sequence "9007199254741007") (valid-before 1784700120))
  "Return one authoritative native Gateway group poke event payload."
  `((account_id . ,account-id)
    (generation . ,generation)
    (poke
     . ((message_id . ,message-id)
        (sent_at . ,sent-at)
        (sequence . ,sequence)
        (conversation . ((kind . "group") (group_uin . ,group-uin)))
        (actor_uin . ,actor-uin)
        (target_uin . ,target-uin)
        (action . "戳了戳")
        (action_image_url . "https://example.invalid/poke.png")
        (suffix . "的肩膀")
        (recall . ((tips_sequence . ,tips-sequence)
                   (valid_before . ,valid-before)))))))

(cl-defun qq-gateway-message-test-history-result
    (messages start-sequence end-sequence
              &key (response-start start-sequence)
              (response-end end-sequence) (unsupported-count 0))
  "Return a closed history result containing MESSAGES.

START-SEQUENCE and END-SEQUENCE are echoed as the requested range."
  `((account_id . "slot-a")
    (generation . "7")
    (requested_start_sequence . ,start-sequence)
    (requested_end_sequence . ,end-sequence)
    (response_start_sequence . ,response-start)
    (response_end_sequence . ,response-end)
    (unsupported_message_count . ,unsupported-count)
    (messages . ,(copy-tree messages))))

(defmacro qq-gateway-message-test-with-state (&rest body)
  "Run BODY with one selected account and isolated message projection state."
  (declare (indent 0) (debug t))
  `(let ((qq-backend 'gateway)
         (qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--gateway-instance-id nil)
         (qq-gateway--resync-request-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-gateway-desync-hook nil)
         (qq-gateway-message--projection-owner nil)
         (qq-gateway-message--peer-uin-by-uid
          (make-hash-table :test #'equal))
         (qq-gateway-message--pending-recalls
          (make-hash-table :test #'equal))
         (qq-gateway-message--pending-sends
          (make-hash-table :test #'equal))
         (qq-gateway-message--live-frontiers
          (make-hash-table :test #'equal))
         (qq-gateway-message-event-hook nil)
         (qq-gateway-message-projection-error-hook nil)
         (qq-gateway-transport--state 'ready)
         (qq-state-change-hook nil))
     (unwind-protect
         (progn
           (qq-state-reset)
           (qq-gateway--replace-accounts
            (list (qq-gateway-message-test-account)) 'ready "gateway-test")
           ,@body)
       (qq-state-reset))))

(ert-deftest qq-gateway-message-validator-keeps-exact-string-identities ()
  (let* ((raw (qq-gateway-message-test-event))
         (validated (qq-gateway-message--validate-message-data raw))
         (message (alist-get 'message validated)))
    (should (equal (alist-get 'message_id message)
                   "7348923749823749823"))
    (should (equal (alist-get 'sequence message)
                   "9007199254740999"))
    (should (stringp (alist-get 'client_sequence message)))
    (let ((numeric (copy-tree raw)))
      (setf (alist-get 'message_id (alist-get 'message numeric))
            7348923749823749823)
      (should-error
       (qq-gateway-message--validate-message-data numeric)))))

(ert-deftest qq-gateway-message-projects-group-segments-and-metadata ()
  (qq-gateway-message-test-with-state
    (let ((segments
           (list
            (qq-gateway-message-test-text-segment "hello")
            '((kind . "at")
              (payload . ((qq . "10002") (name . "Primary"))))
            '((kind . "reply")
              (payload
               . ((target
                   . ((kind . "unresolved")
                      (message_id . "7348923749823749111"))))))
            '((kind . "face") (payload . ((id . "178"))))
            '((kind . "unsupported")
              (payload
               . ((native_keys . ("text.pb_reserve"))
                  (summary . "text.pb_reserve")
                  (raw . ((fallback_text . "@Alice")))))))))
      (qq-gateway-message--handle-event
       "message.received"
       (qq-gateway-message-test-event
        :conversation
        '((kind . "group")
          (group_uin . "8209413637")
          (group_name . "Protocol Lab")
          (sender_card . "Alice"))
        :segments segments))
      (let* ((session-key "group:8209413637")
             (messages (qq-state-session-messages session-key))
             (message (car messages))
             (internal (alist-get 'segments message)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749823"))
        (should (equal (alist-get 'message-seq message)
                       "9007199254740999"))
        (should (equal (alist-get 'native-client-sequence message)
                       "9007199254741001"))
        (should (= (alist-get 'native-random message) 7))
        (should (equal (alist-get 'gateway-account-id message) "slot-a"))
        (should (equal (alist-get 'gateway-generation message) "7"))
        (should (equal (alist-get 'sender-name message) "Alice"))
        (should (equal (alist-get 'mention-kinds message) '(at-me)))
        (should (eq (alist-get 'status message) 'received))
        (should (equal (mapcar (lambda (it) (alist-get 'type it)) internal)
                       '("text" "at" "reply" "face" "__unsupported")))
        (should
         (equal
          (alist-get 'message_id (alist-get 'data (nth 2 internal)))
          "7348923749823749111"))
        (should (equal (alist-get 'id (alist-get 'data (nth 3 internal)))
                       "178"))
        (should (equal (alist-get 'title (qq-state-session session-key))
                       "Protocol Lab"))))))

(ert-deftest qq-gateway-message-projects-private-peer-by-self-endpoint ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received" (qq-gateway-message-test-event))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           (session (qq-state-session session-key)))
      (should-not (alist-get 'self-p message))
      (should (equal (alist-get 'peer-uin message) "10001"))
      (should (equal (alist-get 'peer-uid message) "u_peer"))
      (should (equal (alist-get 'peer-uid session) "u_peer"))
      (should (equal (gethash "u_peer"
                              qq-gateway-message--peer-uin-by-uid)
                     "10001")))))

(ert-deftest qq-gateway-message-observes-other-account-without-projecting ()
  (qq-gateway-message-test-with-state
    (let (observed)
      (add-hook 'qq-gateway-message-event-hook
                (lambda (event data) (setq observed (list event data))))
      (qq-gateway-message--handle-event
       "message.received"
       (qq-gateway-message-test-event
        :account-id "slot-b" :generation "11"))
      (should (equal (car observed) "message.received"))
      (should (equal (alist-get 'account_id (cadr observed)) "slot-b"))
      (should-not qq-gateway-message--projection-owner)
      (should-not (qq-state-sessions)))))

(ert-deftest qq-gateway-message-stale-generation-cannot-project ()
  (qq-gateway-message-test-with-state
    (let ((events 0))
      (add-hook 'qq-gateway-message-event-hook
                (lambda (&rest _) (cl-incf events)))
      (qq-gateway-message--handle-event
       "message.received"
       (qq-gateway-message-test-event :generation "6"))
      (should (= events 1))
      (should-not (qq-state-sessions)))))

(ert-deftest qq-gateway-message-temp-is-valid-but-not-projected ()
  (qq-gateway-message-test-with-state
    (let (reason)
      (add-hook 'qq-gateway-message-projection-error-hook
                (lambda (_event _data failure) (setq reason failure)))
      (qq-gateway-message--handle-event
       "message.received"
       (qq-gateway-message-test-event
        :conversation
        '((kind . "temp")
          (name . "Temporary")
          (from_tiny_id . "1")
          (to_tiny_id . "2"))))
      (should (string-match-p "Temp conversations" reason))
      (should-not (qq-state-sessions)))))

(ert-deftest qq-gateway-message-sequence-recall-waits-for-group-message ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.recalled" (qq-gateway-message-test-recall))
    (should (= (hash-table-count qq-gateway-message--pending-recalls) 1))
    (qq-gateway-message--handle-event
     "message.received"
     (qq-gateway-message-test-event
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should (qq-state-message-recalled-p message))
      (should (= (hash-table-count qq-gateway-message--pending-recalls) 0)))))

(ert-deftest qq-gateway-message-direct-private-recall-marks-exact-message ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received" (qq-gateway-message-test-event))
    (qq-gateway-message--handle-event
     "message.recalled"
     (qq-gateway-message-test-recall
      :conversation '((kind . "private") (peer_uid . "u_peer"))
      :target
      '((kind . "message")
        (message_id . "7348923749823749823")
        (sequence . "9007199254740999"))))
    (should
     (qq-state-message-recalled-p
      (car (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-gateway-message-send-receipt-rekeys-on-exact-self-event ()
  (qq-gateway-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params callback-result
          local-id)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            `((account_id . "slot-a")
                              (generation . "7")
                              (sent_at . ,now)
                              (server_sequence . "8765432109")
                              (client_sequence . "42001")
                              (random . 123)))
                   "request-send")))
        (should
         (equal
          (qq-gateway-message-send-text
           "private:10001" "hello"
           (lambda (receipt) (setq callback-result receipt)))
          "request-send"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (equal sent-method "message.send_text"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (conversation . ((kind . "private")
                                   (peer_uin . "10001")))
                  (text . "hello"))))
        (should (equal (alist-get 'client_sequence callback-result) "42001"))
        (should (= (hash-table-count qq-gateway-message--pending-sends) 1))
        (qq-gateway-message--handle-event
         "message.received"
         (qq-gateway-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "8765432109"
          :client-sequence "42001"
          :random 123))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-gateway-message--pending-sends) 0)))))))

(ert-deftest qq-gateway-message-send-projects-closed-rich-segments ()
  (qq-gateway-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params)
      (qq-gateway-message--handle-event
       "message.received"
       (qq-gateway-message-test-event
        :sent-at now
        :conversation
        '((kind . "group")
          (group_uin . "8209413637")
          (group_name . "Protocol Lab")
          (sender_card . "Alice"))))
      (let ((segments
             '(((type . "reply")
                (data . ((id . "7348923749823749823"))))
               ((type . "at")
                (data . ((qq . "10001") (name . "Alice"))))
               ((type . "face") (data . ((id . "178"))))
               ((type . "text") (data . ((text . " hello")))))))
        (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-gateway-transport-capabilities)
                   (lambda () qq-gateway-message-test-capabilities))
                  ((symbol-function 'qq-gateway-transport-send)
                   (lambda (method params callback _errback &optional _early)
                     (setq sent-method method sent-params params)
                     (funcall callback
                              `((account_id . "slot-a")
                                (generation . "7")
                                (sent_at . ,now)
                                (server_sequence . "8765432110")
                                (client_sequence . "42002")
                                (random . 124)))
                     "request-rich-send")))
          (should
           (equal
            (qq-gateway-message-send "group:8209413637" segments)
            "request-rich-send"))
          (should (equal sent-method "message.send"))
          (should
           (equal
            sent-params
            `((account_id . "slot-a")
              (conversation
               . ((kind . "group") (group_uin . "8209413637")))
              (segments
               . (((kind . "reply")
                   (payload
                    . ((target
                        . ((message_id . "7348923749823749823")
                           (sequence . "9007199254740999")
                           (sender_uin . "10001")
                           (sender_uid . "u_peer")
                           (sent_at . ,now))))))
                  ((kind . "mention")
                   (payload
                    . ((target . ((kind . "user") (uin . "10001")))
                       (display . "Alice"))))
                  ((kind . "face") (payload . ((id . "178"))))
                  ((kind . "text")
                   (payload . ((text . " hello")))))))))
          (let ((pending
                 (seq-find
                  (lambda (message)
                    (eq (alist-get 'status message) 'pending))
                  (qq-state-session-messages "group:8209413637"))))
            (should pending)
            (should (equal (alist-get 'segments pending) segments)))
          (should (= (hash-table-count qq-gateway-message--pending-sends) 1)))))))

(ert-deftest qq-gateway-message-send-rejects-non-base-face-before-pending ()
  (qq-gateway-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (should-error
         (qq-gateway-message-send
          "private:10001"
          '(((type . "face") (data . ((id . "260"))))))
         :type 'user-error)
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-gateway-message-validator-rejects-non-base-face ()
  (let ((event
         (qq-gateway-message-test-event
          :segments '(((kind . "face") (payload . ((id . "260"))))))))
    (should-error (qq-gateway-message--validate-message-data event))))

(ert-deftest qq-gateway-message-send-rejects-unresolved-reply-before-pending ()
  (qq-gateway-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (should-error
         (qq-gateway-message-send
          "private:10001"
          '(((type . "reply")
             (data . ((id . "7348923749823749823"))))
            ((type . "text") (data . ((text . "hello"))))))
         :type 'user-error)
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-gateway-message-poke-uses-exact-uin-and-local-gray-tip ()
  (qq-gateway-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (let (sent-method sent-params applied callback-result)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (generation . "7")
                              (target_uin . "9007199254741001")))
                   "request-poke"))
                ((symbol-function 'qq-state-apply-poke-notice)
                 (lambda (notice) (setq applied notice))))
        (should
         (equal
          (qq-gateway-message-send-poke
           "group:8209413637" "9007199254741001"
           (lambda (result) (setq callback-result result)))
          "request-poke"))
        (should (equal sent-method "message.poke"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (conversation
                   . ((kind . "group") (group_uin . "8209413637")))
                  (target_uin . "9007199254741001"))))
        (should (equal (alist-get 'target_id applied)
                       "9007199254741001"))
        (should (equal (alist-get 'group_id applied) "8209413637"))
        (should (equal (alist-get 'user_id applied) "10002"))
        (should (equal (alist-get 'target_uin callback-result)
                       "9007199254741001"))))))

(ert-deftest qq-gateway-message-authoritative-poke-promotes-local-gray-tip ()
  (qq-gateway-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 1784700000))
              ((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () qq-gateway-message-test-capabilities))
              ((symbol-function 'qq-gateway-transport-send)
               (lambda (_method _params callback _errback &optional _early)
                 (funcall callback
                          '((account_id . "slot-a")
                            (generation . "7")
                            (target_uin . "9007199254741001")))
                 "request-poke")))
      (qq-gateway-message-send-poke
       "group:8209413637" "9007199254741001")
      (qq-gateway-message--handle-event
       "message.poked" (qq-gateway-message-test-poke))
      (let* ((messages
              (qq-state-session-messages "group:8209413637"))
             (message (car messages))
             (raw-event (alist-get 'raw-event message)))
        (should (= (length messages) 1))
        (should (equal (alist-get 'server-id message)
                       "7348923749823749823"))
        (should (equal (alist-get 'message_id
                                  (alist-get 'gateway_recall raw-event))
                       "7348923749823749823"))
        (should (equal (alist-get 'tips_sequence
                                  (alist-get 'gateway_recall raw-event))
                       "9007199254741007"))
        (should (equal (alist-get 'image-url
                                  (qq-state-poke-message-data message))
                       "https://example.invalid/poke.png"))))))

(ert-deftest qq-gateway-message-poke-event-rejects-numeric-identity ()
  (let ((event (qq-gateway-message-test-poke)))
    (setf (alist-get 'actor_uin (alist-get 'poke event))
          9007199254740999)
    (should-error (qq-gateway-message--validate-poke-data event))))

(ert-deftest qq-gateway-message-recall-poke-sends-original-gray-tip-metadata ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.poked" (qq-gateway-message-test-poke))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) 1784700001))
                ((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (generation . "7")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")))
                   "request-poke-recall")))
        (should (equal (qq-gateway-message-recall-poke message)
                       "request-poke-recall"))
        (should (equal sent-method "message.recall_poke"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (poke . ((message_id . "7348923749823749823")
                     (sequence . "9007199254740999")
                     (sent_at . 1784700000)
                     (tips_sequence . "9007199254741007"))))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-gateway-message-self-event-before-receipt-still-rekeys ()
  (qq-gateway-message-test-with-state
    (let ((now (floor (float-time))) response-callback local-id)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-send")))
        (qq-gateway-message-send-text "private:10001" "hello")
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-gateway-message--handle-event
         "message.received"
         (qq-gateway-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "8765432109"
          :client-sequence "42001"
          :random 123))
        (funcall response-callback
                 `((account_id . "slot-a")
                   (generation . "7")
                   (sent_at . ,now)
                   (server_sequence . "8765432109")
                   (client_sequence . "42001")
                   (random . 123)))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (alist-get 'server-id message))
          (should (= (hash-table-count qq-gateway-message--pending-sends) 0)))))))

(ert-deftest qq-gateway-message-send-failure-marks-only-pending-row ()
  (qq-gateway-message-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params _callback errback &optional _early)
                   (funcall errback
                            '((code . "send_failed") (message . "boom"))
                            "boom")
                   nil)))
        (qq-gateway-message-send-text
         "private:10001" "hello" nil
         (lambda (_body reason) (setq failure reason)))
        (let ((message
               (car (qq-state-session-messages "private:10001"))))
          (should (equal failure "boom"))
          (should (eq (alist-get 'status message) 'failed))
          (should (equal (alist-get 'error message) "boom")))))))

(ert-deftest qq-gateway-message-group-recall-awaits-authoritative-event ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received"
     (qq-gateway-message-test-event
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (generation . "7")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")))
                   "request-recall")))
        (should (equal (qq-gateway-message-recall session-key message)
                       "request-recall"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")
                        (sequence . "9007199254740999"))))))
        (should-not
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))
        (qq-gateway-message--handle-event
         "message.recalled" (qq-gateway-message-test-recall))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-gateway-message-private-recall-includes-native-metadata ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received" (qq-gateway-message-test-event))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (generation . "7")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")))
                   "request-recall")))
        (qq-gateway-message-recall session-key message)
        (should
         (equal
          (alist-get 'message sent-params)
          '((message_id . "7348923749823749823")
            (sequence . "9007199254740999")
            (client_sequence . "9007199254741001")
            (random . 7)
            (sent_at . 1784700000))))
        (should
         (equal (alist-get 'conversation sent-params)
                '((kind . "private") (peer_uin . "10001"))))))))

(ert-deftest qq-gateway-message-history-range-stays-exact-decimal ()
  (should
   (equal
    (qq-gateway-message--decimal-add-small "18446744073709551516" 99)
    "18446744073709551615"))
  (should
   (equal
    (qq-gateway-message--validate-history-range
     "18446744073709551516" "18446744073709551615")
    '("18446744073709551516" . "18446744073709551615")))
  (should (equal (qq-gateway-message--validate-history-range "0" "99")
                 '("0" . "99")))
  (dolist (range '((0 "1") ("00" "1") ("2" "1")
                   ("0" "100")
                   ("18446744073709551616" "18446744073709551616")))
    (should-error
     (qq-gateway-message--validate-history-range (car range) (cadr range))
     :type 'user-error)))

(ert-deftest qq-gateway-message-history-page-arithmetic-never-coerces-sequence ()
  (should
   (equal
    (qq-gateway-message-history-range-ending-at
     "18446744073709551615" 100)
    '("18446744073709551516" . "18446744073709551615")))
  (should
   (equal (qq-gateway-message-history-range-ending-at "12" 20)
          '("0" . "12")))
  (should
   (equal (qq-gateway-message-history-range-around "2" 20)
          '("0" . "19")))
  (should
   (equal
    (qq-gateway-message-history-range-around "9007199254740999" 20)
    '("9007199254740990" . "9007199254741009")))
  (should-error
   (qq-gateway-message-history-range-ending-at "99" 101)
   :type 'user-error))

(ert-deftest qq-gateway-message-live-frontier-uses-events-not-history ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received"
     (qq-gateway-message-test-event
      :message-id "7348923749823749823" :sequence "100"))
    (should
     (equal (qq-gateway-message-live-frontier "private:10001")
            '((message_id . "7348923749823749823")
              (sequence . "100"))))
    (qq-gateway-message--handle-event
     "message.received"
     (qq-gateway-message-test-event
      :message-id "7348923749823749822" :sequence "99"
      :client-sequence "9007199254741000"))
    (should
     (equal (alist-get 'sequence
                       (qq-gateway-message-live-frontier "private:10001"))
            "100"))
    (qq-gateway-message-revoke-projection)
    (should-not (qq-gateway-message-live-frontier "private:10001"))))

(ert-deftest qq-gateway-message-history-request-preserves-large-sequences ()
  (qq-gateway-message-test-with-state
    (let (sent-method sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-gateway-message-test-history-result
                     nil "18446744073709551516" "18446744073709551615"))
                   "request-history")))
        (should
         (equal
          (qq-gateway-message-get-history
           "group:8209413637"
           "18446744073709551516" "18446744073709551615"
           (lambda (meta) (setq callback-meta meta)))
          "request-history"))
        (should (equal sent-method "message.get_history"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (start_sequence . "18446744073709551516")
            (end_sequence . "18446744073709551615"))))
        (should (equal (plist-get callback-meta :message-count) 0))
        (should
         (equal (plist-get callback-meta :requested-start-sequence)
                "18446744073709551516"))))))

(ert-deftest qq-gateway-message-history-merges-once-and-deduplicates-live-row ()
  (qq-gateway-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (newer-event
            (qq-gateway-message-test-event
             :message-id "7348923749823749824"
             :sequence "101" :conversation conversation))
           (older
            (alist-get
             'message
             (qq-gateway-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :sent-at 1784699999
              :conversation conversation)))
           (newer (alist-get 'message newer-event))
           history-events callback-meta)
      (qq-gateway-message--handle-event "message.received" newer-event)
      (add-hook 'qq-state-change-hook
                (lambda (event)
                  (when (eq (plist-get event :type) 'history)
                    (push event history-events))))
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-gateway-message-test-history-result
                     (list older newer) "100" "101"
                     :unsupported-count 2))
                   "request-history")))
        (qq-gateway-message-get-history
         "group:8209413637" "100" "101"
         (lambda (meta) (setq callback-meta meta)))
        (should (= (length (qq-state-session-messages
                            "group:8209413637"))
                   2))
        (should (= (plist-get callback-meta :message-count) 2))
        (should (= (plist-get callback-meta :added-count) 1))
        (should (= (plist-get callback-meta :unsupported-message-count) 2))
        (should (= (length history-events) 1))
        (should
         (equal (plist-get (car history-events) :batch-message-ids)
                '("7348923749823749823" "7348923749823749824")))))))

(ert-deftest qq-gateway-message-history-applies-earlier-sequence-recall ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.recalled"
     (qq-gateway-message-test-recall
      :target '((kind . "sequence") (sequence . "100"))))
    (let* ((message
            (alist-get
             'message
             (qq-gateway-message-test-event
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-gateway-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-gateway-message-get-history
         "group:8209413637" "100" "100")
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages "group:8209413637"))))
        (should (= (hash-table-count qq-gateway-message--pending-recalls) 0))))))

(ert-deftest qq-gateway-message-history-malformed-page-is-not-partially-merged ()
  (qq-gateway-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (valid
            (alist-get
             'message
             (qq-gateway-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :conversation conversation)))
           (invalid
            (alist-get
             'message
             (qq-gateway-message-test-event
              :message-id 7348923749823749824
              :sequence "101" :conversation conversation)))
           failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-gateway-message-test-history-result
                     (list valid invalid) "100" "101"))
                   "request-history")))
        (qq-gateway-message-get-history
         "group:8209413637" "100" "101" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "message_id" failure))
        (should-not (qq-state-sessions))))))

(ert-deftest qq-gateway-message-history-rejects-cross-conversation-page ()
  (qq-gateway-message-test-with-state
    (let* ((group-message
            (alist-get
             'message
             (qq-gateway-message-test-event
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (initial-order qq-state--message-order-counter)
           failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-gateway-message-test-history-result
                     (list group-message) "100" "100"))
                   "request-history")))
        (qq-gateway-message-get-history
         "private:10001" "100" "100" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "requested conversation" failure))
        (should-not (qq-state-sessions))
        (should (= qq-state--message-order-counter initial-order))))))

(ert-deftest qq-gateway-message-history-stale-owner-cannot-mutate-state ()
  (qq-gateway-message-test-with-state
    (let (response-callback failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-history")))
        (qq-gateway-message-get-history
         "group:8209413637" "100" "100" nil
         (lambda (_body reason) (setq failure reason)))
        (qq-gateway--upsert-account
         (qq-gateway-message-test-account
          "slot-b" "11" "10003" "u_other_self")
         'changed)
        (qq-gateway-account-select "slot-b")
        (funcall
         response-callback
         (qq-gateway-message-test-history-result nil "100" "100"))
        (should (string-match-p "generation changed" failure))
        (should-not (qq-state-sessions))))))

(ert-deftest qq-gateway-message-history-recovers-lost-self-event ()
  (qq-gateway-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-message-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method _params callback _errback &optional _early)
                   (pcase method
                     ("message.send_text"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (generation . "7")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432109")
                                 (client_sequence . "42001")
                                 (random . 123))))
                     ("message.get_history"
                      (let ((message
                             (alist-get
                              'message
                              (qq-gateway-message-test-event
                               :sent-at now
                               :sender '((uin . "10002") (uid . "u_self"))
                               :recipient '((uin . "10001") (uid . "u_peer"))
                               :sequence "8765432109"
                               :client-sequence "42001"
                               :random 123))))
                        (funcall
                         callback
                         (qq-gateway-message-test-history-result
                          (list message) "8765432109" "8765432109")))))
                   (concat "request-" method))))
        (qq-gateway-message-send-text "private:10001" "hello")
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-gateway-message-get-history
         "private:10001" "8765432109" "8765432109")
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (= (hash-table-count qq-gateway-message--pending-sends) 0)))))))

(ert-deftest qq-gateway-message-malformed-event-is-protocol-violation ()
  (qq-gateway-message-test-with-state
    (let ((bad (qq-gateway-message-test-event)) violation)
      (setf (alist-get 'message_id (alist-get 'message bad)) 42)
      (cl-letf (((symbol-function
                  'qq-gateway-transport--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (setq violation
                         (apply #'format format-string arguments)))))
        (qq-gateway-message--handle-event "message.received" bad)
        (should (string-match-p "Malformed message.received event" violation))
        (should-not (qq-state-sessions))))))

(ert-deftest qq-gateway-message-selection-change-revokes-old-state ()
  (qq-gateway-message-test-with-state
    (qq-gateway-message--handle-event
     "message.received" (qq-gateway-message-test-event))
    (should (qq-state-sessions))
    (should (equal qq-gateway-message--projection-owner
                   '("slot-a" . "7")))
    (qq-gateway--upsert-account
     (qq-gateway-message-test-account
      "slot-b" "11" "10003" "u_other_self")
     'changed)
    (let ((qq-gateway-current-account-changed-hook
           '(qq-gateway-message--handle-selection-change)))
      (qq-gateway-account-select "slot-b"))
    (should-not (qq-state-sessions))
    (should (equal (alist-get 'user_id (qq-state-self-info)) "10003"))
    (should (equal qq-gateway-message--projection-owner
                   '("slot-b" . "11")))))

(ert-deftest qq-gateway-message-account-ready-claims-selected-owner ()
  (qq-gateway-message-test-with-state
    (should-not qq-gateway-message--projection-owner)
    (qq-gateway-message--handle-account-change 'ready nil)
    (should (equal qq-gateway-message--projection-owner
                   '("slot-a" . "7")))
    (should (equal (alist-get 'user_id (qq-state-self-info)) "10002"))
    (should (eq (qq-state-connection-status) 'ready))))

(provide 'qq-gateway-message-test)

;;; qq-gateway-message-test.el ends here
