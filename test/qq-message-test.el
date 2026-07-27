;;; qq-message-test.el --- Tests for service messages -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-attachment)
(require 'qq-message)

(defconst qq-message-test-capabilities
  '("message.send" "file.send" "message.poke"
    "message.send_merged_forward"
    "message.recall_poke" "message.recall" "message.set_reaction"
    "message.set_essence" "message.set_todo" "message.get_history"
    "message.get_group_history_window" "message.get_private_history"
    "message.get_forward"
    "message.mark_read")
  "Native Gateway capabilities exercised by message tests.")

(defun qq-message-test-account (&optional account-id uin uid)
  "Return an online account for ACCOUNT-ID, UIN, and UID."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . "online")
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (challenge)
    (problem)))

(defun qq-message-test-text-segment (text)
  "Return a native text segment containing TEXT."
  `((kind . "text") (payload . ((text . ,text)))))

(defun qq-message-test-reply-segment (message-id)
  "Return a UI reply segment targeting exact MESSAGE-ID."
  `((type . "reply")
    (data . ((target . ((kind . "message")
                        (message_id . ,message-id)))))))

(defun qq-message-test-wire-segment (segment)
  "Return an owned domain copy of readable SEGMENT."
  (copy-tree segment))

(defun qq-message-test-ready-image (&optional attachment-id)
  "Return one ready group-image attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee"))
    (resource_id . "res-image-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "image") (summary . "[图片]") (sub_type . 0)))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "3")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(defun qq-message-test-ready-record (&optional attachment-id)
  "Return one ready group-record attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeef"))
    (resource_id . "res-record-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "record")))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "3")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(defun qq-message-test-ready-video (&optional attachment-id)
  "Return one ready group-video attachment fixture."
  `((attachment_id
     . ,(or attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeee0"))
    (resource_id . "res-video-wire")
    (account_id . "slot-a")
    (conversation . ((kind . "group") (group_uin . "8209413637")))
    (use . ((kind . "video")
            (thumbnail_resource_id . "res-video-thumbnail-wire")))
    (phase . "ready")
    (bytes_done . "0")
    (bytes_total . "6")
    (fast_path . t)
    (created_at . 1784700000)
    (updated_at . 1784700001)
    (error)))

(cl-defun qq-message-test-event
    (&key
     (account-id "slot-a")
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
        (segments
         . ,(mapcar #'qq-message-test-wire-segment
                    (append segments nil)))))))

(cl-defun qq-message-test-recall
    (&key
     (account-id "slot-a")
     (conversation '((kind . "group") (group_uin . "8209413637")))
     (target '((kind . "sequence") (sequence . "9007199254740999")))
     (author-uid "u_peer") (operator-uid "u_admin")
     (tip "message recalled"))
  "Return one closed native Gateway recall event payload."
  `((account_id . ,account-id)
    (recall
     . ((conversation . ,(copy-tree conversation))
        (target . ,(copy-tree target))
        (author_uid . ,author-uid)
        (operator_uid . ,operator-uid)
        (tip . ,tip)))))

(cl-defun qq-message-test-poke
    (&key
     (account-id "slot-a")
     (message-id "7348923749823749823") (sent-at 1784700000)
     (sequence "9007199254740999") (group-uin "8209413637")
     (actor-uin "10002") (target-uin "9007199254741001")
     (tips-sequence "9007199254741007") (valid-before 1784700120))
  "Return one authoritative native Gateway group poke event payload."
  `((account_id . ,account-id)
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

(cl-defun qq-message-test-reaction
    (&key
     (account-id "slot-a")
     (group-uin "8209413637") (sequence "9007199254740999")
     (operator-uid "u_member") (operator-uin "10001")
     (emoji-id "178") (emoji-type "1") (is-add t) (count 3))
  "Return one authoritative native Gateway group reaction event payload."
  `((account_id . ,account-id)
    (reaction
     . ((conversation . ((kind . "group") (group_uin . ,group-uin)))
        (sequence . ,sequence)
        (operator_uid . ,operator-uid)
        ,@(when operator-uin `((operator_uin . ,operator-uin)))
        (emoji_id . ,emoji-id)
        (emoji_type . ,emoji-type)
        (is_add . ,is-add)
        (count . ,count)))))

(cl-defun qq-message-test-essence
    (&key
     (account-id "slot-a")
     (group-uin "8209413637") (sequence "9007199254740999")
     (random 7) (is-set t) (sender-uin "10001")
     (operator-uin "10002") (changed-at 1784700000)
     (operator-nickname "Moderator") (sender-nickname "Alice"))
  "Return one authoritative native Gateway group essence event payload."
  `((account_id . ,account-id)
    (essence
     . ((conversation . ((kind . "group") (group_uin . ,group-uin)))
        (sequence . ,sequence)
        (random . ,random)
        (is_set . ,is-set)
        (sender_uin . ,sender-uin)
        (operator_uin . ,operator-uin)
        (changed_at . ,changed-at)
        ,@(when operator-nickname
            `((operator_nickname . ,operator-nickname)))
        ,@(when sender-nickname
            `((sender_nickname . ,sender-nickname)))))))

(cl-defun qq-message-test-history-result
    (messages start-sequence end-sequence
              &key (response-start start-sequence)
              (response-end end-sequence) (unsupported-count 0))
  "Return a closed history result containing MESSAGES.

START-SEQUENCE and END-SEQUENCE are echoed as the requested range."
  `((account_id . "slot-a")
    (requested_start_sequence . ,start-sequence)
    (requested_end_sequence . ,end-sequence)
    (response_start_sequence . ,response-start)
    (response_end_sequence . ,response-end)
    (unsupported_message_count . ,unsupported-count)
    (messages . ,(vconcat (mapcar #'copy-tree (append messages nil))))))

(cl-defun qq-message-test-private-history-result
    (messages requested response &key complete (unsupported-count 0))
  "Return a closed private history result containing MESSAGES."
  `((account_id . "slot-a")
    (requested_cursor . ,(copy-tree requested))
    (response_cursor . ,(copy-tree response))
    (complete . ,(if complete t :false))
    (unsupported_message_count . ,unsupported-count)
    (messages . ,(vconcat (mapcar #'copy-tree (append messages nil))))))

(cl-defun qq-message-test-group-history-window-result
    (messages after frontier start end next
              &key caught-up (unsupported-count 0))
  "Return a closed authoritative group history window result."
  `((account_id . "slot-a")
    (requested_after_sequence . ,after)
    (frontier_sequence . ,frontier)
    (requested_start_sequence . ,start)
    (requested_end_sequence . ,end)
    (response_start_sequence . ,start)
    (response_end_sequence . ,end)
    (next_after_sequence . ,next)
    (caught_up . ,(if caught-up t :false))
    (unsupported_message_count . ,unsupported-count)
    (messages . ,(vconcat (mapcar #'copy-tree (append messages nil))))))

(defmacro qq-message-test-with-state (&rest body)
  "Run BODY with one selected account and isolated message projection state."
  (declare (indent 0) (debug t))
  `(let ((qq-account--accounts (make-hash-table :test #'equal))
         (qq-account--account-order nil)
         (qq-account--current-account-id nil)
         (qq-account--gateway-instance-id nil)
         (qq-account--resync-request-id nil)
         (qq-account-registry-changed-hook nil)
         (qq-account-selection-changed-hook nil)
         (qq-account-desync-hook nil)
         (qq-account-projection-resync-hook nil)
         (qq-attachment--attachments
          (make-hash-table :test #'equal))
         (qq-attachment--order nil)
         (qq-attachment-changed-hook nil)
         (qq-message--peer-uin-by-uid
          (make-hash-table :test #'equal))
         (qq-message--pending-recalls
          (make-hash-table :test #'equal))
         (qq-message--pending-reactions
          (make-hash-table :test #'equal))
         (qq-message--pending-essences
          (make-hash-table :test #'equal))
         (qq-message--pending-sends
          (make-hash-table :test #'equal))
         (qq-message--live-frontiers
          (make-hash-table :test #'equal))
         (qq-runtime--app nil)
         (qq-runtime--accounts (make-hash-table :test #'equal))
         (qq-state--partitions (make-hash-table :test #'equal))
         (qq-state--active-account-id nil)
         (qq-message-event-hook nil)
         (qq-message-projection-error-hook nil)
         (qq-server--state 'ready)
         (qq-state-change-hook nil))
     (unwind-protect
         (progn
           (qq-state-reset)
           (qq-account--replace-accounts
            (list (qq-message-test-account)) 'ready "gateway-test")
           (qq-runtime-with-account "slot-a"
             (qq-state-reset)
             ,@body))
       (qq-runtime-stop)
       (qq-state-reset))))

(ert-deftest qq-message-send-file-publishes-staged-resource-to-group ()
  (qq-message-test-with-state
    (let (method params)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (sent-method sent-params _callback _errback
                                      &optional _early)
                   (setq method sent-method
                         params sent-params)
                   "file-send-request")))
        (should
         (equal
          (qq-message-send-file
           "group:8209413637" "res-group-file")
          "file-send-request"))
        (should (equal method "file.send"))
        (should
         (equal
          params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (resource_id . "res-group-file"))))
        (should-error
         (qq-message-send-file
          "private:10001" "res-group-file")
         :type 'user-error)))))

(ert-deftest qq-message-send-merged-forward-preserves-source-order ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p) (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (resource_id . "native-long-message-resource")
                      (sent_at . 1785100000)
                      (server_sequence . "8765432109")
                      (client_sequence . "42001")
                      (random . 123)))
                   "request-forward")))
        (should
         (equal
          (qq-message-send-merged-forward
           "private:10001" "group:8209413637"
           '("9007199254742007001"
             "9007199254742007001"
             "9007199254742007002")
           (lambda (receipt) (setq callback-result receipt)))
          "request-forward"))
        (should (equal sent-method "message.send_merged_forward"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (destination . ((kind . "group")
                            (group_uin . "8209413637")))
            (source . ((kind . "private")
                       (peer_uin . "10001")))
            (message_ids
             . ("9007199254742007001"
                "9007199254742007001"
                "9007199254742007002")))))
        (should
         (equal (alist-get 'resource_id callback-result)
                "native-long-message-resource"))))))

(ert-deftest qq-message-send-merged-forward-rejects-invalid-input-locally ()
  (qq-message-test-with-state
    (let (transport-called)
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments)
                   (setq transport-called t))))
        (should-error
         (qq-message-send-merged-forward
          "private:10001" "group:8209413637" nil)
         :type 'user-error)
        (should-error
         (qq-message-send-merged-forward
          "private:10001" "group:8209413637" '("local-pending"))
         :type 'user-error)
        (should-error
         (qq-message-send-merged-forward
          "service:u:mail:x" "group:8209413637"
          '("9007199254742007001"))
         :type 'user-error))
      (should-not transport-called))))

(ert-deftest qq-message-conversation-params-use-session-identity ()
  (should
   (equal
    (qq-message--conversation-params
     "private:18446744073709551615")
    '((kind . "private") (peer_uin . "18446744073709551615"))))
  (should
   (equal
    (qq-message--conversation-params "group:8209413637")
    '((kind . "group") (group_uin . "8209413637")))))

(ert-deftest qq-message-event-boundary-domainizes-and-owns-wire-data ()
  (qq-message-test-with-state
    (let* ((source-text (copy-sequence "mutable"))
           (event
            (qq-message-test-event
             :segments
             (list
              (qq-message-test-text-segment source-text)
              '((kind . "unsupported")
                (payload . ((native_keys . ("ark" "xml"))
                            (summary . "unsupported")))))))
           observed)
      (let* ((message (alist-get 'message event))
             (segments (alist-get 'segments message)))
        (setf (alist-get 'native_keys
                         (alist-get 'payload (cadr segments)))
              ["ark" "xml"]
              (alist-get 'segments message)
              (vconcat segments)))
      (let ((qq-message-event-hook
             (list (lambda (_event data) (setq observed data)))))
        (qq-rpc--handle-transport-event
         "message.received" event))
      (let* ((message (alist-get 'message observed))
             (segments (alist-get 'segments message))
             (text (alist-get 'text (alist-get 'payload (car segments))))
             (native-keys
              (alist-get 'native_keys
                         (alist-get 'payload (cadr segments)))))
        (should (listp segments))
        (should (listp native-keys))
        (should (equal (alist-get 'message_id message)
                       "7348923749823749823"))
        (should (equal text source-text))
        (should-not (eq text source-text))
        (aset text 0 ?X)
        (should (equal source-text "mutable"))))))

(ert-deftest qq-message-mark-read-sends-exact-message-references ()
  (qq-message-test-with-state
    (let* ((private
            '((session-key . "private:10001")
              (server-id . "7348923749823749823")
              (gateway-account-id . "slot-a")))
           (group
            '((session-key . "group:8209413637")
              (server-id . "7348923749823749824")
              (gateway-account-id . "slot-a")))
           calls receipts)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method (copy-tree params)) calls)
                   (let ((message (alist-get 'message params)))
                     (funcall
                      callback
                      `((account_id . "slot-a")
                        (message_id . ,(alist-get 'message_id message)))))
                   (format "read-%d" (length calls)))))
        (should (qq-message-read-capable-p private))
        (should (qq-message-read-capable-p group))
        (qq-message-mark-read
         private (lambda (receipt) (push receipt receipts)))
        (qq-message-mark-read
         group (lambda (receipt) (push receipt receipts))))
      (setq calls (nreverse calls)
            receipts (nreverse receipts))
      (should
       (equal
        calls
        '(("message.mark_read"
           ((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (message . ((message_id . "7348923749823749823")))))
          ("message.mark_read"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749824"))))))))
      (should
       (equal receipts
              '(((account_id . "slot-a")
                 (message_id . "7348923749823749823"))
                ((account_id . "slot-a")
                 (message_id . "7348923749823749824"))))))))

(ert-deftest qq-message-projects-group-segments-and-metadata ()
  (qq-message-test-with-state
    (let ((segments
           (list
            (qq-message-test-text-segment "hello")
            '((kind . "at")
              (payload . ((qq . "10002") (name . "Primary"))))
            '((kind . "reply")
              (payload
               . ((target
                   . ((kind . "native")
                      (sequence . "4000000001"))))))
            '((kind . "face") (payload . ((id . "178"))))
            '((kind . "record") (payload . ((duration_seconds . 17))))
            '((kind . "unsupported")
              (payload
               . ((native_keys . ("text.pb_reserve"))
                  (summary . "text.pb_reserve")
                  (raw . ((fallback_text . "@Alice")))))))))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
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
        (should-not (assq 'native-sent-at message))
        (should (equal (alist-get 'gateway-account-id message) "slot-a"))
        (should (equal (alist-get 'sender-name message) "Alice"))
        (should (equal (alist-get 'mention-kinds message) '(at-me)))
        (should (eq (alist-get 'status message) 'received))
        (should (equal (mapcar (lambda (it) (alist-get 'type it)) internal)
                       '("text" "at" "reply" "face" "record"
                         "__unsupported")))
        (should
         (equal
          (alist-get 'message_seq (alist-get 'data (nth 2 internal)))
          "4000000001"))
        (should (equal (alist-get 'id (alist-get 'data (nth 3 internal)))
                       "178"))
        (should (= (alist-get 'duration_seconds
                              (alist-get 'data (nth 4 internal)))
                   17))
        (should (equal (alist-get 'title (qq-state-session session-key))
                       "Protocol Lab"))))))

(ert-deftest qq-message-record-projects-opaque-media-handle ()
  (let* ((media-id "media-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
         (record `((kind . "record")
                   (payload . ((duration_seconds . 17)
                               (media_id . ,media-id)))))
         (internal (qq-message--segment-to-internal record)))
    (should (equal (alist-get 'media_id (alist-get 'data internal)) media-id))))

(ert-deftest qq-message-projects-private-peer-by-self-endpoint ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-event))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           (session (qq-state-session session-key)))
      (should-not (alist-get 'self-p message))
      (should (equal (alist-get 'peer-uin message) "10001"))
      (should (equal (alist-get 'peer-uid message) "u_peer"))
      (should (equal (alist-get 'peer-uid session) "u_peer"))
      (should (equal (gethash '("slot-a" "u_peer")
                              qq-message--peer-uin-by-uid)
                     "10001")))))

(ert-deftest qq-message-projects-nonselected-managed-account ()
  (qq-message-test-with-state
    (let (observed)
      (add-hook 'qq-message-event-hook
                (lambda (event data) (setq observed (list event data))))
      (qq-account--upsert-account
       (qq-message-test-account
        "slot-b" "10003" "u_other_self")
       'changed)
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :account-id "slot-b"
        :recipient '((uin . "10003") (uid . "u_other_self"))))
      (should (equal (car observed) "message.received"))
      (should (equal (alist-get 'account_id (cadr observed)) "slot-b"))
      (should-not (qq-state-sessions))
      (qq-runtime-with-account "slot-b"
        (should (qq-state-session "private:10001"))
        (should
         (equal (alist-get 'gateway-account-id
                           (car (qq-state-session-messages "private:10001")))
                "slot-b"))))))

(ert-deftest qq-message-temp-is-valid-but-not-projected ()
  (qq-message-test-with-state
    (let (reason)
      (add-hook 'qq-message-projection-error-hook
                (lambda (_event _data failure) (setq reason failure)))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :conversation
        '((kind . "temp")
          (name . "Temporary")
          (from_tiny_id . "1")
          (to_tiny_id . "2"))))
      (should (string-match-p "Temp conversations" reason))
      (should-not (qq-state-sessions)))))

(ert-deftest qq-message-sequence-recall-waits-for-group-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.recalled" (qq-message-test-recall))
    (should (= (hash-table-count qq-message--pending-recalls) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should (qq-state-message-recalled-p message))
      (should (= (hash-table-count qq-message--pending-recalls) 0)))))

(ert-deftest qq-message-reaction-applies-authoritative-count ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction :count 7))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (reaction (car (qq-state-message-reactions message))))
      (should (equal (alist-get 'emoji-id reaction) "178"))
      (should (equal (alist-get 'emoji-type reaction) "1"))
      (should (= (alist-get 'count reaction) 7))
      (should-not (alist-get 'chosen-p reaction)))))

(ert-deftest qq-message-sequence-reaction-waits-for-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction
      :operator-uid "u_self" :operator-uin "10002" :count 4))
    (should (= (hash-table-count qq-message--pending-reactions) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           (reaction (car (qq-state-message-reactions message))))
      (should (= (alist-get 'count reaction) 4))
      (should (alist-get 'chosen-p reaction))
      (should (= (hash-table-count
                  qq-message--pending-reactions)
                 0)))))

(ert-deftest qq-message-essence-applies-authoritative-metadata ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should (eq (alist-get 'essence-p message) t))
      (should (equal (alist-get 'essence-sender-id message) "10001"))
      (should (equal (alist-get 'essence-operator-id message) "10002"))
      (should (= (alist-get 'essence-changed-at message) 1784700000))
      (should (equal (alist-get 'essence-operator-nickname message)
                     "Moderator")))))

(ert-deftest qq-message-native-target-essence-waits-for-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence :is-set :false))
    (should (= (hash-table-count qq-message--pending-essences) 1))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let ((message
           (car (qq-state-session-messages "group:8209413637"))))
      (should-not (alist-get 'essence-p message))
      (should (equal (alist-get 'essence-operator-id message) "10002"))
      (should (= (hash-table-count qq-message--pending-essences) 0)))))

(ert-deftest qq-message-direct-private-recall-marks-exact-message ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-event))
    (qq-message--handle-event
     "message.recalled"
     (qq-message-test-recall
      :conversation '((kind . "private") (peer_uid . "u_peer"))
      :target
      '((kind . "message")
        (message_id . "7348923749823749823")
        (sequence . "9007199254740999"))))
    (should
     (qq-state-message-recalled-p
      (car (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-send-receipt-rekeys-on-exact-self-event ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params callback-result
          local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (setq sent-method method sent-params params)
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432109")
                                 (client_sequence . "42001")
                                 (random . 123)))
                      "request-send")
                     ;; Private send immediately tries C2C history recovery.
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "hello")))))
           nil
           (lambda (receipt) (setq callback-result receipt)))
          "request-send"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (equal sent-method "message.send"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (conversation . ((kind . "private")
                                   (peer_uin . "10001")))
                  (segments
                   . (((kind . "text")
                       (payload . ((text . "hello")))))))))
        (should (equal (alist-get 'client_sequence callback-result) "42001"))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
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
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))
(ert-deftest qq-message-private-rekeys-when-receipt-sequence-is-client-echo ()
  "C2C PbSendMsgResp field 14 may echo client_sequence, not ContentHead.Sequence.

client_sequence + session must still rekey even when conversation sequences differ."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 ;; field-14 style client echo, not conversation seq
                                 (server_sequence . "42001")
                                 (client_sequence . "42001")
                                 (random . 99)))
                      "request-private-c2c")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "c2c-hello"))))))
          "request-private-c2c"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        ;; Live private self-echo: conversation sequence differs from receipt.
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "9007199254740999"
          :client-sequence "42001"
          :random 99
          :segments (list (qq-message-test-text-segment "c2c-hello"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-private-rekeys-when-push-sequence-is-outbound-client-seq ()
  "Live C2C CommonMessage stores outbound client_sequence on ContentHead.Sequence.

receipt.client_sequence=40909 and receipt.server_sequence=30202, while the
push carries sequence=40909 and client_sequence=30202."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "30202")
                                 (client_sequence . "40909")
                                 (random . 694993522)))
                      "request-private-inverted")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "test"))))))
          "request-private-inverted"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (should (= (hash-table-count qq-message--pending-sends) 1))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "40909"
          :client-sequence "30202"
          :random 694993522
          :segments (list (qq-message-test-text-segment "test"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-private-send-recovers-snowflake-via-c2c-history ()
  "When OlPush self-echo is missing, fetch SsoGetC2cMsg by server_sequence."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id methods)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push method methods)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "30203")
                                 (client_sequence . "40910")
                                 (random . 1689609174)))
                      "request-private-recover")
                     ("message.get_history"
                      (should
                       (equal params
                              '((account_id . "slot-a")
                                (conversation
                                 . ((kind . "private")
                                    (peer_uin . "10001")))
                                (start_sequence . "30203")
                                (end_sequence . "30203"))))
                      (funcall callback
                               `((account_id . "slot-a")
                                 (requested_start_sequence . "30203")
                                 (requested_end_sequence . "30203")
                                 (response_start_sequence . "30203")
                                 (response_end_sequence . "30203")
                                 (unsupported_message_count . 0)
                                 (messages
                                  .
                                  (,(alist-get
                                     'message
                                     (qq-message-test-event
                                      :sent-at now
                                      :message-id "72057595727537110"
                                      :sender '((uin . "10002")
                                                (uid . "u_self"))
                                      :recipient '((uin . "10001")
                                                   (uid . "u_peer"))
                                      :sequence "40910"
                                      :client-sequence "30203"
                                      :random 1689609174
                                      :segments
                                      (list
                                       (qq-message-test-text-segment
                                        "rekey-probe"))))))))
                      "request-private-history")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "private:10001"
           '(((type . "text") (data . ((text . "rekey-probe"))))))
          "request-private-recover"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (equal (nreverse methods)
                         '("message.send" "message.get_history")))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "72057595727537110"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-group-send-recovers-snowflake-via-group-history ()
  "When the group self-echo push never arrives, fetch SsoGetGroupMsg by sequence."
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id methods)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push method methods)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432112")
                                 (client_sequence . "42005")
                                 (random . 7777)))
                      "request-group-recover")
                     ("message.get_history"
                      (should
                       (equal params
                              '((account_id . "slot-a")
                                (conversation
                                 . ((kind . "group")
                                    (group_uin . "8209413637")))
                                (start_sequence . "8765432112")
                                (end_sequence . "8765432112"))))
                      (funcall callback
                               `((account_id . "slot-a")
                                 (requested_start_sequence . "8765432112")
                                 (requested_end_sequence . "8765432112")
                                 (response_start_sequence . "8765432112")
                                 (response_end_sequence . "8765432112")
                                 (unsupported_message_count . 0)
                                 (messages
                                  .
                                  (,(alist-get
                                     'message
                                     (qq-message-test-event
                                      :sent-at now
                                      :message-id "72057595727537120"
                                      :sender '((uin . "10002")
                                                (uid . "u_self"))
                                      :conversation
                                      '((kind . "group")
                                        (group_uin . "8209413637"))
                                      :sequence "8765432112"
                                      :random 7777
                                      :message-type 82
                                      :segments
                                      (list
                                       (qq-message-test-text-segment
                                        "group-rekey-probe"))))))))
                      "request-group-history")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "group:8209413637"
           '(((type . "text") (data . ((text . "group-rekey-probe"))))))
          "request-group-recover"))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "group:8209413637"))))
        (let* ((messages (qq-state-session-messages "group:8209413637"))
               (message
                (or (seq-find
                     (lambda (it) (equal (alist-get 'local-id it) local-id))
                     messages)
                    (car messages))))
          (should (equal (nreverse methods)
                         '("message.send" "message.get_history")))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "72057595727537120"))
          (should (eq (alist-get 'status message) 'sent))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-send-lifts-reference-out-of-rich-content ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) sent-method sent-params)
      (let ((segments
             (list
              (qq-message-test-reply-segment "7348923749823749823")
              '((type . "at")
                (data . ((qq . "10001") (name . "Alice"))))
              '((type . "face") (data . ((id . "178"))))
              '((type . "text") (data . ((text . " hello")))))))
        (cl-letf (((symbol-function 'qq-server-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-server-capabilities)
                   (lambda () qq-message-test-capabilities))
                  ((symbol-function 'qq-server-send)
                   (lambda (method params callback errback &optional _early)
                     (pcase method
                       ("message.send"
                        (setq sent-method method sent-params params)
                        (funcall callback
                                 `((account_id . "slot-a")
                                   (sent_at . ,now)
                                   (server_sequence . "8765432110")
                                   (client_sequence . "42002")
                                   (random . 124)))
                        "request-rich-send")
                       ;; Send recovery fetches the snowflake from history.
                       ("message.get_history"
                        (funcall errback nil "history not mocked in this test")
                        "request-history-skip")
                       (_ (error "unexpected method %S" method))))))
          (should
           (equal
            (qq-message-send "group:8209413637" segments)
            "request-rich-send"))
          (should (equal sent-method "message.send"))
          (should
           (equal
            sent-params
            `((account_id . "slot-a")
              (conversation
               . ((kind . "group") (group_uin . "8209413637")))
              (reply_to
               . ((kind . "message")
                  (message_id . "7348923749823749823")))
              (segments
               . (((kind . "mention")
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
          (should (= (hash-table-count qq-message--pending-sends) 1)))))))

(ert-deftest qq-message-send-image-keeps-local-path-off-wire ()
  (qq-message-test-with-state
    (let* ((now (floor (float-time)))
           (attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")
           (wire-segments
            `(((type . "image")
               (data . ((attachment_id . ,attachment-id))))))
           (optimistic-segments
            '(((type . "image")
               (data . ((file . "/tmp/private-source.png")
                        (summary . "[图片]")
                        (sub_type . 0))))))
           sent-params)
      (qq-attachment--upsert
       (qq-message-test-ready-image attachment-id) 'test)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback errback &optional _early)
                   (pcase method
                     ("message.send"
                      (setq sent-params params)
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432111")
                                 (client_sequence . "42003")
                                 (random . 125)))
                      "request-image-send")
                     ("message.get_history"
                      (funcall errback nil "history not mocked in this test")
                      "request-history-skip")
                     (_ (error "unexpected method %S" method))))))
        (should
         (equal
          (qq-message-send
           "group:8209413637" wire-segments nil nil nil optimistic-segments)
          "request-image-send"))
        (should
         (equal
          (alist-get 'segments sent-params)
          `(((kind . "image")
             (payload . ((attachment_id . ,attachment-id)))))))
        (should-not
         (string-match-p "/tmp/private-source\\.png"
                         (prin1-to-string sent-params)))
        (let ((pending
               (seq-find
                (lambda (message)
                  (eq (alist-get 'status message) 'pending))
                (qq-state-session-messages "group:8209413637"))))
          (should pending)
          (should (equal (alist-get 'segments pending)
                         optimistic-segments)))))))

(ert-deftest qq-message-serializes-record-attachment-id-only ()
  (qq-message-test-with-state
    (let ((attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeef"))
      (qq-attachment--upsert
       (qq-message-test-ready-record attachment-id) 'test)
      (should
       (equal
        (qq-message--prepare-outbound
         "group:8209413637"
         `(((type . "record")
            (data . ((attachment_id . ,attachment-id)))))
         "slot-a")
        `(:reply-to nil
          :segments
          (((kind . "record")
            (payload . ((attachment_id . ,attachment-id))))))))
      (should-error
       (qq-message--prepare-outbound
        "group:8209413637"
        `(((type . "image")
           (data . ((attachment_id . ,attachment-id)))))
        "slot-a")
       :type 'user-error))))

(ert-deftest qq-message-serializes-video-attachment-id-only ()
  (qq-message-test-with-state
    (let ((attachment-id "att-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeee0"))
      (qq-attachment--upsert
       (qq-message-test-ready-video attachment-id) 'test)
      (should
       (equal
        (qq-message--prepare-outbound
         "group:8209413637"
         `(((type . "video")
            (data . ((attachment_id . ,attachment-id)
                     (file . "/tmp/must-not-cross-wire.mp4")))))
         "slot-a")
        `(:reply-to nil
          :segments
          (((kind . "video")
            (payload . ((attachment_id . ,attachment-id))))))))
      (should-error
       (qq-message--prepare-outbound
        "group:8209413637"
        `(((type . "image")
           (data . ((attachment_id . ,attachment-id)))))
        "slot-a")
       :type 'user-error))))

(ert-deftest qq-message-send-allows-unloaded-reply-reference ()
  (qq-message-test-with-state
    (let (sent-method sent-params)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params _callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   "request-unloaded-reply")))
        (should
         (equal
          (qq-message-send
           "private:10001"
           (list
            (qq-message-test-reply-segment "7348923749823749823")
            '((type . "text") (data . ((text . "hello"))))))
          "request-unloaded-reply"))
        (should (equal sent-method "message.send"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (reply_to
             . ((kind . "message")
                (message_id . "7348923749823749823")))
            (segments
             . (((kind . "text")
                 (payload . ((text . "hello")))))))))
        (let ((pending
               (car (qq-state-session-messages "private:10001"))))
          (should pending)
          (should
           (equal
            (alist-get 'segments pending)
            (list
             (qq-message-test-reply-segment "7348923749823749823")
             '((type . "text") (data . ((text . "hello")))))))
          (should (eq (alist-get 'status pending) 'pending)))))))

(ert-deftest
    qq-message-send-rejects-malformed-or-duplicate-reply-before-pending
    ()
  (qq-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (dolist (message-id
                 '(42 "0" "07348923749823749823"
                   "18446744073709551616"))
          (should-error
           (qq-message-send
            "private:10001"
            (list
             (qq-message-test-reply-segment message-id)
             '((type . "text") (data . ((text . "hello"))))))
           :type 'user-error))
        (should-error
         (qq-message-send
          "private:10001"
          (list
           (qq-message-test-reply-segment "7348923749823749823")
           '((type . "text") (data . ((text . "hello"))))
           (qq-message-test-reply-segment "7348923749823749824")))
         :type 'user-error)
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-send-requires-non-reply-content ()
  (qq-message-test-with-state
    (let ((sent nil))
      (cl-letf (((symbol-function 'qq-server-send)
                 (lambda (&rest _arguments) (setq sent t))))
        (dolist
            (segments
             (list nil
                   (list
                    (qq-message-test-reply-segment
                     "7348923749823749823"))))
          (should-error
           (qq-message-send "private:10001" segments)
           :type 'user-error))
        (should-not sent)
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-reply-does-not-count-toward-content-limit ()
  (qq-message-test-with-state
    (let* ((reply
             (qq-message-test-reply-segment "18446744073709551615"))
           (contents
            (cl-loop repeat 128
                     collect
                     '((type . "text") (data . ((text . "hello"))))))
           (outbound
            (qq-message--prepare-outbound
             "private:10001" (cons reply contents) "slot-a")))
      (should (= (length (plist-get outbound :segments)) 128))
      (should
       (equal
        (plist-get outbound :reply-to)
        '((kind . "message")
          (message_id . "18446744073709551615")))))))

(ert-deftest qq-message-poke-uses-exact-uin-and-local-gray-tip ()
  (qq-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (let (sent-method sent-params applied callback-result)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (target_uin . "9007199254741001")))
                   "request-poke"))
                ((symbol-function 'qq-state-apply-poke-notice)
                 (lambda (notice) (setq applied notice))))
        (should
         (equal
          (qq-message-send-poke
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

(ert-deftest qq-message-authoritative-poke-promotes-local-gray-tip ()
  (qq-message-test-with-state
    (qq-state-upsert-session
     "group:8209413637"
     '((type . group) (target-id . "8209413637") (title . "Protocol Lab")))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 1784700000))
              ((symbol-function 'qq-server-ready-p)
               (lambda () t))
              ((symbol-function 'qq-server-capabilities)
               (lambda () qq-message-test-capabilities))
              ((symbol-function 'qq-server-send)
               (lambda (_method _params callback _errback &optional _early)
                 (funcall callback
                          '((account_id . "slot-a")
                            (target_uin . "9007199254741001")))
                 "request-poke")))
      (qq-message-send-poke
       "group:8209413637" "9007199254741001")
      (qq-message--handle-event
       "message.poked" (qq-message-test-poke))
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

(ert-deftest qq-message-reaction-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params callback-result)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")
                              (emoji_id . "128077")
                              (set . t)))
                   "request-reaction")))
        (should
         (equal
          (qq-message-set-reaction
           message "128077" t
           (lambda (result) (setq callback-result result)))
          "request-reaction"))
        (should (equal sent-method "message.set_reaction"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (emoji_id . "128077")
            (set . t))))
        (should (equal (alist-get 'emoji_id callback-result) "128077"))
        (should-not
         (qq-state-message-reactions
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.reaction_changed"
         (qq-message-test-reaction
          :operator-uin "10002"
          :emoji-id "128077"
          :emoji-type "2"
          :count 1))
        (let ((reaction
               (car (qq-state-message-reactions
                     (car (qq-state-session-messages session-key))))))
          (should (equal (alist-get 'emoji-id reaction) "128077"))
          (should (equal (alist-get 'emoji-type reaction) "2"))
          (should (= (alist-get 'count reaction) 1))
          (should (alist-get 'chosen-p reaction)))))))

(ert-deftest qq-message-essence-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :random 4277998232
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params callback-result)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")
                              (random . 4277998232)
                              (set . t)))
                   "request-essence")))
        (should
         (equal
          (qq-message-set-essence
           message t (lambda (result) (setq callback-result result)))
          "request-essence"))
        (should (equal sent-method "message.set_essence"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (set . t))))
        (should (equal (alist-get 'message_id callback-result)
                       "7348923749823749823"))
        (should-not
         (alist-get
          'essence-p
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.essence_changed"
         (qq-message-test-essence :random 4277998232))
        (should
         (eq (alist-get
              'essence-p
              (car (qq-state-session-messages session-key)))
             t))))))

(ert-deftest qq-message-authoritative-essence-beats-racing-receipt ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           response-callback)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-essence")))
        (qq-message-set-essence message t)
        (qq-message--handle-event
         "message.essence_changed"
         (qq-message-test-essence :is-set :false))
        (funcall response-callback
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749823")
                   (sequence . "9007199254740999")
                   (random . 7)
                   (set . t)))
        (let ((projected (car (qq-state-session-messages session-key))))
          (should-not (alist-get 'essence-p projected))
          (should (= (alist-get 'essence-changed-at projected)
                     1784700000)))))))

(ert-deftest qq-message-todo-keeps-exact-target-and-operation ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group") (group_uin . "8209413637")
        (group_name . "Protocol Lab") (sender_card . "Alice"))))
    (let* ((message
            (car (qq-state-session-messages "group:8209413637")))
           calls receipts)
      (dolist (key '(message-seq native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method (copy-tree params)) calls)
                   (funcall
                   callback
                    `((account_id . "slot-a")
                      (group_uin . "8209413637")
                      (message_id . "7348923749823749823")
                      (sequence . "9007199254740999")
                      (operation . ,(alist-get 'operation params))))
                   (format "todo-%s" (alist-get 'operation params)))))
        (dolist (operation '(set complete cancel))
          (should
           (equal
            (qq-message-set-todo
             message operation
             (lambda (receipt) (push receipt receipts)))
            (format "todo-%s" operation)))))
      (should
       (equal
        (nreverse calls)
        '(("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "set")))
          ("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "complete")))
          ("message.set_todo"
           ((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (message . ((message_id . "7348923749823749823")))
            (operation . "cancel"))))))
      (should
       (equal (mapcar (lambda (receipt) (alist-get 'operation receipt))
                      (nreverse receipts))
              '("set" "complete" "cancel"))))))

(ert-deftest qq-message-recall-poke-sends-original-gray-tip-metadata ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.poked" (qq-message-test-poke))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-method sent-params)
      (cl-letf (((symbol-function 'float-time)
                 (lambda (&optional _time) 1784700001))
                ((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (message_id . "7348923749823749823")
                              (sequence . "9007199254740999")))
                   "request-poke-recall")))
        (should (equal (qq-message-recall-poke message)
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

(ert-deftest qq-message-self-event-before-receipt-still-rekeys ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) response-callback local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-send")))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello"))))))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-message--handle-event
         "message.received"
         (qq-message-test-event
          :sent-at now
          :sender '((uin . "10002") (uid . "u_self"))
          :recipient '((uin . "10001") (uid . "u_peer"))
          :sequence "8765432109"
          :client-sequence "42001"
          :random 123))
        (funcall response-callback
                 `((account_id . "slot-a")
                   (sent_at . ,now)
                   (server_sequence . "8765432109")
                   (client_sequence . "42001")
                   (random . 123)))
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (alist-get 'server-id message))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-send-failure-marks-only-pending-row ()
  (qq-message-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params _callback errback &optional _early)
                   (funcall errback
                            '((code . "send_failed") (message . "boom"))
                            "boom")
                   nil)))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello")))))
         nil nil
         (lambda (_body reason) (setq failure reason)))
        (let ((message
               (car (qq-state-session-messages "private:10001"))))
          (should (equal failure "boom"))
          (should (eq (alist-get 'status message) 'failed))
          (should (equal (alist-get 'error message) "boom")))))))

(ert-deftest qq-message-group-recall-projects-typed-acknowledgement ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :conversation
      '((kind . "group")
        (group_uin . "8209413637")
        (group_name . "Protocol Lab")
        (sender_card . "Alice"))))
    (let* ((session-key "group:8209413637")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (setq message (assq-delete-all 'message-seq message))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (target . ((kind . "message")
                                         (message_id
                                          . "7348923749823749823")))))
                   "request-recall")))
        (should (equal (qq-message-recall session-key message)
                       "request-recall"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (target . ((kind . "message")
                       (message_id . "7348923749823749823"))))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))
        (qq-message--handle-event
         "message.recalled" (qq-message-test-recall))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-private-recall-sends-only-the-message-reference ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received" (qq-message-test-event))
    (let* ((session-key "private:10001")
           (message (car (qq-state-session-messages session-key)))
           sent-params)
      (dolist (key '(message-seq native-client-sequence native-random))
        (setq message (assq-delete-all key message)))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            '((account_id . "slot-a")
                              (target . ((kind . "message")
                                         (message_id
                                          . "7348923749823749823")))))
                   "request-recall")))
        (qq-message-recall session-key message)
        (should
         (equal
          (alist-get 'target sent-params)
          '((kind . "message")
            (message_id . "7348923749823749823"))))
        (should
         (equal (alist-get 'conversation sent-params)
                '((kind . "private") (peer_uin . "10001"))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-group-recall-uses-sequence-without-a-snowflake ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (wire-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105544"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (message
            (qq-message-normalize-snapshot
             wire-message "slot-a" (qq-account-get "slot-a") nil t))
           sent-params)
      (qq-message--merge-normalized message 'history)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (target . ((kind . "sequence")
                                 (sequence . "105544")))))
                   "request-sequence-recall")))
        (should (equal (qq-message-recall session-key message)
                       "request-sequence-recall"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (target . ((kind . "sequence")
                       (sequence . "105544"))))))
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages session-key))))))))

(ert-deftest qq-message-group-reply-uses-sequence-without-a-snowflake ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (wire-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105544"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (message
            (qq-message-normalize-snapshot
             wire-message "slot-a" (qq-account-get "slot-a") nil t))
           (target (qq-message-reply-target session-key message))
           (outbound
            (qq-message--prepare-outbound
             session-key
             (list
              `((type . "reply") (data . ((target . ,target))))
              '((type . "text") (data . ((text . "ack")))))
             "slot-a")))
      (should
       (equal target
              '((kind . "sequence") (sequence . "105544"))))
      (should (equal (plist-get outbound :reply-to) target))
      (should
       (equal
        (plist-get outbound :segments)
        '(((kind . "text") (payload . ((text . "ack"))))))))))

(ert-deftest qq-message-history-range-stays-exact-decimal ()
  (should
   (equal
    (qq-message--decimal-add-small "18446744073709551516" 99)
    "18446744073709551615"))
  (should
   (equal
    (qq-message--validate-history-range
     "18446744073709551516" "18446744073709551615")
    '("18446744073709551516" . "18446744073709551615")))
  (should (equal (qq-message--validate-history-range "0" "99")
                 '("0" . "99")))
  (dolist (range '((0 "1") ("00" "1") ("2" "1")
                   ("0" "100")
                   ("18446744073709551616" "18446744073709551616")))
    (should-error
     (qq-message--validate-history-range (car range) (cadr range))
     :type 'user-error)))

(ert-deftest qq-message-history-page-arithmetic-never-coerces-sequence ()
  (should
   (equal
    (qq-message-history-range-ending-at
     "18446744073709551615" 100)
    '("18446744073709551516" . "18446744073709551615")))
  (should
   (equal (qq-message-history-range-ending-at "12" 20)
          '("0" . "12")))
  (should
   (equal (qq-message-history-range-around "2" 20)
          '("0" . "19")))
  (should
   (equal
    (qq-message-history-range-around "9007199254740999" 20)
    '("9007199254740990" . "9007199254741009")))
  (should-error
   (qq-message-history-range-ending-at "99" 101)
   :type 'user-error))

(ert-deftest qq-message-live-frontier-uses-events-not-history ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :message-id "7348923749823749823" :sequence "100"))
    (should
     (equal (qq-message-live-frontier "private:10001")
            '((message_id . "7348923749823749823")
              (sequence . "100"))))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :message-id "7348923749823749822" :sequence "99"
      :client-sequence "9007199254741000"))
    (should
     (equal (alist-get 'sequence
                       (qq-message-live-frontier "private:10001"))
            "100"))
    (qq-message-reset-correlations)
    (should-not (qq-message-live-frontier "private:10001"))))

(ert-deftest qq-message-revoke-clears-essence-correlation ()
  (qq-message-test-with-state
    (puthash '(owner target) t qq-message--pending-essences)
    (qq-message-reset-correlations)
    (should (= (hash-table-count qq-message--pending-essences) 0))))

(ert-deftest qq-message-history-request-preserves-large-sequences ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     nil "18446744073709551516" "18446744073709551615"))
                   "request-history")))
        (should
         (equal
          (qq-message-get-history
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

(ert-deftest qq-message-forward-request-keeps-resource-scene-and-transient-body ()
  (qq-message-test-with-state
    (let (sent-method sent-params projected)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method
                         sent-params params)
                   (funcall
                    callback
                    `((account_id . "slot-a")
                      (messages
                       . ,(list
                           '((entry_id . "1")
                             (state . "live")
                             (sent_at . 1710000000)
                             (sender . ((kind . "anonymous")
                                        (name . "Alice")))
                             (origin . ((kind . "unknown")))
                             (segments . (((kind . "text")
                                           (payload
                                            . ((text . "inside")))))))))))
                   "request-forward")))
        (should
         (equal
          (qq-message-get-forward
           "resid-private" "private"
           (lambda (messages) (setq projected messages)))
          "request-forward"))
        (should (equal sent-method "message.get_forward"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (resource_id . "resid-private")
                  (scene . "private"))))
        (should (equal (alist-get 'entry_id (car projected)) "1"))
        ;; The result is viewer-local data, not a timeline/history merge.
        (should-not (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-message-group-history-window-anchors-at-server-frontier ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-message-test-group-history-window-result
                     nil nil "100" "81" "100" "100" :caught-up t))
                   "request-group-window")))
        (should
         (equal
          (qq-message-get-group-history-window
           "group:8209413637" nil
           (lambda (meta) (setq callback-meta meta))
           nil 20)
          "request-group-window"))
        (should (equal sent-method "message.get_group_history_window"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "group")
                             (group_uin . "8209413637")))
            (limit . 20))))
        (should (plist-get callback-meta :group-history-window-p))
        (should (plist-get callback-meta :history-at-latest-p))
        (should
         (equal (plist-get callback-meta :history-frontier-sequence)
                "100"))
        (should
         (equal (plist-get callback-meta :next-after-sequence) "100"))))))

(ert-deftest qq-message-group-history-window-advances-across-unsupported-slots ()
  (qq-message-test-with-state
    (let (sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall
                    callback
                    (qq-message-test-group-history-window-result
                     nil "100" "140" "101" "120" "120"
                     :unsupported-count 20))
                   "request-group-window")))
        (qq-message-get-group-history-window
         "group:8209413637" "100"
         (lambda (meta) (setq callback-meta meta))
         nil 20)
        (should (equal (alist-get 'after_sequence sent-params) "100"))
        (should-not (plist-get callback-meta :history-at-latest-p))
        (should
         (equal (plist-get callback-meta :next-after-sequence) "120"))
        (should
         (= (plist-get callback-meta :unsupported-message-count) 20))))))

(ert-deftest qq-message-private-history-bootstraps-and-keeps-cursor ()
  (qq-message-test-with-state
    (let (sent-method sent-params callback-meta)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    (qq-message-test-private-history-result
                     nil
                     '((timestamp . 1784700000) (random . 0))
                     '((timestamp . 1784699900) (random . 7))))
                   "request-private-history")))
        (should
         (equal
          (qq-message-get-private-history
           "private:10001" nil
           (lambda (meta) (setq callback-meta meta))
           nil 20)
          "request-private-history"))
        (should (equal sent-method "message.get_private_history"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (conversation . ((kind . "private") (peer_uin . "10001")))
            (limit . 20))))
        (should (plist-get callback-meta :private-history-p))
        (should-not (plist-get callback-meta :history-at-oldest-p))
        (should
         (equal
          (plist-get callback-meta :response-private-cursor)
          '((timestamp . 1784699900) (random . 7))))))))

(ert-deftest qq-message-private-history-rejects-stalled-empty-page ()
  (qq-message-test-with-state
    (let ((cursor '((timestamp . 1784699900) (random . 7)))
          failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback errback &optional _early)
                   (condition-case error-data
                       (funcall
                        callback
                        (qq-message-test-private-history-result
                         nil cursor cursor))
                     (error
                      (funcall errback nil (error-message-string error-data))))
                   "request-private-history")))
        (qq-message-get-private-history
         "private:10001" cursor nil
         (lambda (_body reason) (setq failure reason))
         20)
        (should (string-match-p "did not advance" failure))))))

(ert-deftest qq-message-history-merges-once-and-deduplicates-live-row ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (newer-event
            (qq-message-test-event
             :message-id "7348923749823749824"
             :sequence "101" :conversation conversation))
           (older
            (alist-get
             'message
             (qq-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :sent-at 1784699999
              :conversation conversation)))
           (newer (alist-get 'message newer-event))
           history-events callback-meta)
      (qq-message--handle-event "message.received" newer-event)
      (add-hook 'qq-state-change-hook
                (lambda (event)
                  (when (eq (plist-get event :type) 'history)
                    (push event history-events))))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list older newer) "100" "101"
                     :unsupported-count 2))
                   "request-history")))
        (qq-message-get-history
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

(ert-deftest qq-message-group-history-keeps-sequence-only-row-and-promotes-live-id ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (history-message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "105525"
              :random nil
              :conversation conversation)))
           (result
            (qq-message-test-group-history-window-result
             (list history-message)
             nil "105525" "105525" "105525" "105525"
             :caught-up t))
           (meta
            (qq-message--merge-history
             "group:8209413637"
             (qq-server-wire-domain-copy result)
             "slot-a"
             '(:group-history-window-p t)))
           (history-anchor
            (car (plist-get meta :batch-message-ids))))
      (should (= (plist-get meta :added-count) 1))
      (should (string-prefix-p "history:slot-a:group:8209413637:105525:"
                               history-anchor))
      (let ((messages (qq-state-session-messages "group:8209413637")))
        (should (= (length messages) 1))
        (should-not (alist-get 'server-id (car messages)))
        (should (equal (qq-state-message-anchor (car messages))
                       history-anchor)))

      ;; The live push may add random as well as the authoritative snowflake.
      ;; Group sequence is the native locator, so this promotes rather than
      ;; duplicating the already rendered history observation.
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event
        :message-id "2083983882109904220"
        :sequence "105525"
        :random 995353755
        :conversation conversation))
      (let ((messages (qq-state-session-messages "group:8209413637")))
        (should (= (length messages) 1))
        (should
         (equal (alist-get 'server-id (car messages))
                "2083983882109904220"))
        (should
         (equal (qq-state-message-anchor (car messages))
                "2083983882109904220"))))))

(ert-deftest qq-message-history-applies-earlier-sequence-recall ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.recalled"
     (qq-message-test-recall
      :target '((kind . "sequence") (sequence . "100"))))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :message-id nil
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message-get-history
         "group:8209413637" "100" "100")
        (should
         (qq-state-message-recalled-p
          (car (qq-state-session-messages "group:8209413637"))))
        (should (= (hash-table-count qq-message--pending-recalls) 0))))))

(ert-deftest qq-message-history-applies-earlier-sequence-reaction ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.reaction_changed"
     (qq-message-test-reaction
      :sequence "100" :operator-uin nil :count 6))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message-get-history
         "group:8209413637" "100" "100")
        (let* ((projected
                (car (qq-state-session-messages "group:8209413637")))
               (reaction (car (qq-state-message-reactions projected))))
          (should (= (alist-get 'count reaction) 6)))
        (should (= (hash-table-count
                    qq-message--pending-reactions)
                   0))))))

(ert-deftest qq-message-history-applies-earlier-native-essence ()
  (qq-message-test-with-state
    (qq-message--handle-event
     "message.essence_changed"
     (qq-message-test-essence
      :sequence "100" :random 4294967295 :sender-nickname nil))
    (let* ((message
            (alist-get
             'message
             (qq-message-test-event
              :sequence "100" :random 4294967295
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (result (qq-message-test-history-result
                    (list message) "100" "100")))
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-history")))
        (qq-message-get-history
         "group:8209413637" "100" "100")
        (let ((projected
               (car (qq-state-session-messages "group:8209413637"))))
          (should (eq (alist-get 'essence-p projected) t))
          (should (= (alist-get 'native-random projected) 4294967295)))
        (should (= (hash-table-count
                    qq-message--pending-essences)
                   0))))))

(ert-deftest qq-message-history-malformed-page-is-not-partially-merged ()
  (qq-message-test-with-state
    (let* ((conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice")))
           (valid
            (alist-get
             'message
             (qq-message-test-event
              :message-id "7348923749823749823"
              :sequence "100" :conversation conversation)))
           (invalid
            (alist-get
             'message
             (qq-message-test-event
              :message-id 7348923749823749824
              :sequence "101" :conversation conversation)))
           failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list valid invalid) "100" "101"))
                   "request-history")))
        (qq-message-get-history
         "group:8209413637" "100" "101" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "message_id" failure))
        (should-not (qq-state-sessions))))))

(ert-deftest qq-message-history-rejects-cross-conversation-page ()
  (qq-message-test-with-state
    (let* ((group-message
            (alist-get
             'message
             (qq-message-test-event
              :sequence "100"
              :conversation
              '((kind . "group")
                (group_uin . "8209413637")
                (group_name . "Protocol Lab")
                (sender_card . "Alice")))))
           (initial-order qq-state--message-order-counter)
           failure)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    (qq-message-test-history-result
                     (list group-message) "100" "100"))
                   "request-history")))
        (qq-message-get-history
         "private:10001" "100" "100" nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "requested conversation" failure))
        (should-not (qq-state-sessions))
        (should (= qq-state--message-order-counter initial-order))))))

(ert-deftest qq-message-history-survives-ui-account-selection ()
  (qq-message-test-with-state
    (let (response-callback failure delivered)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-history")))
        (qq-message-get-history
         "group:8209413637" "100" "100"
         (lambda (_metadata) (setq delivered t))
         (lambda (_body reason) (setq failure reason)))
        (qq-account--upsert-account
         (qq-message-test-account
          "slot-b" "10003" "u_other_self")
         'changed)
        (qq-account-select "slot-b")
        (funcall
         response-callback
         (qq-message-test-history-result nil "100" "100"))
        (should delivered)
        (should-not failure)
        (should-not (qq-state-sessions))))))

(ert-deftest qq-message-history-recovers-lost-self-event ()
  (qq-message-test-with-state
    (let ((now (floor (float-time))) local-id)
      (cl-letf (((symbol-function 'qq-server-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-server-capabilities)
                 (lambda () qq-message-test-capabilities))
                ((symbol-function 'qq-server-send)
                 (lambda (method _params callback _errback &optional _early)
                   (pcase method
                     ("message.send"
                      (funcall callback
                               `((account_id . "slot-a")
                                 (sent_at . ,now)
                                 (server_sequence . "8765432109")
                                 (client_sequence . "42001")
                                 (random . 123))))
                     ("message.get_history"
                      (let ((message
                             (alist-get
                              'message
                              (qq-message-test-event
                               :sent-at now
                               :sender '((uin . "10002") (uid . "u_self"))
                               :recipient '((uin . "10001") (uid . "u_peer"))
                               :sequence "8765432109"
                               :client-sequence "42001"
                               :random 123))))
                        (funcall
                         callback
                         (qq-message-test-history-result
                          (list message) "8765432109" "8765432109")))))
                   (concat "request-" method))))
        (qq-message-send
         "private:10001"
         '(((type . "text") (data . ((text . "hello"))))))
        (setq local-id
              (alist-get 'local-id
                         (car (qq-state-session-messages "private:10001"))))
        (qq-message-get-history
         "private:10001" "8765432109" "8765432109")
        (let* ((messages (qq-state-session-messages "private:10001"))
               (message (car messages)))
          (should (= (length messages) 1))
          (should (equal (alist-get 'local-id message) local-id))
          (should (equal (alist-get 'server-id message)
                         "7348923749823749823"))
          (should (= (hash-table-count qq-message--pending-sends) 0)))))))

(ert-deftest qq-message-selection-change-preserves-account-partitions ()
  (qq-message-test-with-state
    (qq-message--handle-event
    "message.received" (qq-message-test-event))
    (should (qq-state-sessions))
    (qq-account--upsert-account
     (qq-message-test-account
     "slot-b" "10003" "u_other_self")
     'changed)
    (qq-message--handle-account-change 'changed "slot-b")
    (qq-account-select "slot-b")
    (should (qq-state-sessions))
    (qq-state-with-account "slot-b"
      (should-not (qq-state-sessions)))
    (qq-message--handle-event
     "message.received"
     (qq-message-test-event
      :account-id "slot-b"
      :recipient '((uin . "10003") (uid . "u_other_self"))
      :message-id "7348923749823749824"))
    (qq-state-with-account "slot-a"
      (should (= (length (qq-state-sessions)) 1)))
    (qq-state-with-account "slot-b"
      (should (= (length (qq-state-sessions)) 1)))))

(ert-deftest qq-message-same-slot-restart-preserves-state-and-pending ()
  (qq-message-test-with-state
    (let* ((session-key "group:8209413637")
           (conversation
            '((kind . "group")
              (group_uin . "8209413637")
              (group_name . "Protocol Lab")
              (sender_card . "Alice"))))
      (qq-message--handle-event
       "message.received"
       (qq-message-test-event :conversation conversation))
      (let* ((old-message (car (qq-state-session-messages session-key)))
             (pending
              (qq-state-insert-pending-message
               session-key
               '(((type . "text") (data . ((text . "in flight")))))))
             (local-id (alist-get 'local-id pending))
             sent-params delivered)
        (puthash '(old-runtime) t qq-message--pending-recalls)
        (puthash (qq-message--frontier-key "slot-a" session-key)
                 '((message_id . "7348923749823749823")
                   (sequence . "9007199254740999"))
                 qq-message--live-frontiers)
        (let ((stopped (qq-message-test-account)))
          (setf (alist-get 'phase stopped) "stopped")
          (qq-account--upsert-account stopped 'changed))
        (qq-message--handle-account-change 'changed "slot-a")
        (qq-account--upsert-account
         (qq-message-test-account) 'changed)
        (qq-message--handle-account-change 'changed "slot-a")
        (should (= (hash-table-count qq-message--pending-recalls) 1))
        (should (= (hash-table-count qq-message--live-frontiers) 1))
        (let ((retained
               (seq-find
                (lambda (message)
                  (equal (alist-get 'server-id message)
                         "7348923749823749823"))
                (qq-state-session-messages session-key)))
              (in-flight
               (seq-find
                (lambda (message)
                  (equal (alist-get 'local-id message) local-id))
                (qq-state-session-messages session-key))))
          (should retained)
          (should (eq (alist-get 'status in-flight) 'pending))
          (should-not (alist-get 'error in-flight)))
        (cl-letf (((symbol-function 'qq-server-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-server-capabilities)
                   (lambda () qq-message-test-capabilities))
                  ((symbol-function 'qq-server-send)
                   (lambda (_method params callback _errback &optional _early)
                     (setq sent-params params)
                     (funcall
                      callback
                      '((account_id . "slot-a")
                        (target . ((kind . "message")
                                   (message_id
                                    . "7348923749823749823")))))
                     "restart-recall")))
          (should
           (equal
            (qq-message-recall
             session-key old-message
             (lambda (receipt) (setq delivered receipt)))
            "restart-recall")))
        (should delivered)
        (should (equal (alist-get 'account_id sent-params) "slot-a"))
        (should
         (equal (alist-get 'target sent-params)
                '((kind . "message")
                  (message_id . "7348923749823749823"))))
        (should-not (assq 'generation sent-params))))))

(ert-deftest qq-message-account-ready-synchronizes-every-owner ()
  (qq-message-test-with-state
    (qq-message--handle-account-change 'ready nil)
    (should (equal (alist-get 'user_id (qq-state-self-info)) "10002"))
    (should (eq (qq-state-connection-status) 'ready))))

(provide 'qq-message-test)

;;; qq-message-test.el ends here
