;;; qq-state-test.el --- Tests for qq-state -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-state)

(defmacro qq-test-with-reset (&rest body)
  "Run BODY with a clean in-memory qq-state store."
  `(let ((qq-state-change-hook nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(defun qq-state-test--poke-recall-reference
    (message-id chat-type peer-uid)
  "Return a strict native poke recall reference for tests."
  `((message_id . ,message-id)
    (peer . ((chat_type . ,chat-type)
             (peer_uid . ,peer-uid)
             (guild_id . "")))
    (valid_before . 4102444800)))

(ert-deftest qq-state-session-key-normalizes-type-and-id ()
  (qq-test-with-reset
   (should (equal (qq-state-session-key 'private 12345)
                  "private:12345"))
   (should (equal (qq-state-session-key 'group "67890")
                  "group:67890"))
   (should (equal (qq-state-session-key 'dataline "device-1")
                  "dataline:device-1"))
   (should (equal (qq-state-session-key 'service "u_mail")
                  "service:u_mail"))
   (should-error (qq-state-session-key 'unknown "target"))))

(ert-deftest qq-state-session-sendable-p-is-an-explicit-capability ()
  (should (qq-state-session-sendable-p "private:10001"))
  (should (qq-state-session-sendable-p "group:20001"))
  (should (qq-state-session-sendable-p "dataline:device-1"))
  (should-not (qq-state-session-sendable-p "service:u_mail"))
  (should-not (qq-state-session-sendable-p "unknown:target"))
  (should-not (qq-state-session-sendable-p nil)))

(ert-deftest qq-state-apply-input-status-tracks-and-clears ()
  "NapCat input_status maps to telega-like session actions."
  (qq-test-with-reset
   (let ((qq-input-status-ttl 30)
         seen)
     (cl-letf (((symbol-function 'run-at-time)
                (lambda (&rest _)
                  'fake-timer)))
       (add-hook 'qq-state-change-hook
                 (lambda (event) (push (plist-get event :type) seen)))
       (qq-state-apply-input-status
        '((notice_type . "notify")
          (sub_type . "input_status")
          (user_id . 10001)
          (event_type . 1)
          (status_text . "对方正在输入...")))
       (should (equal (qq-state-action-text "private:10001")
                      "对方正在输入..."))
       (let ((actions (qq-state-session-actions "private:10001")))
         (should (equal (car (car actions)) "10001"))
         (should (eq (alist-get 'type (cdr (car actions))) 'typing)))
       (should (memq 'action seen))
       ;; event_type 0 / empty text = chatActionCancel
       (qq-state-apply-input-status
        '((notice_type . "notify")
          (sub_type . "input_status")
          (user_id . "10001")
          (event_type . 0)
          (status_text . "")))
       (should (null (qq-state-action-text "private:10001")))
       (should (null (qq-state-session-actions "private:10001")))))))

(ert-deftest qq-state-apply-input-status-defaults-when-status-text-missing ()
  "Kernel often omits statusText; still treat as typing when not cancel."
  (qq-test-with-reset
   (cl-letf (((symbol-function 'run-at-time)
              (lambda (&rest _) 'fake-timer)))
     ;; no status_text, no event_type
     (qq-state-apply-input-status
      '((notice_type . "notify")
        (sub_type . "input_status")
        (user_id . 20002)))
     (should (equal (qq-state-action-text "private:20002")
                    "对方正在输入..."))
     ;; event_type present, status_text null-like
     (qq-state-apply-input-status
      '((notice_type . "notify")
        (sub_type . "input_status")
        (user_id . "20003")
        (event_type . 1)
        (status_text . nil)))
     (should (equal (qq-state-action-text "private:20003")
                    "对方正在输入...")))))

(ert-deftest qq-state-apply-poke-notice-uses-private-contact-names ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (qq-state-apply-friends
    '(((user_id . "10001")
       (remark . "Alice")
       (nickname . "Alice Nick"))))
   (let ((message
          (qq-state-apply-poke-notice
           `((post_type . "notice")
             (notice_type . "notify")
             (sub_type . "poke")
             (user_id . "10001")
             (sender_id . "10001")
             (target_id . "90001")
             (recall_reference
              . ,(qq-state-test--poke-recall-reference
                  "9007199254741007701" 1 "u_private_alice"))))))
     (should (equal (alist-get 'session-key message) "private:10001"))
     (should (equal (alist-get 'sender-name message) "Alice"))
     (should (equal (alist-get 'preview message) "戳了戳 我"))
     (should-not (string-match-p "unknown" (alist-get 'preview message))))))

(ert-deftest qq-state-apply-poke-notice-uses-id-fallback-in-group ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (let ((message
          (qq-state-apply-poke-notice
           `((post_type . "notice")
             (notice_type . "notify")
             (sub_type . "poke")
             (group_id . "20001")
             (user_id . "10002")
             (target_id . "90001")
             (recall_reference
              . ,(qq-state-test--poke-recall-reference
                  "9007199254741007702" 2 "20001"))))))
     (should (equal (alist-get 'session-key message) "group:20001"))
     (should (equal (alist-get 'sender-name message) "10002"))
     (should (equal (alist-get 'preview message) "戳了戳 我"))
     (should-not (string-match-p "unknown" (alist-get 'preview message))))))

(ert-deftest qq-state-authoritative-poke-requires-native-recall-reference ()
  (qq-test-with-reset
   (should-error
    (qq-state-apply-poke-notice
     '((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001"))))
   (should-error
    (qq-state-apply-poke-notice
     '((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001")
       (message_id . "9007199254741007702"))))
   (should-error
    (qq-state-apply-poke-notice
     '((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001")
       (recall_reference
        . ((message_id . 9007199254741007702)
            (peer . ((chat_type . 2)
                     (peer_uid . "20001")
                     (guild_id . "")))
            (valid_before . 4102444800))))))
   (should-error
    (qq-state-apply-poke-notice
     '((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001")
       (emacs_local_p . :false))))
   (should-error
    (qq-state-apply-poke-notice
     `((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001")
       (emacs_local_p . t)
       (recall_reference
        . ,(qq-state-test--poke-recall-reference
            "9007199254741007702" 2 "20001")))))
   (should-error
    (qq-state-apply-poke-notice
     `((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (group_id . "20001")
       (user_id . "10002")
       (target_id . "90001")
       (message_id . "9007199254741007799")
       (recall_reference
        . ,(qq-state-test--poke-recall-reference
            "9007199254741007702" 2 "20001")))))
   (dolist (reference
            (list
             (qq-state-test--poke-recall-reference
              "9007199254741007702" 1 "u_private")
             (qq-state-test--poke-recall-reference
              "9007199254741007702" 2 "99999")))
     (should-error
      (qq-state-apply-poke-notice
       `((post_type . "notice")
         (notice_type . "notify")
         (sub_type . "poke")
         (group_id . "20001")
         (user_id . "10002")
         (target_id . "90001")
         (recall_reference . ,reference)))))
   (should-error
    (qq-state-apply-poke-notice
     `((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (user_id . "10002")
       (sender_id . "10002")
       (target_id . "90001")
       (recall_reference
        . ,(qq-state-test--poke-recall-reference
            "9007199254741007702" 2 "20001")))))
   (qq-state-upsert-session
    "private:10002"
    '((type . private)
      (target-id . "10002")
      (peer-uid . "u_expected"))
    nil)
   (should-error
    (qq-state-apply-poke-notice
     `((post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (user_id . "10002")
       (sender_id . "10002")
       (target_id . "90001")
       (recall_reference
        . ,(qq-state-test--poke-recall-reference
            "9007199254741007702" 1 "u_wrong")))))))

(ert-deftest qq-state-apply-gray-tip-notice-creates-group-system-row ()
  (qq-test-with-reset
   (let (events)
     (add-hook 'qq-state-change-hook (lambda (event) (push event events)))
     (let ((message
            (qq-state-apply-gray-tip-notice
             '((post_type . "notice")
               (notice_type . "notify")
               (sub_type . "gray_tip")
               (group_id . 987654321)
               (user_id . 0)
               (message_id . "9007199254750003456")
               (busi_id . "19366")
               (content . "{\"align\":\"center\",\"items\":[{\"txt\":\"新进群账号疑似来自非大陆地区，请谨慎核实对方身份。\",\"type\":\"nor\"},{\"txt\":\"查看异常>\",\"type\":\"url\"}]}\n")
               (raw_info . ((msgTime . "1710000000")))))))
       (should (qq-state-gray-tip-message-p message))
       (should (equal (alist-get 'session-key message) "group:987654321"))
       (should (equal (alist-get 'server-id message)
                      "9007199254750003456"))
       (should (equal (alist-get 'preview message)
                      "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"))
       (should (equal (qq-state-message-preview message)
                      "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"))
       (should (equal
                (alist-get 'last-message-preview
                           (qq-state-session "group:987654321"))
                "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"))
       (should (equal (plist-get (car events) :message-anchor)
                      "9007199254750003456"))))))

(ert-deftest qq-state-merge-history-normalizes-poke-notice ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (qq-state-merge-history
    "group:20001"
    `(((time . 1710000001)
       (post_type . "notice")
       (notice_type . "notify")
       (sub_type . "poke")
       (target_id . "90001")
       (user_id . "10002")
       (group_id . "20001")
       (recall_reference
        . ,(qq-state-test--poke-recall-reference
            "9007199254741007703" 2 "20001")))))
   (let ((message (car (qq-state-session-messages "group:20001"))))
     (should (equal (alist-get 'server-id message)
                    "9007199254741007703"))
     (should-not (alist-get 'local-id message))
     (should (equal (alist-get 'preview message) "戳了戳 我"))
     (should (equal (alist-get 'raw-event message)
                    `((time . 1710000001)
                      (post_type . "notice")
                      (notice_type . "notify")
                      (sub_type . "poke")
                      (target_id . "90001")
                      (user_id . "10002")
                      (group_id . "20001")
                      (recall_reference
                       . ,(qq-state-test--poke-recall-reference
                           "9007199254741007703" 2 "20001"))))))))

(ert-deftest qq-state-poke-with-server-id-is-recallable ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (let ((message
          (qq-state-apply-poke-notice
           `((time . 1710000001)
             (post_type . "notice")
             (notice_type . "notify")
             (sub_type . "poke")
             (group_id . "20001")
             (user_id . "90001")
             (target_id . "10002")
             (recall_reference
              . ,(qq-state-test--poke-recall-reference
                  "9007199254741007777" 2 "20001"))))))
     (should (equal (alist-get 'id message) "9007199254741007777"))
     (should (equal (alist-get 'server-id message) "9007199254741007777"))
     (should (qq-state-poke-message-p message))
     (should-not (alist-get 'local-id message))
     (should (equal
              (qq-state-poke-recall-reference message)
              (qq-state-test--poke-recall-reference
               "9007199254741007777" 2 "20001")))
     (should (equal (alist-get 'preview message) "戳了戳 10002")))))

(ert-deftest qq-state-poke-preserves-native-decoration-data ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (let* ((message
           (qq-state-apply-poke-notice
            `((time . 1710000001)
              (post_type . "notice")
              (notice_type . "notify")
              (sub_type . "poke")
              (group_id . "20001")
              (user_id . "10002")
              (target_id . "90001")
              (recall_reference
               . ,(qq-state-test--poke-recall-reference
                   "9007199254741007778" 2 "20001"))
              (raw_info .
               (((type . "qq") (uid . "actor"))
                ((type . "img")
                 (src . "https://example.com/poke.png"))
                ((type . "nor") (txt . "喷了喷"))
                ((type . "qq") (uid . "target"))
                ((type . "nor") (txt . "的加分喷雾，分数++")))))))
          (data (qq-state-poke-message-data message)))
     (should (equal (alist-get 'type (car (alist-get 'segments message)))
                    "poke"))
     (should (equal (alist-get 'image-url data)
                    "https://example.com/poke.png"))
     (should (equal (alist-get 'action data) "喷了喷"))
     (should (equal (alist-get 'detail data) "的加分喷雾，分数++")))))

(ert-deftest qq-state-poke-promotes-local-row-when-notice-arrives-later ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (let (events local-id)
     (add-hook 'qq-state-change-hook (lambda (event) (push event events)))
     (setq local-id
           (alist-get
            'local-id
            (qq-state-apply-poke-notice
             '((time . 1710000000)
               (emacs_local_p . t)
               (post_type . "notice")
               (notice_type . "notify")
               (sub_type . "poke")
               (group_id . "20001")
               (user_id . "90001")
               (target_id . "10002")))))
     (qq-state-apply-poke-notice
      `((time . 1710000001)
        (post_type . "notice")
        (notice_type . "notify")
        (sub_type . "poke")
        (group_id . "20001")
        (user_id . "90001")
        (target_id . "10002")
        (recall_reference
         . ,(qq-state-test--poke-recall-reference
             "9007199254741007780" 2 "20001"))))
     (let ((messages (qq-state-session-messages "group:20001")))
       (should (= (length messages) 1))
       (should (equal (alist-get 'server-id (car messages))
                      "9007199254741007780"))
       (should-not (alist-get 'local-id (car messages))))
     (should (equal (plist-get (car events) :previous-anchor) local-id)))))

(ert-deftest qq-state-poke-ignores-late-local-callback-after-real-notice ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "我")))
   (qq-state-apply-poke-notice
    `((time . 1710000000)
      (post_type . "notice")
      (notice_type . "notify")
      (sub_type . "poke")
      (group_id . "20001")
      (user_id . "90001")
      (target_id . "10002")
      (recall_reference
       . ,(qq-state-test--poke-recall-reference
           "9007199254741007781" 2 "20001"))
      (raw_info . (((type . "nor") (txt . "喷了喷"))))))
   (qq-state-apply-poke-notice
    '((time . 1710000001)
      (emacs_local_p . t)
      (post_type . "notice")
      (notice_type . "notify")
      (sub_type . "poke")
      (group_id . "20001")
      (user_id . "90001")
      (target_id . "10002")))
   (let* ((messages (qq-state-session-messages "group:20001"))
          (message (car messages)))
     (should (= (length messages) 1))
     (should (equal (alist-get 'server-id message)
                    "9007199254741007781"))
     (should-not (alist-get 'local-id message))
     (should (equal (alist-get 'action (qq-state-poke-message-data message))
                    "喷了喷")))))

(ert-deftest qq-state-apply-recent-contacts-creates-session-summary ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 1)
       (peerUin . "10001")
       (remark . "Alice")
       (peerName . "Alice Nick")
       (msgTime . "1710000000")
       (msgId . "42")
       (lastestMsg
        (time . 1710000000)
        (message_type . "private")
        (user_id . 10001)
        (raw_message . "hello from napcat")
        (message . (((type . "text")
                     (data . ((text . "hello from napcat"))))))))))
   (let ((session (qq-state-session "private:10001")))
     (should (equal (alist-get 'title session) "Alice"))
     (should (equal (alist-get 'last-message-id session) "42"))
     (should (equal (alist-get 'last-message-preview session)
                    "hello from napcat")))))

(ert-deftest qq-state-apply-recent-contacts-uses-kernel-unread-count ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (unreadCount . 17))))
   (should (= 17 (alist-get 'unread-count
                            (qq-state-session "group:20001"))))))

(ert-deftest qq-state-apply-recent-contacts-calibrates-native-mentions ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (unreadCount . 5)
       (firstUnreadAtMeSeq . "30003")
       (firstUnreadAtAllSeq . "30004"))))
   (let ((session (qq-state-session "group:20001")))
     (should (equal "30003"
                    (alist-get 'unread-at-me-message-seq session)))
     (should (equal "30004"
                    (alist-get 'unread-at-all-message-seq session))))))

(ert-deftest qq-state-apply-recent-contacts-preserves-message-disturb-state ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Muted Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (unreadCount . 17)
       (isMsgDisturb . t)
       (messageNotifyMode . "receive"))))
   (let ((session (qq-state-session "group:20001")))
     (should (eq t (alist-get 'muted-p session)))
     (should (eq 'receive (alist-get 'message-notify-mode session))))
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Muted Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (unreadCount . 0)
       (isMsgDisturb . :false))))
   (should-not (alist-get 'muted-p
                          (qq-state-session "group:20001")))))

(ert-deftest qq-state-apply-session-read-state-keeps-snowflake-string ()
  (qq-test-with-reset
   (qq-state-upsert-session "group:20001" nil nil)
   (qq-state-apply-session-read-state
    "group:20001"
    '((unread_count . 5)
      (first_unread
       . ((sequence . "30001")
          (message_id . "9007199254742007089")))
      (latest
       . ((sequence . "30005")
          (message_id . "9007199254742007094")))
      (mentions
       (at_me . ((sequence . "30003")
                 (message_id . "9007199254742007091")))
       (at_all . ((sequence . "30004")
                  (message_id . "9007199254742007092"))))))
   (let ((session (qq-state-session "group:20001")))
     (should (= 5 (alist-get 'unread-count session)))
     (should (equal "30001" (alist-get 'first-unread-message-seq session)))
     (should (equal "9007199254742007089"
                    (alist-get 'first-unread-message-id session)))
     (should (eq t (alist-get 'read-position-available session)))
     (should (equal "9007199254742007094"
                    (alist-get 'read-latest-message-id session)))
     (should (equal "9007199254742007091"
                    (alist-get 'unread-at-me-message-id session)))
     (should (equal "30004"
                    (alist-get 'unread-at-all-message-seq session))))))

(ert-deftest qq-state-read-position-keeps-unresolved-sequence-unavailable ()
  (qq-test-with-reset
   (qq-state-upsert-session "group:20001" nil nil)
   (qq-state-apply-session-read-state
    "group:20001"
    '((unread_count . 5)
      (first_unread
       . ((sequence . "30001")
          (message_id)))))
   (let ((session (qq-state-session "group:20001")))
     (should-not (alist-get 'first-unread-message-id session))
     (should-not (alist-get 'first-unread-message-seq session))
     (should-not (alist-get 'read-position-available session)))))

(ert-deftest qq-state-rejects-numeric-message-id-from-wire ()
  (qq-test-with-reset
   (should-error
    (qq-state-merge-live-message
     '((post_type . "message")
       (message_type . "group")
       (message_id . 9007199254742007089)
       (group_id . "20001")
       (user_id . "10001")
       (time . 1710000001)
       (sender . ((user_id . "10001") (nickname . "Alice")))
       (message . (((type . "text") (data . ((text . "hello")))))))))))

(ert-deftest qq-state-apply-recent-contacts-creates-service-session ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 103)
       (peerUid . "u_mail")
       (peerUin . "90002")
       (remark . "")
       (peerName . "QQ邮箱提醒")
       (msgTime . "1710000100")
       (msgId . "9007199254747004845")
       (lastestMsg
        (time . 1710000100)
        (message_type . "private")
        (chat_type . 103)
        (peer_uid . "u_mail")
        (peer_uin . "90002")
        (user_id . 90002)
        (sender . ((user_id . 90002)
                   (nickname . "QQ邮箱提醒")))
        (raw_message . "[CQ:mail]")
        (message . (((type . "mail")
                     (data . ((sender . "Henrik Lissner")
                              (subject . "Re: Doom Emacs")
                              (prompt . "Henrik Lissner: Re: Doom Emacs"))))))))))
   (let ((session (qq-state-session "service:u_mail")))
     (should (equal (alist-get 'title session) "QQ邮箱提醒"))
     (should (equal (alist-get 'type session) 'service))
     (should (equal (alist-get 'chat-type session) "103"))
     (should (equal (alist-get 'peer-uid session) "u_mail"))
     (should (equal (alist-get 'last-message-preview session)
                    "Henrik Lissner: Re: Doom Emacs")))))

(ert-deftest qq-state-apply-recent-contacts-keeps-dataline-sessions-distinct ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 8)
       (peerUid . "device-1")
       (peerUin . "0")
       (remark . "")
       (peerName . "")
       (msgTime . "1710000000")
       (msgId . "42")
       (lastestMsg
        (time . 1710000000)
        (message_type . "private")
        (chat_type . 8)
        (peer_uid . "device-1")
        (peer_uin . "0")
        (user_id . 0)
        (sender . ((user_id . 0)
                   (nickname . "我的手机")))
        (raw_message . "hello from phone")
        (message . (((type . "text")
                     (data . ((text . "hello from phone"))))))))
      ((chatType . 8)
       (peerUid . "device-2")
       (peerUin . "0")
       (remark . "")
       (peerName . "")
       (msgTime . "1710000001")
       (msgId . "43")
       (lastestMsg
        (time . 1710000001)
        (message_type . "private")
        (chat_type . 8)
        (peer_uid . "device-2")
        (peer_uin . "0")
        (user_id . 0)
        (sender . ((user_id . 0)
                   (nickname . "我的手机")))
        (raw_message . "second device")
        (message . (((type . "text")
                     (data . ((text . "second device"))))))))))
   (let ((session-1 (qq-state-session "dataline:device-1"))
         (session-2 (qq-state-session "dataline:device-2")))
     (should (equal (alist-get 'title session-1) "我的手机"))
     (should (equal (alist-get 'title session-2) "我的手机"))
     (should (equal (alist-get 'peer-uid session-1) "device-1"))
     (should (equal (alist-get 'peer-uid session-2) "device-2"))
     (should (equal (alist-get 'last-message-preview session-1) "hello from phone"))
     (should (equal (alist-get 'last-message-preview session-2) "second device")))))

(ert-deftest qq-state-send-pending-message-transitions-to-sent ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-upsert-session "private:10001" '((title . "Alice")) nil)
   (let* ((pending (qq-state-insert-pending-text-message "private:10001" "ping"))
          (local-id (alist-get 'local-id pending))
          ;; NapCat hard-cut: message_id is an NT snowflake string.
          (snowflake "9007199254741004645"))
     (qq-state-mark-pending-message-sent "private:10001" local-id snowflake)
     (let ((message (car (qq-state-session-messages "private:10001"))))
       (should (equal (alist-get 'server-id message) snowflake))
       (should (equal (alist-get 'id message) snowflake))
       (should (eq (alist-get 'status message) 'sent))
       (should (equal (alist-get 'raw-message message) "ping"))
       ;; Anchor prefers server snowflake after send.
       (should (equal (or (alist-get 'server-id message)
                          (alist-get 'local-id message))
                      snowflake))))))

(ert-deftest qq-state-live-message-leaves-unread-to-kernel-and-supports-recall ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (message_id . "9007199254741004123")
      (user_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 10001)
                 (nickname . "Alice")))
      (raw_message . "hello")
      (message . (((type . "text")
                   (data . ((text . "hello"))))))))
   (let ((session (qq-state-session "private:10001")))
     (should (= (alist-get 'unread-count session) 0)))
   (qq-state-apply-recall "9007199254741004123")
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (qq-state-message-recalled-p message))
     (should (equal (alist-get 'preview message) "[message recalled]")))))

(ert-deftest qq-state-live-message-classifies-only-native-self-and-all-mentions ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001) (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "group")
      (group_id . 20001)
      (message_id . "9007199254741004991")
      (user_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 10001) (nickname . "Alice")))
      (message . (((type . "at") (data . ((qq . "12345"))))
                  ((type . "at") (data . ((qq . "90001"))))
                  ((type . "at") (data . ((qq . "all"))))))))
   (let* ((message (car (qq-state-session-messages "group:20001")))
          (session (qq-state-session "group:20001")))
     (should (equal '(at-me at-all)
                    (qq-state-message-mention-kinds message)))
     (should (qq-state-message-mentions-self-p message))
     (should (qq-state-message-mentions-all-p message))
     ;; Mention classification belongs to the message.  Session-level unread
     ;; anchors come only from the authoritative kernel read-state event.
     (should-not (alist-get 'unread-at-me-message-id session))
     (should-not (alist-get 'unread-at-all-message-id session))
     (should (= 0 (alist-get 'unread-count session))))))

(ert-deftest qq-state-live-message-does-not-change-authoritative-unread-count ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001) (nickname . "Me")))
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (target-id . "10001") (unread-count . 5))
    nil)
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (message_id . "9007199254741004992")
      (user_id . 10001)
      (time . 1710000002)
      (sender . ((user_id . 10001) (nickname . "Alice")))
      (message . (((type . "text") (data . ((text . "next"))))))))
   (should (= 5 (alist-get 'unread-count
                           (qq-state-session "private:10001"))))))

(ert-deftest qq-state-apply-recall-keeps-stub-in-store ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message_sent")
      (message_type . "private")
      (message_id . "9007199254741005555")
      (user_id . 90001)
      (target_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 90001)
                 (nickname . "Me")))
      (raw_message . "will recall")
      (message . (((type . "text")
                   (data . ((text . "will recall"))))))))
   (qq-state-apply-recall "9007199254741005555")
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (qq-state-message-recalled-p message))
     (should (equal (alist-get 'server-id message) "9007199254741005555")))))

(ert-deftest qq-state-napcat-recalled-flag-stores-stub ()
  "Protocol `recalled' stores a stub (display policy is chat's job)."
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (message_id . "9007199254741006666")
      (user_id . 10001)
      (time . 1710000001)
      (recalled . t)
      (recall_time . "1710000099")
      (sender . ((user_id . 10001)
                 (nickname . "Alice")))
      (raw_message . "")
      (message . ())))
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (qq-state-message-recalled-p message))
     (should (equal (alist-get 'preview message) "[message recalled]")))))

(ert-deftest qq-state-history-merge-preserves-recalled-status ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001) (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message_sent")
      (message_type . "private")
      (message_id . "9007199254741005555")
      (user_id . 90001)
      (target_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 90001) (nickname . "Me")))
      (raw_message . "x")
      (message . (((type . "text") (data . ((text . "x"))))))))
   (qq-state-apply-recall "9007199254741005555")
   (qq-state-merge-history
    "private:10001"
    (list
     '((post_type . "message_sent")
       (message_type . "private")
       (message_id . "9007199254741005555")
       (user_id . 90001)
       (target_id . 10001)
       (time . 1710000001)
       (sender . ((user_id . 90001) (nickname . "Me")))
       (raw_message . "")
       (message . ()))))
   (should (qq-state-message-recalled-p
            (car (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-state-live-dataline-message-routes-by-peer-uid ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 8)
      (peer_uid . "device-1")
      (peer_uin . "0")
      (peer_name . "我的手机")
      (message_id . "123")
      (user_id . 0)
      (time . 1710000001)
      (sender . ((user_id . 0)
                 (nickname . "我的手机")))
      (raw_message . "hello")
      (message . (((type . "text")
                   (data . ((text . "hello"))))))))
   (let* ((session (qq-state-session "dataline:device-1"))
          (message (car (qq-state-session-messages "dataline:device-1"))))
     (should (equal (alist-get 'title session) "我的手机"))
     (should (equal (alist-get 'sender-name message) "我的手机"))
     (should-not (alist-get 'self-p message))
     (should (= (alist-get 'unread-count session) 0)))))

(ert-deftest qq-state-live-service-message-routes-by-peer-uid ()
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 103)
      (peer_uid . "u_mail")
      (peer_uin . "90002")
      (peer_name . "QQ邮箱提醒")
      (message_id . "9007199254747004845")
      (user_id . 90002)
      (time . 1710000100)
      (sender . ((user_id . 90002)
                 (nickname . "QQ邮箱提醒")))
      (raw_message . "[CQ:mail]")
      (message . (((type . "mail")
                   (data . ((sender . "Henrik Lissner")
                            (subject . "Re: Doom Emacs")
                            (prompt . "Henrik Lissner: Re: Doom Emacs"))))))))
   (let* ((session (qq-state-session "service:u_mail"))
          (message (car (qq-state-session-messages "service:u_mail"))))
     (should (equal (alist-get 'title session) "QQ邮箱提醒"))
     (should (equal (alist-get 'preview message)
                    "Henrik Lissner: Re: Doom Emacs"))
     (should (= (alist-get 'unread-count session) 0)))))

(ert-deftest qq-state-mail-segment-preview-prefers-protocol-prompt ()
  (should
   (equal
    (qq-state-message-preview-from-segments
     '(((type . "mail")
        (data . ((sender . "Henrik Lissner")
                 (subject . "Re: Doom Emacs")
                 (prompt . "Henrik Lissner: Re: Doom Emacs"))))))
    "Henrik Lissner: Re: Doom Emacs")))

(ert-deftest qq-state-card-segment-preview-prefers-protocol-prompt ()
  (should
   (equal
    (qq-state-message-preview-from-segments
     '(((type . "card")
        (data . ((kind . "share")
                 (title . "Article")
                 (prompt . "[分享]Article"))))))
    "[分享]Article")))

(ert-deftest qq-state-live-message-ignores-empty-group-card-when-choosing-sender-name ()
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "group")
      (message_id . "123")
      (group_id . 20001)
      (user_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 10001)
                 (card . "")
                 (nickname . "Alice")))
      (raw_message . "hello")
      (message . (((type . "text")
                   (data . ((text . "hello"))))))))
   (let ((message (car (qq-state-session-messages "group:20001"))))
     (should (equal (alist-get 'sender-name message) "Alice"))
     (should-not (alist-get 'sender-secondary-name message)))))

(ert-deftest qq-state-group-message-keeps-card-and-nickname-for-display ()
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "group")
      (message_id . "123")
      (group_id . 20001)
      (user_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 10001)
                 (card . "Alice Card")
                 (nickname . "Alice Nick")))
      (raw_message . "hello")
      (message . (((type . "text")
                   (data . ((text . "hello"))))))))
   (let ((message (car (qq-state-session-messages "group:20001"))))
     (should (equal (alist-get 'sender-name message) "Alice Card"))
     (should (equal (alist-get 'sender-secondary-name message) "Alice Nick"))
     (should (equal (alist-get 'sender-card message) "Alice Card"))
     (should (equal (alist-get 'sender-nickname message) "Alice Nick")))))

(ert-deftest qq-state-private-message-prefers-remark-and-keeps-nickname-trail ()
  (qq-test-with-reset
   (qq-state-apply-friends
    '(((user_id . 10001)
       (remark . "Alice Remark")
       (nickname . "Alice Nick"))))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (message_id . "123")
      (user_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 10001)
                 (nickname . "Alice Nick")))
      (raw_message . "hello")
      (message . (((type . "text")
                   (data . ((text . "hello"))))))))
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (equal (alist-get 'sender-name message) "Alice Remark"))
     (should (equal (alist-get 'sender-secondary-name message) "Alice Nick"))
     (should (equal (alist-get 'sender-remark message) "Alice Remark"))
     (should (equal (alist-get 'sender-nickname message) "Alice Nick")))))

(ert-deftest qq-state-pending-message-ignores-empty-self-nickname ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "")))
   (let ((message (qq-state-insert-pending-text-message "private:10001" "ping")))
     (should (equal (alist-get 'sender-name message) "90001")))))

(ert-deftest qq-state-insert-pending-message-keeps-segments-and-preview ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-upsert-session "private:10001" '((title . "Alice")) nil)
   (let ((message (qq-state-insert-pending-message
                   "private:10001"
                   '(((type . "reply")
                      (data . ((id . "42"))))
                     ((type . "text")
                      (data . ((text . "hello")))))
                   "hello")))
     (should (equal (alist-get 'raw-message message) "hello"))
     ;; Reply segment is chrome, not body — preview is just the text.
     (should (equal (alist-get 'preview message) "hello"))
     (should (equal (alist-get 'segments message)
                    '(((type . "reply")
                       (data . ((id . "42"))))
                      ((type . "text")
                       (data . ((text . "hello"))))))))))

(ert-deftest qq-state-message-preview-prefers-segments-over-cq-raw ()
  (qq-test-with-reset
   (let ((message
          '((raw-message . "[CQ:image,file=foo.png,url=https://example.com/x]")
            (preview . "[image]")
            (segments . (((type . "image")
                          (data . ((file . "foo.png")
                                   (url . "https://example.com/x")))))))))
     (should (equal (qq-state-message-preview message) "[image]")))))

(ert-deftest qq-state-unsupported-segment-preview-is-visible-but-does-not-dump-raw ()
  (let ((preview
         (qq-state-message-preview-from-segments
          '(((type . "__unsupported")
             (data . ((native_keys . ["mysteryElement"])
                      (summary . "mystery  native\n element")
                      (raw . ((secret . "DO-NOT-RENDER"))))))))))
    (should (equal preview
                   "[unsupported QQ element: mystery native element]"))
    (should-not (string-match-p "DO-NOT-RENDER" preview))))

(ert-deftest qq-state-face-preview-uses-native-description ()
  (should (equal
           (qq-state-message-preview-from-segments
            '(((type . "face")
               (data . ((id . "478")
                        (raw . ((faceIndex . 478)
                                (faceText . "/对的对的")
                                (faceType . 3))))))))
           "/对的对的")))

(ert-deftest qq-state-message-preview-prefers-native-face-description-over-stale-preview ()
  (should (equal
           (qq-state-message-preview
            '((preview . "[face:478]")
              (segments . (((type . "face")
                            (data . ((id . "478")
                                     (raw . ((faceText . "/对的对的"))))))))))
           "/对的对的")))

(ert-deftest qq-state-message-preview-from-cq-strips-reply-and-face ()
  ;; Prefer human face name when qq-media face table is available.
  (should (equal
           (qq-state-message-preview-from-cq
            "[CQ:reply,id=9007199254746000940][CQ:face,id=178,raw={\"faceIndex\":178&#44;\"faceText\":null}]hi")
           (concat (if (fboundp 'qq-media-face-text-fallback)
                       (qq-media-face-text-fallback "178")
                     "[face:178]")
                   "hi")))
  (should (equal
           (qq-state-message-preview-from-cq
            "[CQ:image,file=43DFE.png,url=http://example.com/a.png]")
           "[image]")))

(ert-deftest qq-state-message-preview-collapses-multiline-content ()
  (should (equal
           "first second [markdown] line"
           (qq-state-message-preview-from-segments
            '(((type . "text") (data . ((text . "first\n second"))))
              ((type . "markdown") (data . ((content . "[markdown]\nline"))))))))
  (should (equal "plain text after"
                 (qq-state-message-preview
                  '((preview . "plain\n text\t after"))))))

(ert-deftest qq-state-message-preview-falls-back-to-cq-when-no-segments ()
  (qq-test-with-reset
   (let ((message
          '((raw-message . "[CQ:face,id=178]ok")
            (segments . nil)
            (preview . ""))))
     (should (equal (qq-state-message-preview message)
                    (concat (if (fboundp 'qq-media-face-text-fallback)
                                (qq-media-face-text-fallback "178")
                              "[face:178]")
                            "ok"))))))

(ert-deftest qq-state-apply-recent-contacts-does-not-surface-cq ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 1)
       (peerUin . "10002")
       (remark . "Bob")
       (msgTime . "1710000001")
       (msgId . "99")
       (lastestMsg
        (time . 1710000001)
        (message_type . "private")
        (user_id . 10002)
        (raw_message . "[CQ:image,file=x.png,url=http://e/x]")
        (message . (((type . "image")
                     (data . ((file . "x.png")
                              (url . "http://e/x"))))))))))
   (let ((session (qq-state-session "private:10002")))
     (should (equal (alist-get 'last-message-preview session) "[image]")))))

(ert-deftest qq-state-apply-recent-contacts-uses-server-preview-fallback ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUin . "20002")
       (peerName . "Group")
       (msgTime . "1710000002")
       (msgId . "9007199254741004991")
       (lastestMsg . nil)
       (lastMessagePreview . "群通知第一行\n  第二行"))))
   (should (equal
            "群通知第一行 第二行"
            (alist-get 'last-message-preview
                       (qq-state-session "group:20002"))))))

(ert-deftest qq-state-set-status-deduplicates-identical-events ()
  (qq-test-with-reset
   (let (events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (eq (plist-get event :type) 'status)
                   (push event events))))
     (qq-state-set-status '((online . t)))
     (qq-state-set-status '((online . t)))
     (qq-state-set-status '((online . nil)))
     (should (= 2 (length events))))))

(ert-deftest qq-state-message-events-include-mutation-metadata ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-upsert-session "private:10001" '((title . "Alice")) nil)
   (let (events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (memq (plist-get event :type) '(message history session))
                   (push event events))))
     (let* ((pending (qq-state-insert-pending-text-message "private:10001" "ping"))
            (local-id (alist-get 'local-id pending))
            (snowflake "9007199254741004645")
            (create-event (car events)))
       (should (eq (plist-get create-event :type) 'message))
       (should (eq (plist-get create-event :mutation) 'create))
       (should (eq (plist-get create-event :source) 'local))
       (should (equal (plist-get create-event :message-anchor) local-id))
       (setq events nil)
       (qq-state-mark-pending-message-sent "private:10001" local-id snowflake)
       (let ((sent-event (car events)))
         (should (eq (plist-get sent-event :mutation) 'update))
         (should (eq (plist-get sent-event :source) 'response))
         (should (equal (plist-get sent-event :previous-anchor) local-id))
         (should (equal (plist-get sent-event :message-anchor) snowflake)))
       (setq events nil)
       (qq-state-merge-live-message
        '((post_type . "message")
          (message_type . "private")
          (message_id . "9007199254741004999")
          (user_id . 10001)
          (time . 1710000099)
          (sender . ((user_id . 10001)
                     (nickname . "Alice")))
          (raw_message . "hello")
          (message . (((type . "text")
                       (data . ((text . "hello"))))))))
       (let ((live-event (car events)))
         (should (eq (plist-get live-event :mutation) 'create))
         (should (eq (plist-get live-event :source) 'event))
         (should (equal (plist-get live-event :message-anchor)
                        "9007199254741004999")))
       (setq events nil)
       (qq-state-apply-recall "9007199254741004999")
       (let ((recall-event (car events)))
         (should (eq (plist-get recall-event :mutation) 'update))
         (should (eq (plist-get recall-event :source) 'notice))
         (should (equal (plist-get recall-event :message-anchor)
                        "9007199254741004999")))
       (setq events nil)
       (qq-state-merge-history
        "private:10001"
        (list
         '((message_id . "9007199254741004001")
           (message_type . "private")
           (user_id . 10001)
           (time . 1710000001)
           (sender . ((user_id . 10001)
                      (nickname . "Alice")))
           (raw_message . "older")
           (message . (((type . "text")
                        (data . ((text . "older")))))))))
       (let ((history-event (car events)))
         (should (eq (plist-get history-event :type) 'history))
         (should (eq (plist-get history-event :mutation) 'history))
         (should (= (plist-get history-event :message-count) 1))
         (should (= (plist-get history-event :added-count) 1))
         (should (equal (plist-get history-event :batch-message-ids)
                        '("9007199254741004001")))
         (should (equal (plist-get history-event :batch-oldest-message-id)
                        "9007199254741004001"))
         (should (equal (plist-get history-event :batch-newest-message-id)
                        "9007199254741004001")))
       (setq events nil)
       (qq-state-clear-session-unread "private:10001")
       (let ((read-event (car events)))
         (should (eq (plist-get read-event :type) 'session))
         (should (eq (plist-get read-event :mutation) 'read)))
       (setq events nil)
       (qq-state-set-session-unread "private:10001" 4)
       (let ((restore-event (car events)))
         (should (eq (plist-get restore-event :mutation) 'read))
         (should (= 4 (alist-get 'unread-count
                                 (qq-state-session "private:10001")))))))))

(ert-deftest qq-state-merge-history-added-count-skips-duplicates ()
  (let ((qq-state-change-hook nil)
        meta)
    (qq-state-reset)
    (qq-state-upsert-session "private:10001" '((target-id . "10001")) nil)
    (qq-state-merge-history
     "private:10001"
     (list
      '((message_id . "100")
        (message_type . "private")
        (user_id . 10001)
        (time . 1710000001)
        (sender . ((user_id . 10001) (nickname . "A")))
        (raw_message . "a")
        (message . (((type . "text") (data . ((text . "a")))))))))
    (setq meta
          (qq-state-merge-history
           "private:10001"
           (list
            '((message_id . "100")
              (message_type . "private")
              (user_id . 10001)
              (time . 1710000001)
              (sender . ((user_id . 10001) (nickname . "A")))
              (raw_message . "a")
              (message . (((type . "text") (data . ((text . "a")))))))
            '((message_id . "90")
              (message_type . "private")
              (user_id . 10001)
              (time . 1710000000)
              (sender . ((user_id . 10001) (nickname . "A")))
              (raw_message . "older")
              (message . (((type . "text") (data . ((text . "older"))))))))))
    (should (= (plist-get meta :message-count) 2))
    (should (= (plist-get meta :added-count) 1))
    (should (equal (plist-get meta :oldest-message-id) "90"))
    (should (= (length (qq-state-session-messages "private:10001")) 2))))

(ert-deftest qq-state-history-batch-sorts-store-once ()
  (qq-test-with-reset
   (let ((sort-count 0)
         (original-sort (symbol-function 'qq-state--sort-messages)))
     (cl-letf (((symbol-function 'qq-state--sort-messages)
                (lambda (messages)
                  (cl-incf sort-count)
                  (funcall original-sort messages))))
       (qq-state-merge-history
        "group:20001"
        '(((message_id . "9007199254741004001")
           (message_type . "group") (group_id . "20001")
           (user_id . "10001") (time . 1710000001) (message . ()))
          ((message_id . "9007199254741004002")
           (message_type . "group") (group_id . "20001")
           (user_id . "10001") (time . 1710000002) (message . ()))
          ((message_id . "9007199254741004003")
           (message_type . "group") (group_id . "20001")
           (user_id . "10001") (time . 1710000003) (message . ()))))
       (should (= sort-count 1))))))

(ert-deftest qq-state-message-reactions-normalize-history-snapshot ()
  (qq-test-with-reset
   (qq-state-merge-history
    "group:20001"
    '(((message_id . "9007199254741004001")
       (message_type . "group")
       (group_id . "20001")
       (user_id . "10001")
       (time . 1710000001)
       (message . ())
       (emoji_likes_list
        . (((emoji_id . "178")
            (emoji_type . "1")
            (likes_cnt . "3")
            (is_clicked . "1")))))))
   (let* ((message (car (qq-state-session-messages "group:20001")))
          (reaction (car (qq-state-message-reactions message))))
     (should (equal (alist-get 'emoji-id reaction) "178"))
     (should (equal (alist-get 'emoji-type reaction) "1"))
     (should (= (alist-get 'count reaction) 3))
     (should (eq (alist-get 'chosen-p reaction) t)))))

(ert-deftest qq-state-emoji-like-notice-updates-one-message ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-merge-history
    "group:20001"
    '(((message_id . "9007199254741004001")
       (message_type . "group")
       (group_id . "20001")
       (user_id . "10001")
       (time . 1710000001)
       (message . ())
       (emoji_likes_list
        . (((emoji_id . "178")
            (emoji_type . "1")
            (likes_cnt . "2")
            (is_clicked . "0")))))))
   ;; Another member adds one; packet count is the aggregate.
   (qq-state-apply-emoji-like-notice
    '((notice_type . "group_msg_emoji_like")
      (group_id . "20001")
      (user_id . "10002")
      (message_id . "9007199254741004001")
      (is_add . t)
      (likes . (((emoji_id . "178") (emoji_type . "1") (count . 3))))))
   (let* ((message (car (qq-state-session-messages "group:20001")))
          (reaction (car (qq-state-message-reactions message))))
     (should (= (alist-get 'count reaction) 3))
     (should-not (alist-get 'chosen-p reaction)))
   ;; Our own add without a count is the optimistic one-step delta.
   (qq-state-apply-emoji-like-notice
    '((notice_type . "group_msg_emoji_like")
      (group_id . "20001")
      (user_id . "90001")
      (message_id . "9007199254741004001")
      (is_add . t)
      (likes . (((emoji_id . "178"))))))
   (let* ((message (car (qq-state-session-messages "group:20001")))
          (reaction (car (qq-state-message-reactions message))))
     (should (= (alist-get 'count reaction) 4))
     (should (eq (alist-get 'chosen-p reaction) t)))
   ;; A late action callback after the authoritative event is idempotent.
   (qq-state-apply-emoji-like-notice
    '((notice_type . "group_msg_emoji_like")
      (group_id . "20001")
      (user_id . "90001")
      (message_id . "9007199254741004001")
      (is_add . t)
      (likes . (((emoji_id . "178"))))))
   (should
    (= 4
       (alist-get
        'count
        (car
         (qq-state-message-reactions
          (car (qq-state-session-messages "group:20001")))))))
   ;; An authoritative remove-to-zero deletes the chip.
   (qq-state-apply-emoji-like-notice
    '((notice_type . "group_msg_emoji_like")
      (group_id . "20001")
      (user_id . "90001")
      (message_id . "9007199254741004001")
      (is_add . :false)
      (likes . (((emoji_id . "178") (count . 0))))))
   (should-not
    (qq-state-message-reactions
     (car (qq-state-session-messages "group:20001"))))))

(provide 'qq-state-test)

;;; qq-state-test.el ends here
