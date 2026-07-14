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

(defun qq-state-test--group-message
    (message-id time text &optional sender-name message-seq)
  "Return a raw group message fixture with MESSAGE-ID, TIME, TEXT, and sequence."
  `((post_type . "message")
    (message_type . "group")
    (chat_type . 2)
    (peer_uin . "20001")
    (group_id . 20001)
    (message_id . ,message-id)
    ,@(when message-seq `((message_seq . ,message-seq)))
    (user_id . 10001)
    (time . ,time)
    (sender . ((user_id . 10001)
               (nickname . ,(or sender-name "Alice"))))
    (raw_message . ,text)
    (message . (((type . "text") (data . ((text . ,text))))))))

(defun qq-state-test--recent-group-contact
    (message-id time text &optional peer-name message-seq)
  "Return one structured recent group contact fixture."
  (let ((sequence (or message-seq "10001")))
    `((chatType . 2)
      (peerUid . "20001")
      (peerUin . "20001")
      (peerName . ,(or peer-name "Group"))
      (msgTime . ,(format "%s" time))
      (msgId . ,message-id)
      (msgSeq . ,sequence)
      (lastestMsg
       . ,(qq-state-test--group-message
           message-id time text nil sequence)))))

(ert-deftest qq-state-session-key-normalizes-type-and-id ()
  (qq-test-with-reset
   (should (equal (qq-state-session-key 'private 12345)
                  "private:12345"))
   (should (equal (qq-state-session-key 'group "67890")
                  "group:67890"))
   (should (equal (qq-state-session-key 'dataline "device-1" 'desktop)
                  "dataline:desktop:device-1"))
   (should (equal (qq-state-session-key 'service "u_mail")
                  "service:u_mail"))
   (should-error (qq-state-session-key 'private "12345" 'desktop))
   (should-error (qq-state-session-key 'unknown "target"))))

(ert-deftest qq-state-session-key-round-trips-lossless-native-identities ()
  (let* ((desktop-key
          (qq-state-session-key 'dataline "dev:a" "desktop"))
         (mobile-key
          (qq-state-session-key 'dataline "dev:a" "mobile"))
         (desktop (qq-state-session-key-identity desktop-key))
         (mobile (qq-state-session-key-identity mobile-key))
         (service-key (qq-state-session-key 'service "u:mail:x"))
         (service (qq-state-session-key-identity service-key)))
    (should (equal desktop-key "dataline:desktop:dev:a"))
    (should (equal mobile-key "dataline:mobile:dev:a"))
    (should-not (equal desktop-key mobile-key))
    (should (equal desktop
                   '((type . dataline)
                     (target-id . "dev:a")
                     (chat-type . "8")
                     (peer-uid . "dev:a")
                     (variant . "desktop"))))
    (should (equal mobile
                   '((type . dataline)
                     (target-id . "dev:a")
                     (chat-type . "134")
                     (peer-uid . "dev:a")
                     (variant . "mobile"))))
    (should (equal service-key "service:u:mail:x"))
    (should (equal service
                   '((type . service)
                     (target-id . "u:mail:x")
                     (chat-type . "103")
                     (peer-uid . "u:mail:x")
                     (variant . nil))))))

(ert-deftest qq-state-session-key-rejects-incomplete-or-legacy-identities ()
  (should-error (qq-state-session-key 'dataline "dev:a"))
  (should-error (qq-state-session-key 'dataline "" 'desktop))
  (should-error (qq-state-session-key 'service ""))
  (should-error (qq-state-session-key-identity "dataline:dev:a"))
  (should-error (qq-state-session-key-identity "dataline:mobile:"))
  (should-error (qq-state-session-key-identity "service:"))
  (should-error (qq-state-session-key-identity "private:not-a-number")))

(ert-deftest qq-state-upsert-keeps-key-identity-and-private-native-uid ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . group)
      (target-id . "99999")
      (chat-type . "2")
      (peer-uid . "u:native:alice")
      (variant . "mobile"))
    nil)
   (let ((session (qq-state-session "private:10001")))
     (should (eq (alist-get 'type session) 'private))
     (should (equal (alist-get 'target-id session) "10001"))
     (should (equal (alist-get 'chat-type session) "1"))
     (should (equal (alist-get 'peer-uid session) "u:native:alice"))
     (should-not (alist-get 'variant session))
     (should (= (alist-get 'last-message-summary-token session) 0))
     (should-not (alist-get 'last-message-local-id session))
     (should-not (alist-get 'last-message-order session)))))

(ert-deftest qq-state-session-sendable-p-is-an-explicit-capability ()
  (should (qq-state-session-sendable-p "private:10001"))
  (should (qq-state-session-sendable-p "group:20001"))
  (should (qq-state-session-sendable-p "dataline:desktop:device-1"))
  (should-not (qq-state-session-sendable-p "service:u_mail"))
  (should-not (qq-state-session-sendable-p "unknown:target"))
  (should-not (qq-state-session-sendable-p nil)))

(ert-deftest qq-state-friend-categories-preserve-order-flatten-and-copy ()
  (qq-test-with-reset
   (let* ((source
           '(((category_id . 7) (sort_id . 10) (name . "工作")
              (online_count . 1)
              (friends . (((user_id . "10002") (nickname . "B") (remark))
                          ((user_id . "10001") (nickname . "A")
                           (remark . "Alice")))))
             ((category_id . 3) (sort_id . 20) (name . "空分组")
              (online_count . 0) (friends))))
          events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (eq (plist-get event :type) 'friends-refreshed)
                   (push event events))))
     (qq-state-apply-friend-categories source)
     (should (= (length events) 1))
     (should (= (plist-get (car events) :count) 2))
     (should (= (plist-get (car events) :category-count) 2))
     (should (= (qq-state-friend-count) 2))
     (should (equal (mapcar (lambda (friend) (alist-get 'user_id friend))
                            (qq-state-friends))
                    '("10002" "10001")))
     (should (equal (alist-get 'remark (qq-state-friend "10001")) "Alice"))
     (setf (alist-get 'name (car source)) "mutated")
     (should (equal (alist-get 'name (car (qq-state-friend-categories)))
                    "工作"))
     (let ((copy (qq-state-friend-categories)))
       (setf (alist-get 'nickname
                        (car (alist-get 'friends (car copy))))
             "changed")
       (should (equal (alist-get 'nickname (qq-state-friend "10002")) "B"))))))

(ert-deftest qq-state-joined-groups-preserve-snapshot-order ()
  (qq-test-with-reset
   (qq-state-apply-groups
    '(((group_id . "20002") (group_name . "Second"))
      ((group_id . "20001") (group_name . "First"))))
   (should (equal (mapcar (lambda (group) (alist-get 'group_id group))
                          (qq-state-groups))
                  '("20002" "20001")))
   (should (= (qq-state-group-count) 2))))

(ert-deftest qq-state-reset-clears-directory-order-and-categories ()
  (let ((qq-state-change-hook nil))
    (qq-state-apply-friend-categories
     '(((category_id . 1) (friends . (((user_id . "10001")))))))
    (qq-state-apply-groups '(((group_id . "20001"))))
    (qq-state-reset)
    (should-not (qq-state-friend-categories))
    (should-not (qq-state-friends))
    (should-not (qq-state-groups))
    (should (= (qq-state-friend-count) 0))
    (should (= (qq-state-group-count) 0))))

(ert-deftest qq-state-recent-snapshot-membership-is-not-session-existence ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    (list (qq-state-test--recent-group-contact
           "9007199254741004991" 1710000000 "hello")))
   (should (qq-state-session-recent-p "group:20001"))
   (qq-state-upsert-session
    "group:20002" '((type . group) (target-id . "20002")) nil)
   (should-not (qq-state-session-recent-p "group:20002"))
   (should (equal (qq-state-recent-session-keys) '("group:20001")))
   (let ((copy (qq-state-recent-session-keys)))
     (setcar copy "changed")
     (should (equal (qq-state-recent-session-keys) '("group:20001"))))))

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
   (qq-state-apply-friend-categories
    '(((category_id . 0) (sort_id . 0) (name . "好友")
       (online_count . 0)
       (friends . (((user_id . "10001")
                    (remark . "Alice")
                    (nickname . "Alice Nick")))))))
   (qq-state-upsert-session
    "private:10001"
    '((type . private)
      (target-id . "10001")
      (peer-uid . "u_private_alice"))
    nil)
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
     (should-not (string-match-p "unknown" (alist-get 'preview message)))
     (should-not
      (alist-get 'last-message-sender-name
                 (qq-state-session "private:10001")))
     (should-not
      (alist-get 'last-message-self-p
                 (qq-state-session "private:10001"))))))

(ert-deftest qq-state-private-poke-recall-requires-exact-stored-peer-uid ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (target-id . "10001"))
    nil)
   (should-error
    (qq-state-validate-poke-recall-reference
     "private:10001"
     (qq-state-test--poke-recall-reference
      "9007199254741007701" 1 "u_private_alice")))))

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
       (should-not
        (alist-get 'last-message-sender-name
                   (qq-state-session "group:987654321")))
       (should-not
        (alist-get 'last-message-self-p
                   (qq-state-session "group:987654321")))
       (should (equal (plist-get (car events) :message-anchor)
                      "9007199254750003456"))))))

(ert-deftest qq-state-apply-gray-tip-notice-prefers-native-semantic-text ()
  (qq-test-with-reset
   (let ((message
          (qq-state-apply-gray-tip-notice
           '((post_type . "notice")
             (notice_type . "notify")
             (sub_type . "gray_tip")
             (group_id . 20001)
             (user_id . 0)
             (message_id . "9007199254750003457")
             (busi_id . "group-member-add")
             (gray_tip_kind . "member-add")
             (text . "新同学加入群聊")
             (content . "新同学加入群聊")
             (raw_info . ((msgTime . "1710000001")))))))
     (should (equal (alist-get 'preview message) "新同学加入群聊"))
     (should (equal (alist-get 'raw-message message) "新同学加入群聊"))
     (should
      (equal
       (alist-get 'kind
                  (alist-get 'data (car (alist-get 'segments message))))
       "member-add")))))

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
       (msgSeq . "10001")
       (lastestMsg
        (time . 1710000000)
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10001")
        (user_id . 10001)
        (raw_message . "hello from napcat")
        (message . (((type . "text")
                     (data . ((text . "hello from napcat"))))))))))
   (let ((session (qq-state-session "private:10001")))
     (should (equal (alist-get 'title session) "Alice"))
     (should (equal (alist-get 'last-message-id session) "42"))
     (should (equal (alist-get 'last-message-seq session) "10001"))
     (should (equal (alist-get 'last-message-preview session)
                    "hello from napcat")))))

(ert-deftest qq-state-recent-contact-requires-one-exact-native-sequence ()
  (qq-test-with-reset
   (dolist (bad-sequence '(nil 10001 "" "-1" "1.5"))
     (should-error
      (qq-state-apply-recent-contacts
       `(((chatType . 2)
          (peerUid . "20001")
          (peerUin . "20001")
          (peerName . "Group")
          (msgTime . "1710000000")
          (msgId . "9007199254741004991")
          (msgSeq . ,bad-sequence)
          (lastestMsg . nil)))))
     (should-not (qq-state-session "group:20001")))
   (should-error
    (qq-state-apply-recent-contacts
     '(((chatType . 2)
        (peerUid . "20001")
        (peerUin . "20001")
        (peerName . "Group")
        (msgTime . "1710000000")
        (msgId . "9007199254741004991")
        (msgSeq . "10002")
        (lastestMsg
         (message_seq . "10001")
         (time . 1710000000)
         (message_type . "group")
         (chat_type . 2)
         (peer_uin . "20001")
         (user_id . 10001)
         (message . (((type . "text")
                      (data . ((text . "mismatch")))))))))))
   (should-not (qq-state-session "group:20001"))))

(ert-deftest qq-state-recent-private-self-message-routes-by-native-peer-uin ()
  "A self-sent preview belongs to its peer, never to the logged-in account."
  (qq-test-with-reset
   (qq-state-set-self-info
    '((user_id . "90001") (nickname . "Me")))
   (qq-state-apply-recent-contacts
    '(((chatType . 1)
       (peerUin . "10001")
       (peerName . "Alice")
       (msgTime . "1710000000")
       (msgId . "9007199254744001234")
       (msgSeq . "10001")
       (lastestMsg
        (post_type . "message_sent")
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10001")
        (self_id . "90001")
        (user_id . "90001")
        (time . 1710000000)
        (message . (((type . "text")
                     (data . ((text . "hello"))))))))))
   (let ((messages (qq-state-session-messages "private:10001")))
     (should (= (length messages) 1))
     (should (equal (alist-get 'session-key (car messages))
                    "private:10001"))
     (should (equal (alist-get 'target-id (car messages)) "10001")))
   (let ((session (qq-state-session "private:10001")))
     (should (equal (alist-get 'last-message-sender-name session)
                    "90001"))
     (should (eq (alist-get 'last-message-self-p session) t)))
   (should-not (qq-state-session "private:90001"))))

(ert-deftest qq-state-apply-recent-contacts-uses-kernel-unread-count ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (msgSeq . "10001")
       (unreadCount . 17))))
   (should (= 17 (alist-get 'unread-count
                            (qq-state-session "group:20001"))))))

(ert-deftest qq-state-apply-recent-contacts-null-unread-preserves-read-state ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (target-id . "20001")
      (title . "Old") (unread-count . 6))
    nil)
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Fresh title")
       (msgTime . "1710000000")
       (msgId . "9007199254741004991")
       (msgSeq . "10001")
       (lastMessagePreview . "latest")
       (unreadCount))))
   (let ((session (qq-state-session "group:20001")))
     (should (= 6 (alist-get 'unread-count session)))
     (should (equal "Fresh title" (alist-get 'title session))))))

(ert-deftest qq-state-apply-recent-unread-invalidates-older-exact-position ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (target-id . "20001")
      (unread-count . 6)
      (first-unread-message-id . "9007199254742007089")
      (first-unread-message-seq . "30001")
      (read-latest-message-id . "9007199254742007094")
      (read-position-available . t))
    nil)
   (qq-state-apply-recent-contacts
    '(((chatType . 2) (peerUid . "20001") (peerUin . "20001")
       (msgTime . "1710000000")
       (msgId . "9007199254741004991")
       (msgSeq . "10001")
       (lastMessagePreview . "latest")
       (unreadCount . 4))))
   (let ((session (qq-state-session "group:20001")))
     (should (= 4 (alist-get 'unread-count session)))
     (should-not (alist-get 'first-unread-message-id session))
     (should-not (alist-get 'first-unread-message-seq session))
     (should-not (alist-get 'read-latest-message-id session))
     (should-not (alist-get 'read-position-available session)))))

(ert-deftest qq-state-apply-recent-contacts-can-reject-stale-unread-only ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (target-id . "20001")
      (title . "Old") (unread-count . 6))
    nil)
   (let (checked-key)
     (qq-state-apply-recent-contacts
      '(((chatType . 2)
         (peerUid . "20001")
         (peerUin . "20001")
         (peerName . "Fresh title")
         (msgTime . "1710000000")
         (msgId . "9007199254741004991")
         (msgSeq . "10001")
         (lastMessagePreview . "latest")
         (unreadCount . 9)))
      (lambda (session-key)
        (setq checked-key session-key)
        nil))
     (let ((session (qq-state-session "group:20001")))
       (should (equal checked-key "group:20001"))
       (should (= 6 (alist-get 'unread-count session)))
       (should (equal "Fresh title" (alist-get 'title session)))))))

(ert-deftest qq-state-apply-recent-contacts-calibrates-native-mentions ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUid . "20001")
       (peerUin . "20001")
       (peerName . "Group")
       (msgTime . "1710000000")
       (msgId . "9007199254743009336")
       (msgSeq . "10001")
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
       (msgSeq . "10001")
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
       (msgSeq . "10001")
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

(ert-deftest qq-state-read-position-preserves-sequence-without-exact-anchor ()
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
     (should (equal "30001"
                    (alist-get 'first-unread-message-seq session)))
     (should-not (alist-get 'read-position-available session)))))

(ert-deftest qq-state-identical-authoritative-read-state-is-a-no-op ()
  (qq-test-with-reset
   (qq-state-upsert-session "group:20001" nil nil)
   (let ((read-state
          '((unread_count . 5)
            (first_unread
             . ((sequence . "30001")
                (message_id . "9007199254742007089")))
            (mentions . ((at_me . nil) (at_all . nil)))
            (latest . nil)))
         events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (and (eq (plist-get event :type) 'session)
                            (eq (plist-get event :mutation) 'read))
                   (push event events))))
     (qq-state-apply-session-read-state "group:20001" read-state)
     (qq-state-apply-session-read-state "group:20001" read-state)
     (should (= 1 (length events))))))

(ert-deftest qq-state-rejects-numeric-message-id-from-wire ()
  (qq-test-with-reset
   (should-error
    (qq-state-merge-live-message
     '((post_type . "message")
       (message_type . "group")
       (chat_type . 2)
       (peer_uin . "20001")
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
       (msgSeq . "10001")
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
       (peerUid . "dev:a")
       (peerUin . "0")
       (remark . "")
       (peerName . "")
       (msgTime . "1710000000")
       (msgId . "42")
       (msgSeq . "0")
       (lastestMsg
        (time . 1710000000)
        (message_type . "private")
        (chat_type . 8)
        (peer_uid . "dev:a")
        (peer_uin . "0")
        (user_id . 0)
        (sender . ((user_id . 0)
                   (nickname . "我的手机")))
        (raw_message . "hello from phone")
        (message . (((type . "text")
                     (data . ((text . "hello from phone"))))))))
      ((chatType . 134)
       (peerUid . "dev:a")
       (peerUin . "0")
       (remark . "")
       (peerName . "")
       (msgTime . "1710000001")
       (msgId . "43")
       (msgSeq . "0")
       (lastestMsg
        (time . 1710000001)
        (message_type . "private")
        (chat_type . 134)
        (peer_uid . "dev:a")
        (peer_uin . "0")
        (user_id . 0)
        (sender . ((user_id . 0)
                   (nickname . "我的手机")))
        (raw_message . "second device")
        (message . (((type . "text")
                     (data . ((text . "second device"))))))))))
   (let ((session-1 (qq-state-session "dataline:desktop:dev:a"))
         (session-2 (qq-state-session "dataline:mobile:dev:a")))
     (should (equal (alist-get 'title session-1) "我的手机"))
     (should (equal (alist-get 'title session-2) "我的手机"))
     (should (equal (alist-get 'peer-uid session-1) "dev:a"))
     (should (equal (alist-get 'peer-uid session-2) "dev:a"))
     (should (equal (alist-get 'variant session-1) "desktop"))
     (should (equal (alist-get 'variant session-2) "mobile"))
     (should (equal (alist-get 'chat-type session-1) "8"))
     (should (equal (alist-get 'chat-type session-2) "134"))
     (should (equal (alist-get 'last-message-preview session-1) "hello from phone"))
     (should (equal (alist-get 'last-message-preview session-2) "second device")))))

(ert-deftest qq-state-recent-contacts-require-exact-native-identity ()
  (qq-test-with-reset
   (qq-state-apply-recent-contacts
    '(((chatType . 8) (peerUid . "") (peerUin . "90001"))
      ((chatType . 134) (peerUin . "90002"))
      ((chatType . 103) (peerUid . "") (peerUin . "90003"))
      ((chatType . 999) (peerUid . "u:unknown") (peerUin . "90004"))))
   (should-not (qq-state-sessions))))

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
                      snowflake)))
     (let ((session (qq-state-session "private:10001")))
       (should (equal (alist-get 'last-message-id session) snowflake))
       (should (equal (alist-get 'last-message-local-id session) local-id))
       (should (= (alist-get 'last-message-order session)
                  (alist-get 'order pending)))))))

(ert-deftest qq-state-late-send-failure-does-not-downgrade-server-backed-message ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001) (nickname . "Me")))
   (qq-state-upsert-session "private:10001" '((title . "Alice")) nil)
   (let* ((pending (qq-state-insert-pending-text-message
                    "private:10001" "ping"))
          (local-id (alist-get 'local-id pending))
          (snowflake "9007199254741004645"))
     (qq-state-mark-pending-message-sent
      "private:10001" local-id snowflake)
     (should-not
      (qq-state-mark-pending-message-failed
       "private:10001" local-id "late timeout"))
     (let ((message (car (qq-state-session-messages "private:10001"))))
       (should (eq (alist-get 'status message) 'sent))
       (should (equal (alist-get 'server-id message) snowflake))
       (should-not (alist-get 'error message))))))

(ert-deftest qq-state-rich-self-event-rekeys-pending-before-late-failure ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10001" '((type . private) (target-id . "10001")) nil)
   (let* ((segments '(((type . "at")
                       (data . ((qq . "10001") (name . "Alice Card"))))))
          (pending (qq-state-insert-pending-message
                    "private:10001" segments))
          (local-id (alist-get 'local-id pending))
          (snowflake "9007199254741004646")
          (now (truncate (float-time))))
     ;; NapCat's authoritative self event uses CQ raw_message while the local
     ;; rich pending row uses its readable preview.  Segment identity, not
     ;; those incompatible strings, must reconcile the one timeline row.
     (qq-state-merge-live-message
      `((post_type . "message_sent")
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10001")
        (message_id . ,snowflake)
        (self_id . "90001")
        (user_id . "90001")
        (target_id . "10001")
        (time . ,now)
        (sender . ((user_id . "90001") (nickname . "Me")))
        (raw_message . "[CQ:at,qq=10001]")
        (message . ,segments)))
     (let ((messages (qq-state-session-messages "private:10001")))
       (should (= 1 (length messages)))
       (should (equal (alist-get 'local-id (car messages)) local-id))
       (should (equal (alist-get 'server-id (car messages)) snowflake))
       (should (eq (alist-get 'status (car messages)) 'sent)))
     (should-not
      (qq-state-mark-pending-message-failed
       "private:10001" local-id "late timeout")))))

(ert-deftest qq-state-equal-media-self-events-reconcile-pending-fifo ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10001" '((type . private) (target-id . "10001")) nil)
   (let* ((first (qq-state-insert-pending-message
                  "private:10001"
                  '(((type . "image") (data . ((file . "/tmp/a.png")))))))
          (second (qq-state-insert-pending-message
                   "private:10001"
                   '(((type . "image") (data . ((file . "/tmp/b.png")))))))
          (first-local (alist-get 'local-id first))
          (second-local (alist-get 'local-id second))
          (first-server "9007199254741004647")
          (second-server "9007199254741004648")
          (now (truncate (float-time))))
     (cl-labels
         ((merge-image
           (server-id remote-file)
           (qq-state-merge-live-message
            `((post_type . "message_sent")
              (message_type . "private")
              (chat_type . 1)
              (peer_uin . "10001")
              (message_id . ,server-id)
              (self_id . "90001")
              (user_id . "90001")
              (target_id . "10001")
              (time . ,now)
              (sender . ((user_id . "90001") (nickname . "Me")))
              (raw_message . ,(format "[CQ:image,file=%s]" remote-file))
              (message . (((type . "image")
                           (data . ((file . ,remote-file)
                                    (sub_type . 0))))))))))
       (merge-image first-server "remote-a")
       (merge-image second-server "remote-b"))
     (let* ((messages (qq-state-session-messages "private:10001"))
            (first-row (seq-find
                        (lambda (message)
                          (equal (alist-get 'local-id message) first-local))
                        messages))
            (second-row (seq-find
                         (lambda (message)
                           (equal (alist-get 'local-id message) second-local))
                         messages)))
       (should (= 2 (length messages)))
       (should (equal (alist-get 'server-id first-row) first-server))
       (should (equal (alist-get 'server-id second-row) second-server))
       (should-not (seq-some
                    (lambda (message)
                      (eq (alist-get 'status message) 'pending))
                    messages))))))

(ert-deftest qq-state-rich-signature-outranks-equal-readable-preview ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10001" '((type . private) (target-id . "10001")) nil)
   (let* ((first (qq-state-insert-pending-message
                  "private:10001"
                  '(((type . "at")
                     (data . ((qq . "10011") (name . "Alex")))))))
          (second (qq-state-insert-pending-message
                   "private:10001"
                   '(((type . "at")
                      (data . ((qq . "10012") (name . "Alex")))))))
          (first-local (alist-get 'local-id first))
          (second-local (alist-get 'local-id second))
          (first-server "9007199254741004649")
          (second-server "9007199254741004650")
          (now (truncate (float-time))))
     (cl-labels
         ((merge-at
           (server-id target)
           (qq-state-merge-live-message
            `((post_type . "message_sent")
              (message_type . "private")
              (chat_type . 1)
              (peer_uin . "10001")
              (message_id . ,server-id)
              (self_id . "90001")
              (user_id . "90001")
              (target_id . "10001")
              (time . ,now)
              (sender . ((user_id . "90001") (nickname . "Me")))
              ;; Both targets deliberately have the same visible name.
              (raw_message . "@Alex")
              (message . (((type . "at")
                           (data . ((qq . ,target) (name . "Alex"))))))))))
       ;; Even an out-of-order event can select the stable @target rather than
       ;; the first row with the same readable preview.
       (merge-at second-server "10012")
       (merge-at first-server "10011"))
     (let ((messages (qq-state-session-messages "private:10001")))
       (should (= 2 (length messages)))
       (should
        (equal first-server
               (alist-get
                'server-id
                (seq-find (lambda (message)
                            (equal (alist-get 'local-id message) first-local))
                          messages))))
       (should
        (equal second-server
               (alist-get
                'server-id
                (seq-find (lambda (message)
                            (equal (alist-get 'local-id message) second-local))
                          messages))))))))

(ert-deftest qq-state-live-message-leaves-unread-to-kernel-and-supports-recall ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 1)
      (peer_uin . "10001")
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
   (qq-state-apply-recall "private:10001" "9007199254741004123")
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (qq-state-message-recalled-p message))
     (should (equal (alist-get 'preview message) "[message recalled]")))))

(ert-deftest qq-state-live-message-classifies-only-native-self-and-all-mentions ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001) (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "group")
      (chat_type . 2)
      (peer_uin . "20001")
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
      (chat_type . 1)
      (peer_uin . "10001")
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
      (chat_type . 1)
      (peer_uin . "10001")
      (message_id . "9007199254741005555")
      (user_id . 90001)
      (target_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 90001)
                 (nickname . "Me")))
      (raw_message . "will recall")
      (message . (((type . "text")
                   (data . ((text . "will recall"))))))))
   (qq-state-apply-recall "private:10001" "9007199254741005555")
   (let ((message (car (qq-state-session-messages "private:10001"))))
     (should (qq-state-message-recalled-p message))
     (should (equal (alist-get 'server-id message) "9007199254741005555")))))

(ert-deftest qq-state-napcat-recalled-flag-stores-stub ()
  "Protocol `recalled' stores a stub (display policy is chat's job)."
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 1)
      (peer_uin . "10001")
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
      (chat_type . 1)
      (peer_uin . "10001")
      (message_id . "9007199254741005555")
      (user_id . 90001)
      (target_id . 10001)
      (time . 1710000001)
      (sender . ((user_id . 90001) (nickname . "Me")))
      (raw_message . "x")
      (message . (((type . "text") (data . ((text . "x"))))))))
   (qq-state-apply-recall "private:10001" "9007199254741005555")
   (qq-state-merge-history
    "private:10001"
    (list
     '((post_type . "message_sent")
       (message_type . "private")
       (chat_type . 1)
       (peer_uin . "10001")
       (message_id . "9007199254741005555")
       (user_id . 90001)
       (target_id . 10001)
       (time . 1710000001)
       (sender . ((user_id . 90001) (nickname . "Me")))
       (raw_message . "")
       (message . ()))))
   (should (qq-state-message-recalled-p
            (car (qq-state-session-messages "private:10001"))))))

(ert-deftest qq-state-live-mobile-dataline-preserves-colon-peer-uid ()
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . 90001)
                             (nickname . "Me")))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 134)
      (peer_uid . "dev:a")
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
   (let* ((session (qq-state-session "dataline:mobile:dev:a"))
          (message (car (qq-state-session-messages
                         "dataline:mobile:dev:a"))))
     (should (equal (alist-get 'title session) "我的手机"))
     (should (equal (alist-get 'peer-uid session) "dev:a"))
     (should (equal (alist-get 'variant session) "mobile"))
     (should (equal (alist-get 'sender-name message) "我的手机"))
     (should-not (alist-get 'self-p message))
     (should (= (alist-get 'unread-count session) 0)))))

(ert-deftest qq-state-live-missing-or-unknown-chat-type-is-not-routed ()
  (qq-test-with-reset
   (should-not
    (qq-state-merge-live-message
     '((post_type . "message")
       (message_type . "private")
       (message_id . "125")
       (user_id . 10001)
       (time . 1710000001)
       (sender . ((user_id . 10001) (nickname . "Unknown")))
       (message . (((type . "text") (data . ((text . "ignored")))))))))
   (should-not
    (qq-state-merge-live-message
     '((post_type . "message")
       (message_type . "private")
       (chat_type . 999)
       (peer_uid . "u:unknown")
       (message_id . "124")
       (user_id . 10001)
       (time . 1710000001)
       (sender . ((user_id . 10001) (nickname . "Unknown")))
       (message . (((type . "text") (data . ((text . "ignored")))))))))
   (should-not (qq-state-sessions))))

(ert-deftest qq-state-history-requires-the-requested-native-identity ()
  (qq-test-with-reset
   (should-error
    (qq-state-merge-history
     "private:10001"
     '(((message_id . "125")
        (message_type . "private")
        (user_id . 10001)
        (time . 1710000001)
        (sender . ((user_id . 10001) (nickname . "Alice")))
        (message . ())))))
   (should-error
    (qq-state-merge-history
     "dataline:desktop:dev:a"
     '(((message_id . "126")
        (message_type . "private")
        (chat_type . 134)
        (peer_uid . "dev:a")
        (user_id . 0)
        (time . 1710000002)
        (sender . ((user_id . 0) (nickname . "我的手机")))
        (message . ())))))))

(ert-deftest qq-state-private-history-self-message-routes-by-native-peer-uin ()
  (qq-test-with-reset
   (qq-state-set-self-info
    '((user_id . "90001") (nickname . "Me")))
   (qq-state-merge-history
    "private:10001"
    '(((post_type . "message_sent")
       (message_type . "private")
       (chat_type . 1)
       (peer_uin . "10001")
       (message_id . "9007199254744001234")
       (self_id . "90001")
       (user_id . "90001")
       ;; Participant/recipient metadata is deliberately contradictory.  It
       ;; must not participate in conversation identity.
       (target_id . "90001")
       (time . 1710000001)
       (sender . ((user_id . "90001") (nickname . "Me")))
       (message . (((type . "text") (data . ((text . "hello")))))))))
   (let ((message
          (car (qq-state-session-messages "private:10001"))))
     (should (equal (alist-get 'session-key message)
                    "private:10001"))
     (should (equal (alist-get 'target-id message) "10001"))
     (should (equal (alist-get 'peer-uin message) "10001")))
   (should-not (qq-state-session "private:90001"))))

(ert-deftest qq-state-private-history-rejects-peer-uin-context-conflict ()
  (qq-test-with-reset
   (should-error
    (qq-state-merge-history
     "private:10001"
     '(((message_type . "private")
        (chat_type . 1)
        (peer_uin . "90001")
        (message_id . "9007199254744001235")
        (target_id . "10001")
        (user_id . "90001")
        (time . 1710000002)
        (message . ())))))))

(ert-deftest qq-state-private-history-rejects-missing-native-peer-uin ()
  (qq-test-with-reset
   (should-error
    (qq-state-merge-history
     "private:10001"
     '(((message_type . "private")
        (chat_type . 1)
        (message_id . "9007199254744001236")
        (target_id . "10001")
        (user_id . "10001")
        (time . 1710000003)
        (message . ())))))))

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
      (chat_type . 2)
      (peer_uin . "20001")
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
     (should-not (alist-get 'sender-secondary-name message))
     (let ((session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-sender-name session) "Alice"))
       (should-not (alist-get 'last-message-self-p session))))))

(ert-deftest qq-state-group-message-keeps-card-and-nickname-for-display ()
  (qq-test-with-reset
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "group")
      (chat_type . 2)
      (peer_uin . "20001")
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
   (qq-state-apply-friend-categories
    '(((category_id . 0) (sort_id . 0) (name . "好友")
       (online_count . 0)
       (friends . (((user_id . "10001")
                    (remark . "Alice Remark")
                    (nickname . "Alice Nick")))))))
   (qq-state-merge-live-message
    '((post_type . "message")
      (message_type . "private")
      (chat_type . 1)
      (peer_uin . "10001")
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
       (msgSeq . "10001")
       (lastestMsg
        (time . 1710000001)
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10002")
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
       (msgSeq . "10001")
       (lastestMsg . nil)
       (lastMessagePreview . "群通知第一行\n  第二行"))))
   (should (equal
            "群通知第一行 第二行"
            (alist-get 'last-message-preview
                       (qq-state-session "group:20002"))))))

(ert-deftest qq-state-recent-fallback-preview-clears-stale-latest-sender ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "group:20002"
    '((last-message-id . "9007199254741004990")
      (last-message-preview . "old")
      (last-message-sender-name . "Alice")
      (last-message-self-p . t))
    nil)
   (qq-state-apply-recent-contacts
    '(((chatType . 2)
       (peerUin . "20002")
       (peerName . "Group")
       (msgTime . "1710000002")
       (msgId . "9007199254741004991")
       (msgSeq . "10001")
       (lastestMsg . nil)
       (lastMessagePreview . "new fallback"))))
   (let ((session (qq-state-session "group:20002")))
     (should (equal (alist-get 'last-message-preview session)
                    "new fallback"))
     (should-not (alist-get 'last-message-sender-name session))
     (should-not (alist-get 'last-message-self-p session)))))

(ert-deftest qq-state-recent-fallback-preserves-sender-at-same-frontier ()
  (qq-test-with-reset
   (let* ((message-id "9007199254741005100")
          (first-token (qq-state-session-summary-observation-start))
          (second-token (qq-state-session-summary-observation-start)))
     (qq-state-apply-recent-contacts
      (list (qq-state-test--recent-group-contact
             message-id 1710000100 "structured"))
      nil first-token)
     (qq-state-apply-recent-contacts
      `(((chatType . 2)
         (peerUid . "20001")
         (peerUin . "20001")
         (peerName . "Group")
         (msgTime . "1710000100")
         (msgId . ,message-id)
         (msgSeq . "10001")
         (lastestMsg . nil)
         (lastMessagePreview . "fallback")))
      nil second-token)
     (let ((session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-preview session) "fallback"))
       (should (equal (alist-get 'last-message-sender-name session) "Alice"))
       (should-not (alist-get 'last-message-self-p session))
       (should (= (alist-get 'last-message-summary-token session)
                  second-token))))))

(ert-deftest qq-state-summary-frontier-rejects-older-history-and-patches ()
  (qq-test-with-reset
   (let ((frontier-id "9007199254741005200")
         (older-id "9007199254741005120"))
     ;; Model a recent-contact fallback whose authoritative latest message is
     ;; not materialized in the canonical timeline.
     (qq-state-apply-recent-contacts
      `(((chatType . 2)
         (peerUid . "20001")
         (peerUin . "20001")
         (peerName . "Group")
         (msgTime . "1710000200")
         (msgId . ,frontier-id)
         (msgSeq . "10002")
         (lastestMsg . nil)
         (lastMessagePreview . "M100"))))
     (let ((frontier-token
            (alist-get 'last-message-summary-token
                       (qq-state-session "group:20001"))))
       (qq-state-merge-history
        "group:20001"
        (list (qq-state-test--group-message
               older-id 1710000120 "M20")))
       (should (equal (alist-get 'last-message-id
                                 (qq-state-session "group:20001"))
                      frontier-id))
       (qq-state-apply-emoji-like-notice
        "group:20001"
        `((group_id . "20001")
          (user_id . "10002")
          (message_id . ,older-id)
          (is_add . t)
          (likes . (((emoji_id . "178"))))))
       (qq-state-apply-recall "group:20001" older-id)
       (let ((session (qq-state-session "group:20001")))
         (should (equal (alist-get 'last-message-id session) frontier-id))
         (should (equal (alist-get 'last-message-preview session) "M100"))
         (should (= (alist-get 'last-message-summary-token session)
                    frontier-token)))))))

(ert-deftest qq-state-summary-selects-max-sequence-inside-same-second-batch ()
  (qq-test-with-reset
   (let ((newer-id "9007199254741005300")
         (older-id "9007199254749009718"))
     ;; NT ids from incoming/self paths are not chronological.  Even though
     ;; the older identity is numerically larger and arrives second, the
     ;; per-session message sequence owns the exact same-second order.
     (qq-state-merge-history
      "group:20001"
      (list (qq-state-test--group-message
             newer-id 1710000300 "newer" nil "40002")
            (qq-state-test--group-message
             older-id 1710000300 "older" nil "40001")))
     (let ((session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-id session) newer-id))
       (should (equal (alist-get 'last-message-seq session) "40002"))
       (should (equal (alist-get 'last-message-preview session) "newer"))))))

(ert-deftest qq-state-summary-does-not-order-self-and-incoming-by-message-id ()
  (qq-test-with-reset
   ;; Concrete Example Group shape: a self message has a numerically larger NT id,
   ;; but later incoming messages advance both server time and session seq.
   (qq-state-merge-live-message
    (qq-state-test--group-message
     "9007199254749009718" 1710000300 "earlier message"
     "Me" "40001"))
   (qq-state-merge-live-message
    (qq-state-test--group-message
     "9007199254748009884" 1710000301 "later message"
     "Bob" "40002"))
   (let ((session (qq-state-session "group:20001")))
     (should (equal (alist-get 'last-message-id session)
                    "9007199254748009884"))
     (should (equal (alist-get 'last-message-seq session) "40002"))
     (should (equal (alist-get 'last-message-preview session)
                    "later message"))
     (should (equal (alist-get 'last-message-sender-name session)
                    "Bob")))))

(ert-deftest qq-state-summary-older-recent-cannot-overwrite-later-live-message ()
  (qq-test-with-reset
   (let* ((older-token (qq-state-session-summary-observation-start))
          (older-id "9007199254741005401")
          (live-id "9007199254741005402")
          (contact (qq-state-test--recent-group-contact
                    older-id 1710000401 "older recent" "stale metadata")))
     (qq-state-merge-live-message
      (qq-state-test--group-message live-id 1710000402 "live" "Bob"))
     (push '(unreadCount . 9) contact)
     (qq-state-apply-recent-contacts
      (list contact) (lambda (_session-key) t) older-token)
     (let ((session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-id session) live-id))
       (should (equal (alist-get 'last-message-preview session) "live"))
       (should (equal (alist-get 'last-message-sender-name session) "Bob"))
       ;; Metadata and the independently accepted unread projection still
       ;; refresh even though the message-summary owner lost.
       (should (equal (alist-get 'title session) "stale metadata"))
       (should (= (alist-get 'unread-count session) 9))))))

(ert-deftest qq-state-summary-newer-recent-response-supersedes-older-request ()
  (qq-test-with-reset
   (let ((older-token (qq-state-session-summary-observation-start))
         (newer-token (qq-state-session-summary-observation-start)))
     (qq-state-apply-recent-contacts
      (list (qq-state-test--recent-group-contact
             "9007199254741005502" 1710000502 "newer" "new metadata"))
      nil newer-token)
     (qq-state-apply-recent-contacts
      (list (qq-state-test--recent-group-contact
             "9007199254741005501" 1710000501 "older" "old metadata"))
      nil older-token)
     (let ((session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-id session)
                      "9007199254741005502"))
       (should (equal (alist-get 'last-message-preview session) "newer"))
       (should (= (alist-get 'last-message-summary-token session)
                  newer-token))
       (should (equal (alist-get 'title session) "old metadata"))))))

(ert-deftest qq-state-recent-malformed-structured-latest-is-atomic ()
  (qq-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Before")
      (unread-count . 3)
      (last-message-id . "9007199254741005600")
      (last-message-time . 1710000600)
      (last-message-preview . "before")
      (last-message-sender-name . "Alice")
      (last-message-summary-token . 7))
    nil)
   (let ((before (qq-state-session "group:20001"))
         (gate-called nil))
     (should-error
      (qq-state-apply-recent-contacts
       '(((chatType . 2)
          (peerUid . "20001")
          (peerUin . "20001")
          (peerName . "After")
          (msgTime . "1710000601")
          (msgId . "9007199254741005601")
          (msgSeq . "10002")
          (unreadCount . 9)
          (lastestMsg
           (message_type . "private")
           (chat_type . 1)
           (peer_uin . "99999")
           (user_id . 99999)
           (message . (((type . "text")
                        (data . ((text . "malformed")))))))))
       (lambda (_session-key) (setq gate-called t))))
     (should-not gate-called)
     (should (equal (qq-state-session "group:20001") before)))))

(ert-deftest qq-state-summary-orders-consecutive-same-second-local-messages ()
  (qq-test-with-reset
   (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1710000700.0)))
     (let* ((first (qq-state-insert-pending-text-message
                    "group:20001" "first"))
            (second (qq-state-insert-pending-text-message
                     "group:20001" "second"))
            (session (qq-state-session "group:20001")))
       (should (equal (alist-get 'last-message-id session)
                      (alist-get 'local-id second)))
       (should (equal (alist-get 'last-message-preview session) "second"))
       (should (> (alist-get 'last-message-order session)
                  (alist-get 'order first)))))))

(ert-deftest qq-state-summary-keeps-local-pending-over-equal-time-fallback ()
  (qq-test-with-reset
   (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1710000800.0)))
     (let* ((pending (qq-state-insert-pending-text-message
                      "group:20001" "pending"))
            (local-id (alist-get 'local-id pending))
            (token (qq-state-session-summary-observation-start)))
       (qq-state-apply-recent-contacts
        '(((chatType . 2)
           (peerUid . "20001")
           (peerUin . "20001")
           (peerName . "Group")
           (msgTime . "1710000800")
           (msgId . "9007199254741005800")
           (msgSeq . "10001")
           (lastestMsg . nil)
           (lastMessagePreview . "fallback")))
        nil token)
       (let ((session (qq-state-session "group:20001")))
         (should (equal (alist-get 'last-message-id session) local-id))
         (should (equal (alist-get 'last-message-preview session)
                        "pending")))))))

(ert-deftest qq-state-summary-releases-local-frontier-after-promotion ()
  (qq-test-with-reset
   (cl-letf (((symbol-function 'float-time) (lambda (&optional _) 1710000900.0)))
     (let* ((pending (qq-state-insert-pending-text-message
                      "group:20001" "local A"))
            (local-id (alist-get 'local-id pending))
            (sent-a "9007199254741005900")
            (remote-m2 "9007199254741005901"))
       ;; The unresolved local row wins the equal-second race temporarily.
       (qq-state-merge-live-message
        (qq-state-test--group-message
         remote-m2 1710000900 "remote M2" "Bob"))
       (should (equal (alist-get 'last-message-id
                                 (qq-state-session "group:20001"))
                      local-id))
       ;; Promotion proves A's exact server position.  The canonical maximum
       ;; is M2, so root must leave the local frontier and select M2.
       (qq-state-mark-pending-message-sent
        "group:20001" local-id sent-a)
       (let ((session (qq-state-session "group:20001")))
         (should (equal (alist-get 'last-message-id session) remote-m2))
         (should (equal (alist-get 'last-message-preview session)
                        "remote M2"))
         (should-not (string-prefix-p
                      "local-" (alist-get 'last-message-id session))))))))

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
          (chat_type . 1)
          (peer_uin . "10001")
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
       (qq-state-apply-recall "private:10001" "9007199254741004999")
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
           (chat_type . 1)
           (peer_uin . "10001")
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

(ert-deftest qq-state-filter-only-recall-emits-exact-message-patch ()
  (qq-test-with-reset
   (let ((message-id "9007199254741004881")
         events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (eq (plist-get event :type) 'message)
                   (push event events))))
     (should-not (qq-state-session-messages "private:10001"))
     (should-not (qq-state-apply-recall "private:10001" message-id))
     (let ((event (car events)))
       (should (equal (plist-get event :session-key) "private:10001"))
       (should (equal (plist-get event :message-anchor) message-id))
       (should (eq (plist-get event :mutation) 'update))
       (should (eq (plist-get event :source) 'notice))
       (should (integerp (plist-get event :observation-token)))
       (should (equal (plist-get event :observation-token)
                      (plist-get (plist-get event :message-patch)
                                 :observation-token)))
       (should (eq (plist-get (plist-get event :message-patch) :kind)
                   'recall))
       (should-not (plist-member event :message)))
     (should-not (qq-state-session-messages "private:10001")))))

(ert-deftest qq-state-filter-only-emoji-like-emits-exact-message-patch ()
  (qq-test-with-reset
   (let* ((message-id "9007199254741004882")
          (notice
           `((notice_type . "group_msg_emoji_like")
             (group_id . "20001")
             (user_id . "10002")
             (message_id . ,message-id)
             (is_add . t)
             (likes . (((emoji_id . "178") (count . 2))))))
          (before-token (qq-state-message-observation-token))
          events)
     (add-hook 'qq-state-change-hook
               (lambda (event)
                 (when (eq (plist-get event :type) 'message)
                   (push event events))))
     (should-not (qq-state-session-messages "group:20001"))
     (should-not
      (qq-state-apply-emoji-like-notice "group:20001" notice))
     (let* ((event (car events))
            (patch (plist-get event :message-patch)))
       (should (equal (plist-get event :session-key) "group:20001"))
       (should (equal (plist-get event :message-anchor) message-id))
       (should (eq (plist-get event :mutation) 'update))
       (should (eq (plist-get event :source) 'notice))
       (should (eq (plist-get patch :kind) 'emoji-like))
       (should (integerp (plist-get event :observation-token)))
       (should (= (1+ before-token)
                  (plist-get event :observation-token)))
       (should (equal (plist-get event :observation-token)
                      (plist-get patch :observation-token)))
       (should (equal (plist-get patch :notice) notice))
       (should-not (plist-member event :message)))
     (should-not (qq-state-session-messages "group:20001")))))

(ert-deftest qq-state-recall-before-history-remains-authoritative ()
  "A notice must survive an older snapshot materializing the message later."
  (qq-test-with-reset
   (let ((message-id "9007199254741004885"))
     (should-not (qq-state-apply-recall "private:10001" message-id))
     (qq-state-merge-history
      "private:10001"
      `(((message_id . ,message-id)
         (message_type . "private")
         (chat_type . 1)
         (peer_uin . "10001")
         (user_id . "10001")
         (time . 1710000001)
         (sender . ((user_id . "10001") (nickname . "Alice")))
         (raw_message . "stale history")
         (message . (((type . "text")
                      (data . ((text . "stale history")))))))))
     (let ((message (car (qq-state-session-messages "private:10001"))))
       (should (qq-state-message-recalled-p message))
       (should (equal (alist-get 'preview message) "[message recalled]")))
     ;; A duplicate live snapshot is another stale materialization boundary.
     (qq-state-merge-live-message
      `((post_type . "message")
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10001")
        (message_id . ,message-id)
        (user_id . "10001")
        (time . 1710000001)
        (sender . ((user_id . "10001") (nickname . "Alice")))
        (raw_message . "stale live")
        (message . (((type . "text")
                     (data . ((text . "stale live"))))))))
     (should (qq-state-message-recalled-p
              (car (qq-state-session-messages "private:10001")))))))

(ert-deftest qq-state-reaction-window-repairs-only-older-request ()
  "A reaction repairs an older request without freezing future snapshots."
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (let* ((message-id "9007199254741004886")
          (notice
           `((notice_type . "group_msg_emoji_like")
             (group_id . "20001")
             (user_id . "10002")
             (message_id . ,message-id)
             (is_add . t)
             (likes . (((emoji_id . "178") (emoji_type . "1"))))))
          (stale-snapshot
           `((message_id . ,message-id)
             (message_type . "group")
             (chat_type . 2)
             (peer_uin . "20001")
             (group_id . "20001")
             (user_id . "10001")
             (time . 1710000001)
             (sender . ((user_id . "10001") (nickname . "Alice")))
             (message . ())
             (emoji_likes_list
              . (((emoji_id . "178")
                  (emoji_type . "1")
                  (likes_cnt . "2")
                  (is_clicked . "0"))))))
          (current-owner
           (qq-state-materialization-request-begin "group:20001")))
     (should-not
      (qq-state-apply-emoji-like-notice "group:20001" notice))
     (qq-state-merge-history
      "group:20001" (list stale-snapshot) current-owner)
     (should
      (= 3
         (alist-get
          'count
          (car (qq-state-message-reactions
                (car (qq-state-session-messages "group:20001")))))))
     ;; Re-materializing the same explicit base under the same owner remains
     ;; base 2 + one delta, rather than using the already-updated base 3.
     (qq-state-merge-history
      "group:20001" (list stale-snapshot) current-owner)
     (should
      (= 3
         (alist-get
          'count
          (car (qq-state-message-reactions
                (car (qq-state-session-messages "group:20001")))))))
     (should (qq-state-materialization-request-end current-owner))
     (should-not
      (gethash (cons "group:20001" message-id)
               qq-state--message-patch-journal))
     ;; A future owner starts after the notice.  Its base 3 must stay 3: the
     ;; old +1 is outside this request's observation window.
     (let* ((future-owner
             (qq-state-materialization-request-begin "group:20001"))
            (future-snapshot (copy-tree stale-snapshot)))
       (setf (alist-get 'likes_cnt
                        (car (alist-get 'emoji_likes_list future-snapshot)))
             "3")
       (qq-state-merge-history
        "group:20001" (list future-snapshot) future-owner)
       (should
        (= 3
           (alist-get
            'count
            (car (qq-state-message-reactions
                  (car (qq-state-session-messages "group:20001")))))))
       ;; A later authoritative snapshot remains free to advance to 4.
       (setf (alist-get 'likes_cnt
                        (car (alist-get 'emoji_likes_list future-snapshot)))
             "4")
       (qq-state-merge-history
        "group:20001" (list future-snapshot) future-owner)
       (should (qq-state-materialization-request-end future-owner)))
     (should
      (= 4
         (alist-get
          'count
          (car (qq-state-message-reactions
                (car (qq-state-session-messages "group:20001"))))))))))

(ert-deftest qq-state-reaction-window-does-not-replay-on-merged-current-base ()
  "A response omitting reactions keeps the already patched canonical base."
  (qq-test-with-reset
   (let* ((session-key "group:20001")
          (message-id "9007199254741004888")
          (base
           `((message_id . ,message-id)
             (message_type . "group")
             (chat_type . 2)
             (peer_uin . "20001")
             (group_id . "20001")
             (user_id . "10001")
             (time . 1710000001)
             (sender . ((user_id . "10001") (nickname . "Alice")))
             (message . ())
             (emoji_likes_list
              . (((emoji_id . "178") (likes_cnt . "2"))))))
          (without-reactions (copy-tree base)))
     (setq without-reactions
           (assq-delete-all 'emoji_likes_list without-reactions))
     (qq-state-merge-history session-key (list base))
     (let ((owner (qq-state-materialization-request-begin session-key)))
       (qq-state-apply-emoji-like-notice
        session-key
        `((notice_type . "group_msg_emoji_like")
          (group_id . "20001")
          (user_id . "10002")
          (message_id . ,message-id)
          (is_add . t)
          (likes . (((emoji_id . "178"))))))
       (qq-state-merge-history session-key (list without-reactions) owner)
       (should
        (= 3
           (alist-get
            'count
            (car (qq-state-message-reactions
                  (car (qq-state-session-messages session-key)))))))
       (should (qq-state-materialization-request-end owner))))))

(ert-deftest qq-state-pending-promotion-materializes-earlier-recall ()
  "The send response is a materialization boundary for a known snowflake."
  (qq-test-with-reset
   (let* ((message-id "9007199254741004887")
          (pending
           (qq-state-insert-pending-text-message "private:10001" "race"))
          (local-id (alist-get 'local-id pending)))
     (should-not (qq-state-apply-recall "private:10001" message-id))
     (qq-state-mark-pending-message-sent
      "private:10001" local-id message-id)
     (let ((message (car (qq-state-session-messages "private:10001"))))
       (should (equal (alist-get 'server-id message) message-id))
       (should (qq-state-message-recalled-p message))))))

(ert-deftest qq-state-pending-promotion-replays-only-owner-window-reaction ()
  "A send response applies a reaction observed while that send was active."
  (qq-test-with-reset
   (let* ((session-key "group:20001")
          (message-id "9007199254741004889")
          (pending
           (qq-state-insert-pending-text-message session-key "race"))
          (local-id (alist-get 'local-id pending))
          (owner (qq-state-materialization-request-begin session-key)))
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10002")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     (qq-state-mark-pending-message-sent
      session-key local-id message-id owner)
     (should
      (= 1
         (alist-get
          'count
          (car (qq-state-message-reactions
                (car (qq-state-session-messages session-key)))))))
     (should (qq-state-materialization-request-end owner)))))

(ert-deftest qq-state-send-response-replays-notice-before-live-promotion ()
  "A late send response applies a notice missed by ownerless live promotion."
  (qq-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (let* ((session-key "group:20001")
          (message-id "9007199254741004891")
          (pending
           (qq-state-insert-pending-text-message session-key "race"))
          (local-id (alist-get 'local-id pending))
          (owner (qq-state-materialization-request-begin session-key))
          (now (truncate (float-time))))
     ;; The reaction is ID-scoped, but the optimistic row does not know that
     ;; server ID yet, so the active send owner keeps it in the journal.
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10002")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     ;; The real websocket event wins the promotion race.  Use the production
     ;; live merge, including weak pending reconciliation and timeline rekey.
     (qq-state-merge-live-message
      `((post_type . "message_sent")
        (message_type . "group")
        (chat_type . 2)
        (peer_uin . "20001")
        (group_id . "20001")
        (message_id . ,message-id)
        (self_id . "90001")
        (user_id . "90001")
        (time . ,now)
        (sender . ((user_id . "90001") (nickname . "Me")))
        (raw_message . "race")
        (message . (((type . "text") (data . ((text . "race"))))))))
     (let ((promoted (car (qq-state-session-messages session-key))))
       (should (equal (alist-get 'local-id promoted) local-id))
       (should (equal (alist-get 'server-id promoted) message-id))
       (should-not (qq-state-message-reactions promoted)))
     ;; server-id was already installed by the live event, but the canonical
     ;; reaction watermark still proves that this owner-window delta is absent.
     (qq-state-mark-pending-message-sent
      session-key local-id message-id owner)
     (qq-state-mark-pending-message-sent
      session-key local-id message-id owner)
     (let* ((message (car (qq-state-session-messages session-key)))
            (reaction (car (qq-state-message-reactions message))))
       (should (equal (alist-get 'server-id message) message-id))
       (should (= 1 (alist-get 'count reaction)))
       (should (integerp
                (alist-get 'reaction-observation-token message))))
     (should (qq-state-materialization-request-end owner)))))

(ert-deftest qq-state-overlapping-reaction-owners-replay-selectively-and-prune ()
  "Overlapping owners replay their own windows and retain only shared work."
  (qq-test-with-reset
   (let* ((session-key "group:20001")
          (message-id "9007199254741004892")
          (journal-key (cons session-key message-id))
          (older-owner
           (qq-state-materialization-request-begin session-key)))
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10002")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     (let ((newer-owner
            (qq-state-materialization-request-begin session-key)))
       (qq-state-apply-emoji-like-notice
        session-key
        `((notice_type . "group_msg_emoji_like")
          (group_id . "20001")
          (user_id . "10003")
          (message_id . ,message-id)
          (is_add . t)
          (likes . (((emoji_id . "178"))))))
       (cl-labels
           ((snapshot
             (count)
             `((message_id . ,message-id)
               (message_type . "group")
               (chat_type . 2)
               (peer_uin . "20001")
               (group_id . "20001")
               (user_id . "10001")
               (time . 1710000001)
               (sender . ((user_id . "10001") (nickname . "Alice")))
               (message . ())
               (emoji_likes_list
                . (((emoji_id . "178")
                    (emoji_type . "1")
                    (likes_cnt . ,(number-to-string count))
                    (is_clicked . "0"))))))
            (reaction-count
             ()
             (alist-get
              'count
              (car (qq-state-message-reactions
                    (car (qq-state-session-messages session-key)))))))
         ;; The older response base predates both deltas; the newer response
         ;; base already includes the first.  Both converge to exactly two.
         (qq-state-merge-history
          session-key (list (snapshot 0)) older-owner)
         (should (= 2 (reaction-count)))
         (qq-state-merge-history
          session-key (list (snapshot 1)) newer-owner)
         (should (= 2 (reaction-count)))
         (should
          (= 2 (length (plist-get (gethash journal-key
                                           qq-state--message-patch-journal)
                                  :reaction-patches))))
         ;; Once the older window closes, its first-only delta is pruned while
         ;; the second remains available to the overlapping newer owner.
         (should (qq-state-materialization-request-end older-owner))
         (let ((retained
                (plist-get (gethash journal-key
                                    qq-state--message-patch-journal)
                           :reaction-patches)))
           (should (= 1 (length retained)))
           (should
            (< (plist-get newer-owner :start-token)
               (plist-get (car retained) :observation-token))))
         ;; Replaying the newer response is idempotent despite that retained
         ;; delta, because its explicit base resets the per-row watermark.
         (qq-state-merge-history
          session-key (list (snapshot 1)) newer-owner)
         (should (= 2 (reaction-count)))
         (should (qq-state-materialization-request-end newer-owner))
         (should-not
          (gethash journal-key qq-state--message-patch-journal)))))))

(ert-deftest qq-state-reaction-watermark-catches-up-before-newer-delta ()
  "A newer live delta first folds an older journaled delta in arrival order."
  (qq-test-with-reset
   (let* ((session-key "group:20001")
          (message-id "9007199254741004893")
          (owner (qq-state-materialization-request-begin session-key)))
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10002")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     (qq-state-merge-live-message
      (qq-state-test--group-message
       message-id (truncate (float-time)) "hello"))
     (should-not
      (qq-state-message-reactions
       (car (qq-state-session-messages session-key))))
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10003")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     (let* ((message (car (qq-state-session-messages session-key)))
            (reaction (car (qq-state-message-reactions message))))
       (should (= 2 (alist-get 'count reaction)))
       (should (= (qq-state-message-observation-token)
                  (alist-get 'reaction-observation-token message))))
     (should (qq-state-materialization-request-end owner)))))

(ert-deftest qq-state-send-response-does-not-replay-on-live-promoted-base ()
  "A late send response keeps reactions already applied to the promoted row."
  (qq-test-with-reset
   (let* ((session-key "group:20001")
          (message-id "9007199254741004890")
          (pending
           (qq-state-insert-pending-text-message session-key "race"))
          (local-id (alist-get 'local-id pending))
          (owner (qq-state-materialization-request-begin session-key)))
     ;; Model the websocket event winning the send-response race.
     (qq-state-mark-pending-message-sent session-key local-id message-id)
     (qq-state-apply-emoji-like-notice
      session-key
      `((notice_type . "group_msg_emoji_like")
        (group_id . "20001")
        (user_id . "10002")
        (message_id . ,message-id)
        (is_add . t)
        (likes . (((emoji_id . "178"))))))
     ;; The pending row is already server-backed and contains the +1.  The
     ;; request response carries no reaction snapshot base, so it must not
     ;; apply the same patch again.
     (qq-state-mark-pending-message-sent
      session-key local-id message-id owner)
     (should
      (= 1
         (alist-get
          'count
          (car (qq-state-message-reactions
                (car (qq-state-session-messages session-key)))))))
     (should (qq-state-materialization-request-end owner)))))

(ert-deftest qq-state-message-patches-reject-identity-contradictions ()
  (qq-test-with-reset
   (let ((message-id "9007199254741004883"))
     (qq-state-merge-live-message
      `((post_type . "message")
        (message_type . "private")
        (chat_type . 1)
        (peer_uin . "10001")
        (message_id . ,message-id)
        (user_id . "10001")
        (time . 1710000001)
        (sender . ((user_id . "10001") (nickname . "Alice")))
        (raw_message . "owned")
        (message . (((type . "text") (data . ((text . "owned"))))))))
     (let ((index-error
            (should-error
             (qq-state-apply-recall "group:20001" message-id))))
       (should (string-match-p "contradicts indexed session"
                               (error-message-string index-error))))
     (let ((group-error
            (should-error
             (qq-state-apply-emoji-like-notice
              "group:20001"
              `((notice_type . "group_msg_emoji_like")
                (group_id . "20002")
                (user_id . "10002")
                (message_id . ,message-id)
                (is_add . t)
                (likes . (((emoji_id . "178") (count . 1)))))))))
       (should (string-match-p "requires the explicit session group"
                               (error-message-string group-error)))))))

(ert-deftest qq-state-normalize-message-snapshot-does-not-consume-global-order ()
  (qq-test-with-reset
   (setq qq-state--message-order-counter 41)
   (let ((snapshot
          (qq-state-normalize-message-snapshot
           "group:20001"
           '((chat . ((kind . "group") (group_id . "20001")))
             (message_id . "9007199254741004884")
             (message_seq . "10001")
             (sent_at . 1710000001)
             (sender . ((user_id . "10001") (name . "Alice")))
             (outgoing . :false)
             (state . "live")
             (segments . (((kind . "text")
                           (payload . ((text . "snapshot"))))))
             (reactions . (((emoji_id . "178")
                            (emoji_type . "1")
                            (count . 2)
                            (chosen . t))))))))
     (should (= (alist-get 'order snapshot) 42))
     (should (= qq-state--message-order-counter 41))
     (should (equal (alist-get 'message-seq snapshot) "10001"))
     (should (equal (alist-get 'preview snapshot) "snapshot"))
     (should (equal (alist-get 'segments snapshot)
                    '(((type . "text") (data . ((text . "snapshot")))))))
     (should (equal (car (alist-get 'reactions snapshot))
                    '((emoji-id . "178") (emoji-type . "1")
                      (count . 2) (chosen-p . t))))
     (should-not (assq 'raw-event snapshot))
     (should-not (qq-state-session-messages "group:20001")))))

(ert-deftest qq-state-message-snapshot-rejects-contradictory-session ()
  (qq-test-with-reset
   (should-error
    (qq-state-normalize-message-snapshot
     "group:20001"
     '((chat . ((kind . "group") (group_id . "20002")))
       (message_id . "9007199254741004884")
       (message_seq . "10001")
       (sent_at . 1710000001)
       (sender . ((user_id . "10001") (name . "Alice")))
       (outgoing . :false)
       (state . "live")
       (segments)
       (reactions))))))

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
        (chat_type . 1)
        (peer_uin . "10001")
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
              (chat_type . 1)
              (peer_uin . "10001")
              (user_id . 10001)
              (time . 1710000001)
              (sender . ((user_id . 10001) (nickname . "A")))
              (raw_message . "a")
              (message . (((type . "text") (data . ((text . "a")))))))
            '((message_id . "90")
              (message_type . "private")
              (chat_type . 1)
              (peer_uin . "10001")
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
           (message_type . "group") (chat_type . 2) (peer_uin . "20001")
           (group_id . "20001")
           (user_id . "10001") (time . 1710000001) (message . ()))
          ((message_id . "9007199254741004002")
           (message_type . "group") (chat_type . 2) (peer_uin . "20001")
           (group_id . "20001")
           (user_id . "10001") (time . 1710000002) (message . ()))
          ((message_id . "9007199254741004003")
           (message_type . "group") (chat_type . 2) (peer_uin . "20001")
           (group_id . "20001")
           (user_id . "10001") (time . 1710000003) (message . ()))))
       (should (= sort-count 1))))))

(ert-deftest qq-state-message-reactions-normalize-history-snapshot ()
  (qq-test-with-reset
   (qq-state-merge-history
    "group:20001"
    '(((message_id . "9007199254741004001")
       (message_type . "group")
       (chat_type . 2)
       (peer_uin . "20001")
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
       (chat_type . 2)
       (peer_uin . "20001")
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
    "group:20001"
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
    "group:20001"
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
    "group:20001"
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
    "group:20001"
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
