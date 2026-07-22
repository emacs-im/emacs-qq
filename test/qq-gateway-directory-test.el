;;; qq-gateway-directory-test.el --- Tests for Gateway contacts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-directory)

(defconst qq-gateway-directory-test-capabilities
  '("contact.list_friends" "contact.list_groups"
    "contact.list_group_members")
  "Native contact capabilities exercised by directory tests.")

(defun qq-gateway-directory-test-account
    (&optional account-id generation uin uid)
  "Return an online test account with exact identity fields."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . "online")
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (generation . ,(or generation "7"))
    (challenge)
    (problem)))

(defun qq-gateway-directory-test-friends-result ()
  "Return one closed friend-list result."
  (copy-tree
   '((account_id . "slot-a")
    (generation . "7")
    (friends
     . (((uid . "u_alice")
         (uin . "9007199254740999")
         (category_id . 7)
         (nickname . "Alice")
         (remark . "A")
         (personal_sign . "hello")
         (qid . "alice-qid")
         (age . 20)
         (gender . 2))
        ((uid . "u_bob")
         (uin . "10003")
         (category_id . 3)
         (nickname . "Bob")
         (remark)
         (personal_sign)
         (qid)
         (age)
         (gender))))
    (categories
     . (((id . 7) (name . "Work") (member_count . 1) (sort_id . 20))
        ((id . 3) (name . "Other") (member_count . 1) (sort_id . 21)))))))

(defun qq-gateway-directory-test-groups-result ()
  "Return one closed joined-group result."
  (copy-tree
   '((account_id . "slot-a")
    (generation . "7")
    (groups
     . (((group_uin . "8209413637")
         (owner_uid . "u_self")
         (name . "Protocol Lab")
         (member_count . 3)
         (member_max . 500)
         (created_time . 1700000000)
         (description . "Native Gateway")
         (question)
         (announcement . "Welcome")
         (remark . "Lab")
         (last_speak_time . 1784700000)
         (latest_sequence . 123)))))))

(defun qq-gateway-directory-test-members-result ()
  "Return one closed group-member result."
  (copy-tree
   '((account_id . "slot-a")
    (generation . "7")
    (group_uin . "8209413637")
    (members
     . (((uid . "u_owner")
         (uin . "10001")
         (nickname . "Owner")
         (member_card . "Boss")
         (special_title)
         (level . 10)
         (permission . ((kind . "owner")))
         (join_timestamp . 1)
         (last_message_timestamp . 2)
         (shut_up_timestamp . 0))
        ((uid . "u_self")
         (uin . "10002")
         (nickname . "Self")
         (member_card . "Admin")
         (special_title . "Maintainer")
         (level . 9)
         (permission . ((kind . "admin")))
         (join_timestamp . 3)
         (last_message_timestamp . 4)
         (shut_up_timestamp . 0))
        ((uid . "u_unknown")
         (uin . "10003")
         (nickname . "Unknown")
         (member_card)
         (special_title)
         (level . 1)
         (permission . ((kind . "unknown") (code . 99)))
         (join_timestamp . 5)
         (last_message_timestamp . 6)
         (shut_up_timestamp . 0))))
    (member_count . 3)
    (member_list_change_sequence . 11)
    (member_card_sequence . 12))))

(defmacro qq-gateway-directory-test-with-state (&rest body)
  "Run BODY with isolated selected Gateway and directory state."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
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
         (qq-gateway-directory--request-counter 0)
         (qq-gateway-directory--active-requests
          (make-hash-table :test #'equal))
         (qq-gateway-directory--member-pages
          (make-hash-table :test #'equal))
         (qq-gateway-directory--cache-owner nil)
         (qq-gateway-transport--state 'ready)
         (qq-state-change-hook nil))
     (unwind-protect
         (progn
           (qq-state-reset)
           (qq-gateway--replace-accounts
            (list (qq-gateway-directory-test-account)) 'ready "gateway-test")
           (qq-gateway-message--ensure-projection-owner
            (qq-gateway-current-account-owner))
           ,@body)
       (qq-state-reset))))

(ert-deftest qq-gateway-directory-friends-project-exact-order-and-identities ()
  (qq-gateway-directory-test-with-state
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-gateway-directory-test-friends-result))
                   "request-friends")))
        (should
         (equal
          (qq-gateway-directory-refresh-friends
           (lambda (categories) (setq callback-value categories)))
          "request-friends"))
        (should (equal sent-method "contact.list_friends"))
        (should
         (equal sent-params
                '((account_id . "slot-a") (refresh . :false))))
        (should (equal (mapcar (lambda (category)
                                (alist-get 'category_id category))
                              callback-value)
                       '(7 3)))
        (should (equal (mapcar (lambda (friend)
                                (alist-get 'user_id friend))
                              (qq-state-friends))
                       '("9007199254740999" "10003")))
        (should (equal (alist-get 'category_name
                                  (qq-state-friend "9007199254740999"))
                       "Work"))
        (should (equal (gethash "u_alice"
                                qq-gateway-message--peer-uin-by-uid)
                       "9007199254740999"))
        (should (= (hash-table-count
                    qq-gateway-directory--active-requests)
                   0))))))

(ert-deftest qq-gateway-directory-friend-count-drift-is-preserved ()
  (qq-gateway-directory-test-with-state
    (let ((result (qq-gateway-directory-test-friends-result)) callback-value)
      (setf (alist-get 'member_count (car (alist-get 'categories result))) 2)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                    (funcall callback result)
                    "request-friends")))
        (qq-gateway-directory-refresh-friends
         (lambda (categories) (setq callback-value categories)))
        (should (= (alist-get 'member_count (car callback-value)) 2))
        (should (= (length (alist-get 'friends (car callback-value))) 1))
        (should (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-gateway-directory-friend-unknown-category-is-atomic ()
  (qq-gateway-directory-test-with-state
    (let ((result (qq-gateway-directory-test-friends-result)) failure)
      (setf (alist-get 'category_id (car (alist-get 'friends result))) 99)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-friends")))
        (qq-gateway-directory-refresh-friends
         nil (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "unknown category" failure))
        (should-not (qq-state-friend-categories-loaded-p))
        (should (= (hash-table-count
                    qq-gateway-message--peer-uin-by-uid)
                   0))))))

(ert-deftest qq-gateway-directory-groups-project-native-metadata ()
  (qq-gateway-directory-test-with-state
    (let (sent-params callback-value)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method params callback _errback &optional _early)
                   (setq sent-params params)
                   (funcall callback
                            (qq-gateway-directory-test-groups-result))
                   "request-groups")))
        (qq-gateway-directory-refresh-groups
         (lambda (groups) (setq callback-value groups)) nil t)
        (should
         (equal sent-params
                '((account_id . "slot-a") (refresh . t))))
        (should (= (length callback-value) 1))
        (let ((group (qq-state-group "8209413637")))
          (should (equal (alist-get 'group_name group) "Protocol Lab"))
          (should (equal (alist-get 'group_remark group) "Lab"))
          (should (equal (alist-get 'self_permission group) "owner"))
          (should (equal (alist-get 'latest_sequence group) "123"))
          (should (= (alist-get 'max_member_count group) 500)))))))

(ert-deftest qq-gateway-directory-members-map-and-enrich-group-ownership ()
  (qq-gateway-directory-test-with-state
    (qq-state-apply-groups
     '(((group_id . "8209413637")
        (group_name . "Protocol Lab")
        (member_count . 3)
        (max_member_count . 500))))
    (let (sent-method sent-params callback-value)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-gateway-directory-test-members-result))
                   "request-members")))
        (qq-gateway-directory-list-group-members
         "8209413637" (lambda (members) (setq callback-value members)))
        (should (equal sent-method "contact.list_group_members"))
        (should
         (equal sent-params
                '((account_id . "slot-a")
                  (group_uin . "8209413637")
                  (refresh . :false))))
        (should (equal (mapcar (lambda (member) (alist-get 'role member))
                               callback-value)
                       '("owner" "admin" nil)))
        (should (equal (alist-get 'native_permission (nth 2 callback-value))
                       '((kind . "unknown") (code . 99))))
        (let ((group (qq-state-group "8209413637")))
          (should (equal (alist-get 'owner_id group) "10001"))
          (should (equal (alist-get 'owner_uid group) "u_owner"))
          (should (equal (alist-get 'self_permission group) "admin")))
        (let ((page
               (qq-gateway-directory-group-member-page "8209413637")))
          (should (= (alist-get 'member_count page) 3))
          (should (= (length (alist-get 'members page)) 3)))
        (should (equal (gethash "u_unknown"
                                qq-gateway-message--peer-uin-by-uid)
                       "10003"))))))

(ert-deftest qq-gateway-directory-members-reject-numeric-identity ()
  (qq-gateway-directory-test-with-state
    (let ((result (qq-gateway-directory-test-members-result)) failure)
      (setf (alist-get 'uin (car (alist-get 'members result))) 10001)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-members")))
        (qq-gateway-directory-list-group-members
         "8209413637" #'ignore
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "uin must be exact decimal string" failure))
        (should-not
         (qq-gateway-directory-group-member-page "8209413637"))
        (should (= (hash-table-count
                    qq-gateway-message--peer-uin-by-uid)
                   0))))))

(ert-deftest qq-gateway-directory-member-count-drift-is-preserved ()
  (qq-gateway-directory-test-with-state
    (let ((result (qq-gateway-directory-test-members-result)) callback-value)
      (setf (alist-get 'member_count result) 4)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback result)
                   "request-members")))
        (qq-gateway-directory-list-group-members
         "8209413637" (lambda (members) (setq callback-value members)))
        (should (= (length callback-value) 3))
        (let ((page
               (qq-gateway-directory-group-member-page "8209413637")))
          (should (= (alist-get 'member_count page) 4))
          (should (= (length (alist-get 'members page)) 3)))))))

(ert-deftest qq-gateway-directory-newest-request-owns-projection ()
  (qq-gateway-directory-test-with-state
    (let (calls first-failure callbacks)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq calls (append calls (list (cons callback errback))))
                   (format "request-%d" (length calls)))))
        (qq-gateway-directory-refresh-friends
         (lambda (_) (push 'first callbacks))
         (lambda (_body reason) (setq first-failure reason)))
        (qq-gateway-directory-refresh-friends
         (lambda (_) (push 'second callbacks)) #'ignore t)
        (funcall (car (nth 0 calls))
                 (qq-gateway-directory-test-friends-result))
        (should (string-match-p "superseded" first-failure))
        (should-not (qq-state-friend-categories-loaded-p))
        (funcall (car (nth 1 calls))
                 (qq-gateway-directory-test-friends-result))
        (should (equal callbacks '(second)))
        (should (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-gateway-directory-account-switch-rejects-stale-response ()
  (qq-gateway-directory-test-with-state
    (let (response-callback failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-directory-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq response-callback callback)
                   "request-friends")))
        (qq-gateway-directory-refresh-friends
         nil (lambda (_body reason) (setq failure reason)))
        (qq-gateway--upsert-account
         (qq-gateway-directory-test-account
          "slot-b" "11" "10003" "u_other_self")
         'changed)
        (qq-gateway-account-select "slot-b")
        (funcall response-callback
                 (qq-gateway-directory-test-friends-result))
        (should (string-match-p "changed owner" failure))
        (should-not (qq-state-friend-categories-loaded-p))))))

(ert-deftest qq-gateway-directory-generation-change-clears-member-cache ()
  (qq-gateway-directory-test-with-state
    (setq qq-gateway-directory--cache-owner '("slot-a" . "7"))
    (puthash "8209413637" '((members . test))
             qq-gateway-directory--member-pages)
    (puthash 'friends 'old-token qq-gateway-directory--active-requests)
    (qq-gateway--upsert-account
     (qq-gateway-directory-test-account
      "slot-a" "8" "10002" "u_self")
     'changed)
    (qq-gateway-directory--handle-account-context 'changed "slot-a")
    (should (equal qq-gateway-directory--cache-owner '("slot-a" . "8")))
    (should (= (hash-table-count qq-gateway-directory--member-pages) 0))
    (should (= (hash-table-count qq-gateway-directory--active-requests) 0))))

(provide 'qq-gateway-directory-test)

;;; qq-gateway-directory-test.el ends here
