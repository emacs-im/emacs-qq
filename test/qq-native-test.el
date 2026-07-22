;;; qq-native-test.el --- Tests for the native operation boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq)

(defun qq-native-test-message-event ()
  "Return one valid private native service message event."
  '((account_id . "slot-a")
    (generation . "7")
    (message
     . ((message_id . "7348923749823749823")
        (sent_at . 1784700000)
        (sender . ((uin . "10001") (uid . "u_peer")))
        (recipient . ((uin . "10002") (uid . "u_self")))
        (conversation . ((kind . "private") (name . "Peer")))
        (sequence . "9007199254740999")
        (client_sequence . "9007199254741001")
        (random . 7)
        (message_type . 166)
        (sub_type . 0)
        (segments . (((kind . "text")
                      (payload . ((text . "hello"))))))))))

(defun qq-native-test-account ()
  "Return one selected online QQ account."
  '((account_id . "slot-a")
    (label . "Primary")
    (phase . "online")
    (uin . "10002")
    (uid . "u_self")
    (generation . "7")
    (challenge)
    (problem)))

(defun qq-native-test-prepared-image (attachment-id resource-id)
  "Return the identities reported for one prepared image."
  `((attachment_id . ,attachment-id)
    (resource_id . ,resource-id)))

(defconst qq-native-test-members
  '(((user_id . "9007199254740999")
     (uid . "u_alice")
     (nickname . "Alice")
     (card . "Maintainer")
     (remark)
     (qid . "alice-qid")
     (title)
     (role . "admin")
     (robot)
     (group_id . "8209413637"))
    ((user_id . "10003")
     (uid . "u_bob")
     (nickname . "Bob")
     (card)
     (remark . "Friend")
     (qid)
     (title)
     (role . "member")
     (robot)
     (group_id . "8209413637")))
  "Mapped Gateway members used by backend search tests.")

(ert-deftest qq-native-connect-and-disconnect-use-only-native-transport ()
  (let (calls)
    (cl-letf (((symbol-function 'qq-native-activate)
               (lambda () (push 'activate calls)))
              ((symbol-function 'qq-gateway-transport-start)
               (lambda () (push 'native-start calls)))
              ((symbol-function 'qq-gateway-transport-stop)
               (lambda () (push 'native-stop calls))))
      (qq-native-connect)
      (qq-native-disconnect))
    (should (equal (nreverse calls) '(activate native-start native-stop)))))

(ert-deftest qq-native-v1-onebot-actions-fail-closed ()
  (let (sent)
    (cl-letf (((symbol-function 'qq-transport-send)
               (lambda (&rest arguments) (setq sent arguments))))
      (should-error
       (qq-api-call "get_login_info" nil #'ignore) :type 'user-error)
      (should-not sent))))

(ert-deftest qq-native-v1-onebot-events-cannot-write-native-state ()
  (let (bootstrap status)
    (cl-letf (((symbol-function 'qq-api-bootstrap)
               (lambda () (setq bootstrap t)))
              ((symbol-function 'qq-state-set-connection-status)
               (lambda (value) (setq status value))))
      (qq-api-handle-event
       '((post_type . "meta_event")
         (meta_event_type . "lifecycle")
         (sub_type . "connect")))
      (should-not bootstrap)
      (should-not status))))

(ert-deftest qq-native-events-project-the-selected-account-generation ()
  (let ((qq-gateway--accounts (make-hash-table :test #'equal))
        (qq-gateway--account-order nil)
        (qq-gateway--current-account-id nil)
        (qq-gateway-message--projection-owner nil)
        (qq-gateway-message--peer-uin-by-uid
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-recalls
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-reactions
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-sends
         (make-hash-table :test #'equal))
        (qq-gateway-message--live-frontiers
         (make-hash-table :test #'equal))
        (qq-gateway-accounts-changed-hook nil)
        (qq-gateway-current-account-changed-hook nil)
        observed)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-gateway--replace-accounts
           (list (qq-native-test-account)) 'ready "test-gateway")
          (let ((qq-gateway-message-event-hook
                 (list (lambda (event _data) (setq observed event)))))
            (qq-gateway-message--handle-event
             "message.received" (qq-native-test-message-event)))
          (should (equal observed "message.received"))
          (should (qq-state-session "private:10001"))
          (should (equal qq-gateway-message--projection-owner
                         '("slot-a" . "7"))))
      (qq-state-reset))))

(ert-deftest qq-native-activation-claims-selected-account-projection ()
  (let (activated bootstrapped)
    (cl-letf (((symbol-function 'qq-gateway-message-activate-projection)
               (lambda () (setq activated t)))
              ((symbol-function 'qq-native--maybe-bootstrap)
               (lambda (&rest _) (setq bootstrapped t))))
      (should (qq-native-activate))
      (should activated)
      (should bootstrapped))))

(ert-deftest qq-native-directory-request-retains-cancellation-token ()
  (let (sent cancelled)
    (cl-letf (((symbol-function 'qq-gateway-directory-refresh-friends)
               (lambda (callback errback refresh)
                 (setq sent (list callback errback refresh))
                 "gateway-request"))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (setq cancelled token))))
      (let ((request
             (qq-native-refresh-friend-categories #'ignore #'ignore t)))
        (should (qq-native-request-p request))
        (should (equal (qq-native-request-token request) "gateway-request"))
        (should (eq (nth 0 sent) #'ignore))
        (should (eq (nth 1 sent) #'ignore))
        (should (eq (nth 2 sent) t))
        (qq-native-cancel-request request)
        (should (equal cancelled "gateway-request"))))))

(ert-deftest qq-native-group-profile-uses-owned-directory-state ()
  (let (profile refreshed)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-state-apply-groups
           '(((group_id . "8209413637")
              (group_name . "Protocol Lab")
              (group_remark . "Lab")
              (member_count . 3)
              (max_member_count . 500)
              (created_at . 1700000000)
              (description . "Native Gateway")
              (announcement . "Welcome")
              (self_permission . "owner"))))
          (cl-letf (((symbol-function 'qq-native-refresh-joined-groups)
                     (lambda (&rest _arguments) (setq refreshed t))))
            (should-not
             (qq-native-get-group
              "8209413637" (lambda (value) (setq profile value))))
            (should-not refreshed)
            (should (equal (alist-get 'group_id profile) "8209413637"))
            (should (equal (alist-get 'name profile) "Protocol Lab"))
            (should (= (alist-get 'member_count profile) 3))))
      (qq-state-reset))))

(ert-deftest qq-native-group-settings-update-shared-directory-after-receipt ()
  (let (calls callbacks)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-state-apply-groups
           '(((group_id . "8209413637")
              (group_name . "Old")
              (group_remark . "Old Remark")
              (member_count . 3)
              (max_member_count . 500))))
          (cl-letf
              (((symbol-function 'qq-gateway-directory-set-group-name)
                (lambda (group-id value callback &optional _errback)
                  (push (list 'name group-id value) calls)
                  (funcall callback '((name . "New")))
                  "name-request"))
               ((symbol-function 'qq-gateway-directory-set-group-remark)
                (lambda (group-id value callback &optional _errback)
                  (push (list 'remark group-id value) calls)
                  (funcall callback '((remark . "")))
                  "remark-request"))
               ((symbol-function 'qq-gateway-directory-set-group-whole-mute)
                (lambda (group-id value callback &optional _errback)
                  (push (list 'mute group-id value) calls)
                  (funcall callback '((enabled . t)))
                  "mute-request"))
               ((symbol-function 'qq-gateway-directory-set-group-pinned)
                (lambda (group-id value callback &optional _errback)
                  (push (list 'pinned group-id value) calls)
                  (funcall callback '((pinned . :false)))
                  "pinned-request"))
               ((symbol-function 'qq-gateway-directory-clock-in-group)
                (lambda (group-id callback &optional _errback)
                  (push (list 'clock-in group-id) calls)
                  (funcall callback '((title . "今日已打卡")))
                  "clock-in-request")))
            (let ((name-request
                   (qq-native-set-group-name
                    "8209413637" "New"
                    (lambda (_receipt) (push 'name callbacks))))
                  (remark-request
                   (qq-native-set-group-remark
                    "8209413637" ""
                    (lambda (_receipt) (push 'remark callbacks))))
                  (mute-request
                   (qq-native-set-group-whole-mute
                    "8209413637" t
                    (lambda (_receipt) (push 'mute callbacks))))
                  (pinned-request
                   (qq-native-set-group-pinned
                    "8209413637" nil
                    (lambda (_receipt) (push 'pinned callbacks))))
                  (clock-in-request
                   (qq-native-clock-in-group
                    "8209413637"
                    (lambda (_receipt) (push 'clock-in callbacks)))))
              (should (equal (qq-native-request-token name-request)
                             "name-request"))
              (should (equal (qq-native-request-token remark-request)
                             "remark-request"))
              (should (equal (qq-native-request-token mute-request)
                             "mute-request"))
              (should (equal (qq-native-request-token pinned-request)
                             "pinned-request"))
              (should (equal (qq-native-request-token clock-in-request)
                             "clock-in-request"))))
          (let ((group (qq-state-group "8209413637")))
            (should (equal (alist-get 'group_name group) "New"))
            (should-not (alist-get 'group_remark group))
            (should (eq (alist-get 'pinned group) :false)))
          (should (equal (sort callbacks
                               (lambda (left right)
                                 (string< (symbol-name left)
                                          (symbol-name right))))
                         '(clock-in mute name pinned remark)))
          (should
           (equal (nreverse calls)
                  '((name "8209413637" "New")
                    (remark "8209413637" "")
                    (mute "8209413637" t)
                    (pinned "8209413637" nil)
                    (clock-in "8209413637")))))
      (qq-state-reset))))

(ert-deftest qq-native-friend-pinned-routes-exact-uin ()
  (let (called callback-value)
    (cl-letf (((symbol-function 'qq-gateway-directory-set-friend-pinned)
               (lambda (user-id pinned callback &optional _errback)
                 (setq called (list user-id pinned))
                 (funcall callback `((friend_uin . ,user-id) (pinned . t)))
                 "friend-pin")))
      (let ((request
             (qq-native-set-friend-pinned
              "9007199254740999" t
              (lambda (receipt) (setq callback-value receipt)))))
        (should (equal (qq-native-request-token request) "friend-pin"))))
    (should (equal called '("9007199254740999" t)))
    (should (eq (alist-get 'pinned callback-value) t))))

(ert-deftest qq-native-presence-targets-selected-account ()
  (let (called callback-value)
    (cl-letf (((symbol-function 'qq-gateway-current-account-owner)
               (lambda () '("slot-a" . "7")))
              ((symbol-function 'qq-gateway-account-set-presence)
               (lambda (account-id presence callback &optional _errback)
                 (setq called (list account-id presence))
                 (funcall callback
                          `((account_id . ,account-id)
                            (generation . "7")
                            (presence . ,presence)))
                 "presence")))
      (let* ((presence '((kind . "away")))
             (request
              (qq-native-set-presence
               presence (lambda (receipt) (setq callback-value receipt)))))
        (should (equal (qq-native-request-token request) "presence"))
        (should (equal called (list "slot-a" presence)))
        (should (equal (alist-get 'presence callback-value) presence))))))

(ert-deftest qq-native-group-leave-converges-loaded-state ()
  (let ((group-id "8209413637") called callback-value)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-state-apply-groups
           `(((group_id . ,group-id) (group_name . "Leave"))
             ((group_id . "20002") (group_name . "Keep"))))
          (cl-letf (((symbol-function 'qq-gateway-directory-leave-group)
                     (lambda (candidate callback &optional _errback)
                       (setq called candidate)
                       (funcall callback `((group_uin . ,candidate)))
                       "leave")))
            (let ((request
                   (qq-native-leave-group
                    group-id
                    (lambda (receipt) (setq callback-value receipt)))))
              (should (equal (qq-native-request-token request) "leave"))))
          (should (equal called group-id))
          (should callback-value)
          (should-not (qq-state-group group-id))
          (should (qq-state-group "20002")))
      (qq-state-reset))))

(ert-deftest qq-native-group-at-all-query-routes-wide-group-uin ()
  (let (called callback-value)
    (cl-letf (((symbol-function
               'qq-gateway-directory-get-group-at-all-remaining)
               (lambda (candidate callback &optional _errback)
                 (setq called candidate)
                 (funcall callback
                          `((group_uin . ,candidate)
                            (can_at_all . t)
                            (remain_at_all_count_for_uin . 3)
                            (remain_at_all_count_for_group . 9)))
                 "at-all")))
      (let ((request
             (qq-native-get-group-at-all-remaining
              "8209413637"
              (lambda (receipt) (setq callback-value receipt)))))
        (should (equal (qq-native-request-token request) "at-all"))))
    (should (equal called "8209413637"))
    (should (= (alist-get 'remain_at_all_count_for_uin callback-value) 3))
    (should (= (alist-get 'remain_at_all_count_for_group callback-value) 9))))

(ert-deftest qq-native-group-leave-does-not-invent-unloaded-directory ()
  (unwind-protect
      (progn
        (qq-state-reset)
        (cl-letf (((symbol-function 'qq-gateway-directory-leave-group)
                   (lambda (_group-id callback &optional _errback)
                     (funcall callback '((status . "ok")))
                     "leave")))
          (qq-native-leave-group "8209413637"))
        (should-not (qq-state-groups-loaded-p)))
    (qq-state-reset)))

(ert-deftest qq-native-group-member-settings-route-wide-native-identities ()
  (let (calls callbacks)
    (cl-letf
        (((symbol-function 'qq-gateway-directory-set-group-member-card)
          (lambda (group-id user-id value callback &optional _errback)
            (push (list 'card group-id user-id value) calls)
            (funcall callback '((card . "Ferris")))
            "card-request"))
         ((symbol-function
           'qq-gateway-directory-set-group-member-special-title)
          (lambda (group-id user-id value callback &optional _errback)
            (push (list 'title group-id user-id value) calls)
            (funcall callback '((special_title . "Maintainer")))
            "title-request"))
         ((symbol-function 'qq-gateway-directory-kick-group-member)
          (lambda (group-id user-id reject callback &optional _errback)
            (push (list 'kick group-id user-id reject) calls)
            (funcall callback '((reject_add_request . t)))
            "kick-request")))
      (let ((card-request
             (qq-native-set-group-member-card
              "8209413637" "9007199254741001" "Ferris"
              (lambda (_receipt) (push 'card callbacks))))
            (title-request
             (qq-native-set-group-member-special-title
              "8209413637" "9007199254741001" "Maintainer"
              (lambda (_receipt) (push 'title callbacks))))
            (kick-request
             (qq-native-kick-group-member
              "8209413637" "9007199254741001" t
              (lambda (_receipt) (push 'kick callbacks)))))
        (should (equal (qq-native-request-token card-request)
                       "card-request"))
        (should (equal (qq-native-request-token title-request)
                       "title-request"))
        (should (equal (qq-native-request-token kick-request)
                       "kick-request"))))
    (should (equal (nreverse calls)
                   '((card "8209413637" "9007199254741001" "Ferris")
                     (title "8209413637" "9007199254741001"
                            "Maintainer")
                     (kick "8209413637" "9007199254741001" t))))
    (should (equal (sort callbacks
                         (lambda (left right)
                           (string< (symbol-name left) (symbol-name right))))
                   '(card kick title)))
    (should (qq-native-group-id-p "8209413637"))
    (should (qq-native-user-id-p "9007199254741001"))))

(ert-deftest qq-native-member-search-filters-cached-exact-ids ()
  (let (result fetched)
    (cl-letf (((symbol-function 'qq-gateway-directory-group-member-page)
               (lambda (_group-id)
                 `((members . ,(copy-tree qq-native-test-members)))))
              ((symbol-function 'qq-gateway-directory-list-group-members)
               (lambda (&rest _) (setq fetched t))))
      (should-not
       (qq-native-search-group-members
        "8209413637" "alice" (lambda (members) (setq result members)) nil 1))
      (should-not fetched)
      (should (= (length result) 1))
      (should (equal (alist-get 'user_id (car result))
                     "9007199254740999"))
      (should (stringp (alist-get 'user_id (car result)))))))

(ert-deftest qq-native-member-search-fetches-on-cache-miss ()
  (let (callback result)
    (cl-letf (((symbol-function 'qq-gateway-directory-group-member-page)
               (lambda (_group-id) nil))
              ((symbol-function 'qq-gateway-directory-list-group-members)
               (lambda (_group-id success _errback &optional _refresh)
                 (setq callback success)
                 "member-request")))
      (let ((request
             (qq-native-search-group-members
              "8209413637" "friend"
              (lambda (members) (setq result members)) nil 200)))
        (should (equal (qq-native-request-token request) "member-request"))
        (funcall callback (copy-tree qq-native-test-members))
        (should (= (length result) 1))
        (should (equal (alist-get 'user_id (car result)) "10003"))))))

(ert-deftest qq-native-send-routes-closed-segments ()
  (let (sent)
    (cl-letf (((symbol-function 'qq-gateway-message-send)
               (lambda (session-key segments &optional raw callback errback)
                 (setq sent (list session-key segments raw callback errback))
                 "send-request")))
      (let ((segments
             '(((type . "reply")
                (data . ((id . "7348923749823749823"))))
               ((type . "at")
                (data . ((qq . "10002") (name . "Alice"))))
               ((type . "face") (data . ((id . "178"))))
               ((type . "text") (data . ((text . "hello")))))))
        (should
         (equal
          (qq-native-send-message
           "group:8209413637" segments "optimistic")
          "send-request"))
        (should (equal (nth 0 sent) "group:8209413637"))
        (should (eq (nth 1 sent) segments))
        (should (equal (nth 2 sent) "optimistic"))))))

(ert-deftest qq-native-local-images-finish-concurrently-but-send-in-draft-order ()
  (let ((path-a (make-temp-file "qq-native-image-a-" nil ".png" "aaa"))
        (path-b (make-temp-file "qq-native-image-b-" nil ".png" "bbb"))
        (owner '("slot-a" . "7"))
        staged sent released-resources)
    (unwind-protect
        (let ((segments
               `(((type . "text") (data . ((text . "before"))))
                 ((type . "image")
                  (data . ((file . ,path-a)
                           (summary . "first")
                           (sub_type . 0))))
                 ((type . "text") (data . ((text . "between"))))
                 ((type . "image")
                  (data . ((file . ,path-b)
                           (summary . "second")
                           (sub_type . 1)))))))
          (cl-letf
              (((symbol-function 'qq-gateway-current-account-owner)
                (lambda () owner))
               ((symbol-function
                 'qq-gateway-attachment-stage-and-prepare-image)
                (lambda (session path summary sub-type callback errback)
                  (let ((operation
                         (qq-gateway-attachment-operation-create
                          :active-p t)))
                    (push (list session path summary sub-type callback
                                errback operation)
                          staged)
                    operation)))
               ((symbol-function 'qq-native--release-send-resource)
                (lambda (resource-id)
                  (push resource-id released-resources)))
               ((symbol-function 'qq-gateway-message-send)
                (lambda (session ready-segments
                                 &optional raw callback errback optimistic)
                  (setq sent (list session ready-segments raw callback
                                   errback optimistic))
                  "send-request")))
            (let ((request
                   (qq-native-send-message
                    "group:8209413637" segments "optimistic")))
              (should (qq-native-request-p request))
              (should (= (length staged) 2))
              (let* ((entry-b (seq-find (lambda (entry)
                                          (equal (nth 1 entry) path-b))
                                        staged))
                     (operation-b (nth 6 entry-b)))
                (setf (qq-gateway-attachment-operation-active-p operation-b)
                      nil)
                (funcall
                 (nth 4 entry-b)
                 (qq-native-test-prepared-image
                  "att-bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
                  "res-image-b")))
              (should-not sent)
              (let* ((entry-a (seq-find (lambda (entry)
                                          (equal (nth 1 entry) path-a))
                                        staged))
                     (operation-a (nth 6 entry-a)))
                (setf (qq-gateway-attachment-operation-active-p operation-a)
                      nil)
                (funcall
                 (nth 4 entry-a)
                 (qq-native-test-prepared-image
                  "att-aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                  "res-image-a")))
              (should (equal (qq-native-request-token request) "send-request"))
              (should (equal (car sent) "group:8209413637"))
              (should
               (equal
                (nth 1 sent)
                '(((type . "text") (data . ((text . "before"))))
                  ((type . "image")
                   (data
                    . ((attachment_id
                        . "att-aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"))))
                  ((type . "text") (data . ((text . "between"))))
                  ((type . "image")
                   (data
                    . ((attachment_id
                        . "att-bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")))))))
              (should (equal (nth 2 sent) "optimistic"))
              (should (equal (nth 5 sent) segments))
              (should-not
               (string-match-p "qq-native-image-"
                               (prin1-to-string (nth 1 sent))))
              (should
               (equal (sort released-resources #'string<)
                      '("res-image-a" "res-image-b"))))))
      (delete-file path-a)
      (delete-file path-b))))

(ert-deftest qq-native-local-image-cancel-stops-before-message-dispatch ()
  (let ((path (make-temp-file "qq-native-image-cancel-" nil ".png" "abc"))
        (owner '("slot-a" . "7"))
        operation sent)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-owner)
              (lambda () owner))
             ((symbol-function
               'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (&rest _arguments)
                (setq operation
                      (qq-gateway-attachment-operation-create :active-p t))))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (let ((request
                 (qq-native-send-message
                  "private:10001"
                  `(((type . "image") (data . ((file . ,path))))))))
            (should (qq-gateway-attachment-operation-active-p operation))
            (qq-native-cancel-request request)
            (should-not
             (qq-gateway-attachment-operation-active-p operation))
            (should-not sent)))
      (delete-file path))))

(ert-deftest qq-native-local-image-generation-drift-releases-completion ()
  (let ((path (make-temp-file "qq-native-image-owner-" nil ".png" "abc"))
        (owner '("slot-a" . "7"))
        operation ready-callback sent failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-owner)
              (lambda () owner))
             ((symbol-function
               'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (_session _path _summary _sub-type callback _errback)
                (setq ready-callback callback
                      operation
                      (qq-gateway-attachment-operation-create :active-p t))
                operation))
             ((symbol-function 'qq-native--release-send-resource)
              (lambda (resource-id) (push resource-id released-resources)))
             ((symbol-function 'qq-native--release-send-attachment)
              (lambda (attachment-id)
                (push attachment-id released-attachments)))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (qq-native-send-message
           "private:10001"
           `(((type . "image") (data . ((file . ,path)))))
           nil nil
           (lambda (body reason) (setq failure (list body reason))))
          (setq owner '("slot-a" . "8"))
          (setf (qq-gateway-attachment-operation-active-p operation) nil)
          (funcall
           ready-callback
           (qq-native-test-prepared-image
            "att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"
            "res-image-owner"))
          (should-not sent)
          (should (equal (cadr failure)
                         "QQ account generation changed while preparing images"))
          (should (equal released-resources '("res-image-owner")))
          (should
           (equal released-attachments
                  '("att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"))))
      (delete-file path))))

(ert-deftest qq-native-url-only-image-fails-before-staging ()
  (let (staged sent)
    (cl-letf (((symbol-function
                'qq-gateway-attachment-stage-and-prepare-image)
               (lambda (&rest _arguments) (setq staged t)))
              ((symbol-function 'qq-gateway-message-send)
               (lambda (&rest _arguments) (setq sent t))))
      (should-error
       (qq-native-send-message
        "private:10001"
        '(((type . "image")
           (data . ((url . "https://example.invalid/changeable.png"))))))
       :type 'user-error)
      (should-not staged)
      (should-not sent))))

(ert-deftest qq-native-poke-routes-exact-target ()
  (let (call)
    (cl-letf (((symbol-function 'qq-gateway-message-send-poke)
               (lambda (session-key target-id &optional callback errback)
                 (setq call (list session-key target-id callback errback))
                 "poke-request")))
      (should
       (equal (qq-native-send-poke "group:8209413637" "10002")
              "poke-request"))
      (should (equal (nth 0 call) "group:8209413637"))
      (should (equal (nth 1 call) "10002"))
      (should (functionp (nth 3 call))))))

(ert-deftest qq-native-reaction-routes-whole-message ()
  (let (call)
    (cl-letf (((symbol-function 'qq-gateway-message-set-reaction)
               (lambda (message emoji-id set &optional callback errback)
                 (setq call (list message emoji-id set callback errback))
                 "reaction-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (should
         (equal (qq-native-set-message-reaction message "178" t)
                "reaction-request"))
        (should (eq (nth 0 call) message))
        (should (equal (nth 1 call) "178"))
        (should (eq (nth 2 call) t))
        (should (functionp (nth 4 call)))))))

(ert-deftest qq-native-essence-routes-whole-message ()
  (let (call)
    (cl-letf (((symbol-function 'qq-gateway-message-set-essence)
               (lambda (message set &optional callback errback)
                 (setq call (list message set callback errback))
                 "essence-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999")
               (native-random . 7))))
        (should
         (equal (qq-native-set-message-essence message t)
                "essence-request"))
        (should (eq (nth 0 call) message))
        (should (eq (nth 1 call) t))
        (should (functionp (nth 3 call)))))))

(ert-deftest qq-native-todo-routes-whole-message-and-operation ()
  (let (calls)
    (cl-letf (((symbol-function 'qq-gateway-message-set-todo)
               (lambda (message operation &optional callback errback)
                 (push (list message operation callback errback) calls)
                 (format "todo-%s" operation))))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (dolist (operation '(set complete cancel))
          (should
           (equal (qq-native-set-message-todo message operation)
                  (format "todo-%s" operation))))
        (dolist (call calls)
          (should (eq (nth 0 call) message))
          (should (functionp (nth 3 call)))))
    (should-error
     (qq-native-set-message-todo
      '((session-key . "group:8209413637")
        (server-id . "7348923749823749823"))
     'finish)
     :type 'user-error))))

(ert-deftest qq-native-poke-recall-routes-whole-message ()
  (let (call)
    (cl-letf (((symbol-function 'qq-state-poke-message-p) (lambda (_message) t))
              ((symbol-function 'qq-state-poke-recall-reference)
               (lambda (_message) '((message_id . "7348923749823749823"))))
              ((symbol-function 'qq-gateway-message-recall-poke)
               (lambda (message &optional callback errback)
                 (setq call (list message callback errback))
                 "poke-recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (segments . (((type . "poke")))))))
        (should (equal (qq-native-recall-poke message)
                       "poke-recall-request"))
        (should (eq (car call) message))
        (should (functionp (nth 2 call)))))))

(ert-deftest qq-native-recall-dispatches-the-owned-native-message ()
  (let (call)
    (cl-letf (((symbol-function 'qq-gateway-message-recall)
               (lambda (session-key message &optional callback errback)
                 (setq call (list session-key message callback errback))
                 "recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "99"))))
        (should
         (equal (qq-native-recall-message message)
                "recall-request"))
        (should (equal (car call) "group:8209413637"))
        (should (eq (cadr call) message))))))

(ert-deftest qq-native-history-frontier-prefers-newer-live-sequence ()
  (progn
    (cl-letf (((symbol-function 'qq-state-group)
               (lambda (_group-id) '((latest_sequence . "9007199254740999"))))
              ((symbol-function 'qq-gateway-message-live-frontier)
               (lambda (_session-key)
                 '((message_id . "7348923749823749823")
                   (sequence . "9007199254741001")))))
      (let ((frontier
             (qq-native-history-frontier "group:8209413637")))
        (should (equal (plist-get frontier :sequence)
                       "9007199254741001"))
        (should (equal (plist-get frontier :message-id)
                       "7348923749823749823"))
        (should (eq (plist-get frontier :source) 'live-event))))))

(ert-deftest qq-native-history-ranges-stay-exact-and-bounded ()
  (should
   (equal (qq-native-history-range-before
           "9007199254741000" 20)
          '("9007199254740980" . "9007199254740999")))
  (should
   (equal (qq-native-history-range-after
           "9007199254740999" 20 "9007199254741005")
          '("9007199254741000" . "9007199254741005")))
  (should-not (qq-native-history-range-before "0" 20))
  (should-not
   (qq-native-history-range-after "100" 20 "100")))

(ert-deftest qq-native-latest-history-uses-authoritative-range ()
  (let (sent properties callback-meta)
    (cl-letf (((symbol-function 'qq-native-history-frontier)
               (lambda (_session-key)
                 '(:sequence "9007199254741005" :authoritative-p t)))
              ((symbol-function 'qq-native-fetch-history-range)
               (lambda (_session start end callback _errback metadata)
                 (setq sent (cons start end) properties metadata)
                 (funcall callback '(:message-count 0 :batch-message-ids nil))
                 (qq-native-request-create :token "history-request"))))
      (let ((request
             (qq-native-fetch-latest-history
              "group:8209413637"
              (lambda (meta) (setq callback-meta meta)) nil 20)))
        (should (qq-native-request-p request))
        (should (equal sent
                       '("9007199254740986" . "9007199254741005")))
        (should (eq (plist-get properties :history-at-latest-p) t))
        (should (= (plist-get callback-meta :message-count) 0))))))

(ert-deftest qq-native-private-latest-never-guesses-a-sequence ()
  (let (callback-meta fetched)
    (cl-letf (((symbol-function 'qq-native-history-frontier)
               (lambda (_session-key)
                 '(:unavailable-reason private-latest-sequence)))
              ((symbol-function 'qq-native-fetch-history-range)
               (lambda (&rest _) (setq fetched t))))
      (should-not
       (qq-native-fetch-latest-history
        "private:10001" (lambda (meta) (setq callback-meta meta))))
      (should-not fetched)
      (should (eq (plist-get callback-meta :history-frontier-unavailable)
                  'private-latest-sequence)))))

(ert-deftest qq-native-around-requires-cached-exact-sequence ()
  (let (range failure)
    (cl-letf (((symbol-function 'qq-state-session-messages)
               (lambda (_session-key)
                 '(((server-id . "7348923749823749823")
                    (message-seq . "9007199254740999")))))
              ((symbol-function 'qq-native-fetch-history-range)
               (lambda (_session start end _callback _errback _properties)
                 (setq range (cons start end))
                 "history-request")))
      (should
       (equal
        (qq-native-fetch-history-around
         "private:10001" "7348923749823749823" #'ignore nil 20)
        "history-request"))
      (should
       (equal range
              '("9007199254740990" . "9007199254741009")))
      (qq-native-fetch-history-around
       "private:10001" "missing" #'ignore
       (lambda (_body reason) (setq failure reason)) 20)
      (should (string-match-p "cached message" failure)))))

(ert-deftest qq-native-bootstrap-coalesces-one-owner ()
  (let ((qq-native--bootstrap-owner nil)
        (qq-native--bootstrap-pending 0)
        calls)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
              ((symbol-function 'qq-gateway-current-account)
               (lambda () (qq-native-test-account)))
              ((symbol-function 'qq-gateway-current-account-owner)
               (lambda () '("slot-a" . "7")))
              ((symbol-function 'qq-state-friend-categories-loaded-p)
               (lambda () nil))
              ((symbol-function 'qq-state-groups-loaded-p) (lambda () nil))
              ((symbol-function 'qq-native-refresh-friend-categories)
               (lambda (callback _errback &optional _refresh)
                 (push 'friends calls)
                 (funcall callback nil)))
              ((symbol-function 'qq-native-refresh-joined-groups)
               (lambda (callback _errback &optional _refresh)
                 (push 'groups calls)
                 (funcall callback nil))))
      (qq-native--maybe-bootstrap)
      (qq-native--maybe-bootstrap)
      (should (equal (sort calls
                           (lambda (left right)
                             (string-lessp (symbol-name left)
                                           (symbol-name right))))
                     '(friends groups)))
      (should (equal qq-native--bootstrap-owner
                     '("slot-a" . "7")))
      (should (= qq-native--bootstrap-pending 0)))))

(ert-deftest qq-v2-has-no-backend-selector ()
  (should-not (boundp 'qq-backend))
  (should-not (fboundp 'qq-switch-backend)))

(provide 'qq-native-test)

;;; qq-native-test.el ends here
