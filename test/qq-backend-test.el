;;; qq-backend-test.el --- Tests for explicit backend dispatch -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq)

(defun qq-backend-test-message-event ()
  "Return one valid private native Gateway message event."
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

(defun qq-backend-test-account ()
  "Return one selected online Gateway account."
  '((account_id . "slot-a")
    (label . "Primary")
    (phase . "online")
    (uin . "10002")
    (uid . "u_self")
    (generation . "7")
    (challenge)
    (problem)))

(defconst qq-backend-test-members
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

(ert-deftest qq-backend-connect-and-disconnect-use-explicit-selection ()
  (let (calls)
    (cl-letf (((symbol-function 'qq-backend-activate)
               (lambda () (push 'activate calls)))
              ((symbol-function 'qq-transport-start)
               (lambda () (push 'onebot-start calls)))
              ((symbol-function 'qq-transport-stop)
               (lambda () (push 'onebot-stop calls)))
              ((symbol-function 'qq-gateway-transport-start)
               (lambda () (push 'gateway-start calls)))
              ((symbol-function 'qq-gateway-transport-stop)
               (lambda () (push 'gateway-stop calls))))
      (let ((qq-backend 'onebot))
        (qq-backend-connect)
        (qq-backend-disconnect))
      (let ((qq-backend 'gateway))
        (qq-backend-connect)
        (qq-backend-disconnect)))
    (should
     (equal (nreverse calls)
            '(activate onebot-start onebot-stop
              activate gateway-start gateway-stop)))))

(ert-deftest qq-backend-onebot-actions-fail-closed-under-gateway ()
  (let ((qq-backend 'gateway) sent)
    (cl-letf (((symbol-function 'qq-transport-send)
               (lambda (&rest arguments) (setq sent arguments))))
      (should-error
       (qq-api-call "get_login_info" nil #'ignore) :type 'user-error)
      (should-not sent))))

(ert-deftest qq-backend-onebot-events-cannot-write-gateway-state ()
  (let ((qq-backend 'gateway) bootstrap status)
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

(ert-deftest qq-backend-gateway-events-remain-observable-but-not-projected ()
  (let ((qq-backend 'onebot)
        (qq-gateway--accounts (make-hash-table :test #'equal))
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
           (list (qq-backend-test-account)) 'ready "test-gateway")
          (let ((qq-gateway-message-event-hook
                 (list (lambda (event _data) (setq observed event)))))
            (qq-gateway-message--handle-event
             "message.received" (qq-backend-test-message-event)))
          (should (equal observed "message.received"))
          (should-not (qq-state-session "private:10001"))
          (should-not qq-gateway-message--projection-owner))
      (qq-state-reset))))

(ert-deftest qq-backend-onebot-activation-clears-only-owned-gateway-state ()
  (let ((qq-backend 'onebot)
        (qq-gateway-message--projection-owner '("slot-a" . "7"))
        (qq-gateway-message--peer-uin-by-uid
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-recalls
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-reactions
         (make-hash-table :test #'equal))
        (qq-gateway-message--pending-sends
         (make-hash-table :test #'equal))
        reset)
    (puthash "u_peer" "10001" qq-gateway-message--peer-uin-by-uid)
    (cl-letf (((symbol-function 'qq-state-reset)
               (lambda () (setq reset t))))
      (qq-backend-activate)
      (should reset)
      (should-not qq-gateway-message--projection-owner)
      (should (= (hash-table-count
                  qq-gateway-message--peer-uin-by-uid)
                 0))
      (setq reset nil)
      (qq-backend-activate)
      (should-not reset))))

(ert-deftest qq-backend-directory-dispatch-retains-cancellation-origin ()
  (let ((qq-backend 'gateway) sent cancelled)
    (cl-letf (((symbol-function 'qq-gateway-directory-refresh-friends)
               (lambda (callback errback refresh)
                 (setq sent (list callback errback refresh))
                 "gateway-request"))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (setq cancelled token))))
      (let ((request
             (qq-backend-refresh-friend-categories #'ignore #'ignore t)))
        (should (qq-backend-request-p request))
        (should (eq (qq-backend-request-backend request) 'gateway))
        (should (equal (qq-backend-request-token request) "gateway-request"))
        (should (eq (nth 0 sent) #'ignore))
        (should (eq (nth 1 sent) #'ignore))
        (should (eq (nth 2 sent) t))
        (let ((qq-backend 'onebot))
          (qq-backend-cancel-request request))
        (should (equal cancelled "gateway-request"))))))

(ert-deftest qq-backend-gateway-group-profile-uses-owned-directory-state ()
  (let ((qq-backend 'gateway) profile refreshed)
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
          (cl-letf (((symbol-function 'qq-backend-refresh-joined-groups)
                     (lambda (&rest _arguments) (setq refreshed t))))
            (should-not
             (qq-backend-get-group
              "8209413637" (lambda (value) (setq profile value))))
            (should-not refreshed)
            (should (equal (alist-get 'group_id profile) "8209413637"))
            (should (equal (alist-get 'name profile) "Protocol Lab"))
            (should (= (alist-get 'member_count profile) 3))))
      (qq-state-reset))))

(ert-deftest qq-backend-group-settings-update-shared-directory-after-receipt ()
  (let ((qq-backend 'gateway) calls callbacks)
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
                   (qq-backend-set-group-name
                    "8209413637" "New"
                    (lambda (_receipt) (push 'name callbacks))))
                  (remark-request
                   (qq-backend-set-group-remark
                    "8209413637" ""
                    (lambda (_receipt) (push 'remark callbacks))))
                  (mute-request
                   (qq-backend-set-group-whole-mute
                    "8209413637" t
                    (lambda (_receipt) (push 'mute callbacks))))
                  (pinned-request
                   (qq-backend-set-group-pinned
                    "8209413637" nil
                    (lambda (_receipt) (push 'pinned callbacks))))
                  (clock-in-request
                   (qq-backend-clock-in-group
                    "8209413637"
                    (lambda (_receipt) (push 'clock-in callbacks)))))
              (should (equal (qq-backend-request-token name-request)
                             "name-request"))
              (should (equal (qq-backend-request-token remark-request)
                             "remark-request"))
              (should (equal (qq-backend-request-token mute-request)
                             "mute-request"))
              (should (equal (qq-backend-request-token pinned-request)
                             "pinned-request"))
              (should (equal (qq-backend-request-token clock-in-request)
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

(ert-deftest qq-backend-onebot-group-pinned-retains-request-origin ()
  (let ((qq-backend 'onebot) called callback-value)
    (cl-letf (((symbol-function 'qq-api-set-group-pinned)
               (lambda (group-id pinned callback &optional _errback)
                 (setq called (list group-id pinned))
                 (funcall callback
                          `((group_id . ,group-id) (pinned . t)))
                 "onebot-pinned")))
      (let ((request
             (qq-backend-set-group-pinned
              "20001" t
              (lambda (receipt) (setq callback-value receipt)))))
        (should (eq (qq-backend-request-backend request) 'onebot))
        (should (equal (qq-backend-request-token request) "onebot-pinned"))))
    (should (equal called '("20001" t)))
    (should (eq (alist-get 'pinned callback-value) t))))

(ert-deftest qq-backend-group-leave-routes-both-backends-and-converges-state ()
  (dolist (backend '(onebot gateway))
    (let ((qq-backend backend)
          (group-id (if (eq backend 'onebot) "20001" "8209413637"))
          called callback-value)
      (unwind-protect
          (progn
            (qq-state-reset)
            (qq-state-apply-groups
             `(((group_id . ,group-id) (group_name . "Leave"))
               ((group_id . "20002") (group_name . "Keep"))))
            (cl-letf
                (((symbol-function 'qq-api-leave-group)
                  (lambda (group-id callback &optional _errback)
                    (setq called (list 'onebot group-id))
                    (funcall callback '((status . "ok")))
                    "onebot-leave"))
                 ((symbol-function 'qq-gateway-directory-leave-group)
                  (lambda (group-id callback &optional _errback)
                    (setq called (list 'gateway group-id))
                    (funcall callback
                             `((account_id . "slot-a") (generation . "7")
                               (group_uin . ,group-id)))
                    "gateway-leave")))
              (let ((request
                     (qq-backend-leave-group
                      group-id
                      (lambda (receipt) (setq callback-value receipt)))))
                (should (eq (qq-backend-request-backend request) backend))
                (should
                 (equal (qq-backend-request-token request)
                        (if (eq backend 'onebot)
                            "onebot-leave"
                          "gateway-leave")))))
            (should (equal called (list backend group-id)))
            (should callback-value)
            (should-not (qq-state-group group-id))
            (should (qq-state-group "20002")))
        (qq-state-reset)))))

(ert-deftest qq-backend-group-at-all-query-routes-both-backends ()
  (dolist (backend '(onebot gateway))
    (let ((qq-backend backend)
          (group-id (if (eq backend 'onebot) "20001" "8209413637"))
          called callback-value)
      (cl-letf
          (((symbol-function 'qq-api-get-group-at-all-remaining)
            (lambda (candidate callback &optional _errback)
              (setq called (list 'onebot candidate))
              (funcall
               callback
               '((can_at_all . :false)
                 (remain_at_all_count_for_group . 9)
                 (remain_at_all_count_for_uin . 3)))
              "onebot-at-all"))
           ((symbol-function
             'qq-gateway-directory-get-group-at-all-remaining)
            (lambda (candidate callback &optional _errback)
              (setq called (list 'gateway candidate))
              (funcall
               callback
               `((account_id . "slot-a") (generation . "7")
                 (group_uin . ,candidate) (can_at_all . t)
                 (remain_at_all_count_for_uin . 3)
                 (remain_at_all_count_for_group . 9)))
              "gateway-at-all")))
        (let ((request
               (qq-backend-get-group-at-all-remaining
                group-id (lambda (receipt) (setq callback-value receipt)))))
          (should (eq (qq-backend-request-backend request) backend))
          (should
           (equal (qq-backend-request-token request)
                  (if (eq backend 'onebot)
                      "onebot-at-all"
                    "gateway-at-all")))))
      (should (equal called (list backend group-id)))
      (should (= (alist-get 'remain_at_all_count_for_uin callback-value) 3))
      (should (= (alist-get 'remain_at_all_count_for_group callback-value) 9))))
  (let ((qq-backend 'onebot))
    (should-error
     (qq-backend-get-group-at-all-remaining "8209413637" #'ignore)
     :type 'user-error)))

(ert-deftest qq-backend-group-leave-does-not-invent-unloaded-directory ()
  (let ((qq-backend 'onebot))
    (unwind-protect
        (progn
          (qq-state-reset)
          (cl-letf (((symbol-function 'qq-api-leave-group)
                     (lambda (_group-id callback &optional _errback)
                       (funcall callback '((status . "ok")))
                       "leave")))
            (qq-backend-leave-group "20001"))
          (should-not (qq-state-groups-loaded-p)))
      (qq-state-reset))))

(ert-deftest qq-backend-group-member-settings-route-wide-native-identities ()
  (let ((qq-backend 'gateway) calls callbacks)
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
             (qq-backend-set-group-member-card
              "8209413637" "9007199254741001" "Ferris"
              (lambda (_receipt) (push 'card callbacks))))
            (title-request
             (qq-backend-set-group-member-special-title
              "8209413637" "9007199254741001" "Maintainer"
              (lambda (_receipt) (push 'title callbacks))))
            (kick-request
             (qq-backend-kick-group-member
              "8209413637" "9007199254741001" t
              (lambda (_receipt) (push 'kick callbacks)))))
        (should (equal (qq-backend-request-token card-request)
                       "card-request"))
        (should (equal (qq-backend-request-token title-request)
                       "title-request"))
        (should (equal (qq-backend-request-token kick-request)
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
    (should (qq-backend-group-id-p "8209413637"))
    (should (qq-backend-user-id-p "9007199254741001")))
  (let ((qq-backend 'onebot))
    (should-not (qq-backend-group-id-p "8209413637"))
    (should (qq-backend-user-id-p "9007199254741001"))))

(ert-deftest qq-backend-gateway-member-search-filters-cached-exact-ids ()
  (let ((qq-backend 'gateway) result fetched)
    (cl-letf (((symbol-function 'qq-gateway-directory-group-member-page)
               (lambda (_group-id)
                 `((members . ,(copy-tree qq-backend-test-members)))))
              ((symbol-function 'qq-gateway-directory-list-group-members)
               (lambda (&rest _) (setq fetched t))))
      (should-not
       (qq-backend-search-group-members
        "8209413637" "alice" (lambda (members) (setq result members)) nil 1))
      (should-not fetched)
      (should (= (length result) 1))
      (should (equal (alist-get 'user_id (car result))
                     "9007199254740999"))
      (should (stringp (alist-get 'user_id (car result)))))))

(ert-deftest qq-backend-gateway-member-search-fetches-on-cache-miss ()
  (let ((qq-backend 'gateway) callback result)
    (cl-letf (((symbol-function 'qq-gateway-directory-group-member-page)
               (lambda (_group-id) nil))
              ((symbol-function 'qq-gateway-directory-list-group-members)
               (lambda (_group-id success _errback &optional _refresh)
                 (setq callback success)
                 "member-request")))
      (let ((request
             (qq-backend-search-group-members
              "8209413637" "friend"
              (lambda (members) (setq result members)) nil 200)))
        (should (eq (qq-backend-request-backend request) 'gateway))
        (funcall callback (copy-tree qq-backend-test-members))
        (should (= (length result) 1))
        (should (equal (alist-get 'user_id (car result)) "10003"))))))

(ert-deftest qq-backend-gateway-send-routes-closed-segments ()
  (let ((qq-backend 'gateway) sent)
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
          (qq-backend-send-message
           "group:8209413637" segments "optimistic")
          "send-request"))
        (should (equal (nth 0 sent) "group:8209413637"))
        (should (eq (nth 1 sent) segments))
        (should (equal (nth 2 sent) "optimistic"))))))

(ert-deftest qq-backend-gateway-poke-routes-exact-target ()
  (let ((qq-backend 'gateway) call)
    (cl-letf (((symbol-function 'qq-gateway-message-send-poke)
               (lambda (session-key target-id &optional callback errback)
                 (setq call (list session-key target-id callback errback))
                 "poke-request")))
      (should
       (equal (qq-backend-send-poke "group:8209413637" "10002")
              "poke-request"))
      (should (equal (nth 0 call) "group:8209413637"))
      (should (equal (nth 1 call) "10002"))
      (should (functionp (nth 3 call))))))

(ert-deftest qq-backend-gateway-reaction-routes-whole-message ()
  (let ((qq-backend 'gateway) call)
    (cl-letf (((symbol-function 'qq-gateway-message-set-reaction)
               (lambda (message emoji-id set &optional callback errback)
                 (setq call (list message emoji-id set callback errback))
                 "reaction-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (should
         (equal (qq-backend-set-message-reaction message "178" t)
                "reaction-request"))
        (should (eq (nth 0 call) message))
        (should (equal (nth 1 call) "178"))
        (should (eq (nth 2 call) t))
        (should (functionp (nth 4 call)))))))

(ert-deftest qq-backend-onebot-reaction-retains-closed-reference ()
  (let ((qq-backend 'onebot) call)
    (cl-letf (((symbol-function 'qq-api-set-message-emoji-like)
               (lambda (reference emoji-id set &optional callback errback)
                 (setq call (list reference emoji-id set callback errback))
                 "onebot-reaction")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823"))))
        (should
         (equal (qq-backend-set-message-reaction message "178" nil)
                "onebot-reaction"))
        (should
         (equal (nth 0 call)
                '((message_id . "7348923749823749823")
                  (chat . ((kind . "group")
                           (group_id . "8209413637"))))))
        (should (equal (nth 1 call) "178"))
        (should-not (nth 2 call))))))

(ert-deftest qq-backend-gateway-essence-routes-whole-message ()
  (let ((qq-backend 'gateway) call)
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
         (equal (qq-backend-set-message-essence message t)
                "essence-request"))
        (should (eq (nth 0 call) message))
        (should (eq (nth 1 call) t))
        (should (functionp (nth 3 call)))))))

(ert-deftest qq-backend-onebot-essence-retains-closed-reference ()
  (let ((qq-backend 'onebot) call)
    (cl-letf (((symbol-function 'qq-api-set-message-essence)
               (lambda (reference set &optional callback errback)
                 (setq call (list reference set callback errback))
                 "onebot-essence")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823"))))
        (should
         (equal (qq-backend-set-message-essence message nil)
                "onebot-essence"))
        (should
         (equal (nth 0 call)
                '((message_id . "7348923749823749823")
                  (chat . ((kind . "group")
                           (group_id . "8209413637"))))))
        (should-not (nth 1 call))))))

(ert-deftest qq-backend-gateway-poke-recall-routes-whole-message ()
  (let ((qq-backend 'gateway) call)
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
        (should (equal (qq-backend-recall-poke message)
                       "poke-recall-request"))
        (should (eq (car call) message))
        (should (functionp (nth 2 call)))))))

(ert-deftest qq-backend-recall-dispatches-the-owned-native-message ()
  (let ((qq-backend 'gateway) call)
    (cl-letf (((symbol-function 'qq-gateway-message-recall)
               (lambda (session-key message &optional callback errback)
                 (setq call (list session-key message callback errback))
                 "recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "99"))))
        (should
         (equal (qq-backend-recall-message message)
                "recall-request"))
        (should (equal (car call) "group:8209413637"))
        (should (eq (cadr call) message))))))

(ert-deftest qq-backend-gateway-history-frontier-prefers-newer-live-sequence ()
  (let ((qq-backend 'gateway))
    (cl-letf (((symbol-function 'qq-state-group)
               (lambda (_group-id) '((latest_sequence . "9007199254740999"))))
              ((symbol-function 'qq-gateway-message-live-frontier)
               (lambda (_session-key)
                 '((message_id . "7348923749823749823")
                   (sequence . "9007199254741001")))))
      (let ((frontier
             (qq-backend-history-frontier "group:8209413637")))
        (should (equal (plist-get frontier :sequence)
                       "9007199254741001"))
        (should (equal (plist-get frontier :message-id)
                       "7348923749823749823"))
        (should (eq (plist-get frontier :source) 'live-event))))))

(ert-deftest qq-backend-gateway-history-ranges-stay-exact-and-bounded ()
  (should
   (equal (qq-backend-history-range-before
           "9007199254741000" 20)
          '("9007199254740980" . "9007199254740999")))
  (should
   (equal (qq-backend-history-range-after
           "9007199254740999" 20 "9007199254741005")
          '("9007199254741000" . "9007199254741005")))
  (should-not (qq-backend-history-range-before "0" 20))
  (should-not
   (qq-backend-history-range-after "100" 20 "100")))

(ert-deftest qq-backend-gateway-latest-history-uses-authoritative-range ()
  (let ((qq-backend 'gateway) sent properties callback-meta)
    (cl-letf (((symbol-function 'qq-backend-history-frontier)
               (lambda (_session-key)
                 '(:backend gateway :sequence "9007199254741005"
                   :authoritative-p t)))
              ((symbol-function 'qq-backend-fetch-history-range)
               (lambda (_session start end callback _errback metadata)
                 (setq sent (cons start end) properties metadata)
                 (funcall callback '(:message-count 0 :batch-message-ids nil))
                 (qq-backend-request-create
                  :backend 'gateway :token "history-request"))))
      (let ((request
             (qq-backend-fetch-latest-history
              "group:8209413637"
              (lambda (meta) (setq callback-meta meta)) nil 20)))
        (should (qq-backend-request-p request))
        (should (equal sent
                       '("9007199254740986" . "9007199254741005")))
        (should (eq (plist-get properties :history-at-latest-p) t))
        (should (= (plist-get callback-meta :message-count) 0))))))

(ert-deftest qq-backend-gateway-private-latest-never-guesses-a-sequence ()
  (let ((qq-backend 'gateway) callback-meta fetched)
    (cl-letf (((symbol-function 'qq-backend-history-frontier)
               (lambda (_session-key)
                 '(:backend gateway
                   :unavailable-reason private-latest-sequence)))
              ((symbol-function 'qq-backend-fetch-history-range)
               (lambda (&rest _) (setq fetched t))))
      (should-not
       (qq-backend-fetch-latest-history
        "private:10001" (lambda (meta) (setq callback-meta meta))))
      (should-not fetched)
      (should (eq (plist-get callback-meta :history-frontier-unavailable)
                  'private-latest-sequence)))))

(ert-deftest qq-backend-gateway-around-requires-cached-exact-sequence ()
  (let ((qq-backend 'gateway) range failure)
    (cl-letf (((symbol-function 'qq-state-session-messages)
               (lambda (_session-key)
                 '(((server-id . "7348923749823749823")
                    (message-seq . "9007199254740999")))))
              ((symbol-function 'qq-backend-fetch-history-range)
               (lambda (_session start end _callback _errback _properties)
                 (setq range (cons start end))
                 "history-request")))
      (should
       (equal
        (qq-backend-fetch-history-around
         "private:10001" "7348923749823749823" #'ignore nil 20)
        "history-request"))
      (should
       (equal range
              '("9007199254740990" . "9007199254741009")))
      (qq-backend-fetch-history-around
       "private:10001" "missing" #'ignore
       (lambda (_body reason) (setq failure reason)) 20)
      (should (string-match-p "cached message" failure)))))

(ert-deftest qq-backend-gateway-bootstrap-coalesces-one-owner ()
  (let ((qq-backend 'gateway)
        (qq-backend--gateway-bootstrap-owner nil)
        (qq-backend--gateway-bootstrap-pending 0)
        calls)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
              ((symbol-function 'qq-gateway-current-account)
               (lambda () (qq-backend-test-account)))
              ((symbol-function 'qq-gateway-current-account-owner)
               (lambda () '("slot-a" . "7")))
              ((symbol-function 'qq-state-friend-categories-loaded-p)
               (lambda () nil))
              ((symbol-function 'qq-state-groups-loaded-p) (lambda () nil))
              ((symbol-function 'qq-backend-refresh-friend-categories)
               (lambda (callback _errback &optional _refresh)
                 (push 'friends calls)
                 (funcall callback nil)))
              ((symbol-function 'qq-backend-refresh-joined-groups)
               (lambda (callback _errback &optional _refresh)
                 (push 'groups calls)
                 (funcall callback nil))))
      (qq-backend--maybe-bootstrap-gateway)
      (qq-backend--maybe-bootstrap-gateway)
      (should (equal (sort calls
                           (lambda (left right)
                             (string-lessp (symbol-name left)
                                           (symbol-name right))))
                     '(friends groups)))
      (should (equal qq-backend--gateway-bootstrap-owner
                     '("slot-a" . "7")))
      (should (= qq-backend--gateway-bootstrap-pending 0)))))

(ert-deftest qq-switch-backend-resets-before-starting-new-client ()
  (let ((qq-backend 'onebot) calls)
    (cl-letf (((symbol-function 'qq-reset-session-state)
               (lambda () (push (list 'reset qq-backend) calls)))
              ((symbol-function 'qq)
               (lambda () (push (list 'start qq-backend) calls))))
      (qq-switch-backend 'gateway)
      (should (eq qq-backend 'gateway))
      (should
       (equal (nreverse calls)
              '((reset onebot) (start gateway)))))))

(provide 'qq-backend-test)

;;; qq-backend-test.el ends here
