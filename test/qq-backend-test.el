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
