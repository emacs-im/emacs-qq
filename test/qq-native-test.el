;;; qq-native-test.el --- Tests for the native operation boundary -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq)

(defun qq-native-test-message-event ()
  "Return one valid private native service message event."
  '((account_id . "slot-a")
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
        (segments . [((kind . "text")
                      (payload . ((text . "hello"))))])))))

(defun qq-native-test-account ()
  "Return one selected online QQ account."
  '((account_id . "slot-a")
    (label . "Primary")
    (phase . "online")
    (uin . "10002")
    (uid . "u_self")
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

(ert-deftest qq-native-events-project-the-selected-account-slot ()
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
                         "slot-a")))
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

(ert-deftest qq-native-recent-projects-uid-only-private-and-skips-temporary ()
  (let* ((raw-message (alist-get 'message (qq-native-test-message-event)))
         (message (qq-gateway-message--validate-message raw-message))
         (temporary-message (copy-tree message))
         (page
         `((account_id . "slot-a")
            (conversations
             . (((conversation . ((kind . "private") (peer_uid . "u_peer")))
                 (pinned . t)
                 (activity_revision . "2")
                 (latest_message . ,message)
                 (latest_message_recalled . :false))
                ((conversation
                  . ((kind . "temporary")
                     (peer_uid . "u_peer")
                     (from_tiny_id . "10")))
                 (activity_revision . "1")
                 (latest_message . ,temporary-message)
                 (latest_message_recalled . :false))))
            (truncated . :false))))
    (setf (alist-get 'conversation temporary-message nil nil #'eq)
          '((kind . "temp") (name . "Temporary")
            (from_tiny_id . "10")))
    (unwind-protect
        (progn
          (qq-state-reset)
          (let ((entry
                 (qq-native--recent-row-state-entry
                  page
                  (car (alist-get 'conversations page))
                  (qq-native-test-account))))
            (should-not (plist-member entry :read-cursor-known-p))
            (should-not (plist-member entry :read-cursor)))
          (cl-letf (((symbol-function 'qq-gateway-current-account)
                     (lambda () (qq-native-test-account))))
            (qq-native--apply-recent-page
             page (qq-state-session-summary-observation-start)))
          (should (equal (qq-state-recent-session-keys)
                         '("private:10001")))
          (let ((session (qq-state-session "private:10001")))
            (should (eq (alist-get 'pinned session) t))
            (should (equal
                     (alist-get 'last-message-gateway-account-id session)
                     "slot-a"))
            (should-not (qq-state-session-messages "private:10001"))))
      (qq-state-reset))))

(ert-deftest qq-gateway-message-pure-normalizer-does-not-consume-projection-state ()
  (let* ((raw-message (alist-get 'message (qq-native-test-message-event)))
         (message (qq-gateway-message--validate-message raw-message))
         (qq-state--message-order-counter 41)
         (qq-gateway-message--pending-sends (make-hash-table :test #'equal))
         (qq-gateway-message--pending-recalls (make-hash-table :test #'equal))
         (qq-gateway-message--live-frontiers (make-hash-table :test #'equal)))
    (puthash '(sentinel) t qq-gateway-message--pending-sends)
    (let ((normalized
           (qq-gateway-message-normalize-snapshot
            message "slot-a" (qq-native-test-account))))
      (should (= qq-state--message-order-counter 41))
      (should (gethash '(sentinel) qq-gateway-message--pending-sends))
      (should (= (hash-table-count qq-gateway-message--pending-recalls) 0))
      (should (= (hash-table-count qq-gateway-message--live-frontiers) 0))
      (should-not (assq 'order normalized))
      (should (equal (alist-get 'gateway-account-id normalized) "slot-a")))))

(ert-deftest qq-native-directory-request-owns-cancellation-and-late-callback ()
  (let ((qq-gateway--current-account-id "slot-a") sent cancelled delivered)
    (cl-letf (((symbol-function 'qq-gateway-directory-refresh-friends)
               (lambda (callback errback refresh)
                 (setq sent (list callback errback refresh))
                 "gateway-request"))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (setq cancelled token))))
      (let ((request
             (qq-native-refresh-friend-categories
              (lambda (value) (setq delivered value)) #'ignore t)))
        (should (qq-native-request-p request))
        (should (equal (qq-native-request-token request) "gateway-request"))
        (should (functionp (nth 0 sent)))
        (should (functionp (nth 1 sent)))
        (should (eq (nth 2 sent) t))
        (should (qq-native-cancel-request request))
        (should-not (qq-native-cancel-request request))
        (should (eq (qq-native-request-state request) 'cancelled))
        (should (equal cancelled "gateway-request"))
        (funcall (nth 0 sent) 'late)
        (should-not delivered)))))

(ert-deftest qq-native-recent-request-owns-the-only-lifecycle ()
  (let (cancelled)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-state-session-summary-observation-start)
               (lambda () '(recent-observation)))
              ((symbol-function 'qq-gateway-conversation-list-recent)
               (lambda (_account-id &rest _arguments) "recent-request"))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (setq cancelled token) t)))
      (let ((request (qq-native-refresh-recent-conversations)))
        (should (qq-native-request-active-p request))
        (should (equal (qq-native-request-token request) "recent-request"))
        (should (eq (qq-native-request-replace-key request)
                    'recent-conversations))
        (should (qq-native-cancel-request request))
        (should (equal cancelled "recent-request"))
        (should (eq (qq-native-request-state request) 'cancelled))))))

(ert-deftest qq-native-recent-replacement-revokes-old-projector ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        callbacks cancelled projected delivered (request-count 0))
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-state-session-summary-observation-start)
               (lambda () (list 'observation (1+ request-count))))
              ((symbol-function 'qq-gateway-conversation-list-recent)
               (lambda (account-id &rest arguments)
                 (cl-incf request-count)
                 (push (list account-id (plist-get arguments :callback))
                       callbacks)
                 (format "recent-%d" request-count)))
              ((symbol-function 'qq-native--apply-recent-page)
               (lambda (page observation)
                 (push (list page observation) projected)
                 page))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (qq-native-refresh-recent-conversations
       (lambda (value) (push value delivered)) #'ignore)
      (qq-native-refresh-recent-conversations
       (lambda (value) (push value delivered)) #'ignore)
      (should (equal cancelled '("recent-1")))
      ;; Adapter callbacks can race transport cancellation.  The old product
      ;; request must reject the callback before state projection begins.
      (funcall (cadr (cadr callbacks)) 'old-page)
      (should-not projected)
      (funcall (cadr (car callbacks)) 'new-page)
      (should (equal projected
                     '((new-page (observation 2)))))
      (should (equal delivered '(new-page))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-handles-synchronous-completion-once ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        values)
    (let ((request
           (qq-native-request-start
            (lambda (success _failure)
              (funcall success 'done)
              "already-completed")
            :callback (lambda (value) (push value values)))))
      (should (equal values '(done)))
      (should (eq (qq-native-request-state request) 'settled))
      (should-not (qq-native-request-token request))
      (should-not (qq-native-cancel-request request))
      (should (= (hash-table-count qq-native-request--active) 0)))))

(ert-deftest qq-native-request-replace-key-supersedes-predecessor-once ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled errors first-success second-success projected delivered)
    (cl-letf (((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (let ((first
             (qq-native-request-start
              (lambda (success _failure)
                (setq first-success success)
                "first-token")
              :owner nil
              :replace-key 'recent-conversations
              :projector (lambda (value) (push value projected) value)
              :callback (lambda (value) (push value delivered))
              :errback (lambda (body _reason) (push body errors)))))
        (let ((second
               (qq-native-request-start
                (lambda (success _failure)
                  (setq second-success success)
                  "second-token")
                :owner nil
                :replace-key 'recent-conversations
                :projector (lambda (value) (push value projected) value)
                :callback (lambda (value) (push value delivered))
                :errback (lambda (body _reason) (push body errors)))))
          (should (eq (qq-native-request-state first) 'cancelled))
          (should (qq-native-request-active-p second))
          (should (equal cancelled '("first-token")))
          (should (= (length errors) 1))
          (should (equal (alist-get 'code (car errors))
                         "superseded_request"))
          ;; A racy adapter callback may still arrive after local cancellation;
          ;; revoked ownership keeps both projection and leaf delivery inert.
          (funcall first-success 'stale)
          (should-not projected)
          (should-not delivered)
          (funcall second-success 'fresh)
          (should (equal projected '(fresh)))
          (should (equal delivered '(fresh)))
          (should (eq (qq-native-request-state second) 'settled)))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-projector-failure-is-owned-and-cancels-orphan ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled delivered failure)
    (let ((request
           (qq-native-request-start
            (lambda (success _failure)
              (funcall success 'raw)
              "projector-orphan")
            :owner nil
            :projector (lambda (_value) (error "projection exploded"))
            :callback (lambda (value) (setq delivered value))
            :errback (lambda (body _reason) (setq failure body))
            :cancel-function (lambda (token) (setq cancelled token)))))
      (should (eq (qq-native-request-state request) 'failed))
      (should-not delivered)
      (should (equal (alist-get 'code failure) "invalid_gateway_result"))
      (should (equal cancelled "projector-orphan"))
      (should (= (hash-table-count qq-native-request--active) 0)))))

(ert-deftest qq-native-request-projector-is-an-irrevocable-completion-commit ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        success replacement delivered errors cancelled)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (let ((request
             (qq-native-request-start
              (lambda (callback _failure)
                (setq success callback)
                "completed-token")
              :owner "slot-a"
              :replace-key 'recent-conversations
              :projector
              (lambda (value)
                (setq replacement
                      (qq-native-request-start
                       (lambda (_next-success _next-failure)
                         "replacement-token")
                       :owner "slot-a"
                       :replace-key 'recent-conversations
                       :errback #'ignore))
                value)
              :callback (lambda (value) (setq delivered value))
              :errback (lambda (body _reason) (push body errors)))))
        (funcall success 'committed)
        (should (eq (qq-native-request-state request) 'settled))
        (should (eq delivered 'committed))
        (should-not errors)
        (should-not cancelled)
        (should (qq-native-request-active-p replacement))
        (qq-native-cancel-request replacement)
        (should (equal cancelled '("replacement-token")))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-projector-rechecks-owner-before-leaf-delivery ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        (account-id "slot-a") success delivered)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () account-id)))
      (let ((request
             (qq-native-request-start
              (lambda (callback _failure)
                (setq success callback)
                "switch-token")
              :owner "slot-a"
              :projector
              (lambda (value)
                (setq account-id "slot-b")
                value)
              :callback (lambda (value) (setq delivered value)))))
        (funcall success 'projected)
        (should-not delivered)
        (should (eq (qq-native-request-state request) 'cancelled))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-replace-key-publishes-before-reentrant-errback ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled third second-errors (second-started 0))
    (cl-letf (((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (qq-native-request-start
       (lambda (_success _failure) "first-token")
       :owner nil
       :replace-key 'recent-conversations
       :errback
       (lambda (_body _reason)
         (setq third
               (qq-native-request-start
                (lambda (_success _failure) "third-token")
                :owner nil
                :replace-key 'recent-conversations
                :errback #'ignore))))
      (let ((second
             (qq-native-request-start
              (lambda (_success _failure)
                (cl-incf second-started)
                "second-token")
              :owner nil
              :replace-key 'recent-conversations
              :errback
              (lambda (body _reason) (push body second-errors)))))
        (should (eq (qq-native-request-state second) 'cancelled))
        (should (= second-started 0))
        (should (= (length second-errors) 1))
        (should (equal (alist-get 'code (car second-errors))
                       "superseded_request")))
      (should (qq-native-request-active-p third))
      (should (equal (qq-native-request-token third) "third-token"))
      (should (equal cancelled '("first-token")))
      (qq-native-cancel-request third)
      (should (equal cancelled '("third-token" "first-token"))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-replace-key-cancels-token-returned-after-reentry ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled replacement errors)
    (cl-letf (((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (let ((first
             (qq-native-request-start
              (lambda (_success _failure)
                (setq replacement
                      (qq-native-request-start
                       (lambda (_next-success _next-failure)
                         "replacement-token")
                       :owner nil
                       :replace-key 'recent-conversations
                       :errback #'ignore))
                "orphan-token")
              :owner nil
              :replace-key 'recent-conversations
              :errback (lambda (body _reason) (push body errors)))))
        (should (eq (qq-native-request-state first) 'cancelled))
        (should (qq-native-request-active-p replacement))
        (should (equal cancelled '("orphan-token")))
        (should (= (length errors) 1))
        (should (equal (alist-get 'code (car errors))
                       "superseded_request"))
        (qq-native-cancel-request replacement)
        (should (equal cancelled
                       '("replacement-token" "orphan-token")))))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-replace-key-is-scoped-by-account ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled)
    (cl-letf (((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled) t)))
      (let ((first
             (qq-native-request-start
              (lambda (_success _failure) "slot-a-token")
              :owner "slot-a" :replace-key 'recent-conversations))
            (second
             (qq-native-request-start
              (lambda (_success _failure) "slot-b-token")
              :owner "slot-b" :replace-key 'recent-conversations)))
        (should (qq-native-request-active-p first))
        (should (qq-native-request-active-p second))
        (should-not cancelled)
        (qq-native-cancel-request first)
        (qq-native-cancel-request second)))
    (should (equal (sort cancelled #'string<)
                   '("slot-a-token" "slot-b-token")))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-cancels-token-returned-after-reentrant-revocation ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cancelled)
    (let ((request (qq-native-request-create "slot-a")))
      (should
       (eq
        (qq-native-request-start
         (lambda (_success _failure)
           (qq-native-cancel-request request)
           "late-token")
         :owner "slot-a" :request request
         :cancel-function (lambda (token) (setq cancelled token)))
        request))
      (should (equal cancelled "late-token"))
      (should (eq (qq-native-request-state request) 'cancelled))
      (should (= (hash-table-count qq-native-request--active) 0)))))

(ert-deftest qq-native-request-starter-quit-revokes-registered-ownership ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cleaned cleanup-inhibited-p caught)
    (let ((request
           (qq-native-request-create
            "slot-a"
            (lambda ()
              (setq cleanup-inhibited-p inhibit-quit
                    cleaned t)))))
      (condition-case error-data
          (qq-native-request-start
           (lambda (_success _failure)
             (signal 'quit nil))
           :request request)
        (quit (setq caught error-data)))
      (should (equal caught '(quit)))
      (should cleaned)
      (should cleanup-inhibited-p)
      (should (eq (qq-native-request-state request) 'cancelled))
      (should (= (hash-table-count qq-native-request--active) 0)))))

(ert-deftest qq-native-request-starter-handoff-inhibits-quit ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        (request (qq-native-request-create "slot-a"))
        cancelled starter-inhibited-p)
    (should
     (eq
      (qq-native-request-start
       (lambda (_success _failure)
         (setq starter-inhibited-p inhibit-quit)
         "handoff-token")
       :request request
       :cancel-function (lambda (token) (setq cancelled token)))
      request))
    (should starter-inhibited-p)
    (should (qq-native-request-active-p request))
    (qq-native-cancel-request request)
    (should (equal cancelled "handoff-token"))
    (should (eq (qq-native-request-state request) 'cancelled))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-revoke-all-isolates-quit-through-sweep ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        cleaned cleanup-inhibited)
    (dolist (name '(first second))
      (let ((identity name))
        (qq-native-request-create
         nil
         (lambda ()
           (push identity cleaned)
           (push inhibit-quit cleanup-inhibited)
           (signal 'quit nil)))))
    (qq-native-request-revoke-all)
    (should (equal (sort cleaned
                         (lambda (left right)
                           (string< (symbol-name left) (symbol-name right))))
                   '(first second)))
    (should (equal cleanup-inhibited '(t t)))
    (should (= (hash-table-count qq-native-request--active) 0))))

(ert-deftest qq-native-request-slot-scope-survives-restart-but-not-selection ()
  (let ((qq-native-request--active (make-hash-table :test #'eq))
        (account-id "slot-a")
        success delivered cancelled)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () account-id))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled))))
      (let ((request
             (qq-native-request-start
              (lambda (callback _failure)
                (setq success callback)
                "slot-request")
              :callback (lambda (value) (setq delivered value)))))
        (should (equal (qq-native-request-owner request) "slot-a"))
        ;; The stable slot remains the request's callback observation context.
        (funcall success 'current-page)
        (should (eq delivered 'current-page))
        (should (eq (qq-native-request-state request) 'settled)))
      (setq delivered nil)
      (let ((request
             (qq-native-request-start
              (lambda (callback _failure)
                (setq success callback)
                "switched-slot-request")
              :owner "slot-a"
              :callback (lambda (value) (setq delivered value)))))
        (setq account-id "slot-b")
        (funcall success 'stale-page)
        (should-not delivered)
        (should (eq (qq-native-request-state request) 'cancelled))
        (should (equal cancelled '("switched-slot-request")))))))

(ert-deftest qq-native-start-request-distinguishes-omitted-and-global-scope ()
  (let ((qq-native-request--active (make-hash-table :test #'eq)))
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a")))
      (let ((slot-request
             (qq-native--start-request
              (lambda (_success _failure) "slot-token") nil nil))
            (global-request
             (qq-native--start-request
              (lambda (_success _failure) "global-token") nil nil
              :owner nil)))
        (should (equal (qq-native-request-owner slot-request) "slot-a"))
        (should-not (qq-native-request-owner global-request))
        (qq-native-cancel-request slot-request)
        (qq-native-cancel-request global-request)))))

(ert-deftest qq-native-start-request-requires-slot-unless-global-is-explicit ()
  (let ((qq-native-request--active (make-hash-table :test #'eq)))
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () nil)))
      (should-error
       (qq-native--start-request
        (lambda (_success _failure) "unreachable") nil nil)
       :type 'user-error)
      (let ((global-request
             (qq-native--start-request
              (lambda (_success _failure) "global-token") nil nil
              :owner nil)))
        (should-not (qq-native-request-owner global-request))
        (qq-native-cancel-request global-request)))))

(ert-deftest qq-native-request-rejects-malformed-owner-context ()
  (dolist (owner '("" account ("slot-a" . "runtime")
                   ("" . "runtime") ("slot-a" . 7)))
    (should-error (qq-native-request-create owner))))

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
  (let ((qq-gateway--current-account-id "slot-a") calls callbacks)
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
              (dolist (request (list name-request remark-request mute-request
                                     pinned-request clock-in-request))
                (should (qq-native-request-p request))
                (should (eq (qq-native-request-state request) 'settled))
                (should-not (qq-native-request-token request)))))
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
  (let ((qq-gateway--current-account-id "slot-a") called callback-value)
    (cl-letf (((symbol-function 'qq-gateway-directory-set-friend-pinned)
               (lambda (user-id pinned callback &optional _errback)
                 (setq called (list user-id pinned))
                 (funcall callback `((friend_uin . ,user-id) (pinned . t)))
                 "friend-pin")))
      (let ((request
             (qq-native-set-friend-pinned
              "9007199254740999" t
              (lambda (receipt) (setq callback-value receipt)))))
        (should (eq (qq-native-request-state request) 'settled))))
    (should (equal called '("9007199254740999" t)))
    (should (eq (alist-get 'pinned callback-value) t))))

(ert-deftest qq-native-presence-targets-selected-account ()
  (let (called callback-value)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-account-set-presence)
               (lambda (account-id presence callback &optional _errback)
                 (setq called (list account-id presence))
                 (funcall callback
                          `((account_id . ,account-id)
                            (presence . ,presence)))
                 "presence")))
      (let* ((presence '((kind . "away")))
             (request
              (qq-native-set-presence
               presence (lambda (receipt) (setq callback-value receipt)))))
        (should (eq (qq-native-request-state request) 'settled))
        (should (equal called (list "slot-a" presence)))
        (should (equal (alist-get 'presence callback-value) presence))))))

(ert-deftest qq-native-group-leave-converges-loaded-state ()
  (let ((qq-gateway--current-account-id "slot-a")
        (group-id "8209413637") called callback-value)
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
              (should (eq (qq-native-request-state request) 'settled))))
          (should (equal called group-id))
          (should callback-value)
          (should-not (qq-state-group group-id))
          (should (qq-state-group "20002")))
      (qq-state-reset))))

(ert-deftest qq-native-group-at-all-query-routes-wide-group-uin ()
  (let ((qq-gateway--current-account-id "slot-a") called callback-value)
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
        (should (eq (qq-native-request-state request) 'settled))))
    (should (equal called "8209413637"))
    (should (= (alist-get 'remain_at_all_count_for_uin callback-value) 3))
    (should (= (alist-get 'remain_at_all_count_for_group callback-value) 9))))

(ert-deftest qq-native-group-leave-does-not-invent-unloaded-directory ()
  (unwind-protect
      (progn
        (qq-state-reset)
        (cl-letf (((symbol-function 'qq-gateway-current-account-id)
                   (lambda () "slot-a"))
                  ((symbol-function 'qq-gateway-directory-leave-group)
                   (lambda (_group-id callback &optional _errback)
                     (funcall callback '((status . "ok")))
                     "leave")))
          (qq-native-leave-group "8209413637"))
        (should-not (qq-state-groups-loaded-p)))
    (qq-state-reset)))

(ert-deftest qq-native-group-member-settings-route-wide-native-identities ()
  (let ((qq-gateway--current-account-id "slot-a") calls callbacks)
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
        (dolist (request (list card-request title-request kick-request))
          (should (eq (qq-native-request-state request) 'settled)))))
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
  (let ((qq-gateway--current-account-id "slot-a") callback result)
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
  (let ((qq-gateway--current-account-id "slot-a") sent)
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
        (let ((request
               (qq-native-send-message
                "group:8209413637" segments "optimistic")))
          (should (qq-native-request-p request))
          (should (equal (qq-native-request-token request) "send-request")))
        (should (equal (nth 0 sent) "group:8209413637"))
        (should (eq (nth 1 sent) segments))
        (should (equal (nth 2 sent) "optimistic"))))))

(ert-deftest qq-native-local-images-finish-concurrently-but-send-in-draft-order ()
  (let ((path-a (make-temp-file "qq-native-image-a-" nil ".png" "aaa"))
        (path-b (make-temp-file "qq-native-image-b-" nil ".png" "bbb"))
        (owner "slot-a")
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
              (((symbol-function 'qq-gateway-current-account-id)
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

(ert-deftest qq-native-local-media-starter-signal-retires-and-releases ()
  (let ((path-a (make-temp-file "qq-native-image-ready-" nil ".png" "aaa"))
        (path-b (make-temp-file "qq-native-image-signal-" nil ".png" "bbb"))
        (qq-native-request--active (make-hash-table :test #'eq))
        (real-create (symbol-function 'qq-native-request-create))
        request caught sent failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
             ((symbol-function 'qq-native-request-create)
              (lambda (&optional owner cancel-function)
                (setq request
                      (funcall real-create owner cancel-function))))
             ((symbol-function
               'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (_session path _summary _sub-type callback _errback)
                (if (equal path path-a)
                    (progn
                      ;; The first starter completes before returning.  Its
                      ;; attachment is consequently owned by the composite
                      ;; request when the next starter signals.
                      (funcall
                       callback
                       (qq-native-test-prepared-image
                        "att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
                        "res-ready-before-signal"))
                      (qq-gateway-attachment-operation-create
                       :active-p nil))
                  (signal 'file-error
                          (list "staging exploded" path-b)))))
             ((symbol-function 'qq-native--release-send-resource)
              (lambda (resource-id) (push resource-id released-resources)))
             ((symbol-function 'qq-native--release-send-attachment)
              (lambda (attachment-id)
                (push attachment-id released-attachments)))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (condition-case error-data
              (qq-native-send-message
               "private:10001"
               `(((type . "image") (data . ((file . ,path-a))))
                 ((type . "image") (data . ((file . ,path-b)))))
               nil nil
               (lambda (body reason) (setq failure (list body reason))))
            (file-error (setq caught error-data)))
          (should (equal caught
                         (list 'file-error "staging exploded" path-b)))
          (should (qq-native-request-p request))
          (should (eq (qq-native-request-state request) 'failed))
          (should-not (qq-native-request-active-p request))
          (should (= (hash-table-count qq-native-request--active) 0))
          (should-not sent)
          ;; A synchronous Lisp signal is not translated into the async
          ;; ERRBACK contract.
          (should-not failure)
          (should (equal released-resources
                         '("res-ready-before-signal")))
          (should
           (equal released-attachments
                  '("att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"))))
      (delete-file path-a)
      (delete-file path-b))))

(ert-deftest qq-native-local-media-starter-quit-cleans-composite-ownership ()
  (let ((path-a (make-temp-file "qq-native-image-ready-" nil ".png" "aaa"))
        (path-b (make-temp-file "qq-native-image-ready-" nil ".png" "bbb"))
        (path-c (make-temp-file "qq-native-image-quit-" nil ".png" "ccc"))
        (qq-native-request--active (make-hash-table :test #'eq))
        (real-create (symbol-function 'qq-native-request-create))
        request operations late-ready caught sent failure cleanup-quit-p
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
             ((symbol-function 'qq-native-request-create)
              (lambda (&optional owner cancel-function)
                (setq request
                      (funcall real-create owner cancel-function))))
             ((symbol-function
              'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (_session path _summary _sub-type callback _errback)
                (cond
                 ((equal path path-c)
                  (setq late-ready callback)
                  (signal 'quit nil))
                 (t
                  (funcall
                   callback
                   (if (equal path path-a)
                       (qq-native-test-prepared-image
                        "att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
                        "res-ready-a")
                     (qq-native-test-prepared-image
                      "att-bbbbbbbb-1111-4111-8111-bbbbbbbbbbbb"
                      "res-ready-b")))
                  (let ((operation
                         (qq-gateway-attachment-operation-create
                          :active-p t)))
                    (push operation operations)
                    operation)))))
             ((symbol-function 'qq-native--release-send-resource)
              (lambda (resource-id) (push resource-id released-resources)))
             ((symbol-function 'qq-native--release-send-attachment)
              (lambda (attachment-id)
                (push attachment-id released-attachments)
                ;; The first cleanup item must not prevent the remaining
                ;; attachment sweep or later resource/late-result cleanup.
                (unless cleanup-quit-p
                  (setq cleanup-quit-p t)
                  (signal 'quit nil))))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (condition-case error-data
              (qq-native-send-message
               "private:10001"
               `(((type . "image") (data . ((file . ,path-a))))
                 ((type . "image") (data . ((file . ,path-b))))
                 ((type . "image") (data . ((file . ,path-c)))))
               nil nil
               (lambda (body reason) (setq failure (list body reason))))
            (quit (setq caught error-data)))
          (should (equal caught '(quit)))
          (should (qq-native-request-p request))
          (should (eq (qq-native-request-state request) 'failed))
          (should-not (qq-native-request-active-p request))
          (should (= (hash-table-count qq-native-request--active) 0))
          (should (= (length operations) 2))
          (dolist (operation operations)
            (should-not
             (qq-gateway-attachment-operation-active-p operation)))
          (should-not sent)
          (should-not failure)
          ;; A completion from the starter that was interrupted is inert and
          ;; releases its orphan instead of dispatching message.send.
          (funcall
           late-ready
           (qq-native-test-prepared-image
            "att-cccccccc-1111-4111-8111-cccccccccccc"
            "res-ready-c"))
          (should-not sent)
          (should
           (equal (sort released-resources #'string<)
                  '("res-ready-a" "res-ready-b" "res-ready-c")))
          (should
           (equal
            (sort released-attachments #'string<)
            '("att-aaaaaaaa-1111-4111-8111-aaaaaaaaaaaa"
              "att-bbbbbbbb-1111-4111-8111-bbbbbbbbbbbb"
              "att-cccccccc-1111-4111-8111-cccccccccccc"))))
      (delete-file path-a)
      (delete-file path-b)
      (delete-file path-c))))

(ert-deftest qq-native-local-media-operation-handoff-inhibits-quit ()
  (let ((path (make-temp-file "qq-native-image-pending-" nil ".png" "abc"))
        (qq-native-request--active (make-hash-table :test #'eq))
        (real-create (symbol-function 'qq-native-request-create))
        request operation starter-inhibited-p sent)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
             ((symbol-function 'qq-native-request-create)
              (lambda (&optional owner cancel-function)
                (setq request
                      (funcall real-create owner cancel-function))))
             ((symbol-function
               'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (&rest _arguments)
                (setq operation
                      (qq-gateway-attachment-operation-create :active-p t)
                      starter-inhibited-p inhibit-quit)
                operation))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (&rest _arguments) (setq sent t))))
          (should
           (eq
            (qq-native-send-message
             "private:10001"
             `(((type . "image") (data . ((file . ,path))))))
            request))
          (should starter-inhibited-p)
          (should (qq-native-request-active-p request))
          (qq-native-cancel-request request)
          (should-not (qq-gateway-attachment-operation-active-p operation))
          (should-not sent)
          (should (= (hash-table-count qq-native-request--active) 0)))
      (delete-file path))))

(ert-deftest qq-native-local-media-send-handoff-inhibits-quit ()
  (let ((path (make-temp-file "qq-native-image-send-" nil ".png" "abc"))
        (qq-native-request--active (make-hash-table :test #'eq))
        (real-create (symbol-function 'qq-native-request-create))
        request cancelled send-errback send-inhibited-p
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
             ((symbol-function 'qq-native-request-create)
              (lambda (&optional owner cancel-function)
                (setq request
                      (funcall real-create owner cancel-function))))
             ((symbol-function
               'qq-gateway-attachment-stage-and-prepare-image)
              (lambda (_session _path _summary _sub-type callback _errback)
                (funcall
                 callback
                 (qq-native-test-prepared-image
                  "att-dddddddd-1111-4111-8111-dddddddddddd"
                  "res-pending-send"))
                (qq-gateway-attachment-operation-create :active-p nil)))
             ((symbol-function 'qq-native--release-send-resource)
              (lambda (resource-id) (push resource-id released-resources)))
             ((symbol-function 'qq-native--release-send-attachment)
              (lambda (attachment-id)
                (push attachment-id released-attachments)))
             ((symbol-function 'qq-gateway-message-send)
              (lambda (_session _segments &optional _raw _callback errback
                       _optimistic)
                (setq send-errback errback
                      send-inhibited-p inhibit-quit)
                "send-handoff-token"))
             ((symbol-function 'qq-gateway-transport-cancel)
              (lambda (token) (push token cancelled))))
          (should
           (eq
            (qq-native-send-message
             "private:10001"
             `(((type . "image") (data . ((file . ,path))))))
            request))
          (should send-inhibited-p)
          (should (qq-native-request-active-p request))
          (qq-native-cancel-request request)
          (should (equal cancelled '("send-handoff-token")))
          (should (equal released-resources '("res-pending-send")))
          (should
           (equal released-attachments
                  '("att-dddddddd-1111-4111-8111-dddddddddddd")))
          (funcall send-errback nil "late send failure")
          (should
           (equal released-attachments
                  '("att-dddddddd-1111-4111-8111-dddddddddddd")))
          (should (= (hash-table-count qq-native-request--active) 0)))
      (delete-file path))))

(ert-deftest qq-native-local-record-is-prepared-before-message-send ()
  (let ((path (make-temp-file "qq-native-record-" nil ".wav" "pcm"))
        (owner "slot-a")
        operation prepared sent released-resources released-attachments)
    (unwind-protect
        (let ((segments
               `(((type . "text") (data . ((text . "voice:"))))
                 ((type . "record")
                  (data . ((file . ,path) (name . "voice.wav")))))))
          (cl-letf
              (((symbol-function 'qq-gateway-current-account-id)
                (lambda () owner))
               ((symbol-function
                 'qq-gateway-attachment-stage-and-prepare-record)
                (lambda (session record-path callback errback)
                  (setq prepared
                        (list session record-path callback errback)
                        operation
                        (qq-gateway-attachment-operation-create :active-p t))
                  operation))
               ((symbol-function 'qq-native--release-send-resource)
                (lambda (resource-id) (push resource-id released-resources)))
               ((symbol-function 'qq-native--release-send-attachment)
                (lambda (attachment-id)
                  (push attachment-id released-attachments)))
               ((symbol-function 'qq-gateway-message-send)
                (lambda (session ready-segments
                                 &optional raw callback errback optimistic)
                  (setq sent (list session ready-segments raw callback
                                   errback optimistic))
                  "send-record-request")))
            (let ((request
                   (qq-native-send-message
                    "private:10001" segments "optimistic")))
              (should (qq-native-request-p request))
              (should (equal (car prepared) "private:10001"))
              (should (equal (cadr prepared) path))
              (setf (qq-gateway-attachment-operation-active-p operation) nil)
              (funcall
               (nth 2 prepared)
               (qq-native-test-prepared-image
                "att-dddddddd-dddd-4ddd-8ddd-dddddddddddd"
                "res-record-ready"))
              (should (equal (qq-native-request-token request)
                             "send-record-request"))
              (should
               (equal
                (nth 1 sent)
                '(((type . "text") (data . ((text . "voice:"))))
                  ((type . "record")
                   (data
                    . ((attachment_id
                        . "att-dddddddd-dddd-4ddd-8ddd-dddddddddddd")))))))
              (should (equal (nth 5 sent) segments))
              (should-not
               (string-match-p "qq-native-record-"
                               (prin1-to-string (nth 1 sent))))
              (should (equal released-resources '("res-record-ready")))
              (funcall (nth 3 sent) '((sent . t)))
              (should (eq (qq-native-request-state request) 'settled))
              (should-not released-attachments))))
      (delete-file path))))

(ert-deftest qq-native-local-image-cancel-stops-before-message-dispatch ()
  (let ((path (make-temp-file "qq-native-image-cancel-" nil ".png" "abc"))
        (owner "slot-a")
        operation sent)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
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

(ert-deftest qq-native-local-image-account-switch-releases-completion ()
  (let ((path (make-temp-file "qq-native-image-owner-" nil ".png" "abc"))
        (owner "slot-a")
        operation ready-callback sent failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
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
          (let ((request
                 (qq-native-send-message
                  "private:10001"
                  `(((type . "image") (data . ((file . ,path)))))
                  nil nil
                  (lambda (body reason) (setq failure (list body reason))))))
            (setq owner "slot-b")
            (setf (qq-gateway-attachment-operation-active-p operation) nil)
            (funcall
             ready-callback
             (qq-native-test-prepared-image
              "att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"
              "res-image-owner"))
            (should (eq (qq-native-request-state request) 'cancelled)))
          (should-not sent)
          (should-not failure)
          (should (equal released-resources '("res-image-owner")))
          (should
           (equal released-attachments
                  '("att-cccccccc-cccc-4ccc-8ccc-cccccccccccc"))))
      (delete-file path))))

(ert-deftest qq-native-local-image-preflight-failure-releases-prepared-object ()
  (let ((path (make-temp-file "qq-native-image-preflight-" nil ".png" "abc"))
        ready-callback operation failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
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
              (lambda (_session _segments &optional _raw _callback errback
                       _optimistic)
                (funcall errback
                         '((code . "capability_unavailable"))
                         "message.send unavailable")
                nil)))
          (let ((request
                 (qq-native-send-message
                  "private:10001"
                  `(((type . "image") (data . ((file . ,path)))))
                  nil nil
                  (lambda (body reason) (setq failure (list body reason))))))
            (setf (qq-gateway-attachment-operation-active-p operation) nil)
            (funcall
             ready-callback
             (qq-native-test-prepared-image
              "att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee"
              "res-image-preflight"))
            (should (eq (qq-native-request-state request) 'failed))))
      (delete-file path))
    (should (equal released-resources '("res-image-preflight")))
    (should
     (equal released-attachments
            '("att-eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")))
    (should (equal (cadr failure) "message.send unavailable"))))

(ert-deftest qq-native-local-image-async-send-failure-releases-submitted-object ()
  (let ((path (make-temp-file "qq-native-image-async-failure-" nil ".png" "abc"))
        ready-callback operation send-errback failure
        released-resources released-attachments)
    (unwind-protect
        (cl-letf
            (((symbol-function 'qq-gateway-current-account-id)
              (lambda () "slot-a"))
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
              (lambda (_session _segments &optional _raw _callback errback
                       _optimistic)
                (setq send-errback errback)
                "send-async-failure")))
          (let ((request
                 (qq-native-send-message
                  "private:10001"
                  `(((type . "reply")
                     (data . ((id . "7348923749823749823"))))
                    ((type . "image") (data . ((file . ,path)))))
                  nil nil
                  (lambda (body reason) (setq failure (list body reason))))))
            (setf (qq-gateway-attachment-operation-active-p operation) nil)
            (funcall
             ready-callback
             (qq-native-test-prepared-image
              "att-ffffffff-eeee-4eee-8eee-ffffffffffff"
              "res-image-async-failure"))
            (should (equal (qq-native-request-token request)
                           "send-async-failure"))
            (should (equal released-resources
                           '("res-image-async-failure")))
            (should-not released-attachments)
            (funcall send-errback
                     '((code . "message_reference_unknown"))
                     "reply target is unknown")
            (should (eq (qq-native-request-state request) 'failed))
            (should
             (equal released-attachments
                    '("att-ffffffff-eeee-4eee-8eee-ffffffffffff")))
            (should (equal (cadr failure) "reply target is unknown"))
            ;; A duplicate terminal callback cannot release the ID twice.
            (funcall send-errback
                     '((code . "message_reference_unknown"))
                     "reply target is unknown")
            (should
             (equal released-attachments
                    '("att-ffffffff-eeee-4eee-8eee-ffffffffffff")))))
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
  (let ((qq-gateway--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-gateway-message-send-poke)
               (lambda (session-key target-id &optional callback errback)
                 (setq call (list session-key target-id callback errback))
                 "poke-request")))
      (let ((request
             (qq-native-send-poke "group:8209413637" "10002")))
        (should (qq-native-request-p request))
        (should (equal (qq-native-request-token request) "poke-request")))
      (should (equal (nth 0 call) "group:8209413637"))
      (should (equal (nth 1 call) "10002"))
      (should (functionp (nth 3 call))))))

(ert-deftest qq-native-reaction-routes-whole-message ()
  (let ((qq-gateway--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-gateway-message-set-reaction)
               (lambda (message emoji-id set &optional callback errback)
                 (setq call (list message emoji-id set callback errback))
                 "reaction-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (let ((request (qq-native-set-message-reaction message "178" t)))
          (should (qq-native-request-p request))
          (should (equal (qq-native-request-token request)
                         "reaction-request")))
        (should (eq (nth 0 call) message))
        (should (equal (nth 1 call) "178"))
        (should (eq (nth 2 call) t))
        (should (functionp (nth 4 call)))))))

(ert-deftest qq-native-read-reports-coalesce-to-newest-timeline-message ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        calls completed timeline)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (message)
                 (equal (qq-gateway-current-account-id)
                        (alist-get 'gateway-account-id message))))
              ((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) timeline))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (message &optional callback errback)
                 (setq calls
                       (append calls
                               (list (list message callback errback))))
                 (format "read-%d" (length calls)))))
      (let* ((base
              '((session-key . "group:8209413637")
                (server-id . "7348923749823749823")
                (group-id . "8209413637")
                (gateway-account-id . "slot-a")))
             (newest (copy-tree base))
             (middle (copy-tree base)))
        (setf (alist-get 'server-id newest) "7348923749823749825"
              (alist-get 'server-id middle) "7348923749823749824")
        (setq timeline (list base middle newest))
        (let ((request
               (qq-native-mark-message-read
                base (lambda (_receipt) (push 'base completed)))))
          (should (qq-native-request-p request))
          (should
           (eq
            (qq-native-mark-message-read
             newest (lambda (_receipt) (push 'newest completed)))
            request))
          ;; A later-but-not-newest intent cannot replace the queued frontier.
          (should
           (eq
            (qq-native-mark-message-read
             middle (lambda (_receipt) (push 'middle completed)))
            request)))
        (let ((other-account (copy-tree newest)))
          (setf (alist-get 'server-id other-account) "7348923749823749826"
                (alist-get 'gateway-account-id other-account) "slot-b")
          (should-error
           (qq-native-mark-message-read other-account #'ignore)
           :type 'user-error))
        (should (= (length calls) 1))
        (funcall
         (nth 1 (car calls))
         '((account_id . "slot-a")
           (message_id . "7348923749823749823")))
        (should (= (length calls) 2))
        (should (eq (car (cadr calls)) newest))
        (funcall
         (nth 1 (cadr calls))
         '((account_id . "slot-a")
           (message_id . "7348923749823749825")))
        ;; MIDDLE never reached the wire and therefore owns no callback result.
        (should (equal (nreverse completed) '(base newest)))
        (should (= (hash-table-count qq-native--read-operations) 0))))))

(ert-deftest qq-native-read-callback-reentry-sees-live-stable-successor ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        calls cancelled reentered live-successor queued-callback
        reentered-active-p reentered-is-successor-p timeline)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (_message) t))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled)))
              ((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) timeline))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (message &optional callback errback)
                 (setq calls
                       (append calls
                               (list (list message callback errback))))
                 (format "read-%d" (length calls)))))
      (let* ((base
              '((session-key . "group:8209413637")
                (server-id . "7348923749823749823")
                (group-id . "8209413637")
                (gateway-account-id . "slot-a")))
             (newest (copy-tree base))
             request)
        (setf (alist-get 'server-id newest) "7348923749823749824")
        (setq timeline (list base newest))
        (setq request
              (qq-native-mark-message-read
               base
               (lambda (_receipt)
                 (setq live-successor
                       (gethash "group:8209413637"
                                qq-native--read-operations)
                       reentered
                       (qq-native-mark-message-read newest))
                 (setq reentered-is-successor-p
                       (and
                        (eq reentered request)
                        (eq reentered
                            (qq-native--read-operation-request
                             live-successor)))
                       reentered-active-p
                       (qq-native-request-active-p reentered))
                 (qq-native-cancel-request reentered))))
        (should
         (eq (qq-native-mark-message-read
              newest (lambda (_receipt) (setq queued-callback t)))
             request))
        (funcall (nth 1 (car calls))
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749823")))
        (should (= (length calls) 2))
        (should (eq reentered request))
        (should reentered-is-successor-p)
        (should reentered-active-p)
        (should (equal cancelled '("read-2")))
        (should (eq (qq-native-request-state request) 'cancelled))
        (should-not queued-callback)
        (should (= (hash-table-count qq-native--read-operations) 0))
        ;; The canceled successor's late completion is inert and cannot
        ;; dispatch a hidden third request.
        (funcall (nth 1 (cadr calls))
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749824")))
        (should (= (length calls) 2))))))

(ert-deftest qq-native-read-synchronous-completion-cancels-orphan-token ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        cancelled receipts)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (_message) t))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled)))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (_message &optional callback _errback)
                 (funcall callback
                          '((account_id . "slot-a")
                            (message_id . "7348923749823749823")))
                 "late-read-token")))
      (let ((request
             (qq-native-mark-message-read
              '((session-key . "group:8209413637")
                (server-id . "7348923749823749823")
                (group-id . "8209413637")
                (gateway-account-id . "slot-a"))
              (lambda (receipt) (push receipt receipts)))))
        (should (eq (qq-native-request-state request) 'settled))
        (should (equal cancelled '("late-read-token")))
        (should (equal receipts
                       '(((account_id . "slot-a")
                          (message_id . "7348923749823749823")))))
        (should (= (hash-table-count qq-native--read-operations) 0))
        (should (= (hash-table-count qq-native-request--active) 0))))))

(ert-deftest qq-native-read-leaf-handoff-inhibits-quit ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        (real-create (symbol-function 'qq-native-request-create))
        request cancelled starter-inhibited-p)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (_message) t))
              ((symbol-function 'qq-native-request-create)
               (lambda (&optional owner cancel-function)
                 (setq request
                       (funcall real-create owner cancel-function))))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled)))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (&rest _arguments)
                 (setq starter-inhibited-p inhibit-quit)
                 "read-handoff-token")))
      (should
       (eq
        (qq-native-mark-message-read
         '((session-key . "group:8209413637")
           (server-id . "7348923749823749823")
           (group-id . "8209413637")
           (gateway-account-id . "slot-a")))
        request))
      (should starter-inhibited-p)
      (should (qq-native-request-active-p request))
      (qq-native-cancel-request request)
      (should (eq (qq-native-request-state request) 'cancelled))
      (should (equal cancelled '("read-handoff-token")))
      (should (= (hash-table-count qq-native--read-operations) 0))
      (should (= (hash-table-count qq-native-request--active) 0)))))

(ert-deftest qq-native-account-switch-revokes-stale-read-callback ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        (current-account-id "slot-a")
        calls canceled completed)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () current-account-id))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (message)
                 (equal current-account-id
                        (alist-get 'gateway-account-id message))))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (request) (push request canceled)))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (message &optional callback errback)
                 (setq calls
                       (append calls
                               (list (list message callback errback))))
                 (format "read-%d" (length calls)))))
      (let* ((old
              '((session-key . "group:8209413637")
                (server-id . "7348923749823749823")
                (group-id . "8209413637")
                (gateway-account-id . "slot-a")))
             (new (copy-tree old)))
        (setf (alist-get 'server-id new) "7348923749823749824"
              (alist-get 'gateway-account-id new) "slot-b")
        (qq-native-mark-message-read
         old (lambda (_receipt) (push 'old completed)))
        (qq-native--revoke-read-operations "slot-a" "slot-b")
        (should (equal canceled '("read-1")))
        (setq current-account-id "slot-b")
        (qq-native-mark-message-read
         new (lambda (_receipt) (push 'new completed)))
        (should (= (length calls) 2))
        ;; The retired account's callback cannot settle the new operation.
        (funcall (nth 1 (car calls))
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749823")))
        (should-not completed)
        (should (= (hash-table-count qq-native--read-operations) 1))
        (funcall (nth 1 (cadr calls))
                 '((account_id . "slot-b")
                   (message_id . "7348923749823749824")))
        (should (equal completed '(new)))
        (should (= (hash-table-count qq-native--read-operations) 0))))))

(ert-deftest qq-native-read-coalescer-survives-same-slot-runtime-restart ()
  (let ((qq-native--read-operations (make-hash-table :test #'equal))
        (qq-native-request--active (make-hash-table :test #'eq))
        (current-account-id "slot-a")
        calls cancelled failures completed timeline)
    (cl-letf (((symbol-function 'qq-gateway-current-account-id)
               (lambda () current-account-id))
              ((symbol-function 'qq-native-message-read-capable-p)
               (lambda (message)
                 (equal current-account-id
                        (alist-get 'gateway-account-id message))))
              ((symbol-function 'qq-gateway-transport-cancel)
               (lambda (token) (push token cancelled)))
              ((symbol-function 'qq-state-session-messages)
               (lambda (_session-key) timeline))
              ((symbol-function 'qq-gateway-message-mark-read)
               (lambda (message &optional callback errback)
                 (setq calls
                       (append calls (list (list message callback errback))))
                 (format "read-%d" (length calls)))))
      (let* ((old
              '((session-key . "group:8209413637")
                (server-id . "7348923749823749823")
                (group-id . "8209413637")
                (gateway-account-id . "slot-a")))
             (new (copy-tree old))
             (request
              (qq-native-mark-message-read
               old #'ignore
               (lambda (_body reason) (push reason failures)))))
        (setf (alist-get 'server-id new) "7348923749823749824")
        (setq timeline (list old new))
        ;; A registry update for the same stable slot must not revoke the
        ;; coalescer's outer request.
        (qq-native--revoke-stale-read-operations)
        (should (qq-native-request-active-p request))
        (should-not cancelled)
        (should (eq (qq-native-mark-message-read
                     new (lambda (_receipt) (push 'new completed)))
                    request))
        ;; An in-flight adapter failure reports through the stable outer
        ;; operation and advances its queued cursor.
        (funcall (nth 2 (car calls))
                 '((code . "stale_request")) "runtime changed")
        (should (equal failures '("runtime changed")))
        (should (= (length calls) 2))
        (should (eq (car (cadr calls)) new))
        (funcall (nth 1 (cadr calls))
                 '((account_id . "slot-a")
                   (message_id . "7348923749823749824")))
        (should (equal completed '(new)))
        (should (= (hash-table-count qq-native--read-operations) 0))))))

(ert-deftest qq-native-read-capability-requires-current-account-slot ()
  (let ((message
         '((session-key . "private:10001")
           (server-id . "7348923749823749823")
           (gateway-account-id . "slot-a"))))
    (cl-letf (((symbol-function 'qq-native-ready-p) (lambda () t))
              ((symbol-function 'qq-gateway-transport-capabilities)
               (lambda () '("message.mark_read")))
              ((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-current-account)
               (lambda () '((phase . "online")))))
      (should (qq-native-message-read-capable-p message))
      (let ((other-account (copy-tree message)))
        (setf (alist-get 'gateway-account-id other-account) "slot-b")
        (should-not (qq-native-message-read-capable-p other-account)))
      (let ((missing-id (copy-tree message)))
        (setf (alist-get 'server-id missing-id) nil)
        (should-not (qq-native-message-read-capable-p missing-id)))
      (let ((service (copy-tree message)))
        (setf (alist-get 'session-key service) "service:u_peer")
        (should-not (qq-native-message-read-capable-p service))))))

(ert-deftest qq-native-capabilities-follow-negotiated-methods ()
  (cl-letf (((symbol-function 'qq-gateway-transport-capabilities)
             (lambda () '("message.send" "message.mark_read"))))
    (should (qq-native-implemented-p 'presence))
    (should-not (qq-native-supports-p 'presence))
    (should (qq-native-supports-p 'send-message))
    (should (qq-native-supports-p 'read-receipt))
    (should-not (qq-native-implemented-p 'chat-action))
    (should-not (qq-native-supports-p 'chat-action))))

(ert-deftest qq-native-essence-routes-whole-message ()
  (let ((qq-gateway--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-gateway-message-set-essence)
               (lambda (message set &optional callback errback)
                 (setq call (list message set callback errback))
                 "essence-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999")
               (native-random . 7))))
        (let ((request (qq-native-set-message-essence message t)))
          (should (qq-native-request-p request))
          (should (equal (qq-native-request-token request)
                         "essence-request")))
        (should (eq (nth 0 call) message))
        (should (eq (nth 1 call) t))
        (should (functionp (nth 3 call)))))))

(ert-deftest qq-native-todo-routes-whole-message-and-operation ()
  (let ((qq-gateway--current-account-id "slot-a") calls)
    (cl-letf (((symbol-function 'qq-gateway-message-set-todo)
               (lambda (message operation &optional callback errback)
                 (push (list message operation callback errback) calls)
                 (format "todo-%s" operation))))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "9007199254740999"))))
        (dolist (operation '(set complete cancel))
          (let ((request (qq-native-set-message-todo message operation)))
            (should (qq-native-request-p request))
            (should (equal (qq-native-request-token request)
                           (format "todo-%s" operation)))))
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
  (let ((qq-gateway--current-account-id "slot-a") call)
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
        (let ((request (qq-native-recall-poke message)))
          (should (qq-native-request-p request))
          (should (equal (qq-native-request-token request)
                         "poke-recall-request")))
        (should (eq (car call) message))
        (should (functionp (nth 2 call)))))))

(ert-deftest qq-native-recall-dispatches-the-owned-native-message ()
  (let ((qq-gateway--current-account-id "slot-a") call)
    (cl-letf (((symbol-function 'qq-gateway-message-recall)
               (lambda (session-key message &optional callback errback)
                 (setq call (list session-key message callback errback))
                 "recall-request")))
      (let ((message
             '((session-key . "group:8209413637")
               (server-id . "7348923749823749823")
               (message-seq . "99"))))
        (let ((request (qq-native-recall-message message)))
          (should (qq-native-request-p request))
          (should (equal (qq-native-request-token request)
                         "recall-request")))
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
                 (let ((request (qq-native-request-create)))
                   (qq-native-request-finish request)
                   request))))
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
                 (qq-native-request-create))))
      (let ((request
             (qq-native-fetch-history-around
              "private:10001" "7348923749823749823" #'ignore nil 20)))
        (should (qq-native-request-p request)))
      (should
       (equal range
              '("9007199254740990" . "9007199254741009")))
      (qq-native-fetch-history-around
       "private:10001" "missing" #'ignore
       (lambda (_body reason) (setq failure reason)) 20)
      (should (string-match-p "cached message" failure)))))

(ert-deftest qq-native-bootstrap-completion-requires-opaque-attempt-token ()
  (let* ((old-token (list 'old-bootstrap))
         (current-token (list 'current-bootstrap))
         (qq-native--bootstrap-token current-token)
         (qq-native--bootstrap-owner "slot-a")
         (qq-native--bootstrap-instance-id "gateway-a")
         (qq-native--bootstrap-pending 2)
         reported)
    (cl-letf (((symbol-function 'qq-native--default-error)
               (lambda (_body reason) (setq reported reason))))
      (qq-native--bootstrap-success old-token nil)
      (qq-native--bootstrap-failure old-token nil "superseded"))
    (should (= qq-native--bootstrap-pending 2))
    (should-not reported)
    (qq-native--bootstrap-success current-token nil)
    (should (= qq-native--bootstrap-pending 1))))

(ert-deftest qq-native-bootstrap-coalesces-one-owner ()
  (let ((qq-native--bootstrap-owner nil)
        (qq-native--bootstrap-instance-id nil)
        (qq-native--bootstrap-pending 0)
        (qq-native--bootstrap-token nil)
        (qq-native--observed-account-id nil)
        (qq-native--observed-account-phase nil)
        calls)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
              ((symbol-function 'qq-gateway-current-account)
               (lambda () (qq-native-test-account)))
              ((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-transport-gateway-instance-id)
               (lambda () "gateway-a"))
              ((symbol-function 'qq-native-supports-p) (lambda (_capability) t))
              ((symbol-function 'qq-state-friend-categories-loaded-p)
               (lambda () nil))
              ((symbol-function 'qq-state-groups-loaded-p) (lambda () nil))
              ((symbol-function 'qq-native-refresh-recent-conversations)
               (lambda (&optional callback _errback _limit)
                 (push 'recent calls)
                 (when callback (funcall callback nil))))
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
                     '(friends groups recent)))
      (should (equal qq-native--bootstrap-owner
                     "slot-a"))
      (should (equal qq-native--bootstrap-instance-id "gateway-a"))
      (should (= qq-native--bootstrap-pending 0)))))

(ert-deftest qq-native-bootstrap-refreshes-directories-after-runtime-restart ()
  (let ((qq-native--bootstrap-owner "slot-a")
        (qq-native--bootstrap-instance-id "gateway-a")
        (qq-native--bootstrap-pending 0)
        (qq-native--bootstrap-token '(old-bootstrap))
        (qq-native--friend-directory-owner "slot-a")
        (qq-native--group-directory-owner "slot-a")
        (qq-native--observed-account-id "slot-a")
        (qq-native--observed-account-phase "stopped")
        calls)
    (unwind-protect
        (progn
          (qq-state-reset)
          (qq-state-apply-friend-categories
           '(((category_id . "1")
              (category_name . "Old friends")
              (friends . (((user_id . "10001")
                           (nickname . "Alice")))))))
          (qq-state-apply-groups
           '(((group_id . "8209413637")
              (group_name . "Old group"))))
          (cl-letf
              (((symbol-function 'qq-gateway-transport-ready-p)
                (lambda () t))
               ((symbol-function 'qq-gateway-current-account)
                (lambda () (qq-native-test-account)))
               ((symbol-function 'qq-gateway-current-account-id)
                (lambda () "slot-a"))
               ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                (lambda () "gateway-a"))
               ((symbol-function 'qq-native-supports-p)
                (lambda (_capability) t))
               ((symbol-function 'qq-native-refresh-recent-conversations)
                (lambda (&optional _callback _errback _limit)
                  (push '(recent) calls)))
               ((symbol-function 'qq-native-refresh-friend-categories)
                (lambda (_callback _errback &optional refresh)
                  (push (list 'friends refresh) calls)))
               ((symbol-function 'qq-native-refresh-joined-groups)
                (lambda (_callback _errback &optional refresh)
                  (push (list 'groups refresh) calls))))
            (qq-native--maybe-bootstrap)
            (should (member '(friends t) calls))
            (should (member '(groups t) calls))
            ;; Scheduling the newly-online runtime's refresh must not blank the
            ;; still-useful previous projection while the requests are open.
            (should (equal (alist-get 'nickname (qq-state-friend "10001"))
                           "Alice"))
            (should (equal (alist-get 'group_name
                                      (qq-state-group "8209413637"))
                           "Old group"))))
      (qq-state-reset))))

(ert-deftest qq-native-stopped-bootstrap-and-refresh-request-only-recent ()
  (let ((qq-native--bootstrap-owner nil)
        (qq-native--bootstrap-instance-id nil)
        (qq-native--bootstrap-pending 0)
        (qq-native--bootstrap-token nil)
        (qq-native--observed-account-id nil)
        (qq-native--observed-account-phase nil)
        calls)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
              ((symbol-function 'qq-gateway-current-account)
               (lambda ()
                 (let ((account (copy-tree (qq-native-test-account))))
                   (setf (alist-get 'phase account) "stopped")
                   account)))
              ((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-transport-gateway-instance-id)
               (lambda () "gateway-a"))
              ((symbol-function 'qq-native-supports-p) (lambda (_capability) t))
              ((symbol-function 'qq-native-refresh-recent-conversations)
               (lambda (&optional callback _errback _limit)
                 (push 'recent calls)
                 (when callback (funcall callback nil))))
              ((symbol-function 'qq-native-refresh-friend-categories)
               (lambda (&rest _) (push 'friends calls)))
              ((symbol-function 'qq-native-refresh-joined-groups)
               (lambda (&rest _) (push 'groups calls))))
      (qq-native--maybe-bootstrap)
      (should (equal calls '(recent)))
      (setq calls nil)
      (qq-native-refresh)
      (should (equal calls '(recent))))))

(ert-deftest qq-native-lagged-recent-resync-coalesces-in-flight-context ()
  (let ((qq-native--recent-resync-context nil)
        (calls 0)
        success)
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
              ((symbol-function 'qq-native-supports-p) (lambda (_capability) t))
              ((symbol-function 'qq-gateway-current-account-id)
               (lambda () "slot-a"))
              ((symbol-function 'qq-gateway-transport-gateway-instance-id)
               (lambda () "gateway-a"))
              ((symbol-function 'qq-native-refresh-recent-conversations)
               (lambda (callback _errback &optional _limit)
                 (cl-incf calls)
                 (setq success callback))))
      (qq-native--handle-desync)
      (qq-native--handle-desync)
      (should (= calls 1))
      (funcall success nil)
      (should-not qq-native--recent-resync-context)
      (qq-native--handle-desync)
      (should (= calls 2)))))

(ert-deftest qq-v2-has-no-backend-selector ()
  (should-not (boundp 'qq-backend))
  (should-not (fboundp 'qq-switch-backend)))

(provide 'qq-native-test)

;;; qq-native-test.el ends here
