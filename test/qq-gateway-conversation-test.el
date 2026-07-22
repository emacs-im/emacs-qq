;;; qq-gateway-conversation-test.el --- Recent conversation adapter tests -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'qq-gateway-conversation)

(defun qq-gateway-conversation-test-account (&optional account-id uin uid phase)
  "Return a managed ACCOUNT-ID fixture for UIN, UID, and PHASE."
  `((account_id . ,(or account-id "slot-a"))
    (label . "Primary")
    (phase . ,(or phase "online"))
    (uin . ,(or uin "10002"))
    (uid . ,(or uid "u_self"))
    (challenge)
    (problem)))

(cl-defun qq-gateway-conversation-test-message
    (&key
     (message-id "7348923749823749823")
     (sequence "9007199254740999")
     (sender '((uin . "10001") (uid . "u_peer")))
     (recipient '((uin . "10002") (uid . "u_self")))
     (conversation '((kind . "private") (name . "Peer"))))
  "Return a closed MESSAGE-ID snapshot.

SEQUENCE, SENDER, RECIPIENT, and CONVERSATION provide its native context."
  `((message_id . ,message-id)
    (sent_at . 1784700000)
    (sender . ,(copy-tree sender))
    (recipient . ,(copy-tree recipient))
    (conversation . ,(copy-tree conversation))
    (sequence . ,sequence)
    (client_sequence . "9007199254741001")
    (random . 7)
    (message_type . 166)
    (sub_type . 0)
    (segments . [((kind . "text") (payload . ((text . "hello"))))])))

(cl-defun qq-gateway-conversation-test-row
    (&key
     (identity '((kind . "private")
                 (peer_uin . "10001") (peer_uid . "u_peer")))
     (revision "12")
     (message (qq-gateway-conversation-test-message))
     (recalled :false)
     (pinned 'absent)
     (read-cursor
      '((read_through_message_id . "7348923749823749822")
        (read_through_sequence . "9007199254740998")
        (server_read_sequence . "9007199254740999"))))
  "Return a closed recent-conversation row for IDENTITY and REVISION.

MESSAGE, RECALLED, PINNED, and READ-CURSOR supply its latest projection
metadata.  The symbol `absent' omits unknown pin state."
  `((conversation . ,(copy-tree identity))
    ,@(unless (eq pinned 'absent) `((pinned . ,pinned)))
    (activity_revision . ,revision)
    (latest_message . ,(copy-tree message))
    (latest_message_recalled . ,recalled)
    ,@(when read-cursor `((read_cursor . ,(copy-tree read-cursor))))))

(cl-defun qq-gateway-conversation-test-page
    (&key
     (account-id "slot-a")
     (rows (list (qq-gateway-conversation-test-row)))
     (truncated :false))
  "Return ACCOUNT-ID's closed page containing ROWS.

TRUNCATED is its exact wire boolean."
  `((account_id . ,account-id)
    (conversations . ,(vconcat (mapcar #'copy-tree rows)))
    (truncated . ,truncated)))

(defmacro qq-gateway-conversation-test-with-state (&rest body)
  "Run BODY with one isolated selected Gateway account."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--gateway-instance-id nil)
         (qq-gateway--resync-request-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook
          '(qq-gateway-conversation-reset))
         (qq-gateway-desync-hook nil)
         (qq-gateway-transport--gateway-instance-id "gateway-a")
         (qq-gateway-transport--capabilities '("conversation.list_recent"))
         (qq-gateway-transport--state 'ready)
         (qq-gateway-conversation--active-request nil))
     (qq-gateway--replace-accounts
      (list (qq-gateway-conversation-test-account)) 'ready "gateway-a")
     (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                (lambda () t))
               ((symbol-function 'qq-gateway-transport-capabilities)
                (lambda () '("conversation.list_recent"))))
       ,@body)))

(ert-deftest qq-gateway-conversation-requests-slot-and-returns-domain-page ()
  (qq-gateway-conversation-test-with-state
    (let (method params success delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (request-method request-params callback _errback
                          &optional _early)
                   (setq method request-method
                         params request-params
                         success callback)
                   "recent-1")))
        (should (equal
                 (qq-gateway-conversation-list-recent
                  (lambda (page) (setq delivered page)) nil 37)
                 "recent-1"))
        (should (equal method "conversation.list_recent"))
        (should (equal params '((account_id . "slot-a") (limit . 37))))
        (funcall success (qq-gateway-conversation-test-page)))
      (should (null qq-gateway-conversation--active-request))
      (should (proper-list-p (alist-get 'conversations delivered)))
      (let* ((row (car (alist-get 'conversations delivered)))
             (message (alist-get 'latest_message row))
             (cursor (alist-get 'read_cursor row)))
        (should-not (assq 'latest_message_generation row))
        (should-not (assq 'generation cursor))
        (should (proper-list-p (alist-get 'segments message)))
        (should (equal (alist-get 'message_id message)
                       "7348923749823749823"))))))

(ert-deftest qq-gateway-conversation-decoder-preserves-optional-pinned-state ()
  (qq-gateway-conversation-test-with-state
    (let* ((owner "slot-a")
           (pinned-page
            (qq-gateway-conversation-test-page
             :rows (list (qq-gateway-conversation-test-row :pinned t))))
           (unknown-page
            (qq-gateway-conversation-test-page
             :rows (list (qq-gateway-conversation-test-row))))
           (pinned-row
            (car (alist-get 'conversations
                            (qq-gateway-conversation--validate-page
                             pinned-page owner))))
           (unknown-row
            (car (alist-get 'conversations
                            (qq-gateway-conversation--validate-page
                             unknown-page owner)))))
      (should (eq (alist-get 'pinned pinned-row) t))
      (should-not (assq 'pinned unknown-row))
      (let* ((bad-row (qq-gateway-conversation-test-row :pinned nil))
             (bad-page
              (qq-gateway-conversation-test-page :rows (list bad-row))))
        (should-error
         (qq-gateway-conversation--validate-page bad-page owner))))))

(ert-deftest qq-gateway-conversation-decoder-preserves-stable-account-page ()
  (qq-gateway-conversation-test-with-state
    (let* ((page (qq-gateway-conversation-test-page))
           (validated
            (qq-gateway-conversation--validate-page page "slot-a"))
           (row (car (alist-get 'conversations validated)))
           (cursor (alist-get 'read_cursor row)))
      (should (equal (alist-get 'account_id validated) "slot-a"))
      (should-not (assq 'generation validated))
      (should-not (assq 'latest_message_generation row))
      (should-not (assq 'generation cursor)))))

(ert-deftest qq-gateway-conversation-decoder-is-closed-and-rejects-generation ()
  (qq-gateway-conversation-test-with-state
    (let ((owner "slot-a"))
      (let ((page (qq-gateway-conversation-test-page)))
        (push '(extra . t) page)
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let ((page (append (qq-gateway-conversation-test-page)
                          '((generation . "8")))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((row (append (qq-gateway-conversation-test-row)
                          '((latest_message_generation . "8"))))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((cursor
              '((generation . "8")
                (read_through_message_id . "7348923749823749823")
                (read_through_sequence . "10")))
             (row (qq-gateway-conversation-test-row :read-cursor cursor))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((cursor
              '((read_through_message_id . "7348923749823749823")
                (read_through_sequence . "10")
                (server_read_sequence . "9")))
             (row (qq-gateway-conversation-test-row :read-cursor cursor))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((message (qq-gateway-conversation-test-message
                       :message-id 7348923749823749823))
             (row (qq-gateway-conversation-test-row :message message))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner))))))

(ert-deftest qq-gateway-conversation-decoder-validates-identity-contract ()
  (qq-gateway-conversation-test-with-state
    (let ((owner "slot-a"))
      (let* ((row (qq-gateway-conversation-test-row
                   :identity '((kind . "private"))))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((message
              (qq-gateway-conversation-test-message
               :conversation
               '((kind . "group") (group_uin . "8209413637"))))
             (row (qq-gateway-conversation-test-row :message message))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner)))
      (let* ((identity '((kind . "group") (group_uin . "8209413637")))
             (message
              (qq-gateway-conversation-test-message
               :conversation
               '((kind . "group") (group_uin . "8209413638"))))
             (row (qq-gateway-conversation-test-row
                   :identity identity :message message))
             (page (qq-gateway-conversation-test-page :rows (list row))))
        (should-error
         (qq-gateway-conversation--validate-page page owner))))))

(ert-deftest qq-gateway-conversation-limit-is-closed-before-send ()
  (qq-gateway-conversation-test-with-state
    (let ((sent 0)
          (qq-recent-contact-count 23))
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (&rest _arguments) (cl-incf sent))))
        (dolist (invalid '(0 501 1.5 "10"))
          (should-error
           (qq-gateway-conversation-list-recent nil nil invalid)
           :type 'user-error))
        (qq-gateway-conversation-list-recent nil nil)
        (should (= sent 1))))))

(ert-deftest qq-gateway-conversation-stale-owner-or-instance-cannot-deliver ()
  (dolist (stale-kind '(owner instance))
    (qq-gateway-conversation-test-with-state
      (let (success delivered error-body)
        (cl-letf (((symbol-function 'qq-gateway-transport-send)
                   (lambda (_method _params callback _errback &optional _early)
                     (setq success callback)
                     "recent-stale")))
          (qq-gateway-conversation-list-recent
           (lambda (page) (setq delivered page))
           (lambda (body _reason) (setq error-body body))
           10)
          (pcase stale-kind
            ('owner
             (puthash
              "slot-b"
              (qq-gateway-conversation-test-account
               "slot-b" "10003" "u_other")
              qq-gateway--accounts)
             (setq qq-gateway--current-account-id "slot-b"))
            ('instance
             (setq qq-gateway-transport--gateway-instance-id "gateway-b")))
          (funcall success (qq-gateway-conversation-test-page)))
        (should-not delivered)
        (should (equal (alist-get 'code error-body) "superseded_request"))
        (should-not qq-gateway-conversation--active-request)))))

(ert-deftest qq-gateway-conversation-same-slot-restart-can-deliver-current-page ()
  (qq-gateway-conversation-test-with-state
    (let (success delivered failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq success callback)
                   "recent-restart")))
        (qq-gateway-conversation-list-recent
         (lambda (page) (setq delivered page))
         (lambda (body _reason) (setq failure body)) 10)
        ;; The stable slot restarted while this slot-scoped request was queued.
        (puthash "slot-a"
                 (qq-gateway-conversation-test-account
                  "slot-a" "10002" "u_self" "stopped")
                 qq-gateway--accounts)
        (puthash "slot-a"
                 (qq-gateway-conversation-test-account
                  "slot-a" "10002" "u_self" "online")
                 qq-gateway--accounts)
        (funcall
         success
         (qq-gateway-conversation-test-page
          :rows
          (list (qq-gateway-conversation-test-row
                 :read-cursor
                 '((read_through_message_id . "7348923749823749822")
                   (read_through_sequence . "9007199254740998")))))))
      (should delivered)
      (should-not failure)
      (should (equal (alist-get 'account_id delivered) "slot-a")))))

(ert-deftest qq-gateway-conversation-new-request-supersedes-old-delivery ()
  (qq-gateway-conversation-test-with-state
    (let (requests cancelled first-error first-value second-value)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (let ((token (format "recent-%d" (1+ (length requests)))))
                     (setq requests
                           (append requests
                                   (list (list token callback errback))))
                     token)))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token cancelled) t)))
        (qq-gateway-conversation-list-recent
         (lambda (page) (setq first-value page))
         (lambda (body _reason) (setq first-error body)) 10)
        (qq-gateway-conversation-list-recent
         (lambda (page) (setq second-value page)) #'ignore 20)
        (should (equal cancelled '("recent-1")))
        (should (equal (alist-get 'code first-error) "superseded_request"))
        ;; A transport implementation may still race a callback after local
        ;; cancellation; revoked ownership keeps it inert.
        (funcall (nth 1 (car requests))
                 (qq-gateway-conversation-test-page))
        (should-not first-value)
        (funcall (nth 1 (cadr requests))
                 (qq-gateway-conversation-test-page)))
      (should second-value)
      (should-not qq-gateway-conversation--active-request))))

(ert-deftest qq-gateway-conversation-synchronous-settlement-cancels-token ()
  (dolist (settlement '(success failure cancel))
    (qq-gateway-conversation-test-with-state
      (let (cancelled delivered failure)
        (cl-letf (((symbol-function 'qq-gateway-transport-send)
                   (lambda (_method _params callback errback &optional _early)
                     (pcase settlement
                       ('success
                        (funcall callback
                                 (qq-gateway-conversation-test-page)))
                       ('failure
                        (funcall errback '((code . "server_failure")) "boom"))
                       ('cancel
                        (qq-gateway-conversation-reset)))
                     "synchronous-orphan"))
                  ((symbol-function 'qq-gateway-transport-cancel)
                   (lambda (token) (push token cancelled) t)))
          (should (equal
                   (qq-gateway-conversation-list-recent
                    (lambda (page) (setq delivered page))
                    (lambda (body _reason) (setq failure body))
                    10)
                   "synchronous-orphan")))
        (should (equal cancelled '("synchronous-orphan")))
        (should-not qq-gateway-conversation--active-request)
        (pcase settlement
          ('success (should delivered))
          ('failure
           (should (equal (alist-get 'code failure) "server_failure")))
          ('cancel
           (should-not delivered)
           (should-not failure)))))))

(ert-deftest qq-gateway-conversation-reentrant-supersession-cancels-token ()
  (qq-gateway-conversation-test-with-state
    (let ((send-depth 0)
          cancelled
          first-error)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params _callback _errback &optional _early)
                   (cl-incf send-depth)
                   (if (= send-depth 1)
                       (progn
                         (qq-gateway-conversation-list-recent nil #'ignore 20)
                         "superseded-orphan")
                     "replacement-live")))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token cancelled) t)))
        (should (equal
                 (qq-gateway-conversation-list-recent
                  nil
                  (lambda (body _reason) (setq first-error body))
                  10)
                 "superseded-orphan")))
      (should (equal cancelled '("superseded-orphan")))
      (should (equal (alist-get 'code first-error) "superseded_request"))
      (should
       (equal
        (qq-gateway-conversation--request-transport-token
         qq-gateway-conversation--active-request)
        "replacement-live")))))

(ert-deftest qq-gateway-conversation-reset-cancels-and-revokes-callback ()
  (qq-gateway-conversation-test-with-state
    (let (success cancelled delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq success callback)
                   "recent-reset"))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (setq cancelled token) t)))
        (qq-gateway-conversation-list-recent
         (lambda (page) (setq delivered page)) #'ignore 10)
        (qq-gateway-conversation-reset)
        (funcall success (qq-gateway-conversation-test-page)))
      (should (equal cancelled "recent-reset"))
      (should-not delivered)
      (should-not qq-gateway-conversation--active-request))))

(ert-deftest qq-gateway-conversation-adapter-cancel-clears-active-marker ()
  (qq-gateway-conversation-test-with-state
    (let (success cancelled delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq success callback)
                   "recent-cancel"))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (setq cancelled token) t)))
        (let ((token
               (qq-gateway-conversation-list-recent
                (lambda (page) (setq delivered page)) #'ignore 10)))
          (should (equal token "recent-cancel"))
          (should qq-gateway-conversation--active-request)
          (should (qq-gateway-conversation-cancel token))
          (should-not qq-gateway-conversation--active-request)
          (funcall success (qq-gateway-conversation-test-page))))
      (should (equal cancelled "recent-cancel"))
      (should-not delivered))))

(ert-deftest qq-gateway-conversation-slot-switch-cancels-active-request ()
  (qq-gateway-conversation-test-with-state
    (let (cancelled)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (&rest _arguments) "recent-switch"))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (setq cancelled token) t)))
        (qq-gateway-conversation-list-recent nil #'ignore 10)
        (qq-gateway--upsert-account
         (qq-gateway-conversation-test-account
          "slot-b" "10003" "u_other")
         'changed)
        (qq-gateway-account-select "slot-b"))
      (should (equal cancelled "recent-switch"))
      (should-not qq-gateway-conversation--active-request))))

(ert-deftest qq-gateway-conversation-transport-signal-revokes-ownership ()
  (qq-gateway-conversation-test-with-state
    (cl-letf (((symbol-function 'qq-gateway-transport-send)
               (lambda (&rest _arguments) (error "transport exploded"))))
      (should-error (qq-gateway-conversation-list-recent nil #'ignore 10))
      (should-not qq-gateway-conversation--active-request))))

(provide 'qq-gateway-conversation-test)
;;; qq-gateway-conversation-test.el ends here
