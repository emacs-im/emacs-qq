;;; qq-gateway-test.el --- Tests for native QQ account client -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway)

(defconst qq-gateway-test-capabilities
  '("account.list" "account.create" "account.status" "account.set_presence"
    "account.start"
    "account.login.password" "account.login.captcha"
    "account.login.new_device" "account.login.unusual_device"
    "account.stop" "account.logout" "account.remove"))

(defun qq-gateway-test-account
    (account-id &optional phase uin label challenge problem)
  "Return a closed test snapshot for ACCOUNT-ID."
  `((account_id . ,account-id)
    (label . ,label)
    (phase . ,(or phase "stopped"))
    (uin . ,uin)
    (uid)
    (challenge . ,challenge)
    (problem . ,problem)))

(defmacro qq-gateway-test-with-state (&rest body)
  "Run BODY with an isolated client-side Gateway registry."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--gateway-instance-id nil)
         (qq-gateway--refresh-owner nil)
         (qq-gateway--resync-request-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-gateway-ready-hook nil)
         (qq-gateway-desync-hook nil))
     ,@body))

(ert-deftest qq-gateway-uint64-wire-predicate-is-exact-and-bounded ()
  (should (qq-gateway--uint64-decimal-p "1"))
  (should
   (qq-gateway--uint64-decimal-p qq-gateway--max-uint64-decimal))
  (should (qq-gateway--uint64-decimal-p "0" t))
  (dolist (value
           '(nil 0 "0" "00" "01" "-1" "18446744073709551616"))
    (should-not (qq-gateway--uint64-decimal-p value)))
  (dolist (value '("00" "01" "18446744073709551616"))
    (should-not (qq-gateway--uint64-decimal-p value t))))

(ert-deftest qq-gateway-account-registry-preserves-domain-and-owns-accessors ()
  (qq-gateway-test-with-state
    (let* ((account-id (copy-sequence "slot-a"))
           (label (copy-sequence "Primary"))
           (raw
            (append
             (qq-gateway-test-account
              account-id "online" "9007199254740993" label)
             '((future_field . t)))))
      (qq-gateway--replace-accounts (list raw) 'ready "gateway-1")
      (let ((snapshot (qq-gateway-account "slot-a"))
            (selected (qq-gateway-current-account-id)))
        (should (equal (alist-get 'uin snapshot) "9007199254740993"))
        (should (eq (alist-get 'future_field snapshot) t))
        (aset (alist-get 'account_id snapshot) 0 ?X)
        (aset (alist-get 'label snapshot) 0 ?X)
        (aset selected 0 ?X))
      (should (equal (alist-get 'account_id
                                (qq-gateway-account "slot-a"))
                     "slot-a"))
      (should (equal (alist-get 'label (qq-gateway-account "slot-a"))
                     "Primary"))
      (should (equal (qq-gateway-current-account-id) "slot-a"))
      (should (equal account-id "slot-a"))
      (should (equal label "Primary")))))

(ert-deftest qq-gateway-ready-replaces-registry-and-selects-singleton ()
  (qq-gateway-test-with-state
    (let (selection changes ready)
      (add-hook 'qq-gateway-current-account-changed-hook
                (lambda (old new) (push (list old new) selection)))
      (add-hook 'qq-gateway-accounts-changed-hook
                (lambda (reason account-id)
                  (push (list reason account-id) changes)))
      (add-hook 'qq-gateway-ready-hook
                (lambda (instance-id)
                  (setq ready
                        (list instance-id
                              (qq-gateway-current-account-id)
                              (length (qq-gateway-accounts))))))
      (qq-gateway--handle-event
       "gateway.ready"
       `((gateway_instance_id . "gateway-1")
         (accounts . (,(qq-gateway-test-account "slot-a")))))
      (should (equal (qq-gateway-current-account-id) "slot-a"))
      (should (equal selection '((nil "slot-a"))))
      (should (equal changes '((ready nil))))
      (should (equal ready '("gateway-1" "slot-a" 1)))
      (should (= (length (qq-gateway-accounts)) 1)))))

(ert-deftest qq-gateway-ready-does-not-guess-between-multiple-accounts ()
  (qq-gateway-test-with-state
    (qq-gateway--replace-accounts
     (list (qq-gateway-test-account "slot-a")
           (qq-gateway-test-account "slot-b"))
     'ready "gateway-1")
    (should-not (qq-gateway-current-account-id))
    (qq-gateway-account-select "slot-b")
    (qq-gateway--replace-accounts
     (list (qq-gateway-test-account "slot-a" "login_required")
           (qq-gateway-test-account "slot-b" "online" "10002"))
     'ready "gateway-1")
    (should (equal (qq-gateway-current-account-id) "slot-b"))
    (should (equal (alist-get 'phase (qq-gateway-current-account)) "online"))
    (should (equal (alist-get 'uin (qq-gateway-current-account)) "10002"))))

(ert-deftest qq-gateway-ready-cancels-and-settles-pending-account-refresh ()
  (qq-gateway-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'account-refresh-token))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-gateway-refresh-accounts
         nil (lambda (body _reason) (push body failures)))
        (qq-gateway--handle-event
         "gateway.ready"
         '((gateway_instance_id . "gateway-ready") (accounts)))
        (should (equal canceled '(account-refresh-token)))
        (should (= (length failures) 1))
        (should (equal (alist-get 'code (car failures))
                       "superseded_request"))
        (should-not qq-gateway--refresh-owner)
        (funcall late-success
                 `((accounts . [,(qq-gateway-test-account "late-slot")])))
        (should (= (length failures) 1))
        (should-not (qq-gateway-account "late-slot"))))))

(ert-deftest qq-gateway-account-change-replaces-same-slot-stop-start-snapshots ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "stopped" "10001")
     'changed)
    (should (equal (qq-gateway-current-account-id) "slot-a"))
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "starting" "10001")
     'changed)
    (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                   "starting"))
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "stopped" "10001")
     'changed)
    (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                   "stopped"))
    (should (equal (qq-gateway-current-account-id) "slot-a"))
    (should (equal qq-gateway--account-order '("slot-a")))))

(ert-deftest qq-gateway-account-removed-clears-selection-without-stopping-peer ()
  (qq-gateway-test-with-state
    (let (selection)
      (qq-gateway--replace-accounts
       (list (qq-gateway-test-account "slot-a")
             (qq-gateway-test-account "slot-b"))
       'ready "gateway-1")
      (qq-gateway-account-select "slot-a")
      (add-hook 'qq-gateway-current-account-changed-hook
                (lambda (old new) (setq selection (list old new))))
      (qq-gateway--handle-event
       "account.removed" '((account_id . "slot-a")))
      ;; The remaining singleton becomes the local projection; no lifecycle
      ;; command is emitted for it.
      (should (equal (qq-gateway-current-account-id) "slot-b"))
      (should (equal selection '("slot-a" "slot-b")))
      (should-not (qq-gateway-account "slot-a"))
      (should (qq-gateway-account "slot-b")))))

(ert-deftest qq-gateway-account-start-uses-explicit-account-id ()
  (qq-gateway-test-with-state
    (let (sent-method sent-params callback-result)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall callback
                            (qq-gateway-test-account
                             "slot-a" "login_required"))
                   "request-1")))
        (should
         (equal
          (qq-gateway-account-start
           "slot-a" (lambda (snapshot) (setq callback-result snapshot)))
          "request-1"))
        (should (equal sent-method "account.start"))
        (should (equal sent-params '((account_id . "slot-a"))))
        (should (equal (alist-get 'phase callback-result) "login_required"))
        (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                       "login_required"))))))

(ert-deftest qq-gateway-account-presence-validates-account-and-value ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "online" "10001") 'changed)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    '((account_id . "slot-a")
                      (presence
                       (kind . "custom")
                       (face_id . 4294967295)
                       (wording . "writing Emacs Lisp"))))
                   "presence-request")))
        (should
         (equal
          (qq-gateway-account-set-presence
           "slot-a"
           '((kind . "custom") (face_id . 4294967295)
             (wording . "writing Emacs Lisp"))
           (lambda (receipt) (setq delivered receipt)))
          "presence-request"))
        (should (equal sent-method "account.set_presence"))
        (should
         (equal
          sent-params
          '((account_id . "slot-a")
            (presence
             (kind . "custom")
             (face_id . 4294967295)
             (wording . "writing Emacs Lisp")))))
        (should
         (equal delivered
                '((account_id . "slot-a")
                  (presence
                   (kind . "custom")
                   (face_id . 4294967295)
                   (wording . "writing Emacs Lisp")))))
        (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                       "online"))))))

(ert-deftest qq-gateway-account-presence-accepts-receipt-after-slot-update ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "online" "10001") 'changed)
    (let (delivered failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (qq-gateway--upsert-account
                    (qq-gateway-test-account
                     "slot-a" "stopped" "10001")
                    'changed)
                   (funcall callback
                            '((account_id . "slot-a")
                              (presence (kind . "away"))))
                   "presence-request")))
        (qq-gateway-account-set-presence
         "slot-a" '((kind . "away"))
         (lambda (receipt) (setq delivered receipt))
         (lambda (_body reason) (setq failure reason)))
        (should (equal delivered
                       '((account_id . "slot-a")
                         (presence (kind . "away")))))
        (should-not failure)
        (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                       "stopped"))))))

(ert-deftest qq-gateway-password-login-copies-secret-and-keeps-uin-string ()
  (qq-gateway-test-with-state
    (let ((password (copy-sequence "correct horse battery staple"))
          wire)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params _callback _errback &optional _early)
                   (setq wire
                         (qq-gateway-transport--json-encode
                          `((method . ,method) (params . ,params))))
                   "request-2")))
        (should
         (equal
          (qq-gateway-account-login-password
           "slot-a" "9007199254740993" password)
          "request-2"))
        (should (equal password "correct horse battery staple"))
        (let* ((decoded (qq-gateway-transport--json-decode wire))
               (params (alist-get 'params decoded)))
          (should (equal (alist-get 'account_id params) "slot-a"))
          (should (equal (alist-get 'uin params) "9007199254740993"))
          (should (stringp (alist-get 'uin params)))
          (should (equal (alist-get 'password params)
                         "correct horse battery staple")))))))

(ert-deftest qq-gateway-account-remove-consumes-returned-snapshot ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a") 'changed)
    (let (callback-result)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback (qq-gateway-test-account "slot-a"))
                   "request-4")))
        (qq-gateway-account-remove
         "slot-a" (lambda (snapshot) (setq callback-result snapshot)))
        (should (equal (alist-get 'account_id callback-result) "slot-a"))
        (should-not (qq-gateway-account "slot-a"))
        (should-not (qq-gateway-current-account-id))))))

(ert-deftest qq-gateway-event-stream-lag-triggers-one-registry-resync ()
  (qq-gateway-test-with-state
    (let ((calls 0) observed)
      (add-hook 'qq-gateway-desync-hook
                (lambda (body) (setq observed body)))
      (cl-letf (((symbol-function 'qq-gateway-refresh-accounts)
                 (lambda (&rest _)
                   (cl-incf calls)
                   "resync-1")))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed 3")))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed 4")))
        (should (= calls 1))
        (should (equal (car qq-gateway--resync-request-id)
                       'account-resync))
        (should (equal (alist-get 'code observed) "event_stream_lagged"))))))

(ert-deftest qq-gateway-synchronous-resync-settlement-does-not-publish-token ()
  (qq-gateway-test-with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-1"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (cl-incf calls)
                   (funcall callback '((accounts . [])))
                   "already-settled")))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed")))
        (should (= calls 1))
        (should-not qq-gateway--resync-request-id)))))

(ert-deftest qq-gateway-synchronous-preflight-error-clears-resync-marker ()
  (qq-gateway-test-with-state
    (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
               (lambda () nil)))
      (qq-gateway--handle-protocol-error
       '((code . "event_stream_lagged") (message . "missed")))
      (should-not qq-gateway--resync-request-id))))

(ert-deftest qq-gateway-refresh-replaces-registry-authoritatively ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "old-slot") 'changed)
    (let (result)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-2"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback
                            `((accounts .
                               [,(qq-gateway-test-account
                                  "new-slot" "online" "10007")])))
                   "request-5")))
        (qq-gateway-refresh-accounts
         (lambda (accounts) (setq result accounts)))
        (should (= (length result) 1))
        (should-not (qq-gateway-account "old-slot"))
        (should (qq-gateway-account "new-slot"))
        (should (equal qq-gateway--gateway-instance-id "gateway-2"))))))

(ert-deftest qq-gateway-refresh-publishes-owner-before-synchronous-settlement ()
  (qq-gateway-test-with-state
    (let (owner-seen callback-value)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-sync"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (setq owner-seen qq-gateway--refresh-owner)
                   (funcall callback
                            `((accounts .
                               [,(qq-gateway-test-account "slot-sync")])))
                   'opaque-sync-token)))
        (should
         (eq (qq-gateway-refresh-accounts
              (lambda (accounts) (setq callback-value accounts)))
             'opaque-sync-token))
        (should owner-seen)
        (should-not qq-gateway--refresh-owner)
        (should (equal (alist-get 'account_id (car callback-value))
                       "slot-sync"))))))

(ert-deftest qq-gateway-newest-refresh-prevents-out-of-order-registry-rollback ()
  (qq-gateway-test-with-state
    (let (requests first-callback first-error second-callback)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-order"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "request-%d" (length requests))))))
        (should
         (eq (qq-gateway-refresh-accounts
              (lambda (_) (setq first-callback t))
              (lambda (body _failure) (setq first-error body)))
             'request-1))
        (should
         (eq (qq-gateway-refresh-accounts
              (lambda (_) (setq second-callback t)) #'ignore)
             'request-2))
        (funcall (car (nth 1 requests))
                 `((accounts .
                    [,(qq-gateway-test-account "new-slot")])))
        (funcall (car (nth 0 requests))
                 `((accounts .
                    [,(qq-gateway-test-account "old-slot")])))
        (should second-callback)
        (should-not first-callback)
        (should (equal (alist-get 'code first-error)
                       "superseded_request"))
        (should (qq-gateway-account "new-slot"))
        (should-not (qq-gateway-account "old-slot"))
        (should-not qq-gateway--refresh-owner)))))

(ert-deftest qq-gateway-stale-refresh-error-cannot-clear-newer-owner ()
  (qq-gateway-test-with-state
    (let (requests stale-error newer-owner)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-errors"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "error-request-%d" (length requests))))))
        (qq-gateway-refresh-accounts
         nil (lambda (body _failure) (setq stale-error body)))
        (qq-gateway-refresh-accounts nil #'ignore)
        (setq newer-owner qq-gateway--refresh-owner)
        (funcall (cdr (nth 0 requests))
                 '((code . "transport_failure") (message . "old failed"))
                 "old failed")
        (should (eq newer-owner qq-gateway--refresh-owner))
        (should (equal (alist-get 'code stale-error)
                       "superseded_request"))
        (funcall (car (nth 1 requests)) '((accounts . [])))
        (should-not qq-gateway--refresh-owner)))))

(ert-deftest qq-gateway-reentrant-refresh-owns-projection-and-delivery ()
  (qq-gateway-test-with-state
    (let (first-success launched callbacks first-error)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-reentrant"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (if first-success
                       (progn
                         (funcall callback
                                  `((accounts .
                                     [,(qq-gateway-test-account
                                        "inner-slot")])))
                         'inner-request)
                     (setq first-success callback)
                     'outer-request))))
        (add-hook
         'qq-gateway-accounts-changed-hook
         (lambda (reason _account-id)
           (when (and (eq reason 'outer) (not launched))
             (setq launched t)
             (qq-gateway-refresh-accounts
              (lambda (_) (push 'inner callbacks)) #'ignore 'inner))))
        (qq-gateway-refresh-accounts
         (lambda (_) (push 'outer callbacks))
         (lambda (body _failure) (setq first-error body))
         'outer)
        (funcall first-success
                 `((accounts .
                    [,(qq-gateway-test-account "outer-slot")])))
        (should launched)
        (should (equal callbacks '(inner)))
        (should (equal (alist-get 'code first-error)
                       "superseded_request"))
        (should (qq-gateway-account "inner-slot"))
        (should-not (qq-gateway-account "outer-slot"))
        (should-not qq-gateway--refresh-owner)))))

(ert-deftest qq-gateway-manual-refresh-settles-automatic-resync-owner ()
  (qq-gateway-test-with-state
    (let (requests marker canceled)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-marker"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "marker-request-%d" (length requests)))))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "missed")))
        (setq marker qq-gateway--resync-request-id)
        (qq-gateway-refresh-accounts nil #'ignore 'manual)
        ;; The manual request does not mutate MARKER directly.  Claiming it
        ;; cancels request A, whose own superseded errback eq-clears MARKER.
        (should marker)
        (should-not qq-gateway--resync-request-id)
        (should (equal canceled '(marker-request-1)))
        (funcall (car (nth 1 requests))
                 `((accounts .
                    [,(qq-gateway-test-account "manual-slot")])))
        (should-not qq-gateway--resync-request-id)
        (funcall (car (nth 0 requests))
                 `((accounts .
                    [,(qq-gateway-test-account "automatic-slot")])))
        (should-not qq-gateway--resync-request-id)
        (should (qq-gateway-account "manual-slot"))
        (should-not (qq-gateway-account "automatic-slot"))))))

(ert-deftest qq-gateway-lag-replaces-auto-marker-superseded-by-manual-refresh ()
  (qq-gateway-test-with-state
    (let (requests first-marker second-marker canceled)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("account.list")))
                ((symbol-function 'qq-gateway-transport-gateway-instance-id)
                 (lambda () "gateway-resync-marker"))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "resync-request-%d" (length requests)))))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "first")))
        (setq first-marker qq-gateway--resync-request-id)
        (should (qq-gateway-rpc-latest-request-p
                 qq-gateway--refresh-owner))
        ;; A manual authoritative request supersedes the automatic request but
        ;; does not mutate its marker.  The next lag must still be able to
        ;; replace that now-stale marker instead of waiting for request A.
        (qq-gateway-refresh-accounts nil #'ignore 'manual)
        (should-not qq-gateway--resync-request-id)
        (should (equal canceled '(resync-request-1)))
        (qq-gateway--handle-protocol-error
         '((code . "event_stream_lagged") (message . "second")))
        (setq second-marker qq-gateway--resync-request-id)
        (should-not (eq first-marker second-marker))
        (should second-marker)
        (should (= (length requests) 3))
        (should (equal canceled '(resync-request-2 resync-request-1)))
        (funcall (car (nth 0 requests)) '((accounts . [])))
        (should (eq second-marker qq-gateway--resync-request-id))
        (funcall (car (nth 1 requests)) '((accounts . [])))
        (should (eq second-marker qq-gateway--resync-request-id))
        (funcall (car (nth 2 requests)) '((accounts . [])))
        (should-not qq-gateway--resync-request-id)))))

(ert-deftest qq-gateway-account-event-accepts-forward-compatible-domain-fields ()
  (qq-gateway-test-with-state
    (let (violation)
      (cl-letf (((symbol-function 'qq-gateway-transport--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (setq violation (apply #'format format-string arguments)))))
        (qq-gateway-dispatch--handle-transport-event
         "account.changed"
         (append (qq-gateway-test-account "slot-a")
                 '((future_field . t)))))
      (should-not violation)
      (should (eq (alist-get 'future_field
                             (qq-gateway-account "slot-a"))
                  t)))))

(provide 'qq-gateway-test)

;;; qq-gateway-test.el ends here
