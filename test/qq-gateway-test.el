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
    (account-id &optional generation phase uin label challenge problem)
  "Return a closed test snapshot for ACCOUNT-ID."
  `((account_id . ,account-id)
    (label . ,label)
    (phase . ,(or phase "stopped"))
    (uin . ,uin)
    (uid)
    (generation . ,(or generation "0"))
    (challenge . ,challenge)
    (problem . ,problem)))

(defmacro qq-gateway-test-with-state (&rest body)
  "Run BODY with an isolated client-side Gateway registry."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--gateway-instance-id nil)
         (qq-gateway--resync-request-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-gateway-desync-hook nil))
     ,@body))

(ert-deftest qq-gateway-account-validator-preserves-exact-identities ()
  (let* ((raw (qq-gateway-test-account
               "slot-a" "9007199254740999" "online"
               "9007199254740993" "Primary"))
         (snapshot (qq-gateway--validate-account raw)))
    (should (equal (alist-get 'uin snapshot) "9007199254740993"))
    (should (equal (alist-get 'generation snapshot) "9007199254740999"))
    (setf (alist-get 'uin raw) 9007199254740993)
    (should-error (qq-gateway--validate-account raw))))

(ert-deftest qq-gateway-account-validator-rejects-open-shapes ()
  (let ((raw (append (qq-gateway-test-account "slot-a")
                     '((future_field . t)))))
    (should-error (qq-gateway--validate-account raw))))

(ert-deftest qq-gateway-ready-replaces-registry-and-selects-singleton ()
  (qq-gateway-test-with-state
    (let (selection changes)
      (add-hook 'qq-gateway-current-account-changed-hook
                (lambda (old new) (push (list old new) selection)))
      (add-hook 'qq-gateway-accounts-changed-hook
                (lambda (reason account-id)
                  (push (list reason account-id) changes)))
      (qq-gateway--handle-event
       "gateway.ready"
       `((gateway_instance_id . "gateway-1")
         (accounts . (,(qq-gateway-test-account "slot-a")))))
      (should (equal (qq-gateway-current-account-id) "slot-a"))
      (should (equal selection '((nil "slot-a"))))
      (should (equal changes '((ready nil))))
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
     (list (qq-gateway-test-account "slot-a" "1" "login_required")
           (qq-gateway-test-account "slot-b" "3" "online" "10002"))
     'ready "gateway-1")
    (should (equal (qq-gateway-current-account-id) "slot-b"))
    (should (equal (qq-gateway-current-account-owner) '("slot-b" . "3")))))

(ert-deftest qq-gateway-account-change-ignores-older-generation ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "10" "online" "10001")
     'changed)
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "9" "failed" "10001")
     'changed)
    (let ((stored (qq-gateway-account "slot-a")))
      (should (equal (alist-get 'generation stored) "10"))
      (should (equal (alist-get 'phase stored) "online")))))

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
                             "slot-a" "1" "login_required"))
                   "request-1")))
        (should
         (equal
          (qq-gateway-account-start
           "slot-a" (lambda (snapshot) (setq callback-result snapshot)))
          "request-1"))
        (should (equal sent-method "account.start"))
        (should (equal sent-params '((account_id . "slot-a"))))
        (should (equal (alist-get 'phase callback-result) "login_required"))
        (should (equal (alist-get 'generation
                                  (qq-gateway-account "slot-a"))
                       "1"))))))

(ert-deftest qq-gateway-account-presence-is-owned-by-explicit-generation ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "7" "online" "10001") 'changed)
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
                      (generation . "7")
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
                  (generation . "7")
                  (presence
                   (kind . "custom")
                   (face_id . 4294967295)
                   (wording . "writing Emacs Lisp")))))
        (should (equal (alist-get 'phase (qq-gateway-account "slot-a"))
                       "online"))))))

(ert-deftest qq-gateway-account-presence-rejects-stale-acknowledgement ()
  (qq-gateway-test-with-state
    (qq-gateway--upsert-account
     (qq-gateway-test-account "slot-a" "7" "online" "10001") 'changed)
    (let (failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (qq-gateway--upsert-account
                    (qq-gateway-test-account
                     "slot-a" "8" "online" "10001")
                    'changed)
                   (funcall callback
                            '((account_id . "slot-a")
                              (generation . "7")
                              (presence (kind . "away"))))
                   "presence-request")))
        (qq-gateway-account-set-presence
         "slot-a" '((kind . "away")) nil
         (lambda (_body reason) (setq failure reason)))
        (should
         (string-match-p "generation changed" failure))))))

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

(ert-deftest qq-gateway-login-response-cannot-switch-account ()
  (qq-gateway-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall callback
                            (qq-gateway-test-account
                             "slot-b" "1" "logging_in" "10002"))
                   "request-3")))
        (qq-gateway-account-login-password
         "slot-a" "10001" "secret" nil nil
         (lambda (_body reason) (setq failure reason)))
        (should (string-match-p "contradicts request" failure))
        (should-not (qq-gateway-account "slot-a"))
        (should-not (qq-gateway-account "slot-b"))))))

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
        (should (equal qq-gateway--resync-request-id "resync-1"))
        (should (equal (alist-get 'code observed) "event_stream_lagged"))))))

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
                               (,(qq-gateway-test-account
                                  "new-slot" "7" "online" "10007")))))
                   "request-5")))
        (qq-gateway-refresh-accounts
         (lambda (accounts) (setq result accounts)))
        (should (= (length result) 1))
        (should-not (qq-gateway-account "old-slot"))
        (should (qq-gateway-account "new-slot"))
        (should (equal qq-gateway--gateway-instance-id "gateway-2"))))))

(ert-deftest qq-gateway-malformed-domain-event-reconnects-transport ()
  (qq-gateway-test-with-state
    (let (violation)
      (cl-letf (((symbol-function 'qq-gateway-transport--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (setq violation (apply #'format format-string arguments)))))
        (qq-gateway--handle-event
         "account.changed" '((account_id . "open-shape")))
        (should (string-match-p "Malformed account.changed event" violation))))))

(provide 'qq-gateway-test)

;;; qq-gateway-test.el ends here
