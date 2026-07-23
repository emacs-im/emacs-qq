;;; qq-login-test.el --- Tests for native QQ login interaction -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-login)

(defun qq-login-test-account
    (account-id phase &optional uin challenge problem)
  "Return one test account ACCOUNT-ID in PHASE."
  `((account_id . ,account-id)
    (label)
    (phase . ,phase)
    (uin . ,uin)
    (uid)
    (challenge . ,challenge)
    (problem . ,problem)))

(defmacro qq-login-test-with-state (&rest body)
  "Run BODY with an isolated login interaction and account registry."
  (declare (indent 0) (debug t))
  `(let ((qq-login--current nil)
         (qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--gateway-instance-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-login-change-hook nil)
         (qq-login-open-verification-url nil))
     (unwind-protect
         (progn ,@body)
       (when (qq-login-active-p)
         (qq-login-cancel)))))

(defun qq-login-test-session (&optional account-id create-p label label-read-p)
  "Install and return one active login session."
  (setq qq-login--current
        (qq-login--session-create
         :active-p t
         :account-id account-id
         :create-p create-p
         :label label
         :label-read-p label-read-p
         :retry-failed-p t
         :status "Preparing QQ login…")))

(ert-deftest qq-login-starts-a-stopped-account ()
  (qq-login-test-with-state
    (qq-gateway--upsert-account
     (qq-login-test-account "slot-a" "stopped") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          started)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'qq-gateway-account-start)
                 (lambda (account-id success _failure)
                   (setq started account-id)
                   (funcall success
                            (qq-login-test-account
                             account-id "login_required")))))
        (qq-login--drive session))
      (should (equal started "slot-a"))
      (should-not (qq-login--session-in-flight-p session))
      (should (qq-login-active-p)))))

(ert-deftest qq-login-password-preserves-exact-uin-and-clears-input-secret ()
  (qq-login-test-with-state
    (qq-gateway--upsert-account
     (qq-login-test-account "slot-a" "login_required") 'test)
    (let ((session (qq-login-test-session "slot-a"))
          (password (copy-sequence "correct horse"))
          sent)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'read-string)
                 (lambda (&rest _arguments) "9007199254740993"))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _arguments) password))
                ((symbol-function 'qq-gateway-account-login-password)
                 (lambda (account-id uin secret qimei success _failure)
                   (setq sent
                         (list account-id uin (copy-sequence secret) qimei))
                   (funcall success
                            (qq-login-test-account
                             account-id "logging_in" uin)))))
        (qq-login--drive session))
      (should
       (equal sent
              '("slot-a" "9007199254740993" "correct horse" nil)))
      (should (seq-every-p (lambda (character) (= character 0)) password))
      (should-not (qq-login--session-in-flight-p session)))))

(ert-deftest qq-login-new-device-only-projects-the-rust-owned-qr ()
  (qq-login-test-with-state
    (let* ((challenge
            '((kind . "new_device")
              (challenge_id . "challenge-7")
              (qr_url . "https://example.invalid/scan")))
           (account
            (qq-login-test-account
             "slot-a" "logging_in" "10001" challenge))
           (session (qq-login-test-session "slot-a"))
           rendered)
      (qq-gateway--upsert-account account 'test)
      (let ((qq-login-open-verification-url t))
        (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                   (lambda () t))
                  ((symbol-function 'qq-login--schedule)
                   #'ignore)
                  ((symbol-function 'qq-login--prepare-qr)
                   (lambda (_session url) (setq rendered url)))
                  ((symbol-function 'qq-gateway-rpc-call)
                   (lambda (&rest _arguments)
                     (ert-fail "QR projection sent an RPC"))))
          (qq-login--drive session)))
      (should (equal rendered "https://example.invalid/scan"))
      (should-not (qq-login--session-in-flight-p session)))))

(ert-deftest qq-login-new-device-error-keeps-the-qr-visible ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-in-flight-p session) t
            (qq-login--session-qr-url session)
            "https://example.invalid/scan"
            (qq-login--session-qr-display session) "[QR]\n")
      (qq-login--request-error
       session
       '((code . "login_pipeline_failed"))
       "OIDB polling failed")
      (let ((model (qq-login-view-model)))
        (should (qq-login-active-p))
        (should-not (qq-login--session-in-flight-p session))
        (should (equal (plist-get model :display) "[QR]\n"))
        (should
         (equal (plist-get model :error)
                "Login failed [login_pipeline_failed]: OIDB polling failed"))))))

(ert-deftest qq-login-reentry-does-not-replace-an-in-flight-session ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-in-flight-p session) t
            (qq-login--session-submitted-challenge-id session) "challenge-7"
            (qq-login--session-qr-display session) "[QR]\n"
            (qq-login--session-status session)
            "Waiting for mobile QQ confirmation…")
      (should (eq (qq-login) session))
      (should (eq qq-login--current session))
      (should (qq-login--session-in-flight-p session))
      (should
       (equal (qq-login--session-submitted-challenge-id session)
              "challenge-7"))
      (should (equal (plist-get (qq-login-view-model) :display) "[QR]\n")))))

(ert-deftest qq-login-does-not-automatically-restart-a-failed-attempt ()
  (qq-login-test-with-state
    (qq-gateway--upsert-account
     (qq-login-test-account
      "slot-a" "failed" "10001" nil
      '((code . "login_pipeline_failed")
        (message . "new-device polling failed")))
     'test)
    (let ((session (qq-login-test-session "slot-a")))
      (setf (qq-login--session-retry-failed-p session) nil)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-account-start)
                 (lambda (&rest _arguments)
                   (ert-fail "failed login restarted without user action"))))
        (qq-login--drive session))
      (should (qq-login-active-p))
      (should
       (equal
        (qq-login--session-error session)
        "[login_pipeline_failed] new-device polling failed")))))

(ert-deftest qq-login-terminal-qr-rendering-never-opens-the-url ()
  (let (arguments)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _display) nil))
              ((symbol-function 'executable-find)
               (lambda (_program) "/test/qrencode"))
              ((symbol-function 'call-process)
               (lambda (_program _infile destination _display &rest args)
                 (setq arguments args)
                 (when (eq destination t)
                   (insert "[terminal QR]\n"))
                 0)))
      (pcase-let ((`(,display . ,file)
                   (qq-login--render-qr "https://example.invalid/scan")))
        (should (equal display "[terminal QR]\n"))
        (should-not file)
        (should (equal arguments
                       '("-m" "1" "-t" "UTF8"
                         "https://example.invalid/scan")))))))

(ert-deftest qq-login-new-account-remembers-an-empty-label-choice ()
  (qq-login-test-with-state
    (let ((session (qq-login-test-session nil t nil t))
          created-label)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-login--schedule)
                 #'ignore)
                ((symbol-function 'qq-login--read-label)
                 (lambda () (ert-fail "label was read twice")))
                ((symbol-function 'qq-gateway-account-create)
                 (lambda (label success _failure)
                   (setq created-label label)
                   (let ((snapshot
                          (qq-login-test-account
                           "slot-new" "stopped")))
                     (qq-gateway--upsert-account snapshot 'response)
                     (funcall success snapshot)))))
        (qq-login--drive session))
      (should-not created-label)
      (should (equal (qq-login--session-account-id session) "slot-new"))
      (should-not (qq-login--session-create-p session))
      (should (equal (qq-gateway-current-account-id) "slot-new")))))

(ert-deftest qq-login-finishes-when-account-is-online ()
  (qq-login-test-with-state
    (qq-gateway--upsert-account
     (qq-login-test-account "slot-a" "online" "10001") 'test)
    (let ((session (qq-login-test-session "slot-a")))
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t)))
        (qq-login--drive session))
      (should-not (qq-login--session-active-p session))
      (should-not qq-login--current))))

(provide 'qq-login-test)

;;; qq-login-test.el ends here
