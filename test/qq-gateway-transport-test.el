;;; qq-gateway-transport-test.el --- Tests for native Gateway transport -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-transport)

(defmacro qq-gateway-transport-test-with-state (&rest body)
  "Run BODY with isolated native Gateway transport state."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway-transport--ws 'test-websocket)
         (qq-gateway-transport--connecting nil)
         (qq-gateway-transport--connection-owner nil)
         (qq-gateway-transport--stopping nil)
         (qq-gateway-transport--pending (make-hash-table :test #'equal))
         (qq-gateway-transport--request-counter 0)
         (qq-gateway-transport--reconnect-timer nil)
         (qq-gateway-transport--reconnect-attempt 0)
         (qq-gateway-transport--ready-timer nil)
         (qq-gateway-transport--state 'ready)
         (qq-gateway-transport--hello-instance-id "gateway-1")
         (qq-gateway-transport--gateway-instance-id "gateway-1")
         (qq-gateway-transport--capabilities '("account.list"))
         (qq-gateway-transport--ready-accounts nil)
         (qq-gateway-transport-event-hook nil)
         (qq-gateway-transport-protocol-error-hook nil)
         (qq-gateway-transport-state-hook nil)
         (qq-native-request-timeout nil)
         (qq-native-ready-timeout nil))
     ,@body))

(ert-deftest qq-gateway-transport-token-file-matches-server-policy ()
  (let ((path (make-temp-file "qq-gateway-token-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789abcdef0123456789abcdef\n"))
          (set-file-modes path #o600)
          (let ((qq-native-auth-token-file path))
            (should
             (equal (qq-gateway-transport--read-auth-token)
                    "0123456789abcdef0123456789abcdef"))))
      (delete-file path))))

(ert-deftest qq-gateway-transport-token-file-rejects-weak-secret ()
  (let ((path (make-temp-file "qq-gateway-token-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "too-short\n"))
          (set-file-modes path #o600)
          (let ((qq-native-auth-token-file path))
            (should-error (qq-gateway-transport--read-auth-token)
                          :type 'user-error)))
      (delete-file path))))

(ert-deftest qq-gateway-transport-token-file-rejects-public-mode ()
  (skip-unless (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix
                                   darwin cygwin)))
  (let ((path (make-temp-file "qq-gateway-token-")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "0123456789abcdef0123456789abcdef\n"))
          (set-file-modes path #o640)
          (let ((qq-native-auth-token-file path))
            (should-error (qq-gateway-transport--read-auth-token)
                          :type 'user-error)))
      (delete-file path))))

(ert-deftest qq-gateway-transport-send-uses-native-envelope ()
  (qq-gateway-transport-test-with-state
    (let (wire)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (_ws text) (setq wire text))))
        (let* ((id (qq-gateway-transport-send
                    "account.list" (make-hash-table :test #'equal)))
               (decoded (qq-gateway-transport--json-decode wire)))
          (should (equal (alist-get 'kind decoded) "request"))
          (should (equal (alist-get 'id decoded) id))
          (should (equal (alist-get 'method decoded) "account.list"))
          (should (equal (alist-get 'params decoded) nil))
          (should-not (assq 'action decoded))
          (should-not (assq 'echo decoded)))))))

(ert-deftest qq-gateway-transport-request-waits-for-ready ()
  (qq-gateway-transport-test-with-state
    (let ((qq-gateway-transport--state 'authenticating)
          reason sent)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (setq sent t))))
        (should-not
         (qq-gateway-transport-send
          "account.list" nil nil
          (lambda (_body failure) (setq reason failure))))
        (should (equal reason "Gateway handshake is not ready"))
        (should-not sent)
        (should (= 0 (hash-table-count qq-gateway-transport--pending)))))))

(ert-deftest qq-gateway-transport-response-completes-exactly-once ()
  (qq-gateway-transport-test-with-state
    (let (wire result (calls 0))
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (_ws text) (setq wire text))))
        (let ((id (qq-gateway-transport-send
                   "account.list" nil
                   (lambda (value)
                     (cl-incf calls)
                     (setq result value)))))
          (qq-gateway-transport--handle-payload
           `((kind . "response") (id . ,id)
             (result . ((accounts . [])))))
          (qq-gateway-transport--handle-payload
           `((kind . "response") (id . ,id)
             (result . ((accounts . [])))))
          (should wire)
          (should (= calls 1))
          (should (equal (alist-get 'accounts result) []))
          (should-not (gethash id qq-gateway-transport--pending)))))))

(ert-deftest qq-gateway-transport-error-completes-request ()
  (qq-gateway-transport-test-with-state
    (let (failure body)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore))
        (let ((id
               (qq-gateway-transport-send
                "account.status" '((account_id . "missing")) nil
                (lambda (error-body reason)
                  (setq body error-body failure reason)))))
          (qq-gateway-transport--handle-payload
           `((kind . "error") (id . ,id)
             (error . ((code . "account_not_found")
                       (message . "unknown account")))))
          (should (equal failure "unknown account"))
          (should (equal (alist-get 'code body) "account_not_found"))
          (should-not (gethash id qq-gateway-transport--pending)))))))

(ert-deftest qq-gateway-transport-unsolicited-error-has-separate-hook ()
  (qq-gateway-transport-test-with-state
    (let (observed)
      (add-hook 'qq-gateway-transport-protocol-error-hook
                (lambda (body) (setq observed body)))
      (qq-gateway-transport--handle-payload
       '((kind . "error") (id)
         (error . ((code . "event_stream_lagged")
                   (message . "missed 2 events")))))
      (should (equal (alist-get 'code observed) "event_stream_lagged"))
      (should (= 0 (hash-table-count qq-gateway-transport--pending))))))

(ert-deftest qq-gateway-transport-unsolicited-wire-null-id-is-accepted ()
  (qq-gateway-transport-test-with-state
    (let (observed)
      (add-hook 'qq-gateway-transport-protocol-error-hook
                (lambda (body) (setq observed body)))
      (qq-gateway-transport--handle-payload
       (qq-gateway-transport--json-decode
        "{\"kind\":\"error\",\"id\":null,\"error\":{\"code\":\"event_stream_lagged\",\"message\":\"missed\"}}"))
      (should (equal (alist-get 'code observed) "event_stream_lagged"))
      (should-not (qq-gateway-wire-null-p observed)))))

(ert-deftest qq-gateway-transport-hooks-own-deep-copies ()
  (qq-gateway-transport-test-with-state
    (let* ((source (copy-sequence "account-1"))
           (qq-gateway-transport-event-hook
            (list (lambda (_event data)
                    (aset (alist-get 'account_id data) 0 ?X))
                  (lambda (_event data)
                    (should (equal (alist-get 'account_id data)
                                   "account-1"))))))
      (qq-gateway-transport--run-hook
       'qq-gateway-transport-event-hook
       "account.changed" `((account_id . ,source)))
      (should (equal source "account-1")))))

(ert-deftest qq-gateway-transport-metadata-accessors-own-strings ()
  (qq-gateway-transport-test-with-state
    (let* ((instance (copy-sequence "gateway-1"))
           (capability (copy-sequence "account.list"))
           (account-id (copy-sequence "account-1"))
           (qq-gateway-transport--gateway-instance-id instance)
           (qq-gateway-transport--capabilities (list capability))
           (qq-gateway-transport--ready-accounts
            `(((account_id . ,account-id)))))
      (let ((returned-instance
             (qq-gateway-transport-gateway-instance-id))
            (returned-capabilities
             (qq-gateway-transport-capabilities))
            (returned-accounts
             (qq-gateway-transport-ready-accounts)))
        (aset returned-instance 0 ?G)
        (aset (car returned-capabilities) 0 ?A)
        (aset (alist-get 'account_id (car returned-accounts)) 0 ?Q))
      (should (equal instance "gateway-1"))
      (should (equal capability "account.list"))
      (should (equal account-id "account-1")))))

(ert-deftest qq-gateway-transport-consumer-hook-error-is-isolated ()
  (qq-gateway-transport-test-with-state
    (let (later-called)
      (add-hook 'qq-gateway-transport-event-hook
                (lambda (&rest _) (setq later-called t)) 90)
      (add-hook 'qq-gateway-transport-event-hook
                (lambda (&rest _) (error "broken consumer")) 10)
      (should
       (condition-case nil
           (progn
             (qq-gateway-transport--handle-payload
              '((kind . "event") (event . "account.changed")
                (data . ((account_id . "a-1")))))
             t)
         (error nil)))
      ;; One broken consumer cannot suppress later consumers or cause a
      ;; reconnect by masquerading as a malformed wire envelope.
      (should later-called)
      (should (eq qq-gateway-transport--state 'ready)))))

(ert-deftest qq-gateway-transport-handshake-requires-authoritative-ready ()
  (let ((qq-gateway-transport--ws nil)
        (qq-gateway-transport--connecting nil)
        (qq-gateway-transport--connection-owner nil)
        (qq-gateway-transport--stopping nil)
        (qq-gateway-transport--pending (make-hash-table :test #'equal))
        (qq-gateway-transport--request-counter 0)
        (qq-gateway-transport--reconnect-timer nil)
        (qq-gateway-transport--reconnect-attempt 4)
        (qq-gateway-transport--ready-timer nil)
        (qq-gateway-transport--state 'stopped)
        (qq-gateway-transport--hello-instance-id nil)
        (qq-gateway-transport--gateway-instance-id nil)
        (qq-gateway-transport--capabilities nil)
        (qq-gateway-transport--ready-accounts nil)
        (qq-gateway-transport-event-hook nil)
        (qq-gateway-transport-protocol-error-hook nil)
        (qq-gateway-transport-state-hook nil)
        (qq-native-request-timeout nil)
        (qq-native-ready-timeout nil)
        callbacks
        wire
        events)
    (add-hook 'qq-gateway-transport-event-hook
              (lambda (event data) (push (cons event data) events)))
    (cl-letf (((symbol-function 'qq-gateway-transport--read-auth-token)
               (lambda ()
                 (copy-sequence "0123456789abcdef0123456789abcdef")))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest arguments)
                 (setq callbacks arguments)
                 'gateway-socket))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-send-text)
               (lambda (_ws text) (setq wire text))))
      (qq-gateway-transport-start)
      (should (eq qq-gateway-transport--state 'connecting))
      (funcall (plist-get callbacks :on-open) 'gateway-socket)
      (should (eq qq-gateway-transport--state 'authenticating))
      ;; Model a reconnect attempt already counted by the scheduler.  Merely
      ;; opening/authenticating must not erase it.
      (setq qq-gateway-transport--reconnect-attempt 4)
      (let* ((hello (qq-gateway-transport--json-decode wire))
             (id (alist-get 'id hello)))
        (should (equal (alist-get 'method hello) "gateway.hello"))
        (should (= (alist-get 'protocol_version (alist-get 'params hello)) 3))
        (should
         (equal (alist-get 'auth_token (alist-get 'params hello))
                "0123456789abcdef0123456789abcdef"))
        (qq-gateway-transport--handle-payload
         `((kind . "response") (id . ,id)
           (result . ((protocol_version . 3)
                      (gateway_instance_id . "gateway-42")
                      (capabilities . ["account.list" "account.start"])))))
        ;; A successful hello response alone is not a synchronized session.
        (should (eq qq-gateway-transport--state 'authenticating))
        (should (= qq-gateway-transport--reconnect-attempt 4))
        (qq-gateway-transport--handle-payload
         '((kind . "event") (event . "gateway.ready")
           (data . ((gateway_instance_id . "gateway-42")
                    (accounts . [((account_id . "a-1"))])))))
        (should (eq qq-gateway-transport--state 'ready))
        (should (= qq-gateway-transport--reconnect-attempt 0))
        (should (equal (qq-gateway-transport-gateway-instance-id)
                       "gateway-42"))
        (should
         (equal (alist-get 'account_id
                           (car (qq-gateway-transport-ready-accounts)))
                "a-1"))
        (should (equal (caar events) "gateway.ready"))
        ;; Raw event data retains its array vector until the account-domain
        ;; validator deliberately normalizes it.
        (should (vectorp (alist-get 'accounts (cdar events))))))))

(ert-deftest qq-gateway-transport-hello-result-has-closed-schema ()
  (qq-gateway-transport-test-with-state
    (let (success-callback violations timer-started)
      (cl-letf (((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback _allow-before-ready)
                   (setq success-callback callback)
                   "hello-request"))
                ((symbol-function 'qq-gateway-transport--protocol-violation)
                 (lambda (&rest arguments)
                   (push (apply #'format arguments) violations)))
                ((symbol-function 'qq-gateway-transport--start-ready-timer)
                 (lambda () (setq timer-started t))))
        (qq-gateway-transport--send-hello
         (copy-sequence "0123456789abcdef0123456789abcdef"))
        (funcall
         success-callback
         '((protocol_version . 3)
           (gateway_instance_id . "gateway-42")
           (capabilities . ["account.list" "account.start"])))
        (should timer-started)
        (should (equal qq-gateway-transport--hello-instance-id "gateway-42"))
        (should (equal qq-gateway-transport--capabilities
                       '("account.list" "account.start")))
        (dolist (extra '((generation . 7) (future_field . "unknown")))
          (setq timer-started nil
                qq-gateway-transport--hello-instance-id nil
                qq-gateway-transport--capabilities nil)
          (funcall
           success-callback
           `((protocol_version . 3)
             (gateway_instance_id . "gateway-42")
             (capabilities . ["account.list"])
             ,extra))
          (should-not timer-started)
          (should-not qq-gateway-transport--hello-instance-id)
          (should-not qq-gateway-transport--capabilities))
        (should (= (length violations) 2))
        (should (cl-every (lambda (violation)
                            (equal violation "Malformed gateway.hello result"))
                          violations))))))

(ert-deftest qq-gateway-transport-ready-null-is-not-an-empty-account-array ()
  (qq-gateway-transport-test-with-state
    (let ((qq-gateway-transport--state 'authenticating))
      (should-error
       (qq-gateway-transport--handle-payload
        `((kind . "event") (event . "gateway.ready")
          (data . ((gateway_instance_id . "gateway-1")
                   (accounts . ,qq-gateway-wire-null)))))
       :type 'error)
      (should (eq qq-gateway-transport--state 'authenticating)))))

(ert-deftest qq-gateway-transport-ready-object-is-not-an-account-array ()
  (qq-gateway-transport-test-with-state
    (let ((qq-gateway-transport--state 'authenticating))
      (dolist (object '(nil ((field . "object"))))
        (should-error
         (qq-gateway-transport--handle-payload
          `((kind . "event") (event . "gateway.ready")
            (data . ((gateway_instance_id . "gateway-1")
                     (accounts . ,object)))))
         :type 'error))
      (should (eq qq-gateway-transport--state 'authenticating)))))

(ert-deftest qq-gateway-transport-ready-instance-mismatch-reconnects ()
  (qq-gateway-transport-test-with-state
    (let ((qq-gateway-transport--state 'authenticating)
          reconnect)
      (cl-letf (((symbol-function 'qq-gateway-transport--disconnect)
                 (lambda (&optional requested) (setq reconnect requested))))
        (qq-gateway-transport--protocol-violation
         "%s"
         (condition-case error-data
             (progn
               (qq-gateway-transport--handle-payload
               '((kind . "event") (event . "gateway.ready")
                  (data . ((gateway_instance_id . "different")
                           (accounts . [])))))
               "not rejected")
           (error (error-message-string error-data))))
        (should reconnect)))))

(ert-deftest qq-gateway-transport-ready-timeout-is-connection-owned ()
  (qq-gateway-transport-test-with-state
    (let* ((old-owner (list :socket 'old :settled nil))
           (new-owner (list :socket 'new :settled nil))
           (qq-gateway-transport--connection-owner new-owner)
           (qq-gateway-transport--state 'authenticating)
           reconnect)
      (cl-letf (((symbol-function 'qq-gateway-transport--disconnect)
                 (lambda (&optional requested) (setq reconnect requested))))
        (qq-gateway-transport--ready-timeout old-owner)
        (should-not reconnect)
        (qq-gateway-transport--ready-timeout new-owner)
        (should reconnect)))))

(ert-deftest qq-gateway-transport-stop-never-sends-account-command ()
  (qq-gateway-transport-test-with-state
    (let (sent closed)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (setq sent t)))
                ((symbol-function 'websocket-close)
                 (lambda (socket) (setq closed socket))))
        (qq-gateway-transport-stop)
        (should (eq closed 'test-websocket))
        (should-not sent)
        (should (eq qq-gateway-transport--state 'stopped))))))

(ert-deftest qq-gateway-transport-old-socket-cannot-retire-new-owner ()
  (let* ((old-owner (list :socket 'old-socket :settled nil))
         (new-owner (list :socket 'new-socket :settled nil))
         (qq-gateway-transport--connection-owner new-owner)
         (qq-gateway-transport--ws 'new-socket)
         (qq-gateway-transport--connecting nil)
         (qq-gateway-transport--stopping nil))
    (should-not
     (qq-gateway-transport--settle-connection
      old-owner 'old-socket 'close))
    (should (eq qq-gateway-transport--connection-owner new-owner))
    (should (eq qq-gateway-transport--ws 'new-socket))))

(provide 'qq-gateway-transport-test)

;;; qq-gateway-transport-test.el ends here
