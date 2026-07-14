;;; qq-transport-test.el --- Tests for qq-transport -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-transport)

(defmacro qq-transport-test-with-state (&rest body)
  "Run BODY with isolated transport request state."
  (declare (indent 0) (debug t))
  `(let ((qq-transport--pending (make-hash-table :test #'equal))
         (qq-transport--echo-counter 0)
         (qq-transport--ws 'test-websocket)
         (qq-transport-request-timeout 30))
     ,@body))

(ert-deftest qq-transport-response-completes-request-exactly-once ()
  (qq-transport-test-with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore))
        (let ((echo (qq-transport-send
                     "get_status" nil (lambda (_) (cl-incf calls)))))
          (qq-transport--dispatch-response
           `((echo . ,echo) (status . "ok") (retcode . 0)))
          (qq-transport--dispatch-response
           `((echo . ,echo) (status . "ok") (retcode . 0)))
          (should (= calls 1))
          (should-not (gethash echo qq-transport--pending)))))))

(ert-deftest qq-transport-timeout-removes-request-and-reports-action ()
  (qq-transport-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore))
        (let ((echo (qq-transport-send
                     "emacs_get_forward" nil nil
                     (lambda (_response reason) (setq failure reason)))))
          (qq-transport--request-timeout echo "emacs_get_forward")
          (should (equal failure "emacs_get_forward request timed out"))
          (should-not (gethash echo qq-transport--pending))
          (qq-transport--request-timeout echo "emacs_get_forward")
          (should (equal failure "emacs_get_forward request timed out")))))))

(ert-deftest qq-transport-cancel-forgets-callback-ownership ()
  (qq-transport-test-with-state
    (let ((calls 0))
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text) #'ignore))
        (let ((echo (qq-transport-send
                     "get_status" nil (lambda (_) (cl-incf calls)))))
          (should (qq-transport-cancel echo))
          (qq-transport--dispatch-response
           `((echo . ,echo) (status . "ok") (retcode . 0)))
          (should (= calls 0))
          (should-not (qq-transport-cancel echo)))))))

(ert-deftest qq-transport-fail-pending-isolates-callback-errors ()
  (qq-transport-test-with-state
    (let (second-called)
      (puthash "one" (list :error (lambda (&rest _) (error "broken")))
               qq-transport--pending)
      (puthash "two" (list :error (lambda (&rest _) (setq second-called t)))
               qq-transport--pending)
      (qq-transport--fail-pending "disconnected")
      (should second-called)
      (should (= 0 (hash-table-count qq-transport--pending))))))

(ert-deftest qq-transport-synchronous-connect-error-schedules-retry ()
  "A refused socket must not leave the transport permanently connecting."
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws nil)
        (qq-transport--connecting nil)
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (qq-transport-reconnect-max-attempts 3)
        (qq-transport-reconnect-delay 0.2)
        status
        retry
        (open-count 0))
    (cl-letf (((symbol-function 'qq-build-websocket-url)
               (lambda () "ws://127.0.0.1:3001"))
              ((symbol-function 'websocket-open)
               (lambda (&rest _)
                 (cl-incf open-count)
                 (signal 'file-error '("Connection refused"))))
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function)
                 (setq retry function)
                 'retry-timer))
              ((symbol-function 'qq-state-set-connection-status)
               (lambda (next) (setq status next))))
      (qq-transport--connect)
      (should-not qq-transport--connecting)
      (should-not qq-transport--ws)
      (should-not (qq-transport-running-p))
      (should (eq qq-transport--reconnect-timer 'retry-timer))
      (should (= qq-transport--reconnect-attempt 1))
      (should (eq status 'reconnecting))
      ;; Exercise the scheduled callback: the cleared flag must permit a real
      ;; second connection attempt instead of suppressing it forever.
      (funcall retry)
      (should (= open-count 2))
      (should-not (qq-transport-running-p))
      (should (= qq-transport--reconnect-attempt 2)))))

(provide 'qq-transport-test)

;;; qq-transport-test.el ends here
