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

(provide 'qq-transport-test)

;;; qq-transport-test.el ends here
