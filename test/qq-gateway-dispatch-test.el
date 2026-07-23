;;; qq-gateway-dispatch-test.el --- Tests for Gateway domain dispatch -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-dispatch)
(require 'qq-gateway-message)

(defvar qq-gateway-dispatch-test--calls nil)

(defun qq-gateway-dispatch-test--event-owner (event data)
  "Record routed EVENT and DATA."
  (push (list 'event event data) qq-gateway-dispatch-test--calls))

(defun qq-gateway-dispatch-test--other-event-owner (event data)
  "Record a conflicting EVENT and DATA owner call."
  (push (list 'other event data) qq-gateway-dispatch-test--calls))

(defun qq-gateway-dispatch-test--bad-event-owner (_event _data)
  "Reject a routed test event."
  (error "invalid domain payload"))

(defun qq-gateway-dispatch-test--error-owner (body)
  "Record routed unsolicited error BODY."
  (push (list 'error body) qq-gateway-dispatch-test--calls))

(defmacro qq-gateway-dispatch-test-with-state (&rest body)
  "Run BODY with isolated dispatch tables and observations."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway-dispatch--event-handlers
          (make-hash-table :test #'equal))
         (qq-gateway-dispatch--error-handlers
          (make-hash-table :test #'equal))
         (qq-gateway-dispatch-test--calls nil))
     ,@body))

(ert-deftest qq-gateway-dispatch-routes-only-the-exact-event-owner ()
  (qq-gateway-dispatch-test-with-state
    (qq-gateway-dispatch-register-event
     "example.changed" #'qq-gateway-dispatch-test--event-owner)
    (qq-gateway-dispatch--handle-transport-event
     "example.unknown" '((ignored . t)))
    (should-not qq-gateway-dispatch-test--calls)
    (qq-gateway-dispatch--handle-transport-event
     "example.changed"
     `((value . ["opaque" ,qq-gateway-wire-null])))
    (should
     (equal qq-gateway-dispatch-test--calls
            '((event "example.changed" ((value . ("opaque" nil)))))))))

(ert-deftest qq-gateway-dispatch-registration-is-owned-and-reload-safe ()
  (qq-gateway-dispatch-test-with-state
    (qq-gateway-dispatch-register-event
     "example.changed" #'qq-gateway-dispatch-test--event-owner)
    (should
     (eq (qq-gateway-dispatch-register-event
          "example.changed" #'qq-gateway-dispatch-test--event-owner)
         #'qq-gateway-dispatch-test--event-owner))
    (should-error
     (qq-gateway-dispatch-register-event
      "example.changed" #'qq-gateway-dispatch-test--other-event-owner))
    (should-error
     (qq-gateway-dispatch-register-event "" #'ignore))
    (should-error
     (qq-gateway-dispatch-register-event "example.lambda" (lambda (&rest _))))))

(ert-deftest qq-gateway-dispatch-domain-error-is-not-a-protocol-violation ()
  (qq-gateway-dispatch-test-with-state
    (let (violations)
      (qq-gateway-dispatch-register-event
       "example.changed" #'qq-gateway-dispatch-test--bad-event-owner)
      (cl-letf (((symbol-function 'qq-gateway-transport--protocol-violation)
                 (lambda (format-string &rest arguments)
                   (push (apply #'format format-string arguments) violations))))
        (should-error
         (qq-gateway-dispatch--handle-transport-event
          "example.changed" '((invalid . t)))))
      (should-not violations))))

(ert-deftest qq-gateway-dispatch-routes-unsolicited-errors-by-code ()
  (qq-gateway-dispatch-test-with-state
    (qq-gateway-dispatch-register-error
     "example_lagged" #'qq-gateway-dispatch-test--error-owner)
    (qq-gateway-dispatch--handle-transport-error
     '((code . "other_error") (message . "ignored")))
    (should-not qq-gateway-dispatch-test--calls)
    (qq-gateway-dispatch--handle-transport-error
     '((code . "example_lagged") (message . "missed 2")))
    (should
     (equal qq-gateway-dispatch-test--calls
            '((error ((code . "example_lagged")
                      (message . "missed 2"))))))))

(ert-deftest qq-gateway-dispatch-domain-vocabulary-has-explicit-owners ()
  (dolist (route
           '(("gateway.ready" . qq-gateway--handle-event)
             ("account.changed" . qq-gateway--handle-event)
             ("account.removed" . qq-gateway--handle-event)
             ("resource.changed" . qq-gateway-resource--handle-event)
             ("resource.removed" . qq-gateway-resource--handle-event)
             ("media.changed" . qq-gateway-media--handle-event)
             ("media.removed" . qq-gateway-media--handle-event)
             ("attachment.changed" . qq-gateway-attachment--handle-event)
             ("message.received" . qq-gateway-message--handle-event)
             ("message.recalled" . qq-gateway-message--handle-event)
             ("message.poked" . qq-gateway-message--handle-event)
             ("message.reaction_changed" . qq-gateway-message--handle-event)
             ("message.essence_changed" . qq-gateway-message--handle-event)))
    (should
     (eq (gethash (car route) qq-gateway-dispatch--event-handlers)
         (cdr route))))
  (dolist (route
           '(("event_stream_lagged" . qq-gateway--handle-protocol-error)
             ("resource_event_stream_lagged"
              . qq-gateway-resource--handle-protocol-error)
             ("media_event_stream_lagged"
              . qq-gateway-media--handle-protocol-error)
             ("attachment_event_stream_lagged"
              . qq-gateway-attachment--handle-protocol-error)))
    (should
     (eq (gethash (car route) qq-gateway-dispatch--error-handlers)
         (cdr route))))
  (should
   (memq #'qq-gateway-dispatch--handle-transport-event
         qq-gateway-transport-event-hook))
  (dolist (old-consumer
           '(qq-gateway--handle-event
             qq-gateway-resource--handle-event
             qq-gateway-media--handle-event
             qq-gateway-attachment--handle-event
             qq-gateway-message--handle-event))
    (should-not (memq old-consumer qq-gateway-transport-event-hook))))

(provide 'qq-gateway-dispatch-test)
;;; qq-gateway-dispatch-test.el ends here
