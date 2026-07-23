;;; qq-gateway-dispatch.el --- Native Gateway domain dispatch -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Explicit ownership for native Gateway events and unsolicited errors.
;; Transport owns envelopes and connection state; exactly one domain handler
;; owns each advertised event name or error code.

;;; Code:

(require 'subr-x)
(require 'qq-gateway-transport)
(require 'qq-gateway-wire)

(defvar qq-gateway-dispatch--event-handlers
  (make-hash-table :test #'equal)
  "Domain handler indexed by native Gateway event name.")

(defvar qq-gateway-dispatch--error-handlers
  (make-hash-table :test #'equal)
  "Domain handler indexed by unsolicited native Gateway error code.")

(defun qq-gateway-dispatch--register (table kind name handler)
  "Register named HANDLER for NAME of KIND in TABLE.

Registering the same symbol again is idempotent, which keeps feature reloads
safe.  A different owner for an existing NAME is an architecture error."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "qq: Gateway %s name must be a non-empty string" kind))
  (unless (and handler (symbolp handler))
    (error "qq: Gateway %s handler must be a named function" kind))
  (let ((existing (gethash name table)))
    (when (and existing (not (eq existing handler)))
      (error "qq: Gateway %s %s already belongs to %S"
             kind name existing))
    (puthash name handler table))
  handler)

(defun qq-gateway-dispatch-register-event (event handler)
  "Make named HANDLER the sole domain owner of native EVENT.

HANDLER receives EVENT and an owned, recursively domainized DATA value."
  (qq-gateway-dispatch--register
   qq-gateway-dispatch--event-handlers "event" event handler))

(defun qq-gateway-dispatch-register-error (code handler)
  "Make named HANDLER the sole domain owner of unsolicited error CODE.

HANDLER receives the validated protocol error body."
  (qq-gateway-dispatch--register
   qq-gateway-dispatch--error-handlers "error" code handler))

(defun qq-gateway-dispatch--handle-transport-event (event data)
  "Route native EVENT and DATA to its explicit domain owner.

Unknown events remain observable through the transport hook but are ignored by
the client projection.  Transport has already established the exact protocol
version; feature-handler failures are client projection errors, not reasons to
reconnect a healthy socket."
  (let ((handler (gethash event qq-gateway-dispatch--event-handlers)))
    (when handler
      (funcall handler event (qq-gateway-wire-domain-copy data)))))

(defun qq-gateway-dispatch--handle-transport-error (body)
  "Route unsolicited protocol error BODY by its exact code.

The transport already validated BODY.  A failing recovery handler is a client
bug rather than malformed wire data, so it is isolated without reconnecting."
  (let* ((code (and (listp body) (alist-get 'code body)))
         (handler (and (stringp code)
                       (gethash code qq-gateway-dispatch--error-handlers))))
    (when handler
      (condition-case error-data
          (funcall handler (qq-gateway-wire-domain-copy body))
        (error
         (message "qq: Gateway error handler %S failed for %s: %s"
                  handler code (error-message-string error-data)))))))

(add-hook 'qq-gateway-transport-event-hook
          #'qq-gateway-dispatch--handle-transport-event)
(add-hook 'qq-gateway-transport-protocol-error-hook
          #'qq-gateway-dispatch--handle-transport-error)

(provide 'qq-gateway-dispatch)
;;; qq-gateway-dispatch.el ends here
