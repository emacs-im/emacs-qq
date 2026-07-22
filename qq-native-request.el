;;; qq-native-request.el --- Native request ownership -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Product requests outlive individual transport frames: some requests stage
;; media before dispatch and some are superseded.  This module provides the
;; single request handle exposed by the native facade and keeps those lifecycle
;; rules out of UI callers.
;;
;; A request's `owner' is only the observation context in which its callback
;; may still be delivered; it is not Gateway authority and is never serialized
;; onto the wire.  Nil denotes global work, a string follows one stable selected
;; account slot across Native Session replacement. Exact session identity is
;; deliberately kept behind the Gateway boundary; product callers never
;; coordinate it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway)
(require 'qq-gateway-transport)

(cl-defstruct (qq-native-request
               (:constructor qq-native-request--create))
  "Opaque, exactly-once native request handle."
  owner
  (state 'active)
  token
  cancel-function)

(defvar qq-native-request--active (make-hash-table :test #'eq)
  "Active native requests, keyed by request identity.")

(defun qq-native-request--copy-owner (owner)
  "Return an isolated copy of callback observation OWNER context."
  (cond
   ((and (stringp owner) (not (string-empty-p owner)))
    (copy-sequence owner))
   ((null owner) nil)
   (t (error "qq: invalid native request owner context %S" owner))))

(defun qq-native-request-create (&optional owner cancel-function)
  "Create and register an active request for OWNER context.

Nil denotes global work and a non-empty string denotes a stable selected
account slot.  These scopes only control callback delivery; they do not
authorize a request or add fields to its wire parameters.

CANCEL-FUNCTION revokes adapter work and is called at most once.  Callers that
only own a Gateway transport token should use `qq-native-request-start'."
  (let ((request
         (qq-native-request--create
          :owner (qq-native-request--copy-owner owner)
          :cancel-function cancel-function)))
    (puthash request t qq-native-request--active)
    request))

(defun qq-native-request-active-p (request)
  "Return non-nil when REQUEST still owns asynchronous work."
  (and (qq-native-request-p request)
       (eq (qq-native-request-state request) 'active)))

(defun qq-native-request--retire (request state)
  "Move active REQUEST to terminal STATE exactly once."
  (let ((inhibit-quit t))
    (when (qq-native-request-active-p request)
      (setf (qq-native-request-state request) state
            (qq-native-request-token request) nil
            (qq-native-request-cancel-function request) nil)
      (remhash request qq-native-request--active)
      t)))

(defun qq-native-request-finish (request)
  "Mark REQUEST successfully settled without invoking a callback."
  (qq-native-request--retire request 'settled))

(defun qq-native-request-fail (request)
  "Mark REQUEST failed without invoking an error callback."
  (qq-native-request--retire request 'failed))

(defun qq-native-request--owner-current-p (request)
  "Return non-nil when REQUEST may deliver account-scoped callbacks.

A string owner denotes a stable managed-account slot and deliberately survives
Native Session replacement."
  (let ((owner (qq-native-request-owner request)))
    (or (null owner)
        (equal owner (qq-gateway-current-account-id)))))

(defun qq-native-request--invoke (callback &rest arguments)
  "Invoke leaf CALLBACK with ARGUMENTS while isolating consumer errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "qq: native request callback failed: %s"
                (error-message-string error-data))))))

(defun qq-native-cancel-request (request)
  "Cancel REQUEST locally and revoke its adapter work exactly once."
  (let ((inhibit-quit t))
    (when (qq-native-request-active-p request)
      (let ((cancel (qq-native-request-cancel-function request))
            (token (qq-native-request-token request)))
        ;; Retire before calling user-defined cancellation code.  A synchronous
        ;; transport callback or reentrant cancellation must observe revocation.
        (qq-native-request--retire request 'cancelled)
        (condition-case error-data
            (cond
             (cancel (funcall cancel))
             (token (qq-gateway-transport-cancel token)))
          ((error quit)
           (message "qq: native request cancellation failed: %s"
                    (error-message-string error-data))))
        t))))

(cl-defun qq-native-request-start
    (starter &key callback errback (owner nil owner-supplied-p)
             cancel-function request)
  "Start one callback-scoped request through STARTER.

STARTER is called with success and error continuations and returns its opaque
adapter token.  CALLBACK receives one successful value; ERRBACK receives an
error body and human-readable reason.  OWNER uses the scope vocabulary of
`qq-native-request-create' and defaults to the stable selected account slot.
CANCEL-FUNCTION, when non-nil, receives the adapter token.

The returned `qq-native-request' is safe even when STARTER completes
synchronously.  Late callbacks and callbacks for a replaced OWNER are inert.
REQUEST may supply an already-registered handle to composite schedulers that
must publish identity before STARTER can complete synchronously."
  (let* ((owner (if owner-supplied-p
                    owner
                  (if request
                      (qq-native-request-owner request)
                    (qq-gateway-current-account-id))))
         (request request)
         (cancel-token (or cancel-function #'qq-gateway-transport-cancel))
         returned-p)
    ;; Keep registration, validation, STARTER, and token adoption inside one
    ;; revocation boundary.  In particular, `quit' is not an `error' condition
    ;; and must not strand either a registered request or an adopted token.
    (unwind-protect
        (progn
          ;; If quit is already pending, defer it until REQUEST is visible to
          ;; this unwind-protect's cleanup clause.
          (unless request
            (let ((inhibit-quit t))
              (setq request (qq-native-request-create owner))))
          (unless (qq-native-request-active-p request)
            (error "qq: cannot start an inactive native request"))
          (unless (equal owner (qq-native-request-owner request))
            (error "qq: native request owner contradicts starter owner"))
          (cl-labels
              ((success
                (value)
                (when (qq-native-request-active-p request)
                  (if (not (qq-native-request--owner-current-p request))
                      (qq-native-cancel-request request)
                    (qq-native-request--retire request 'settled)
                    (qq-native-request--invoke callback value))))
               (failure
                (body reason)
                (when (qq-native-request-active-p request)
                  (if (not (qq-native-request--owner-current-p request))
                      (qq-native-cancel-request request)
                    (qq-native-request--retire request 'failed)
                    (qq-native-request--invoke errback body reason)))))
            ;; Defer a real C-g across the adapter call and token adoption.  If
            ;; quit is delivered on leaving this scope, the outer cleanup can
            ;; already see and revoke the adopted token.
            (let ((inhibit-quit t))
              (let ((token (funcall starter #'success #'failure)))
                (cond
                 ((qq-native-request-active-p request)
                  (if token
                      (setf (qq-native-request-token request) token
                            (qq-native-request-cancel-function request)
                            (lambda () (funcall cancel-token token)))
                    ;; No token and no synchronous callback means no owned work.
                    (qq-native-request--retire request 'settled)))
                 ;; STARTER may reentrantly settle, cancel, or replace REQUEST
                 ;; and still return a live transport token.  Adoption after
                 ;; revocation would leak that work, so cancel the orphan now.
                 (token
                  (condition-case cancellation-error
                      (funcall cancel-token token)
                    ((error quit)
                     (message
                      "qq: orphan native request cancellation failed: %s"
                      (error-message-string cancellation-error))))))))
            (setq returned-p t)
            request))
      (unless returned-p
        ;; `qq-native-cancel-request' detaches ownership before invoking its
        ;; cleanup function, so reentrant settlement remains inert.  The
        ;; unwind-protect naturally preserves the original error/quit signal.
        (when request
          (qq-native-cancel-request request))))))

(defun qq-native-request-revoke-stale (&rest _ignored)
  "Cancel requests whose callback observation context is no longer current."
  (let ((inhibit-quit t)
        stale)
    (maphash
     (lambda (request _present)
       (when (and (qq-native-request-owner request)
                  (not (qq-native-request--owner-current-p request)))
         (push request stale)))
     qq-native-request--active)
    (dolist (request stale)
      (qq-native-cancel-request request))))

(defun qq-native-request-revoke-all ()
  "Cancel every active native request."
  (let ((inhibit-quit t)
        requests)
    (maphash (lambda (request _present) (push request requests))
             qq-native-request--active)
    (dolist (request requests)
      (qq-native-cancel-request request))))

(provide 'qq-native-request)

;;; qq-native-request.el ends here
