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
   ((null owner) nil)
   ((and (stringp owner) (> (length owner) 0)) (copy-sequence owner))
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
  (when (qq-native-request-active-p request)
    (setf (qq-native-request-state request) state
          (qq-native-request-token request) nil
          (qq-native-request-cancel-function request) nil)
    (remhash request qq-native-request--active)
    t))

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
  (when (qq-native-request-active-p request)
    (let ((cancel (qq-native-request-cancel-function request))
          (token (qq-native-request-token request)))
      ;; Revoke callback ownership before adapter cancellation can reenter.
      (qq-native-request--retire request 'cancelled)
      (condition-case error-data
          (cond
           (cancel (funcall cancel))
           (token (qq-gateway-transport-cancel token)))
        (error
         (message "qq: native request cancellation failed: %s"
                  (error-message-string error-data))))
      t)))

(cl-defun qq-native-request-start
    (starter &key callback errback (owner nil owner-supplied-p))
  "Start one callback-scoped request through STARTER.

STARTER is called with success and error continuations and returns its opaque
adapter token.  CALLBACK receives one successful value; ERRBACK receives an
error body and human-readable reason.  OWNER uses the scope vocabulary of
`qq-native-request-create' and defaults to the stable selected account slot.

The request retires before invoking either leaf callback.  Late callbacks and
callbacks for a replaced OWNER are inert.  Product-specific replacement and
projection policy belongs to the product operation, not this lifecycle type.

A nil token must be paired with a synchronous failure callback.  A non-nil
token is accepted work and its callback runs later on the event loop."
  (let* ((owner (if owner-supplied-p
                    owner
                  (qq-gateway-current-account-id)))
         (request (qq-native-request-create owner)))
    (condition-case error-data
        (cl-labels
            ((success
               (value)
               (when (qq-native-request-active-p request)
                 (if (qq-native-request--owner-current-p request)
                     (when (qq-native-request-finish request)
                       (qq-native-request--invoke callback value))
                   (qq-native-cancel-request request))))
             (failure
               (body reason)
               (when (qq-native-request-active-p request)
                 (if (qq-native-request--owner-current-p request)
                     (when (qq-native-request-fail request)
                       (qq-native-request--invoke errback body reason))
                   (qq-native-cancel-request request)))))
          (let ((token (funcall starter #'success #'failure)))
            (when (qq-native-request-active-p request)
              (unless token
                (error "qq: native request starter returned without settling"))
              (setf (qq-native-request-token request) token)))
          request)
      ((error quit)
       (qq-native-cancel-request request)
       (signal (car error-data) (cdr error-data))))))

(defun qq-native-request-revoke-stale (&rest _ignored)
  "Cancel requests whose callback observation context is no longer current."
  (let (stale)
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
  (let (requests)
    (maphash (lambda (request _present) (push request requests))
             qq-native-request--active)
    (dolist (request requests)
      (qq-native-cancel-request request))))

(provide 'qq-native-request)

;;; qq-native-request.el ends here
