;;; qq-gateway-rpc.el --- Typed native Gateway RPC boundary -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; This module is the single business-RPC boundary above
;; `qq-gateway-transport'.  It owns readiness and capability checks, result
;; transformation, stale-context rejection, and callback isolation.  It does
;; not depend on account or domain projections, so projection modules may all
;; depend on it without introducing dependency cycles.

;;; Code:

(require 'cl-lib)
(require 'qq-gateway-transport)
(require 'qq-gateway-wire)

(defun qq-gateway-rpc-invoke (callback &rest arguments)
  "Invoke leaf CALLBACK with owned copies of ARGUMENTS.

Ordinary callback errors are isolated from Gateway request machinery."
  (when callback
    (condition-case error-data
        (apply callback (mapcar #'qq-gateway-value-copy arguments))
      (error
       (message "qq: Gateway RPC callback failed: %s"
                (error-message-string error-data))))))

(defun qq-gateway-rpc-client-error
    (errback code format-string &rest arguments)
  "Synchronously invoke ERRBACK with client CODE and formatted reason.

FORMAT-STRING and ARGUMENTS produce the human-readable reason.  Return nil so
preflight failures preserve the request API's unavailable-token convention."
  (let ((reason (apply #'format format-string arguments)))
    (qq-gateway-rpc-invoke
     errback `((code . ,code) (message . ,reason)) reason)
    nil))

(defun qq-gateway-rpc-method-available-p (method)
  "Return non-nil when the ready Gateway advertises METHOD."
  (member method (qq-gateway-transport-capabilities)))

(defun qq-gateway-rpc--success
    (result decoder projector callback errback current-p
            stale-code stale-message)
  "Transform one successful RESULT and invoke CALLBACK.

DECODER and PROJECTOR are optional unary functions.  CURRENT-P is an optional
zero-argument context predicate.  ERRBACK receives STALE-CODE and
STALE-MESSAGE when the predicate normally returns nil.  Signals from the
predicate or either transformer are reported as `invalid_gateway_result'."
  (let ((outcome
         (condition-case error-data
             (if (and current-p (not (funcall current-p)))
                 '(stale)
               (let* ((decoded (if decoder
                                   (funcall decoder result)
                                 result))
                      (projected (if projector
                                     (funcall projector decoded)
                                   decoded)))
                 (list 'ok projected)))
           (error (list 'invalid error-data)))))
    (pcase (car outcome)
      ('ok
       ;; Keep the user's leaf callback outside the transformer handler: a
       ;; consumer bug must not be misreported as malformed Gateway data.
       (qq-gateway-rpc-invoke callback (cadr outcome)))
      ('stale
       (qq-gateway-rpc-client-error
        errback stale-code "%s" stale-message))
      ('invalid
       (qq-gateway-rpc-client-error
        errback "invalid_gateway_result" "%s"
        (error-message-string (cadr outcome)))))))

(defun qq-gateway-rpc--failure
    (body reason errback current-p stale-code stale-message)
  "Deliver transport failure BODY and REASON for the current context.

CURRENT-P has the same ownership semantics as on the success path.  A stale
request reports STALE-CODE and STALE-MESSAGE instead of leaking a late server
failure into a replacement request owner.  A signal from CURRENT-P is a local
contract failure and is therefore reported as `invalid_gateway_result'."
  (let ((outcome
         (condition-case error-data
             (if (and current-p (not (funcall current-p)))
                 '(stale)
               '(current))
           (error (list 'invalid error-data)))))
    (pcase (car outcome)
      ('current
       (qq-gateway-rpc-invoke errback body reason))
      ('stale
       (qq-gateway-rpc-client-error
        errback stale-code "%s" stale-message))
      ('invalid
       (qq-gateway-rpc-client-error
        errback "invalid_gateway_result" "%s"
        (error-message-string (cadr outcome)))))))

(cl-defun qq-gateway-rpc-call
    (method params
            &key decoder projector callback errback current-p
            (stale-code "stale_request")
            (stale-message "Gateway request context is no longer current"))
  "Call advertised Gateway METHOD with PARAMS through the typed RPC boundary.

DECODER validates and domainizes the successful wire result.  PROJECTOR may
then update a client-side projection and returns the value for CALLBACK.
Both are optional unary functions.  Any ordinary error they signal becomes
an `invalid_gateway_result' delivered to ERRBACK.

When optional CURRENT-P normally returns nil, skip transformation and report
STALE-CODE with STALE-MESSAGE.  An error signaled by CURRENT-P is treated like
a transformer error.  User CALLBACK and ERRBACK functions are isolated leaf
consumers and receive owned copies.

Preflight errors invoke ERRBACK synchronously and return nil.  Otherwise
return exactly the opaque token from `qq-gateway-transport-send'.  Transport
signals are deliberately not caught."
  (cond
   ((not (qq-gateway-transport-ready-p))
    (qq-gateway-rpc-client-error
     errback "gateway_not_ready" "Gateway transport is not ready"))
   ((not (qq-gateway-rpc-method-available-p method))
    (qq-gateway-rpc-client-error
     errback "capability_unavailable"
     "Gateway does not advertise capability %s" method))
   (t
    (qq-gateway-transport-send
     method params
     (lambda (result)
       (qq-gateway-rpc--success
        result decoder projector callback errback current-p
        stale-code stale-message))
     (lambda (body reason)
       (qq-gateway-rpc--failure
        body reason errback current-p stale-code stale-message))))))

(cl-defstruct (qq-gateway-rpc-latest-request
               (:constructor qq-gateway-rpc-latest-request--create))
  "One newest-owned RPC request with an exactly-once local lifecycle."
  state
  transport-token
  errback
  owner-symbol
  method)

(defun qq-gateway-rpc--latest-request-current-p (request)
  "Return non-nil when REQUEST is active and still owns its registry."
  (and (qq-gateway-rpc-latest-request-p request)
       (eq (qq-gateway-rpc-latest-request-state request) 'active)
       (eq request
           (symbol-value
            (qq-gateway-rpc-latest-request-owner-symbol request)))))

(defun qq-gateway-rpc--finish-latest-request (request)
  "Settle active REQUEST exactly once and release only its own marker."
  (when (and (qq-gateway-rpc-latest-request-p request)
             (eq (qq-gateway-rpc-latest-request-state request) 'active))
    (setf (qq-gateway-rpc-latest-request-state request) 'settled
          (qq-gateway-rpc-latest-request-transport-token request) nil)
    (let ((owner-symbol
           (qq-gateway-rpc-latest-request-owner-symbol request)))
      (when (eq request (symbol-value owner-symbol))
        (set owner-symbol nil)))
    t))

(defun qq-gateway-rpc--cancel-latest-request (request code message)
  "Cancel and settle active REQUEST with client CODE and MESSAGE."
  (when (and (qq-gateway-rpc-latest-request-p request)
             (eq (qq-gateway-rpc-latest-request-state request) 'active))
    (let ((token (qq-gateway-rpc-latest-request-transport-token request))
          (errback (qq-gateway-rpc-latest-request-errback request)))
      ;; Revoke before touching transport or user code.  Either may reenter.
      (qq-gateway-rpc--finish-latest-request request)
      (when token
        (qq-gateway-transport-cancel token))
      (qq-gateway-rpc-client-error errback code "%s" message)
      t)))

(defun qq-gateway-rpc-cancel-latest
    (owner-symbol &optional code message)
  "Cancel and settle the request currently stored in OWNER-SYMBOL.

CODE defaults to `superseded_request'.  MESSAGE is copied into the standard
client error delivered exactly once to the request errback.  Legacy opaque
owners left by a live reload are safely revoked but cannot be cancelled."
  (let ((request (symbol-value owner-symbol)))
    (cond
     ((qq-gateway-rpc-latest-request-p request)
      (qq-gateway-rpc--cancel-latest-request
       request (or code "superseded_request")
       (or message "Gateway registry request was superseded")))
     (request
      (when (eq request (symbol-value owner-symbol))
        (set owner-symbol nil)
        t)))))

(cl-defun qq-gateway-rpc-latest-call
    (owner-symbol method params
                  &key decoder projector callback errback)
  "Run the newest-owned registry request for OWNER-SYMBOL.

Publish a request record before cancelling its predecessor or calling METHOD.
The record owns the transport token and settles exactly once.  A newer call
cancels and reports the predecessor as superseded before it sends; reentrant
errbacks may themselves replace the new request.  Only the surviving owner
may decode, project, or deliver.  Return the transport token; if synchronous
callbacks already revoked this request, cancel that returned token immediately."
  (let* ((previous (symbol-value owner-symbol))
         (request
          (qq-gateway-rpc-latest-request--create
           :state 'active :errback errback
           :owner-symbol owner-symbol :method method))
         returned-p)
    ;; Publish first so a predecessor's possibly reentrant errback observes
    ;; the replacement rather than an empty ownership window.
    (set owner-symbol request)
    (unwind-protect
        (prog1
            (progn
              (when previous
                (if (qq-gateway-rpc-latest-request-p previous)
                    (qq-gateway-rpc--cancel-latest-request
                     previous "superseded_request"
                     (format "Gateway %s request was superseded" method))
                  ;; A live reload may leave an older opaque identity.  The
                  ;; published record already makes its callbacks inert.
                  nil))
              (when (qq-gateway-rpc--latest-request-current-p request)
                (cl-labels
                    ((current-p ()
                       (qq-gateway-rpc--latest-request-current-p request))
                     (finish-success (value)
                       (if (current-p)
                           (when (qq-gateway-rpc--finish-latest-request request)
                             (qq-gateway-rpc-invoke callback value))
                         (when (qq-gateway-rpc--finish-latest-request request)
                           (qq-gateway-rpc-client-error
                            errback "superseded_request"
                            "Gateway %s request was superseded" method))))
                     (finish-error (body failure)
                       (when (eq (qq-gateway-rpc-latest-request-state request)
                                 'active)
                         (let ((current (current-p)))
                           (when (qq-gateway-rpc--finish-latest-request request)
                             (if current
                                 (qq-gateway-rpc-invoke errback body failure)
                               (qq-gateway-rpc-client-error
                                errback "superseded_request"
                                "Gateway %s request was superseded" method)))))))
                  (let ((token
                         (qq-gateway-rpc-call
                          method params
                          :current-p #'current-p
                          :stale-code "superseded_request"
                          :stale-message
                          (format "Gateway %s request was superseded" method)
                          :decoder decoder
                          :projector
                          (and projector
                               (lambda (value)
                                 (if (current-p)
                                     (funcall projector value)
                                   value)))
                          :callback #'finish-success
                          :errback #'finish-error)))
                    ;; Publish the returned token only while REQUEST still
                    ;; owns the slot.  A synchronous callback may have settled
                    ;; or replaced it before `qq-gateway-rpc-call' returns; in
                    ;; that case the token must not remain live and unowned.
                    (if (current-p)
                        (setf (qq-gateway-rpc-latest-request-transport-token
                               request)
                              token)
                      (when token
                        (qq-gateway-transport-cancel token)))
                    token))))
          (setq returned-p t))
      ;; Preserve transport signals and quits without leaking a false owner.
      (unless returned-p
        (qq-gateway-rpc--finish-latest-request request)))))

(defun qq-gateway-rpc-request-single-flight
    (marker-symbol marker-tag starter failure-label)
  "Start one automatic registry request owned by MARKER-SYMBOL.

An existing marker coalesces work until its request succeeds, fails, or is
cancelled by a newer request or registry reset.  MARKER-TAG names the opaque
`eq' identity.  STARTER is called with success and failure callbacks.  Only
the marker's own callbacks clear it.  FAILURE-LABEL names diagnostics."
  (unless (symbol-value marker-symbol)
      (let ((marker (list marker-tag)))
        (set marker-symbol marker)
        (cl-labels
            ((finish ()
               (when (eq marker (symbol-value marker-symbol))
                 (set marker-symbol nil)
                 t))
             (succeed (&rest _arguments)
               (finish))
             (fail (body failure)
               (when (finish)
                 (unless (equal (alist-get 'code body) "superseded_request")
                   (message "qq: Gateway %s resync failed: %s"
                            failure-label failure)))))
          (let (returned-p)
            (unwind-protect
                (prog1 (funcall starter #'succeed #'fail)
                  (setq returned-p t))
              (unless returned-p
                (finish))))))))

(provide 'qq-gateway-rpc)

;;; qq-gateway-rpc.el ends here
