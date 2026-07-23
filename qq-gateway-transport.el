;;; qq-gateway-transport.el --- Native nt-gateway websocket transport -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Versioned request/response/event transport for the native nt-gateway.
;; This is deliberately separate from `qq-transport': OneBot echoes and
;; native service request IDs have different envelopes and lifecycle rules.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)
(require 'qq-customize)
(require 'qq-gateway-wire)

(defconst qq-gateway-transport-protocol-version 3
  "Native Gateway protocol version implemented by this client.")

(defvar qq-gateway-transport-event-hook nil
  "Hook called with EVENT and DATA for each native service event.")

(defvar qq-gateway-transport-protocol-error-hook nil
  "Hook called with one unsolicited native service error body.")

(defvar qq-gateway-transport-state-hook nil
  "Hook called with the new native service connection state.")

(defvar qq-gateway-transport--ws nil)
(defvar qq-gateway-transport--connecting nil)
(defvar qq-gateway-transport--connection-owner nil
  "Identity owner of the current connecting or open websocket.")
(defvar qq-gateway-transport--stopping nil)
(defvar qq-gateway-transport--pending (make-hash-table :test #'equal))
(defvar qq-gateway-transport--request-counter 0)
(defvar qq-gateway-transport--reconnect-timer nil)
(defvar qq-gateway-transport--reconnect-attempt 0)
(defvar qq-gateway-transport--ready-timer nil)
(defvar qq-gateway-transport--state 'stopped)
(defvar qq-gateway-transport--hello-instance-id nil)
(defvar qq-gateway-transport--gateway-instance-id nil)
(defvar qq-gateway-transport--capabilities nil)
(defvar qq-gateway-transport--ready-accounts nil)

(defun qq-gateway-transport-state ()
  "Return the current native service connection state."
  qq-gateway-transport--state)

(defun qq-gateway-transport-gateway-instance-id ()
  "Return the current authenticated Gateway instance ID, or nil."
  (and qq-gateway-transport--gateway-instance-id
       (copy-sequence qq-gateway-transport--gateway-instance-id)))

(defun qq-gateway-transport-capabilities ()
  "Return a copy of capabilities advertised by the current Gateway."
  (qq-gateway-value-copy qq-gateway-transport--capabilities))

(defun qq-gateway-transport-ready-accounts ()
  "Return a copy of the accounts in the latest `gateway.ready' snapshot."
  (qq-gateway-value-copy qq-gateway-transport--ready-accounts))

(defun qq-gateway-transport-running-p ()
  "Return non-nil while the native service connection is active."
  (or qq-gateway-transport--connecting
      (and qq-gateway-transport--ws
           (websocket-openp qq-gateway-transport--ws))
      (timerp qq-gateway-transport--reconnect-timer)))

(defun qq-gateway-transport-ready-p ()
  "Return non-nil when native service business requests may be sent."
  (and (eq qq-gateway-transport--state 'ready)
       qq-gateway-transport--ws
       (websocket-openp qq-gateway-transport--ws)))

(defun qq-gateway-transport--run-hook (hook &rest arguments)
  "Run HOOK with ARGUMENTS without handing transport ownership to consumers."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function (mapcar #'qq-gateway-value-copy hook-arguments))
       (error
        (message "qq: Gateway hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-gateway-transport--set-state (state)
  "Publish native service connection STATE when it changes."
  (unless (eq state qq-gateway-transport--state)
    (setq qq-gateway-transport--state state)
    (qq-gateway-transport--run-hook
     'qq-gateway-transport-state-hook state))
  state)

(defun qq-gateway-transport--next-request-id ()
  "Return a fresh opaque native service request ID."
  (format "emacs-qq-%d" (cl-incf qq-gateway-transport--request-counter)))

(defun qq-gateway-transport--json-encode (object)
  "Encode OBJECT into compact JSON text."
  (let ((json-encoding-pretty-print nil)
        (json-false :false)
        (json-null nil))
    (json-encode object)))

(defun qq-gateway-transport--json-decode (text)
  "Decode native service JSON TEXT without collapsing wire value kinds."
  (qq-gateway-wire-decode text))

(defun qq-gateway-transport--frame-text (frame)
  "Return UTF-8 text carried by websocket FRAME, or nil."
  (pcase (websocket-frame-opcode frame)
    ('text (websocket-frame-text frame))
    ('binary (error "Gateway server sent a binary frame"))
    (_ nil)))

(defun qq-gateway-transport--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (qq-gateway-wire-exact-object-keys-p object keys))

(defun qq-gateway-transport--read-auth-token ()
  "Read and validate `qq-native-auth-token-file'."
  (let ((path (and (stringp qq-native-auth-token-file)
                   (expand-file-name qq-native-auth-token-file))))
    (unless (and path (file-regular-p path))
      (user-error "qq: Gateway token file is not a regular file: %s"
                  (or path qq-native-auth-token-file)))
    (when (and (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix
                                   darwin cygwin))
               (let ((mode (file-modes path)))
                 (or (null mode) (/= 0 (logand mode #o077)))))
      (user-error
       "qq: Gateway token file is accessible by group/others; use chmod 600: %s"
       path))
    (let ((token
           (with-temp-buffer
             (let ((coding-system-for-read 'utf-8))
               (insert-file-contents path))
             (replace-regexp-in-string
              "[[:space:]]+\\'" "" (buffer-substring-no-properties
                                      (point-min) (point-max))))))
      (when (or (< (string-bytes token) 32)
                (string-match-p "[[:space:]]" token))
        (user-error
         "qq: Gateway token must be one non-whitespace value of at least 32 bytes"))
      token)))

(defun qq-gateway-transport--cancel-entry-timer (entry)
  "Cancel the timeout stored in pending ENTRY."
  (when-let* ((timer (plist-get entry :timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun qq-gateway-transport--invoke-callback (callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS while isolating ordinary errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "qq: Gateway callback error: %s"
                (error-message-string error-data))))))

(defun qq-gateway-transport--complete (id outcome value reason)
  "Complete pending request ID exactly once.

OUTCOME is `success' or `error'.  VALUE is the result or protocol error body;
REASON is a human-readable failure.  Return non-nil when ID was pending."
  (when-let* ((entry (gethash id qq-gateway-transport--pending)))
    (remhash id qq-gateway-transport--pending)
    (qq-gateway-transport--cancel-entry-timer entry)
    (if (eq outcome 'success)
        (qq-gateway-transport--invoke-callback
         (plist-get entry :success) value)
      (qq-gateway-transport--invoke-callback
       (plist-get entry :error) value reason))
    t))

(defun qq-gateway-transport-cancel (id)
  "Forget pending native service request ID without invoking callbacks."
  (when-let* ((entry (gethash id qq-gateway-transport--pending)))
    (remhash id qq-gateway-transport--pending)
    (qq-gateway-transport--cancel-entry-timer entry)
    t))

(defun qq-gateway-transport--request-timeout (id method)
  "Fail pending request ID for METHOD after its local timeout."
  (qq-gateway-transport--complete
   id 'error nil (format "%s request timed out" method)))

(defun qq-gateway-transport--fail-pending (reason)
  "Fail every pending request with REASON, even if one callback quits."
  (let (ids quit-seen-p)
    (maphash (lambda (id _entry) (push id ids))
             qq-gateway-transport--pending)
    (dolist (id ids)
      (condition-case nil
          (qq-gateway-transport--complete id 'error nil reason)
        (quit
         (setq quit-seen-p t
               quit-flag nil))))
    (when quit-seen-p
      (message "qq: ignored quit from callback during Gateway cleanup"))))

(defun qq-gateway-transport--clear-session-metadata ()
  "Forget metadata tied to the previous websocket handshake."
  (setq qq-gateway-transport--hello-instance-id nil
        qq-gateway-transport--gateway-instance-id nil
        qq-gateway-transport--capabilities nil
        qq-gateway-transport--ready-accounts nil))

(defun qq-gateway-transport--clear-owner-token (owner)
  "Erase the authentication token retained by connection OWNER."
  (when-let* ((token (plist-get owner :auth-token)))
    (when (stringp token)
      (clear-string token))
    (setf (plist-get owner :auth-token) nil)))

(defun qq-gateway-transport--connection-current-p (owner socket)
  "Return non-nil when OWNER still owns SOCKET.

The first synchronous callback may bind OWNER before `websocket-open'
returns."
  (and (eq owner qq-gateway-transport--connection-owner)
       (not (plist-get owner :settled))
       (let ((owned-socket (plist-get owner :socket)))
         (cond
          ((null owned-socket)
           (setf (plist-get owner :socket) socket)
           t)
          ((eq owned-socket socket))))))

(defun qq-gateway-transport--clear-reconnect-timer ()
  "Cancel the current native service reconnect timer."
  (when (timerp qq-gateway-transport--reconnect-timer)
    (cancel-timer qq-gateway-transport--reconnect-timer))
  (setq qq-gateway-transport--reconnect-timer nil))

(defun qq-gateway-transport--clear-ready-timer ()
  "Cancel the timer awaiting the authoritative ready snapshot."
  (when (timerp qq-gateway-transport--ready-timer)
    (cancel-timer qq-gateway-transport--ready-timer))
  (setq qq-gateway-transport--ready-timer nil))

(defun qq-gateway-transport--ready-timeout (owner)
  "Reconnect when connection OWNER never receives `gateway.ready'."
  (when (and (eq owner qq-gateway-transport--connection-owner)
             (eq qq-gateway-transport--state 'authenticating))
    (setq qq-gateway-transport--ready-timer nil)
    (message "qq: Gateway ready snapshot timed out")
    (qq-gateway-transport--disconnect t)))

(defun qq-gateway-transport--start-ready-timer ()
  "Start the current connection's `gateway.ready' deadline."
  (qq-gateway-transport--clear-ready-timer)
  (when (and (numberp qq-native-ready-timeout)
             (> qq-native-ready-timeout 0))
    (let ((owner qq-gateway-transport--connection-owner))
      (setq qq-gateway-transport--ready-timer
            (run-at-time qq-native-ready-timeout nil
                         #'qq-gateway-transport--ready-timeout owner)))))

(defun qq-gateway-transport--schedule-reconnect ()
  "Schedule a native service reconnect when policy permits."
  (let ((next-attempt (1+ qq-gateway-transport--reconnect-attempt))
        (max-attempts qq-native-reconnect-max-attempts))
    (if (and (integerp max-attempts) (> next-attempt max-attempts))
        (progn
          (setq qq-gateway-transport--stopping t)
          (qq-gateway-transport--set-state 'stopped)
          (message "qq: Gateway reached reconnect attempt limit (%d)"
                   max-attempts))
      (setq qq-gateway-transport--reconnect-attempt next-attempt)
      (setq qq-gateway-transport--reconnect-timer
            (run-at-time
             (max 0.2 (float qq-native-reconnect-delay)) nil
             (lambda ()
               (setq qq-gateway-transport--reconnect-timer nil)
               (unless qq-gateway-transport--stopping
                 (qq-gateway-transport--connect)))))
      (qq-gateway-transport--set-state 'reconnecting)
      (message "qq: reconnecting to Gateway in %.1fs (attempt %d)"
               (max 0.2 (float qq-native-reconnect-delay)) next-attempt))))

(defun qq-gateway-transport--disconnect (&optional reconnect)
  "Disconnect only this Emacs Gateway client.

When RECONNECT is non-nil, schedule a new websocket connection.  This never
  sends an account stop or logout command to the long-lived Gateway."
  (when qq-gateway-transport--connection-owner
    (qq-gateway-transport--clear-owner-token
     qq-gateway-transport--connection-owner)
    (setf (plist-get qq-gateway-transport--connection-owner :settled) t))
  (setq qq-gateway-transport--connection-owner nil
        qq-gateway-transport--connecting nil)
  (when qq-gateway-transport--ws
    (ignore-errors (websocket-close qq-gateway-transport--ws))
    (setq qq-gateway-transport--ws nil))
  (qq-gateway-transport--fail-pending "Gateway transport disconnected")
  (qq-gateway-transport--clear-session-metadata)
  (qq-gateway-transport--clear-reconnect-timer)
  (qq-gateway-transport--clear-ready-timer)
  (if (and reconnect (not qq-gateway-transport--stopping))
      (qq-gateway-transport--schedule-reconnect)
    (qq-gateway-transport--set-state 'stopped)))

(defun qq-gateway-transport--settle-connection
    (owner socket event &optional error-data)
  "Settle OWNER's SOCKET once for EVENT and optional ERROR-DATA."
  (when (qq-gateway-transport--connection-current-p owner socket)
    (qq-gateway-transport--clear-owner-token owner)
    (setf (plist-get owner :settled) t)
    (setq qq-gateway-transport--connection-owner nil
          qq-gateway-transport--connecting nil)
    (pcase event
      ('close (message "qq: Gateway websocket closed"))
      ('error (message "qq: Gateway websocket error: %s"
                       (error-message-string error-data))))
    (unless qq-gateway-transport--stopping
      (qq-gateway-transport--disconnect t))
    t))

(defun qq-gateway-transport--protocol-violation (format-string &rest arguments)
  "Close and reconnect after a native protocol violation.

FORMAT-STRING and ARGUMENTS describe the violation."
  (message "qq: Gateway protocol violation: %s"
           (apply #'format format-string arguments))
  (qq-gateway-transport--disconnect t))

(defun qq-gateway-transport--dispatch-response (payload)
  "Dispatch a validated response PAYLOAD."
  (let ((id (alist-get 'id payload)))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "Gateway response id must be a non-empty string"))
    (qq-gateway-transport--complete
     id 'success (alist-get 'result payload nil nil #'eq) nil)))

(defun qq-gateway-transport--dispatch-error (payload)
  "Dispatch a validated error PAYLOAD."
  (let* ((wire-id (alist-get 'id payload nil nil #'eq))
         (id (if (qq-gateway-wire-null-p wire-id) nil wire-id))
         (wire-body (alist-get 'error payload nil nil #'eq))
         (code (and (qq-gateway-wire-object-p wire-body)
                    (alist-get 'code wire-body)))
         (reason (and (qq-gateway-wire-object-p wire-body)
                      (alist-get 'message wire-body))))
    (unless (and (or (null id)
                     (and (stringp id) (not (string-empty-p id))))
                 (qq-gateway-transport--exact-object-keys-p
                  wire-body '(code message))
                 (stringp code) (not (string-empty-p code))
                 (stringp reason) (not (string-empty-p reason)))
      (error "Gateway error envelope is malformed"))
    (let ((body (qq-gateway-wire-domain-copy wire-body)))
      (if id
          (qq-gateway-transport--complete id 'error body reason)
        (progn
          (qq-gateway-transport--run-hook
           'qq-gateway-transport-protocol-error-hook body)
          (message "qq: Gateway protocol error %s: %s" code reason))))))

(defun qq-gateway-transport--dispatch-event (payload)
  "Dispatch a validated event PAYLOAD."
  (let ((event (alist-get 'event payload))
        (data (alist-get 'data payload nil nil #'eq)))
    (unless (and (stringp event) (not (string-empty-p event)))
      (error "Gateway event name must be a non-empty string"))
    (when (equal event "gateway.ready")
      (unless (and (qq-gateway-transport--exact-object-keys-p
                    data '(gateway_instance_id accounts))
                   (stringp (alist-get 'gateway_instance_id data)))
        (error "Gateway ready data is malformed"))
      (let ((domain-accounts
             (qq-gateway-wire-array
              (alist-get 'accounts data nil nil #'eq)
              "Gateway ready accounts")))
        (unless (and qq-gateway-transport--hello-instance-id
                     (equal qq-gateway-transport--hello-instance-id
                            (alist-get 'gateway_instance_id data)))
          (error "Gateway ready instance does not match hello response"))
        (setq data (qq-gateway-value-copy data))
        (setq qq-gateway-transport--gateway-instance-id
              (copy-sequence (alist-get 'gateway_instance_id data))
              qq-gateway-transport--ready-accounts
              (mapcar #'qq-gateway-wire-domain-copy domain-accounts)
              qq-gateway-transport--reconnect-attempt 0)
        (qq-gateway-transport--clear-ready-timer)
        (qq-gateway-transport--set-state 'ready)
        (message "qq: native service ready")))
    (qq-gateway-transport--run-hook
     'qq-gateway-transport-event-hook event data)))

(defun qq-gateway-transport--handle-payload (payload)
  "Validate and dispatch one decoded native service PAYLOAD."
  (unless (qq-gateway-wire-object-p payload)
    (error "Gateway envelope must be an object"))
  (pcase (alist-get 'kind payload)
    ("response"
     (unless (qq-gateway-transport--exact-object-keys-p
              payload '(kind id result))
       (error "Gateway response envelope has invalid fields"))
     (qq-gateway-transport--dispatch-response payload))
    ("error"
     (unless (qq-gateway-transport--exact-object-keys-p
              payload '(kind id error))
       (error "Gateway error envelope has invalid fields"))
     (qq-gateway-transport--dispatch-error payload))
    ("event"
     (unless (qq-gateway-transport--exact-object-keys-p
              payload '(kind event data))
       (error "Gateway event envelope has invalid fields"))
     (qq-gateway-transport--dispatch-event payload))
    (_ (error "Unknown Gateway envelope kind"))))

(defun qq-gateway-transport-send
    (method params &optional callback errback allow-before-ready)
  "Send native service METHOD with PARAMS.

CALLBACK receives the successful result object.  ERRBACK receives the
protocol error body (or nil) and a human-readable reason.  Return the opaque
request ID, or nil when unavailable.  Unavailable requests invoke ERRBACK
before returning nil.  A non-nil ID denotes accepted work whose callback runs
later from the WebSocket or timeout event loop, never before this function
returns.  ALLOW-BEFORE-READY is private transport machinery used only for the
initial `gateway.hello' request."
  (if (not (and qq-gateway-transport--ws
                (websocket-openp qq-gateway-transport--ws)
                (or allow-before-ready
                    (qq-gateway-transport-ready-p))))
      (progn
        (qq-gateway-transport--invoke-callback
         errback nil
         (if (and qq-gateway-transport--ws
                  (websocket-openp qq-gateway-transport--ws))
             "Gateway handshake is not ready"
           "Gateway transport is not connected"))
        nil)
    (when quit-flag
      (setq quit-flag nil)
      (signal 'quit nil))
    (let* ((id (qq-gateway-transport--next-request-id))
           (timeout qq-native-request-timeout)
           timer
           (entry (list :method method :success callback :error errback
                        :timer nil))
           (payload `((kind . "request")
                      (id . ,id)
                      (method . ,method)
                      (params . ,(or params
                                     (make-hash-table :test #'equal))))))
      (let ((inhibit-quit t))
        (condition-case error-data
            (progn
              (setq timer
                    (and (numberp timeout) (> timeout 0)
                         (run-at-time timeout nil
                                      #'qq-gateway-transport--request-timeout
                                      id method)))
              (setf (plist-get entry :timer) timer)
              (puthash id entry qq-gateway-transport--pending)
              (websocket-send-text
               qq-gateway-transport--ws
               (qq-gateway-transport--json-encode payload))
              (setq quit-flag nil)
              id)
          (quit
           (if (gethash id qq-gateway-transport--pending)
               (progn (setq quit-flag nil) id)
             (when (timerp timer) (cancel-timer timer))
             (setq quit-flag nil)
             (signal 'quit nil)))
          (error
           (let ((reason (error-message-string error-data)))
             (if (gethash id qq-gateway-transport--pending)
                 (qq-gateway-transport--complete id 'error nil reason)
               (when (timerp timer) (cancel-timer timer))
               (qq-gateway-transport--invoke-callback errback nil reason)))
           (setq quit-flag nil)
           nil))))))

(defun qq-gateway-transport--hello-failed (_error-body reason)
  "Stop reconnecting after terminal handshake failure REASON."
  (setq qq-gateway-transport--stopping t)
  (qq-gateway-transport--disconnect nil)
  (qq-gateway-transport--set-state 'failed)
  (message "qq: Gateway handshake failed: %s" reason))

(defun qq-gateway-transport--send-hello (token)
  "Send the mandatory first request using authentication TOKEN."
  (unwind-protect
      (unless
          (qq-gateway-transport-send
           "gateway.hello"
           `((protocol_version . ,qq-gateway-transport-protocol-version)
             (client_name . ,qq-native-client-name)
             (auth_token . ,token))
           (lambda (result)
             (condition-case nil
                 (let ((capabilities
                        (qq-gateway-wire-array
                         (and (qq-gateway-wire-object-p result)
                              (alist-get 'capabilities result nil nil #'eq))
                         "Gateway capabilities")))
                   (if (and (equal (alist-get 'protocol_version result)
                                   qq-gateway-transport-protocol-version)
                            (qq-gateway-transport--exact-object-keys-p
                             result
                             '(protocol_version gateway_instance_id
                               capabilities))
                            (stringp (alist-get 'gateway_instance_id result))
                            (cl-every (lambda (capability)
                                        (and (stringp capability)
                                             (not (string-empty-p capability))))
                                      capabilities))
                       (progn
                         (setq qq-gateway-transport--hello-instance-id
                               (copy-sequence
                                (alist-get 'gateway_instance_id result))
                               qq-gateway-transport--capabilities
                               (qq-gateway-value-copy capabilities))
                         (qq-gateway-transport--start-ready-timer))
                     (qq-gateway-transport--protocol-violation
                      "Malformed gateway.hello result")))
               (error
                (qq-gateway-transport--protocol-violation
                 "Malformed gateway.hello result"))))
           #'qq-gateway-transport--hello-failed
           t)
        (error "Could not send gateway.hello"))
    ;; Avoid retaining the original file-backed secret in the connection
    ;; callback after the request text has been handed to websocket.el.
    (clear-string token)))

(defun qq-gateway-transport--connect ()
  "Open and authenticate the native service websocket when needed."
  (unless (qq-gateway-transport-running-p)
    ;; Resolve credentials before publishing a connecting owner.  A local
    ;; configuration error is terminal until the user calls start again.
    (condition-case error-data
        (let ((auth-token (qq-gateway-transport--read-auth-token)))
          (let ((owner (list :socket nil :settled nil
                             :auth-token auth-token)))
            (setq qq-gateway-transport--connection-owner owner
                  qq-gateway-transport--connecting t
                  qq-gateway-transport--stopping nil)
            (qq-gateway-transport--set-state 'connecting)
            (condition-case socket-error
                (let ((socket
                       (websocket-open
                        qq-native-websocket-url
                        :on-open
                        (lambda (ws)
                          (when (qq-gateway-transport--connection-current-p
                                 owner ws)
                            (setq qq-gateway-transport--ws ws
                                  qq-gateway-transport--connecting nil)
                            (qq-gateway-transport--set-state 'authenticating)
                            (condition-case hello-error
                                (progn
                                  (qq-gateway-transport--send-hello auth-token)
                                  (setf (plist-get owner :auth-token) nil))
                              (error
                               (qq-gateway-transport--hello-failed
                                nil (error-message-string hello-error))))))
                        :on-message
                        (lambda (ws frame)
                          (when (qq-gateway-transport--connection-current-p
                                 owner ws)
                            (condition-case payload-error
                                (when-let* ((text
                                             (qq-gateway-transport--frame-text
                                              frame)))
                                  (unless (string-empty-p (string-trim text))
                                    (qq-gateway-transport--handle-payload
                                     (qq-gateway-transport--json-decode text))))
                              (error
                               (when
                                   (qq-gateway-transport--connection-current-p
                                    owner ws)
                                 (qq-gateway-transport--protocol-violation
                                  "%s"
                                  (error-message-string payload-error)))))))
                        :on-close
                        (lambda (ws)
                          (qq-gateway-transport--settle-connection
                           owner ws 'close))
                        :on-error
                        (lambda (ws _type error-value)
                          (qq-gateway-transport--settle-connection
                           owner ws 'error error-value)))))
                  (when (qq-gateway-transport--connection-current-p
                         owner socket)
                    (setq qq-gateway-transport--ws socket)))
              (error
               (qq-gateway-transport--settle-connection
                owner nil 'error socket-error)))))
      (error
       (setq qq-gateway-transport--stopping t)
       (qq-gateway-transport--set-state 'failed)
       (message "qq: cannot start Gateway transport: %s"
                (error-message-string error-data))))))

(defun qq-gateway-transport-start ()
  "Start the native service websocket without starting a Native Session."
  (interactive)
  (setq qq-gateway-transport--stopping nil)
  (unless (timerp qq-gateway-transport--reconnect-timer)
    (setq qq-gateway-transport--reconnect-attempt 0))
  (qq-gateway-transport--connect))

(defun qq-gateway-transport-stop ()
  "Stop this Emacs websocket without stopping any QQ account."
  (interactive)
  (setq qq-gateway-transport--stopping t
        qq-gateway-transport--reconnect-attempt 0)
  (qq-gateway-transport--disconnect nil)
  (message "qq: native service connection stopped; accounts remain managed"))

(provide 'qq-gateway-transport)

;;; qq-gateway-transport.el ends here
