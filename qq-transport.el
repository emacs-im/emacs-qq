;;; qq-transport.el --- Websocket transport for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Websocket transport used to talk to NapCat OneBot.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)
(require 'qq-customize)
(require 'qq-state)

(defvar qq-transport-request-timeout)

(defvar qq-transport-event-hook nil
  "Hook called with one raw websocket event alist argument.")

(defvar qq-transport--ws nil)
(defvar qq-transport--connecting nil)
(defvar qq-transport--connection-owner nil
  "Identity owner of the current connecting or open websocket.")
(defvar qq-transport--stopping nil)
(defvar qq-transport--pending (make-hash-table :test #'equal))
(defvar qq-transport--echo-counter 0)
(defvar qq-transport--reconnect-timer nil)
(defvar qq-transport--reconnect-attempt 0)

(defun qq-transport-running-p ()
  "Return non-nil when websocket transport is active or connecting."
  (or qq-transport--connecting
      (and qq-transport--ws (websocket-openp qq-transport--ws))))

(defun qq-transport--next-echo ()
  "Return a new OneBot echo token."
  (format "qq-%d" (cl-incf qq-transport--echo-counter)))

(defun qq-transport--json-encode (obj)
  "Encode OBJ into compact JSON text."
  ;; The protocol uses `:false' as its decoded and in-memory JSON false
  ;; sentinel.  Emacs defaults `json-false' to `:json-false'; without this
  ;; binding `json-encode' serializes `:false' as the string "false".
  (let ((json-encoding-pretty-print nil)
        (json-false :false))
    (json-encode obj)))

(defun qq-transport--json-decode (text)
  "Decode JSON TEXT into alists and lists."
  (json-parse-string text
                     :object-type 'alist
                     :array-type 'list
                     :false-object :false
                     :null-object nil))

(defun qq-transport--frame-text (frame)
  "Return decoded text payload from websocket FRAME."
  (pcase (websocket-frame-opcode frame)
    ('text (websocket-frame-text frame))
    ('binary (decode-coding-string (websocket-frame-payload frame) 'utf-8 t))
    (_ nil)))

(defun qq-transport--schedule-reconnect ()
  "Schedule one reconnect attempt when reconnect policy allows it."
  (let ((max-attempts qq-transport-reconnect-max-attempts)
        (next-attempt (1+ qq-transport--reconnect-attempt)))
    (if (and (integerp max-attempts)
             (> next-attempt max-attempts))
        (progn
          (setq qq-transport--stopping t)
          (qq-state-set-connection-status 'disconnected)
          (message "qq: reached reconnect attempt limit (%d)" max-attempts))
      (setq qq-transport--reconnect-attempt next-attempt)
      (setq qq-transport--reconnect-timer
            (run-at-time
             (max 0.2 (float qq-transport-reconnect-delay))
             nil
             (lambda ()
               (setq qq-transport--reconnect-timer nil)
               (unless qq-transport--stopping
                 (qq-transport--connect)))))
      (qq-state-set-connection-status 'reconnecting)
      (message "qq: reconnecting in %.1fs (attempt %d)"
               (max 0.2 (float qq-transport-reconnect-delay))
               next-attempt))))

(defun qq-transport--clear-reconnect-timer ()
  "Cancel queued reconnect timer when present."
  (when (timerp qq-transport--reconnect-timer)
    (cancel-timer qq-transport--reconnect-timer)
    (setq qq-transport--reconnect-timer nil)))

(defun qq-transport--fail-pending (reason)
  "Fail all pending callbacks with REASON."
  (let (echoes)
    (maphash (lambda (echo _entry) (push echo echoes))
             qq-transport--pending)
    (dolist (echo echoes)
      (qq-transport--complete echo 'error nil reason))))

(defun qq-transport--connection-current-p (owner socket)
  "Return non-nil when OWNER still owns SOCKET.

The callback may run synchronously before `websocket-open' returns SOCKET, so
the first callback is allowed to bind the owner's socket identity."
  (and (eq owner qq-transport--connection-owner)
       (not (plist-get owner :settled))
       (let ((owned-socket (plist-get owner :socket)))
         (cond
          ((null owned-socket)
           (setf (plist-get owner :socket) socket)
           t)
          ((eq owned-socket socket))))))

(defun qq-transport--settle-connection (owner socket event &optional error-data)
  "Settle OWNER's SOCKET once for EVENT and optional ERROR-DATA.

Return non-nil only for the callback which still owns the current connection.
Late close/error callbacks from an older or already-settled socket are inert."
  (when (qq-transport--connection-current-p owner socket)
    (setf (plist-get owner :settled) t)
    (setq qq-transport--connection-owner nil)
    (setq qq-transport--connecting nil)
    (pcase event
      ('close (message "qq: websocket closed"))
      ('error (message "qq: websocket error: %s"
                       (error-message-string error-data))))
    (unless qq-transport--stopping
      (qq-transport--disconnect t))
    t))

(defun qq-transport--disconnect (&optional schedule-reconnect)
  "Disconnect websocket transport.

When SCHEDULE-RECONNECT is non-nil, queue a reconnect attempt."
  (when qq-transport--connection-owner
    (setf (plist-get qq-transport--connection-owner :settled) t))
  (setq qq-transport--connection-owner nil)
  (setq qq-transport--connecting nil)
  (when qq-transport--ws
    (ignore-errors (websocket-close qq-transport--ws))
    (setq qq-transport--ws nil))
  (qq-transport--fail-pending "transport disconnected")
  (if schedule-reconnect
      (progn
        (qq-transport--clear-reconnect-timer)
        (unless qq-transport--stopping
          (qq-transport--schedule-reconnect)))
    (qq-transport--clear-reconnect-timer)
    (qq-state-set-connection-status 'disconnected)))

(defun qq-transport-stop ()
  "Stop websocket transport and cancel reconnects."
  (interactive)
  (setq qq-transport--stopping t)
  (setq qq-transport--reconnect-attempt 0)
  (qq-transport--disconnect nil)
  (message "qq: transport stopped"))

(defun qq-transport--cancel-entry-timer (entry)
  "Cancel the request timeout stored in pending ENTRY."
  (when-let* ((timer (plist-get entry :timer)))
    (when (timerp timer)
      (cancel-timer timer))))

(defun qq-transport--invoke-callback (callback &rest args)
  "Invoke CALLBACK with ARGS without breaking transport cleanup."
  (when callback
    (condition-case err
        (apply callback args)
      (error
       (message "qq: transport callback error: %s"
                (error-message-string err))))))

(defun qq-transport--complete (echo outcome response reason)
  "Complete ECHO exactly once with OUTCOME, RESPONSE, and REASON.

OUTCOME is `success' or `error'.  Return non-nil when a pending request was
completed, and nil for stale responses or timers."
  (when-let* ((entry (gethash echo qq-transport--pending)))
    (remhash echo qq-transport--pending)
    (qq-transport--cancel-entry-timer entry)
    (if (eq outcome 'success)
        (qq-transport--invoke-callback
         (plist-get entry :success) response)
      (qq-transport--invoke-callback
       (plist-get entry :error) response reason))
    t))

(defun qq-transport-cancel (echo)
  "Forget pending request ECHO without invoking either callback.

NapCat cannot cancel an action already executing, but removing its callback
matches telega's request-token cancellation: any later response is stale and
is ignored."
  (when-let* ((entry (gethash echo qq-transport--pending)))
    (remhash echo qq-transport--pending)
    (qq-transport--cancel-entry-timer entry)
    t))

(defun qq-transport--request-timeout (echo action)
  "Fail pending ECHO for ACTION after its local timeout."
  (qq-transport--complete
   echo 'error nil (format "%s request timed out" action)))

(defun qq-transport--dispatch-response (payload)
  "Dispatch action response PAYLOAD to a pending callback."
  (let* ((echo (alist-get 'echo payload nil nil #'equal))
         (status (alist-get 'status payload))
         (retcode (alist-get 'retcode payload))
         (message-text (or (alist-get 'message payload)
                           (alist-get 'wording payload)
                           "request failed")))
    (when echo
      (if (and (equal status "ok")
               (or (null retcode) (equal retcode 0)))
          (qq-transport--complete echo 'success payload nil)
        (qq-transport--complete echo 'error payload message-text)))))

(defun qq-transport--handle-payload (payload)
  "Handle websocket PAYLOAD decoded from JSON."
  (cond
   ((alist-get 'post_type payload nil nil #'equal)
    (run-hook-with-args 'qq-transport-event-hook payload))
   ((alist-get 'status payload nil nil #'equal)
    (qq-transport--dispatch-response payload))))

(defun qq-transport--connect ()
  "Connect websocket transport when not already connected."
  (unless (qq-transport-running-p)
    (let ((owner (list :socket nil :settled nil)))
      (setq qq-transport--connection-owner owner)
      (setq qq-transport--connecting t)
      (setq qq-transport--stopping nil)
      (qq-state-set-connection-status 'connecting)
      (condition-case err
          (let ((socket
                 (websocket-open
                  (qq-build-websocket-url)
                  :on-open
                  (lambda (ws)
                    (when (qq-transport--connection-current-p owner ws)
                      (setq qq-transport--ws ws)
                      (setq qq-transport--connecting nil)
                      (setq qq-transport--reconnect-attempt 0)
                      (qq-state-set-connection-status 'open)
                      (message "qq: websocket opened")))
                  :on-message
                  (lambda (ws frame)
                    (when (qq-transport--connection-current-p owner ws)
                      (condition-case payload-error
                          (when-let* ((text (qq-transport--frame-text frame)))
                            (unless (string-empty-p (string-trim text))
                              (qq-transport--handle-payload
                               (qq-transport--json-decode text))))
                        (error
                         (message "qq: websocket payload error: %s"
                                  (error-message-string payload-error))))))
                  :on-close
                  (lambda (ws)
                    (qq-transport--settle-connection owner ws 'close))
                  :on-error
                  (lambda (ws _type socket-error)
                    (qq-transport--settle-connection
                     owner ws 'error socket-error)))))
            ;; A callback may have synchronously settled OWNER before
            ;; `websocket-open' returns.  Never resurrect that socket.
            (when (qq-transport--connection-current-p owner socket)
              (setq qq-transport--ws socket)))
        (error
         ;; `websocket-open' can fail synchronously before it has an object on
         ;; which to deliver `:on-error'.  Settle the same owner path so later
         ;; retries are not suppressed by a stale `--connecting' flag.
         (qq-transport--settle-connection owner nil 'error err))))))

(defun qq-transport-start ()
  "Start websocket transport when needed."
  (interactive)
  (setq qq-transport--stopping nil)
  (unless qq-transport--reconnect-timer
    (setq qq-transport--reconnect-attempt 0))
  (qq-transport--connect))

(defun qq-transport-send (action params &optional callback errback)
  "Send OneBot ACTION with PARAMS.

CALLBACK is called with the raw response alist on success.
ERRBACK is called with arguments RESPONSE and REASON on failure.
Return the generated echo token, or nil if transport is unavailable."
  (if (not (and qq-transport--ws (websocket-openp qq-transport--ws)))
      (progn
        (when errback
          (funcall errback nil "transport is not connected"))
        nil)
    (let* ((echo (qq-transport--next-echo))
           (timeout qq-transport-request-timeout)
           (timer (and (numberp timeout)
                       (> timeout 0)
                       (run-at-time timeout nil
                                    #'qq-transport--request-timeout
                                    echo action)))
           (entry (list :action action :success callback :error errback
                        :timer timer))
           (payload `((action . ,action)
                      (params . ,(or params (make-hash-table :test #'equal)))
                      (echo . ,echo))))
      (puthash echo entry qq-transport--pending)
      (condition-case err
          (progn
            (websocket-send-text qq-transport--ws (qq-transport--json-encode payload))
            echo)
        (error
         (qq-transport--complete
          echo 'error nil (error-message-string err))
         nil)))))

(provide 'qq-transport)

;;; qq-transport.el ends here
