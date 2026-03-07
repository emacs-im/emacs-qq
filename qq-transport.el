;;; qq-transport.el --- Websocket transport for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Websocket transport used to talk to NapCat OneBot.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'websocket)
(require 'qq-customize)
(require 'qq-state)

(defvar qq-transport-event-hook nil
  "Hook called with one raw websocket event alist argument.")

(defvar qq-transport--ws nil)
(defvar qq-transport--connecting nil)
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
  (let ((json-encoding-pretty-print nil))
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
  (maphash
   (lambda (_echo entry)
     (let ((errback (plist-get entry :error)))
       (when errback
         (funcall errback nil reason))))
   qq-transport--pending)
  (clrhash qq-transport--pending))

(defun qq-transport--disconnect (&optional schedule-reconnect)
  "Disconnect websocket transport.

When SCHEDULE-RECONNECT is non-nil, queue a reconnect attempt."
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

(defun qq-transport--dispatch-response (payload)
  "Dispatch action response PAYLOAD to a pending callback."
  (let* ((echo (alist-get 'echo payload nil nil #'equal))
         (entry (and echo (gethash echo qq-transport--pending)))
         (status (alist-get 'status payload))
         (retcode (alist-get 'retcode payload))
         (message-text (or (alist-get 'message payload)
                           (alist-get 'wording payload)
                           "request failed")))
    (when echo
      (remhash echo qq-transport--pending))
    (when entry
      (if (and (equal status "ok")
               (or (null retcode) (equal retcode 0)))
          (when-let* ((callback (plist-get entry :success)))
            (funcall callback payload))
        (when-let* ((errback (plist-get entry :error)))
          (funcall errback payload message-text))))))

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
    (setq qq-transport--connecting t)
    (setq qq-transport--stopping nil)
    (qq-state-set-connection-status 'connecting)
    (setq qq-transport--ws
          (websocket-open
           (qq-build-websocket-url)
           :on-open (lambda (_ws)
                      (setq qq-transport--connecting nil)
                      (setq qq-transport--reconnect-attempt 0)
                      (qq-state-set-connection-status 'open)
                      (message "qq: websocket opened"))
           :on-message (lambda (_ws frame)
                         (condition-case err
                             (when-let* ((text (qq-transport--frame-text frame)))
                               (unless (string-empty-p (string-trim text))
                                 (qq-transport--handle-payload
                                  (qq-transport--json-decode text))))
                           (error
                            (message "qq: websocket payload error: %s"
                                     (error-message-string err)))))
           :on-close (lambda (_ws)
                       (setq qq-transport--ws nil)
                       (setq qq-transport--connecting nil)
                       (unless qq-transport--stopping
                         (message "qq: websocket closed")
                         (qq-transport--disconnect t)))
           :on-error (lambda (_ws _type err)
                       (setq qq-transport--connecting nil)
                       (message "qq: websocket error: %s"
                                (error-message-string err))
                       (unless qq-transport--stopping
                         (qq-transport--disconnect t)))))))

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
           (entry (list :action action :success callback :error errback))
           (payload `((action . ,action)
                      (params . ,(or params (make-hash-table :test #'equal)))
                      (echo . ,echo))))
      (puthash echo entry qq-transport--pending)
      (condition-case err
          (progn
            (websocket-send-text qq-transport--ws (qq-transport--json-encode payload))
            echo)
        (error
         (remhash echo qq-transport--pending)
         (when errback
           (funcall errback nil (error-message-string err)))
         nil)))))

(provide 'qq-transport)

;;; qq-transport.el ends here
