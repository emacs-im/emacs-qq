;;; qq-transport-test.el --- Tests for qq-transport -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-transport)
(require 'qq)

(defmacro qq-transport-test-with-state (&rest body)
  "Run BODY with isolated transport request state."
  (declare (indent 0) (debug t))
  `(let ((qq-transport--pending (make-hash-table :test #'equal))
         (qq-transport--echo-counter 0)
         (qq-transport--ws 'test-websocket)
         (qq-transport-request-timeout 30))
     ,@body))

(ert-deftest qq-transport-json-encode-preserves-protocol-false ()
  "The protocol's `:false' sentinel must become a JSON boolean, not a string."
  (let* ((wire (qq-transport--json-encode
                '((top . :false)
                  (nested . ((enabled . t) (disabled . :false)))
                  (literal . "false")
                  (nothing)
                  (items . [:false t]))))
         (decoded (json-parse-string
                   wire
                   :object-type 'alist
                   :array-type 'list
                   :false-object 'wire-false
                   :null-object 'wire-null)))
    (should (eq (alist-get 'top decoded) 'wire-false))
    (should (eq (alist-get 'enabled (alist-get 'nested decoded)) t))
    (should (eq (alist-get 'disabled (alist-get 'nested decoded))
                'wire-false))
    (should (equal (alist-get 'literal decoded) "false"))
    (should (eq (alist-get 'nothing decoded) 'wire-null))
    (should (equal (alist-get 'items decoded) '(wire-false t)))
    (should-not (string-match-p "\\\"top\\\":\\\"false\\\"" wire))
    (should-not (string-match-p "\\\"disabled\\\":\\\"false\\\"" wire))))

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

(ert-deftest qq-transport-quit-during-handoff-retains-request-ownership ()
  "C-g during handoff is consumed so the registered request remains owned."
  (qq-transport-test-with-state
    (let ((quit-flag nil)
          cancelled-timer errback-called observed-pending callback-called)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'request-timer))
                ((symbol-function 'timerp)
                 (lambda (timer) (eq timer 'request-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (timer) (setq cancelled-timer timer)))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _)
                   (setq observed-pending
                         (= (hash-table-count qq-transport--pending) 1))
                   ;; This is how C-g is deferred while `inhibit-quit' is set.
                   (setq quit-flag t))))
        (let ((echo
               (qq-transport-send
                "emacs_send_forward" nil
                (lambda (_) (setq callback-called t))
                (lambda (&rest _) (setq errback-called t)))))
          (should (stringp echo))
          (should (gethash echo qq-transport--pending))
          (should-not quit-flag)
          (should-not cancelled-timer)
          (qq-transport--dispatch-response
           `((echo . ,echo) (status . "ok") (retcode . 0)))))
      (should observed-pending)
      (should callback-called)
      (should-not errback-called)
      (should (eq cancelled-timer 'request-timer))
      (should (= (hash-table-count qq-transport--pending) 0)))))

(ert-deftest qq-transport-pending-quit-before-handoff-registers-nothing ()
  "A quit from the selection phase wins before transport ownership begins."
  (qq-transport-test-with-state
    (let ((inhibit-quit t)
          (quit-flag t)
          send-called timer-created quit-seen)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) (setq timer-created t)))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (setq send-called t))))
        (condition-case nil
            (qq-transport-send "emacs_send_forward" nil)
          (quit (setq quit-seen t))))
      (should quit-seen)
      (should-not quit-flag)
      (should-not send-called)
      (should-not timer-created)
      (should (= (hash-table-count qq-transport--pending) 0)))))

(ert-deftest qq-transport-send-error-settles-and-cancels-timer-once ()
  (qq-transport-test-with-state
    (let (failure (cancel-count 0))
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _) 'request-timer))
                ((symbol-function 'timerp)
                 (lambda (timer) (eq timer 'request-timer)))
                ((symbol-function 'cancel-timer)
                 (lambda (_timer) (cl-incf cancel-count)))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (error "synthetic write failure"))))
        (should-not
         (qq-transport-send
          "emacs_send_forward" nil nil
          (lambda (_response reason) (setq failure reason)))))
      (should (equal failure "synthetic write failure"))
      (should (= cancel-count 1))
      (should (= (hash-table-count qq-transport--pending) 0)))))

(ert-deftest qq-transport-settled-callback-quit-does-not-return-stale-token ()
  (qq-transport-test-with-state
    (let (returned quit-seen)
      (cl-letf (((symbol-function 'websocket-openp) (lambda (_) t))
                ((symbol-function 'websocket-send-text)
                 (lambda (&rest _) (error "synthetic write failure"))))
        (condition-case nil
            (setq returned
                  (qq-transport-send
                   "emacs_send_forward" nil nil
                   (lambda (&rest _) (signal 'quit nil))))
          (quit (setq quit-seen t))))
      (should quit-seen)
      (should-not returned)
      (should (= (hash-table-count qq-transport--pending) 0)))))

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

(ert-deftest qq-transport-disconnect-settles-all-pending-after-callback-quit ()
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws 'socket)
        (qq-transport--connecting t)
        (qq-transport--connection-owner (list :socket 'socket))
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (qq-transport-reconnect-max-attempts 3)
        (qq-transport-reconnect-delay 0.2)
        (quit-flag nil)
        second-called status canceled-timers)
    (puthash
     "quit"
     (list :timer 'timer-one
           :error (lambda (&rest _) (signal 'quit nil)))
     qq-transport--pending)
    (puthash
     "later"
     (list :timer 'timer-two
           :error (lambda (&rest _) (setq second-called t)))
     qq-transport--pending)
    (cl-letf (((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'timerp)
               (lambda (timer) (memq timer '(timer-one timer-two))))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer canceled-timers)))
              ((symbol-function 'run-at-time)
               (lambda (&rest _) 'retry-timer))
              ((symbol-function 'qq-state-set-connection-status)
               (lambda (next) (setq status next))))
      (qq-transport--disconnect t))
    (should second-called)
    (should-not quit-flag)
    (should (= 0 (hash-table-count qq-transport--pending)))
    (should (equal (sort (mapcar #'symbol-name canceled-timers)
                         #'string-lessp)
                   '("timer-one" "timer-two")))
    (should-not qq-transport--ws)
    (should-not qq-transport--connecting)
    (should-not qq-transport--connection-owner)
    (should (eq qq-transport--reconnect-timer 'retry-timer))
    (should (= qq-transport--reconnect-attempt 1))
    (should (eq status 'reconnecting))))

(ert-deftest qq-reset-session-state-completes-after-pending-errback-quit ()
  (let ((qq-runtime--app (appkit-start-app 'qq :id 'test-runtime-app))
        (qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws 'socket)
        (qq-transport--connecting nil)
        (qq-transport--connection-owner nil)
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (quit-flag nil)
        second-called canceled-timers)
    (qq-state-upsert-session
     "group:20001"
     '((type . group) (target-id . "20001") (title . "Old account"))
     nil)
    (puthash
     "quit"
     (list :timer 'timer-one
           :error (lambda (&rest _) (signal 'quit nil)))
     qq-transport--pending)
    (puthash
     "later"
     (list :timer 'timer-two
           :error (lambda (&rest _) (setq second-called t)))
     qq-transport--pending)
    (cl-letf (((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'timerp)
               (lambda (timer) (memq timer '(timer-one timer-two))))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (push timer canceled-timers))))
      (qq-reset-session-state))
    (should second-called)
    (should-not quit-flag)
    (should (= 0 (hash-table-count qq-transport--pending)))
    (should (equal (sort (mapcar #'symbol-name canceled-timers)
                         #'string-lessp)
                   '("timer-one" "timer-two")))
    (should-not qq-runtime--app)
    (should-not (qq-state-session "group:20001"))))

(ert-deftest qq-reset-session-state-kills-renamed-and-legacy-client-buffers ()
  (let* ((qq-runtime--app
          (appkit-start-app 'qq :id 'reset-buffer-privacy-test))
         (qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (qq-transport--pending (make-hash-table :test #'equal))
         (qq-transport--ws nil)
         (qq-transport--connecting nil)
         (qq-transport--connection-owner nil)
         (qq-transport--stopping nil)
         (qq-transport--reconnect-timer nil)
         (qq-transport--reconnect-attempt 0)
         (view-buffer (generate-new-buffer " *qq-reset-view*"))
         (legacy-buffer (generate-new-buffer " *qq-reset-legacy*"))
         ;; A QQ-looking name alone must not make an unrelated buffer a
         ;; destructive-reset target.
         (unrelated-buffer (generate-new-buffer " *qq-reset-unrelated*"))
         kill-observations)
    (unwind-protect
        (progn
          (qq-state-upsert-session
           "group:20001"
           '((type . group) (target-id . "20001")
             (title . "OLD ACCOUNT SECRET"))
           nil)
          (with-current-buffer view-buffer
            (qq-root-mode)
            (let ((inhibit-read-only t))
              (insert "OLD ACCOUNT GENERATED VIEW"))
            (appkit-attach-view
             :app qq-runtime--app :id 'renamed-reset-view
             :mode 'qq-root-mode :parts '(root))
            (rename-buffer
             (generate-new-buffer-name " *renamed-account-view*"))
            (setq-local kill-buffer-query-functions
                        (list (lambda () nil)))
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (push (list (null qq-runtime--app)
                           (null (qq-state-session "group:20001")))
                     kill-observations))
             nil t))
          (with-current-buffer legacy-buffer
            (qq-user-mode)
            (let ((inhibit-read-only t))
              (insert "OLD ACCOUNT LEGACY PROFILE"))
            (setq-local kill-buffer-query-functions
                        (list (lambda () nil)))
            (add-hook
             'kill-buffer-hook
             (lambda ()
               (push (list (null qq-runtime--app)
                           (null (qq-state-session "group:20001")))
                     kill-observations))
             nil t))
          (with-current-buffer unrelated-buffer
            (special-mode)
            (let ((inhibit-read-only t))
              (insert "UNRELATED SENTINEL")))
          (qq-reset-session-state)
          ;; Repeating a destructive reset is harmless and does not broaden
          ;; its buffer selection after the original runtime is gone.
          (qq-reset-session-state)
          (should-not (buffer-live-p view-buffer))
          (should-not (buffer-live-p legacy-buffer))
          (should (= 2 (length kill-observations)))
          (should (cl-every (lambda (observation)
                              (equal observation '(t t)))
                            kill-observations))
          (should-not qq-runtime--app)
          (should-not (qq-state-session "group:20001"))
          (should (buffer-live-p unrelated-buffer))
          (with-current-buffer unrelated-buffer
            (should (equal (buffer-string) "UNRELATED SENTINEL"))))
      (dolist (buffer (list view-buffer legacy-buffer unrelated-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (when (appkit-app-live-p qq-runtime--app)
        (appkit-stop-app qq-runtime--app))
      (setq qq-runtime--app nil)
      (qq-state-reset))))

(ert-deftest qq-transport-synchronous-connect-error-schedules-retry ()
  "A refused socket must not leave the transport permanently connecting."
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws nil)
        (qq-transport--connecting nil)
        (qq-transport--connection-owner nil)
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

(ert-deftest qq-transport-old-socket-close-cannot-settle-new-connection ()
  "A delayed callback is owned by the socket generation that created it."
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws nil)
        (qq-transport--connecting nil)
        (qq-transport--connection-owner nil)
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (qq-transport-reconnect-max-attempts 3)
        (qq-transport-reconnect-delay 0.2)
        callbacks
        retry
        status)
    (cl-letf (((symbol-function 'qq-build-websocket-url)
               (lambda () "ws://127.0.0.1:3001"))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket (if callbacks 'new-socket 'old-socket)))
                   (setq callbacks (append callbacks (list (cons socket args))))
                   socket)))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (_delay _repeat function)
                 (setq retry function)
                 'retry-timer))
              ((symbol-function 'timerp) (lambda (_) t))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'qq-state-set-connection-status)
               (lambda (next) (setq status next))))
      (qq-transport--connect)
      (let* ((old (car callbacks))
             (old-socket (car old))
             (old-args (cdr old)))
        (funcall (plist-get old-args :on-open) old-socket)
        (funcall (plist-get old-args :on-error)
                 old-socket 'connection '(error "failed"))
        (funcall retry)
        (let* ((new (cadr callbacks))
               (new-socket (car new))
               (new-args (cdr new)))
          (funcall (plist-get new-args :on-open) new-socket)
          (should (eq qq-transport--ws new-socket))
          (should (eq status 'open))
          (funcall (plist-get old-args :on-close) old-socket)
          (should (eq qq-transport--ws new-socket))
          (should (eq status 'open))
          (should (qq-transport-running-p)))))))

(ert-deftest qq-transport-close-and-error-settle-one-socket-once ()
  "Invalid-handshake close/error callbacks schedule only one retry."
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws nil)
        (qq-transport--connecting nil)
        (qq-transport--connection-owner nil)
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (qq-transport-reconnect-max-attempts 3)
        (qq-transport-reconnect-delay 0.2)
        args
        (schedule-count 0))
    (cl-letf (((symbol-function 'qq-build-websocket-url)
               (lambda () "ws://127.0.0.1:3001"))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest callback-args)
                 (setq args callback-args)
                 'socket))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _)
                 (cl-incf schedule-count)
                 'retry-timer))
              ((symbol-function 'qq-state-set-connection-status) #'ignore))
      (qq-transport--connect)
      (funcall (plist-get args :on-close) 'socket)
      (funcall (plist-get args :on-error)
               'socket 'connection '(error "same failure"))
      (should (= schedule-count 1))
      (should (= qq-transport--reconnect-attempt 1))
      (should-not qq-transport--connection-owner)
      (should-not (qq-transport-running-p)))))

(ert-deftest qq-transport-synchronous-close-cannot-resurrect-returned-socket ()
  "A socket settled inside `websocket-open' stays settled when it returns."
  (let ((qq-transport--pending (make-hash-table :test #'equal))
        (qq-transport--ws nil)
        (qq-transport--connecting nil)
        (qq-transport--connection-owner nil)
        (qq-transport--stopping nil)
        (qq-transport--reconnect-timer nil)
        (qq-transport--reconnect-attempt 0)
        (qq-transport-reconnect-max-attempts 3)
        (qq-transport-reconnect-delay 0.2)
        captured-owner
        status-history
        (schedule-count 0))
    (cl-letf (((symbol-function 'qq-build-websocket-url)
               (lambda () "ws://127.0.0.1:3001"))
              ((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (let ((socket 'synchronously-settled-socket))
                   (setq captured-owner qq-transport--connection-owner)
                   ;; A failed handshake can notify more than once before the
                   ;; constructor returns.  Even a late open must stay inert.
                   (funcall (plist-get args :on-close) socket)
                   (funcall (plist-get args :on-error)
                            socket 'connection '(error "handshake failed"))
                   (funcall (plist-get args :on-open) socket)
                   socket)))
              ((symbol-function 'websocket-openp) (lambda (_) t))
              ((symbol-function 'websocket-close) #'ignore)
              ((symbol-function 'run-at-time)
               (lambda (&rest _)
                 (cl-incf schedule-count)
                 'retry-timer))
              ((symbol-function 'qq-state-set-connection-status)
               (lambda (next) (push next status-history))))
      (qq-transport--connect)
      (should (= schedule-count 1))
      (should (= qq-transport--reconnect-attempt 1))
      (should (eq qq-transport--reconnect-timer 'retry-timer))
      (should (equal (nreverse status-history) '(connecting reconnecting)))
      (should (plist-get captured-owner :settled))
      (should (eq (plist-get captured-owner :socket)
                  'synchronously-settled-socket))
      (should-not qq-transport--connection-owner)
      (should-not qq-transport--connecting)
      (should-not qq-transport--ws)
      (should-not (qq-transport-running-p)))))

(provide 'qq-transport-test)

;;; qq-transport-test.el ends here
