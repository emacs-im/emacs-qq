;;; qq-completion-test.el --- Tests for QQ composer completion -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qq-chat)
(require 'qq-completion)

(defmacro qq-completion-test-with-group (&rest body)
  "Evaluate BODY in a temporary writable QQ group composer."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (qq-state-reset)
         (qq-state-upsert-session
          "group:20001"
          '((type . group) (target-id . "20001") (title . "Group")) nil)
         (with-temp-buffer
           (qq-chat-mode)
           (setq-local qq-chat--session-key "group:20001")
           (appkit-chatbuf-install-prompt "qq> ")
           ,@body))
     (qq-state-reset)))

(defmacro qq-completion-test-with-private (&rest body)
  "Evaluate BODY in a temporary private chat with peer and self identity."
  (declare (indent 0) (debug t))
  `(unwind-protect
       (progn
         (qq-state-reset)
         (qq-state-set-self-info
          '((user_id . "90001") (nickname . "Myself")))
         (qq-state-upsert-session
          "private:10001"
          '((type . private)
            (target-id . "10001")
            (peer-uin . "10001")
            (title . "Alice"))
          nil)
         (with-temp-buffer
           (qq-chat-mode)
           (setq-local qq-chat--session-key "private:10001")
           ,@body))
     (qq-state-reset)))

(defconst qq-completion-test--member
  '((user_id . "10001")
    (uid . "uid-alice")
    (nickname . "Alice")
    (card . "Alice Card")
    (remark . nil)
    (qid . "alice")
    (title . "管理员")
    (role . "admin")
    (robot . nil)))

(ert-deftest qq-completion-member-capf-inserts-real-at-segment ()
  (qq-completion-test-with-group
    (puthash "alice" (list qq-completion-test--member)
             qq-completion--member-cache)
    (insert "@alice")
    (cl-letf (((symbol-function 'qq-media-avatar-display-string)
               (lambda (_user-id) "@")))
      (let* ((capf (qq-completion-member-capf))
             (table (nth 2 capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function))
             (label (car (all-completions "@alice" table))))
        (should (equal "@Alice Card" label))
        (delete-region (- (point) 6) (point))
        (insert label)
        (funcall exit label 'finished)))
    (goto-char (appkit-chatbuf-input-start-position))
    (let* ((object (appkit-chatbuf-input-object-at-point))
           (segment (plist-get object :segment)))
      (should (equal "at" (alist-get 'type segment)))
      (should (equal "10001"
                     (alist-get 'qq (alist-get 'data segment))))
      (should (equal "Alice Card"
                     (alist-get 'name (alist-get 'data segment)))))
    (should (equal '(((type . "at")
                      (data . ((qq . "10001") (name . "Alice Card")))))
                   (qq-chat--current-input-segments)))))

(ert-deftest qq-completion-member-candidates-search-aliases-and-disambiguate ()
  (let* ((second (copy-tree qq-completion-test--member))
         (_ (setf (alist-get 'user_id second) "10002"
                  (alist-get 'uid second) "u-second"
                  (alist-get 'nickname second) "Another"))
         (candidates
          (qq-completion--member-candidates
           (list qq-completion-test--member second))))
    (should (equal '("@Alice Card" "@Alice Card · 10002")
                   (mapcar #'appkit-chat-completion-candidate-label candidates)))
    (should (member "Alice"
                    (appkit-chat-completion-candidate-search-terms
                     (car candidates))))))

(ert-deftest qq-completion-private-poke-target-reader-is-strict-peer-or-self ()
  (qq-completion-test-with-private
    (let (chosen candidates initial-input)
      (cl-letf (((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt values &rest arguments)
                   (setq candidates values
                         initial-input (plist-get arguments :initial-input))
                   (seq-find
                    (lambda (candidate)
                      (equal
                       (qq-completion--poke-candidate-user-id candidate)
                       "90001"))
                    values))))
        (qq-completion-read-poke-target
         "private:10001"
         (lambda (user-id) (setq chosen user-id))
         "90001"))
      (should (equal chosen "90001"))
      (should
       (equal '("10001" "90001")
              (mapcar #'qq-completion--poke-candidate-user-id candidates)))
      (should
       (equal initial-input
              (appkit-chat-completion-candidate-label (cadr candidates))))
      (should
       (equal (concat (qq-media-avatar-display-string "10001") " ")
              (funcall
               (appkit-chat-completion-candidate-prefix (car candidates))
               (car candidates))))
      (should
       (string-match-p
        "QQ 10001"
        (appkit-chat-completion-candidate-annotation (car candidates)))))))

(ert-deftest qq-completion-private-poke-rejects-non-peer-initial-target ()
  (qq-completion-test-with-private
    (cl-letf (((symbol-function 'appkit-chat-completion-read)
               (lambda (&rest _)
                 (ert-fail "strict private target should fail before reading"))))
      (should-error
       (qq-completion-read-poke-target
        "private:10001" #'ignore "77777")
       :type 'user-error))))

(ert-deftest qq-completion-poke-rejects-zero-user-identities ()
  (qq-completion-test-with-private
    (should-error
     (qq-completion-read-poke-target
      "private:10001" #'ignore "0")
     :type 'user-error)
    (should-error
     (qq-completion--poke-candidate-user-id
      (appkit-chat-completion-candidate-create
       :label "invalid" :value '(:kind poke-target :user-id "0")))
     :type 'error)))

(ert-deftest qq-completion-private-poke-does-not-fallback-to-peer-uin ()
  (qq-completion-test-with-private
    (cl-letf (((symbol-function 'qq-state-session)
               (lambda (_session-key)
                 '((type . private)
                   (target-id . nil)
                   (peer-uin . "10001")
                   (title . "Alice")))))
      (should-error
       (qq-completion-read-poke-target "private:10001" #'ignore)
       :type 'user-error))))

(ert-deftest qq-completion-group-poke-searches-native-then-schedules-picker ()
  (qq-completion-test-with-group
    (let (captured-group captured-query captured-limit
          success errback scheduled chosen initial-query)
      ;; A broad composer cache must never become a poke target source.
      (puthash "" (list '((user_id . "99999")))
               qq-completion--member-cache)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial _history &rest _)
                   (setq initial-query initial)
                   "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (group-id query callback &optional error-callback limit)
                   (setq captured-group group-id
                         captured-query query
                         captured-limit limit
                         success callback
                         errback error-callback)
                   "poke-request-1"))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (setq scheduled (cons function arguments))
                     'scheduled-picker)))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001"
         (lambda (user-id) (setq chosen user-id))
         "10001")
        (should (equal initial-query "10001"))
        (should (equal captured-group "20001"))
        (should (equal captured-query "green"))
        (should (= captured-limit 200))
        (should (functionp success))
        (should (functionp errback))
        (should-not scheduled)
        (funcall success (list qq-completion-test--member))
        (should scheduled)
        (should-not chosen)
        (apply (car scheduled) (cdr scheduled))
        (should (equal chosen "10001"))
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-rejects-empty-query-without-request ()
  (qq-completion-test-with-group
    (let ((requests 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "  "))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (&rest _)
                   (cl-incf requests))))
        (should-error
         (qq-completion-read-poke-target "group:20001" #'ignore)
         :type 'user-error)
        (should (= requests 0))))))

(ert-deftest qq-completion-group-poke-no-result-clears-owner-without-target ()
  (qq-completion-test-with-group
    (let (success chosen)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "missing"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest _)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (ert-fail
                      "empty native result must not open a picker")))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall success nil)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-sync-signal-clears-owner ()
  (qq-completion-test-with-group
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "green"))
              ((symbol-function 'qq-api-search-group-members)
               (lambda (&rest _)
                 (error "synchronous transport failure"))))
      (should-error
       (qq-completion-read-poke-target "group:20001" #'ignore)
       :type 'error)
      (should-not qq-completion--poke-request))))

(ert-deftest qq-completion-group-poke-error-clears-owner-without-target ()
  (qq-completion-test-with-group
    (let (errback chosen)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query _callback &optional error-callback _limit)
                   (setq errback error-callback)
                   "poke-request")))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall errback nil "transport failed")
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-new-request-cancels-old-owner ()
  (qq-completion-test-with-group
    (let ((queries '("first" "second"))
          callbacks tokens cancelled scheduled)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _)
                   (prog1 (car queries) (setq queries (cdr queries)))))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (let ((token (format "request-%d" (1+ (length tokens)))))
                     (setq callbacks (append callbacks (list callback))
                           tokens (append tokens (list token)))
                     token)))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (token) (push token cancelled)))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (push (cons function arguments) scheduled)
                     'scheduled-picker))))
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (should (equal cancelled '("request-1")))
        ;; The cancelled owner's response cannot schedule a picker.
        (funcall (car callbacks) (list qq-completion-test--member))
        (should-not scheduled)
        ;; The current owner still can.
        (funcall (cadr callbacks) (list qq-completion-test--member))
        (should (= (length scheduled) 1))))))

(ert-deftest qq-completion-group-poke-ignores-stale-session-response ()
  (qq-completion-test-with-group
    (let (success scheduled chosen)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (setq scheduled arguments)
                     'scheduled-picker))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq-local qq-chat--session-key "group:other")
        (funcall success (list qq-completion-test--member))
        (should-not scheduled)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-clears-owner-when-picker-turns-stale ()
  (qq-completion-test-with-group
    (let (success scheduled chosen picker-called)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (setq scheduled (cons function arguments))
                     'scheduled-picker)))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (&rest _)
                   (setq picker-called t))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (funcall success (list qq-completion-test--member))
        (should scheduled)
        (setq-local qq-chat--session-key "group:other")
        (apply (car scheduled) (cdr scheduled))
        (should-not picker-called)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-defers-picker-behind-active-minibuffer ()
  (qq-completion-test-with-group
    (let (success scheduled chosen (minibuffer-active t) (picker-calls 0))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function &rest arguments)
                   (when (eq function
                             #'qq-completion--present-group-poke-targets)
                     (push (cons function arguments) scheduled)
                     'scheduled-picker)))
                ((symbol-function 'active-minibuffer-window)
                 (lambda () minibuffer-active))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (cl-incf picker-calls)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (funcall success (list qq-completion-test--member))
        (let ((first-attempt (pop scheduled)))
          (apply (car first-attempt) (cdr first-attempt)))
        (should (= picker-calls 0))
        (should qq-completion--poke-request)
        (should (= (length scheduled) 1))
        (setq minibuffer-active nil)
        (let ((second-attempt (pop scheduled)))
          (apply (car second-attempt) (cdr second-attempt)))
        (should (= picker-calls 1))
        (should (equal chosen "10001"))
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-face-capf-inserts-structured-face ()
  (qq-completion-test-with-group
    (insert "/斜")
    (let* ((capf (qq-completion-face-capf))
           (table (nth 2 capf))
           (exit (plist-get (nthcdr 3 capf) :exit-function))
           (label (seq-find (lambda (candidate)
                              (string-suffix-p "(178)" candidate))
                            (all-completions "/斜" table))))
      (should label)
      (delete-region (- (point) 2) (point))
      (insert label)
      (funcall exit label 'finished))
    (goto-char (appkit-chatbuf-input-start-position))
    (let* ((object (appkit-chatbuf-input-object-at-point))
           (segment (plist-get object :segment)))
      (should (equal "face" (alist-get 'type segment)))
      (should (equal "178" (alist-get 'id (alist-get 'data segment)))))))

(ert-deftest qq-completion-custom-face-prefixes-keep-per-candidate-face ()
  (let ((faces '(((md5 . "one") (file . "/tmp/one.png"))
                 ((md5 . "two") (file . "/tmp/two.png")))))
    (cl-letf (((symbol-function 'qq-media-custom-face-to-segment)
               (lambda (face)
                 `((type . "image") (data . ((name . ,(alist-get 'md5 face)))))))
              ((symbol-function 'qq-media--custom-face-completion-prefix)
               (lambda (face) (alist-get 'md5 face))))
      (let ((candidates (qq-completion--custom-face-candidates faces)))
        (should
         (equal '("one" "two")
                (mapcar
                 (lambda (candidate)
                   (funcall
                    (appkit-chat-completion-candidate-prefix candidate)
                    candidate))
                 candidates)))))))

(ert-deftest qq-completion-cold-member-tab-owns-reopen-request ()
  (qq-completion-test-with-group
    (insert "@missing")
    (cl-letf (((symbol-function 'qq-api-search-group-members)
               (lambda (_group-id _query _callback &optional _errback _limit)
                 'request-token)))
      (should (qq-completion-complete)))
    (should (plist-get (gethash "missing" qq-completion--member-pending)
                       :reopen))))

(ert-deftest qq-completion-mode-binds-telega-style-completion-keys ()
  (should (eq (lookup-key qq-chat-mode-map (kbd "TAB")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "<tab>")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "C-M-i")) #'qq-chat-complete)))

(provide 'qq-completion-test)

;;; qq-completion-test.el ends here
