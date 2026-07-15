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
           (qq-chat--ensure-view)
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
           (qq-chat--ensure-view)
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

(ert-deftest qq-completion-token-at-point-classifies-all-composer-syntax ()
  (qq-completion-test-with-group
    (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
               (lambda ()
                 (list
                  (appkit-chat-completion-candidate-create
                   :label "/斜眼笑  (178)" :search-terms '("斜眼笑" "178")))))
              ((symbol-function 'appkit-chat-emoji-candidates)
               (lambda (&optional _force)
                 (list
                  (appkit-chat-completion-candidate-create
                   :label ":rocket:" :search-terms '("rocket" "🚀"))))))
      (dolist (case '(("@green" member "green")
                      ("/斜眼" face "斜眼")
                      ("/178" face "178")
                      ("/fav" favorite-face "fav")
                      (":rocket" unicode-emoji "rocket")
                      (":rocket:" unicode-emoji "rocket")))
        (appkit-chatbuf-input-set-text (car case))
        (goto-char (point-max))
        (let ((token (qq-completion-token-at-point)))
          (should (eq (plist-get token :kind) (nth 1 case)))
          (should (equal (plist-get token :query) (nth 2 case)))))
      (dolist (text '("hello@example.com" "12:30" "plain"
                      "https://example.com" "/tmp/foo" ":unknown" ":)"))
        (appkit-chatbuf-input-set-text text)
        (goto-char (point-max))
        (should-not (qq-completion-token-at-point))))))

(ert-deftest qq-completion-member-token-is-group-only ()
  (qq-completion-test-with-private
    (appkit-chatbuf-input-set-text "@alice")
    (goto-char (point-max))
    (should-not (qq-completion-token-at-point))))

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
    (should (string-suffix-p " " (appkit-chatbuf-input-string)))
    (should (equal '(((type . "at")
                      (data . ((qq . "10001") (name . "Alice Card")))))
                   (qq-chat--current-input-segments)))
    (goto-char (point-max))
    (appkit-chatbuf-input-backward-delete 1)
    (should (equal "" (appkit-chatbuf-input-string)))
    (should-not (qq-chat--current-input-segments))))

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

(ert-deftest qq-completion-group-poke-result-waits-for-explicit-continuation ()
  (qq-completion-test-with-group
    (let (captured-group captured-query captured-limit success errback chosen
          initial-query owner
          (query-reads 0)
          (picker-reads 0))
      ;; A broad composer cache must never become a poke target source.
      (puthash "" (list '((user_id . "99999")))
               qq-completion--member-cache)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (_prompt &optional initial _history &rest _)
                   (cl-incf query-reads)
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
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (cl-incf picker-reads)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001"
         (lambda (user-id) (setq chosen user-id))
         "10001")
        (setq owner qq-completion--poke-request)
        (should (equal initial-query "10001"))
        (should (equal captured-group "20001"))
        (should (equal captured-query "green"))
        (should (= captured-limit 200))
        (should (functionp success))
        (should (functionp errback))
        (should (eq (plist-get owner :view) (appkit-current-view)))
        (should (eq (plist-get owner :status) 'pending))

        ;; The transport callback updates only the owner model.  It cannot
        ;; open the picker or invoke the poke continuation.
        (funcall success (list qq-completion-test--member))
        (should (= picker-reads 0))
        (should-not chosen)
        (should (eq (plist-get owner :status) 'ready))
        (should (equal (plist-get owner :members)
                       (list qq-completion-test--member)))

        ;; A second explicit command consumes that accepted model.
        (qq-completion-read-poke-target
         "group:20001"
         (lambda (user-id) (setq chosen user-id)))
        (should (= query-reads 1))
        (should (= picker-reads 1))
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
    (let (success chosen notice)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "missing"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall success nil)
        (should-not chosen)
        (should-not notice)
        (should (eq (plist-get qq-completion--poke-request :status) 'empty))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should (equal notice "qq: no matching group member"))
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
    (let (errback chosen notice)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query _callback &optional error-callback _limit)
                   (setq errback error-callback)
                   "poke-request"))
                ((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq notice (apply #'format format-string arguments)))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should qq-completion--poke-request)
        (funcall errback nil "transport failed")
        (should-not chosen)
        (should-not notice)
        (should (eq (plist-get qq-completion--poke-request :status) 'failed))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should (equal notice
                       "qq: failed to search group members: transport failed"))
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-new-request-cancels-old-owner ()
  (qq-completion-test-with-group
    (let ((queries '("first" "second"))
          callbacks tokens cancelled)
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
                 (lambda (token) (push token cancelled))))
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (qq-completion-read-poke-target "group:20001" #'ignore)
        (should (equal cancelled '("request-1")))
        ;; The cancelled owner's response cannot replace the current model.
        (funcall (car callbacks) (list qq-completion-test--member))
        (should (eq (plist-get qq-completion--poke-request :status) 'pending))
        ;; The current owner can accept the model, but still cannot present it.
        (funcall (cadr callbacks) (list qq-completion-test--member))
        (should (eq (plist-get qq-completion--poke-request :status) 'ready))))))

(ert-deftest qq-completion-group-poke-ignores-stale-session-response ()
  (qq-completion-test-with-group
    (let (success chosen picker-called)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (&rest _args) (setq picker-called t))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq-local qq-chat--session-key "group:other")
        (funcall success (list qq-completion-test--member))
        (should-not picker-called)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-ignores-replacement-view-response ()
  (qq-completion-test-with-group
    (let (success chosen picker-called old-view)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (&rest _)
                   (setq picker-called t))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq old-view (plist-get qq-completion--poke-request :view))
        (appkit-kill-view old-view)
        (should-not (eq old-view (qq-chat--ensure-view)))
        (funcall success (list qq-completion-test--member))
        (should-not picker-called)
        (should-not chosen)
        (should-not qq-completion--poke-request)))))

(ert-deftest qq-completion-group-poke-picker-rechecks-exact-view-after-read ()
  (qq-completion-test-with-group
    (let (success chosen owner)
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) "green"))
                ((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   "poke-request"))
                ((symbol-function 'appkit-chat-completion-read)
                 (lambda (_prompt candidates &rest _)
                   (appkit-kill-view (plist-get owner :view))
                   (qq-chat--ensure-view)
                   (car candidates))))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (setq owner qq-completion--poke-request)
        (funcall success (list qq-completion-test--member))
        (qq-completion-read-poke-target
         "group:20001" (lambda (user-id) (setq chosen user-id)))
        (should-not chosen)
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
  (let ((faces '(((md5 . "one") (url . "https://example.invalid/one.png"))
                 ((md5 . "two") (url . "https://example.invalid/two.png")))))
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

(ert-deftest qq-completion-fav-capf-inserts-thumbnail-backed-segment ()
  (qq-completion-test-with-group
    (let ((old-faces qq-media--custom-faces)
          (old-fetched-at qq-media--custom-faces-fetched-at)
          (face '((md5 . "abcdef123456")
                  (desc . "趴")
                  (emo_id . 2)
                  (is_mark_face . :false)
                  (url . "https://example.invalid/favorite.png"))))
      (unwind-protect
          (cl-letf (((symbol-function 'qq-media-custom-face-display-string)
                     (lambda (_face)
                       (propertize "favorite" 'display 'favorite-image))))
            (setq qq-media--custom-faces (list face)
                  qq-media--custom-faces-fetched-at (float-time))
            (insert "/fav趴")
            (let* ((capf (qq-completion-face-capf))
                   (table (nth 2 capf))
                   (exit (plist-get (nthcdr 3 capf) :exit-function))
                   (label (car (all-completions "/fav趴" table))))
              (should (stringp label))
              (delete-region (- (point) 5) (point))
              (insert label)
              (funcall exit label 'finished))
            (goto-char (appkit-chatbuf-input-start-position))
            (let* ((object (appkit-chatbuf-input-object-at-point))
                   (segment (plist-get object :segment)))
              (should (equal "image" (alist-get 'type segment)))
              (should (= 1 (alist-get 'sub_type (alist-get 'data segment))))
              (should (eq 'favorite-image
                          (get-text-property 0 'display
                                             (plist-get object :label))))))
        (setq qq-media--custom-faces old-faces
              qq-media--custom-faces-fetched-at old-fetched-at)))))

(ert-deftest qq-completion-unicode-emoji-capf-inserts-plain-text ()
  (qq-completion-test-with-group
    (insert ":rocket:")
    (let ((appkit-chat-emoji--candidates
           (list
            (appkit-chat-completion-candidate-create
             :label ":rocket:"
             :insert "🚀"))))
      (let* ((capf (qq-completion-unicode-emoji-capf))
             (exit (plist-get (nthcdr 3 capf) :exit-function)))
        (should capf)
        (funcall exit ":rocket:" 'finished)
        (should (equal "🚀" (appkit-chatbuf-input-string)))))))

(ert-deftest qq-completion-cold-fav-tab-caches-without-async-presentation ()
  (qq-completion-test-with-group
    (insert "/fav")
    (let (success owner)
      (cl-letf (((symbol-function 'qq-media-custom-faces-loaded-p)
                 (lambda () nil))
                ((symbol-function 'qq-media-refresh-custom-faces)
                 (lambda (callback &optional _errback _count)
                   (setq success callback)))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "favorite callback must not reopen completion")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "favorite callback must not schedule UI"))))
        (should (qq-completion-complete))
        (setq owner qq-completion--custom-face-pending)
        (should (eq (plist-get owner :view) (appkit-current-view)))
        (funcall success '(((md5 . "one"))))
        (should-not qq-completion--custom-face-pending)))))

(ert-deftest qq-completion-favorite-request-tracks-latest-query ()
  (qq-completion-test-with-group
    (let (success)
      (cl-letf (((symbol-function 'qq-media-ensure-custom-faces)
                 (lambda (callback &optional _errback)
                   (setq success callback)
                   'request-token))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "favorite callback must not schedule UI")))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "favorite callback must not reopen completion"))))
        (qq-completion--request-custom-faces "fav" t)
        (let ((owner qq-completion--custom-face-pending))
          (qq-completion--request-custom-faces "fav趴" t)
          (should (eq owner qq-completion--custom-face-pending))
          (should (eq (plist-get owner :view) (appkit-current-view)))
          (should (equal "fav趴" (plist-get owner :query)))
          (funcall success nil)
          (should-not qq-completion--custom-face-pending))))))

(ert-deftest qq-completion-favorite-aliases-filter-by-description ()
  (let* ((face '((md5 . "one")
                 (desc . "趴")
                 (url . "https://example.invalid/one.png")))
         (candidate (car (qq-completion--custom-face-candidates (list face)))))
    (dolist (alias qq-completion--custom-face-query-prefixes)
      (should
       (appkit-chat-completion--candidate-matches-p
        candidate (concat "/" alias "趴"))))))

(ert-deftest qq-completion-favorite-candidates-exclude-unsendable-faces ()
  (let ((candidates
         (qq-completion--custom-face-candidates
          '("malformed"
            ((md5 . "bad") (desc . "broken"))
            ((md5 . "good")
             (desc . "works")
             (url . "https://example.invalid/good.png"))))))
    (should (= 1 (length candidates)))
    (should (equal "good"
                   (alist-get
                    'md5
                    (plist-get
                     (appkit-chat-completion-candidate-value (car candidates))
                     :face))))))

(ert-deftest qq-completion-cold-member-tab-needs-explicit-second-completion ()
  (qq-completion-test-with-group
    (insert "@missing")
    (let (success (frontend-calls 0))
      (cl-letf (((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   'request-token))
                ((symbol-function 'appkit-chat-completion-complete)
                 (lambda ()
                   (cl-incf frontend-calls)
                   t))
                ((symbol-function 'completion-at-point)
                 (lambda ()
                   (ert-fail "member callback must not reopen completion")))
                ((symbol-function 'run-at-time)
                 (lambda (&rest _args)
                   (ert-fail "member callback must not schedule UI"))))
        (should (qq-completion-complete))
        (let ((owner (gethash "missing" qq-completion--member-pending)))
          (should (eq (plist-get owner :view) (appkit-current-view)))
          (should-not (plist-member owner :reopen)))
        (funcall success (list qq-completion-test--member))
        (should (= frontend-calls 0))
        (should-not (gethash "missing" qq-completion--member-pending))
        (should (equal (qq-completion--cached-members "missing")
                       (list qq-completion-test--member)))
        (should (qq-completion-complete))
        (should (= frontend-calls 1))))))

(ert-deftest qq-completion-replacement-view-rejects-member-response ()
  (qq-completion-test-with-group
    (let (success old-view)
      (cl-letf (((symbol-function 'qq-api-search-group-members)
                 (lambda (_group-id _query callback &optional _errback _limit)
                   (setq success callback)
                   'request-token)))
        (qq-completion--request-members "alice"))
      (setq old-view
            (plist-get (gethash "alice" qq-completion--member-pending) :view))
      (appkit-kill-view old-view)
      (should-not (eq old-view (qq-chat--ensure-view)))
      (funcall success (list qq-completion-test--member))
      (should-not (gethash "alice" qq-completion--member-pending))
      (should (eq qq-completion--cache-miss
                  (qq-completion--cached-members "alice"))))))

(ert-deftest qq-completion-replacement-view-rejects-favorite-response ()
  (qq-completion-test-with-group
    (let (success old-view)
      (cl-letf (((symbol-function 'qq-media-ensure-custom-faces)
                 (lambda (callback &optional _errback)
                   (setq success callback)
                   'request-token)))
        (qq-completion--request-custom-faces "fav"))
      (setq old-view (plist-get qq-completion--custom-face-pending :view))
      (appkit-kill-view old-view)
      (should-not (eq old-view (qq-chat--ensure-view)))
      (funcall success nil)
      (should-not qq-completion--custom-face-pending))))

(ert-deftest qq-completion-mode-binds-telega-style-completion-keys ()
  (should (eq (lookup-key qq-chat-mode-map (kbd "TAB")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "<tab>")) #'qq-chat-complete))
  (should (eq (lookup-key qq-chat-mode-map (kbd "C-M-i")) #'qq-chat-complete)))

(provide 'qq-completion-test)

;;; qq-completion-test.el ends here
