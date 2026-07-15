;;; qq-user-test.el --- Tests for qq-user -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-user)
(require 'qq-user-photo)
(require 'qq-chat)
(require 'qq-root)

(defconst qq-user-test--profile
  '((user_id . "10001")
    (nickname . "Alice")
    (remark . "A")
    (signature . "hello from Emacs")
    (qid . "alice")
    (sex . "female")
    (age . 26)
    (birthday . ((year . 2000) (month . 7) (day . 10)))
    (location . ((country . "中国") (province . "上海") (city)))
    (occupation . "Engineer")
    (college . "Example University")
    (labels . ("Emacs" "QQ"))
    (status . ((code . 10) (extended_code . 0) (description . "在线")))
    (relationship . ((kind . "friend")
                     (blocked_by_me . :false)
                     (special_care . t)
                     (muted . t)
                     (friend_category . ((id . 7) (name . "Friends")))))
    (qq_level . ((stars . 1) (moons . 2) (suns . 3) (crowns . 4)))
    (vip . ((kind . "svip") (level . 8) (annual . t))))
  "One complete native profile fixture.")

(defmacro qq-user-test-with-profile-view (&rest body)
  "Evaluate BODY in a live Appkit user-profile view.

BODY may refer to the lexical variables `buffer' and `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-user-mode)
           (setq qq-user--user-id "10001")
           (let ((buffer (current-buffer))
                 (view (qq-user--ensure-view)))
             (ignore buffer view)
             ,@body))
       (qq-runtime-stop))))

(ert-deftest qq-api-get-user-sends-string-identity ()
  (let (action params value)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall callback `((data . ,qq-user-test--profile)))
                 'request)))
      (should (eq (qq-api-get-user "10001" (lambda (profile)
                                              (setq value profile)))
                  'request))
      (should (equal action "emacs_get_user"))
      (should (equal params '((user_id . "10001"))))
      (should (equal (alist-get 'user_id value) "10001"))
      (should-error (qq-api-get-user 10001 #'ignore) :type 'user-error))))

(ert-deftest qq-user-render-shows-telega-style-profile-card ()
  (with-temp-buffer
    (qq-user-mode)
    (setq qq-user--user-id "10001"
          qq-user--profile (copy-tree qq-user-test--profile)
          qq-user--like-count 42
          qq-user--photo-loaded t
          qq-user--photos
          '(((id . "p1")
             (original_url . "https://example.test/p1.jpg")
             (thumbnail_url . "https://example.test/p1-thumb.jpg"))))
    (cl-letf (((symbol-function 'qq-media-avatar-display-string)
               (lambda (_user-id) "@"))
              ((symbol-function 'qq-user-photo-preview-display-string)
               (lambda (_user-id _photo fallback) fallback)))
      (qq-user-render))
    (let ((text (buffer-string)))
      (should (string-match-p "A" text))
      (should (string-match-p "QQ:[[:space:]]+10001" text))
      (should (string-match-p "特别关心" text))
      (should (string-match-p "消息免打扰" text))
      (should (string-match-p "分组:[[:space:]]+Friends" text))
      (should (string-match-p "获赞:[[:space:]]+42" text))
      (should (string-match-p "照片墙:[[:space:]]+1" text))
      (should (string-match-p "SVIP 8" text))
      (should (string-match-p "hello from Emacs" text))
      (should-not (string-match-p "\\[Open\\]" text))
      (should (equal (get-text-property (point-min) 'qq-user-profile-key)
                     '(user-profile "10001")))
      (goto-char (point-min))
      (search-forward "发消息")
      (should (button-at (1- (point))))
      (search-forward "点赞")
      (should (button-at (1- (point))))
      (search-forward "照片 1")
      (should (button-at (1- (point)))))))

(ert-deftest qq-user-render-omits-private-placeholder-values ()
  (with-temp-buffer
    (qq-user-mode)
    (setq qq-user--user-id "10001"
          qq-user--profile
          '((user_id . "10001")
            (nickname . "Alice")
            (sex . "unknown")
            (age)
            (status . ((code . 99) (description)))
            (relationship . ((kind . "friend")))
            (qq_level)
            (vip)
            (registered_at . 123456)))
    (cl-letf (((symbol-function 'qq-media-avatar-display-string)
               (lambda (_user-id) "@")))
      (qq-user-render))
    (let ((text (buffer-string)))
      (should (string-match-p "关系:[[:space:]]+好友" text))
      (should-not (string-match-p "状态:" text))
      (should-not (string-match-p "年龄:" text))
      (should-not (string-match-p "性别:" text))
      (should-not (string-match-p "等级:" text))
      (should-not (string-match-p "会员:" text))
      (should-not (string-match-p "注册:" text)))))

(ert-deftest qq-user-render-exposes-profile-like-loading-and-errors ()
  (with-temp-buffer
    (qq-user-mode)
    (setq qq-user--user-id "10001"
          qq-user--profile (copy-tree qq-user-test--profile)
          qq-user--like-loading t)
    (cl-letf (((symbol-function 'qq-media-avatar-display-string)
               (lambda (_user-id) "@")))
      (qq-user-render))
    (should (string-match-p "获赞:[[:space:]]+加载中" (buffer-string)))
    (setq qq-user--like-loading nil
          qq-user--like-error "native query failed")
    (qq-user-render)
    (should (string-match-p "获赞:[[:space:]]+获取失败" (buffer-string)))))

(ert-deftest qq-user-refresh-handles-synchronous-response-ownership ()
  (qq-user-test-with-profile-view
    (cl-letf (((symbol-function 'qq-user-render) #'ignore)
              ((symbol-function 'qq-api-get-user)
               (lambda (_user-id callback &optional _errback)
                 (funcall callback (copy-tree qq-user-test--profile))
                 'request))
              ((symbol-function 'qq-api-get-user-like)
               (lambda (_user-id callback &optional _errback)
                 (funcall callback 42)
                 'like-request))
              ((symbol-function 'qq-api-get-user-photo-wall)
               (lambda (_user-id callback &optional _errback)
                 (funcall callback nil)
                 'photo-request)))
      (qq-user-refresh)
      (should (equal qq-user--profile qq-user-test--profile))
      (should (= qq-user--like-count 42))
      (should qq-user--photo-loaded)
      (should-not qq-user--loading)
      (should-not qq-user--request)
      (should-not qq-user--request-owner))))

(ert-deftest qq-chat-open-user-at-point-uses-sender-id ()
  (let (opened)
    (cl-letf (((symbol-function 'qq-chat--message-at-point)
               (lambda () '((sender-id . "10001"))))
              ((symbol-function 'qq-user-open)
               (lambda (user-id) (setq opened user-id))))
      (qq-chat-open-user-at-point)
      (should (equal opened "10001")))))

(ert-deftest qq-root-open-user-at-point-requires-private-session ()
  (let (opened)
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . private)
                            (target-id . "10001"))))
              ((symbol-function 'qq-user-open)
               (lambda (user-id) (setq opened user-id))))
      (qq-root-open-user-at-point)
      (should (equal opened "10001")))
    (cl-letf (((symbol-function 'qq-root--session-at-point)
               (lambda () '((type . group)
                            (target-id . "20001")))))
      (should-error (qq-root-open-user-at-point) :type 'user-error))))

(ert-deftest qq-user-entry-keymaps-are-discoverable ()
  (should (eq (lookup-key qq-chat-timeline-mode-map (kbd "i"))
              #'qq-chat-open-user-at-point))
  (should (eq (lookup-key qq-root-mode-map (kbd "i"))
              #'qq-root-open-info-at-point))
  (should (eq (lookup-key qq-user-mode-map (kbd "m"))
              #'qq-user-open-chat))
  (should (eq (lookup-key qq-user-mode-map (kbd "l"))
              #'qq-user-like))
  (should (eq (lookup-key qq-user-mode-map (kbd "p"))
              #'qq-user-open-photo-wall)))

(ert-deftest qq-user-reuses-one-profile-buffer-like-telega ()
  (should (equal (qq-user--buffer-name "10001") "*qq-user*"))
  (should (equal (qq-user--buffer-name "10002") "*qq-user*")))

(ert-deftest qq-user-open-reuses-renamed-live-view-and-media-hook ()
  (qq-user-test-with-profile-view
    (let* ((profile (copy-tree qq-user-test--profile))
           (hook qq-user--media-hook-function)
           (renamed (generate-new-buffer-name " *qq-user-renamed*"))
           selected
           calls)
      (setq qq-user--profile profile)
      (rename-buffer renamed)
      (cl-letf (((symbol-function 'pop-to-buffer)
                 (lambda (candidate &rest _arguments)
                   (setq selected candidate)
                   candidate))
                ((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest arguments)
                   (push (cons candidate arguments) calls)
                   'owned-timer)))
        (should (eq buffer (qq-user-open "10001")))
        (should (eq selected buffer))
        (should (eq view (appkit-current-view)))
        (should (equal qq-user--view-id (appkit-view-id view)))
        (should (eq profile qq-user--profile))
        (should (eq hook qq-user--media-hook-function))
        (should (equal renamed (buffer-name buffer)))
        (should-not (get-buffer (qq-user--buffer-name "10001")))
        (setq calls nil)
        (funcall hook "avatar:10001"))
      (should (= 1 (length calls)))
      (should (eq view (caar calls)))
      (should (equal '(user-profile "10001")
                     (plist-get (cdar calls) :entry)))
      (should (equal "avatar:10001"
                     (plist-get (cdar calls) :resource)))
      (should-not (plist-get (cdar calls) :structure)))))

(ert-deftest qq-user-replacement-setup-clears-owned-view-state ()
  (qq-user-test-with-profile-view
    (let ((old-hook qq-user--media-hook-function)
          cancelled)
      (setq qq-user--profile (copy-tree qq-user-test--profile)
            qq-user--loading t
            qq-user--request 'old-profile-request
            qq-user--request-owner 'old-owner
            qq-user--like-count 42
            qq-user--like-loading t
            qq-user--photos '(((id . "old-photo")))
            qq-user--photo-loading t
            qq-user--photo-loaded t)
      (cl-letf (((symbol-function 'qq-api-cancel-request)
                 (lambda (request) (push request cancelled))))
        (appkit-kill-view view)
        (should (member 'old-profile-request cancelled))
        (should-not qq-user--profile)
        (should-not qq-user--loading)
        (should-not qq-user--like-count)
        (should-not qq-user--like-loading)
        (should-not qq-user--photos)
        (should-not qq-user--photo-loading)
        (should-not qq-user--photo-loaded)
        (should-not qq-user--media-hook-function)
        (should-not (memq old-hook qq-media-cache-update-hook))
        ;; Simulate residue in the detached buffer.  The replacement's
        ;; `:setup' boundary must clear it without affecting ordinary reuse.
        (setq qq-user--profile '((user_id . "stale"))
              qq-user--loading t
              qq-user--request 'orphan-profile-request
              qq-user--request-owner 'orphan-owner
              qq-user--media-hook-function old-hook)
        (add-hook 'qq-media-cache-update-hook old-hook)
        (let ((replacement
               (appkit-open-view
                :app (qq-runtime-app)
                :id qq-user--view-id
                :mode 'qq-user-mode
                :buffer-name (qq-user--buffer-name "10001")
                :sync-function #'qq-user--sync-invalidations
                :parts '(profile)
                :setup #'qq-user--setup-view)))
          (should (appkit-view-live-p replacement))
          (should-not (eq replacement view))
          (should (member 'orphan-profile-request cancelled))
          (should-not qq-user--profile)
          (should-not qq-user--loading)
          (should-not qq-user--request)
          (should-not qq-user--request-owner)
          (should-not (memq old-hook qq-media-cache-update-hook))
          (should qq-user--media-hook-function)
          (should (memq qq-user--media-hook-function
                        qq-media-cache-update-hook))
          (setq qq-user--profile '((user_id . "replacement")))
          (qq-user--release-view-work view buffer)
          (should (equal qq-user--profile
                         '((user_id . "replacement")))))))))

(ert-deftest qq-user-dead-view-makes-late-profile-response-inert ()
  (qq-user-test-with-profile-view
    (let (success)
      (cl-letf (((symbol-function 'qq-api-get-user)
                 (lambda (_user-id callback &optional _errback)
                   (setq success callback)
                   'profile-token))
                ((symbol-function 'qq-api-get-user-like)
                 (lambda (&rest _arguments) 'like-token))
                ((symbol-function 'qq-api-get-user-photo-wall)
                 (lambda (&rest _arguments) 'photo-token))
                ((symbol-function 'qq-api-cancel-request) #'ignore))
        (qq-user-refresh)
        (appkit-kill-view view)
        (let ((replacement (qq-user--ensure-view))
              (replacement-profile '((user_id . "10001")
                                     (nickname . "Replacement"))))
          (setq qq-user--profile replacement-profile)
          (cl-letf (((symbol-function 'appkit-request-sync)
                     (lambda (&rest _arguments)
                       (ert-fail "late user response requested a sync")))
                    ((symbol-function 'qq-api--default-error)
                     (lambda (&rest _arguments)
                       (ert-fail "late user response emitted an error"))))
            (funcall success (copy-tree qq-user-test--profile)))
          (should (appkit-view-live-p replacement))
          (should (eq replacement-profile qq-user--profile))
          (should-not qq-user--request-owner))))))

(ert-deftest qq-user-profile-callback-uses-atomic-appkit-request-sync ()
  (qq-user-test-with-profile-view
    (let (success calls)
      (cl-letf (((symbol-function 'qq-api-get-user)
                 (lambda (_user-id callback &optional _errback)
                   (setq success callback)
                   'profile-token))
                ((symbol-function 'qq-api-get-user-like)
                 (lambda (&rest _arguments) 'like-token))
                ((symbol-function 'qq-api-get-user-photo-wall)
                 (lambda (&rest _arguments) 'photo-token)))
        (qq-user-refresh))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (candidate &rest arguments)
                     (push (cons candidate arguments) calls)
                     'owned-timer))
                  ((symbol-function 'appkit-invalidate)
                   (lambda (&rest _arguments)
                     (ert-fail "user callback used bare invalidation")))
                  ((symbol-function 'appkit-schedule-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "user callback used bare scheduling")))
                  ((symbol-function 'appkit-sync-invalidations)
                   (lambda (&rest _arguments)
                     (ert-fail "user callback synchronized inline")))
                  ((symbol-function 'qq-user-render)
                   (lambda () (ert-fail "user callback rendered directly")))
                  ((symbol-function 'erase-buffer)
                   (lambda () (ert-fail "user callback erased the buffer"))))
          (funcall success (copy-tree qq-user-test--profile)))
        (should (equal before (buffer-string))))
      (should (equal qq-user--profile qq-user-test--profile))
      (should-not qq-user--loading)
      (should-not qq-user--request-owner)
      (should (= 1 (length calls)))
      (should (eq view (caar calls)))
      (should (plist-get (cdar calls) :structure))
      (should (eq 'profile (plist-get (cdar calls) :part))))))

(ert-deftest qq-user-global-search-result-is-strict-and-closed ()
  (let ((result
         `((user_id . "9007199254740993")
           (uid . "u_synthetic_candidate")
           (nickname . "Synthetic User")
           (avatar_url . "https://example.invalid/avatar")
           (candidate . ,(make-string 43 ?C)))))
    (should (qq-user--global-search-result-p result))
    (should-not
     (qq-user--global-search-result-p
      (append (copy-tree result) '((native . "must not escape")))))
    (setf (alist-get 'user_id result) 9007199254740993)
    (should-not (qq-user--global-search-result-p result))))

(ert-deftest qq-user-friend-add-prepare-settles-synchronous-callback ()
  (with-temp-buffer
    (qq-user-mode)
    (let* ((candidate (make-string 43 ?C))
           (preparation (make-string 43 ?P))
           (state
            (qq-user--friend-add-state-create
             :user-id "9007199254740993"
             :candidate candidate
             :status 'candidate)))
      (setq qq-user--user-id "9007199254740993"
            qq-user--friend-add-state state)
      (cl-letf (((symbol-function 'qq-user-render) #'ignore)
                ((symbol-function 'qq-api-friend-add-prepare)
                 (lambda (user-id token callback &optional _errback)
                   (should (equal user-id "9007199254740993"))
                   (should (equal token candidate))
                   (funcall callback
                            `((kind . "prepared")
                              (user_id . "9007199254740993")
                              (verification . "message")
                              (questions)
                              (preparation . ,preparation)))
                   'already-finished)))
        (qq-user--prepare-friend-add state))
      (should (eq (qq-user--friend-add-state-status state) 'prepared))
      (should-not (qq-user--friend-add-state-candidate state))
      (should (equal (qq-user--friend-add-state-preparation state)
                     preparation))
      (should-not (qq-user--friend-add-state-request state)))))

(ert-deftest qq-user-friend-add-prepare-failure-is-terminal-and-tokenless ()
  (with-temp-buffer
    (qq-user-mode)
    (let ((state
           (qq-user--friend-add-state-create
            :user-id "9007199254740993"
            :candidate (make-string 43 ?C)
            :status 'candidate)))
      (setq qq-user--user-id "9007199254740993"
            qq-user--friend-add-state state)
      (cl-letf (((symbol-function 'qq-user-render) #'ignore)
                ((symbol-function 'qq-api-friend-add-prepare)
                 (lambda (_user-id _token _callback &optional errback)
                   (funcall errback nil "synthetic protocol failure")
                   'already-finished)))
        (qq-user--prepare-friend-add state))
      (should (eq (qq-user--friend-add-state-status state) 'error))
      (should-not (qq-user--friend-add-state-candidate state))
      (should-not (qq-user--friend-add-state-preparation state))
      (should-not (qq-user--friend-add-state-request state))
      (should (equal (qq-user--friend-add-state-error state)
                     "synthetic protocol failure")))))

(ert-deftest qq-user-friend-add-stale-callback-cannot-mutate-new-state ()
  (with-temp-buffer
    (qq-user-mode)
    (let* ((user-id "9007199254740993")
           (old
            (qq-user--friend-add-state-create
             :user-id user-id :candidate (make-string 43 ?O)
             :status 'candidate))
           (new
            (qq-user--friend-add-state-create
             :user-id user-id :candidate (make-string 43 ?N)
             :status 'candidate))
           callback)
      (setq qq-user--user-id user-id
            qq-user--friend-add-state old)
      (cl-letf (((symbol-function 'qq-user-render) #'ignore)
                ((symbol-function 'qq-api-friend-add-prepare)
                 (lambda (_user-id _token success &optional _errback)
                   (setq callback success)
                   'pending-request)))
        (qq-user--prepare-friend-add old)
        (setq qq-user--friend-add-state new)
        (funcall callback
                 `((kind . "prepared")
                   (user_id . ,user-id)
                   (verification . "none")
                   (questions)
                   (preparation . ,(make-string 43 ?P)))))
      (should (eq (qq-user--friend-add-state-status new) 'candidate))
      (should (qq-user--friend-add-state-candidate new))
      (should-not (qq-user--friend-add-state-preparation new)))))

(ert-deftest qq-user-friend-add-state-must-own-its-exact-user ()
  (with-temp-buffer
    (qq-user-mode)
    (let ((state
           (qq-user--friend-add-state-create
            :user-id "9007199254740993" :status 'candidate)))
      (setq qq-user--user-id "9007199254740994"
            qq-user--friend-add-state state)
      (should-not
       (qq-user--friend-add-current-p state "9007199254740994")))))

(ert-deftest qq-user-friend-add-message-reads-only-verification-message ()
  (let ((state
         (qq-user--friend-add-state-create
          :verification "message" :questions nil)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _args)
                 (should (equal prompt "验证留言: "))
                 "Synthetic verification message")))
      (should
       (equal (qq-user--read-friend-verification state)
              '((verification_message . "Synthetic verification message")
                (answers)))))))

(ert-deftest qq-user-friend-add-question-answer-reads-every-answer ()
  (let ((state
         (qq-user--friend-add-state-create
          :verification "question_answer"
          :questions '("Synthetic question one?"
                       "Synthetic question two?")))
        (responses '("Synthetic answer one" "Synthetic answer two"))
        prompts)
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _args)
                 (push prompt prompts)
                 (prog1 (car responses)
                   (setq responses (cdr responses))))))
      (should
       (equal (qq-user--read-friend-verification state)
              '((verification_message . "")
                (answers . ("Synthetic answer one"
                            "Synthetic answer two"))))))
    (should-not responses)
    (should
     (equal (nreverse prompts)
            '("问题 1/2（Synthetic question one?）答案: "
              "问题 2/2（Synthetic question two?）答案: ")))))

(ert-deftest qq-user-friend-add-none-does-not-prompt-or-invent-answers ()
  (let ((state (qq-user--friend-add-state-create :verification "none")))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) (ert-fail "must not prompt"))))
      (should
       (equal (qq-user--read-friend-verification state)
              '((verification_message . "") (answers)))))))

(ert-deftest qq-user-friend-add-rejects-old-question-mode ()
  (let ((state
         (qq-user--friend-add-state-create
          :verification "question" :questions '("Legacy question"))))
    (should-error (qq-user--read-friend-verification state) :type 'error)))

(ert-deftest qq-user-friend-add-verification-uses-utf-16-code-units ()
  (let ((astral #x1f600)
        (message-state
         (qq-user--friend-add-state-create :verification "message"))
        (answer-state
         (qq-user--friend-add-state-create
          :verification "question_answer" :questions '("Question?"))))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) (make-string 150 astral))))
      (should (qq-user--read-friend-verification message-state))
      (should (qq-user--read-friend-verification answer-state)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _args) (make-string 151 astral))))
      (should-error (qq-user--read-friend-verification message-state)
                    :type 'user-error)
      (should-error (qq-user--read-friend-verification answer-state)
                    :type 'user-error))))

(ert-deftest qq-user-friend-add-prevalidates-options-before-consuming-token ()
  (with-temp-buffer
    (qq-user-mode)
    (let* ((astral #x1f600)
           (user-id "9007199254740993")
           (preparation (make-string 43 ?P))
           (state
            (qq-user--friend-add-state-create
             :user-id user-id :preparation preparation
             :verification "none" :status 'prepared))
           confirmed dispatched)
      (setq qq-user--user-id user-id
            qq-user--friend-add-state state)
      (cl-letf (((symbol-function 'qq-user--read-friend-verification)
                 (lambda (_state)
                   '((verification_message . "") (answers))))
                ((symbol-function 'read-string)
                 (lambda (&rest _args) (make-string 51 astral)))
                ((symbol-function 'qq-user--read-friend-group-id)
                 (lambda () 0))
                ((symbol-function 'qq-user--read-friend-permissions)
                 (lambda ()
                   '((only_chat) (qzone_not_watch) (qzone_not_watched))))
                ((symbol-function 'yes-or-no-p)
                 (lambda (&rest _args) (setq confirmed t)))
                ((symbol-function 'qq-api-friend-add-submit)
                 (lambda (&rest _args) (setq dispatched t))))
        (should-error (qq-user--submit-friend-add state) :type 'user-error))
      (should-not confirmed)
      (should-not dispatched)
      (should (equal (qq-user--friend-add-state-preparation state)
                     preparation))
      (should (eq (qq-user--friend-add-state-status state) 'prepared)))))

(ert-deftest qq-user-friend-add-submit-sends-closed-explicit-options ()
  (with-temp-buffer
    (qq-user-mode)
    (let* ((user-id "9007199254740993")
           (preparation (make-string 43 ?P))
           (state
            (qq-user--friend-add-state-create
             :user-id user-id
             :preparation preparation
             :verification "message"
             :status 'prepared))
           (permission-answers '(t nil t))
           permission-prompts
           submitted-options
           submitted-preparation)
      (setq qq-user--user-id user-id
            qq-user--friend-add-state state)
      (cl-letf (((symbol-function 'qq-user-render) #'ignore)
                ((symbol-function 'qq-user--read-friend-verification)
                 (lambda (candidate-state)
                   (should (eq candidate-state state))
                   '((verification_message . "Synthetic verification")
                     (answers))))
                ((symbol-function 'read-string)
                 (lambda (prompt &rest _args)
                   (should (equal prompt "好友备注（可空）: "))
                   "Synthetic remark"))
                ((symbol-function 'qq-user--read-friend-group-id)
                 (lambda () 7))
                ((symbol-function 'y-or-n-p)
                 (lambda (prompt)
                   (push prompt permission-prompts)
                   (prog1 (car permission-answers)
                     (setq permission-answers (cdr permission-answers)))))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (should (string-match-p "确认向" prompt))
                   t))
                ((symbol-function 'qq-api-friend-add-submit)
                 (lambda (target-user-id token options callback
                          &optional _errback)
                   (should (equal target-user-id user-id))
                   (setq submitted-preparation token
                         submitted-options (copy-tree options))
                   ;; The one-use capability must leave UI state before any
                   ;; mutating transport dispatch can observe it.
                   (should-not
                    (qq-user--friend-add-state-preparation state))
                   (should (eq (qq-user--friend-add-state-status state)
                               'submitting))
                   (funcall callback
                            `((kind . "submitted") (user_id . ,user-id)))
                   'already-finished)))
        (qq-user--submit-friend-add state))
      (should (equal submitted-preparation preparation))
      (should
       (equal submitted-options
              '((verification_message . "Synthetic verification")
                (answers)
                (remark . "Synthetic remark")
                (friend_group_id . 7)
                (only_chat . t)
                (qzone_not_watch)
                (qzone_not_watched . t))))
      (should (equal (nreverse permission-prompts)
                     '("权限：仅聊天？ "
                       "权限：不看对方的 QQ 空间？ "
                       "权限：不让对方看我的 QQ 空间？ ")))
      (should-not permission-answers)
      (should (eq (qq-user--friend-add-state-status state) 'submitted))
      (should-not (qq-user--friend-add-state-request state))
      (should-not (qq-user--friend-add-state-preparation state)))))

(ert-deftest qq-user-friend-group-selection-has-no-guessed-default ()
  (cl-letf (((symbol-function 'qq-state-friend-categories) (lambda () nil))
            ((symbol-function 'completing-read)
             (lambda (&rest _args)
               (ert-fail "must not prompt without authoritative categories"))))
    (should-not (qq-user--friend-group-choices))
    (should-error (qq-user--read-friend-group-id) :type 'user-error)))

(ert-deftest qq-user-friend-group-choices-show-member-count-not-native-id ()
  (let ((categories
         '(((category_id . 9999) (name . "特别关心") (friends))
           ((category_id . 7) (name . "Synthetic peers")
            (friends . (((user_id . "10001"))
                        ((user_id . "10002"))))))))
    (cl-letf (((symbol-function 'qq-state-friend-categories)
               (lambda () categories)))
      (should
       (equal (qq-user--friend-group-choices)
              '(("特别关心 · 0 位好友" . 9999)
                ("Synthetic peers · 2 位好友" . 7)))))))

(ert-deftest qq-user-friend-group-choices-disambiguate-duplicate-labels ()
  (let ((categories
         '(((category_id . 7) (name . "Synthetic") (friends))
           ((category_id . 9) (name . "Synthetic") (friends)))))
    (cl-letf (((symbol-function 'qq-state-friend-categories)
               (lambda () categories)))
      (should
       (equal (qq-user--friend-group-choices)
              '(("Synthetic · 0 位好友 · 分组 7" . 7)
                ("Synthetic · 0 位好友 · 分组 9" . 9)))))))

(ert-deftest qq-user-friend-group-selection-uses-native-default-group ()
  (let ((categories
         '(((category_id . 9999) (name . "Special") (friends))
           ((category_id . 0) (name . "Default")
            (friends . (((user_id . "10001")))))))
        completion-default
        completion-require-match)
    (cl-letf (((symbol-function 'qq-state-friend-categories)
               (lambda () categories))
              ((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate require-match
                        _initial _history default)
                 (setq completion-default default
                       completion-require-match require-match)
                 default)))
      (should (= (qq-user--read-friend-group-id) 0))
      (should completion-require-match)
      (should (equal completion-default "Default · 1 位好友")))))

(ert-deftest qq-api-get-user-social-actions-preserve-string-identity ()
  (let (calls like added photos)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (push (list action params) calls)
                 (funcall
                  callback
                  (pcase action
                    ("emacs_get_user_like"
                     '((data . ((user_id . "10001") (total_count . 42)))))
                    ("emacs_like_user"
                     '((data . ((user_id . "10001")
                                (outcome . "liked")
                                (added_count . 1)))))
                    ("emacs_get_user_photo_wall"
                     '((data . ((user_id . "10001")
                                (photos . (((id . "p1")
                                            (original_url . "https://example.test/p1.jpg")
                                            (thumbnail_url))))))))))
                 'request)))
      (qq-api-get-user-like "10001" (lambda (count) (setq like count)))
      (qq-api-like-user "10001" (lambda (count) (setq added count)))
      (qq-api-get-user-photo-wall "10001" (lambda (value) (setq photos value))))
    (should (= like 42))
    (should (equal (alist-get 'outcome added) "liked"))
    (should (equal (alist-get 'id (car photos)) "p1"))
    (should (member '("emacs_get_user_like" ((user_id . "10001"))) calls))
    (should (member '("emacs_like_user" ((user_id . "10001"))) calls))
    (should (member '("emacs_get_user_photo_wall" ((user_id . "10001"))) calls))))

(ert-deftest qq-api-like-user-rejects-unknown-domain-outcomes ()
  (let (failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          '((data . ((user_id . "10001")
                                     (outcome . "mystery")))))
                 'request)))
      (qq-api-like-user
       "10001" #'ignore
       (lambda (_response reason) (setq failure reason))))
    (should (string-match-p "unknown outcome" failure))))

(ert-deftest qq-user-like-refreshes-the-exact-profile-after-success ()
  (qq-user-test-with-profile-view
    (setq qq-user--profile (copy-tree qq-user-test--profile))
    (let (liked-user-id refreshed)
      (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "99999"))
                ((symbol-function 'qq-user-render) #'ignore)
                ((symbol-function 'qq-user--refresh-like)
                 (lambda (&optional _view) (setq refreshed t)))
                ((symbol-function 'qq-api-like-user)
                 (lambda (user-id callback &optional _errback)
                   (setq liked-user-id user-id)
                   (funcall callback
                            '((user_id . "10001")
                              (outcome . "liked")
                              (added_count . 1)))
                   'request)))
        (qq-user-like))
      (should (equal liked-user-id "10001"))
      (should refreshed)
      (should-not qq-user--send-like-request)
      (should-not qq-user--send-like-request-owner))))

(ert-deftest qq-user-refresh-like-dispatch-signal-allows-exact-retry ()
  (qq-user-test-with-profile-view
    (let ((calls 0)
          old-callback new-callback)
      (cl-letf (((symbol-function 'qq-api-get-user-like)
                 (lambda (_user-id callback &optional _errback)
                   (cl-incf calls)
                   (if (= calls 1)
                       (progn
                         (setq old-callback callback)
                         (error "like dispatch failed"))
                     (setq new-callback callback)
                     'like-request-2))))
        (qq-user--refresh-like view)
        (should (= calls 1))
        (should-not qq-user--like-loading)
        (should-not qq-user--like-request)
        (should-not qq-user--like-request-owner)
        (should (string-match-p "like dispatch failed"
                                qq-user--like-error))
        (qq-user--refresh-like view)
        (let ((retry-owner qq-user--like-request-owner))
          (should (= calls 2))
          (should qq-user--like-loading)
          (should (eq qq-user--like-request 'like-request-2))
          (should retry-owner)
          (funcall old-callback 99)
          (should (eq qq-user--like-request-owner retry-owner))
          (should (eq qq-user--like-request 'like-request-2))
          (should qq-user--like-loading)
          (should-not qq-user--like-count)
          (funcall new-callback 42)
          (should (= qq-user--like-count 42))
          (should-not qq-user--like-loading)
          (should-not qq-user--like-request)
          (should-not qq-user--like-request-owner))))))

(ert-deftest qq-user-refresh-photos-dispatch-signal-allows-exact-retry ()
  (qq-user-test-with-profile-view
    (let ((calls 0)
          old-callback new-callback)
      (cl-letf (((symbol-function 'qq-api-get-user-photo-wall)
                 (lambda (_user-id callback &optional _errback)
                   (cl-incf calls)
                   (if (= calls 1)
                       (progn
                         (setq old-callback callback)
                         (error "photo dispatch failed"))
                     (setq new-callback callback)
                     'photo-request-2))))
        (qq-user--refresh-photos view)
        (should (= calls 1))
        (should-not qq-user--photo-loading)
        (should-not qq-user--photo-request)
        (should-not qq-user--photo-request-owner)
        (should-not qq-user--photo-loaded)
        (qq-user--refresh-photos view)
        (let ((retry-owner qq-user--photo-request-owner)
              (old-photos '(((id . "OLD_ACCOUNT_SECRET"))))
              (new-photos '(((id . "new-photo")))))
          (should (= calls 2))
          (should qq-user--photo-loading)
          (should (eq qq-user--photo-request 'photo-request-2))
          (should retry-owner)
          (funcall old-callback old-photos)
          (should (eq qq-user--photo-request-owner retry-owner))
          (should (eq qq-user--photo-request 'photo-request-2))
          (should qq-user--photo-loading)
          (should-not qq-user--photos)
          (funcall new-callback new-photos)
          (should (equal qq-user--photos new-photos))
          (should qq-user--photo-loaded)
          (should-not qq-user--photo-loading)
          (should-not qq-user--photo-request)
          (should-not qq-user--photo-request-owner))))))

(ert-deftest qq-user-send-like-dispatch-signal-allows-exact-retry ()
  (qq-user-test-with-profile-view
    (setq qq-user--profile (copy-tree qq-user-test--profile))
    (let ((calls 0)
          old-callback new-callback
          refreshed-view)
      (cl-letf (((symbol-function 'qq-state-self-user-id)
                 (lambda () "99999"))
                ((symbol-function 'qq-api-like-user)
                 (lambda (_user-id callback &optional _errback)
                   (cl-incf calls)
                   (if (= calls 1)
                       (progn
                         (setq old-callback callback)
                         (error "send-like dispatch failed"))
                     (setq new-callback callback)
                     'send-like-request-2)))
                ((symbol-function 'qq-user--refresh-like)
                 (lambda (&optional candidate)
                   (setq refreshed-view candidate))))
        (should-error (qq-user-like) :type 'error)
        (should (= calls 1))
        (should-not qq-user--send-like-request)
        (should-not qq-user--send-like-request-owner)
        (qq-user-like)
        (let ((retry-owner qq-user--send-like-request-owner))
          (should (= calls 2))
          (should retry-owner)
          (should (eq qq-user--send-like-request 'send-like-request-2))
          (funcall old-callback
                   '((user_id . "10001")
                     (outcome . "liked")
                     (added_count . 1)))
          (should (eq qq-user--send-like-request-owner retry-owner))
          (should (eq qq-user--send-like-request 'send-like-request-2))
          (should-not refreshed-view)
          (funcall new-callback
                   '((user_id . "10001")
                     (outcome . "liked")
                     (added_count . 1)))
          (should (eq refreshed-view view))
          (should-not qq-user--send-like-request)
          (should-not qq-user--send-like-request-owner))))))

(ert-deftest qq-user-like-renders-the-daily-limit-as-domain-state ()
  (qq-user-test-with-profile-view
    (setq qq-user--profile (copy-tree qq-user-test--profile))
    (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "99999"))
              ((symbol-function 'qq-api-like-user)
               (lambda (_user-id callback &optional _errback)
                 (funcall callback
                          '((user_id . "10001") (outcome . "daily_limit")))
                 'request)))
      (qq-user-like))
    (should (qq-user--like-limit-reached-p))
    (should (string-match-p "今日已达上限" (buffer-string)))
    (should-error (qq-user-like) :type 'user-error)))

(ert-deftest qq-user-like-rejects-the-current-account ()
  (with-temp-buffer
    (qq-user-mode)
    (setq qq-user--user-id "10001")
    (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "10001")))
      (should-error (qq-user-like) :type 'user-error))))

(defmacro qq-user-photo-test-with-view (&rest body)
  "Evaluate BODY in a live Appkit photo-wall view.

BODY may refer to the lexical variable `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-user-photo-mode)
           (setq qq-user-photo--user-id "10001")
           (let ((view (qq-user-photo--ensure-view)))
             ,@body))
       (qq-runtime-stop))))

(defun qq-user-photo-test--sync (view)
  "Synchronize pending photo-wall presentation state for VIEW."
  (qq-user-photo--request-sync view)
  (qq-user-photo--sync-now view))

(ert-deftest qq-user-photo-sync-has-no-inline-open-button ()
  (qq-user-photo-test-with-view
    (setq qq-user-photo--photos
          '(((id . "p1")
             (original_url . "https://example.test/p1.jpg")
             (thumbnail_url . "https://example.test/p1-thumb.jpg"))))
    (cl-letf (((symbol-function 'qq-media-url-preview-display-string)
               (lambda (_key _url fallback) fallback)))
      (qq-user-photo-test--sync view))
    (should (string-match-p "Photo 1" (buffer-string)))
    (should-not (string-match-p "\\[Open\\]" (buffer-string)))
    (should-not (string-match-p "g refresh" (buffer-string)))
    (goto-char (point-min))
    (search-forward "Photo 1")
    (should (equal (alist-get 'id (qq-user-photo--photo-at-point)) "p1"))))

(ert-deftest qq-user-photo-refresh-retains-content-and-reuses-node ()
  (qq-user-photo-test-with-view
    (setq qq-user-photo--photos
          '(((id . "p1")
             (original_url . "https://example.test/p1.jpg")
             (thumbnail_url . "https://example.test/old-thumb.jpg"))))
    (cl-letf (((symbol-function 'qq-media-url-preview-display-string)
               (lambda (_key url fallback) (format "%s <%s>" fallback url))))
      (qq-user-photo-test--sync view)
      (let ((key '(photo "10001" "p1"))
            (success nil))
        (let ((node (gethash key qq-user-photo--node-table)))
          (should node)
          (cl-letf (((symbol-function 'qq-api-get-user-photo-wall)
                     (lambda (_user-id callback &optional _errback)
                       (setq success callback)
                       'photo-token)))
            (qq-user-photo-refresh))
          (should qq-user-photo--loading)
          (should (string-match-p "Refreshing photo wall" (buffer-string)))
          (should (string-match-p "old-thumb" (buffer-string)))
          (let ((before (buffer-string))
                (updated
                 '(((id . "p1")
                    (original_url . "https://example.test/p1.jpg")
                    (thumbnail_url . "https://example.test/new-thumb.jpg")))))
            (cl-letf (((symbol-function 'appkit-sync-invalidations)
                       (lambda (&rest _arguments)
                         (ert-fail "photo callback synchronized directly"))))
              (funcall success updated))
            (should (equal before (buffer-string)))
            (should (equal "https://example.test/new-thumb.jpg"
                           (qq-user-photo-preview-url
                            (car qq-user-photo--photos))))
            (cl-letf (((symbol-function 'erase-buffer)
                       (lambda ()
                         (ert-fail "incremental photo sync erased buffer"))))
              (appkit-sync-invalidations view))
            (should (eq node (gethash key qq-user-photo--node-table)))
            (should (string-match-p "new-thumb" (buffer-string)))))))))

(ert-deftest qq-user-photo-media-update-targets-one-stable-row ()
  (qq-user-photo-test-with-view
    (let* ((photo '((id . "p1")
                    (original_url . "https://example.test/p1.jpg")
                    (thumbnail_url . "https://example.test/p1-thumb.jpg")))
           (qq-user-photo--photos (list photo))
           (media-key (qq-user-photo-cache-key "10001" photo))
           call)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest arguments)
                   (setq call (cons candidate arguments)))))
        (qq-user-photo--handle-media-cache-update media-key))
      (should (eq view (car call)))
      (should (equal '(photo "10001" "p1")
                     (plist-get (cdr call) :entry)))
      (should (equal media-key (plist-get (cdr call) :resource)))
      (should-not (plist-get (cdr call) :structure)))))

(ert-deftest qq-user-photo-dead-view-makes-media-update-inert ()
  (qq-user-photo-test-with-view
    (let* ((photo '((id . "p1")
                    (original_url . "https://example.test/p1.jpg")
                    (thumbnail_url . "https://example.test/p1-thumb.jpg")))
           (qq-user-photo--photos (list photo))
           (media-key (qq-user-photo-cache-key "10001" photo)))
      (appkit-kill-view view)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "dead photo view requested a media sync"))))
        (qq-user-photo--handle-media-cache-update media-key)))))

(ert-deftest qq-user-photo-replacement-view-can-restart-cancelled-load ()
  (qq-user-photo-test-with-view
    (let ((calls 0)
          cancelled)
      (cl-letf (((symbol-function 'qq-api-get-user-photo-wall)
                 (lambda (_user-id _callback &optional _errback)
                   (cl-incf calls)
                   (list 'photo-request calls)))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (request) (push request cancelled))))
        (qq-user-photo-refresh)
        (should qq-user-photo--loading)
        (appkit-kill-view view)
        (should-not qq-user-photo--loading)
        (should-not qq-user-photo--request)
        (should-not qq-user-photo--request-owner)
        (should (= 1 (length cancelled)))
        (let ((replacement (qq-user-photo--ensure-view)))
          (should (appkit-view-live-p replacement))
          (qq-user-photo-refresh)
          (should (= 2 calls))
          (should qq-user-photo--loading))))))

(ert-deftest qq-user-photo-replacement-runtime-removes-old-account-photo ()
  (let ((qq-runtime--app nil)
        (calls 0)
        buffer)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-get-user-photo-wall)
                   (lambda (_user-id callback &optional errback)
                     (cl-incf calls)
                     (if (= calls 1)
                         (funcall
                          callback
                          '(((id . "old-photo")
                             (original_url
                              . "https://example.test/OLD_ACCOUNT_SECRET.jpg")
                             (thumbnail_url
                              . "https://example.test/OLD_ACCOUNT_SECRET-thumb.jpg"))))
                       (funcall errback nil "replacement failure"))
                     (intern (format "photo-request-%d" calls))))
                  ((symbol-function 'qq-media-url-preview-display-string)
                   (lambda (_key url _fallback) url))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (candidate &rest _arguments) candidate)))
          (setq buffer (qq-user-photo-open "10001" "Old Account"))
          (with-current-buffer buffer
            (should (string-match-p "OLD_ACCOUNT_SECRET"
                                    (buffer-string)))
            (should qq-user-photo--photos))
          (qq-runtime-stop)
          (let ((replacement
                 (qq-user-photo-open "10001" "Replacement Account")))
            (should (eq replacement buffer))
            (should (= calls 2))
            (with-current-buffer replacement
              (should (appkit-view-live-p (appkit-current-view)))
              (should-not qq-user-photo--photos)
              (should (string-match-p "replacement failure"
                                      qq-user-photo--error))
              (should-not (string-match-p "OLD_ACCOUNT_SECRET"
                                          (buffer-string)))
              (should-not
               (text-property-not-all
                (point-min) (point-max) 'qq-user-photo nil)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop))))

(provide 'qq-user-test)

;;; qq-user-test.el ends here
