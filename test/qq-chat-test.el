;;; qq-chat-test.el --- Tests for qq-chat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-forward)
(require 'qq-state)
(require 'qq-transient)

(defmacro qq-chat-test-with-reset (&rest body)
  "Run BODY with clean qq state and disabled live-update hooks."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(defun qq-chat-test-sync-invalidations ()
  "Synchronously flush the current chat view's queued invalidations."
  (appkit-sync-invalidations (appkit-current-view)))

(ert-deftest qq-chat-header-contains-state-not-a-key-cheat-sheet ()
  (with-temp-buffer
    (qq-chat-mode)
    (let ((header (qq-chat--header-text)))
      (should-not (string-match-p "M-<" header))
      (should-not (string-match-p "C-c" header)))))

(ert-deftest qq-chat-header-line-is-cached-between-redisplays ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (time . 100)
       (raw-message . "marked")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--marked-message-anchors '("9007199254743009336"))
     (let ((original-getter (symbol-function 'qq-state-session-messages))
           (getter-calls 0))
       (cl-letf (((symbol-function 'qq-state-session-messages)
                  (lambda (session-key)
                    (cl-incf getter-calls)
                    (funcall original-getter session-key))))
         (qq-chat--header-line-update)
         (should (= getter-calls 1))
         (should (stringp header-line-format))
         (should (string-match-p "1 marked" header-line-format))
         (dotimes (_ 5)
           (format-mode-line header-line-format))
         (should (= getter-calls 1)))))))

(ert-deftest qq-chat-header-line-prunes-recalled-forward-marks ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (status . recalled)
       (time . 100)
       (raw-message . "[message recalled]")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001"
           qq-chat--marked-message-anchors '("9007199254743009336"))
     (qq-chat--header-line-update)
     (should-not qq-chat--marked-message-anchors)
     (should-not (string-match-p "marked" header-line-format)))))

(ert-deftest qq-chat-header-line-recognizes-ready-connection-state ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-state-set-connection-status 'ready)
     (should-not (string-match-p "\\[ready\\]" (qq-chat--header-line)))
     (qq-state-set-connection-status 'reconnecting)
     (should (string-match-p "\\[reconnecting\\]"
                            (qq-chat--header-line))))))

(ert-deftest qq-chat-connection-state-change-refreshes-header-line ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let ((updates 0))
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (cl-incf updates))))
         (qq-chat--handle-state-change '(:type connection :status ready))
         (qq-chat-test-sync-invalidations)
         (should (= updates 1)))))))

(ert-deftest qq-chat-input-region-uses-editing-keymap ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (save-window-excursion
     (let ((buffer (get-buffer-create " *qq-chat-input*")))
       (unwind-protect
           (progn
             (switch-to-buffer buffer)
             (qq-chat-mode)
             (should-not (derived-mode-p 'special-mode))
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (qq-chat-edit-draft)
             (qq-chat--update-context-mode)
             (should (appkit-chatbuf-point-in-input-p))
             (should (eq (key-binding (kbd "q") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "s") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "RET") t) 'qq-chat-return-dwim))
             (should (eq (key-binding (kbd "DEL") t)
                         'appkit-chatbuf-input-backward-delete))
             (should (eq (key-binding (kbd "C-d") t)
                         'appkit-chatbuf-input-forward-delete))
             (execute-kbd-macro "qs")
             (should (equal (qq-chat--current-draft-string) "qs"))
             (goto-char (point-min))
             (qq-chat--update-context-mode)
             (should qq-chat-timeline-mode)
             (should (eq (key-binding (kbd "q") t) 'quit-window))
             (should (eq (key-binding (kbd "r") t) 'qq-chat-reply-to-message))
             (should (eq (key-binding (kbd "d") t) 'qq-chat-delete-message))
             (should (eq (key-binding (kbd "f") t) 'qq-chat-forward-message))
             (should (eq (key-binding (kbd "M") t) 'qq-chat-toggle-forward-mark))
             (should (eq (key-binding (kbd "F") t)
                         'qq-chat-forward-marked-messages))
             (should (eq (key-binding (kbd "!") t)
                         'qq-chat-react-to-message))
             (should (eq (key-binding (kbd "P") t)
                         'qq-chat-poke-sender))
             (should (eq (key-binding (kbd "m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c /") t) 'qq-chat-search))
             (should (eq (key-binding (kbd "C-c m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "C-c ?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c P") t)
                         'qq-chat-send-poke))
             (should (eq (key-binding (kbd "C-c C-a") t) 'qq-chat-attach-transient))
             (should (eq (key-binding (kbd "C-c C-v") t) 'qq-chat-attach-clipboard)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-chat-deleted-tail-does-not-return-after-frame-refresh ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "abc")
     (delete-backward-char 1)
     (should (equal "ab" (qq-chat--current-draft-string)))
     (qq-chat--update-frame)
     (should (equal "ab" (appkit-chatbuf-input-string)))
     (should (equal "ab" (qq-chat--current-draft-string))))))

(ert-deftest qq-chat-return-completes-unresolved-token-without-sending ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (dolist (token '("@green" "/斜眼" "/fav" ":rocket:"))
       (qq-chat--set-draft token)
       (qq-chat-edit-draft)
       (let (completed sent)
         (cl-letf (((symbol-function 'qq-completion-complete)
                    (lambda () (setq completed t) nil))
                   ((symbol-function 'qq-api-send-message)
                    (lambda (&rest _args) (setq sent t))))
           (qq-chat-return-dwim nil))
         (should completed)
         (should-not sent)
         (should (equal token (appkit-chatbuf-input-string))))))))

(ert-deftest qq-chat-prefixed-return-inserts-newline-without-completion-or-send ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "@green")
     (cl-letf (((symbol-function 'qq-completion-complete)
                (lambda () (ert-fail "prefix RET must not complete")))
               ((symbol-function 'qq-api-send-message)
                (lambda (&rest _args) (ert-fail "prefix RET must not send"))))
       (qq-chat-return-dwim '(4)))
     (should (equal "@green\n" (appkit-chatbuf-input-string))))))

(ert-deftest qq-chat-return-sends-non-completion-path-and-colon-text ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'qq-completion--base-face-candidates)
                (lambda () nil))
               ((symbol-function 'appkit-chat-emoji-candidates)
                (lambda (&optional _force) nil)))
       (dolist (text '("/tmp/foo" "https://example.com" ":unknown" ":)"))
         (qq-chat--set-draft text)
         (qq-chat-edit-draft)
         (let (sent)
           (cl-letf (((symbol-function 'qq-completion-complete)
                      (lambda ()
                        (ert-fail "ordinary text must not enter completion")))
                     ((symbol-function 'qq-api-send-message)
                      (lambda (_session segments &rest _args)
                        (setq sent segments))))
             (qq-chat-return-dwim nil))
           (should sent)))))))

(ert-deftest qq-chat-service-session-has-no-composer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "service:u_mail"
    '((title . "QQ邮箱提醒")
      (type . service)
      (target-id . "u_mail")
      (chat-type . "103"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "service:u_mail")
     (qq-chat-render)
     (should-not (appkit-chatbuf-input-region-bounds))
     (should-not (string-match-p ">>> " (buffer-string)))
     (should-error (qq-chat-edit-draft) :type 'user-error)
     (should-error (qq-chat-send-message) :type 'user-error))))

(ert-deftest qq-chat-set-draft-preserves-shared-timeline-and-composer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (qq-chat-render)
     (let ((ewoc (appkit-chat-timeline-ewoc))
           (input-start (appkit-chatbuf-input-start-position))
           (prompt-start (appkit-chatbuf-prompt-start-position)))
       (qq-chat--set-draft "updated body")
       (should (eq ewoc (appkit-chat-timeline-ewoc)))
       (should (= input-start (appkit-chatbuf-input-start-position)))
       (should (= prompt-start (appkit-chatbuf-prompt-start-position)))
       (should (equal "updated body" (appkit-chatbuf-input-state)))
       (should (equal "updated body" (qq-chat--current-draft-string)))))))

(ert-deftest qq-chat-mode-and-render-do-not-duplicate-prompt ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-render)
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((count 0)
           (pos (point-min)))
       (while (< pos (point-max))
         (when (and (eq (get-text-property pos 'field) 'appkit-chatbuf-prompt)
                    (not (eq (get-text-property (max (point-min) (1- pos)) 'field)
                             'appkit-chatbuf-prompt)))
           (setq count (1+ count)))
         (setq pos (1+ pos)))
       (should (= 1 count))))))

(ert-deftest qq-chat-render-preserves-footer-position ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat--set-reply-message
      '((server-id . "42")
        (sender-name . "Alice")
        (raw-message . "[CQ:image,file=x.png,url=http://example.com/x]")
        (preview . "[image]")
        (segments . (((type . "image")
                      (data . ((file . "x.png")
                               (url . "http://example.com/x"))))))))
     (qq-chat--update-frame)
     (goto-char (point-min))
     (search-forward "Reply to Alice")
     (search-forward "[image]")
     (goto-char (point-min))
     (should-not (search-forward "[CQ:" nil t))
     (goto-char (point-min))
     (search-forward "Reply to Alice")
     (beginning-of-line)
     (let ((before (point)))
       (qq-chat-render)
       (should (= before (point)))
       (should-not (appkit-chatbuf-point-in-input-p)))
     ;; Cancel reply (C-c C-k / footer ×).
     (should (qq-chat--reply-message))
     (qq-chat-cancel-dwim)
     (should-not (qq-chat--reply-message))
     (goto-char (point-min))
     (should-not (search-forward "Reply to Alice" nil t)))))

(ert-deftest qq-chat-render-falls-back-to-sender-id-when-sender-name-empty ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Test Group")
      (target-id . "20001")
      (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "10001" nil t)))))

(ert-deftest qq-chat-render-shows-group-card-and-nickname-like-telega ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Test Group")
      (target-id . "20001")
      (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "m1")
       (session-key . "group:20001")
       (message-type . "group")
       (sender-id . "10001")
       (sender-name . "Alice Card")
       (sender-secondary-name . "Alice Nick")
       (sender-card . "Alice Card")
       (sender-nickname . "Alice Nick")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "Alice Card • Alice Nick" nil t)))))

(ert-deftest qq-chat-render-uses-friend-remark-with-nickname-trail ()
  (qq-chat-test-with-reset
   (qq-state-apply-friends
    '(((user_id . 10001)
       (remark . "Alice Remark")
       (nickname . "Alice Nick"))))
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice Remark")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (session-key . "private:10001")
       (message-type . "private")
       (sender-id . "10001")
       (sender-name . "Alice Nick")
       (sender-secondary-name . nil)
       (sender-nickname . "Alice Nick")
       (time . 100)
       (raw-message . "hello")
       (raw-event . ((sender . ((user_id . 10001)
                                (nickname . "Alice Nick")))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "Alice Remark • Alice Nick" nil t)))))

(ert-deftest qq-chat-render-reuses-existing-message-nodes ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((ewoc (appkit-chat-timeline-ewoc))
           (node-m1 (appkit-chat-timeline-node "m1"))
           (node-m2 (appkit-chat-timeline-node "m2")))
       (puthash
        "private:10001"
        '(((server-id . "m1")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 100)
           (raw-message . "first"))
          ((server-id . "m2")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 200)
           (raw-message . "second updated"))
          ((server-id . "m3")
           (sender-id . "10001")
           (sender-name . "Alice")
           (time . 300)
           (raw-message . "third")))
        qq-state--messages-by-session)
       (qq-chat-render)
       (should (eq ewoc (appkit-chat-timeline-ewoc)))
       (should (eq node-m1 (appkit-chat-timeline-node "m1")))
       (should (eq node-m2 (appkit-chat-timeline-node "m2")))
       (should (appkit-chat-timeline-node "m3"))
       (should (equal '("m1" "m2" "m3")
                      (appkit-chat-timeline-keys)))
       (should (string-match-p "second updated" (buffer-string)))))))

(ert-deftest qq-chat-render-preserves-empty-active-region-after-set-mark ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "second")
       (beginning-of-line)
       (push-mark (point) t t)
       (let ((before (point)))
         (qq-chat-render)
         (should mark-active)
         (should (= before (point)))
         (should (= before (mark t))))))))

(ert-deftest qq-chat-node-invalidation-preserves-empty-active-region-after-set-mark ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "second")
       (beginning-of-line)
       (push-mark (point) t t)
       (let ((before (point)))
         (appkit-chat-timeline-invalidate (appkit-chat-timeline-keys))
         (should mark-active)
         (should (= before (point)))
         (should (= before (mark t))))))))

(ert-deftest qq-chat-handle-state-change-ignores-unrelated-events ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (events)
       (cl-letf (((symbol-function 'qq-chat-render)
                  (lambda () (push 'render events)))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest _) (push 'timeline events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events)))
                 ((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header-line events))))
         (qq-chat--handle-state-change '(:type heartbeat :timestamp 1.0))
         (should-not events))))))

(ert-deftest qq-chat-message-state-change-uses-one-projected-sync-path ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (sync-args render-called)
       (cl-letf (((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args) (setq sync-args args)))
                 ((symbol-function 'qq-chat-render)
                  (lambda () (setq render-called t))))
         (qq-chat--handle-state-change
          '(:type message
            :session-key "private:10001"
            :mutation create
            :message-anchor "9007199254741004645"
            :message ((server-id . "9007199254741004645"))))
         (qq-chat-test-sync-invalidations)
         (should (equal (plist-get sync-args :changed-resources)
                        '((:message "9007199254741004645"))))
         (should-not render-called))))))

(ert-deftest qq-chat-session-state-change-updates-shared-frame ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (events)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events))))
         (qq-chat--handle-state-change
          '(:type session :session-key "private:10001" :mutation session))
         (qq-chat-test-sync-invalidations)
         (should (equal events '(frame header))))))))

(ert-deftest qq-chat-read-state-change-uses-projected-context-sync ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (called)
       (cl-letf (((symbol-function 'qq-chat--apply-read-state-change)
                  (lambda () (setq called t))))
         (qq-chat--handle-state-change
          '(:type session :session-key "private:10001" :mutation read))
         (qq-chat-test-sync-invalidations)
         (should called))))))

(ert-deftest qq-chat-recall-hides-node-by-default ()
  "Default: recalled stays in state but leaves the timeline."
  (qq-chat-test-with-reset
   (let ((qq-chat-show-recalled-messages nil))
     (qq-state-set-self-info '((user_id . 90001)
                               (nickname . "Me")))
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice")
        (target-id . "10001"))
      nil)
     (qq-state-merge-live-message
      '((post_type . "message_sent")
        (message_type . "private")
        (chat_type . 1)
        (message_id . "9007199254741007777")
        (user_id . "90001")
        (target_id . "10001")
        (time . 1710000001)
        (sender . ((user_id . 90001)
                   (nickname . "Me")))
        (raw_message . "bye")
        (message . (((type . "text")
                     (data . ((text . "bye"))))))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (should (equal '("9007199254741007777")
                      (appkit-chat-timeline-keys)))
       (qq-state-apply-recall "9007199254741007777")
       (qq-chat--handle-state-change
        (list :type 'message
              :session-key "private:10001"
              :mutation 'update
              :message-anchor "9007199254741007777"
              :message (car (qq-state-session-messages "private:10001"))))
       (qq-chat-test-sync-invalidations)
       (should (equal (appkit-chat-timeline-keys)
                      (list qq-chat--empty-placeholder)))
       (should (qq-state-message-recalled-p
                (car (qq-state-session-messages "private:10001"))))))))

(ert-deftest qq-chat-show-recalled-messages-keeps-stub ()
  (qq-chat-test-with-reset
   (let ((qq-chat-show-recalled-messages t))
     (qq-state-set-self-info '((user_id . 90001)
                               (nickname . "Me")))
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice")
        (target-id . "10001"))
      nil)
     (qq-state-merge-live-message
      '((post_type . "message_sent")
        (message_type . "private")
        (chat_type . 1)
        (message_id . "9007199254741008888")
        (user_id . "90001")
        (target_id . "10001")
        (time . 1710000001)
        (sender . ((user_id . 90001)
                   (nickname . "Me")))
        (raw_message . "bye")
        (message . (((type . "text")
                     (data . ((text . "bye"))))))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (qq-state-apply-recall "9007199254741008888")
       (qq-chat--handle-state-change
        (list :type 'message
              :session-key "private:10001"
              :mutation 'update
              :message-anchor "9007199254741008888"
              :message (car (qq-state-session-messages "private:10001"))))
       (qq-chat-test-sync-invalidations)
       (should (equal '("9007199254741008888")
                      (appkit-chat-timeline-keys)))
       (should (string-match-p
                "recalled"
                (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest qq-chat-handle-state-change-friends-refreshes-timeline-for-name-updates ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (events)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header events)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (push 'frame events)))
                 ((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args) (push (cons 'timeline args) events))))
         (qq-chat--handle-state-change '(:type friends-refreshed :count 1))
         (qq-chat-test-sync-invalidations)
         (should (equal (mapcar (lambda (event)
                                  (if (consp event) (car event) event))
                                events)
                        '(timeline frame header))))))))

(ert-deftest qq-chat-projected-sync-replaces-empty-timeline-placeholder ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (should (equal (appkit-chat-timeline-keys)
                    (list qq-chat--empty-placeholder)))
     (goto-char (point-min))
     (should (search-forward "No messages loaded yet." nil t))
     (let ((messages '(((server-id . "m1")
                        (sender-id . "10001")
                        (sender-name . "Alice")
                        (time . 100)
                        (raw-message . "first")))))
       (puthash "private:10001" messages qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (should (equal (appkit-chat-timeline-keys) '("m1")))
       (goto-char (point-min))
       (should (search-forward "first" nil t))
       (goto-char (point-min))
       (should-not (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-source-update-redisplays-reply-dependent-and-composer ()
  "Updating a source message refreshes its reply rows and active aux."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((source-old
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "old body"))))))))
          (source-new
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "new body"))))))))
          (reply
           '((server-id . "m2")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "m1"))))
                          ((type . "text")
                           (data . ((text . "answer"))))))))
          (initial (list source-old reply))
          (updated (list source-new reply)))
     (puthash "private:10001" initial qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (qq-chat--set-reply-message source-old)
       (qq-chat--set-draft "draft stays")
       (qq-chat--update-frame)
       (puthash "private:10001" updated qq-state--messages-by-session)
       (qq-chat--apply-message-state-change
        (list :type 'message
              :session-key "private:10001"
              :message-anchor "m1"
              :message source-new))
       (should (equal "new body"
                      (qq-state-message-preview (qq-chat--reply-message))))
       (should (equal "draft stays" (qq-chat--current-draft-string)))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: new body" nil t))))))

(ert-deftest qq-chat-source-recall-redisplays-reply-dependent ()
  "Hidden recalled source still refreshes the row that quotes it."
  (qq-chat-test-with-reset
   (let ((qq-chat-show-recalled-messages nil)
         (source
          '((server-id . "m1")
            (sender-id . "10001")
            (sender-name . "Alice")
            (time . 100)
            (segments . (((type . "text")
                          (data . ((text . "old body"))))))))
         (reply
          '((server-id . "m2")
            (sender-id . "10002")
            (sender-name . "Bob")
            (time . 200)
            (segments . (((type . "reply")
                          (data . ((id . "m1"))))
                         ((type . "text")
                          (data . ((text . "answer")))))))))
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice")
        (target-id . "10001"))
      nil)
     (puthash "private:10001" (list source reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (let ((recalled (qq-state--as-recalled-message source)))
         (puthash "private:10001" (list recalled reply)
                  qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message :message-anchor "m1" :message recalled)))
       (should-not (appkit-chat-timeline-node "m1"))
       (should (appkit-chat-timeline-node "m2"))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: [message recalled]" nil t))))))

(ert-deftest qq-chat-history-source-arrival-redisplays-reply-dependent ()
  "History source arrival resolves an already rendered reply preview."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((source
           '((server-id . "m1")
             (sender-id . "10001")
             (sender-name . "Alice")
             (time . 100)
             (segments . (((type . "text")
                           (data . ((text . "source arrived"))))))))
          (reply
           '((server-id . "m2")
             (sender-id . "10002")
             (sender-name . "Bob")
             (time . 200)
             (segments . (((type . "reply")
                           (data . ((id . "m1"))))
                          ((type . "text")
                           (data . ((text . "answer")))))))))
     (puthash "private:10001" (list reply) qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (goto-char (point-min))
       (should (search-forward "↪ id m1" nil t))
       (puthash "private:10001" (list source reply) qq-state--messages-by-session)
       (qq-chat--sync-timeline :changed-resources '((:message "m1")))
       (goto-char (point-min))
       (should (search-forward "↪ Alice: source arrived" nil t))))))

(ert-deftest qq-chat-pending-promote-rekeys-anchor-to-snowflake ()
  "Optimistic local-id row is rekeyed to NT snowflake after send."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let* ((local-id "local-9")
            (snowflake "9007199254741004645")
            (pending `(((local-id . ,local-id)
                        (self-p . t)
                        (status . pending)
                        (sender-id . "90001")
                        (sender-name . "Me")
                        (time . 100)
                        (raw-message . "hi")
                        (preview . "hi"))))
            (sent `(((local-id . ,local-id)
                     (server-id . ,snowflake)
                     (id . ,snowflake)
                     (self-p . t)
                     (status . sent)
                     (sender-id . "90001")
                     (sender-name . "Me")
                     (time . 100)
                     (raw-message . "hi")
                     (preview . "hi")))))
       (puthash "private:10001" pending qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (let ((node (appkit-chat-timeline-node local-id)))
         (should node)
         (should (equal (appkit-chat-timeline-keys) (list local-id)))
         (setq qq-chat--marked-message-anchors (list local-id))
         (puthash "private:10001" sent qq-state--messages-by-session)
         (qq-chat--apply-message-state-change
          (list :type 'message
                :message-anchor snowflake
                :previous-anchor local-id
                :message (car sent)))
         (should-not (appkit-chat-timeline-node local-id))
         (should (eq node (appkit-chat-timeline-node snowflake)))
         (should (equal qq-chat--marked-message-anchors (list snowflake))))
       (should (equal (appkit-chat-timeline-keys) (list snowflake)))
       (goto-char (point-min))
       (should (search-forward "hi" nil t))))))

(ert-deftest qq-chat-projected-sync-restores-empty-timeline-placeholder ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((messages '(((server-id . "m1")
                      (sender-id . "10001")
                      (sender-name . "Alice")
                      (time . 100)
                      (raw-message . "first")))))
     (puthash "private:10001" messages qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (should (equal (appkit-chat-timeline-keys) '("m1")))
       (puthash "private:10001" nil qq-state--messages-by-session)
       (qq-chat--sync-timeline)
       (should (equal (appkit-chat-timeline-keys)
                      (list qq-chat--empty-placeholder)))
       (goto-char (point-min))
       (should (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-media-cache-update-requests-node-refresh ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (puthash "private:10001"
            '(((server-id . "m1") (sender-id . "10001")
               (time . 1) (raw-message . "hello")))
            qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (requested)
       (cl-letf (((symbol-function 'appkit-invalidate)
                  (lambda (_view &rest args)
                    (setq requested (plist-get args :entries))))
                 ((symbol-function 'appkit-schedule-sync)
                  (lambda (&rest _) nil)))
         (qq-chat--rerender-open-chats)
         (should (equal '("m1") requested)))))))

(ert-deftest qq-chat-compact-face-message-uses-image-display ()
  "Same-sender face continuations must not render plain [face:id] text."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((dir (make-temp-file "qq-default-emojis" t))
          (png (expand-file-name "178.png" dir))
          (qq-media-default-emoji-directory dir)
          (qq-media--face-names-table (make-hash-table :test #'equal)))
     (unwind-protect
         (progn
           (puthash "178" "/斜眼笑" qq-media--face-names-table)
           (with-temp-file png
             (set-buffer-multibyte nil)
             (insert (unibyte-string
                      #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
                      #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
                      #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                      #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                      #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41
                      #x54 #x08 #xd7 #x63 #xf8 #xcf #xc0 #x00
                      #x00 #x00 #x03 #x00 #x01 #x00 #x05 #xfe
                      #xd4 #xef #x00 #x00 #x00 #x00 #x49 #x45
                      #x4e #x44 #xae #x42 #x60 #x82)))
           (puthash
            "private:10001"
            '(((server-id . "m1")
               (sender-id . "10001")
               (sender-name . "Alice")
               (time . 100)
               (raw-message . "hello")
               (segments . (((type . "text")
                             (data . ((text . "hello")))))))
              ((server-id . "m2")
               (sender-id . "10001")
               (sender-name . "Alice")
               (time . 200)
               (raw-message . "[CQ:face,id=178]")
               (preview . "[face:178]")
               (segments . (((type . "face")
                             (data . ((id . "178"))))))))
            qq-state--messages-by-session)
           (with-temp-buffer
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (goto-char (point-min))
             (should (search-forward "hello" nil t))
             (let ((found nil)
                   (pos (point-min)))
               (while (and (< pos (point-max)) (not found))
                 (when-let* ((disp (get-text-property pos 'display)))
                   (when (and (consp disp) (eq (car disp) 'image))
                     (setq found t)))
                 (setq pos (1+ pos)))
               (should found))
             ;; Must not leave a bare placeholder without display as the only face token.
             (goto-char (point-min))
             (when (search-forward "[face:178]" nil t)
               (should (get-text-property (match-beginning 0) 'display)))))
       (when (file-directory-p dir)
         (delete-directory dir t))))))

(ert-deftest qq-chat-native-priority-mentions-use-attention-face ()
  (cl-letf (((symbol-function 'qq-state-self-user-id) (lambda () "90001")))
    (let ((self (qq-chat--segment-inline-string
                 '((type . "at") (data . ((qq . "90001") (name . "Me"))))))
          (all (qq-chat--segment-inline-string
                '((type . "at") (data . ((qq . "all"))))))
          (other (qq-chat--segment-inline-string
                  '((type . "at") (data . ((qq . "10002") (name . "Bob")))))))
      (should (eq 'at-me (get-text-property 0 'qq-chat-mention-kind self)))
      (should (eq 'at-all (get-text-property 0 'qq-chat-mention-kind all)))
      (should (eq 'ordinary (get-text-property 0 'qq-chat-mention-kind other)))
      (should (eq 'qq-msg-mention-self (get-text-property 0 'face self)))
      (should (eq 'qq-msg-mention-self (get-text-property 0 'face all)))
      (should (eq 'qq-msg-mention (get-text-property 0 'face other))))))

(ert-deftest qq-chat-renders-clickable-reaction-chips ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254741004001")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (segments . (((type . "text") (data . ((text . "hello"))))))
       (reactions . (((emoji-id . "178")
                      (emoji-type . "1")
                      (count . 3)
                      (chosen-p . t))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (cl-letf (((symbol-function 'qq-media-face-display-string)
                (lambda (_emoji-id) "/斜眼笑")))
       (qq-chat-render))
     (goto-char (point-min))
     (should (search-forward "/斜眼笑 3" nil t))
     (let ((button (button-at (1- (point)))))
       (should button)
       (should (eq (button-get button 'face) 'qq-msg-reaction-chosen))))))

(ert-deftest qq-chat-reaction-chip-toggles-current-selection ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254741004001")
       (reactions . (((emoji-id . "178")
                      (emoji-type . "1")
                      (count . 1)
                      (chosen-p . t))))))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (called)
       (cl-letf (((symbol-function 'qq-api-set-message-emoji-like)
                  (lambda (message-id emoji-id set &rest _)
                    (setq called (list message-id emoji-id set)))))
         (qq-chat-toggle-message-reaction
          "9007199254741004001" "178"))
       (should (equal called
                      '("9007199254741004001" "178" nil)))))))

(ert-deftest qq-chat-react-command-adds-picked-face ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (let ((message '((server-id . "9007199254741004001"))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (let (called)
         (cl-letf (((symbol-function 'qq-api-set-message-emoji-like)
                    (lambda (message-id emoji-id set &rest _)
                      (setq called (list message-id emoji-id set)))))
           (qq-chat-react-to-message "178" message))
         (should (equal called
                        '("9007199254741004001" "178" t))))))))

(ert-deftest qq-chat-poke-sender-uses-sender-not-message-target ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((at-point '((sender-id . "10002")
                       (target-id . "20001")
                       (self-p . t)))
           call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () at-point))
                 ((symbol-function 'qq-api-send-poke)
                  (lambda (session-key target-id &optional callback _errback)
                    (setq call (list session-key target-id))
                    (when callback (funcall callback nil)))))
         (qq-chat-poke-sender))
       (should (equal call '("group:20001" "10002")))))))

(ert-deftest qq-chat-poke-sender-can-target-self-in-private-chat ()
  (qq-chat-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10002"
    '((type . private) (title . "Peer")
      (target-id . "10002") (peer-uin . "10002"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10002")
     (let (call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () '((sender-id . "90001") (self-p . t))))
                 ((symbol-function 'qq-api-send-poke)
                  (lambda (session-key target-id &optional _callback _errback)
                    (setq call (list session-key target-id)))))
         (qq-chat-poke-sender))
       (should (equal call '("private:10002" "90001")))))))

(ert-deftest qq-chat-recalls-pokes-with-their-closed-native-reference ()
  (dolist (reference
           '(((message_id . "9007199254741004001")
              (peer . ((chat_type . 2)
                       (peer_uid . "20001")
                       (guild_id . "")))
              (valid_before . 4102444800))
             ((message_id . "9007199254741004002")
              (peer . ((chat_type . 1)
                       (peer_uid . "u_private-native-peer")
                       (guild_id . "")))
              (valid_before . 4102444800))))
    (let ((message
           `((server-id . ,(alist-get 'message_id reference))
             (self-p . t)
             (poke-recall-reference . ,reference)
             (segments . (((type . "poke"))))))
          recalled-reference
          ordinary-delete-called)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'qq-api-recall-poke)
                 (lambda (value &rest _)
                   (setq recalled-reference value)))
                ((symbol-function 'qq-api-delete-message)
                 (lambda (&rest _)
                   (setq ordinary-delete-called t))))
        (qq-chat--delete-message-internal message))
      (should (equal recalled-reference reference))
      (should-not ordinary-delete-called))))

(ert-deftest qq-chat-refuses-an-unaddressable-poke-before-confirmation ()
  (let ((message
         '((server-id . "9007199254741004001")
           (self-p . t)
           (segments . (((type . "poke"))))))
        prompted
        api-called)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq prompted t)))
              ((symbol-function 'qq-api-recall-poke)
               (lambda (&rest _)
                 (setq api-called t))))
      (should-error (qq-chat--delete-message-internal message)
                    :type 'user-error))
    (should-not prompted)
    (should-not api-called)))

(ert-deftest qq-chat-refuses-an-expired-poke-before-confirmation ()
  (let ((message
         '((server-id . "9007199254741004001")
           (self-p . t)
           (poke-recall-reference
            . ((message_id . "9007199254741004001")
               (peer . ((chat_type . 2)
                        (peer_uid . "20001")
                        (guild_id . "")))
               (valid_before . 200)))
           (segments . (((type . "poke"))))))
        prompted
        api-called
        failure)
    (cl-letf (((symbol-function 'float-time)
               (lambda (&optional _time) 200))
              ((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq prompted t)))
              ((symbol-function 'qq-api-recall-poke)
               (lambda (&rest _)
                 (setq api-called t))))
      (setq failure
            (should-error (qq-chat--delete-message-internal message)
                          :type 'user-error)))
    (should (string-match-p
             "qq: 戳一戳已超过 2 分钟撤回期限"
             (error-message-string failure)))
    (should-not prompted)
    (should-not api-called)))

(ert-deftest qq-chat-send-poke-uses-explicit-member-chooser ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (chooser-call api-call)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda () '((sender-id . "10002")
                               (target-id . "20001"))))
                 ((symbol-function 'qq-completion-read-poke-target)
                  (lambda (session-key callback &optional initial-user-id)
                    (setq chooser-call (list session-key initial-user-id))
                    (funcall callback "10003")))
                 ((symbol-function 'qq-api-send-poke)
                  (lambda (session-key target-id &optional _callback _errback)
                    (setq api-call (list session-key target-id)))))
         (qq-chat-send-poke))
       (should (equal chooser-call '("group:20001" "10002")))
       (should (equal api-call '("group:20001" "10003")))))))

(ert-deftest qq-chat-poke-rejects-service-and-invalid-senders ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "service:u_mail"
    '((type . service) (title . "Mail") (target-id . "u_mail"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "service:u_mail")
     (should-error (qq-chat-send-poke "10002") :type 'user-error))
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (cl-letf (((symbol-function 'qq-chat--message-at-point)
                (lambda () '((sender-id . "0")))))
       (should-error (qq-chat-poke-sender) :type 'user-error)))))

(ert-deftest qq-chat-private-poke-rejects-third-party-target-before-api ()
  (qq-chat-test-with-reset
   (qq-state-set-self-info '((user_id . "90001") (nickname . "Me")))
   (qq-state-upsert-session
    "private:10002"
    '((type . private) (title . "Peer") (target-id . "10002"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10002")
     (let ((api-called nil))
       (cl-letf (((symbol-function 'qq-api-send-poke)
                  (lambda (&rest _args) (setq api-called t))))
         (should-error (qq-chat-send-poke "77777") :type 'user-error))
       (should-not api-called)))))

(ert-deftest qq-chat-media-cache-update-targets-affected-message-anchors ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (segments . (((type . "face")
                     (data . ((id . "88")))))))
      ((server-id . "m2")
       (sender-id . "10002")
       (sender-name . "Bob")
       (time . 200)
       (raw-message . "plain")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (requested)
       (cl-letf (((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args)
                    (setq requested
                          (plist-get args :changed-resources)))))
         (qq-chat--rerender-open-chats "face:88")
         (qq-chat-test-sync-invalidations)
         (should (equal '((:media "face:88")) requested)))))))

(ert-deftest qq-chat-node-refresh-defers-while-mark-active ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first")))
    qq-state--messages-by-session)
   (let ((transient-mark-mode t))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (goto-char (point-min))
       (search-forward "first")
       (beginning-of-line)
       (push-mark (point) t t)
       (let (invalidated)
         (cl-letf (((symbol-function 'appkit-ewoc-invalidate-key)
                    (lambda (_ewoc _table key) (push key invalidated))))
           (qq-chat--request-row-redisplay '("m1"))
           (should-not invalidated)
           (setq mark-active nil)
           (appkit-chat-timeline-flush-deferred)
           (should (equal invalidated '("m1")))))))))

(ert-deftest qq-chat-row-refresh-uses-shared-invalidation-api ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "first"))
      ((server-id . "m2")
       (sender-id . "10002")
       (sender-name . "Bob")
       (time . 200)
       (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (call)
       (cl-letf (((symbol-function 'appkit-chat-timeline-invalidate)
                  (lambda (keys &rest options)
                    (setq call (cons keys options)))))
         (qq-chat--request-row-redisplay '("m1" "m2"))
         (should (equal (car call) '("m1" "m2")))
         (should (eq (plist-get (cdr call) :defer-while-mark-active) t)))))))

(ert-deftest qq-chat-first-unread-anchor-does-not-guess-from-count ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (unread-count . 2))
    nil)
   (let ((messages
          '(((server-id . "m1") (self-p . nil) (time . 1))
            ((server-id . "m2") (self-p . nil) (time . 2))
            ((server-id . "m3") (self-p . nil) (time . 3))
            ((server-id . "m-self") (self-p . t) (time . 4)))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (let ((qq-chat-show-unread-divider t))
         (should-not (qq-chat--first-unread-anchor messages)))))))

(ert-deftest qq-chat-first-unread-anchor-prefers-kernel-position ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (unread-count . 2)
      (first-unread-message-id . "m3"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((qq-chat-show-unread-divider t))
       (should (equal "m3"
                      (qq-chat--first-unread-anchor
                       '(((server-id . "m1") (self-p . nil) (time . 1))
                         ((server-id . "m2") (self-p . nil) (time . 2))
                         ((server-id . "m3") (self-p . nil) (time . 3))))))))))

(ert-deftest qq-chat-render-inserts-unread-divider ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (unread-count . 1)
      (first-unread-message-id . "m2"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (self-p . nil)
       (raw-message . "older"))
      ((server-id . "m2")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 200)
       (self-p . nil)
       (raw-message . "newest")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let ((qq-chat-show-unread-divider t))
       (qq-chat-render)
       (goto-char (point-min))
       (should (search-forward "Unread Messages" nil t))
       (let ((ctx (appkit-chat-timeline-context "m2")))
         (should (plist-get ctx :insert-unread)))
       (qq-state-clear-session-unread "private:10001")
       (qq-chat--apply-read-state-change)
       (should-not (plist-get (appkit-chat-timeline-context "m2")
                              :insert-unread))
       (goto-char (point-min))
       (should-not (search-forward "Unread Messages" nil t))))))

(defconst qq-chat-test--1x1-png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==")
  "Minimal 1x1 PNG bytes for clipboard attach tests.")

(ert-deftest qq-chat-attach-clipboard-from-image-png ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'gui-get-selection)
                (lambda (_selection &optional type)
                  (pcase type
                    ('TARGETS ["TARGETS" "image/png" "TIMESTAMP"])
                    ('image/png qq-chat-test--1x1-png)
                    (_ nil)))))
       (qq-chat-attach-clipboard)
       (should (appkit-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "image" (alist-get 'type (car segments))))
         (should (file-readable-p
                  (alist-get 'file (alist-get 'data (car segments))))))))))

(ert-deftest qq-chat-image-object-label-uses-one-line-preview-and-file-metadata ()
  (let ((path (make-temp-file "qq-composer-preview" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "123456"))
          (cl-letf (((symbol-function 'qq-media-composer-image-preview)
                     (lambda (file)
                       (should (equal file path))
                       '(:composer-preview)))
                    ((symbol-function 'qq-media--image-display-string)
                     (lambda (image fallback)
                       (propertize fallback 'display image))))
            (let ((label
                   (qq-chat--segment-object-label
                    `((type . "image")
                      (data . ((file . ,path) (name . "preview.png")))))))
              (should (string-match-p "\\[image\\]" label))
              (should (string-match-p "preview.png" label))
              (should (string-match-p "(6)" label))
              (should (equal '(:composer-preview)
                             (get-text-property
                              (string-match "▧" label) 'display label))))))
      (ignore-errors (delete-file path)))))

(ert-deftest qq-chat-equal-adjacent-input-objects-send-as-two-segments ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt "> ")
    (let* ((segment '((type . "at")
                      (data . ((qq . "10001") (name . "Alice Card")))))
           (object (qq-chat--segment-input-object segment))
           (label (plist-get object :label)))
      (insert (appkit-chatbuf-input-object-string label object))
      (insert (appkit-chatbuf-input-object-string label object))
      (should (equal (list segment segment)
                     (qq-chat--current-input-segments))))))

(ert-deftest qq-chat-segment-insertion-inside-object-keeps-both-atomic ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((first '((type . "at")
                    (data . ((qq . "10001") (name . "Alice Card")))))
           (second '((type . "face") (data . ((id . "178"))))))
       (qq-chat--insert-input-segment-object first)
       (goto-char (1+ (appkit-chatbuf-input-start-position)))
       (qq-chat--insert-input-segment-object second)
       (appkit-chatbuf-input-prune-broken-objects)
       (should (equal (list first second)
                      (qq-chat--current-input-segments)))))))

(ert-deftest qq-chat-attach-face-inserts-face-segment ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
     (puthash "178" "/斜眼笑" qq-media--face-names-table)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat-render)
       (qq-chat-attach-face "178")
       (should (appkit-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "face" (alist-get 'type (car segments))))
         (should (equal "178"
                        (alist-get 'id (alist-get 'data (car segments))))))))))

(ert-deftest qq-chat-attach-custom-face-inserts-image-sticker-segment ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let* ((file (make-temp-file "qq-fav-chat" nil ".jpg"))
          (face `((url . "https://example.com/x")
                  (file . ,file)
                  (thumb_file . ,file)
                  (desc . "lol")
                  (md5 . "DEADBEEF")
                  (is_mark_face . :false)
                  (e_id . "")
                  (ep_id . "0")))
          (qq-media--custom-faces (list face))
          (qq-media--custom-faces-fetched-at (float-time)))
     (unwind-protect
         (progn
           (with-temp-file file (insert "jpg"))
           (with-temp-buffer
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (cl-letf (((symbol-function 'completing-read)
                        (lambda (&rest _)
                          (qq-media-custom-face-label face 0))))
               (qq-chat-attach-custom-face)
               (let ((segments (qq-chat--current-input-segments)))
                 (should (= 1 (length segments)))
                 (should (equal "image" (alist-get 'type (car segments))))
                 (should (equal 1 (alist-get 'sub_type
                                             (alist-get 'data (car segments)))))
                 (should (equal file
                                (alist-get 'file
                                           (alist-get 'data (car segments)))))))))
       (when (file-exists-p file)
         (delete-file file))))))

(ert-deftest qq-chat-attach-clipboard-uri-list-local-file ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (let ((path (make-temp-file "qq-clip-uri" nil ".txt")))
     (unwind-protect
         (progn
           (with-temp-file path (insert "hello clip"))
           (with-temp-buffer
             (qq-chat-mode)
             (setq qq-chat--session-key "private:10001")
             (qq-chat-render)
             (cl-letf (((symbol-function 'gui-get-selection)
                        (lambda (_selection &optional type)
                          (pcase type
                            ('TARGETS ["TARGETS" "text/uri-list" "text/plain"])
                            ('text/uri-list (concat "file://" path "\n"))
                            (_ nil)))))
               (qq-chat-attach-clipboard)
               (let ((segments (qq-chat--current-input-segments)))
                 (should (= 1 (length segments)))
                 (should (equal "file" (alist-get 'type (car segments))))
                 (should (equal path
                                (alist-get 'file
                                           (alist-get 'data (car segments)))))))))
       (ignore-errors (delete-file path))))))

(ert-deftest qq-chat-attach-clipboard-as-file-prefix ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (cl-letf (((symbol-function 'gui-get-selection)
                (lambda (_selection &optional type)
                  (pcase type
                    ('TARGETS ["TARGETS" "image/png"])
                    ('image/png qq-chat-test--1x1-png)
                    (_ nil)))))
       (qq-chat-attach-clipboard t)
       (should (equal "file"
                      (alist-get 'type
                                 (car (qq-chat--current-input-segments)))))))))

(ert-deftest qq-chat-attach-file-inserts-structured-object-and-sends-segments ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((path (make-temp-file "qq-chat-attach" nil ".txt")))
       (unwind-protect
           (let (sent-session sent-segments sent-raw)
             (qq-chat-attach-file path "file")
             (should (appkit-chatbuf-input-has-objects-p))
             (should (equal '("file")
                            (mapcar (lambda (segment) (alist-get 'type segment))
                                    (qq-chat--current-input-segments))))
             (qq-chat--set-pending-reply
              '((server-id . "42")
                (raw-message . "source")
                (segments . (((type . "text")
                              (data . ((text . "source"))))))))
             (cl-letf (((symbol-function 'qq-api-send-message)
                        (lambda (session-key segments &optional raw-message
                                             _callback _errback)
                          (setq sent-session session-key)
                          (setq sent-segments segments)
                          (setq sent-raw raw-message))))
               (qq-chat-send-message))
             (should (equal "private:10001" sent-session))
             (should-not sent-raw)
             (should (equal "reply" (alist-get 'type (nth 0 sent-segments))))
             (should (equal "42"
                            (alist-get 'id (alist-get 'data (nth 0 sent-segments)))))
             (should (equal "file" (alist-get 'type (nth 1 sent-segments))))
             (should (equal path
                            (alist-get 'file (alist-get 'data (nth 1 sent-segments)))))
             (should (equal (file-name-nondirectory path)
                            (alist-get 'name (alist-get 'data (nth 1 sent-segments))))))
         (ignore-errors (delete-file path)))))))

(ert-deftest qq-chat-send-failure-restores-rich-draft-and-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let* ((reply '((server-id . "9007199254742007094")
                     (raw-message . "source")))
            error-fn)
       (qq-chat--insert-input-segment-object
        '((type . "at")
          (data . ((qq . "10001") (name . "Alice Card")))))
       (qq-chat--set-reply-message reply)
       (cl-letf (((symbol-function 'qq-api-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (should (equal "" (appkit-chatbuf-input-string)))
         (should-not (appkit-chatbuf-aux-state))
         (funcall error-fn nil "network failed"))
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal '("at")
                      (mapcar (lambda (segment) (alist-get 'type segment))
                              (qq-chat--current-input-segments))))
       (should (equal "9007199254742007094"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-synchronous-send-error-restores-rich-draft-and-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((reply '((server-id . "9007199254742007094")
                    (raw-message . "source"))))
       (qq-chat--insert-input-segment-object
        '((type . "at")
          (data . ((qq . "10001") (name . "Alice Card")))))
       (qq-chat--set-reply-message reply)
       (cl-letf (((symbol-function 'qq-api-send-message)
                  (lambda (&rest _args) (error "transport exploded"))))
         (should-error (qq-chat-send-message) :type 'error))
       (should (appkit-chatbuf-input-has-objects-p))
       (should (equal "9007199254742007094"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-frame-error-during-send-restores-canonical-state ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "keep me")
     (qq-chat--set-reply-message
      '((server-id . "9007199254742007094") (raw-message . "source")))
     (let ((updates 0))
       (cl-letf (((symbol-function 'qq-chat--update-frame)
                  (lambda ()
                    (cl-incf updates)
                    (when (= updates 1)
                      (error "frame exploded"))))
                 ((symbol-function 'qq-api-send-message)
                  (lambda (&rest _args)
                    (ert-fail "frame error must happen before API send"))))
         (should-error (qq-chat-send-message) :type 'error))
       (should (= updates 2)))
     (should (equal "keep me" (qq-chat--current-draft-string)))
     (should (equal "9007199254742007094"
                    (alist-get 'server-id (qq-chat--reply-message)))))))

(ert-deftest qq-chat-send-rejects-numeric-reply-id-before-clearing-draft ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001" '((title . "Alice") (target-id . "10001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "keep me")
     (qq-chat--set-reply-message '((server-id . 42) (raw-message . "source")))
     (cl-letf (((symbol-function 'qq-api-send-message)
                (lambda (&rest _args)
                  (ert-fail "invalid reply id must fail before API send"))))
       (should-error (qq-chat-send-message) :type 'user-error))
     (should (equal "keep me" (qq-chat--current-draft-string)))
     (should (= 42 (alist-get 'server-id (qq-chat--reply-message)))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-draft ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let (error-fn)
       (cl-letf (((symbol-function 'qq-api-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (insert "new draft")
         (funcall error-fn nil "late failure"))
       (should (equal "new draft" (appkit-chatbuf-input-string)))
       (should (equal "new draft" (qq-chat--current-draft-string)))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-object ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let (error-fn)
       (cl-letf (((symbol-function 'qq-api-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (qq-chat--insert-input-segment-object
          '((type . "face") (data . ((id . "178")))))
         (funcall error-fn nil "late failure"))
       (should (equal '("face")
                      (mapcar (lambda (segment) (alist-get 'type segment))
                              (qq-chat--current-input-segments))))))))

(ert-deftest qq-chat-stale-send-failure-never-overwrites-new-reply ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (insert "old draft")
     (let ((new-reply '((server-id . "9007199254742007095")
                        (raw-message . "new source")))
           error-fn)
       (cl-letf (((symbol-function 'qq-api-send-message)
                  (lambda (_session _segments &optional _raw _callback errback)
                    (setq error-fn errback)))
                 ((symbol-function 'qq-api--default-error) #'ignore))
         (qq-chat-send-message)
         (qq-chat--set-reply-message new-reply)
         (funcall error-fn nil "late failure"))
       (should (equal "" (appkit-chatbuf-input-string)))
       (should (equal "9007199254742007095"
                      (alist-get 'server-id (qq-chat--reply-message))))))))

(ert-deftest qq-chat-load-older-messages-uses-oldest-cursor-and-guards ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (oldest-message-id . "200"))
    nil)
   ;; Seed one message so oldest cursor is real after merge.
   (qq-state-merge-history
    "private:10001"
    (list
     '((message_id . "200")
       (message_type . "private")
       (chat_type . 1)
       (target_id . "10001")
       (user_id . 10001)
       (time . 1710000200)
       (sender . ((user_id . 10001) (nickname . "Alice")))
       (raw_message . "latest")
       (message . (((type . "text") (data . ((text . "latest")))))))))
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (should (equal (qq-state-session-oldest-message-id "private:10001") "200"))
     (let (captured-before)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (session-key &optional before callback _errback)
                    (setq captured-before before)
                    (should (equal session-key "private:10001"))
                    (when callback
                      (funcall callback
                               (list :session-key session-key
                                     :message-count 1
                                     :added-count 1
                                     :oldest-message-id "100"))))))
         (qq-chat-load-older-messages)
         (should (equal captured-before "200"))
         (should-not qq-chat--history-loading)
         (should-not qq-chat--history-exhausted)))
     ;; Exhausted path: zero added.
     (setq qq-chat--history-loading nil)
     (setq qq-chat--history-exhausted nil)
     (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                (lambda (_session-key &optional _before callback _errback)
                  (when callback
                    (funcall callback
                             (list :added-count 0 :message-count 1))))))
       (qq-chat-load-older-messages)
       (should qq-chat--history-exhausted)
       (should-not qq-chat--history-loading))
     ;; Guard when exhausted.
     (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                (lambda (&rest _) (error "should not fetch when exhausted"))))
       (qq-chat-load-older-messages)
       (should qq-chat--history-exhausted)))))

(ert-deftest qq-api-history-exhausted-error-p ()
  (require 'qq-api)
  (should (qq-api--history-exhausted-error-p nil "消息200不存在"))
  (should (qq-api--history-exhausted-error-p
           '((message . "消息 not exist")) "fail"))
  (should-not (qq-api--history-exhausted-error-p nil "timeout")))

(ert-deftest qq-chat-goto-reply-jumps-to-loaded-target ()
  "telega-style: goto reply target when both messages are in the timeline."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    (list (cons (quote title) "Alice")
          (cons (quote target-id) "10001"))
    nil)
   (qq-state-merge-history
    "private:10001"
    (list
     (list (cons (quote message_id) "100")
           (cons (quote message_type) "private")
           (cons (quote chat_type) 1)
           (cons (quote target_id) "10001")
           (cons (quote user_id) 10001)
           (cons (quote time) 1710000100)
           (cons (quote sender)
                 (list (cons (quote user_id) 10001)
                       (cons (quote nickname) "Alice")))
           (cons (quote raw_message) "source")
           (cons (quote message)
                 (list (list (cons (quote type) "text")
                             (cons (quote data)
                                   (list (cons (quote text) "source")))))))
     (list (cons (quote message_id) "200")
           (cons (quote message_type) "private")
           (cons (quote chat_type) 1)
           (cons (quote target_id) "10001")
           (cons (quote user_id) 10001)
           (cons (quote time) 1710000200)
           (cons (quote sender)
                 (list (cons (quote user_id) 10001)
                       (cons (quote nickname) "Alice")))
           (cons (quote raw_message) "reply body")
           (cons (quote message)
                 (list (list (cons (quote type) "reply")
                             (cons (quote data)
                                   (list (cons (quote id) "100"))))
                       (list (cons (quote type) "text")
                             (cons (quote data)
                                   (list (cons (quote text) "reply body")))))))))
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((reply-pos (qq-chat--message-position "200")))
       (should reply-pos)
       (goto-char reply-pos)
       (should (equal "100"
                      (qq-chat--message-reply-id (qq-chat--message-at-point))))
       (qq-chat-goto-reply)
       (should (equal "100"
                      (get-text-property (point) (quote qq-chat-message-anchor))))
       (should (not (ring-empty-p qq-chat--messages-pop-ring)))
       (should (equal "200" (ring-ref qq-chat--messages-pop-ring 0)))
       (qq-chat-goto-pop-message)
       (should (equal "200"
                      (get-text-property (point) (quote qq-chat-message-anchor))))))))

(ert-deftest qq-chat-message-reply-id-from-segments ()
  (should
   (equal "42"
          (qq-chat--message-reply-id
           (list (cons (quote segments)
                       (list (list (cons (quote type) "reply")
                                   (cons (quote data)
                                         (list (cons (quote id) "42"))))
                             (list (cons (quote type) "text")
                                   (cons (quote data)
                                         (list (cons (quote text) "hi"))))))))))
  (should-not
   (qq-chat--message-reply-id
    (list (cons (quote segments)
                (list (list (cons (quote type) "text")
                            (cons (quote data)
                                  (list (cons (quote text) "hi"))))))))))

(ert-deftest qq-chat-goto-message-uses-history-around ()
  "Jump prefers fork get_msg_history_around, not load-older from oldest."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    (list (cons (quote title) "Alice")
          (cons (quote target-id) "10001")
          (cons (quote oldest-message-id) "900"))
    nil)
   (qq-state-merge-history
    "private:10001"
    (list
     (list (cons (quote message_id) "900")
           (cons (quote message_type) "private")
           (cons (quote chat_type) 1)
           (cons (quote target_id) "10001")
           (cons (quote user_id) 10001)
           (cons (quote time) 1710000900)
           (cons (quote sender)
                 (list (cons (quote user_id) 10001)
                       (cons (quote nickname) "Alice")))
           (cons (quote raw_message) "newest")
           (cons (quote message)
                 (list (list (cons (quote type) "text")
                             (cons (quote data)
                                   (list (cons (quote text) "newest")))))))))
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let (around-called load-older-called)
       (cl-letf (((symbol-function 'qq-api-fetch-history-around)
                  (lambda (session-key message-id callback &optional _errback _count)
                    (setq around-called message-id)
                    (should (equal session-key "private:10001"))
                    (qq-state-merge-history
                     "private:10001"
                      (list
                       (list (cons (quote message_id) "100")
                             (cons (quote message_type) "private")
                             (cons (quote chat_type) 1)
                             (cons (quote target_id) "10001")
                             (cons (quote user_id) 10001)
                            (cons (quote time) 1710000100)
                            (cons (quote sender)
                                  (list (cons (quote user_id) 10001)
                                        (cons (quote nickname) "Alice")))
                            (cons (quote raw_message) "old target")
                            (cons (quote message)
                                  (list (list (cons (quote type) "text")
                                              (cons (quote data)
                                                    (list (cons (quote text)
                                                                "old target")))))))))
                    (qq-chat-render)
                    (when callback
                      (funcall callback
                               (list :added-count 1 :message-count 1)))))
                 ((symbol-function 'qq-chat-load-older-messages)
                  (lambda ()
                    (setq load-older-called t)
                    (error "jump must not call load-older"))))
         (qq-chat-goto-message "100" 'no-pop)
         (should (equal around-called "100"))
         (should-not load-older-called)
         (should (equal "100"
                        (get-text-property (point) 'qq-chat-message-anchor)))
         (should-not qq-chat--pending-jump-id))))))

(ert-deftest qq-chat-jump-reports-missing-around-target-without-retry ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "private:10001"
          qq-chat--pending-jump-id "100")
    (let ((requests 0)
          failure)
      (cl-letf (((symbol-function 'qq-api-fetch-history-around)
                 (lambda (_session _target callback &optional _errback _count)
                   (cl-incf requests)
                   (funcall callback '(:message-count 0))))
                ((symbol-function 'qq-chat--note-history-window) #'ignore)
                ((symbol-function 'qq-chat--finish-jump-if-loaded)
                 (lambda (_target) nil))
                ((symbol-function 'qq-chat--jump-fail)
                 (lambda (target reason) (setq failure (list target reason)))))
        (qq-chat--seek-history-for-jump
         "private:10001" "100" (current-buffer))
        (should (= requests 1))
        (should (equal failure '("100" "around window omitted target")))))))


(ert-deftest qq-chat-input-segments-keep-cjk-text-after-image-object ()
  "Image object must not swallow following Chinese when sending.

Regression: object text-properties were rear-sticky, so CJK typed after an
attachment inherited `appkit-chatbuf-input-object' and was dropped on parse."
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    (list (cons (quote title) "Alice")
          (cons (quote target-id) "10001"))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (qq-chat-edit-draft)
     (let* ((path (make-temp-file "qq-img" nil ".png"))
            (segments nil))
       (unwind-protect
           (progn
             (with-temp-file path (insert "x"))
             (qq-chat-attach-file path "image")
             ;; Type CJK after the attachment object (normal user flow).
             (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
             (insert "你好世界")
             (qq-chat--sync-draft-from-buffer)
             (setq segments (qq-chat--current-input-segments))
             (should (equal '("image" "text")
                            (mapcar (lambda (s) (alist-get (quote type) s))
                                    segments)))
             (should (equal "你好世界"
                            (alist-get (quote text)
                                       (alist-get (quote data)
                                                  (nth 1 segments))))))
         (ignore-errors (delete-file path)))))))

(ert-deftest qq-chat-media-preview-uses-qq-opener-not-browser-url ()
  "A compact QQ card uses shared context and no inline action toolbar."
  (let* ((segment '((type . "image")
                    (data . ((url . "https://example.com/picture.gif")
                             (name . "picture.gif")))))
         preview-url
         opened-segment)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-download-state)
                   (lambda (_segment)
                     '(:status not-downloaded :path "/tmp/picture.gif")))
                  ((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-preview-image)
                   (lambda (_segment) 'qq-test-image))
                  ((symbol-function 'qq-media-segment-preview-fetching-p)
                   (lambda (_segment) nil))
                  ((symbol-function 'appkit-media-insert-image-slices)
                   (lambda (_image url &optional _prefix _fallback)
                     (setq preview-url url)
                     (insert "PREVIEW")))
                  ((symbol-function 'qq-media-segment-open)
                   (lambda (media-segment)
                     (setq opened-segment media-segment))))
          (qq-chat--insert-segment-media-line segment nil nil)
          (should-not preview-url)
          (should (string-match-p (regexp-quote "[image] picture.gif")
                                  (buffer-string)))
          (dolist (old-action '("[Open]" "[Play]" "[Copy URL]"
                                "[Download]" "[Save As]" "transfer:"))
            (should-not (string-match-p (regexp-quote old-action)
                                        (buffer-string))))
          (goto-char (point-min))
          (search-forward "PREVIEW")
          (let ((context (appkit-media-card-context-at-point)))
            (should (equal (plist-get context :payload) segment))
            (should (functionp (plist-get context :download-action)))
            (should (functionp (plist-get context :save-as-action)))
            (should (functionp (plist-get context :copy-url-action)))
            (appkit-media-card-call-action 'open context))
          (should (equal opened-segment segment)))))))

(ert-deftest qq-chat-video-preview-keeps-video-alt-text ()
  (let ((segment '((type . "video")
                   (data . ((name . "short.mp4")
                            (url . "https://example.com/short.mp4")
                            (remote_status . "available")))))
        fallback)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-preview-image)
                   (lambda (_segment) 'qq-video-preview))
                  ((symbol-function 'qq-media-segment-preview-fetching-p)
                   (lambda (_segment) nil))
                  ((symbol-function 'appkit-media-insert-image-slices)
                   (lambda (_image _action &optional _prefix alt-text)
                     (setq fallback alt-text)
                     (insert "VIDEO-PREVIEW"))))
          (qq-chat--insert-segment-media-line segment nil nil)
          (should (equal fallback "[video]")))))))

(ert-deftest qq-chat-media-card-context-targets-exact-segment-at-point ()
  "Shared card context keeps multi-segment QQ messages unambiguous."
  (let ((first '((type . "image")
                 (data . ((name . "first.png")
                          (url . "https://example.com/first.png")))))
        (second '((type . "video")
                  (data . ((name . "second.mp4")
                           (url . "https://example.com/second.mp4"))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-segment-download-state)
                   (lambda (_segment)
                     '(:status not-downloaded :path "/tmp/media")))
                  ((symbol-function 'qq-media-segment-preview-capable-p)
                   (lambda (_segment) nil)))
          (qq-chat--insert-segment-media-line first nil nil)
          (qq-chat--insert-segment-media-line second nil nil)
          (goto-char (point-min))
          (search-forward "second.mp4")
          (should
           (equal second
                  (plist-get (appkit-media-card-context-at-point) :payload))))))))

(ert-deftest qq-chat-renders-structured-mail-segment ()
  (let ((segment '((type . "mail")
                   (data . ((sender . "Henrik Lissner")
                            (subject . "Re: Doom Emacs")
                            (content . "Closed the issue as completed.")
                            (detail . "邮件详情")
                            (url . "https://mail.qq.com/example")))))
        opened-url)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &optional _new-window)
                     (setq opened-url url))))
          (qq-chat--insert-mail-segment segment nil nil)
          (should (string-match-p "Mail · Henrik Lissner" (buffer-string)))
          (should (string-match-p "Re: Doom Emacs" (buffer-string)))
          (should (string-match-p "Closed the issue as completed" (buffer-string)))
          (should-not (string-match-p "com.tencent.template.public" (buffer-string)))
          (goto-char (point-min))
          (search-forward "邮件详情")
          (button-activate (button-at (1- (point))))
          (should (equal opened-url "https://mail.qq.com/example")))))))

(ert-deftest qq-chat-renders-normalized-ark-card-segment ()
  (let ((segment '((type . "card")
                   (data . ((kind . "share")
                            (source . "豆包")
                            (title . "和豆包的对话")
                            (content . "点击查看对话内容")
                            (url . "https://example.com/thread")))))
        opened-url)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'browse-url)
                   (lambda (url &optional _new-window)
                     (setq opened-url url))))
          (qq-chat--insert-card-segment segment nil nil)
          (should (string-match-p "Share · 豆包" (buffer-string)))
          (should (string-match-p "和豆包的对话" (buffer-string)))
          (should (string-match-p "点击查看对话内容" (buffer-string)))
          (should-not (string-match-p "com.tencent.tuwen.lua" (buffer-string)))
          (should-not (string-match-p "\\[Open\\]" (buffer-string)))
          (goto-char (point-min))
          (should (button-at (point)))
          (push-button (point))
          (should (equal opened-url "https://example.com/thread")))))))

(ert-deftest qq-chat-renders-card-preview-image-without-an-open-label ()
  (let ((segment '((type . "card")
                   (data . ((kind . "share")
                            (source . "QQ空间")
                            (title . "一条说说")
                            (image . "https://example.com/preview.png"))))))
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-media-url-preview-display-string)
                   (lambda (&rest _args) "PREVIEW")))
          (qq-chat--insert-card-segment segment nil nil)
          (should (string-match-p "Share · QQ空间" (buffer-string)))
          (should (string-match-p "PREVIEW" (buffer-string)))
          (should-not (string-match-p "\\[Open\\]" (buffer-string))))))))

(ert-deftest qq-chat-renders-poke-as-a-distinct-gray-tip-row ()
  (let ((message
         '((server-id . "poke-1")
           (time . 1710000001)
           (sender-id . "10001")
           (sender-name . "Alice")
           (target-id . "10002")
           (segments
            . (((type . "poke")
                (data .
                 ((actor-name . "Alice")
                 (target-name . "Bob")
                 (image-url . "https://example.com/poke.png")
                 (action . "喷了喷")
                 (detail . "的加分喷雾，分数++")))))))))
    (with-temp-buffer
      (let ((inhibit-read-only t)
            (fill-column 80))
        (cl-letf (((symbol-function 'qq-media-url-preview-display-string)
                   (lambda (&rest _args) "✦")))
          (qq-chat--insert-poke-message message nil)
          (should (string-match-p
                   (regexp-quote
                    "( ✦ Alice 喷了喷 Bob 的加分喷雾，分数++ )")
                   (buffer-string)))
          (should (string-match-p "00:00" (buffer-string)))
          (should (= (count-lines (point-min) (point-max)) 1))
          (should (string-match-p "的加分喷雾，分数++" (buffer-string)))
          (should-not (string-match-p "@ Alice" (buffer-string)))
          (goto-char (point-min))
          (search-forward "00:00")
          (should (equal (get-text-property (- (match-beginning 0) 1)
                                            'display)
                         '(space :align-to 75)))
          (should (eq (get-text-property 2 'face) 'qq-msg-poke)))))))

(ert-deftest qq-chat-renders-json-gray-tip-as-system-divider ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:987654321"
    '((type . group) (title . "Test Group") (target-id . "987654321"))
    nil)
   (qq-state-apply-gray-tip-notice
    '((post_type . "notice")
      (notice_type . "notify")
      (sub_type . "gray_tip")
      (group_id . 987654321)
      (user_id . 0)
      (message_id . "9007199254750003456")
      (busi_id . "19366")
      (content . "{\"items\":[{\"txt\":\"新进群账号疑似来自非大陆地区，请谨慎核实对方身份。\",\"type\":\"nor\"},{\"txt\":\"查看异常>\",\"type\":\"url\"}]}")))
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:987654321")
     (qq-chat-render)
     (should (equal (appkit-chat-timeline-keys)
                    '("9007199254750003456")))
     (should (string-match-p
              "新进群账号疑似来自非大陆地区，请谨慎核实对方身份。查看异常>"
              (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest qq-chat-history-header-right-aligns-time-through-appkit ()
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (fill-column 50)
          (message '((server-id . "m1")
                     (time . 1710000001)
                     (sender-id . "10001")
                     (sender-name . "Alice")
                     (segments . (((type . "text")
                                   (data . ((text . "hello")))))))))
      (cl-letf (((symbol-function 'qq-chat--message-avatar-prefixes)
                 (lambda (&rest _args)
                   '(:header "AVA " :first-body "    " :rest-body "    "))))
        (qq-chat--render-message message nil)
        (goto-char (point-min))
        (search-forward (qq-chat--format-time 1710000001))
        (should (equal (get-text-property (- (match-beginning 0) 1)
                                          'display)
                       `(space :align-to
                               ,(- 50
                                   (string-width
                                    (qq-chat--format-time 1710000001))))))))))

(ert-deftest qq-chat-avatar-spans-heading-and-first-body-line ()
  (with-temp-buffer
    (let ((inhibit-read-only t)
          (fill-column 80)
          (message '((server-id . "m1")
                     (time . 1710000001)
                     (sender-id . "10001")
                     (sender-name . "Alice")
                     (segments . (((type . "text")
                                   (data . ((text . "hello\nworld")))))))))
      (cl-letf (((symbol-function 'qq-chat--message-avatar-prefixes)
                 (lambda (&rest _args)
                   '(:header "TOP "
                     :first-body "BOTTOM "
                     :rest-body "       "))))
        (qq-chat--render-message message nil)
        (goto-char (point-min))
        (should (equal (get-text-property (point) 'line-prefix) "TOP "))
        (forward-line 1)
        (should (equal (get-text-property (point) 'line-prefix) "BOTTOM "))
        (forward-line 1)
        (should (equal (get-text-property (point) 'line-prefix) "       "))))))

(ert-deftest qq-chat-avatar-prefixes-use-shared-two-line-renderer ()
  (let (avatar-user-id shared-args)
    (cl-letf (((symbol-function 'qq-media-avatar-image)
               (lambda (user-id)
                 (setq avatar-user-id user-id)
                 'avatar-image))
              ((symbol-function 'appkit-chat-avatar-two-line-pixel-size)
               (lambda () 42))
              ((symbol-function 'appkit-chat-avatar-prefixes)
               (lambda (image fallback &rest args)
                 (setq shared-args (list image fallback args))
                 '(:header "TOP "
                   :first-body "BOTTOM "
                   :rest-body "       "))))
      (let ((prefixes
             (qq-chat--message-avatar-prefixes
              '((sender-id . "10001") (sender-name . "Alice")))))
        (should (equal avatar-user-id "10001"))
        (should (equal shared-args
                       '(avatar-image "@" (:pixel-size 42 :resize t))))
        (should (eq (get-text-property 0 'mouse-face
                                      (plist-get prefixes :header))
                    'highlight))
        (should-not (get-text-property 0 'mouse-face
                                      (plist-get prefixes :rest-body)))))))

(ert-deftest qq-chat-message-hover-is-limited-to-interactive-children ()
  (let ((properties
         (qq-chat--message-line-properties
          '((server-id . "m1") (local-id . nil)) "m1")))
    ;; Like telega's outer `telega-msg' button: retain message identity and
    ;; read-only behavior without a blanket mouse face.  Interactive children
    ;; (avatar, sender, media, links, reactions) install their own hover face.
    (should (equal (plist-get properties 'qq-chat-message-anchor) "m1"))
    (should (plist-get properties 'read-only))
    (should-not (plist-member properties 'mouse-face))))

(ert-deftest qq-chat-window-resize-refreshes-layout-only-when-width-changes ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--fill-column 70)
    (let ((win (selected-window))
          (refresh-count 0))
      (cl-letf (((symbol-function 'qq-chat--render-window) (lambda () win))
                ((symbol-function 'qq-chat--compute-fill-column)
                 (lambda (&optional candidate)
                   (should (eq candidate win))
                   90))
                ((symbol-function 'qq-chat--refresh-timeline-layout)
                 (lambda () (cl-incf refresh-count))))
        (qq-chat--on-window-size-change)
        (should (= qq-chat--fill-column 90))
        (should (= fill-column 90))
        (should (= refresh-count 1))
        (qq-chat--on-window-size-change)
        (should (= refresh-count 1))))))

(ert-deftest qq-chat-text-scale-refreshes-pixel-alignment-at-same-width ()
  (with-temp-buffer
    (qq-chat-mode)
    (should (= line-spacing 0))
    (setq qq-chat--fill-column 90)
    (let ((win (selected-window))
          (refresh-count 0))
      (cl-letf (((symbol-function 'qq-chat--render-window) (lambda () win))
                ((symbol-function 'qq-chat--compute-fill-column)
                 (lambda (&optional candidate)
                   (should (eq candidate win))
                   90))
                ((symbol-function 'qq-chat--refresh-timeline-layout)
                 (lambda () (cl-incf refresh-count))))
        (qq-chat--on-text-scale-change)
        (should (= qq-chat--fill-column 90))
        (should (= refresh-count 1))))))

(ert-deftest qq-chat-timeline-inserts-explicit-newer-history-gap ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-gap "m1")
    (let* ((timeline (qq-chat--timeline-messages
                      '(((server-id . "m1") (time . 1))
                        ((server-id . "m2") (time . 2)))))
           (anchors (mapcar #'qq-chat--message-anchor timeline)))
      (should (equal anchors '("m1" "history-gap:m1" "m2")))
      (should (qq-chat--history-gap-message-p (nth 1 timeline))))))

(ert-deftest qq-chat-load-newer-clears-gap-on-short-final-page ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-gap "m20")
    (cl-letf (((symbol-function 'qq-api-fetch-history-page)
               (lambda (session-key cursor direction callback &rest _)
                 (should (equal session-key "group:20001"))
                 (should (equal cursor "m20"))
                 (should (eq direction 'newer))
                 (funcall callback
                          '(:added-count 4
                            :message-count 5
                            :batch-message-ids ("m20" "m21" "m22" "m23" "m24")
                            :batch-newest-message-id "m24"))))
              ((symbol-function 'qq-chat--update-frame) #'ignore)
              ((symbol-function 'qq-chat--sync-timeline) #'ignore))
      (qq-chat-load-newer-messages t)
      (should-not (qq-chat--history-get :gap-after-id))
      (should (qq-chat--history-get :newer-loaded)))))

(ert-deftest qq-chat-post-command-auto-loads-older-near-top ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((inhibit-read-only t))
      (insert (make-string 3000 ?x)))
    (goto-char (point-min))
    (let ((qq-chat-history-auto-load-threshold 2000)
          called)
      (cl-letf (((symbol-function 'appkit-chatbuf-point-in-input-p) (lambda () nil))
                ((symbol-function 'qq-chat-load-older-messages)
                 (lambda (&optional quiet) (setq called quiet))))
        (qq-chat--maybe-auto-load-older)
        (should (eq called t))))))

(ert-deftest qq-chat-initial-history-prefers-exact-read-position-window ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (around-call completed)
      (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                 (lambda (_session-key callback &optional _errback)
                   (funcall callback
                            '((unread_count . 7)
                              (first_unread
                               . ((sequence . "30001")
                                  (message_id . "9007199254742007089")))
                              (latest . nil)))))
                ((symbol-function 'qq-api-fetch-history-around)
                 (lambda (session-key message-id callback &optional _errback count)
                   (setq around-call (list session-key message-id count))
                   (funcall callback
                            '(:batch-newest-message-id "9007199254742007090"))))
                ((symbol-function 'qq-chat--complete-initial-history-load)
                 (lambda (&rest args) (setq completed args))))
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (should (equal around-call
                       '("group:20001" "9007199254742007089" 40)))
        (should (equal (nth 3 completed) "9007199254742007089"))))))

(ert-deftest qq-chat-initial-history-ignores-stale-read-state-callback ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (read-callbacks canceled (history-calls 0) (request-count 0))
      (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                 (lambda (_session-key callback &optional _errback)
                   (setq read-callbacks
                         (append read-callbacks (list callback)))
                   (intern (format "request-%d" (cl-incf request-count)))))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (request) (setq canceled request)))
                ((symbol-function 'qq-api-fetch-older-history)
                 (lambda (&rest _)
                   (cl-incf history-calls)
                   'history-request)))
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (should (eq canceled 'request-1))
        (funcall (car read-callbacks)
                 '((unread_count . 0) (first_unread . nil) (latest . nil)))
        (should (= history-calls 0))
        (funcall (cadr read-callbacks)
                 '((unread_count . 0) (first_unread . nil) (latest . nil)))
        (should (= history-calls 1))))))

(ert-deftest qq-chat-forward-segment-uses-dedicated-block-renderer ()
  (let* ((segment '((type . "forward")
                    (data
                     . ((content
                         . ((kind . "remote")
                            (reference
                             . ((kind . "message")
                                (message_id . "9007199254743009336")
                                (chat . ((kind . "group")
                                         (group_id . "20001")))))))))))
         (message `((server-id . "9007199254743009336")
                    (segments . (,segment))))
         called)
    (with-temp-buffer
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function 'qq-forward-insert-segment)
                   (lambda (candidate _prefix _properties)
                     (setq called candidate)
                     (insert "FORWARD-CARD\n"))))
          (should (qq-chat--message-has-block-segments-p message))
          (qq-chat--insert-message-body message nil nil)
          (should (equal called segment))
          (should (equal (buffer-string) "FORWARD-CARD\n")))))))

(ert-deftest qq-chat-single-forward-keeps-snowflake-string ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "hello")
     (let (call)
       (cl-letf (((symbol-function 'qq-api-forward-message)
                  (lambda (message-id source target callback
                                      &optional _errback)
                    (setq call (list message-id source target))
                    (funcall callback nil))))
         (qq-chat-forward-message "private:10002")
         (should (equal call
                        '("9007199254743009336"
                          "group:20001" "private:10002"))))))))

(ert-deftest qq-chat-forward-targets-include-non-session-friends-and-groups ()
  (qq-chat-test-with-reset
   (qq-state-apply-friends
    '(((user_id . "10001") (nickname . "Alice") (remark . "A"))))
   (qq-state-apply-groups
    '(((group_id . "20001") (group_name . "Group A"))))
   (let ((targets (qq-chat--forwardable-target-sessions)))
     (should (assoc "private:10001"
                    (mapcar (lambda (target)
                              (cons (alist-get 'key target) target))
                            targets)))
     (should (assoc "group:20001"
                    (mapcar (lambda (target)
                              (cons (alist-get 'key target) target))
                            targets))))))

(ert-deftest qq-chat-marked-forward-preserves-timeline-order-and-clears ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first"))
      ((server-id . "9007199254743009444")
       (sender-id . "10002") (sender-name . "Bob")
       (time . 101) (raw-message . "second")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     ;; Mark newest first to prove send order follows the timeline, not clicks.
     (goto-char (point-min))
     (search-forward "second")
     (qq-chat-toggle-forward-mark)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-forward-mark)
     (should (= 2 (length (qq-chat-marked-messages))))
     (let (source target captured-ids)
       (cl-letf (((symbol-function 'qq-api-send-forward-bundle)
                  (lambda (source-session-key target-session-key ids callback
                                              &optional _errback)
                    (setq source source-session-key
                          target target-session-key
                          captured-ids ids)
                    (funcall callback nil))))
         (qq-chat-forward-marked-messages "group:30001")
         (should (equal source "group:20001"))
         (should (equal target "group:30001"))
         (should
          (equal captured-ids
                 '("9007199254743009336" "9007199254743009444")))
         (should (seq-every-p #'stringp captured-ids))
         (should-not qq-chat--marked-message-anchors))))))

(ert-deftest qq-chat-forward-callback-preserves-marks-added-in-flight ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Source") (target-id . "20001") (type . group))
    nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254743009336")
       (sender-id . "10001") (sender-name . "Alice")
       (time . 100) (raw-message . "first"))
      ((server-id . "9007199254743009444")
       (sender-id . "10002") (sender-name . "Bob")
       (time . 101) (raw-message . "second"))
      ((server-id . "9007199254743009555")
       (sender-id . "10003") (sender-name . "Carol")
       (time . 102) (raw-message . "third")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "first")
     (qq-chat-toggle-forward-mark)
     (goto-char (point-min))
     (search-forward "second")
     (qq-chat-toggle-forward-mark)
     (let (success-callback)
       (cl-letf (((symbol-function 'qq-api-send-forward-bundle)
                  (lambda (_source _target _ids callback &optional _errback)
                    (setq success-callback callback))))
         (qq-chat-forward-marked-messages "group:30001")
         (should qq-chat--forward-request-active-p)
         (should-error
          (qq-chat-forward-marked-messages "group:30001")
          :type 'user-error)
         (goto-char (point-min))
         (search-forward "third")
         (qq-chat-toggle-forward-mark)
         (funcall success-callback nil)
         (should-not qq-chat--forward-request-active-p)
         (should (equal qq-chat--marked-message-anchors
                        '("9007199254743009555")))
         (should (equal
                  (mapcar #'qq-chat--message-anchor
                          (qq-chat-marked-messages))
                  '("9007199254743009555"))))))))

(provide (quote qq-chat-test))

;;; qq-chat-test.el ends here
(ert-deftest qq-chat-animated-face-is-a-block-segment ()
  (let ((segment '((type . "face")
                   (data . ((id . "478")
                            (raw . ((faceType . 3)
                                    (stickerId . "80"))))))))
    (should (qq-chat--animated-face-segment-p segment))
    (should (qq-chat--message-has-block-segments-p
             `((segments . (,segment)))))))
