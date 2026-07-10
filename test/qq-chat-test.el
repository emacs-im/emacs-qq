;;; qq-chat-test.el --- Tests for qq-chat -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-state)
(require 'qq-transient)

(defmacro qq-chat-test-with-reset (&rest body)
  "Run BODY with clean qq state and disabled live-update hooks."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (qq-media-rerender-function nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

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
             (should (qq-chat--point-in-input-p))
             (should (eq (key-binding (kbd "q") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "s") t)
                         'self-insert-command))
             (should (eq (key-binding (kbd "RET") t) 'qq-chat-return-dwim))
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
             (should (eq (key-binding (kbd "m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c /") t) 'qq-chat-search))
             (should (eq (key-binding (kbd "C-c m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "C-c ?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c C-a") t) 'qq-chat-attach-transient))
             (should (eq (key-binding (kbd "C-c C-v") t) 'qq-chat-attach-clipboard)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-chat-set-draft-preserves-ewoc-and-input-markers ()
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
     (let ((ewoc qq-chat--ewoc)
           (input-marker qq-chat--input-marker)
           (prompt-marker qq-chat--input-prompt-marker))
       (qq-chat--set-draft "updated body")
       (should (eq ewoc qq-chat--ewoc))
       (should (eq input-marker qq-chat--input-marker))
       (should (eq prompt-marker qq-chat--input-prompt-marker))
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
         (when (and (eq (get-text-property pos 'field) 'disco-chatbuf-prompt)
                    (not (eq (get-text-property (max (point-min) (1- pos)) 'field)
                             'disco-chatbuf-prompt)))
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
     (qq-chat--update-frame-preserving-point)
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
       (should-not (qq-chat--point-in-input-p)))
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
     (let ((ewoc qq-chat--ewoc)
           (node-m1 (gethash "m1" qq-chat--message-node-table))
           (node-m2 (gethash "m2" qq-chat--message-node-table))
           redisplayed)
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
       (let ((orig-redisplay (symbol-function 'qq-chat--redisplay-node)))
         (cl-letf (((symbol-function 'qq-chat--redisplay-node)
                    (lambda (node)
                      (push (qq-chat--message-anchor (ewoc--node-data node))
                            redisplayed)
                      (funcall orig-redisplay node))))
           (qq-chat-render)))
       (should (eq ewoc qq-chat--ewoc))
       (should (eq node-m1 (gethash "m1" qq-chat--message-node-table)))
       (should (eq node-m2 (gethash "m2" qq-chat--message-node-table)))
       (should (gethash "m3" qq-chat--message-node-table))
       (should (equal '("m1" "m2" "m3")
                      qq-chat--displayed-message-anchors))
       (should (equal '("m2" "m3") (sort redisplayed #'string<)))
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
         (qq-chat--invalidate-message-anchors-preserving-point qq-chat--displayed-message-anchors)
         (should mark-active)
         (should (= before (point)))
         (should (= before (mark t))))))))

(ert-deftest qq-chat-handle-state-change-ignores-heartbeat ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (events)
       (cl-letf (((symbol-function 'qq-chat-render)
                  (lambda () (push 'render events)))
                 ((symbol-function 'qq-chat--chat-update)
                  (lambda (&rest _parts) (push 'chat-update events)))
                 ((symbol-function 'qq-chat--header-line-update)
                  (lambda () (push 'header-line events)))
                 ((symbol-function 'qq-chat--apply-single-message-change-partially)
                  (lambda (&rest _args) (push 'partial events)))
                 ((symbol-function 'qq-chat--request-node-redisplay)
                  (lambda (&rest _args) (push 'nodes events))))
         (qq-chat--handle-state-change '(:type heartbeat :timestamp 1.0))
         (should-not events))))))

(ert-deftest qq-chat-handle-state-change-prefers-partial-message-update ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (partial-calls render-called)
       (cl-letf (((symbol-function 'qq-chat--apply-single-message-change-partially)
                  (lambda (anchor _messages)
                    (push anchor partial-calls)
                    t))
                 ((symbol-function 'qq-chat-render)
                  (lambda ()
                    (setq render-called t))))
         (qq-chat--handle-state-change
          '(:type message
            :session-key "private:10001"
            :message ((server-id . "9007199254741004645"))))
         (should (equal '("9007199254741004645") partial-calls))
         (should-not render-called))))))

(ert-deftest qq-chat-handle-state-change-falls-back-to-full-render-when-partial-fails ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (render-called)
       (cl-letf (((symbol-function 'qq-chat--apply-single-message-change-partially)
                  (lambda (_anchor _messages)
                    nil))
                 ((symbol-function 'qq-chat-render)
                  (lambda ()
                    (setq render-called t))))
         (qq-chat--handle-state-change '(:type message
                                         :session-key "private:10001"
                                         :message ((server-id . "42"))))
         (should render-called))))))

(ert-deftest qq-chat-handle-state-change-session-uses-partitioned-header-update ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (updates render-called)
       (cl-letf (((symbol-function 'qq-chat--chat-update)
                  (lambda (&rest parts)
                    (push parts updates)))
                 ((symbol-function 'qq-chat-render)
                  (lambda ()
                    (setq render-called t))))
         (qq-chat--handle-state-change '(:type session
                                         :session-key "private:10001"
                                         :mutation session))
         (should (equal '((header-line header)) updates))
         (should-not render-called))))))

(ert-deftest qq-chat-handle-state-change-read-mutation-applies-partial-read ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (partial-called render-called)
       (cl-letf (((symbol-function 'qq-chat--apply-read-state-change-partially)
                  (lambda ()
                    (setq partial-called t)
                    t))
                 ((symbol-function 'qq-chat-render)
                  (lambda ()
                    (setq render-called t))))
         (qq-chat--handle-state-change '(:type session
                                         :session-key "private:10001"
                                         :mutation read))
         (should partial-called)
         (should-not render-called))))))

(ert-deftest qq-chat-handle-state-change-prefers-event-message-anchor ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (partial-calls)
       (cl-letf (((symbol-function 'qq-chat--apply-single-message-change-partially)
                  (lambda (anchor _messages)
                    (push anchor partial-calls)
                    t))
                 ((symbol-function 'qq-chat-render)
                  (lambda () nil)))
         (qq-chat--handle-state-change
          '(:type message
            :session-key "private:10001"
            :mutation create
            :message-anchor "9007199254741004645"
            :message ((server-id . "other"))))
         (should (equal '("9007199254741004645") partial-calls)))))))

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
        (message_id . "9007199254741007777")
        (user_id . 90001)
        (target_id . 10001)
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
                      qq-chat--displayed-message-anchors))
       (qq-state-apply-recall "9007199254741007777")
       (qq-chat--handle-state-change
        (list :type 'message
              :session-key "private:10001"
              :mutation 'update
              :message-anchor "9007199254741007777"
              :message (car (qq-state-session-messages "private:10001"))))
       (should-not qq-chat--displayed-message-anchors)
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
        (message_id . "9007199254741008888")
        (user_id . 90001)
        (target_id . 10001)
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
       (should (equal '("9007199254741008888")
                      qq-chat--displayed-message-anchors))
       (should (string-match-p
                "recalled"
                (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest qq-chat-handle-state-change-friends-refreshes-timeline-for-name-updates ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (let (updates)
       (cl-letf (((symbol-function 'qq-chat--chat-update)
                  (lambda (&rest parts)
                    (push parts updates))))
         (qq-chat--handle-state-change '(:type friends-refreshed :count 1))
         (should (equal '((header-line header timeline)) updates)))))))

(ert-deftest qq-chat-partial-message-update-updates-empty-timeline-placeholder ()
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
     (should qq-chat--empty-node)
     (goto-char (point-min))
     (should (search-forward "No messages loaded yet." nil t))
     (let ((messages '(((server-id . "m1")
                        (sender-id . "10001")
                        (sender-name . "Alice")
                        (time . 100)
                        (raw-message . "first")))))
       (puthash "private:10001" messages qq-state--messages-by-session)
       (should (qq-chat--apply-single-message-change-partially "m1" messages))
       (should-not qq-chat--empty-node)
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
       (qq-chat--chat-update 'footer)
       (puthash "private:10001" updated qq-state--messages-by-session)
       (let ((redisplay-original (symbol-function 'qq-chat--redisplay-node))
             redisplayed)
         (cl-letf (((symbol-function 'qq-chat--redisplay-node)
                    (lambda (node)
                      (push (qq-chat--message-anchor (ewoc--node-data node))
                            redisplayed)
                      (funcall redisplay-original node))))
           (should (qq-chat--apply-single-message-change-partially
                    "m1" (qq-chat--timeline-messages updated))))
         (should (member "m1" redisplayed))
         (should (member "m2" redisplayed)))
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
         (should (qq-chat--apply-single-message-change-partially
                  "m1" (qq-chat--timeline-messages (list recalled reply)))))
       (should-not (gethash "m1" qq-chat--message-node-table))
       (should (gethash "m2" qq-chat--message-node-table))
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
       (qq-chat--chat-update 'timeline)
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
       (should (qq-chat--apply-single-message-change-partially local-id pending))
       (should (gethash local-id qq-chat--message-node-table))
       (should (equal qq-chat--displayed-message-anchors (list local-id)))
       (puthash "private:10001" sent qq-state--messages-by-session)
       (should (qq-chat--apply-single-message-change-partially snowflake sent))
       (should-not (gethash local-id qq-chat--message-node-table))
       (should (gethash snowflake qq-chat--message-node-table))
       (should (equal qq-chat--displayed-message-anchors (list snowflake)))
       (goto-char (point-min))
       (should (search-forward "hi" nil t))))))

(ert-deftest qq-chat-partial-message-delete-restores-empty-timeline-placeholder ()
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
       (should-not qq-chat--empty-node)
       (puthash "private:10001" nil qq-state--messages-by-session)
       (should (qq-chat--apply-single-message-change-partially "m1" nil))
       (should qq-chat--empty-node)
       (goto-char (point-min))
       (should (search-forward "No messages loaded yet." nil t))))))

(ert-deftest qq-chat-media-cache-update-requests-node-refresh ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--displayed-message-anchors '("m1"))
     (let (requested)
       (cl-letf (((symbol-function 'qq-chat--request-node-redisplay)
                  (lambda (anchors)
                    (push anchors requested))))
         (qq-chat--rerender-open-chats)
         (should (equal '(("m1")) requested)))))))

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
       (cl-letf (((symbol-function 'qq-chat--request-node-redisplay)
                  (lambda (anchors)
                    (push anchors requested))))
         (qq-chat--rerender-open-chats "face:88")
         (should (equal '(("m1")) requested)))))))

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
       (let (called)
         (cl-letf (((symbol-function 'qq-chat--invalidate-message-anchors-preserving-point)
                    (lambda (anchors)
                      (setq called anchors))))
           (qq-chat--request-node-redisplay '("m1"))
           (should-not called)
           (should (equal '("m1") qq-chat--deferred-node-anchors))))))))

(ert-deftest qq-chat-node-refresh-uses-single-node-redisplay-helper ()
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
     (let (redisplayed)
       (cl-letf (((symbol-function 'qq-chat--redisplay-node)
                  (lambda (node)
                    (push (qq-chat--message-anchor (ewoc--node-data node))
                          redisplayed))))
         (qq-chat--request-node-redisplay '("m1" "m2"))
         (should (equal '("m1" "m2") (nreverse redisplayed))))))))

(ert-deftest qq-chat-first-unread-anchor-uses-session-unread-count ()
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
         (should (equal "m2" (qq-chat--first-unread-anchor messages)))
         (qq-state-clear-session-unread "private:10001")
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
      (unread-count . 1))
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
       (let ((ctx (gethash "m2" qq-chat--render-context-by-anchor)))
         (should (plist-get ctx :insert-unread)))
       (qq-state-clear-session-unread "private:10001")
       (qq-chat--apply-read-state-change-partially)
       (should-not (plist-get (gethash "m2" qq-chat--render-context-by-anchor)
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
       (should (disco-chatbuf-input-has-objects-p))
       (let ((segments (qq-chat--current-input-segments)))
         (should (= 1 (length segments)))
         (should (equal "image" (alist-get 'type (car segments))))
         (should (file-readable-p
                  (alist-get 'file (alist-get 'data (car segments))))))))))

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
       (should (disco-chatbuf-input-has-objects-p))
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
          (qq-media--custom-faces (list face)))
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
             (should (disco-chatbuf-input-has-objects-p))
             (should (equal '("file")
                            (mapcar (lambda (segment) (alist-get 'type segment))
                                    (qq-chat--current-input-segments))))
             (qq-chat--set-pending-reply
              '((server-id . "42")
                (raw-message . "source")
                (segments . (((type . "text")
                              (data . ((text . "source"))))))))
             (cl-letf (((symbol-function 'qq-api-send-message)
                        (lambda (session-key segments &optional raw-message)
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
       (cl-letf (((symbol-function 'qq-api-fetch-history)
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
     (cl-letf (((symbol-function 'qq-api-fetch-history)
                (lambda (_session-key &optional _before callback _errback)
                  (when callback
                    (funcall callback
                             (list :added-count 0 :message-count 1))))))
       (qq-chat-load-older-messages)
       (should qq-chat--history-exhausted)
       (should-not qq-chat--history-loading))
     ;; Guard when exhausted.
     (cl-letf (((symbol-function 'qq-api-fetch-history)
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
     (let (around-called load-older-called oneside-called)
       (cl-letf (((symbol-function 'qq-api-fetch-history-around)
                  (lambda (session-key message-id callback &optional _errback _count)
                    (setq around-called message-id)
                    (should (equal session-key "private:10001"))
                    (qq-state-merge-history
                     "private:10001"
                     (list
                      (list (cons (quote message_id) "100")
                            (cons (quote message_type) "private")
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
                 ((symbol-function 'qq-api-fetch-history)
                  (lambda (&rest _)
                    (setq oneside-called t)
                    (error "around succeeded; one-side seek should not run")))
                 ((symbol-function 'qq-chat-load-older-messages)
                  (lambda ()
                    (setq load-older-called t)
                    (error "jump must not call load-older")))
                 ((symbol-function 'qq-api-get-msg)
                  (lambda (&rest _)
                    (error "around succeeded; get_msg should not run"))))
         (qq-chat-goto-message "100" 'no-pop)
         (should (equal around-called "100"))
         (should-not load-older-called)
         (should-not oneside-called)
         (should (equal "100"
                        (get-text-property (point) 'qq-chat-message-anchor)))
         (should-not qq-chat--pending-jump-id))))))


(ert-deftest qq-chat-input-segments-keep-cjk-text-after-image-object ()
  "Image object must not swallow following Chinese when sending.

Regression: object text-properties were rear-sticky, so CJK typed after an
attachment inherited `disco-chatbuf-input-object' and was dropped on parse."
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
             (goto-char (or (qq-chat--input-logical-end-position) (point-max)))
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
                  ((symbol-function 'disco-media-insert-image-slices)
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
          (let ((context (disco-media-card-context-at-point)))
            (should (equal (plist-get context :payload) segment))
            (should (functionp (plist-get context :download-action)))
            (should (functionp (plist-get context :save-as-action)))
            (should (functionp (plist-get context :copy-url-action)))
            (disco-media-card-call-action 'open context))
          (should (equal opened-segment segment)))))))

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
                  (plist-get (disco-media-card-context-at-point) :payload))))))))

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
              ((symbol-function 'qq-chat--chat-update) #'ignore))
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
      (cl-letf (((symbol-function 'qq-chat--point-in-input-p) (lambda () nil))
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
                              (first_unread_message_id . "m-first")
                              (position_available . t)))))
                ((symbol-function 'qq-api-fetch-history-around)
                 (lambda (session-key message-id callback &optional _errback count)
                   (setq around-call (list session-key message-id count))
                   (funcall callback '(:batch-newest-message-id "m-new"))))
                ((symbol-function 'qq-chat--complete-initial-history-load)
                 (lambda (&rest args) (setq completed args))))
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (should (equal around-call '("group:20001" "m-first" 40)))
        (should (equal (nth 2 completed) "m-first"))))))

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
