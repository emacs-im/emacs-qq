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

(provide 'qq-chat-test)

;;; qq-chat-test.el ends here
