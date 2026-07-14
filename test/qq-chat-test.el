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

(defun qq-chat-test--search-result (id sequence time &optional preview)
  "Return one strict group search result."
  `((chat . ((kind . "group") (group_id . "20001")))
    (message_id . ,id)
    (message_seq . ,sequence)
    (sent_at . ,time)
    (sender . ((user_id . "10001") (name . "Alice")))
    (preview . ,(or preview "needle"))))

(defun qq-chat-test--canonical-message (id time text &optional order)
  "Return one canonical group message for filter projection tests."
  `((id . ,id)
    (server-id . ,id)
    (session-key . "group:20001")
    (time . ,time)
    (message-seq . ,id)
    (sender-id . "10001")
    (sender-name . "Alice")
    (self-p . nil)
    (status . received)
    (segments . (((type . "text") (data . ((text . ,text))))))
    (raw-message . ,text)
    (preview . ,text)
    (message-type . "group")
    (group-id . "20001")
    (order . ,(or order time))))

(defun qq-chat-test--filter-snapshot (id sequence time text &optional reactions)
  "Return one flat rendering snapshot wire result for chat filter tests."
  `((chat . ((kind . "group") (group_id . "20001")))
    (message_id . ,id)
    (message_seq . ,sequence)
    (sent_at . ,time)
    (sender . ((user_id . "10001") (name . "Alice")))
    (outgoing . :false)
    (state . "live")
    (segments . (((kind . "text")
                  (payload . ((text . ,text))))))
    (reactions . ,(copy-tree reactions))))

(defun qq-chat-test--filter-item (id time text)
  "Return one filter-owned local item."
  (list :message-id id
        :message (qq-chat-test--canonical-message id time text)))

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

(ert-deftest qq-chat-frontier-observation-ignores-ordinary-message-updates ()
  (qq-chat-test-with-reset
   (let ((cached-tail "9007199254742007088")
         (remote-frontier "9007199254742007099"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,cached-tail) (time . 1)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (setq qq-chat--remote-latest-id remote-frontier)
       ;; Recall/reaction notices update an existing cached tail.  Being the
       ;; newest loaded row does not make it the remote live frontier.
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation update
          :source notice
          :message-anchor ,cached-tail
          :message ((server-id . ,cached-tail) (time . 1))))
       (should (equal qq-chat--remote-latest-id
                      remote-frontier))))))

(ert-deftest qq-chat-frontier-observation-accepts-create-and-pending-promotion ()
  (qq-chat-test-with-reset
   (let ((live-id "9007199254742007090")
         (promoted-id "9007199254742007091"))
     (qq-state-upsert-session
     "group:20001"
     '((title . "Group") (target-id . "20001") (type . group))
     nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       ;; A local pending row may sort after the newest server row.  It must
       ;; not hide a real live create from frontier observation.
       (puthash
        "group:20001"
        `(((server-id . ,live-id) (time . 2))
          ((local-id . "local-tail") (time . 3) (status . pending)))
        qq-state--messages-by-session)
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation create
          :source event
          :message-anchor ,live-id
          :message ((server-id . ,live-id) (time . 2))))
       (should (equal qq-chat--remote-latest-id live-id))

       ;; A send response/event may introduce the same remote row by rekeying
       ;; its exact pending local anchor rather than by a create mutation.
       (puthash
        "group:20001"
        `(((server-id . ,promoted-id)
           (local-id . "local-promoted")
           (time . 4)))
        qq-state--messages-by-session)
       (setq qq-chat--remote-latest-id live-id)
       (qq-chat--observe-message-frontier
        `(:type message
          :session-key "group:20001"
          :mutation update
          :source response
          :previous-anchor "local-promoted"
          :message-anchor ,promoted-id
          :message ((server-id . ,promoted-id)
                    (local-id . "local-promoted")
                    (time . 4))))
       (should (equal qq-chat--remote-latest-id
                      promoted-id))))))

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
             (should (eq (key-binding (kbd "C-c /") t)
                         'qq-chat-filter))
             (should (eq (key-binding (kbd "C-c M-/") t)
                         'qq-chat-search-results))
             (should (eq (key-binding (kbd "C-c C-r") t)
                         'qq-chat-search))
             (should (eq (key-binding (kbd "C-c C-s") t)
                         'qq-chat-search-forward))
             (should (eq (key-binding (kbd "M-g s") t)
                         'qq-chat-inplace-search))
             (should (eq (key-binding (kbd "M-g n") t)
                         'qq-chat-search-next))
             (should (eq (key-binding (kbd "M-g p") t)
                         'qq-chat-search-prev))
             (should (eq (key-binding (kbd "C-c C-c") t)
                         'qq-chat-filter-cancel))
             (should (eq (key-binding (kbd "C-c RET") t)
                         'qq-chat-send-message))
             (should (eq (key-binding (kbd "C-c m") t) 'qq-chat-message-transient))
             (should (eq (key-binding (kbd "C-c ?") t) 'qq-chat-transient))
             (should (eq (key-binding (kbd "C-c P") t)
                         'qq-chat-send-poke))
             (should (eq (key-binding (kbd "C-c C-a") t) 'qq-chat-attach-transient))
             (should (eq (key-binding (kbd "C-c C-v") t) 'qq-chat-attach-clipboard)))
         (when (buffer-live-p buffer)
           (kill-buffer buffer)))))))

(ert-deftest qq-chat-history-navigation-keybindings-follow-telega-contract ()
  (with-temp-buffer
    (qq-chat-mode)
    (should (eq (key-binding (kbd "M-g >") t) 'qq-chat-read-all))
    (should (eq (key-binding (kbd "M-g r") t) 'qq-chat-read-all))
    (should (eq (key-binding (kbd "M-g x") t)
                'qq-chat-goto-pop-message))
    ;; Paging is automatic near either edge.  Keep the ordinary Emacs
    ;; beginning/end commands available instead of overloading them.
    (should-not (lookup-key qq-chat-mode-map (kbd "M-<")))
    (should-not (lookup-key qq-chat-mode-map (kbd "M->")))
    (should (eq (key-binding (kbd "M-<") t) 'beginning-of-buffer))
    (should (eq (key-binding (kbd "M->") t) 'end-of-buffer))))

(ert-deftest qq-chat-filter-reader-is-require-match-and-dispatches-command ()
  (let (reader-args dispatched)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq reader-args args)
                 "hashtag"))
              ((symbol-function 'call-interactively)
               (lambda (command &optional _record _keys)
                 (setq dispatched command))))
      (should (eq (qq-chat--read-filter-command)
                  'qq-chat-filter-hashtag))
      (should (eq (nth 3 reader-args) t))
      (qq-chat-filter 'qq-chat-filter-search)
      (should (eq dispatched 'qq-chat-filter-search)))))

(ert-deftest qq-chat-inplace-search-reader-is-require-match-and-dispatches-command ()
  (let (reader-args dispatched)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest args)
                 (setq reader-args args)
                 "query"))
              ((symbol-function 'call-interactively)
               (lambda (command &optional _record _keys)
                 (setq dispatched command))))
      (should (eq (qq-chat--read-inplace-search-command) 'qq-chat-search))
      (should (eq (nth 3 reader-args) t))
      (qq-chat-inplace-search 'qq-chat-search)
      (should (eq dispatched 'qq-chat-search)))))

(ert-deftest qq-chat-filter-hashtag-keeps-its-own-filter-title ()
  (with-temp-buffer
    (qq-chat-mode)
    (let (filter)
      (cl-letf (((symbol-function 'qq-chat--run-filter)
                 (lambda (candidate &optional _append)
                   (setq filter candidate))))
        (qq-chat-filter-hashtag "  topic  ")
        (should (equal filter
                       '(:title "hashtag #topic" :query "#topic")))
        (should-error (qq-chat-filter-hashtag "  ") :type 'user-error)))))

(ert-deftest qq-chat-filter-projection-bypasses-normal-window-with-exact-ids ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (puthash
    "group:20001"
    (list (qq-chat-test--canonical-message "10" 10 "canonical stale")
          (qq-chat-test--canonical-message "20" 20 "normal only")
          (qq-chat-test--canonical-message "30" 30 "canonical stale"))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t
                 :title "search \"match\""
                 :query "match"
                 ;; Native result order is newest-first.
                 :items (list (qq-chat-test--filter-item
                               "30" 30 "new match")
                              (qq-chat-test--filter-item
                               "10" 10 "old match"))))
     (qq-chat--set-history-window "20" nil)
     (should
      (equal (mapcar #'qq-chat--message-anchor
                     (qq-chat--timeline-messages))
             '("10" "30")))
     (qq-chat-render)
     (should (qq-chat--goto-loaded-message "30" nil))
     (let ((filter-message
            (plist-get (car (plist-get qq-chat--msg-filter :items)) :message)))
       (cl-letf (((symbol-function 'qq-state-session-messages)
                  (lambda (_session-key)
                    (error "canonical history must not back a filter"))))
         (should (eq (qq-chat--message-by-server-id "30") filter-message))
         (should (eq (qq-chat--message-at-point) filter-message))
         (should (equal (alist-get 'raw-message
                                   (car (last (qq-chat--timeline-messages))))
                        "new match"))))
     (should (equal (appkit-chat-history-window-first-key) "20")))))

(ert-deftest qq-chat-filter-invalidates-normal-history-owner-and-transports ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "20" nil)
     (let* ((initial-owner (list 'initial))
            (open-owner (list 'open))
            (normal-owner (appkit-chat-history-request-begin 'newer))
            canceled)
       (setq qq-chat--initial-history-owner initial-owner
             qq-chat--initial-history-request 'initial-token
             qq-chat--open-message-owner open-owner
             qq-chat--open-message-request 'open-token)
       (cl-letf (((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (push token canceled)))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query _callback &optional _errback _limit)
                    'filter-token)))
         (qq-chat-filter-search "needle"))
       (should (equal (sort canceled
                            (lambda (left right)
                              (string< (symbol-name left) (symbol-name right))))
                      '(initial-token open-token)))
       (should-not (appkit-chat-history-request-current-p normal-owner))
       (should-not qq-chat--initial-history-owner)
       (should-not qq-chat--open-message-owner)
       (should (equal (appkit-chat-history-window-first-key) "20"))))))

(ert-deftest qq-chat-filter-barrier-rejects-late-older-history-callback ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (puthash
    "group:20001"
    (list (qq-chat-test--canonical-message "10" 10 "older")
          (qq-chat-test--canonical-message "20" 20 "visible"))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "20" nil)
     (let (older-callback)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (_session _before callback &optional _errback _count)
                    (setq older-callback callback)
                    'older-token))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query _callback &optional _errback _limit)
                    'filter-token)))
         (qq-chat-load-older-messages)
         (should (appkit-chat-history-loading-p))
         (qq-chat-filter-search "needle")
         (funcall older-callback
                  '(:batch-message-ids ("10")
                    :batch-oldest-message-id "10"
                    :batch-newest-message-id "10"
                    :added-count 1)))
       (should (equal (appkit-chat-history-window-first-key) "20"))))))

(ert-deftest qq-chat-filter-first-page-does-not-reclaim-moved-point ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (puthash
    "group:20001"
    (list (qq-chat-test--canonical-message "20" 20 "needle"))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "20" nil)
     (qq-chat-render)
     (qq-chat--goto-loaded-message "20" nil)
     (let (callback positioned)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query success &optional _errback _limit)
                    (setq callback success)
                    'filter-token))
                 ((symbol-function 'qq-chat--position-after-filter-first-page)
                  (lambda (_owner) (setq positioned t))))
         (qq-chat-filter-search "needle")
         (if (appkit-chatbuf-point-in-input-p)
             (goto-char (point-min))
           (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))
         (funcall callback
                  `((projection . "message")
                    (results . (,(qq-chat-test--filter-snapshot
                                  "20" "20" 20 "needle")))
                    (next_cursor))))
       (should-not positioned)))))

(ert-deftest qq-chat-filter-refresh-restores-point-while-dispatch-owns-it ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t :title "search \"needle\"" :query "needle"
                 :items (list (qq-chat-test--filter-item "20" 20 "needle"))))
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (callback positioned)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query success &optional _errback _limit)
                    (setq callback success)
                    'filter-token))
                 ((symbol-function 'qq-chat--position-after-filter-first-page)
                  (lambda (_owner) (setq positioned t))))
         (qq-chat-filter-refresh)
         (funcall callback
                  `((projection . "message")
                    (results . (,(qq-chat-test--filter-snapshot
                                  "20" "20" 20 "needle")))
                    (next_cursor))))
       (should positioned)))))

(ert-deftest qq-chat-filter-materializes-pages-and-preserves-normal-window ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (puthash
    "group:20001"
    (list (qq-chat-test--canonical-message "20" 20 "normal"))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "20" nil)
     (qq-chat-render)
     (let (start-callback next-callback canceled)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (session query callback &optional _errback _limit)
                    (should (equal session "group:20001"))
                    (should (equal query "needle"))
                    (setq start-callback callback)
                    'filter-start-token))
                 ((symbol-function 'qq-api-filter-messages-next)
                  (lambda (session cursor callback &optional _errback)
                    (should (equal session "group:20001"))
                    (should (equal cursor "cursor-1"))
                    (setq next-callback callback)
                    'filter-next-token))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (push token canceled))))
         (qq-chat-filter-search "needle")
         (should (qq-chat--msg-filter-active-p))
         (should (eq qq-chat--filter-request 'filter-start-token))
         (should (equal (appkit-chat-history-window-first-key) "20"))

         (funcall
          start-callback
          `((projection . "message")
            (results . (,(qq-chat-test--filter-snapshot
                          "30" "30" 30 "new needle")))
            (next_cursor . "cursor-1")))
         (should-not qq-chat--filter-owner)
         (should (qq-chat--msg-filter-has-more-p))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 '("30")))
         (should (qq-chat--goto-loaded-message "30" nil))
         (should (equal (alist-get 'server-id (qq-chat--message-at-point))
                        "30"))

         (qq-chat-filter-load-more)
         (should (eq qq-chat--filter-request 'filter-next-token))
         ;; Cursor was consumed before dispatch and cannot be replayed.
         (should-not (plist-get qq-chat--msg-filter :next-cursor))
         (funcall
          next-callback
          `((projection . "message")
            (results . (,(qq-chat-test--filter-snapshot
                          "10" "10" 10 "old needle")))
            (next_cursor)))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 '("10" "30")))
         ;; Prepending an older page keeps semantic point on the existing row;
         ;; it must never jump to the composer.
         (should (equal (alist-get 'server-id (qq-chat--message-at-point))
                        "30"))
         (should-not (appkit-chatbuf-point-in-input-p))

         (goto-char (or (appkit-chatbuf-input-start-position) (point-max)))
         (qq-chat-filter-cancel)
         (should-not qq-chat--msg-filter)
         (should (equal (appkit-chat-history-window-first-key) "20"))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 '("20")))
         (should-not canceled))))))

(ert-deftest qq-chat-filter-cancel-rejects-late-page-callback ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (late-callback canceled)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (setq late-callback callback)
                    'late-filter-token))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (setq canceled token))))
         (qq-chat-filter-search "needle")
         (qq-chat-filter-cancel)
         (should (eq canceled 'late-filter-token))
         (funcall
          late-callback
          `((projection . "message")
            (results . (,(qq-chat-test--filter-snapshot
                          "99" "99" 99 "late needle")))
            (next_cursor)))
         (should-not qq-chat--msg-filter)
         (should-not qq-chat--filter-owner)
         (should (appkit-chat-history-window-empty-p)))))))

(ert-deftest qq-chat-filter-follows-empty-pages-with-live-cursors ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (continued)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (funcall callback
                             '((projection . "message")
                               (results)
                               (next_cursor . "empty-1")))
                    'start-token))
                 ((symbol-function 'qq-api-filter-messages-next)
                  (lambda (_session cursor callback &optional _errback)
                    (push cursor continued)
                    (funcall
                     callback
                     (if (equal cursor "empty-1")
                         '((projection . "message")
                           (results)
                           (next_cursor . "empty-2"))
                       `((projection . "message")
                         (results . (,(qq-chat-test--filter-snapshot
                                       "90" "90" 90 "needle")))
                         (next_cursor))))
                    (intern (concat "token-" cursor)))))
         (qq-chat-filter-search "needle")
         (should (equal (nreverse continued) '("empty-1" "empty-2")))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 '("90")))
         (should-not qq-chat--filter-owner))))))

(ert-deftest qq-chat-filter-owned-snapshots-accept-exact-message-patches ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t
                 :title "search \"needle\""
                 :query "needle"
                 :items (list (qq-chat-test--filter-item
                               "90" 90 "needle"))))
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (qq-chat--handle-state-change
      (list :type 'message
            :session-key "group:20001"
            :message-anchor "90"
            :mutation 'update
            :source 'notice
            :observation-token 1
            :message-patch
            (list :kind 'emoji-like
                  :observation-token 1
                  :notice
                  '((message_id . "90")
                    (group_id . "20001")
                    (user_id . "10002")
                    (is_add . t)
                    (likes . (((emoji_id . "178") (count . 2))))))))
     (qq-chat-test-sync-invalidations)
     (let* ((item (car (plist-get qq-chat--msg-filter :items)))
            (message (plist-get item :message))
            (reaction (car (qq-state-message-reactions message))))
       (should (equal (alist-get 'emoji-id reaction) "178"))
       (should (= (alist-get 'count reaction) 2)))
     (qq-chat--handle-state-change
      '(:type message
        :session-key "group:20001"
        :message-anchor "90"
        :mutation update
        :source notice
        :observation-token 2
        :message-patch (:kind recall :observation-token 2)))
     (qq-chat-test-sync-invalidations)
     (should (qq-state-message-recalled-p
              (plist-get (car (plist-get qq-chat--msg-filter :items))
                         :message)))
     (should-not (qq-state-session-messages "group:20001"))
     (should (equal (appkit-chat-timeline-keys)
                    (list qq-chat--empty-placeholder))))))

(ert-deftest qq-chat-filter-replays-only-in-flight-patches-on-new-hit ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (callback owner)
       (cl-letf (((symbol-function 'qq-state-message-observation-token)
                  (lambda () 10))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query success &optional _errback _limit)
                    (setq callback success)
                    'filter-token)))
         (qq-chat-filter-search "needle")
         (setq owner qq-chat--filter-owner)
         (qq-chat--apply-filter-message-patch
          "90"
          (list :kind 'emoji-like
                :observation-token 11
                :notice
                '((message_id . "90")
                  (group_id . "20001")
                  (user_id . "10002")
                  (is_add . t)
                  (likes . (((emoji_id . "178") (count . 2))))))
          11)
         (qq-chat--apply-filter-message-patch
          "90" '(:kind recall :observation-token 12) 12)
         (should (= (length (plist-get owner :patches)) 2))
         (funcall callback
                  `((projection . "message")
                    (results . (,(qq-chat-test--filter-snapshot
                                  "90" "90" 90 "needle")))
                    (next_cursor))))
       (let* ((item (car (plist-get qq-chat--msg-filter :items)))
              (message (plist-get item :message))
              (reaction (car (qq-state-message-reactions message))))
         (should (qq-state-message-recalled-p message))
         (should (equal (alist-get 'emoji-id reaction) "178"))
         (should (= (alist-get 'count reaction) 2))
         (should-not (plist-get owner :patches)))))))

(ert-deftest qq-chat-filter-cancel-discards-request-local-patches ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (owner canceled)
       (cl-letf (((symbol-function 'qq-state-message-observation-token)
                  (lambda () 10))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (&rest _) 'filter-token))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (request) (setq canceled request))))
         (qq-chat-filter-search "needle")
         (setq owner qq-chat--filter-owner)
         (qq-chat--apply-filter-message-patch
          "90" '(:kind recall :observation-token 11) 11)
         (should (plist-get owner :patches))
         (qq-chat--cancel-filter-request)
         (should (eq canceled 'filter-token))
         (should-not (plist-get owner :pending))
         (should-not (plist-get owner :patches))
         (should-not qq-chat--filter-request)
         (should-not qq-chat--filter-owner))))))

(ert-deftest qq-chat-filter-failure-discards-request-local-patches ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (owner errback)
       (cl-letf (((symbol-function 'qq-state-message-observation-token)
                  (lambda () 10))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query _success failure &optional _limit)
                    (setq errback failure)
                    'filter-token)))
         (qq-chat-filter-search "needle")
         (setq owner qq-chat--filter-owner)
         (qq-chat--apply-filter-message-patch
          "90" '(:kind recall :observation-token 11) 11)
         (should (plist-get owner :patches))
         (funcall errback nil "failed")
         (should-not (plist-get owner :pending))
         (should-not (plist-get owner :patches))
         (should-not qq-chat--filter-request)
         (should-not qq-chat--filter-owner))))))

(ert-deftest qq-chat-filter-does-not-replay-reaction-observed-before-request ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t :title "search \"needle\"" :query "needle"
                 :items (list (qq-chat-test--filter-item "90" 90 "needle"))))
     ;; This delta predates the next request.  Its future response already
     ;; reports count 3, so replaying the old +1 would incorrectly produce 4.
     (qq-chat--apply-filter-message-patch
      "90"
      '(:kind emoji-like
        :observation-token 5
        :notice ((message_id . "90")
                 (group_id . "20001")
                 (user_id . "10002")
                 (is_add . t)
                 (likes . (((emoji_id . "178"))))))
      5)
     (let (callback)
       (cl-letf (((symbol-function 'qq-state-message-observation-token)
                  (lambda () 6))
                 ((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query success &optional _errback _limit)
                    (setq callback success)
                    'filter-token)))
         (qq-chat-filter-refresh)
         (funcall
          callback
          `((projection . "message")
            (results
             . (,(qq-chat-test--filter-snapshot
                  "90" "90" 90 "needle"
                  '(((emoji_id . "178")
                     (emoji_type . "1")
                     (count . 3)
                     (chosen . :false))))))
            (next_cursor))))
       (let* ((item (car (plist-get qq-chat--msg-filter :items)))
              (reaction (car (qq-state-message-reactions
                              (plist-get item :message)))))
         (should (= (alist-get 'count reaction) 3)))))))

(ert-deftest qq-chat-filter-applies-recall-observed-before-request ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (let ((message-id "9007199254741004881"))
     ;; Recall is a permanent state tombstone, unlike a reaction observation.
     (qq-state-apply-recall "group:20001" message-id)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--set-empty-history-window)
       (qq-chat-render)
       (let (callback)
         (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                    (lambda (_session _query success &optional _errback _limit)
                      (setq callback success)
                      'filter-token)))
           (qq-chat-filter-search "needle")
           (funcall
            callback
            `((projection . "message")
              (results
               . (,(qq-chat-test--filter-snapshot
                    message-id "90" 90 "stale live result")))
              (next_cursor))))
         (should
          (qq-state-message-recalled-p
           (plist-get (car (plist-get qq-chat--msg-filter :items))
                      :message))))))))

(ert-deftest qq-chat-filter-append-keeps-in-flight-patch-on-existing-item ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t :title "search \"needle\"" :query "needle"
                 :items (list (qq-chat-test--filter-item "90" 90 "needle"))
                 :next-cursor "cursor"))
     (let (callback owner)
       (cl-letf (((symbol-function 'qq-state-message-observation-token)
                  (lambda () 10))
                 ((symbol-function 'qq-api-filter-messages-next)
                  (lambda (_session _cursor success &optional _errback)
                    (setq callback success)
                    'filter-token)))
         (qq-chat-filter-load-more)
         (setq owner qq-chat--filter-owner)
         (qq-chat--handle-state-change
          '(:type message
            :session-key "group:20001"
            :message-anchor "90"
            :mutation update
            :source notice
            :observation-token 11
            :message-patch
            (:kind emoji-like
             :observation-token 11
             :notice ((message_id . "90")
                      (group_id . "20001")
                      (user_id . "10002")
                      (is_add . t)
                      (likes . (((emoji_id . "178"))))))))
         ;; Accept the filter response before AppKit flushes the queued state
         ;; event.  Eager filter observation must already have repaired both
         ;; the visible item and OWNER's captured `:existing' snapshot.
         (funcall
          callback
          `((projection . "message")
            (results . (,(qq-chat-test--filter-snapshot
                          "80" "80" 80 "older needle")))
            (next_cursor))))
       (let* ((item (seq-find
                     (lambda (candidate)
                       (equal (qq-chat--filter-result-id candidate) "90"))
                     (plist-get qq-chat--msg-filter :items)))
              (reaction (car (qq-state-message-reactions
                              (plist-get item :message)))))
         (should (= (alist-get 'count reaction) 1))
         (should-not (plist-get owner :patches)))))))

(ert-deftest qq-chat-reset-invalidates-filter-owner-before-late-callback ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (let (late-callback canceled)
       (cl-letf (((symbol-function 'qq-api-filter-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (setq late-callback callback)
                    'pre-reset-token))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (setq canceled token))))
         (qq-chat-filter-search "needle")
         (qq-chat--handle-state-change '(:type reset))
         (should (eq canceled 'pre-reset-token))
         (should-not qq-chat--msg-filter)
         (should-not qq-chat--filter-owner)
         (funcall
          late-callback
          `((projection . "message")
            (results . (,(qq-chat-test--filter-snapshot
                          "99" "99" 99 "stale")))
            (next_cursor)))
         (should-not qq-chat--msg-filter))))))

(ert-deftest qq-chat-filter-and-inplace-search-are-explicitly-exclusive ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001"
          qq-chat--msg-filter
          '(:active t :title "search \"one\"" :query "one" :items nil))
    (should-error (qq-chat-search "two") :type 'user-error)
    (should-error (qq-chat-search-next) :type 'user-error)
    (should-error (qq-chat-search-prev) :type 'user-error)))

(ert-deftest qq-chat-search-cancel-clears-owned-search-state ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq-local qq-chat--session-key "private:10001"
                 qq-chat--last-search-query "needle"
                 qq-chat--search-results '(((message_id . "11")))
                 qq-chat--search-results-tail qq-chat--search-results
                 qq-chat--search-index 0
                 qq-chat--search-next-cursor "opaque-cursor"
                 qq-chat--search-completed-p t)
     (let ((canceled nil))
       (cl-letf (((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (setq canceled token))))
         (setq-local qq-chat--search-request 'request-token
                     qq-chat--search-owner 'owner)
         (qq-chat-search-cancel))
       (should (eq canceled 'request-token)))
     (should-not qq-chat--last-search-query)
     (should-not qq-chat--search-results)
     (should-not qq-chat--search-results-tail)
     (should-not qq-chat--search-index)
     (should-not qq-chat--search-next-cursor)
     (should-not qq-chat--search-completed-p)
     (should-not qq-chat--search-request)
     (should-not qq-chat--search-owner))))

(ert-deftest qq-chat-filter-cancel-only-clears-inplace-search-highlights ()
  (with-temp-buffer
    (qq-chat-mode)
    (let ((overlay (make-overlay (point-min) (point-min))))
      (setq qq-chat--last-search-query "needle"
            qq-chat--search-results '(((message_id . "11")))
            qq-chat--search-results-tail qq-chat--search-results
            qq-chat--search-index 0
            qq-chat--search-next-cursor "cursor"
            qq-chat--search-highlight-overlays (list overlay))
    (qq-chat-filter-cancel)
      (should-not (overlay-buffer overlay))
      (should-not qq-chat--search-highlight-overlays)
      (should (equal qq-chat--last-search-query "needle"))
      (should (equal qq-chat--search-results '(((message_id . "11")))))
      (should (= qq-chat--search-index 0))
      (should (equal qq-chat--search-next-cursor "cursor")))))

(ert-deftest qq-chat-search-selects-nearest-result-by-exact-sequence ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((origin '((server-id . "8000")
                      (time . 100)
                      (message-seq . "250")))
            (results (list (qq-chat-test--search-result "1000" "300" 100)
                           (qq-chat-test--search-result "9000" "200" 100)
                           (qq-chat-test--search-result "7000" "100" 100)))
            opened)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) origin))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (funcall callback `((results . ,results) (next_cursor)))
                    'request))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "needle")
         (should (equal opened "9000"))
         (should (= qq-chat--search-index 1))
         (setq opened nil)
         (qq-chat-search "needle" t)
         (should (equal opened "1000"))
         (should (= qq-chat--search-index 0)))))))

(ert-deftest qq-chat-search-same-second-orders-by-string-sequence-not-id ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((origin '((server-id . "500")
                      (time . 100)
                      (message-seq . "90071992547409931235")))
            (results
             (list
              (qq-chat-test--search-result
               "100" "90071992547409931236" 100)
              (qq-chat-test--search-result
               "900" "90071992547409931234" 100)))
            opened)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) origin))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (funcall callback `((results . ,results) (next_cursor)))
                    'request))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "needle")
         (should (equal opened "900"))
         (qq-chat-search "needle" t)
         (should (equal opened "100")))))))

(ert-deftest qq-chat-search-exact-anchor-id-skips-the-anchor-itself ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((origin '((server-id . "500")
                      (time . 100)
                      (message-seq . "20")))
            (results
             (list (qq-chat-test--search-result "100" "21" 100)
                   (qq-chat-test--search-result "500" "20" 100)
                   (qq-chat-test--search-result "900" "19" 100)))
            opened)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) origin))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (funcall callback `((results . ,results) (next_cursor)))
                    'request))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "needle")
         (should (equal opened "900"))
         (qq-chat-search "needle" t)
         (should (equal opened "100")))))))

(ert-deftest qq-chat-search-follows-empty-pages-without-restarting ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((start-calls 0) (next-cursors nil) opened)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) nil))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (cl-incf start-calls)
                    (funcall callback
                             '((results) (next_cursor . "cursor-1")))
                    'start))
                 ((symbol-function 'qq-api-search-messages-next)
                  (lambda (session cursor projection callback &optional _errback)
                    (should (equal session "group:20001"))
                    (should (eq projection 'summary))
                    (setq next-cursors (append next-cursors (list cursor)))
                    (funcall
                     callback
                     (if (equal cursor "cursor-1")
                         '((results) (next_cursor . "cursor-2"))
                       `((results . (,(qq-chat-test--search-result
                                      "123" "10" 10)))
                         (next_cursor))))
                    'next))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "needle")
         (should (= start-calls 1))
         (should (equal next-cursors '("cursor-1" "cursor-2")))
         (should (equal opened "123"))
         (should-not qq-chat--search-owner))))))

(ert-deftest qq-chat-search-continues-past-eight-empty-or-duplicate-pages ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let* ((origin '((server-id . "500")
                      (time . 100)
                      (message-seq . "100")))
            (duplicate (qq-chat-test--search-result "501" "101" 101))
            (target (qq-chat-test--search-result "499" "99" 99))
            (expected-cursors
             (mapcar (lambda (number) (format "cursor-%d" number))
                     (number-sequence 1 10)))
            (start-calls 0)
            next-cursors
            opened)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) origin))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (cl-incf start-calls)
                    (funcall callback
                             `((results . (,duplicate))
                               (next_cursor . "cursor-1")))
                    'start-token))
                 ((symbol-function 'qq-api-search-messages-next)
                  (lambda (session cursor projection callback &optional _errback)
                    (should (equal session "group:20001"))
                    (should (eq projection 'summary))
                    (setq next-cursors
                          (append next-cursors (list cursor)))
                    (let ((page-number (length next-cursors)))
                      (funcall
                       callback
                       (if (= page-number 10)
                           `((results . (,target)) (next_cursor))
                         `((results . ,(if (cl-oddp page-number)
                                           nil
                                         (list duplicate)))
                           (next_cursor
                            . ,(format "cursor-%d" (1+ page-number)))))))
                    (intern (format "next-token-%d" (length next-cursors)))))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "needle")
         (should (= start-calls 1))
         (should (equal next-cursors expected-cursors))
         (should (equal opened "499"))
         (should (equal (mapcar (lambda (result)
                                 (alist-get 'message_id result))
                               qq-chat--search-results)
                        '("501" "499")))
         (should qq-chat--search-completed-p)
         (should-not qq-chat--search-owner))))))

(ert-deftest qq-chat-search-consumes-cursor-before-next-failure ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--last-search-query "needle"
           qq-chat--search-results
           (list (qq-chat-test--search-result "101" "10" 10))
           qq-chat--search-results-tail qq-chat--search-results
           qq-chat--search-seen (make-hash-table :test #'equal)
           qq-chat--search-index 0
           qq-chat--search-next-cursor "single-use")
     (let ((next-calls 0))
       (cl-letf (((symbol-function 'qq-api-search-messages-next)
                  (lambda (session _cursor projection _callback &optional errback)
                    (should (equal session "group:20001"))
                    (should (eq projection 'summary))
                    (cl-incf next-calls)
                    (funcall errback nil "network")
                    'request)))
         (qq-chat-search-prev)
         (should (= next-calls 1))
         (should-not qq-chat--search-next-cursor)
         (should-not qq-chat--search-owner)
         (qq-chat-search-prev)
         (should (= next-calls 1)))))))

(ert-deftest qq-chat-search-cancels-and-ignores-stale-owner ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let (callbacks canceled opened (counter 0))
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) nil))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (cl-incf counter)
                    (setq callbacks (append callbacks (list callback)))
                    (intern (format "request-%d" counter))))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (setq canceled (append canceled (list token)))))
                 ((symbol-function 'qq-chat-open-message)
                  (lambda (_session id &optional _query) (setq opened id))))
         (qq-chat-search "first")
         (qq-chat-search "second")
         (should (equal canceled '(request-1)))
         (funcall (car callbacks)
                  `((results . (,(qq-chat-test--search-result
                                  "111" "20" 20)))
                    (next_cursor)))
         (should-not opened)
         (funcall (cadr callbacks)
                  `((results . (,(qq-chat-test--search-result
                                  "222" "10" 10)))
                    (next_cursor)))
         (should (equal opened "222")))))))

(ert-deftest qq-chat-open-message-cancels-pending-search-and-stale-callback ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((buffer (current-buffer))
           search-callback
           canceled
           around-message-ids)
       (cl-letf (((symbol-function 'qq-chat--message-at-point)
                  (lambda (&optional _position) nil))
                 ((symbol-function 'qq-api-search-messages-start)
                  (lambda (_session _query callback &optional _errback _limit)
                    (setq search-callback callback)
                    'search-token))
                 ((symbol-function 'qq-api-cancel-request)
                  (lambda (token) (push token canceled)))
                 ((symbol-function 'qq-chat--open-buffer)
                  (lambda (_session-key) buffer))
                 ((symbol-function 'pop-to-buffer)
                  (lambda (&rest _args) buffer))
                 ((symbol-function 'qq-chat--on-window-size-change) #'ignore)
                 ((symbol-function 'qq-chat--goto-loaded-message)
                  (lambda (&rest _args) nil))
                 ((symbol-function 'qq-api-fetch-history-around)
                  (lambda (_session-key message-id _callback
                           &optional _errback _count)
                    (setq around-message-ids
                          (append around-message-ids (list message-id)))
                    (intern (format "around-%s" message-id)))))
         (qq-chat-search "needle")
         (should (eq qq-chat--search-request 'search-token))
         (should qq-chat--search-owner)

         (qq-chat-open-message "group:20001" "999" "needle")
         (should (memq 'search-token canceled))
         (should-not qq-chat--search-request)
         (should-not qq-chat--search-owner)
         (should (eq qq-chat--open-message-request 'around-999))
         (should (equal around-message-ids '("999")))

         (funcall search-callback
                  `((results . (,(qq-chat-test--search-result
                                  "111" "20" 20)))
                    (next_cursor)))
         (should (equal around-message-ids '("999")))
         (should (eq qq-chat--open-message-request 'around-999))
         (should-not (memq 'around-999 canceled))
         (should-not qq-chat--search-results)
         (should-not qq-chat--search-index))))))

(ert-deftest qq-chat-open-message-cancels-initial-and-normal-open-cancels-around ()
  (qq-chat-test-with-reset
   (save-window-excursion
     (let ((buffer (generate-new-buffer " *qq-open-message-race*"))
           canceled around-call (initial-loads 0))
       (unwind-protect
           (progn
             (with-current-buffer buffer
               (qq-chat-mode)
               (setq qq-chat--session-key "group:20001"
                     qq-chat--initial-history-request 'initial-token
                     qq-chat--initial-history-owner '(initial)))
             (cl-letf (((symbol-function 'qq-chat--open-buffer)
                        (lambda (_session) buffer))
                       ((symbol-function 'qq-api-cancel-request)
                        (lambda (token) (push token canceled)))
                       ((symbol-function 'qq-chat--on-window-size-change)
                        #'ignore)
                       ((symbol-function 'qq-chat--goto-loaded-message)
                        (lambda (&rest _) nil))
                       ((symbol-function 'qq-api-fetch-history-around)
                        (lambda (session id _callback &optional _errback count)
                          (setq around-call (list session id count))
                          'around-token))
                       ((symbol-function 'qq-chat--load-initial-history)
                        (lambda (&rest _args) (cl-incf initial-loads))))
               (qq-chat-open-message "group:20001" "11" "needle")
               (should (memq 'initial-token canceled))
               (should (equal (car around-call) "group:20001"))
               (should (equal (cadr around-call) "11"))
               (should (zerop (or initial-loads 0)))
               (qq-chat-open "group:20001")
               (should (memq 'around-token canceled))
               (should (= initial-loads 1))))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest qq-chat-open-message-unknown-window-fetches-around-despite-cache ()
  (qq-chat-test-with-reset
   (let ((target "9007199254742007089"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,target) (time . 1) (raw-message . "cached target")))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (appkit-chat-history-window-clear)
       (let ((buffer (current-buffer))
             fast-path-called
             around-call)
         (should (qq-chat--message-by-server-id target))
         (should-not (qq-chat--history-window-known-p))
         (cl-letf (((symbol-function 'qq-chat--open-buffer)
                    (lambda (_session-key) buffer))
                   ((symbol-function 'pop-to-buffer)
                    (lambda (&rest _args) buffer))
                   ((symbol-function 'qq-chat--on-window-size-change) #'ignore)
                   ((symbol-function 'qq-chat--finish-open-message)
                    (lambda (&rest _args)
                      (setq fast-path-called t)
                      t))
                   ((symbol-function 'qq-api-fetch-history-around)
                    (lambda (session-key message-id _callback
                             &optional _errback _count)
                      (setq around-call (list session-key message-id))
                      'around-token)))
           (qq-chat-open-message "group:20001" target "cached")
           (should-not fast-path-called)
           (should (equal around-call (list "group:20001" target)))
           (should (eq qq-chat--open-message-request 'around-token))
           (should (eq (appkit-chat-history-loading) 'around))))))))

(ert-deftest qq-chat-header-line-shows-lightweight-search-state ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--last-search-query "a rather long search query"
           qq-chat--search-index 0
           qq-chat--search-next-cursor "cursor")
     (let ((header (qq-chat--header-line)))
       (should (string-match-p "search \\\"a rather long" header))
       (should (string-match-p "1+" header)))
     (setq qq-chat--search-index nil
           qq-chat--search-next-cursor nil
           qq-chat--search-completed-p t)
     (should (string-match-p " 0" (qq-chat--header-line))))))

(ert-deftest qq-chat-search-highlight-never-scans-heading-ui ()
  (with-temp-buffer
    (insert "needle sender heading\n")
    (let ((body-start (point)))
      (insert "actual needle body\n")
      (add-text-properties body-start (point) '(qq-chat-search-text t))
      (cl-letf (((symbol-function 'qq-chat--message-position)
                 (lambda (_id) (point-min)))
                ((symbol-function 'qq-chat--message-end-position)
                 (lambda (_position) (point-max))))
        (qq-chat--highlight-search-text "1" "needle")
        (should (= (length qq-chat--search-highlight-overlays) 1))
        (should (>= (overlay-start (car qq-chat--search-highlight-overlays))
                    body-start))))))

(ert-deftest qq-chat-filter-highlights-every-projected-result ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--msg-filter
           (list :active t
                 :title "search \"needle\""
                 :query "needle"
                 :items (list (qq-chat-test--filter-item
                               "20" 20 "second needle")
                              (qq-chat-test--filter-item
                               "10" 10 "first needle"))))
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (should (= (length qq-chat--search-highlight-overlays) 2))
     (should
      (equal
       (sort
        (mapcar
         (lambda (overlay)
           (get-text-property
            (overlay-start overlay) 'qq-chat-message-anchor))
         qq-chat--search-highlight-overlays)
        #'string-lessp)
       '("10" "20"))))))

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

(ert-deftest qq-chat-refuses-to-sync-timeline-text-into-canonical-input ()
  (with-temp-buffer
    (qq-chat-mode)
    (appkit-chatbuf-input-state-set "safe draft")
    (cl-letf (((symbol-function 'appkit-chat-timeline-live-p) (lambda () t))
              ((symbol-function 'appkit-chatbuf-input-start-position)
               (lambda () 2))
              ((symbol-function 'appkit-chatbuf-prompt-start-position)
               (lambda () 1))
              ((symbol-function 'appkit-chat-timeline-footer-start-position)
               (lambda () 100))
              ((symbol-function 'appkit-chatbuf-input-state-sync)
               (lambda (&rest _args)
                 (ert-fail "invalid boundary must not sync input"))))
      (let ((result (qq-chat--sync-draft-from-buffer)))
        (should (equal "safe draft" result))
        (should (equal "safe draft" (appkit-chatbuf-input-state)))))))

(ert-deftest qq-chat-refuses-canonical-input-containing-message-rows ()
  (with-temp-buffer
    (qq-chat-mode)
    (appkit-chatbuf-input-state-set
     (propertize "not a draft" 'qq-chat-message-anchor "m1"))
    (should-error (qq-chat--render-canonical-input)
                  :type 'error)))

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

(ert-deftest qq-chat-render-keeps-composer-after-timeline-footer ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice") (target-id . "10001"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "m1")
       (sender-id . "10001")
       (sender-name . "Alice")
       (time . 100)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (let ((footer (appkit-chat-timeline-footer-start-position))
           (prompt (appkit-chatbuf-prompt-start-position))
           (input (appkit-chatbuf-input-start-position)))
       (should (<= footer prompt input))
       (should (appkit-chatbuf-prompt-button-live-p))
       (should (string-suffix-p ">>> " (buffer-string)))))))

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
     (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window "m1" nil)
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
        (peer_uin . "10001")
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
       (qq-chat--set-history-window "9007199254741007777" nil)
       (qq-chat-render)
       (should (equal '("9007199254741007777")
                      (appkit-chat-timeline-keys)))
       (qq-state-apply-recall
        "private:10001" "9007199254741007777")
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
        (peer_uin . "10001")
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
       (qq-chat--set-history-window "9007199254741008888" nil)
       (qq-chat-render)
       (qq-state-apply-recall
        "private:10001" "9007199254741008888")
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
                        '(timeline header))))))))

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
     (qq-chat--set-history-window nil nil)
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

(ert-deftest qq-chat-empty-placeholder-state-forces-same-key-redisplay ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001")) nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-empty-history-window)
     (qq-chat-render)
     (goto-char (point-min))
     (should (search-forward "No messages loaded yet." nil t))
     (setq qq-chat--msg-filter '(:active t :title "search" :query "needle")
           qq-chat--filter-owner (list :pending t))
     (qq-chat--sync-timeline)
     (goto-char (point-min))
     (should (search-forward "Searching messages…" nil t))
     (setq qq-chat--filter-owner nil)
     (qq-chat--sync-timeline)
     (goto-char (point-min))
     (should (search-forward "No matching messages." nil t)))))

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
       (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window "m1" nil)
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
       (qq-chat--set-history-window nil nil)
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
     (qq-chat--set-history-window nil nil)
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
       (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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
             (qq-chat--set-history-window "m1" nil)
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
      (should (eq 'qq-msg-mention (get-text-property 0 'face other)))
      (should (equal "90001"
                     (get-text-property 0 'qq-chat-mention-user-id self)))
      (should-not (get-text-property 0 'qq-chat-mention-user-id all))
      (should (equal "10002"
                     (get-text-property 0 'qq-chat-mention-user-id other))))))

(ert-deftest qq-chat-historical-mention-opens-mentioned-user-profile ()
  (let ((message
         '((segments
            . (((type . "text") (data . ((text . "你好，"))))
               ((type . "at")
                (data . ((qq . "10001") (name . "Alice Card"))))
               ((type . "text") (data . ((text . "！"))))))))
        opened-user-id)
    (with-temp-buffer
      (qq-chat--insert-message-body message nil nil)
      (goto-char (point-min))
      (should (search-forward "@Alice Card" nil t))
      (let ((button (button-at (1- (point)))))
        (should button)
        (goto-char (button-start button))
        (should (equal (key-binding (kbd "RET")) #'push-button))
        (should (equal "10001"
                       (get-text-property (point) 'qq-chat-mention-user-id)))
        (cl-letf (((symbol-function 'qq-user-open)
                   (lambda (user-id) (setq opened-user-id user-id))))
          (button-activate button)))
      (should (equal opened-user-id "10001")))))

(ert-deftest qq-chat-at-all-is-emphasized-but-not-a-user-link ()
  (let ((mention (qq-chat--segment-inline-string
                  '((type . "at") (data . ((qq . "all")))))))
    (with-temp-buffer
      (insert mention)
      (goto-char (point-min))
      (should (equal (buffer-string) "@全体成员"))
      (should-not (button-at (point)))
      (should (eq (get-text-property (point) 'qq-chat-mention-kind) 'at-all)))))

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
     (qq-chat--set-history-window "9007199254741004001" nil)
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
       (session-key . "group:20001")
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
                  (lambda (reference emoji-id set &rest _)
                    (setq called (list reference emoji-id set)))))
         (qq-chat-toggle-message-reaction
          "9007199254741004001" "178"))
       (should (equal called
                      '(((message_id . "9007199254741004001")
                         (chat . ((kind . "group")
                                  (group_id . "20001"))))
                        "178" nil)))))))

(ert-deftest qq-chat-react-command-adds-picked-face ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((type . group) (title . "Group") (target-id . "20001"))
    nil)
   (let ((message '((server-id . "9007199254741004001")
                    (session-key . "group:20001"))))
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (let (called)
         (cl-letf (((symbol-function 'qq-api-set-message-emoji-like)
                    (lambda (reference emoji-id set &rest _)
                      (setq called (list reference emoji-id set)))))
           (qq-chat-react-to-message "178" message))
         (should (equal called
                        '(((message_id . "9007199254741004001")
                           (chat . ((kind . "group")
                                    (group_id . "20001"))))
                          "178" t))))))))

(ert-deftest qq-chat-filter-snapshot-builds-reference-without-caching-message ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((message
            (qq-state-normalize-message-snapshot
             "group:20001"
             (qq-chat-test--filter-snapshot
              "9007199254741004001" "101" 1710000000 "hit"))))
       (should
        (equal (qq-chat--message-reference message)
               '((message_id . "9007199254741004001")
                 (chat . ((kind . "group") (group_id . "20001"))))))
       (should-not (gethash "9007199254741004001"
                            qq-state--message-session-index))
       (should-not (qq-state-session-messages "group:20001"))))))

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
    (let* ((chat-type (alist-get 'chat_type (alist-get 'peer reference)))
           (qq-chat--session-key
            (if (= chat-type 2) "group:20001" "private:10001"))
           (message
           `((server-id . ,(alist-get 'message_id reference))
             (self-p . t)
             (poke-recall-reference . ,reference)
             (segments . (((type . "poke"))))))
          recalled-reference
          ordinary-delete-called)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'qq-api-recall-poke)
                 (lambda (session-key value &rest _)
                   (setq recalled-reference (list session-key value))))
                ((symbol-function 'qq-api-delete-message)
                 (lambda (&rest _)
                   (setq ordinary-delete-called t))))
        (qq-chat--delete-message-internal message))
      (should (equal recalled-reference
                     (list qq-chat--session-key reference)))
      (should-not ordinary-delete-called))))

(ert-deftest qq-chat-recalls-ordinary-message-with-closed-reference ()
  (let ((qq-chat--session-key "group:20001")
        (message '((server-id . "9007199254741004001")
                   (session-key . "group:20001")
                   (segments . (((type . "text"))))))
        called)
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
              ((symbol-function 'qq-api-delete-message)
               (lambda (reference) (setq called reference))))
      (qq-chat--delete-message-internal message))
    (should
     (equal called
            '((message_id . "9007199254741004001")
              (chat . ((kind . "group") (group_id . "20001"))))))))

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
     (let (requested frame-updated)
       (cl-letf (((symbol-function 'qq-chat--sync-timeline)
                  (lambda (&rest args)
                    (setq requested
                          (plist-get args :changed-resources))))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (setq frame-updated t))))
         (qq-chat--rerender-open-chats "face:88")
         (qq-chat-test-sync-invalidations)
         (should (equal '((:media "face:88")) requested))
         (should-not frame-updated))))))

(ert-deftest qq-chat-sessions-refresh-does-not-rebuild-composer ()
  (qq-chat-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--ensure-view)
     (let (header-updated frame-updated)
       (cl-letf (((symbol-function 'qq-chat--header-line-update)
                  (lambda () (setq header-updated t)))
                 ((symbol-function 'qq-chat--update-frame)
                  (lambda () (setq frame-updated t))))
         (qq-chat--handle-state-change '(:type sessions-refreshed :count 1))
         (qq-chat-test-sync-invalidations)
         (should header-updated)
         (should-not frame-updated))))))

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
       (qq-chat--set-history-window "m1" nil)
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
     (qq-chat--set-history-window "m1" nil)
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

(ert-deftest qq-chat-load-older-prefers-window-first-and-uses-exact-bounds ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((title . "Alice")
      (target-id . "10001")
      (oldest-message-id . "100"))
    nil)
   (puthash
    "private:10001"
    '(((server-id . "200") (time . 200))
      ((server-id . "300") (time . 300)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat--set-history-window "200" "300")
     (should (equal (qq-state-session-oldest-message-id "private:10001") "100"))
     (let (captured-before)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (session-key &optional before callback _errback _count)
                    (setq captured-before
                          (append captured-before (list before)))
                    (should (equal session-key "private:10001"))
                    (when (equal before "200")
                      ;; The transport merge precedes its callback in real
                      ;; requests.  Populate the canonical timeline so the
                      ;; controller can prove that 150 precedes 200.
                      (puthash
                       "private:10001"
                       '(((server-id . "150") (time . 150))
                         ((server-id . "200") (time . 200))
                         ((server-id . "300") (time . 300)))
                       qq-state--messages-by-session))
                    (when callback
                      (funcall callback
                               (if (equal before "200")
                                   '(:message-count 2
                                     :added-count 1
                                     :batch-message-ids ("150" "200")
                                     :batch-oldest-message-id "150"
                                     :batch-newest-message-id "200")
                                 '(:message-count 1
                                   :added-count 0
                                   :batch-message-ids ("150")
                                   :batch-oldest-message-id "150"
                                   :batch-newest-message-id "150"))))))
                 ((symbol-function 'qq-chat--header-line-update) #'ignore)
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-load-older-messages t)
         (should (equal captured-before '("200")))
         (should (equal (appkit-chat-history-window-first-key) "150"))
         (should (equal (appkit-chat-history-window-last-key) "300"))
         (should-not (appkit-chat-history-loading-p))
         (should-not (appkit-chat-history-older-loaded-p))

         ;; Returning the exact cursor is authoritative no-progress, even
         ;; when a mock reports a non-empty batch containing that cursor.
         (qq-chat-load-older-messages t)
         (should (equal captured-before '("200" "150")))
         (should (appkit-chat-history-older-loaded-p))
         (should-not (appkit-chat-history-loading-p))

         ;; Exhaustion guards subsequent calls at the buffer-local edge.
         (qq-chat-load-older-messages t)
         (should (equal captured-before '("200" "150"))))))))

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
           (cons (quote peer_uin) "10001")
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
           (cons (quote peer_uin) "10001")
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
     (qq-chat--set-history-window "100" nil)
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
           (cons (quote peer_uin) "10001")
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
     (qq-chat--set-history-window "900" nil)
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
                            (cons (quote peer_uin) "10001")
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
                               (list :added-count 1
                                     :message-count 1
                                     :batch-message-ids '("100")
                                     :batch-oldest-message-id "100"
                                     :batch-newest-message-id "100")))))
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
     (qq-chat--set-history-window "9007199254750003456" nil)
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

(ert-deftest qq-chat-history-window-slice-honors-exact-first-and-last ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((messages '(((server-id . "m1") (time . 1))
                      ((server-id . "m2") (time . 2))
                      ((server-id . "m3") (time . 3))
                      ((server-id . "m8") (time . 8))
                      ((server-id . "m9") (time . 9)))))
      (qq-chat--set-history-window "m2" "m3")
      (should
       (equal (mapcar #'qq-chat--message-anchor
                      (qq-chat--timeline-messages messages))
              '("m2" "m3")))

      ;; If either exact boundary is missing, continuity with any other cache
      ;; island is unknown; render no substitute slice from that island.
      (qq-chat--set-history-window "missing-first" "m8")
      (should-not (qq-chat--timeline-messages messages))
      (qq-chat--set-history-window "m2" "missing-last")
      (should-not (qq-chat--timeline-messages messages)))))

(ert-deftest qq-chat-partial-window-footer-has-delimiter-not-gap-controls ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (setq qq-chat--fill-column 24)
    (qq-chat--set-history-window "m10" "m20")
    (let ((footer (qq-chat--footer-text)))
      (should (string-match-p "····" footer))
      (should-not
       (string-match-p
        "\\(?:newer messages are not loaded\\|Load newer\\|Latest\\)"
        footer))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (insert footer))
        (should-not (next-button (point-min)))))
    (appkit-chat-history-request-begin 'newer)
    (let ((footer (qq-chat--footer-text)))
      (should (string-match-p "加载中…" footer))
      (should-not (string-match-p "loading" footer))
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (insert footer))
        (should-not (next-button (point-min)))))))

(ert-deftest qq-chat-history-batch-bounds-require-canonical-batch-members ()
  (qq-chat-test-with-reset
   (puthash
    "group:20001"
    '(((server-id . "m10") (time . 10))
      ((server-id . "m20") (time . 20))
      ((server-id . "m30") (time . 30)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     ;; Batch wire order carries no chronology; canonical timeline order does.
     (should
      (equal (qq-chat--history-batch-bounds
              '(:batch-message-ids ("m30" "m10")))
             '("m10" . "m30")))
     ;; An empty batch has no boundary, irrespective of cached session rows.
     (should
      (equal (qq-chat--history-batch-bounds
              '(:batch-message-ids nil))
             (cons nil nil))))))

(ert-deftest qq-chat-load-newer-does-not-guess-eof-from-short-page ()
  (qq-chat-test-with-reset
   (puthash
    "group:20001"
    '(((server-id . "m10") (time . 10))
      ((server-id . "m20") (time . 20))
      ((server-id . "m21") (time . 21)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (qq-chat--set-history-window "m10" "m20")
     (setq qq-chat--remote-latest-id "m99")
     (cl-letf (((symbol-function 'qq-api-fetch-history-page)
                (lambda (session-key cursor direction callback
                         &optional _errback count)
                  (should (equal session-key "group:20001"))
                  (should (equal cursor "m20"))
                  (should (eq direction 'newer))
                  (should (= count qq-history-fetch-count))
                  (funcall callback
                           '(:added-count 1
                             :message-count 2
                             :batch-message-ids ("m20" "m21")
                             :batch-oldest-message-id "m20"
                             :batch-newest-message-id "m21"))))
               ((symbol-function 'appkit-chatbuf-point-in-input-p)
                (lambda (&optional _position) nil))
               ((symbol-function 'qq-chat--update-frame) #'ignore)
               ((symbol-function 'qq-chat--sync-timeline) #'ignore))
       (qq-chat-load-newer-messages t)
       (should (equal (appkit-chat-history-window-first-key) "m10"))
       (should (equal (appkit-chat-history-window-last-key) "m21"))
       (should (qq-chat--history-window-partial-p))
       (should-not (appkit-chat-history-loading-p))))))

(ert-deftest qq-chat-load-newer-finishes-at-authoritative-remote-latest ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (setq qq-chat--remote-latest-id "m24")
    (cl-letf (((symbol-function 'qq-api-fetch-history-page)
               (lambda (_session-key _cursor _direction callback &rest _)
                 (funcall callback
                          '(:added-count 4
                            :message-count 5
                            :batch-message-ids
                            ("m20" "m21" "m22" "m23" "m24")
                            :batch-oldest-message-id "m20"
                            :batch-newest-message-id "m24"))))
              ((symbol-function 'appkit-chatbuf-point-in-input-p)
               (lambda (&optional _position) nil))
              ((symbol-function 'qq-chat--update-frame) #'ignore)
              ((symbol-function 'qq-chat--sync-timeline) #'ignore))
      (qq-chat-load-newer-messages t)
      (should (equal (appkit-chat-history-window-first-key) "m10"))
      (should-not (appkit-chat-history-window-last-key))
      (should-not (qq-chat--history-window-partial-p)))))

(ert-deftest qq-chat-load-newer-attaches-on-no-progress-without-known-frontier ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (should-not qq-chat--remote-latest-id)
    (cl-letf (((symbol-function 'qq-api-fetch-history-page)
               (lambda (_session-key _cursor _direction callback &rest _)
                 (funcall callback
                          '(:added-count 0
                            :message-count 1
                            :batch-message-ids ("m20")
                            :batch-oldest-message-id "m20"
                            :batch-newest-message-id "m20"))))
              ((symbol-function 'appkit-chatbuf-point-in-input-p)
               (lambda (&optional _position) nil))
              ((symbol-function 'qq-chat--update-frame) #'ignore)
              ((symbol-function 'qq-chat--sync-timeline) #'ignore))
      (qq-chat-load-newer-messages t)
      (should-not (appkit-chat-history-window-last-key))
      (should-not (qq-chat--history-window-partial-p))
      (should-not (appkit-chat-history-newer-stalled-p)))))

(ert-deftest qq-chat-load-newer-stalls-on-no-progress-before-known-frontier ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (setq qq-chat--remote-latest-id "m99")
    (cl-letf (((symbol-function 'qq-api-fetch-history-page)
               (lambda (_session-key _cursor _direction callback &rest _)
                 (funcall callback
                          '(:added-count 0
                            :message-count 1
                            :batch-message-ids ("m20")
                            :batch-oldest-message-id "m20"
                            :batch-newest-message-id "m20"))))
              ((symbol-function 'appkit-chatbuf-point-in-input-p)
               (lambda (&optional _position) nil))
              ((symbol-function 'qq-chat--update-frame) #'ignore)
              ((symbol-function 'qq-chat--sync-timeline) #'ignore))
      (qq-chat-load-newer-messages t)
      (should (equal (appkit-chat-history-window-last-key) "m20"))
      (should (qq-chat--history-window-partial-p))
      (should (appkit-chat-history-newer-stalled-p)))))

(ert-deftest qq-chat-load-newer-attaches-after-discarding-stale-frontier ()
  (qq-chat-test-with-reset
   (let ((stale "9007199254742007080")
         (cursor "9007199254742007090"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,stale) (time . 1))
        ((server-id . ,cursor) (time . 2)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--set-history-window cursor cursor)
       (setq qq-chat--remote-latest-id stale)
       (cl-letf (((symbol-function 'qq-api-fetch-history-page)
                  (lambda (_session-key _cursor _direction callback &rest _)
                    (funcall callback
                             `(:added-count 0
                               :message-count 1
                               :batch-message-ids (,cursor)
                               :batch-oldest-message-id ,cursor
                               :batch-newest-message-id ,cursor))))
                 ((symbol-function 'appkit-chatbuf-point-in-input-p)
                  (lambda (&optional _position) nil))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-load-newer-messages t)
         (should-not qq-chat--remote-latest-id)
         (should-not (appkit-chat-history-window-last-key))
         (should-not (appkit-chat-history-newer-stalled-p)))))))

(ert-deftest qq-chat-stale-newer-callback-does-not-replace-owned-window ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (setq qq-chat--remote-latest-id "m99")
    (let (newer-callback)
      (cl-letf (((symbol-function 'qq-api-fetch-history-page)
                 (lambda (_session-key _cursor _direction callback &rest _)
                   (setq newer-callback callback)
                   'newer-request))
                ((symbol-function 'appkit-chatbuf-point-in-input-p)
                 (lambda (&optional _position) nil))
                ((symbol-function 'qq-chat--update-frame) #'ignore)
                ((symbol-function 'qq-chat--sync-timeline) #'ignore))
        (qq-chat-load-newer-messages t)
        (should newer-callback)
        (let ((replacement
               (qq-chat--begin-around-history-window
                "m90" (list 'replacement-window))))
          (qq-chat--set-history-window "m70" "m80")
          (funcall newer-callback
                   '(:added-count 10
                     :message-count 11
                     :batch-message-ids ("m20" "m30")
                     :batch-oldest-message-id "m20"
                     :batch-newest-message-id "m30"))
          (should (appkit-chat-history-request-current-p replacement))
          (should (equal qq-chat--remote-latest-id "m90"))
          (should (equal (appkit-chat-history-window-first-key) "m70"))
          (should (equal (appkit-chat-history-window-last-key) "m80"))
          ;; The stale newer callback must not clear the replacement around
          ;; request's in-flight state.
          (should (eq (appkit-chat-history-loading) 'around)))))))

(ert-deftest qq-chat-around-success-clears-loading-and-owner ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (loading-during-request)
      (cl-letf (((symbol-function 'qq-api-fetch-history-around)
                 (lambda (session-key target callback
                          &optional _errback _count)
                   (should (equal session-key "group:20001"))
                   (should (equal target "m20"))
                   (should (appkit-chat-history-request-owner))
                   (setq loading-during-request
                         (appkit-chat-history-loading))
                   ;; Real history callbacks run after the transport has
                   ;; merged the batch into canonical session order.
                   (puthash
                    "group:20001"
                    '(((server-id . "m19") (time . 19))
                      ((server-id . "m20") (time . 20)))
                    qq-state--messages-by-session)
                   (funcall callback
                            '(:added-count 2
                              :message-count 2
                              :batch-message-ids ("m19" "m20")))
                   'around-request))
                ((symbol-function 'qq-chat--finish-jump-if-loaded)
                 (lambda (_target) t))
                ((symbol-function 'qq-chat--update-frame) #'ignore)
                ((symbol-function 'qq-chat--sync-timeline) #'ignore))
        (qq-chat--seek-history-for-jump
         "group:20001" "m20" (current-buffer))
        (should (eq loading-during-request 'around))
        (should-not (appkit-chat-history-loading-p))
        (should-not (appkit-chat-history-request-owner))
        (should (equal (appkit-chat-history-window-first-key) "m19"))
        (should (equal (appkit-chat-history-window-last-key) "m20"))))))

(ert-deftest qq-chat-around-failure-clears-loading-and-owner ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (loading-during-request failure)
      (cl-letf (((symbol-function 'qq-api-fetch-history-around)
                 (lambda (session-key target _callback
                          &optional errback _count)
                   (should (equal session-key "group:20001"))
                   (should (equal target "m20"))
                   (should (appkit-chat-history-request-owner))
                   (setq loading-during-request
                         (appkit-chat-history-loading))
                   (funcall errback nil "network failure")
                   'around-request))
                ((symbol-function 'qq-chat--jump-fail)
                 (lambda (target reason)
                   (setq failure (list target reason)))))
        (qq-chat--seek-history-for-jump
         "group:20001" "m20" (current-buffer))
        (should (eq loading-during-request 'around))
        (should (equal failure '("m20" "network failure")))
        (should-not (appkit-chat-history-loading-p))
        (should-not (appkit-chat-history-request-owner))))))

(ert-deftest qq-chat-around-batch-advances-captured-frontier-to-normalized-newest ()
  (qq-chat-test-with-reset
   (let ((first "9007199254742007088")
         (captured-latest "9007199254742007089")
         (normalized-newest "9007199254742007090"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,first) (time . 1))
        ((server-id . ,captured-latest) (time . 2))
        ((server-id . ,normalized-newest) (time . 3)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--begin-around-history-window captured-latest)
       (cl-letf (((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         ;; Wire order and fallback newest are deliberately misleading;
         ;; normalized session order makes NORMALIZED-NEWEST authoritative.
         (qq-chat--note-history-window
          `(:added-count 3
            :message-count 3
            :batch-message-ids
            (,normalized-newest ,captured-latest ,first)
            :batch-oldest-message-id ,first
            :batch-newest-message-id ,captured-latest))
         (should (equal qq-chat--remote-latest-id normalized-newest))
         (should (equal (appkit-chat-history-window-first-key) first))
         (should-not (appkit-chat-history-window-last-key))
         (should (qq-chat--history-window-known-p)))))))

(ert-deftest qq-chat-around-batch-discards-a-proven-stale-remote-frontier ()
  (qq-chat-test-with-reset
   (let ((stale-frontier "9007199254742007080")
         (first "9007199254742007090")
         (newest "9007199254742007092"))
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,stale-frontier) (time . 1))
        ((server-id . ,first) (time . 2))
        ((server-id . ,newest) (time . 3)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--begin-around-history-window stale-frontier)
       (cl-letf (((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--note-history-window
          `(:added-count 2
            :message-count 2
            :batch-message-ids (,first ,newest)
            :batch-oldest-message-id ,first
            :batch-newest-message-id ,newest))
         ;; Canonical order, rather than snowflake arithmetic, proves that
         ;; the disconnected frontier predates this around window.  Unknown
         ;; is safer and lets a no-progress newer page attach at real latest.
         (should-not qq-chat--remote-latest-id)
         (should (equal (appkit-chat-history-window-first-key) first))
         (should (equal (appkit-chat-history-window-last-key) newest)))))))

(ert-deftest qq-chat-return-to-latest-fetches-nil-cursor-and-rebuilds-window ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "old-first" "old-last")
    (setq qq-chat--remote-latest-id "old-remote")
    (appkit-chat-history-older-loaded-set t)
    (let (fetches goto-id)
      (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                 (lambda (session-key &optional before callback _errback _count)
                   (setq fetches
                         (append fetches (list (list session-key before))))
                   ;; Mirror `qq-api-fetch-older-history': canonical merge
                   ;; precedes the metadata callback.
                   (puthash
                    "group:20001"
                    '(((server-id . "latest-first") (time . 1))
                      ((server-id . "latest-last") (time . 2)))
                    qq-state--messages-by-session)
                   (funcall callback
                            '(:added-count 2
                              :message-count 2
                              :batch-message-ids
                              ("latest-first" "latest-last")))
                   'latest-request))
                ((symbol-function 'qq-chat--goto-latest-window-end)
                 (lambda (&optional preferred-id) (setq goto-id preferred-id)))
                ((symbol-function 'qq-chat--update-frame) #'ignore)
                ((symbol-function 'qq-chat--sync-timeline) #'ignore))
        (qq-chat-return-to-latest)
        (should (equal fetches '(("group:20001" nil))))
        (should (equal (appkit-chat-history-window-first-key)
                       "latest-first"))
        (should-not (appkit-chat-history-window-last-key))
        (should-not (qq-chat--history-window-partial-p))
        (should-not (appkit-chat-history-older-loaded-p))
        (should (equal qq-chat--remote-latest-id "latest-last"))
        (should (equal goto-id "latest-last"))
        (should-not (appkit-chat-history-request-owner))
        (should-not (appkit-chat-history-loading-p))))))

(ert-deftest qq-chat-return-to-latest-preserves-live-frontier-seen-in-flight ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Group") (target-id . "20001") (type . group))
    nil)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001")
     (let ((frontier-at-start "9007199254742007087")
           (page-first "9007199254742007088")
           (page-last "9007199254742007089")
           (live-frontier "9007199254742007090")
           latest-callback
           goto-id
           marked)
       (qq-chat--set-history-window "old-first" "old-last")
       (setq qq-chat--remote-latest-id frontier-at-start)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (session-key &optional before callback
                           _errback _count)
                    (should (equal session-key "group:20001"))
                    (should-not before)
                    (setq latest-callback callback)
                    'latest-request))
                 ((symbol-function 'qq-api-mark-message-read)
                  (lambda (session-key message-id)
                    (push (list session-key message-id) marked)))
                 ((symbol-function 'qq-chat--goto-latest-window-end)
                  (lambda (&optional preferred-id)
                    (setq goto-id preferred-id)))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-return-to-latest nil t)
         (should latest-callback)
         (should (eq (appkit-chat-history-loading) 'newer))
         (should
          (equal (plist-get (appkit-chat-history-request-owner)
                            :frontier-at-start)
                 frontier-at-start))

         ;; A live event advances the authoritative frontier while the
         ;; nil-cursor page is still in flight.
         (puthash
          "group:20001"
          `(((server-id . ,page-first) (time . 1))
            ((server-id . ,page-last) (time . 2))
            ((server-id . ,live-frontier) (time . 3)))
          qq-state--messages-by-session)
         (setq qq-chat--remote-latest-id live-frontier)

         ;; The older response must rebuild the latest window without
         ;; regressing either navigation or the exact mark-read target.
         (funcall latest-callback
                  `(:added-count 2
                    :message-count 2
                    :batch-message-ids (,page-first ,page-last)
                    :batch-oldest-message-id ,page-first
                    :batch-newest-message-id ,page-last))
         (should (equal qq-chat--remote-latest-id live-frontier))
         (should (equal goto-id live-frontier))
         (should (equal marked `(("group:20001" ,live-frontier))))
         (should-not (appkit-chat-history-request-owner))
         (should-not (appkit-chat-history-loading-p)))))))

(ert-deftest qq-chat-return-to-latest-adopts-live-row-after-empty-snapshot ()
  (qq-chat-test-with-reset
   (let ((old-frontier "9007199254742007089")
         (live-frontier "9007199254742007090")
         latest-callback
         goto-id
         marked)
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--set-history-window "old-first" "old-last")
       (setq qq-chat--remote-latest-id old-frontier)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (_session-key &optional _before callback
                           _errback _count)
                    (setq latest-callback callback)
                    'latest-request))
                 ((symbol-function 'qq-api-mark-message-read)
                  (lambda (session-key message-id)
                    (push (list session-key message-id) marked)))
                 ((symbol-function 'qq-chat--goto-latest-window-end)
                  (lambda (&optional preferred-id)
                    (setq goto-id preferred-id)))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-return-to-latest nil t)
         (puthash
          "group:20001"
          `(((server-id . ,live-frontier) (time . 2)))
          qq-state--messages-by-session)
         (qq-chat--observe-message-frontier
          `(:type message
            :session-key "group:20001"
            :mutation create
            :source event
            :message-anchor ,live-frontier
            :message ((server-id . ,live-frontier) (time . 2))))
         ;; The nil-cursor snapshot preceded the live delivery and therefore
         ;; returned no rows.  The exact live row still forms a safe one-row
         ;; window attached to latest.
         (funcall latest-callback
                  '(:added-count 0
                    :message-count 0
                    :batch-message-ids nil
                    :batch-oldest-message-id nil
                    :batch-newest-message-id nil))
         (should (qq-chat--history-window-known-p))
         (should (equal (appkit-chat-history-window-first-key)
                        live-frontier))
         (should-not (appkit-chat-history-window-last-key))
         (should (equal goto-id live-frontier))
         (should (equal marked `(("group:20001" ,live-frontier))))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 (list live-frontier))))))))

(ert-deftest qq-chat-return-to-latest-empty-snapshot-hides-old-cache-island ()
  (qq-chat-test-with-reset
   (qq-state-upsert-session
    "group:20001"
    '((title . "Group") (target-id . "20001") (type . group))
   nil)
   (puthash
    "group:20001"
    '(((server-id . "9007199254742007087") (time . 1))
      ((server-id . "9007199254742007088") (time . 2)))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "group:20001"
           qq-chat--remote-latest-id "9007199254742007088")
     (qq-chat--set-history-window
      "9007199254742007087" "9007199254742007088")
     (let (latest-callback goto-called)
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (_session-key &optional before callback
                           _errback _count)
                    (should-not before)
                    (setq latest-callback callback)
                    'latest-request))
                 ((symbol-function 'qq-chat--goto-latest-window-end)
                  (lambda (&optional _preferred-id)
                    (setq goto-called t)))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-return-to-latest nil nil)
         (funcall latest-callback
                  '(:added-count 0
                    :message-count 0
                    :batch-message-ids nil
                    :batch-oldest-message-id nil
                    :batch-newest-message-id nil))
         (should (appkit-chat-history-window-empty-p))
         (should (appkit-chat-history-older-loaded-p))
         (should-not qq-chat--remote-latest-id)
         (should-not (qq-chat--timeline-messages))
         (should goto-called)
         (should-not (appkit-chat-history-request-owner))
         (should-not (appkit-chat-history-loading-p)))))))

(ert-deftest qq-chat-auto-loads-newer-only-near-partial-window-footer ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (let ((qq-chat-history-auto-load-threshold 50)
          calls)
      (cl-letf (((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'qq-chat-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls))))
        (qq-chat--maybe-auto-load-newer 800)
        (should-not calls)
        (qq-chat--maybe-auto-load-newer 975)
        (should (equal calls '(t)))

        ;; No-progress against a known different remote frontier suppresses
        ;; automatic retries until the window cursor changes.
        (setq calls nil)
        (appkit-chat-history-newer-stalled-set "m20")
        (qq-chat--maybe-auto-load-newer 975)
        (should-not calls)))))

(ert-deftest qq-chat-window-scroll-loads-newer-from-selected-viewport-edge ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (let ((window 'test-window)
          (buffer (current-buffer))
          (qq-chat-history-auto-load-threshold 50)
          calls)
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer) (lambda (_window) buffer))
                ((symbol-function 'selected-window) (lambda () window))
                ((symbol-function
                  'appkit-chat-timeline-window-visible-end-position)
                 (lambda (_window) 975))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'qq-chat-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls)))
                ((symbol-function 'qq-chat--manage-read-position)
                 (lambda (&rest _args)
                   (ert-fail "selected scroll must not duplicate read handling"))))
        ;; DISPLAY-START remains far from the footer; the viewport's lower
        ;; edge is what makes this selected-window scroll eligible.
        (qq-chat--window-scroll window 100)
        (should (equal calls '(t)))))))

(ert-deftest qq-chat-window-scroll-loads-newer-and-reads-inactive-window ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" "m20")
    (let ((window 'inactive-window)
          (buffer (current-buffer))
          (qq-chat-history-auto-load-threshold 50)
          calls
          read-position)
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (candidate) (eq candidate window)))
                ((symbol-function 'window-buffer) (lambda (_window) buffer))
                ((symbol-function 'selected-window)
                 (lambda () 'selected-window))
                ((symbol-function 'window-point) (lambda (_window) 700))
                ((symbol-function
                  'appkit-chat-timeline-window-visible-end-position)
                 (lambda (_window) 975))
                ((symbol-function 'appkit-chat-timeline-footer-start-position)
                 (lambda () 1000))
                ((symbol-function 'appkit-chatbuf-composer-idle-p)
                 (lambda () t))
                ((symbol-function 'qq-chat-load-newer-messages)
                 (lambda (&optional quiet) (push quiet calls)))
                ((symbol-function 'qq-chat--manage-read-position)
                 (lambda (position) (setq read-position position))))
        (qq-chat--window-scroll window 100)
        (should (= read-position 700))
        (should (equal calls '(t)))))))

(ert-deftest qq-chat-post-command-auto-loads-older-near-top ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (qq-chat--set-history-window "m10" nil)
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

(ert-deftest qq-chat-read-position-follows-cursor-without-regressing ()
  (qq-chat-test-with-reset
   (let ((first "9007199254741004645")
         (second "9007199254741004646")
         (third "9007199254741004647")
         calls)
     (qq-state-upsert-session
      "group:20001"
      `((title . "Group")
        (target-id . "20001")
        (type . group)
        (unread-count . 2)
        (first-unread-message-id . ,second)
        (read-latest-message-id . ,third))
      nil)
     (puthash
      "group:20001"
      `(((server-id . ,first) (time . 1) (raw-message . "first"))
        ((server-id . ,second) (time . 2) (raw-message . "second"))
        ((server-id . ,third) (time . 3) (raw-message . "third")))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (qq-chat--set-history-window first nil)
       (qq-chat-render)
       (cl-letf (((symbol-function 'qq-api-mark-message-read)
                  (lambda (session-key message-id)
                    (setq calls (append calls (list (list session-key message-id)))))))
         ;; An already-read row does not move the native boundary backward.
         (goto-char (point-min))
         (search-forward "first")
         (qq-chat--manage-read-position)
         (should-not calls)
         ;; Point on the timeline reads exactly that row.
         (search-forward "second")
         (qq-chat--manage-read-position)
         (search-forward "third")
         (qq-chat--manage-read-position)
         ;; Moving backward cannot submit an older target.
         (goto-char (point-min))
         (search-forward "second")
         (qq-chat--manage-read-position)
         ;; Point in the composer represents the newest loaded row and dedupes.
         (goto-char (point-max))
         (qq-chat--manage-read-position)
         (should
          (equal calls
                 `(("group:20001" ,second)
                   ("group:20001" ,third)))))))))

(ert-deftest qq-chat-read-all-submits-latest-exact-message-when-auto-read-is-off ()
  (qq-chat-test-with-reset
   (let ((qq-auto-mark-read nil)
         (first "9007199254741004645")
         (latest "9007199254741004647")
         call)
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice") (target-id . "10001") (type . private))
      nil)
     (puthash
      "private:10001"
      `(((server-id . ,first) (time . 1))
        ((server-id . ,latest) (time . 2)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (qq-chat--set-history-window first nil)
       (cl-letf (((symbol-function 'qq-api-mark-message-read)
                  (lambda (session-key message-id)
                    (setq call (list session-key message-id)))))
         (qq-chat-read-all)
         (should (equal call (list "private:10001" latest))))))))

(ert-deftest qq-chat-read-all-unknown-window-fetches-before-marking-cache ()
  (qq-chat-test-with-reset
   (let ((first "9007199254741004645")
         (latest "9007199254741004647")
         fetch-call
         latest-callback
         marked)
     (qq-state-upsert-session
      "private:10001"
      '((title . "Alice") (target-id . "10001") (type . private))
      nil)
     (puthash
      "private:10001"
      `(((server-id . ,first) (time . 1))
        ((server-id . ,latest) (time . 2)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "private:10001")
       (appkit-chat-history-window-clear)
       (should-not (qq-chat--history-window-known-p))
       (cl-letf (((symbol-function 'qq-api-fetch-older-history)
                  (lambda (session-key &optional before callback
                           _errback _count)
                    (setq fetch-call (list session-key before)
                          latest-callback callback)
                    'latest-request))
                 ((symbol-function 'qq-api-mark-message-read)
                  (lambda (session-key message-id)
                    (push (list session-key message-id) marked)))
                 ((symbol-function 'qq-chat--goto-latest-window-end) #'ignore)
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat-read-all)
         (should (equal fetch-call '("private:10001" nil)))
         (should latest-callback)
         ;; Cached rows cannot be marked until a nil-cursor response proves
         ;; which contiguous latest window they belong to.
         (should-not marked)
         (should (eq (appkit-chat-history-loading) 'newer))
         (funcall latest-callback
                  `(:added-count 2
                    :message-count 2
                    :batch-message-ids (,first ,latest)
                    :batch-oldest-message-id ,first
                    :batch-newest-message-id ,latest))
         (should (qq-chat--history-window-known-p))
         (should (equal marked `(("private:10001" ,latest))))
         (should-not (appkit-chat-history-loading-p)))))))

(ert-deftest qq-chat-initial-latest-success-clears-loading-and-establishes-window ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (read-callback history-callback)
      (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                 (lambda (_session-key callback &optional _errback)
                   (setq read-callback callback)
                   'read-state-token))
                ((symbol-function 'qq-api-fetch-older-history)
                 (lambda (_session-key &optional before callback
                          _errback _count)
                   (should-not before)
                   (setq history-callback callback)
                   'history-token))
                ((symbol-function 'qq-chat--update-frame) #'ignore)
                ((symbol-function 'qq-chat--sync-timeline) #'ignore))
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (should (eq (appkit-chat-history-loading) 'initial))
        (should-not (qq-chat--history-window-known-p))
        (should (appkit-chat-history-request-owner))

        (funcall read-callback
                 '((unread_count . 0) (first_unread . nil) (latest . nil)))
        (should history-callback)
        (should (eq qq-chat--initial-history-request 'history-token))
        (should (eq (appkit-chat-history-loading) 'initial))

        ;; The API installs normalized history before invoking its callback.
        (puthash
         "group:20001"
         '(((server-id . "m10") (time . 10))
           ((server-id . "m20") (time . 20)))
         qq-state--messages-by-session)
        (funcall history-callback
                 '(:added-count 2
                   :message-count 2
                   :batch-message-ids ("m10" "m20")))
        (should (qq-chat--history-window-known-p))
        (should (equal (appkit-chat-history-window-first-key) "m10"))
        (should-not (appkit-chat-history-window-last-key))
        (should-not (appkit-chat-history-loading-p))
        (should-not (appkit-chat-history-request-owner))
        (should-not qq-chat--initial-history-owner)
        (should-not qq-chat--initial-history-request)))))

(ert-deftest qq-chat-initial-empty-snapshot-adopts-in-flight-live-row ()
  (qq-chat-test-with-reset
   (let ((live-frontier "9007199254742007090")
         read-callback
         history-callback)
     (qq-state-upsert-session
     "group:20001"
     '((title . "Group") (target-id . "20001") (type . group))
     nil)
     ;; Canonical state may retain a disconnected older cache island.  The
     ;; authoritative empty latest response must not project it.
     (puthash
      "group:20001"
      '(((server-id . "9007199254742007087") (time . 1)))
      qq-state--messages-by-session)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                  (lambda (_session-key callback &optional _errback)
                    (setq read-callback callback)
                    'read-state-token))
                 ((symbol-function 'qq-api-fetch-older-history)
                  (lambda (_session-key &optional _before callback
                           _errback _count)
                    (setq history-callback callback)
                    'history-token))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--load-initial-history (current-buffer) "group:20001")
         (funcall read-callback
                  '((unread_count . 0)
                    (first_unread . nil)
                    (latest . nil)))
         (should history-callback)
         (funcall history-callback
                  '(:added-count 0
                    :message-count 0
                    :batch-message-ids nil
                    :batch-oldest-message-id nil
                    :batch-newest-message-id nil))
         (should (qq-chat--history-window-known-p))
         (should (appkit-chat-history-window-empty-p))
         (should-not (qq-chat--timeline-messages))

         ;; A live delivery after the empty snapshot seeds its exact first
         ;; edge instead of exposing arbitrary cache entries through nil ids.
         (puthash
          "group:20001"
          `(((server-id . ,live-frontier) (time . 2)))
          qq-state--messages-by-session)
         (qq-chat--observe-message-frontier
          `(:type message
            :session-key "group:20001"
            :mutation create
            :source event
            :message-anchor ,live-frontier
            :message ((server-id . ,live-frontier) (time . 2))))
         (should-not (appkit-chat-history-window-empty-p))
         (should (equal (appkit-chat-history-window-first-key)
                        live-frontier))
         (should-not (appkit-chat-history-window-last-key))
         (should-not (appkit-chat-history-loading-p))
         (should-not (appkit-chat-history-request-owner))
         (should
          (equal (mapcar #'qq-chat--message-anchor
                         (qq-chat--timeline-messages))
                 (list live-frontier))))))))

(ert-deftest qq-chat-initial-read-state-failure-clears-loading-and-owner ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let (read-errback failure)
      (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                 (lambda (_session-key _callback &optional errback)
                   (setq read-errback errback)
                   'read-state-token))
                ((symbol-function 'qq-api--default-error)
                 (lambda (_response reason) (setq failure reason)))
                ((symbol-function 'qq-chat--update-frame) #'ignore)
                ((symbol-function 'qq-chat--sync-timeline) #'ignore))
        (qq-chat--load-initial-history (current-buffer) "group:20001")
        (should (eq (appkit-chat-history-loading) 'initial))
        (should (appkit-chat-history-request-owner))
        (funcall read-errback nil "read-state failed")
        (should (equal failure "read-state failed"))
        (should-not (appkit-chat-history-loading-p))
        (should-not (appkit-chat-history-request-owner))
        (should-not qq-chat--initial-history-owner)
        (should-not qq-chat--initial-history-request)
        (should-not (qq-chat--history-window-known-p))))))

(ert-deftest qq-chat-initial-accepted-read-observation-updates-session ()
  (qq-chat-test-with-reset
   (let ((qq-api--read-observation-clock 0)
         (qq-api--session-read-observation-tokens
          (make-hash-table :test #'equal))
         (first "9007199254742007089")
         (latest "9007199254742007092")
         read-callback
         around-target)
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group")
        (target-id . "20001")
        (type . group)
        (unread-count . 0))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                  (lambda (_session-key callback &optional _errback)
                    (setq read-callback callback)
                    'read-state-token))
                 ((symbol-function 'qq-api-fetch-history-around)
                  (lambda (_session-key message-id _callback
                           &optional _errback _count)
                    (setq around-target message-id)
                    'around-token))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--load-initial-history (current-buffer) "group:20001")
         (funcall read-callback
                  `((unread_count . 3)
                    (first_unread
                     . ((sequence . "101") (message_id . ,first)))
                    (latest
                     . ((sequence . "104") (message_id . ,latest)))
                    (mentions . ((at_me . nil) (at_all . nil)))))
         (let ((session (qq-state-session "group:20001")))
           (should (= (alist-get 'unread-count session) 3))
           (should (equal (alist-get 'first-unread-message-id session)
                          first))
           (should (equal (alist-get 'first-unread-message-seq session)
                          "101"))
           (should (equal (alist-get 'read-latest-message-id session)
                          latest)))
         (should (equal around-target first))
         (should (equal qq-chat--remote-latest-id latest))
         (should (eq qq-chat--initial-history-request 'around-token)))))))

(ert-deftest qq-chat-initial-stale-read-observation-uses-newer-session-state ()
  (qq-chat-test-with-reset
   (let ((qq-api--read-observation-clock 0)
         (qq-api--session-read-observation-tokens
          (make-hash-table :test #'equal))
         (raw-first "9007199254742007081")
         (raw-latest "9007199254742007082")
         (canonical-first "9007199254742007091")
         (canonical-latest "9007199254742007092")
         read-callback
         around-target)
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                  (lambda (_session-key callback &optional _errback)
                    (setq read-callback callback)
                    'read-state-token))
                 ((symbol-function 'qq-api-fetch-history-around)
                  (lambda (_session-key message-id _callback
                           &optional _errback _count)
                    (setq around-target message-id)
                    'around-token))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--load-initial-history (current-buffer) "group:20001")

         ;; A newer notice/recent-contact observation wins before the older
         ;; initial HTTP response arrives and populates canonical state.
         (let ((newer-token (qq-api-read-observation-start)))
           (should
            (qq-api-read-observation-accept-p "group:20001" newer-token)))
         (qq-state-apply-session-read-state
          "group:20001"
          `((unread_count . 5)
            (first_unread
             . ((sequence . "201") (message_id . ,canonical-first)))
            (latest
             . ((sequence . "205") (message_id . ,canonical-latest)))
            (mentions . ((at_me . nil) (at_all . nil)))))

         (funcall read-callback
                  `((unread_count . 1)
                    (first_unread
                     . ((sequence . "101") (message_id . ,raw-first)))
                    (latest
                     . ((sequence . "102") (message_id . ,raw-latest)))
                    (mentions . ((at_me . nil) (at_all . nil)))))
         (let ((session (qq-state-session "group:20001")))
           (should (= (alist-get 'unread-count session) 5))
           (should (equal (alist-get 'first-unread-message-id session)
                          canonical-first))
           (should (equal (alist-get 'read-latest-message-id session)
                          canonical-latest))
           (should-not
            (equal (alist-get 'first-unread-message-id session) raw-first)))
         (should (equal around-target canonical-first))
         (should (equal qq-chat--remote-latest-id canonical-latest)))))))

(ert-deftest qq-chat-initial-read-http-does-not-overwrite-live-frontier ()
  (qq-chat-test-with-reset
   (let ((qq-api--read-observation-clock 0)
         (qq-api--session-read-observation-tokens
          (make-hash-table :test #'equal))
         (first "9007199254742007088")
         (http-latest "9007199254742007089")
         (live-frontier "9007199254742007090")
         read-callback
         around-frontier)
     (qq-state-upsert-session
      "group:20001"
      '((title . "Group") (target-id . "20001") (type . group))
      nil)
     (with-temp-buffer
       (qq-chat-mode)
       (setq qq-chat--session-key "group:20001")
       (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                  (lambda (_session-key callback &optional _errback)
                    (setq read-callback callback)
                    'read-state-token))
                 ((symbol-function 'qq-api-fetch-history-around)
                  (lambda (_session-key _message-id _callback
                           &optional _errback _count)
                    (setq around-frontier qq-chat--remote-latest-id)
                    'around-token))
                 ((symbol-function 'qq-chat--update-frame) #'ignore)
                 ((symbol-function 'qq-chat--sync-timeline) #'ignore))
         (qq-chat--load-initial-history (current-buffer) "group:20001")
         (should-not
          (plist-get qq-chat--initial-history-owner :frontier-at-start))

         ;; Live delivery advances the buffer frontier while read-state HTTP
         ;; is pending.  The accepted HTTP projection may update the session,
         ;; but it cannot move the buffer frontier backward.
         (setq qq-chat--remote-latest-id live-frontier)
         (funcall read-callback
                  `((unread_count . 1)
                    (first_unread
                     . ((sequence . "101") (message_id . ,first)))
                    (latest
                     . ((sequence . "102") (message_id . ,http-latest)))
                    (mentions . ((at_me . nil) (at_all . nil)))))
         (should
          (equal (alist-get 'read-latest-message-id
                            (qq-state-session "group:20001"))
                 http-latest))
         (should (equal around-frontier live-frontier))
         (should (equal qq-chat--remote-latest-id live-frontier))
         (should (equal (plist-get qq-chat--initial-history-owner
                                   :remote-latest-id)
                        live-frontier)))))))

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

(ert-deftest qq-chat-initial-unread-around-hands-off-request-token ()
  (with-temp-buffer
    (qq-chat-mode)
    (setq qq-chat--session-key "group:20001")
    (let ((buffer (current-buffer))
          read-state-callback
          initial-around-callback
          canceled
          around-message-ids
          noted-windows)
      (cl-letf (((symbol-function 'qq-api-fetch-session-read-state)
                 (lambda (_session-key callback &optional _errback)
                   (setq read-state-callback callback)
                   'read-state-token))
                ((symbol-function 'qq-api-fetch-history-around)
                 (lambda (_session-key message-id callback
                          &optional _errback _count)
                   (setq around-message-ids
                         (append around-message-ids (list message-id)))
                   (if (equal message-id "9007199254742007089")
                       (progn
                         (setq initial-around-callback callback)
                         'unread-around-token)
                     'exact-around-token)))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (token) (push token canceled)))
                ((symbol-function 'qq-chat--open-buffer)
                 (lambda (_session-key) buffer))
                ((symbol-function 'pop-to-buffer)
                 (lambda (&rest _args) buffer))
                ((symbol-function 'qq-chat--on-window-size-change) #'ignore)
                ((symbol-function 'qq-chat--goto-loaded-message)
                 (lambda (&rest _args) nil))
                ((symbol-function 'qq-chat--note-history-window)
                 (lambda (meta &optional _remote-latest-id)
                   (push meta noted-windows))))
        (qq-chat--load-initial-history buffer "group:20001")
        (should (eq qq-chat--initial-history-request 'read-state-token))

        (funcall read-state-callback
                 '((unread_count . 7)
                   (first_unread
                    . ((sequence . "30001")
                       (message_id . "9007199254742007089")))
                   (latest . nil)))
        (should (eq qq-chat--initial-history-request 'unread-around-token))
        (should initial-around-callback)

        (qq-chat-open-message "group:20001" "9007199254742007999")
        (should (memq 'unread-around-token canceled))
        (should-not qq-chat--initial-history-request)
        (should-not qq-chat--initial-history-owner)
        (should (eq qq-chat--open-message-request 'exact-around-token))
        (should (equal around-message-ids
                       '("9007199254742007089" "9007199254742007999")))

        (funcall initial-around-callback
                 '(:batch-newest-message-id "9007199254742007090"))
        (should-not noted-windows)
        (should (eq qq-chat--open-message-request 'exact-around-token))))))

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
     (qq-chat--set-history-window "9007199254743009336" nil)
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
     (qq-chat--set-history-window "9007199254743009336" nil)
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
     (qq-chat--set-history-window "9007199254743009336" nil)
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
