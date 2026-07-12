;;; qq-forward-test.el --- Tests for qq-forward -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-forward)

(cl-defun qq-forward-test--native-message
    (entry-id text &key message-id (state "live") sender origin segments)
  "Return one strict fork-native snapshot fixture."
  `((entry_id . ,entry-id)
    ,@(when message-id `((message_id . ,message-id)))
    (state . ,state)
    (sent_at . 1710000000)
    (sender . ,(or sender
                   '((kind . "user")
                     (user_id . "10001")
                     (name . "Alice"))))
    (origin . ,(or origin
                   '((kind . "group") (group_id . "20001"))))
    (segments
     . ,(or segments
            `(((kind . "text") (payload . ((text . ,text)))))))))

(defun qq-forward-test--message-source (message-id chat-id)
  "Return a locator-qualified native message source."
  `((kind . "message")
    (message_id . ,message-id)
    (chat . ((kind . "group") (group_id . ,chat-id)))))

(defun qq-forward-test--context-source (&optional peer-uid)
  "Return a native context source for PEER-UID."
  `((kind . "context")
    (peer . ((chat_type . 2)
             (peer_uid . ,(or peer-uid "u_group-peer"))
             (guild_id . "")))
    (root_message_id . "9007199254742007001")
    (parent_message_id . "9007199254742007002")))

(defun qq-forward-test--remote-segment (source)
  "Return canonical internal remote forward segment for SOURCE."
  `((type . "forward")
    (data . ((content . ((kind . "remote")
                         (reference . ,source)))))))

(defun qq-forward-test--inline-segment (messages)
  "Return canonical internal inline forward segment for MESSAGES."
  `((type . "forward")
    (data . ((content . ((kind . "inline")
                         (messages . ,messages)))))))

(defun qq-forward-test--kill-viewers ()
  "Kill all deterministic forward viewer buffers made by tests."
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*qq-forward:" (buffer-name buffer))
      (kill-buffer buffer))))

(defmacro qq-forward-test--with-clean-viewers (&rest body)
  "Run BODY with forward viewer buffers cleaned before and after."
  (declare (indent 0) (debug t))
  `(progn
     (qq-forward-test--kill-viewers)
     (unwind-protect
         (progn ,@body)
       (qq-forward-test--kill-viewers))))

(ert-deftest qq-forward-mode-has-special-navigation-bindings ()
  (with-temp-buffer
    (qq-forward-mode)
    (should (derived-mode-p 'special-mode))
    (should (eq (lookup-key qq-forward-mode-map (kbd "q")) #'quit-window))
    (should (eq (lookup-key qq-forward-mode-map (kbd "g")) #'qq-forward-refresh))
    (should (eq (lookup-key qq-forward-mode-map (kbd "n")) #'qq-forward-next-message))
    (should (eq (lookup-key qq-forward-mode-map (kbd "p")) #'qq-forward-previous-message))
    (should (eq (lookup-key qq-forward-mode-map (kbd "RET")) #'qq-forward-activate))))

(ert-deftest qq-forward-layout-hooks-use-shared-chat-geometry ()
  (with-temp-buffer
    (qq-forward-mode)
    (should (memq #'qq-forward--on-window-size-change
                  window-size-change-functions))
    (should (memq #'qq-forward--on-text-scale-change
                  text-scale-mode-hook))
    (let (calls)
      (cl-letf (((symbol-function 'qq-chat-sync-timeline-geometry)
                 (lambda (&rest keys)
                   (push keys calls))))
        (qq-forward--on-window-size-change)
        (qq-forward--on-text-scale-change))
      (should (equal (nreverse calls)
                     '(nil (:reset t :force t)))))))

(ert-deftest qq-forward-header-uses-a-live-timeline-only-view ()
  (qq-forward-test--with-clean-viewers
    (let ((source (qq-forward-test--message-source
                   "9007199254742007001" "20001")))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional _errback)
                   (funcall callback nil))))
        (save-window-excursion
          (let ((buffer (qq-forward-open source)))
            (with-current-buffer buffer
              (let ((view (appkit-current-view)))
                (should (appkit-view-live-p view))
                (should (equal (appkit-view-id view)
                               (qq-forward--view-id
                                (qq-forward--source-buffer-key source))))
                (should (equal (appkit-view-parts view) '(timeline)))
                (should-not (appkit-chatbuf-prompt-start-position))
                (should-not (appkit-chatbuf-input-start-position)))
              (should (string-match-p
                       "message_id: 9007199254742007001"
                                      (buffer-string)))
              (should-not (string-match-p "n/p: navigate" (buffer-string)))
              (should-not (string-match-p "RET: open"
                                          (buffer-string))))))))))

(ert-deftest qq-forward-recognizes-only-canonical-native-derived-segments ()
  (let* ((source (qq-forward-test--message-source
                  "9007199254742007089" "20001"))
         (remote (qq-forward-test--remote-segment source))
         (inline (qq-forward-test--inline-segment nil))
         (card '((type . "card")
                 (data . ((kind . "forward")
                          (reference . ((kind . "resource")
                                        (resource_id . "resource-a")))
                          (presentation . nil))))))
    (should (qq-forward-segment-p remote))
    (should (qq-forward-segment-p inline))
    (should (qq-forward-segment-p card))
    (should (equal (qq-forward-reference-id remote)
                   "9007199254742007089"))
    (should (equal (qq-forward-reference-id card) "resource-a"))
    (dolist (legacy
             '(((type . "forward")
                (data . ((message_id . "9007199254742007089"))))
               ((type . "card")
                (data . ((kind . "forward") (res_id . "resource-a"))))
               ((type . "node") (data . ((content . "legacy"))))))
      (should-not (qq-forward-segment-p legacy))
      (should-error (qq-forward-open-segment legacy) :type 'user-error))))

(ert-deftest qq-forward-message-count-does-not-treat-nil-as-zero ()
  "Remote forwards have no inline snapshot; nil must not become 0.

In Elisp the empty list is nil, so a missing inline snapshot and an empty
list are indistinguishable — both mean \"do not claim a count\"."
  (should (null (qq-forward--inline-message-count nil)))
  (should (null (qq-forward--inline-message-count '())))
  (should (equal (qq-forward--inline-message-count []) 0))
  (should (equal (qq-forward--inline-message-count
                  (list (qq-forward-test--native-message "0" "a")
                        (qq-forward-test--native-message "1" "b")))
                 2))
  (should (equal (qq-forward--count-from-presentation
                  '((summary . "查看3条转发消息")))
                 3))
  (should (null (qq-forward--count-from-presentation
                 '((summary . "no count here"))))))

(ert-deftest qq-forward-root-event-adapter-qualifies-message-source ()
  (let* ((legacy
          '((type . "forward")
            (data . ((message_id . "9007199254742007089")
                     (content . "legacy inline data is ignored")))))
         (group-a
          (qq-forward-event-segment-to-internal legacy "group:20001"))
         (group-b
          (qq-forward-event-segment-to-internal legacy "group:20002"))
         (source-a (qq-forward--segment-source group-a))
         (source-b (qq-forward--segment-source group-b)))
    (should (equal source-a
                   (qq-forward-test--message-source
                    "9007199254742007089" "20001")))
    (should (equal source-b
                   (qq-forward-test--message-source
                    "9007199254742007089" "20002")))
    (should-not (equal source-a source-b))
    (should-not (qq-forward--inline-cell group-a))))

(ert-deftest qq-forward-root-event-adapter-translates-ark-resource ()
  (let* ((legacy
          '((type . "card")
            (data . ((kind . "forward")
                     (res_id . "resource-a")
                     (title . "Team history")
                     (summary . "Two messages")))))
         (segment
          (qq-forward-event-segment-to-internal legacy "group:20001"))
         (data (alist-get 'data segment)))
    (should (equal (alist-get 'reference data)
                   '((kind . "resource") (resource_id . "resource-a"))))
    (should (equal (alist-get 'title (alist-get 'presentation data))
                   "Team history"))))

(ert-deftest qq-forward-message-mapper-separates-entry-and-message-identity ()
  (let* ((without-message-id
          (qq-forward-native-message-to-internal
           (qq-forward-test--native-message "1.2" "hello")))
         (with-message-id
          (qq-forward-native-message-to-internal
           (qq-forward-test--native-message
            "1.3" "world" :message-id "9007199254742007089"))))
    (should (equal (alist-get 'id without-message-id) "1.2"))
    (should-not (alist-get 'server-id without-message-id))
    (should (equal (alist-get 'id with-message-id) "1.3"))
    (should (equal (alist-get 'server-id with-message-id)
                   "9007199254742007089"))))

(ert-deftest qq-forward-message-mapper-supports-anonymous-and-recalled ()
  (let* ((native
          (qq-forward-test--native-message
           "2" "secret"
           :message-id "9007199254742007090"
           :state "recalled"
           :sender '((kind . "anonymous") (name . "Visitor"))
           :origin '((kind . "unknown"))))
         (message (qq-forward-native-message-to-internal native)))
    (should (equal (alist-get 'id message) "2"))
    (should (equal (alist-get 'sender-name message) "Visitor"))
    (should-not (alist-get 'sender-id message))
    (should (eq (alist-get 'status message) 'recalled))
    (should-not (alist-get 'segments message))
    (should (equal (alist-get 'preview message) "[message recalled]"))
    (should (equal (alist-get 'origin message) '((kind . "unknown"))))))

(ert-deftest qq-forward-video-mapper-preserves-four-remote-states ()
  (dolist (case
           '(("available" "https://example.test/video.mp4")
             ("expired" nil)
             ("unavailable" nil)
             ("unresolved" nil)))
    (pcase-let ((`(,state ,url) case))
      (let* ((remote (if url
                         `((state . ,state) (url . ,url))
                       `((state . ,state))))
             (internal
              (qq-forward-native-segment-to-internal
               `((kind . "video")
                 (payload . ((file . "video.mp4")
                              (local_path . "/tmp/video.mp4")
                              (size . 42)
                              (name . "clip")
                              (thumb . "thumb.jpg")
                              (remote . ,remote))))))
             (data (alist-get 'data internal)))
        (should (equal (alist-get 'path data) "/tmp/video.mp4"))
        (should (= (alist-get 'file_size data) 42))
        (should (equal (alist-get 'remote_status data) state))
        (if url
            (should (equal (alist-get 'url data) url))
          (should-not (assq 'url data)))))))

(ert-deftest qq-forward-ordinary-file-id-is-preserved-verbatim ()
  (let* ((segment
          '((kind . "image")
            (payload . ((file . "opaque-name")
                        (file_id . "authoritative-download-token")))))
         (internal (qq-forward-native-segment-to-internal segment)))
    (should (equal (alist-get 'file_id (alist-get 'data internal))
                   "authoritative-download-token"))))

(ert-deftest qq-forward-unsupported-mapper-never-renders-raw ()
  (let* ((internal
          (qq-forward-native-segment-to-internal
           '((kind . "unsupported")
             (payload . ((native_keys . ["mystery"])
                         (summary . "mystery element")
                         (raw . ((secret . "DO-NOT-RENDER"))))))))
         (text (alist-get 'text (alist-get 'data internal))))
    (should (string-match-p "mystery element" text))
    (should-not (string-match-p "DO-NOT-RENDER" text))
    (should-error
     (qq-forward-native-segment-to-internal
      `((kind . "unsupported")
        (payload . ((native_keys . nil)
                    (summary . "bad")
                    (raw . ,(current-buffer))))))
     :type 'error)))

(ert-deftest qq-forward-reply-targets-use-explicit-identity-domain ()
  (let* ((first
          (qq-forward-test--native-message
           "1" "original" :message-id "9007199254742007031"))
         (duplicate-message-id
          (qq-forward-test--native-message
           "1.1" "same source id"
           :message-id "9007199254742007031"))
         (entry-reply
          (qq-forward-test--native-message
           "2" "entry answer" :segments
           '(((kind . "reply")
              (payload . ((target . ((kind . "entry")
                                     (entry_id . "1"))))))
             ((kind . "text") (payload . ((text . "entry answer")))))))
         (message-reply
          (qq-forward-test--native-message
           "3" "message answer" :segments
           '(((kind . "reply")
              (payload . ((target . ((kind . "unresolved")
                                     (message_id
                                      . "9007199254742007031"))))))
             ((kind . "text") (payload . ((text . "message answer")))))))
         (wrong-domain
          (qq-forward-test--native-message
           "4" "wrong domain" :segments
           '(((kind . "reply")
              (payload . ((target . ((kind . "entry")
                                     (entry_id
                                      . "9007199254742007031"))))))
             ((kind . "text") (payload . ((text . "wrong domain")))))))
         (qq-forward--messages
          (qq-forward--normalize-messages
           (list first duplicate-message-id
                 entry-reply message-reply wrong-domain))))
    (let ((messages-by-entry
           (qq-forward--messages-by-entry qq-forward--messages))
          (entry-target
           (qq-forward--message-reply-target (nth 2 qq-forward--messages)))
          (unresolved-target
           (qq-forward--message-reply-target (nth 3 qq-forward--messages)))
          (wrong-target
           (qq-forward--message-reply-target (nth 4 qq-forward--messages))))
      (should (eq (gethash (cdr entry-target) messages-by-entry)
                  (car qq-forward--messages)))
      (should (equal unresolved-target
                     '(unresolved . "9007199254742007031")))
      ;; message_id is diagnostic metadata here.  Even when two snapshots
      ;; share it, unresolved must not guess the first row.
      (should-not (gethash (cdr unresolved-target) messages-by-entry))
      (should-not (gethash (cdr wrong-target) messages-by-entry)))))

(ert-deftest qq-forward-remote-viewer-passes-source-and-isolates-locators ()
  (qq-forward-test--with-clean-viewers
    (let ((source-a
           (qq-forward-test--message-source
            "9007199254742007001" "20001"))
          (source-b
           (qq-forward-test--message-source
            "9007199254742007001" "20002"))
          requests)
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (source callback &optional _errback)
                   (push (copy-tree source) requests)
                   (funcall callback
                            (list (qq-forward-test--native-message
                                   "1" "loaded"))))))
        (save-window-excursion
          (let ((first (qq-forward-open source-a))
                (second (qq-forward-open source-b)))
            (should-not (eq first second))
            (should-not (equal (buffer-name first) (buffer-name second)))
            (should (member source-a requests))
            (should (member source-b requests))
            (with-current-buffer first
              (should (equal qq-forward--source source-a)))
            (with-current-buffer second
              (should (equal qq-forward--source source-b)))))))))

(ert-deftest qq-forward-inline-empty-content-never-refetches ()
  (qq-forward-test--with-clean-viewers
    (let ((requests 0)
          (segment (qq-forward-test--inline-segment nil)))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (&rest _) (cl-incf requests))))
        (save-window-excursion
          (let ((buffer (qq-forward-open-segment segment)))
            (with-current-buffer buffer
              (should qq-forward--inline-p)
              (should qq-forward--loaded-p)
              (should (string-match-p "empty chat history"
                                      (buffer-string)))))
          (should (= requests 0)))))))

(ert-deftest qq-forward-nested-remote-reference-uses-native-source ()
  (qq-forward-test--with-clean-viewers
    (let* ((source
            '((kind . "resource") (resource_id . "nested-resource")))
           (native
            (qq-forward-test--native-message
             "1" "outer" :segments
             `(((kind . "forward")
                (payload . ((content . ((kind . "remote")
                                        (reference . ,source)))))))))
           (internal (qq-forward-native-message-to-internal native))
           (segment (car (alist-get 'segments internal)))
           captured)
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (called-source callback &optional _errback)
                   (setq captured called-source)
                   (funcall callback nil))))
        (save-window-excursion
          (qq-forward-open-segment segment))
        (should (equal captured source))))))

(ert-deftest qq-forward-nested-card-context-sends-exact-native-source ()
  (qq-forward-test--with-clean-viewers
    (let* ((source (qq-forward-test--context-source))
           (native
            (qq-forward-test--native-message
             "1" "outer" :segments
             `(((kind . "forward-card")
                (payload . ((reference . ,source)
                            (presentation . nil)))))))
           (internal (qq-forward-native-message-to-internal native))
           (segment (car (alist-get 'segments internal)))
           captured-action captured-params)
      (cl-letf (((symbol-function 'qq-api-call)
                 (lambda (action params callback &optional _errback)
                   (setq captured-action action
                         captured-params params)
                   (funcall callback '((data . ((messages . nil)))))
                   'sent)))
        (save-window-excursion
          (let ((buffer (qq-forward-open-segment segment)))
            (with-current-buffer buffer
              (should (eq qq-forward--lookup-kind 'context))
              (should (equal qq-forward--lookup-id
                             "Nested · u_group-peer · 9007199254742007002"))
              (should (string-match-p
                       (regexp-quote qq-forward--lookup-id)
                       (buffer-string))))))
      (should (equal captured-action "emacs_get_forward"))
      (should (equal captured-params `((source . ,source))))))))

(ert-deftest qq-forward-context-buffer-key-includes-complete-source ()
  (qq-forward-test--with-clean-viewers
    (let ((source-a (qq-forward-test--context-source "u_group-a"))
          (source-b (qq-forward-test--context-source "u_group-b"))
          (requests 0))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional _errback)
                   (cl-incf requests)
                   (funcall callback nil)))
                ((symbol-function 'sxhash-equal) (lambda (_value) 1)))
        (save-window-excursion
          (let ((first (qq-forward-open source-a))
                (second (qq-forward-open source-b)))
            (should-not (eq first second))
            (should-not (equal (buffer-name first) (buffer-name second)))
            (should (= requests 2))
            (cl-mapc
             (lambda (buffer source)
               (with-current-buffer buffer
                 (should qq-forward--loaded-p)
                 (should-not qq-forward--loading)
                 (should
                  (equal
                   (appkit-view-id (appkit-current-view))
                   (qq-forward--view-id
                    (qq-forward--source-buffer-key source))))))
             (list first second) (list source-a source-b))))))))

(ert-deftest qq-forward-context-buffer-key-canonicalizes-object-order ()
  (qq-forward-test--with-clean-viewers
    (let* ((source (qq-forward-test--context-source))
           (reordered
            `((parent_message_id . ,(alist-get 'parent_message_id source))
              (root_message_id . ,(alist-get 'root_message_id source))
              (peer . ((guild_id . "")
                       (peer_uid . "u_group-peer")
                       (chat_type . 2)))
              (kind . "context")))
           (requests 0))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional _errback)
                   (cl-incf requests)
                   (funcall callback nil))))
        (save-window-excursion
          (let ((first (qq-forward-open source))
                (second (qq-forward-open reordered)))
            (should (eq first second))
            (should (= requests 1))
            (with-current-buffer first
              (should (eq (appkit-current-view)
                          (appkit-view-for-id
                           (qq-runtime-app)
                           (qq-forward--view-id
                            (qq-forward--source-buffer-key source)))))
              (should (equal qq-forward--source source))
              (should (equal qq-forward--buffer-key
                             (qq-forward--source-buffer-key source))))))))))

(ert-deftest qq-forward-inline-nested-content-renders-locally ()
  (qq-forward-test--with-clean-viewers
    (let* ((inner
            (list (qq-forward-test--native-message "1.1" "inner")))
           (native-segment
            `((kind . "forward")
              (payload . ((content . ((kind . "inline")
                                      (messages . ,inner)))))))
           (segment (qq-forward-native-segment-to-internal native-segment))
           (requests 0))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (&rest _) (cl-incf requests))))
        (save-window-excursion
          (let ((buffer (qq-forward-open-segment segment)))
            (with-current-buffer buffer
              (should qq-forward--inline-p)
              (should (string-match-p "inner" (buffer-string)))))
          (should (= requests 0)))))))

(ert-deftest qq-forward-card-is-wholly-clickable-without-open-label ()
  (let* ((segment
          '((type . "card")
            (data . ((kind . "forward")
                     (reference . ((kind . "resource")
                                   (resource_id . "resource-a")))
                     (presentation . ((source . "Alice")
                                      (title . "Two messages")
                                      (summary . "A short history")))))))
         opened)
    (with-temp-buffer
      (cl-letf (((symbol-function 'qq-forward-open-segment)
                 (lambda (clicked) (setq opened clicked))))
        (qq-forward-insert-segment segment nil nil)
        (should-not (string-match-p "\\[Open\\]" (buffer-string)))
        (goto-char (point-min))
        (should (button-at (point)))
        (push-button (point))
        (should (equal opened segment))
        (goto-char (1- (point-max)))
        (should (get-text-property (point) 'qq-forward-segment))))))

(ert-deftest qq-forward-entry-header-includes-sender-avatar ()
  "Forward entries render user avatars like the official GUI / qq-chat."
  (qq-forward-test--with-clean-viewers
    (let* ((messages
            (list
             (qq-forward-test--native-message "1" "hello")
             (qq-forward-test--native-message
              "2" "ghost"
              :sender '((kind . "anonymous") (name . "anonymous")))))
           avatar-calls)
      (cl-letf (((symbol-function 'qq-media-avatar-display-string)
                 (lambda (_user-id)
                   (ert-fail "forward headings must use shared avatar slices")))
                ((symbol-function 'qq-media-avatar-image)
                 (lambda (user-id)
                   (push user-id avatar-calls)
                   nil))
                ((symbol-function 'qq-chat--compute-fill-column)
                 (lambda (&optional _window) 50)))
        (save-window-excursion
          (let ((buffer
                 (qq-forward-open-segment
                  (qq-forward-test--inline-segment messages))))
            (with-current-buffer buffer
              (should (equal (appkit-chat-timeline-keys) '("1" "2")))
              (let ((first (appkit-chat-timeline-key-position "1"))
                    (second (appkit-chat-timeline-key-position "2")))
                (let ((first-prefix (get-text-property first 'line-prefix))
                      (second-prefix (get-text-property second 'line-prefix)))
                  (should (equal (substring-no-properties first-prefix 0 1)
                                 "@"))
                  (should (equal
                           (get-text-property
                            0 'qq-chat-avatar-sender-id first-prefix)
                           "10001"))
                  (should (equal (substring-no-properties second-prefix 0 1)
                                 "@"))))
              (should (equal avatar-calls '("10001")))
              (should (string-match-p "Alice" (buffer-string)))
              (should (string-match-p "anonymous"
                                      (buffer-string)))
              (goto-char (point-min))
              (search-forward (qq-chat--format-time 1710000000))
              (should (equal
                       (get-text-property (1- (match-beginning 0)) 'display)
                       `(space :align-to
                               ,(- 50
                                   (string-width
                                    (qq-chat--format-time
                                     1710000000)))))))))))))

(ert-deftest qq-forward-timeline-keys-are-native-entry-ids ()
  "Duplicate message_id metadata must never collapse forwarded entries."
  (qq-forward-test--with-clean-viewers
    (let* ((message-id "9007199254742007031")
           (messages
            (list
             (qq-forward-test--native-message
              "1.2" "first" :message-id message-id)
             (qq-forward-test--native-message
              "9" "second" :message-id message-id))))
      (save-window-excursion
        (let ((buffer
               (qq-forward-open-segment
                (qq-forward-test--inline-segment messages))))
          (with-current-buffer buffer
            (let ((view (appkit-current-view)))
              (should (equal (appkit-view-parts view) '(timeline)))
              (should (equal (appkit-chat-timeline-keys) '("1.2" "9")))
              (should-not
               (eq (appkit-chat-timeline-node "1.2")
                   (appkit-chat-timeline-node "9")))
              (should-not (appkit-chatbuf-prompt-start-position))
              (should-not (appkit-chatbuf-input-start-position)))
            (goto-char (point-min))
            (qq-forward-next-message)
            (should (equal (appkit-chat-timeline-key-at-point) "1.2"))
            (qq-forward-next-message)
            (should (equal (appkit-chat-timeline-key-at-point) "9"))
            (qq-forward-previous-message)
            (should (equal (appkit-chat-timeline-key-at-point)
                           "1.2"))))))))

(ert-deftest qq-forward-status-row-keeps-one-stable-timeline-node ()
  (qq-forward-test--with-clean-viewers
    (let ((source (qq-forward-test--message-source
                   "9007199254742007040" "20001"))
          success errback)
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional error-callback)
                   (setq success callback
                         errback error-callback))))
        (save-window-excursion
          (let ((buffer (qq-forward-open source)))
            (with-current-buffer buffer
              (should (equal (appkit-chat-timeline-keys)
                             (list qq-forward--status-row-key)))
              (let ((status-node
                     (appkit-chat-timeline-node qq-forward--status-row-key)))
                (should (string-match-p "Loading chat history"
                                        (buffer-string)))
                (funcall success nil)
                (should (eq status-node
                            (appkit-chat-timeline-node
                             qq-forward--status-row-key)))
                (should (string-match-p "empty chat history"
                                        (buffer-string)))
                (qq-forward-refresh)
                (should (eq status-node
                            (appkit-chat-timeline-node
                             qq-forward--status-row-key)))
                (should (string-match-p "Loading chat history"
                                        (buffer-string)))
                (funcall errback nil "native failure")
                (should (eq status-node
                            (appkit-chat-timeline-node
                             qq-forward--status-row-key)))
                (should (string-match-p "native failure"
                                        (buffer-string)))
                (goto-char (point-min))
                (let ((before (point)))
                  (qq-forward-next-message)
                  (should (= (point) before)))))))))))

(ert-deftest qq-forward-refresh-keeps-accepted-rows-and-anchor ()
  (qq-forward-test--with-clean-viewers
    (let ((source (qq-forward-test--message-source
                   "9007199254742007041" "20001"))
          success errback)
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional error-callback)
                   (setq success callback
                         errback error-callback))))
        (save-window-excursion
          (let ((buffer (qq-forward-open source)))
            (funcall success
                     (list (qq-forward-test--native-message "1" "a")
                           (qq-forward-test--native-message "2" "b")))
            (with-current-buffer buffer
              (let* ((keys '("1" "2"))
                     (nodes (mapcar #'appkit-chat-timeline-node keys)))
                (goto-char (appkit-chat-timeline-key-position "2"))
                (forward-char 2)
                (let ((point-before (point)))
                  (qq-forward-refresh)
                  (should qq-forward--loading)
                  (should (equal (appkit-chat-timeline-keys) keys))
                  (should (cl-every
                           #'identity
                           (cl-mapcar
                            #'eq nodes
                            (mapcar #'appkit-chat-timeline-node keys))))
                  (should (= (point) point-before))
                  (should (equal (appkit-chat-timeline-key-at-point)
                                 "2"))
                  (funcall errback nil "refresh failure")
                  (should-not qq-forward--loading)
                  (should (equal (appkit-chat-timeline-keys)
                                 (append keys
                                         (list qq-forward--status-row-key))))
                  (should (cl-every
                           #'identity
                           (cl-mapcar
                            #'eq nodes
                            (mapcar #'appkit-chat-timeline-node keys))))
                  (should (= (point) point-before))
                  (should (equal (appkit-chat-timeline-key-at-point)
                                 "2")))))))))))

(ert-deftest qq-forward-refresh-cancels-and-ignores-stale-request ()
  (qq-forward-test--with-clean-viewers
    (let ((source (qq-forward-test--message-source
                   "9007199254742007043" "20001"))
          callbacks canceled
          (request-count 0))
      (cl-letf (((symbol-function 'qq-api-get-forward)
                 (lambda (_source callback &optional _errback)
                   (push callback callbacks)
                   (intern (format "request-%d" (cl-incf request-count)))))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (request)
                   (push request canceled))))
        (save-window-excursion
          (let ((buffer (qq-forward-open source)))
            (with-current-buffer buffer
              (should (eq qq-forward--request 'request-1))
              (qq-forward-refresh)
              (should (equal canceled '(request-1)))
              (should (eq qq-forward--request 'request-2))
              ;; The callback owned by request-1 must not mutate request-2.
              (funcall (cadr callbacks)
                       (list (qq-forward-test--native-message "1" "stale")))
              (should qq-forward--loading)
              (should-not qq-forward--messages)
              (should (eq qq-forward--request 'request-2))
              (fundamental-mode)
              (should (equal canceled '(request-2 request-1))))
            (kill-buffer buffer)))))))

(ert-deftest qq-forward-reply-context-updates-without-replacing-row ()
  (qq-forward-test--with-clean-viewers
    (let ((source (qq-forward-test--message-source
                   "9007199254742007042" "20001"))
          success)
      (cl-labels
          ((snapshot
            (text)
            (list
             (qq-forward-test--native-message "1" text)
             (qq-forward-test--native-message
              "2" "answer"
              :segments
              '(((kind . "reply")
                 (payload . ((target . ((kind . "entry")
                                        (entry_id . "1"))))))
                ((kind . "text") (payload . ((text . "answer")))))))))
        (cl-letf (((symbol-function 'qq-api-get-forward)
                   (lambda (_source callback &optional _errback)
                     (setq success callback))))
          (save-window-excursion
            (let ((buffer (qq-forward-open source)))
              (funcall success (snapshot "original"))
              (with-current-buffer buffer
                (let ((reply-node (appkit-chat-timeline-node "2")))
                  (should (string-match-p "↪ Alice: original"
                                          (buffer-string)))
                  (qq-forward-refresh)
                  (funcall success (snapshot "edited"))
                  (should (eq reply-node
                              (appkit-chat-timeline-node "2")))
                  (should (string-match-p "↪ Alice: edited"
                                          (buffer-string)))
                  (should-not (string-match-p "↪ Alice: original"
                                              (buffer-string)))
                  (goto-char (appkit-chat-timeline-key-position "2"))
                  (search-forward "↪")
                  (goto-char (match-beginning 0))
                  (should (button-at (point)))
                  (push-button (point))
                  (should (equal (appkit-chat-timeline-key-at-point)
                                 "1")))))))))))

(ert-deftest qq-forward-media-cache-update-redisplays-only-affected-entry ()
  "Media invalidation redraws only its dependent keyed timeline row."
  (qq-forward-test--with-clean-viewers
    (let* ((media-key "preview:file-image:media-a.png")
           (messages
            (list
             (qq-forward-test--native-message "1" "first-stable")
             (qq-forward-test--native-message
              "1.1" "image"
              :segments
              '(((kind . "image")
                 (payload . ((file . "media-a.png")
                             (url . "https://example.test/a.png"))))))
             (qq-forward-test--native-message "2" "third-stable")))
           (original-printer (symbol-function 'qq-forward--row-printer))
           (fetching t)
           printed-keys)
      (cl-letf (((symbol-function 'qq-media-segment-cache-keys)
                 (lambda (segment)
                   (when (equal (alist-get 'type segment) "image")
                     (list media-key))))
                ((symbol-function 'qq-media-avatar-image)
                 (lambda (_user-id) nil))
                ((symbol-function 'qq-media-segment-preview-capable-p)
                 (lambda (_segment) t))
                ((symbol-function 'qq-media-segment-preview-image)
                 (lambda (_segment) nil))
                ((symbol-function 'qq-media-segment-preview-fetching-p)
                 (lambda (_segment) fetching))
                ((symbol-function 'qq-media-segment-capabilities)
                 (lambda (_segment)
                   (list :status (if fetching "loading" "unavailable")
                         :open nil :download nil
                         :save nil :copy-url nil)))
                ((symbol-function 'qq-forward--row-printer)
                 (lambda (row)
                   (push (appkit-chat-timeline-row-key row) printed-keys)
                   (funcall original-printer row))))
        (save-window-excursion
          (let ((buffer
                 (qq-forward-open-segment
                  (qq-forward-test--inline-segment messages))))
            (with-current-buffer buffer
              (let* ((view (appkit-current-view))
                     (keys '("1" "1.1" "2"))
                     (nodes (mapcar #'appkit-chat-timeline-node keys))
                     (first-position
                      (appkit-chat-timeline-key-position "1"))
                     (header-before
                      (buffer-substring-no-properties
                       (point-min) first-position)))
                (should (equal (appkit-chat-timeline-keys) keys))
                (goto-char (appkit-chat-timeline-key-position "2"))
                (forward-char 3)
                (let ((column-before (current-column)))
                  (setq printed-keys nil
                        fetching nil)
                  (qq-forward--handle-media-cache-update media-key)
                  (appkit-sync-invalidations view)
                  (should (equal printed-keys '("1.1")))
                  (should (equal (appkit-chat-timeline-key-at-point)
                                 "2"))
                  (should (= (current-column) column-before)))
                (should
                 (cl-every
                  #'identity
                  (cl-mapcar #'eq nodes
                             (mapcar #'appkit-chat-timeline-node keys))))
                (should (equal
                         (buffer-substring-no-properties
                          (point-min)
                          (appkit-chat-timeline-key-position "1"))
                         header-before))
                (setq printed-keys nil)
                (qq-forward--handle-media-cache-update "preview:other:x")
                (appkit-sync-invalidations view)
                (qq-forward--handle-media-cache-update nil)
                (should-not printed-keys)))))))))

(provide 'qq-forward-test)

;;; qq-forward-test.el ends here
