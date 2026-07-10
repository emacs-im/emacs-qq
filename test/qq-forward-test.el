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
    (let ((entry-target
           (qq-forward--message-reply-target (nth 2 qq-forward--messages)))
          (unresolved-target
           (qq-forward--message-reply-target (nth 3 qq-forward--messages)))
          (wrong-target
           (qq-forward--message-reply-target (nth 4 qq-forward--messages))))
      (should (eq (qq-forward--message-by-target entry-target)
                  (car qq-forward--messages)))
      (should (equal unresolved-target
                     '(unresolved . "9007199254742007031")))
      ;; message_id is diagnostic metadata here.  Even when two snapshots
      ;; share it, unresolved must not guess the first row.
      (should-not (qq-forward--message-by-target unresolved-target))
      (should-not (qq-forward--message-by-target wrong-target)))))

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

(provide 'qq-forward-test)

;;; qq-forward-test.el ends here
