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
