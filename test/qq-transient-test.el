;;; qq-transient-test.el --- Tests for qq-transient -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-chat)
(require 'qq-root)
(require 'qq-state)
(require 'qq-transient)

(defmacro qq-transient-test-with-reset (&rest body)
  "Run BODY with clean qq state."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(ert-deftest qq-transient-prefixes-are-commands ()
  (should (commandp #'qq-chat-transient))
  (should (commandp #'qq-chat-message-transient))
  (should (commandp #'qq-chat-attach-transient))
  (should (commandp #'qq-root-transient)))

(ert-deftest qq-transient-message-inapt-without-point-message ()
  (qq-transient-test-with-reset
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (goto-char (point-min))
     (should (qq-transient--no-message-at-point-p))
     (should (qq-transient--reply-inapt-p))
     (should (qq-transient--recall-inapt-p)))))

(ert-deftest qq-transient-message-inapt-with-other-users-message ()
  (qq-transient-test-with-reset
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
       (self-p . nil)
       (raw-message . "hello")))
    qq-state--messages-by-session)
   (with-temp-buffer
     (qq-chat-mode)
     (setq qq-chat--session-key "private:10001")
     (qq-chat-render)
     (goto-char (point-min))
     (search-forward "hello")
     (should-not (qq-transient--no-message-at-point-p))
     (should-not (qq-transient--reply-inapt-p))
     ;; Only self messages can be recalled.
     (should (qq-transient--recall-inapt-p))
     (should-not (qq-transient--avatar-inapt-p)))))

(ert-deftest qq-root-binds-transient-menu ()
  (qq-transient-test-with-reset
   (with-temp-buffer
     (qq-root-mode)
     (should (eq (key-binding (kbd "?") t) 'qq-root-transient)))))

(provide 'qq-transient-test)

;;; qq-transient-test.el ends here
