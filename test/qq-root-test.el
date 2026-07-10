;;; qq-root-test.el --- Tests for qq-root -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-root)
(require 'qq-state)

(defmacro qq-root-test-with-reset (&rest body)
  "Run BODY with a clean in-memory qq-state store."
  `(let ((qq-state-change-hook nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(ert-deftest qq-root-distinguishes-important-and-muted-unread-sessions ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "group:important"
    '((unread-count . 3) (muted-p . nil))
    nil)
   (qq-state-upsert-session
    "group:muted"
    '((unread-count . 9) (muted-p . t))
    nil)
   (let ((metrics (qq-root--activity-metrics)))
     (should (= 2 (plist-get metrics :unread)))
     (should (= 1 (plist-get metrics :important)))
     (should (= 1 (plist-get metrics :muted))))))

(ert-deftest qq-root-renders-muted-session-badge-without-warning-face ()
  (let* ((session '((key . "group:muted")
                    (type . group)
                    (unread-count . 9)
                    (muted-p . t)
                    (last-message-preview . "quiet message")))
         (row (qq-root--session-one-line-row session)))
    (should (equal "[mute:9] quiet message"
                   (disco-view-one-line-row-preview row)))
    (should (= 9 (disco-view-one-line-row-preview-leading-length row)))
    (should (eq 'shadow
                (disco-view-one-line-row-preview-leading-face row)))
    (should-not (disco-view-one-line-row-time-tail-face row))))

(ert-deftest qq-root-shows-muted-state-without-unread-messages ()
  (should (equal "[mute] (no preview yet)"
                 (qq-root--session-preview-text
                  '((muted-p . t) (unread-count . 0))))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
