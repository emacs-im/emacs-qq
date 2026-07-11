;;; qq-modes-test.el --- Tests for QQ mode-line integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'qq-modes)

(ert-deftest qq-mode-line-counts-unmuted-and-priority-mentions ()
  (cl-letf (((symbol-function 'qq-state-sessions)
             (lambda ()
               '(((unread-count . 3) (muted-p . nil))
                 ((unread-count . 8) (muted-p . t)
                  (unread-at-me-message-id . "11"))
                 ((unread-count . 2) (muted-p . nil)
                  (unread-at-all-message-seq . "12"))))))
    (should (equal '(5 . 2) (qq-mode-line--counts)))
    (should (equal " 5" (substring-no-properties
                          (qq-mode-line-unread-unmuted))))
    (should (equal " @2" (substring-no-properties
                           (qq-mode-line-mentions))))))

(ert-deftest qq-mode-line-mode-installs-and-removes-state-hook ()
  (let ((mode-line-misc-info nil)
        (qq-state-change-hook nil))
    (unwind-protect
        (progn
          (qq-mode-line-mode 1)
          (should (memq 'qq-mode-line-format mode-line-misc-info))
          (should (memq #'qq-mode-line-update qq-state-change-hook)))
      (qq-mode-line-mode -1))
    (should-not (memq 'qq-mode-line-format mode-line-misc-info))
    (should-not (memq #'qq-mode-line-update qq-state-change-hook))))

(provide 'qq-modes-test)

;;; qq-modes-test.el ends here
