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

(ert-deftest qq-root-background-render-reuses-last-visible-width ()
  (with-temp-buffer
    (qq-root-mode)
    (let ((qq-root--fill-column 88)
          (compute-calls 0))
      (cl-letf (((symbol-function 'qq-root--selected-window) (lambda () nil))
                ((symbol-function 'qq-root--display-window) (lambda () nil))
                ((symbol-function 'qq-root--compute-fill-column)
                 (lambda (&optional _window)
                   (cl-incf compute-calls)
                   42)))
        (should (= (qq-root--render-fill-column) 88))
        (qq-root-render)
        (should (= qq-root--fill-column 88))
        (should (= compute-calls 0))))))

(ert-deftest qq-root-selected-window-refreshes-cached-width ()
  (with-temp-buffer
    (qq-root-mode)
    (let ((qq-root--fill-column 88)
          rendered)
      (cl-letf (((symbol-function 'qq-root--selected-window)
                 (lambda () 'root-window))
                ((symbol-function 'qq-root--compute-fill-column)
                 (lambda (&optional window)
                   (should (eq window 'root-window))
                   104))
                ((symbol-function 'qq-root-render)
                 (lambda () (setq rendered t))))
        (should (qq-root--reflow-visible))
        (should (= qq-root--fill-column 104))
        (should rendered)))))

(ert-deftest qq-root-render-coalesces-reentrant-update ()
  (with-temp-buffer
    (qq-root-mode)
    (let ((sessions '(((key . "group:1")
                       (type . group)
                       (title . "Group")
                       (last-message-preview . "hello"))))
          (insertions 0)
          scheduled)
      (cl-letf (((symbol-function 'qq-state-sessions) (lambda () sessions))
                ((symbol-function 'qq-root--insert-session-line)
                 (lambda (_session)
                   (cl-incf insertions)
                   (when (= insertions 1)
                     (qq-root-render))
                   (insert "row\n")))
                ((symbol-function 'qq-root--rerender-open-root)
                 (lambda (&rest _) (setq scheduled t))))
        (qq-root-render)
        (should (= insertions 1))
        (should scheduled)
        (should-not qq-root--rendering)
        (should-not qq-root--rerender-pending)
        (should (= 1 (how-many "^row$" (point-min) (point-max))))))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
