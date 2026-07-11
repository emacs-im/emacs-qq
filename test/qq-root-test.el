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

(ert-deftest qq-root-shows-muted-state-without-messages ()
  (should (equal "[mute]"
                 (qq-root--session-preview-text
                  '((muted-p . t) (unread-count . 0))))))

(ert-deftest qq-root-session-preview-is-always-one-line ()
  (should (equal "first second third"
                 (qq-root--session-preview-text
                  '((last-message-preview . " first\nsecond\r\n  third "))))))

(ert-deftest qq-root-does-not-invent-a-missing-message-preview ()
  (should (equal ""
                 (qq-root--session-preview-text
                  '((last-message-id . "9007199254741004991"))))))

(ert-deftest qq-root-mentions-stay-important-through-muted-groups ()
  (let ((session '((muted-p . t)
                   (unread-count . 9)
                   (unread-at-me-message-seq . "10001")
                   (unread-at-all-message-seq . "10002"))))
    (should (qq-root--session-important-unread-p session))
    (should (equal "[@] [@all] [mute:9] "
                   (qq-root--session-badge session)))))

(ert-deftest qq-root-session-row-keeps-help-without-blanket-hover ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
               (lambda (_session) "#"))
              ((symbol-function 'qq-root--buffer-width) (lambda () 80)))
      (qq-root--insert-session-line
       '((key . "group:1")
         (type . group)
         (title . "Group")
         (last-message-preview . "hello"))))
    (should (equal "Open group:1"
                   (get-text-property (point-min) 'help-echo)))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'mouse-face nil))))

(ert-deftest qq-root-background-sync-reuses-last-visible-width ()
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
        (should (= (qq-root--stable-fill-column) 88))
        (qq-root--sync)
        (should (= qq-root--fill-column 88))
        (should (= compute-calls 0))))))

(ert-deftest qq-root-mode-disables-undo-history ()
  (with-temp-buffer
    (qq-root-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest qq-root-sync-preserves-nodes-and-never-erases-buffer ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (last-message-time . 2) (last-message-preview . "old"))
    nil)
   (qq-state-upsert-session
    "group:2"
    '((type . group) (target-id . "2") (title . "Two")
      (last-message-time . 1) (last-message-preview . "quiet"))
    nil)
   (with-temp-buffer
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (qq-root-mode)
       (qq-root--sync)
       (let ((one-node (gethash '(session . "private:1") qq-root--node-table))
             (two-node (gethash '(session . "group:2") qq-root--node-table)))
         (should one-node)
         (should two-node)
         (qq-state-upsert-session
          "private:1" '((last-message-preview . "updated")) nil)
         (cl-letf (((symbol-function 'erase-buffer)
                    (lambda () (ert-fail "incremental sync erased the buffer"))))
           (qq-root--sync))
         (should (eq one-node
                     (gethash '(session . "private:1") qq-root--node-table)))
         (should (eq two-node
                     (gethash '(session . "group:2") qq-root--node-table)))
         (should (string-match-p "updated" (buffer-string)))
         (should-not (string-match-p "old" (buffer-string)))
         (should (eq buffer-undo-list t)))))))

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
                ((symbol-function 'qq-root--sync)
                 (lambda (&optional _force-keys) (setq rendered t))))
        (should (qq-root--reflow-visible))
        (should (= qq-root--fill-column 104))
        (should rendered)))))

(ert-deftest qq-root-media-update-targets-only-owning-session-node ()
  (let ((sessions
         '(((key . "private:1") (type . private) (target-id . "1"))
           ((key . "group:2") (type . group) (target-id . "2"))))
        calls)
    (cl-letf (((symbol-function 'qq-state-sessions) (lambda () sessions))
              ((symbol-function 'qq-root--invalidate-open-root)
               (lambda (&optional keys) (push keys calls))))
      (qq-root--handle-media-cache-update "avatar:1")
      (qq-root--handle-media-cache-update "unrelated")
      (qq-root--handle-media-cache-update "group-avatar:2"))
    (should (equal (nreverse calls)
                   '(((session . "private:1"))
                     ((session . "group:2")))))))

(ert-deftest qq-root-state-events-have-explicit-update-paths ()
  (let (syncs invalidations (headers 0))
    (cl-letf (((symbol-function 'qq-root--sync-open-root)
               (lambda () (push t syncs)))
              ((symbol-function 'qq-root--invalidate-open-root)
               (lambda (keys) (push keys invalidations)))
              ((symbol-function 'qq-root--refresh-header)
               (lambda () (cl-incf headers))))
      (qq-root--handle-state-change '(:type connection))
      (qq-root--handle-state-change
       '(:type action :session-key "group:1"))
      (qq-root--handle-state-change
       '(:type message :session-key "group:1"))
      (qq-root--handle-state-change '(:type heartbeat)))
    (should (= headers 1))
    (should (equal syncs '(t)))
    (should (equal invalidations
                   '(((session . "group:1")))))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
