;;; qq-root-test.el --- Tests for qq-root -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'qq-root)
(require 'qq-runtime)
(require 'qq-state)

(defmacro qq-root-test-with-reset (&rest body)
  "Run BODY with a clean in-memory qq-state store."
  `(let ((qq-state-change-hook nil))
     (qq-state-reset)
     (unwind-protect
         (progn ,@body)
       (qq-state-reset))))

(defmacro qq-root-test-with-live-view (&rest body)
  "Run BODY in a uniquely named live Appkit root view.

BODY may refer to the lexical variables `app', `buffer', and `view'."
  (declare (indent 0) (debug t))
  `(let* ((qq-root-buffer-name
           (generate-new-buffer-name " *qq-root-test*"))
          (app (appkit-start-app 'qq :id (make-symbol "qq-root-test")))
          (buffer (get-buffer-create qq-root-buffer-name))
          view)
     (unwind-protect
         (with-current-buffer buffer
           (qq-root-mode)
           (setq view
                 (appkit-attach-view
                  :app app
                  :id 'root
                  :mode 'qq-root-mode
                  :sync-function #'qq-root--sync-invalidations
                  :parts '(header entries geometry)))
           ,@body)
       (when (appkit-app-live-p app)
         (appkit-stop-app app))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(ert-deftest qq-root-distinguishes-important-and-muted-unread-sessions ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "group:10001"
    '((unread-count . 3) (muted-p . nil))
    nil)
   (qq-state-upsert-session
    "group:10002"
    '((unread-count . 9) (muted-p . t))
    nil)
   (let ((metrics (qq-root--activity-metrics)))
     (should (= 2 (plist-get metrics :unread)))
     (should (= 1 (plist-get metrics :important)))
     (should (= 1 (plist-get metrics :muted))))))

(ert-deftest qq-root-renders-muted-unread-in-title-trail ()
  (let* ((session '((key . "group:muted")
                    (type . group)
                    (unread-count . 9)
                    (muted-p . t)
                    (last-message-preview . "quiet message")))
         (row (qq-root--session-one-line-row session))
         (trail (appkit-view-one-line-row-context-trail row)))
    (should (equal "9" (substring-no-properties trail)))
    (should (eq 'qq-root-muted-count (get-text-property 0 'face trail)))
    (should (equal "quiet message"
                   (appkit-view-one-line-row-preview row)))
    (should-not (appkit-view-one-line-row-time-tail-face row))))

(ert-deftest qq-root-muted-session-without-unread-has-no-activity-trail ()
  (let ((session '((muted-p . t) (unread-count . 0))))
    (should (equal "" (qq-root--session-unread-trail session)))
    (should (equal "" (qq-root--session-preview-text session)))))

(ert-deftest qq-root-session-preview-is-always-one-line ()
  (should (equal "first second third"
                 (qq-root--session-preview-text
                  '((last-message-preview . " first\nsecond\r\n  third "))))))

(ert-deftest qq-root-group-preview-shows-latest-sender ()
  (let* ((session '((type . group)
                    (last-message-sender-name . " Alice\n")
                    (last-message-preview . "first\nsecond")))
         (preview-model (qq-root--session-preview-model session))
         (row (qq-root--session-one-line-row session)))
    (should (equal "Alice: first second" (plist-get preview-model :text)))
    (should (= 7 (plist-get preview-model :leading-length)))
    (should (eq 'qq-msg-user-title (plist-get preview-model :leading-face)))
    (should (= 7 (appkit-view-one-line-row-preview-leading-length row)))
    (should (eq 'qq-msg-user-title
                (appkit-view-one-line-row-preview-leading-face row)))))

(ert-deftest qq-root-private-preview-shows-sender-only-when-outgoing ()
  (let ((incoming '((type . private)
                    (last-message-sender-name . "Alice")
                    (last-message-self-p . nil)
                    (last-message-preview . "hello")))
        (outgoing '((type . private)
                    (last-message-sender-name . "Me")
                    (last-message-self-p . t)
                    (last-message-preview . "hello"))))
    (should (equal "hello" (qq-root--session-preview-text incoming)))
    (should (equal "Me: hello" (qq-root--session-preview-text outgoing)))
    (should (eq 'qq-msg-self-title
                (plist-get (qq-root--session-preview-model outgoing)
                           :leading-face)))))

(ert-deftest qq-root-service-and-dataline-previews-omit-sender ()
  (dolist (type '(service dataline))
    (should
     (equal "Henrik: subject"
            (qq-root--session-preview-text
             `((type . ,type)
               (last-message-sender-name . "QQ Mail")
               (last-message-self-p . t)
               (last-message-preview . "Henrik: subject")))))))

(ert-deftest qq-root-does-not-invent-a-missing-message-preview ()
  (should (equal ""
                 (qq-root--session-preview-text
                  '((type . group)
                    (last-message-id . "9007199254741004991")
                    (last-message-sender-name . "Alice"))))))

(ert-deftest qq-root-mentions-stay-important-through-muted-groups ()
  (let ((session '((muted-p . t)
                   (unread-count . 9)
                   (unread-at-me-message-seq . "10001")
                   (unread-at-all-message-seq . "10002"))))
    (should (qq-root--session-important-unread-p session))
    (let ((trail (qq-root--session-unread-trail session)))
      (should (equal "9 @ @all" (substring-no-properties trail)))
      (should (eq 'qq-root-muted-count
                  (get-text-property 0 'face trail)))
      (should (eq 'qq-root-mention-count
                  (get-text-property 2 'face trail)))
      (should (eq 'qq-root-mention-count
                  (get-text-property 4 'face trail))))))

(ert-deftest qq-root-inserts-unread-count-inside-title-brackets ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
               (lambda (_session) "#"))
              ((symbol-function 'qq-root--buffer-width) (lambda () 80)))
      (qq-root--insert-session-line
       '((key . "group:1")
         (type . group)
         (title . "Example Group")
         (unread-count . 3)
         (muted-p . t)
         (last-message-preview . "[image]"))))
    (should (string-match-p
             "\\[Example Group +3\\] \\[image\\]"
             (buffer-substring-no-properties (point-min) (point-max))))))

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
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((qq-root--fill-column 88)
           (compute-calls 0))
       (cl-letf (((symbol-function 'qq-root--selected-window) (lambda () nil))
                 ((symbol-function 'qq-root--display-window) (lambda () nil))
                 ((symbol-function 'qq-root--compute-fill-column)
                  (lambda (&optional _window)
                    (cl-incf compute-calls)
                    42)))
         (should (= (qq-root--stable-fill-column) 88))
         (appkit-invalidate view :structure t)
         (appkit-sync-invalidations view)
         (should (= qq-root--fill-column 88))
         (should (= compute-calls 0)))))))

(ert-deftest qq-root-mode-disables-undo-history ()
  (with-temp-buffer
    (qq-root-mode)
    (should (eq buffer-undo-list t))))

(ert-deftest qq-root-search-chooses-session-before-message-search ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:10001"
    '((type . private) (title . "Alice") (target-id . "10001"))
    nil)
   (qq-state-upsert-session
    "service:mail"
    '((type . service) (title . "QQ Mail"))
    nil)
   (let (call offered)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (_prompt collection &rest _args)
                  (setq offered collection)
                  (caar collection)))
               ((symbol-function 'qq-search-open)
                (lambda (&rest args) (setq call args))))
       (qq-root-search "needle"))
     (should (= (length offered) 1))
     (should (string-match-p "Alice" (caar offered)))
     (should (equal call '("private:10001" "needle"))))))

(ert-deftest qq-root-search-bindings-separate-session-find-and-message-search ()
  (should (eq (lookup-key qq-root-mode-map (kbd "/"))
              #'qq-root-open-session))
  (should (eq (lookup-key qq-root-mode-map (kbd "s"))
              #'qq-root-search)))

(ert-deftest qq-root-projects-navigation-without-a-key-cheat-sheet ()
  (qq-root-test-with-reset
   (with-temp-buffer
     (cl-letf (((symbol-function 'qq-root--buffer-width) (lambda () 80)))
       (let* ((entries (qq-root--project-entries))
              (texts (delq nil (mapcar #'qq-root--entry-text entries))))
         (should-not (seq-some (lambda (text)
                                 (string-match-p "g refresh\\|Press `g`" text))
                               texts)))))))

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
   (qq-root-test-with-live-view
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (appkit-invalidate view :structure t)
       (appkit-sync-invalidations view)
       (let ((one-node (gethash '(session . "private:1") qq-root--node-table))
             (two-node (gethash '(session . "group:2") qq-root--node-table)))
         (should one-node)
         (should two-node)
         (qq-state-upsert-session
         "private:1" '((last-message-preview . "updated")) nil)
         (cl-letf (((symbol-function 'erase-buffer)
                    (lambda () (ert-fail "incremental sync erased the buffer"))))
           (appkit-invalidate view :structure t)
           (appkit-sync-invalidations view))
         (should (eq one-node
                     (gethash '(session . "private:1") qq-root--node-table)))
         (should (eq two-node
                     (gethash '(session . "group:2") qq-root--node-table)))
         (should (string-match-p "updated" (buffer-string)))
         (should-not (string-match-p "old" (buffer-string)))
         (should (eq buffer-undo-list t)))))))

(ert-deftest qq-root-selected-window-refreshes-cached-width ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
    (let ((qq-root--fill-column 88)
          (syncs 0)
          (original-sync (symbol-function 'qq-root--sync-invalidations)))
      (cl-letf (((symbol-function 'qq-root--selected-window)
                 (lambda () 'root-window))
                ((symbol-function 'qq-root--compute-fill-column)
                 (lambda (&optional window)
                   (should (eq window 'root-window))
                   104))
                ((symbol-function 'qq-root--sync-invalidations)
                 (lambda (candidate invalidations)
                   (cl-incf syncs)
                   (funcall original-sync candidate invalidations))))
        (should (qq-root--reflow-visible))
        (should (= syncs 0))
        (should (= qq-root--fill-column 88))
        (should
         (memq 'geometry
               (appkit-invalidations-parts
                (appkit-view-invalidations-ensure view))))
        (should
         (appkit-invalidations-position-p
          (appkit-view-invalidations-ensure view)))
        (appkit-sync-invalidations view)
        (should (= qq-root--fill-column 104))
        (should (= syncs 1)))))))

(ert-deftest qq-root-media-update-targets-only-owning-session-node ()
  (let ((sessions
         '(((key . "private:1") (type . private) (target-id . "1"))
           ((key . "group:2") (type . group) (target-id . "2"))))
        (qq-state-change-hook nil))
    (qq-root-test-with-live-view
      (cl-letf (((symbol-function 'qq-state-sessions) (lambda () sessions)))
        (qq-root--handle-media-cache-update "avatar:1")
        (qq-root--handle-media-cache-update "unrelated")
        (qq-root--handle-media-cache-update "group-avatar:2"))
      (let ((invalidations (appkit-view-invalidations-ensure view)))
        (should
         (equal
          (sort (copy-sequence
                 (appkit-invalidations-entry-keys invalidations))
                (lambda (left right)
                  (string< (cdr left) (cdr right))))
          '((session . "group:2") (session . "private:1"))))
        (should-not (appkit-invalidations-structure-p invalidations))
        (should
         (appkit-handle-alive-p
          (appkit-invalidations-scheduled-handle invalidations)))))))

(ert-deftest qq-root-state-events-have-explicit-update-paths ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((syncs 0)
           (original-sync (symbol-function 'qq-root--sync-invalidations)))
       (cl-letf (((symbol-function 'qq-root--sync-invalidations)
                  (lambda (candidate invalidations)
                    (cl-incf syncs)
                    (funcall original-sync candidate invalidations))))
         (qq-root--handle-state-change '(:type connection))
         (qq-root--handle-state-change
          '(:type action :session-key "group:1"))
         (qq-root--handle-state-change
          '(:type message :session-key "group:1"))
         (qq-root--handle-state-change '(:type heartbeat))
         (should (= syncs 0))
         (should (string-empty-p (buffer-string)))
         (let ((invalidations (appkit-view-invalidations-ensure view)))
           (should (appkit-invalidations-structure-p invalidations))
           (should (equal '(header)
                          (appkit-invalidations-parts invalidations)))
           (should
            (equal '((session . "group:1"))
                   (appkit-invalidations-entry-keys invalidations))))
         (appkit-sync-invalidations view)
         (should (= syncs 1)))))))

(ert-deftest qq-root-action-event-targets-one-entry-without-structure ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (qq-root--handle-state-change
      '(:type action :session-key "private:7"))
     (let ((invalidations (appkit-view-invalidations-ensure view)))
       (should-not (appkit-invalidations-structure-p invalidations))
       (should-not (appkit-invalidations-parts invalidations))
       (should
        (equal '((session . "private:7"))
               (appkit-invalidations-entry-keys invalidations)))))))

(ert-deftest qq-root-queue-wrapper-uses-appkit-request-sync ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let (call)
       (cl-letf (((symbol-function 'appkit-request-sync)
                  (lambda (candidate &rest arguments)
                    (setq call (cons candidate arguments))
                    'owned-timer))
                 ((symbol-function 'appkit-invalidate)
                  (lambda (&rest _args)
                    (ert-fail "root wrapper called appkit-invalidate directly")))
                 ((symbol-function 'appkit-schedule-sync)
                  (lambda (&rest _args)
                    (ert-fail "root wrapper scheduled separately"))))
         (should
          (eq view
              (qq-root--queue-invalidation
               :part 'entries
               :entry '(session . "private:7")
               :position t))))
       (should (eq view (car call)))
       (should (eq 'entries (plist-get (cdr call) :part)))
       (should
        (equal '(session . "private:7")
               (plist-get (cdr call) :entry)))
       (should (eq t (plist-get (cdr call) :position)))))))

(ert-deftest qq-root-header-event-does-not-touch-ewoc-content ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((before (buffer-string)))
       (qq-root--handle-state-change '(:type self-info))
       (let ((invalidations (appkit-view-invalidations-ensure view)))
         (should (equal '(header)
                        (appkit-invalidations-parts invalidations)))
         (should-not (appkit-invalidations-structure-p invalidations))
         (should-not (appkit-invalidations-entry-keys invalidations)))
       (cl-letf (((symbol-function 'appkit-ewoc-reconcile)
                  (lambda (&rest _args)
                    (ert-fail "header invalidation reconciled the EWOC")))
                 ((symbol-function 'appkit-ewoc-invalidate-key)
                  (lambda (&rest _args)
                    (ert-fail "header invalidation touched an EWOC node"))))
         (appkit-sync-invalidations view))
       (should (equal before (buffer-string)))))))

(ert-deftest qq-root-entries-part-reconciles-the-stable-key-projection ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (last-message-time . 1) (last-message-preview . "old"))
    nil)
   (qq-root-test-with-live-view
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (appkit-invalidate view :structure t)
       (appkit-sync-invalidations view)
       (let ((node (gethash '(session . "private:1") qq-root--node-table)))
         (should node)
         (qq-state-upsert-session
          "private:1" '((last-message-preview . "updated")) nil)
         (appkit-request-sync view :part 'entries)
         (should (string-match-p "old" (buffer-string)))
         (should-not (string-match-p "updated" (buffer-string)))
         (appkit-sync-invalidations view)
         (should
          (eq node (gethash '(session . "private:1") qq-root--node-table)))
         (should (string-match-p "updated" (buffer-string)))
         (should-not (string-match-p "old" (buffer-string))))))))

(ert-deftest qq-root-position-only-invalidation-runs-semantic-position-path ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let (captured restored)
       (cl-letf (((symbol-function 'appkit-position-capture)
                  (lambda (&rest arguments)
                    (setq captured arguments)
                    'root-position-snapshot))
                 ((symbol-function 'appkit-position-restore)
                  (lambda (snapshot &rest _arguments)
                    (setq restored snapshot)))
                 ((symbol-function 'appkit-ewoc-reconcile)
                  (lambda (&rest _arguments)
                    (ert-fail "position-only invalidation reconciled entries")))
                 ((symbol-function 'appkit-ewoc-invalidate-key)
                  (lambda (&rest _arguments)
                    (ert-fail "position-only invalidation touched an entry"))))
         (appkit-request-sync view :position t)
         (appkit-sync-invalidations view))
       (should
        (equal '(:anchor-property qq-root-session-key
                 :preserve-window-start t)
               captured))
       (should (eq 'root-position-snapshot restored))))))

(ert-deftest qq-root-structural-sync-preserves-semantic-point-after-reorder ()
  (qq-root-test-with-reset
   (qq-state-upsert-session
    "private:1"
    '((type . private) (target-id . "1") (title . "One")
      (last-message-time . 2) (last-message-preview . "first"))
    nil)
   (qq-state-upsert-session
    "group:2"
    '((type . group) (target-id . "2") (title . "Two")
      (last-message-time . 1) (last-message-preview . "second"))
    nil)
   (qq-root-test-with-live-view
     (cl-letf (((symbol-function 'qq-media-session-avatar-display-string)
                (lambda (_session) "#")))
       (appkit-invalidate view :structure t)
       (appkit-sync-invalidations view)
       (goto-char
        (ewoc-location
         (gethash '(session . "group:2") qq-root--node-table)))
       (should (equal "group:2" (qq-root--session-key-at-point)))
       (qq-state-upsert-session
        "group:2" '((last-message-time . 3)) nil)
       (appkit-invalidate view :structure t)
       (appkit-sync-invalidations view)
       (should (equal "group:2" (qq-root--session-key-at-point)))
       (should
        (equal '(session . "group:2")
               (qq-root--entry-key
                (ewoc-data (ewoc-nth qq-root--ewoc 3)))))))))

(ert-deftest qq-root-dead-view-makes-all-external-callbacks-inert ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (appkit-kill-view view)
     (cl-letf (((symbol-function 'appkit-request-sync)
                (lambda (&rest _args)
                  (ert-fail "dead root view requested a sync")))
               ((symbol-function 'qq-root--selected-window)
                (lambda () 'root-window))
               ((symbol-function 'qq-root--compute-fill-column)
                (lambda (&optional _window) 100)))
       (qq-root--handle-state-change '(:type connection))
       (qq-root--handle-state-change
        '(:type message :session-key "group:1"))
       (qq-root--handle-media-cache-update "avatar:1")
       (should-not (qq-root--reflow-visible))))))

(ert-deftest qq-root-open-reattaches-and-reuses-one-appkit-view ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (let ((qq-runtime--app app)
           (dead view)
           reopened reused)
       (appkit-kill-view dead)
       (save-window-excursion
         (setq reopened (qq-root-open)))
       (should (eq reopened buffer))
       (setq view (with-current-buffer buffer (appkit-current-view)))
       (should (appkit-view-live-p view))
       (should-not (eq dead view))
       (should (equal 'root (appkit-view-id view)))
       (should-not (string-empty-p
                    (with-current-buffer buffer (buffer-string))))
       (save-window-excursion
         (qq-root-open))
       (setq reused (with-current-buffer buffer (appkit-current-view)))
       (should (eq view reused))))))

(ert-deftest qq-root-sync-never-calls-force-window-update ()
  (qq-root-test-with-reset
   (qq-root-test-with-live-view
     (appkit-invalidate view :structure t :parts '(header geometry))
     (cl-letf (((symbol-function 'force-window-update)
                (lambda (&rest _args)
                  (ert-fail "root sync forced an immediate window update"))))
       (appkit-sync-invalidations view)))))

(provide 'qq-root-test)

;;; qq-root-test.el ends here
