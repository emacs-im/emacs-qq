;;; qq-guilds-test.el --- Tests for QQ Guild directory -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-guilds)

(defconst qq-guilds-test--guild-id "9007199254740993")
(defconst qq-guilds-test--channel-id "9007199254741999")

(defmacro qq-guilds-test-with-live-view (&rest body)
  "Run BODY in a uniquely named live Appkit Guild directory view.

BODY may refer to the lexical variables `app', `buffer', and `view'."
  (declare (indent 0) (debug t))
  `(let* ((qq-guilds-buffer-name
           (generate-new-buffer-name " *qq-guilds-test*"))
          (app (appkit-start-app 'qq :id (make-symbol "qq-guilds-test")))
          (buffer (get-buffer-create qq-guilds-buffer-name))
          view)
     (unwind-protect
         (with-current-buffer buffer
           (qq-guilds-mode)
           (setq view
                 (appkit-attach-view
                  :app app
                  :id qq-guilds--view-id
                  :mode 'qq-guilds-mode
                  :sync-function #'qq-guilds--sync-invalidations
                  :parts '(directory)))
           (qq-guilds--setup-view view)
           (cl-letf (((symbol-function 'qq-runtime-app) (lambda () app)))
             ,@body))
       (when (appkit-app-live-p app)
         (appkit-stop-app app))
       (when (buffer-live-p buffer)
         (kill-buffer buffer)))))

(defun qq-guilds-test--directory ()
  "Return a synthetic closed Guild directory."
  `((guilds . (((guild_id . ,qq-guilds-test--guild-id)
                (name . "Synthetic guild")
                (avatar_seq . "3")
                (pinned_at))))
    (channels . (((guild_id . ,qq-guilds-test--guild-id)
                  (channel_id . ,qq-guilds-test--channel-id)
                  (guild_name . "Synthetic guild")
                  (name . "General")
                  (kind . "text")
                  (avatar_seq . "4")
                  (pinned_at . "1784000000")
                  (latest_sequence . "23"))))))

(defun qq-guilds-test--directory-with-channels (channel-specs)
  "Return one Guild containing CHANNEL-SPECS in order.

Each item is either a channel id string or a cons of id and display name."
  `((guilds . (((guild_id . ,qq-guilds-test--guild-id)
                (name . "Synthetic guild")
                (avatar_seq . "3")
                (pinned_at))))
    (channels
     . ,(mapcar
         (lambda (spec)
           (let ((channel-id (if (consp spec) (car spec) spec))
                 (name (if (consp spec)
                           (cdr spec)
                         (format "Channel %s" spec))))
             `((guild_id . ,qq-guilds-test--guild-id)
               (channel_id . ,channel-id)
               (guild_name . "Synthetic guild")
               (name . ,name)
               (kind . "text")
               (avatar_seq . "4")
               (pinned_at)
               (latest_sequence . "23"))))
         channel-specs))))

(defun qq-guilds-test--channel-id (index)
  "Return a canonical synthetic channel id for INDEX."
  (format "900719925475%04d" index))

(ert-deftest qq-guilds-renders-hierarchy-with-a-line-scoped-channel-button ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory (qq-guilds-test--directory))
          (with-temp-buffer
            (qq-guilds-mode)
            (qq-guilds-render)
            (goto-char (point-min))
            (should (search-forward "Synthetic guild" nil t))
            (should (search-forward "# General" nil t))
            (let* ((button (button-at (1- (point))))
                   (end (button-end button)))
              (should button)
              (should (equal (button-get button 'qq-guild-id)
                             qq-guilds-test--guild-id))
              (should (equal (button-get button 'qq-guild-channel-id)
                             qq-guilds-test--channel-id))
              (should-not (get-text-property end 'mouse-face)))
            (should (search-forward "文字" nil t))))
      (qq-state-reset))))

(ert-deftest qq-guilds-open-channel-installs-complete-session-identity ()
  (let ((qq-state-change-hook nil)
        opened)
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory (qq-guilds-test--directory))
          (cl-letf (((symbol-function 'qq-chat-open)
                     (lambda (session-key) (setq opened session-key))))
            (qq-guilds-open-channel
             qq-guilds-test--guild-id qq-guilds-test--channel-id))
          (should
           (equal opened
                  "guild:9007199254740993:channel:9007199254741999"))
          (let ((session (qq-state-session opened)))
            (should (eq (alist-get 'type session) 'guild-channel))
            (should (equal (alist-get 'guild-id session)
                           qq-guilds-test--guild-id))
            (should (equal (alist-get 'channel-id session)
                           qq-guilds-test--channel-id))
            (should (equal (alist-get 'title session)
                           "Synthetic guild · #General"))
            (should (equal (alist-get 'channel-kind session) "text"))))
      (qq-state-reset))))

(ert-deftest qq-guilds-refresh-settles-loading-after-authoritative-response ()
  (let ((qq-state-change-hook nil)
        action-called)
    (qq-state-reset)
    (unwind-protect
        (with-temp-buffer
          (qq-guilds-mode)
          (cl-letf (((symbol-function 'qq-api-refresh-guild-directory)
                     (lambda (&optional callback _errback)
                       (setq action-called t)
                       (qq-state-apply-guild-directory
                        (qq-guilds-test--directory))
                       (funcall callback (qq-guilds-test--directory)))))
            (qq-guilds-refresh))
          (should action-called)
          (should-not qq-guilds--loading)
          (should-not qq-guilds--error)
          (appkit-sync-invalidations (appkit-current-view))
          (should (string-match-p "General" (buffer-string))))
      (qq-state-reset))))

(ert-deftest qq-guilds-state-and-completion-coalesce-one-appkit-sync ()
  (let ((buffer (get-buffer-create qq-guilds-buffer-name))
        (qq-state-change-hook '(qq-guilds--handle-state-change))
        sync-count)
    (qq-state-reset)
    (unwind-protect
        (with-current-buffer buffer
          (qq-guilds-mode)
          (let ((view (qq-guilds--ensure-view)))
            (cl-letf (((symbol-function 'qq-guilds--reconcile-directory)
                       (lambda ()
                         (setq sync-count (1+ (or sync-count 0)))))
                      ((symbol-function 'qq-api-refresh-guild-directory)
                       (lambda (&optional callback _errback)
                         (qq-state-apply-guild-directory
                          (qq-guilds-test--directory))
                         (funcall callback (qq-guilds-test--directory))
                         'already-finished)))
              (qq-guilds-refresh)
              (should-not sync-count)
              (appkit-sync-invalidations view)
              (should (= sync-count 1))
              (should-not qq-guilds--loading)
              (should-not qq-guilds--refresh-owner)
              (should-not qq-guilds--refresh-request))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (let ((qq-state-change-hook nil))
        (qq-state-reset)))))

(ert-deftest qq-guilds-sync-preserves-stable-node-identity-without-erasing ()
  (let ((qq-state-change-hook nil)
        (first-id (qq-guilds-test--channel-id 1))
        (second-id (qq-guilds-test--channel-id 2)))
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory
           (qq-guilds-test--directory-with-channels (list first-id)))
          (qq-guilds-test-with-live-view
            (appkit-invalidate view :structure t :part 'directory)
            (appkit-sync-invalidations view)
            (let ((guild-node
                   (gethash
                    (qq-guilds--guild-entry-key qq-guilds-test--guild-id)
                    qq-guilds--node-table))
                  (channel-node
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id first-id)
                    qq-guilds--node-table)))
              (should guild-node)
              (should channel-node)
              (qq-state-apply-guild-directory
               (qq-guilds-test--directory-with-channels
                (list (cons first-id "Renamed") second-id)))
              (cl-letf (((symbol-function 'erase-buffer)
                         (lambda ()
                           (ert-fail "incremental Guild sync erased buffer"))))
                (appkit-invalidate view :structure t :part 'directory)
                (appkit-sync-invalidations view))
              (should
               (eq guild-node
                   (gethash
                    (qq-guilds--guild-entry-key qq-guilds-test--guild-id)
                    qq-guilds--node-table)))
              (should
               (eq channel-node
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id first-id)
                    qq-guilds--node-table)))
              (should (string-match-p "# Renamed" (buffer-string)))
              (should (string-match-p
                       (regexp-quote (format "# Channel %s" second-id))
                       (buffer-string)))
              (should (eq buffer-undo-list t)))))
      (qq-state-reset))))

(ert-deftest qq-guilds-loading-and-error-retain-the-directory-snapshot ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory (qq-guilds-test--directory))
          (qq-guilds-test-with-live-view
            (appkit-invalidate view :structure t :part 'directory)
            (appkit-sync-invalidations view)
            (let ((channel-node
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id qq-guilds-test--channel-id)
                    qq-guilds--node-table)))
              (setq qq-guilds--loading t)
              (appkit-invalidate view :structure t :part 'directory)
              (appkit-sync-invalidations view)
              (should (string-match-p "正在读取" (buffer-string)))
              (should (string-match-p "# General" (buffer-string)))
              (should (string-match-p "refreshing" (qq-guilds--header-line)))
              (should
               (eq channel-node
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id qq-guilds-test--channel-id)
                    qq-guilds--node-table)))
              (setq qq-guilds--loading nil
                    qq-guilds--error "读取频道目录失败: synthetic")
              (appkit-invalidate view :structure t :part 'directory)
              (appkit-sync-invalidations view)
              (should (string-match-p "读取频道目录失败" (buffer-string)))
              (should (string-match-p "# General" (buffer-string)))
              (should-not (string-match-p "没有可见" (buffer-string)))
              (should (string-match-p "刷新失败" (qq-guilds--header-line)))
              (should
               (eq channel-node
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id qq-guilds-test--channel-id)
                    qq-guilds--node-table))))))
      (qq-state-reset))))

(ert-deftest qq-guilds-initial-loading-does-not-project-an-empty-flash ()
  (let ((qq-state-change-hook nil))
    (qq-state-reset)
    (unwind-protect
        (qq-guilds-test-with-live-view
          (setq qq-guilds--loading t)
          (appkit-invalidate view :structure t :part 'directory)
          (appkit-sync-invalidations view)
          (should (string-match-p "正在读取" (buffer-string)))
          (should-not (string-match-p "没有可见" (buffer-string)))
          (should (gethash 'status qq-guilds--node-table))
          (should-not (gethash 'empty qq-guilds--node-table)))
      (qq-state-reset))))

(ert-deftest qq-guilds-reconcile-preserves-semantic-point-and-window-start ()
  (let* ((qq-state-change-hook nil)
         (ids (mapcar #'qq-guilds-test--channel-id
                      (number-sequence 1 40)))
         (deleted-id (nth 9 ids))
         (target-id (nth 24 ids))
         (added-id (qq-guilds-test--channel-id 41)))
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory
           (qq-guilds-test--directory-with-channels ids))
          (qq-guilds-test-with-live-view
            (appkit-invalidate view :structure t :part 'directory)
            (appkit-sync-invalidations view)
            (save-window-excursion
              (let ((window (selected-window))
                    (target-key
                     (qq-guilds--channel-entry-key
                      qq-guilds-test--guild-id target-id))
                    (deleted-key
                     (qq-guilds--channel-entry-key
                      qq-guilds-test--guild-id deleted-id)))
                (set-window-buffer window buffer)
                (select-window window)
                (goto-char
                 (ewoc-location (gethash target-key qq-guilds--node-table)))
                (set-window-start
                 window
                 (ewoc-location (gethash deleted-key qq-guilds--node-table))
                 t)
                (let* ((window-start-line
                        (line-number-at-pos (window-start window)))
                       (remaining
                        (delete target-id
                                (delete deleted-id (copy-sequence ids))))
                       (new-order
                        (append (seq-take remaining 15)
                                (list target-id)
                                (seq-drop remaining 15)
                                (list added-id))))
                  (should (equal target-key (qq-guilds--entry-key-at-point)))
                  (should (> window-start-line 1))
                  (qq-state-apply-guild-directory
                   (qq-guilds-test--directory-with-channels new-order))
                  (cl-letf (((symbol-function 'erase-buffer)
                             (lambda ()
                               (ert-fail
                                "reordered Guild sync erased buffer"))))
                    (appkit-invalidate view :structure t :part 'directory)
                    (appkit-sync-invalidations view))
                  (should (equal target-key
                                 (qq-guilds--entry-key-at-point)))
                  (should
                   (= window-start-line
                      (line-number-at-pos (window-start window))))
                  (should-not (gethash deleted-key qq-guilds--node-table))
                  (should
                   (gethash
                    (qq-guilds--channel-entry-key
                     qq-guilds-test--guild-id added-id)
                    qq-guilds--node-table)))))))
      (qq-state-reset))))

(ert-deftest qq-guilds-dead-view-rejects-stale-refresh-and-state-events ()
  (let ((buffer (get-buffer-create qq-guilds-buffer-name)) queued cancelled)
    (unwind-protect
        (with-current-buffer buffer
          (qq-guilds-mode)
          (let ((view (qq-guilds--ensure-view))
                (owner (list 'dead-guild-refresh)))
            (setq qq-guilds--refresh-owner owner
                  qq-guilds--refresh-request 'dead-request
                  qq-guilds--loading t)
            (cl-letf (((symbol-function 'qq-api-cancel-request)
                       (lambda (request) (setq cancelled request)))
                      ((symbol-function 'qq-guilds--queue-view-sync)
                       (lambda (&rest _args) (setq queued t))))
              (appkit-kill-view view)
              (qq-guilds--handle-state-change
               '(:type guild-directory-refreshed))
              (qq-guilds--finish-refresh buffer owner)
              (should-not queued)
              (should (eq cancelled 'dead-request))
              (should-not qq-guilds--loading)
              (should-not qq-guilds--refresh-owner)
              (should-not qq-guilds--refresh-request))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'qq-guilds-test)

;;; qq-guilds-test.el ends here
