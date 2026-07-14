;;; qq-contacts-test.el --- Tests for native QQ directory -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-contacts)
(require 'qq-root)

(defmacro qq-contacts-test-with-state (&rest body)
  "Run BODY with deterministic native directory fixtures."
  `(let ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil))
     (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
       (kill-buffer buffer))
     (qq-state-reset)
     (unwind-protect
         (progn
           (qq-state-apply-friend-categories
            '(((category_id . 7) (sort_id . 1) (name . "工作")
               (online_count . 1)
               (friends
                . (((user_id . "10002") (nickname . "Bob") (remark))
                   ((user_id . "10001") (nickname . "Alice")
                    (remark . "A姐")))))
              ((category_id . 3) (sort_id . 2) (name . "空分组")
               (online_count . 0) (friends))))
           (qq-state-apply-groups
            '(((group_id . "20002") (group_name . "Dormant Group")
               (group_remark) (member_count . 20) (max_member_count . 500)
               (pinned . :false) (self_permission . "member"))
              ((group_id . "20001") (group_name . "Recent Group")
               (group_remark . "常用群") (member_count . 40)
               (max_member_count . 500) (pinned . t)
               (self_permission . "admin"))))
           (qq-state-upsert-session
            "group:20001"
            '((type . group) (target-id . "20001") (title . "Recent Group"))
           nil)
           (setq qq-state--recent-session-keys '("group:20001"))
           (puthash "group:20001" t qq-state--recent-session-key-set)
           ,@body)
       (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
         (kill-buffer buffer))
       (qq-state-reset))))

(defun qq-contacts-test--entry-keys (entries)
  "Return stable keys from ENTRIES."
  (mapcar #'qq-contacts--entry-key entries))

(defun qq-contacts-test--item-position (item-id)
  "Return position of directory ITEM-ID in current buffer."
  (let ((position (point-min)) found)
    (while (and (< position (point-max)) (not found))
      (if (equal (get-text-property position 'qq-contacts-item-id) item-id)
          (setq found position)
        (setq position
              (next-single-property-change
               position 'qq-contacts-item-id nil (point-max)))))
    found))

(ert-deftest qq-contacts-friend-projection-preserves-category-and-friend-order ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-friends))
             '(navigation
               (category . 7)
               (friend . "10002")
               (friend . "10001")
               (category . 3)))))))

(ert-deftest qq-contacts-category-collapse-removes-only-its-friends ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (puthash 7 t qq-contacts--collapsed-categories)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-friends))
             '(navigation (category . 7) (category . 3)))))))

(ert-deftest qq-contacts-all-groups-include-peers-without-recent-session ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups))
             '(navigation (group . "20002") (group . "20001"))))
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups t))
             '(navigation (group . "20002"))))
     ;; Opening/hydrating a chat does not rewrite the authoritative recent
     ;; snapshot membership used by this view.
     (qq-state-upsert-session
      "group:20002" '((type . group) (target-id . "20002")) nil)
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-groups t))
             '(navigation (group . "20002")))))))

(ert-deftest qq-contacts-projects-native-search-pages-and-pagination ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (setq qq-contacts--view 'search
           qq-contacts--query "Emacs"
           qq-contacts--search-friends
           '(((kind . "friend") (user_id . "10001") (uid . "uid-a")
              (qid . "alice") (nickname . "Alice") (remark . "A姐")
              (category_name . "工作") (hits) (recall_reason . "")))
           qq-contacts--search-groups
           '(((group_id . "20002") (group_name . "Emacs Group")
              (group_remark . "Lisp") (member_count . 20)
              (self_permission . "member") (matched_members)))
           qq-contacts--search-friend-cursor "friend-cursor"
           qq-contacts--search-group-cursor "group-cursor")
     (should
      (equal (qq-contacts-test--entry-keys
              (qq-contacts--project-search))
             '(navigation
               (section . friends) (friend . "10001")
               (load-more . friends)
               (section . groups) (group . "20002")
               (load-more . groups)))))))

(ert-deftest qq-contacts-group-search-preview-keeps-native-match-evidence ()
  (let* ((result
          '((group
             . ((group_id . "20001") (name . "Emacs") (remark . "")
                (member_count . 42) (self_permission . "member")
                (hits . ((group_id) (name . (((start . 0)))) (remark)))))
            (discussions . (((discussion_id . "d1") (name . "Lisp 讨论"))))
            (member_profiles
             . (((user_id . "10001") (card . "Alice Card")
                 (remark . "") (nickname . "Alice"))))
            (member_cards . (((uid . "u2") (card . "Bob Card"))))
            (recall_reason . "native recall")))
         (group (qq-contacts--search-group-object result))
         (preview (qq-contacts--group-preview group)))
    (should (string-match-p "命中群名" preview))
    (should (string-match-p "命中讨论组 Lisp 讨论" preview))
    (should (string-match-p "命中成员 Alice Card" preview))
    (should (string-match-p "命中群名片 Bob Card" preview))
    (should (string-match-p "匹配原因 native recall" preview))))

(ert-deftest qq-contacts-empty-native-page-with-cursor-only-offers-pagination ()
  (with-temp-buffer
    (qq-contacts-mode)
    (setq qq-contacts--view 'search
          qq-contacts--query "Emacs"
          qq-contacts--search-friend-cursor "friend-cursor")
    (should
     (equal (qq-contacts-test--entry-keys (qq-contacts--project-search))
            '(navigation (load-more . friends))))
    (setq qq-contacts--view 'members
          qq-contacts--member-group-id "20001"
          qq-contacts--search-friend-cursor nil
          qq-contacts--search-member-cursor "member-cursor")
    (should
     (equal (qq-contacts-test--entry-keys (qq-contacts--project-members))
            '(navigation (load-more . members))))))

(ert-deftest qq-contacts-empty-search-and-clear-outside-search-preserve-view ()
  (with-temp-buffer
    (qq-contacts-mode)
    (setq qq-contacts--view 'groups
          qq-contacts--previous-view 'friends)
    (qq-contacts-search "   ")
    (should (eq qq-contacts--view 'groups))
    (qq-contacts-clear-search)
    (should (eq qq-contacts--view 'groups))))

(ert-deftest qq-contacts-native-search-survives-synchronous-callbacks ()
  (with-temp-buffer
    (qq-contacts-mode)
    (let (contact-call group-call)
      (cl-letf
          (((symbol-function 'qq-contacts--request-reconcile) #'ignore)
           ((symbol-function 'qq-api-search-contacts-start)
            (lambda (scope query callback &optional _errback group-id limit)
              (setq contact-call (list scope query group-id limit))
              (funcall callback
                       '((results
                          . (((kind . "friend") (user_id . "10001")
                              (uid . "uid-a") (qid . "alice")
                              (nickname . "Alice") (remark . "A姐")
                              (category_name . "工作") (hits)
                              (recall_reason . ""))))
                         (next_cursor . "friend-next")))
              'finished-contact))
           ((symbol-function 'qq-api-search-group-chats-start)
            (lambda (query callback &optional _errback sort limit filters)
              (setq group-call (list query sort limit filters))
              (funcall callback
                       '((results
                          . (((group
                               . ((group_id . "20002") (name . "Emacs Group")
                                  (remark . "Lisp") (member_count . 20)
                                  (self_permission . "member") (hits)))
                              (discussions) (member_profiles) (member_cards)
                              (recall_reason . ""))))
                         (multi_user_keywords)
                         (next_cursor)))
              'finished-group)))
        (qq-contacts-search " Emacs ")
        (should (equal contact-call '(friends "Emacs" nil 50)))
        (should (equal group-call '("Emacs" default 50 nil)))
        (should-not qq-contacts--search-pending)
        (should-not qq-contacts--search-friend-request)
        (should-not qq-contacts--search-group-request)
        (should (equal (alist-get 'user_id
                                  (car qq-contacts--search-friends))
                       "10001"))
        (should (equal (alist-get 'group_id
                                  (car qq-contacts--search-groups))
                       "20002"))
        (should (equal qq-contacts--search-friend-cursor "friend-next"))))))

(ert-deftest qq-contacts-load-more-repeats-native-search-owner ()
  (with-temp-buffer
    (qq-contacts-mode)
    (setq qq-contacts--view 'search
          qq-contacts--query "Emacs"
          qq-contacts--search-owner '(owner)
          qq-contacts--search-friends
          '(((kind . "friend") (user_id . "10001") (uid . "uid-a")))
          qq-contacts--search-friend-cursor
          "00000000-0000-4000-8000-000000000000")
    (let (call)
      (cl-letf (((symbol-function 'qq-contacts--request-reconcile) #'ignore)
                ((symbol-function 'qq-api-search-contacts-next)
                 (lambda (scope cursor query callback
                          &optional _errback group-id limit)
                   (setq call (list cursor scope query group-id limit))
                   (funcall callback
                            '((results
                               . (((kind . "friend") (user_id . "10002")
                                   (uid . "uid-b"))))
                              (next_cursor)))
                   'finished)))
        (qq-contacts-load-more-friends)
        (should
         (equal call
                '("00000000-0000-4000-8000-000000000000"
                  friends "Emacs" nil 50)))
        (should
         (equal (mapcar (lambda (friend) (alist-get 'user_id friend))
                        qq-contacts--search-friends)
                '("10001" "10002")))
        (should-not qq-contacts--search-friend-cursor)
        (should-not qq-contacts--search-pending)))))

(ert-deftest qq-contacts-navigation-buttons-are-real-and-switch-view ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                (lambda (_id) "@"))
               ((symbol-function 'qq-media-group-avatar-cached-display-string)
                (lambda (_id) "#")))
       (qq-contacts--reconcile))
     (goto-char (point-min))
     (search-forward "全部群")
     (let ((button (button-at (1- (point)))))
       (should button)
       (push-button button))
     (should (eq qq-contacts--view 'groups)))))

(ert-deftest qq-contacts-action-row-opens-exact-canonical-chat ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (let (opened)
       (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (_id) "@"))
                 ((symbol-function 'qq-media-group-avatar-cached-display-string)
                  (lambda (_id) "#"))
                 ((symbol-function 'qq-chat-open)
                  (lambda (session-key) (setq opened session-key))))
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "10001"))
         (qq-contacts-open-at-point)
         (should (equal opened "private:10001")))))))

(ert-deftest qq-contacts-info-dispatches-to-native-user-and-group-pages ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (let (user group)
       (cl-letf (((symbol-function 'qq-media-avatar-cached-display-string)
                  (lambda (_id) "@"))
                 ((symbol-function 'qq-media-group-avatar-cached-display-string)
                  (lambda (_id) "#"))
                 ((symbol-function 'qq-user-open)
                  (lambda (id) (setq user id)))
                 ((symbol-function 'qq-group-open)
                  (lambda (id) (setq group id))))
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "10002"))
         (qq-contacts-open-info-at-point)
         (should (equal user "10002"))
         (setq qq-contacts--view 'groups)
         (qq-contacts--reconcile)
         (goto-char (qq-contacts-test--item-position "20002"))
         (qq-contacts-open-info-at-point)
         (should (equal group "20002")))))))

(ert-deftest qq-contacts-synchronous-refresh-does-not-retain-stale-tokens ()
  (with-temp-buffer
    (qq-contacts-mode)
    (cl-letf (((symbol-function 'qq-contacts--request-reconcile) #'ignore)
              ((symbol-function 'qq-api-refresh-friend-categories)
               (lambda (callback &optional _errback)
                 (funcall callback nil)
                 'already-finished-friends))
              ((symbol-function 'qq-api-refresh-joined-groups)
               (lambda (callback &optional _errback)
                 (funcall callback nil)
                 'already-finished-groups)))
      (qq-contacts-refresh)
      (should-not qq-contacts--loading)
      (should-not qq-contacts--refresh-owner)
      (should-not qq-contacts--refresh-parts)
      (should-not qq-contacts--friend-request)
      (should-not qq-contacts--group-request))))

(ert-deftest qq-contacts-media-update-targets-only-exact-row-key ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) forced)
    (unwind-protect
        (with-current-buffer buffer
          (qq-contacts-mode)
          (cl-letf (((symbol-function 'qq-contacts--request-reconcile)
                     (lambda (&optional keys) (setq forced keys))))
            (qq-contacts--handle-media-cache-update "avatar:10001")
            (should (equal forced '((friend . "10001")
                                    (member . "10001"))))
            (setq forced nil)
            (qq-contacts--handle-media-cache-update "forward-image:x")
            (should-not forced)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-visible-avatar-update-invalidates-only-target-rows ()
  (with-temp-buffer
    (qq-contacts-mode)
    (setq qq-contacts--fill-column 80)
    (let (invalidated)
      (cl-letf (((symbol-function 'qq-contacts--displayed-p) (lambda () t))
                ((symbol-function 'qq-contacts--usable-width) (lambda () 80))
                ((symbol-function 'qq-contacts--project-entries)
                 (lambda () (ert-fail "avatar update rebuilt full projection")))
                ((symbol-function 'qq-contacts--invalidate-keys)
                 (lambda (keys) (setq invalidated keys))))
        (qq-contacts--request-reconcile
         '((friend . "10001") (member . "10001")))
        (should (equal invalidated
                       '((friend . "10001") (member . "10001"))))))))

(ert-deftest qq-contacts-hidden-avatar-update-retains-forced-row-keys ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (cl-letf (((symbol-function 'qq-contacts--displayed-p) (lambda () nil)))
       (qq-contacts--request-reconcile '((group . "20002"))))
     (should qq-contacts--dirty)
     (should (equal qq-contacts--pending-force-keys '((group . "20002"))))
     (let (forced)
       (cl-letf (((symbol-function 'appkit-ewoc-reconcile)
                  (lambda (_ewoc _entries _key-function &rest args)
                    (setq forced (plist-get args :force-keys))
                    (make-hash-table :test #'equal))))
         (qq-contacts--reconcile))
       (should (equal forced '((group . "20002"))))
       (should-not qq-contacts--pending-force-keys)
       (should-not qq-contacts--dirty)))))

(ert-deftest qq-contacts-reconcile-preserves-force-keys-queued-during-render ()
  (qq-contacts-test-with-state
   (with-temp-buffer
     (qq-contacts-mode)
     (let (forced-pages)
       (cl-letf (((symbol-function 'appkit-ewoc-reconcile)
                  (lambda (_ewoc _entries _key-function &rest args)
                    (push (plist-get args :force-keys) forced-pages)
                    (when (= (length forced-pages) 1)
                      (qq-contacts--reconcile '((group . "20001"))))
                    (make-hash-table :test #'equal))))
         (qq-contacts--reconcile '((friend . "10001"))))
       (should (equal (nreverse forced-pages)
                      '(((friend . "10001")) ((group . "20001")))))
       (should-not qq-contacts--pending-force-keys)))))

(ert-deftest qq-contacts-reconcile-failure-restores-position-and-retries-keys ()
  (with-temp-buffer
    (qq-contacts-mode)
    (let (restored)
      (cl-letf (((symbol-function 'appkit-position-capture)
                 (lambda (&rest _args) 'synthetic-position))
                ((symbol-function 'appkit-position-restore)
                 (lambda (snapshot) (setq restored snapshot)))
                ((symbol-function 'appkit-ewoc-reconcile)
                 (lambda (&rest _args)
                   (error "synthetic reconciliation failure"))))
        (should-error
         (qq-contacts--reconcile '((friend . "synthetic-user")))))
      (should (eq restored 'synthetic-position))
      (should qq-contacts--dirty)
      (should (equal qq-contacts--pending-force-keys
                     '((friend . "synthetic-user"))))
      (should-not qq-contacts--rendering))))

(ert-deftest qq-contacts-targeted-invalidation-retains-key-after-printer-error ()
  (with-temp-buffer
    (qq-contacts-mode)
    (cl-letf (((symbol-function 'appkit-ewoc-invalidate-key)
               (lambda (&rest _args) (error "synthetic printer failure"))))
      (should-error
       (qq-contacts--invalidate-keys '((friend . "10001")))))
    (should qq-contacts--dirty)
    (should (equal qq-contacts--pending-force-keys
                   '((friend . "10001"))))))

(ert-deftest qq-contacts-search-append-rejects-cross-page-identities ()
  (should-error
   (qq-contacts--append-search-items
    '(((group_id . "synthetic-group")))
    '(((group_id . "synthetic-group")))
    '((group_id))))
  (should-error
   (qq-contacts--append-search-items
    '(((kind . "friend") (user_id . "synthetic-user-a")
       (uid . "synthetic-native-a")))
    '(((kind . "friend") (user_id . "synthetic-user-a")
       (uid . "synthetic-native-b")))
    qq-contacts--contact-search-identities))
  ;; A native UID changing its public UIN across cursor pages is independently
  ;; invalid even though `(kind,user_id)' remains unique.
  (should-error
   (qq-contacts--append-search-items
    '(((kind . "friend") (user_id . "synthetic-user-a")
       (uid . "synthetic-native-shared")))
    '(((kind . "friend") (user_id . "synthetic-user-b")
       (uid . "synthetic-native-shared")))
    qq-contacts--contact-search-identities)))

(ert-deftest qq-contacts-duplicate-continuation-settles-pending-section ()
  (with-temp-buffer
    (qq-contacts-mode)
    (let* ((owner (list 'synthetic-search-owner))
           (existing
            '(((kind . "friend")
               (user_id . "synthetic-user")
               (uid . "synthetic-native-user")
               (nickname . "Synthetic Friend")))))
      (setq qq-contacts--view 'search
            qq-contacts--query "Synthetic"
            qq-contacts--search-owner owner
            qq-contacts--search-pending '(friends)
            qq-contacts--search-friends (copy-tree existing)
            qq-contacts--search-friend-request 'synthetic-request)
      (cl-letf (((symbol-function 'qq-contacts--request-reconcile) #'ignore))
        (qq-contacts--finish-search-page
         (current-buffer) owner 'friends t
         '((results
            . (((kind . "friend")
                (user_id . "synthetic-user")
                (uid . "synthetic-native-user")
                (nickname . "Repeated Synthetic Friend"))))
           (next_cursor . "synthetic-next"))))
      (should (equal qq-contacts--search-friends existing))
      (should-not qq-contacts--search-pending)
      (should-not qq-contacts--search-friend-request)
      (should-not qq-contacts--search-friend-cursor)
      (should
       (string-match-p
        "repeated.*across pages"
        (or (alist-get 'friends qq-contacts--search-errors) ""))))))

(ert-deftest qq-contacts-layout-uses-narrowest-visible-window ()
  (with-temp-buffer
    (qq-contacts-mode)
    (cl-letf (((symbol-function 'get-buffer-window-list)
               (lambda (&rest _args) '(wide narrow)))
              ((symbol-function 'appkit-view-window-fill-column)
               (lambda (window _margin)
                 (if (eq window 'wide) 120 72))))
      (should (= (qq-contacts--usable-width) 72)))))

(ert-deftest qq-contacts-ignores-ordinary-session-state-events ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) reconciles)
    (unwind-protect
        (with-current-buffer buffer
          (qq-contacts-mode)
          (cl-letf (((symbol-function 'qq-contacts--request-reconcile)
                     (lambda (&rest _args)
                       (setq reconciles (1+ (or reconciles 0))))))
            (qq-contacts--handle-state-change '(:type session))
            (should-not reconciles)
            (qq-contacts--handle-state-change '(:type sessions-refreshed))
            (should (= reconciles 1))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-mode-cancels-native-work-before-major-mode-change ()
  (with-temp-buffer
    (qq-contacts-mode)
    (should (memq #'qq-contacts--cancel-refresh change-major-mode-hook))
    (should (memq #'qq-contacts--cancel-search change-major-mode-hook))))

(ert-deftest qq-contacts-group-member-search-is-native-and-actionable ()
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)) call opened)
    (unwind-protect
        (cl-letf
            (((symbol-function 'pop-to-buffer) (lambda (&rest _args) buffer))
             ((symbol-function 'qq-contacts--request-reconcile) #'ignore)
             ((symbol-function 'qq-api-search-contacts-start)
              (lambda (scope query callback &optional _errback group-id limit)
                (setq call (list scope query group-id limit))
                (funcall callback
                         '((results
                            . (((kind . "group_member")
                                (group_id . "20002")
                                (group_name . "Emacs Group")
                                (group_remark . "Lisp")
                                (user_id . "10003") (uid . "uid-c")
                                (nickname . "Carol") (remark . "")
                                (card . "C酱") (is_friend . :false)
                                (hits) (recall_reason . ""))))
                           (next_cursor)))
                'finished))
             ((symbol-function 'qq-chat-open)
              (lambda (key) (setq opened key))))
          (with-current-buffer buffer
            (qq-contacts-mode))
          (qq-contacts-search-group-members "20002" " Carol ")
          (with-current-buffer buffer
            (should (equal call '(group-members "Carol" "20002" 50)))
            (should (eq qq-contacts--view 'members))
            (should-not qq-contacts--search-pending)
            (should
             (equal (qq-contacts-test--entry-keys
                     (qq-contacts--project-members))
                    '(navigation (section . members) (member . "10003"))))
            (qq-contacts--activate-entry
             (qq-contacts--member-entry (car qq-contacts--search-members)))
            (should (equal opened "private:10003"))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-contacts-bindings-follow-directory-and-root-conventions ()
  (should (eq (lookup-key qq-root-mode-map (kbd "c")) #'qq-contacts-open))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "g"))
              #'qq-contacts-refresh))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "/"))
              #'qq-contacts-search))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "RET"))
              #'qq-contacts-open-at-point))
  (should (eq (lookup-key qq-contacts-mode-map (kbd "i"))
              #'qq-contacts-open-info-at-point)))

(provide 'qq-contacts-test)

;;; qq-contacts-test.el ends here
