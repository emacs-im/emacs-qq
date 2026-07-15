;;; qq-guild-user-test.el --- Tests for native QQ channel member pages -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression coverage for native QQ channel member views.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-guild-user)

(defconst qq-guild-user-test--profile
  '((guild_id . "9007199254740993")
    (native_id . "144115219000000001")
    (display_name . "Synthetic member")
    (nickname . "Synthetic nickname")
    (member_name . "Synthetic member")
    (avatar_url . "https://example.invalid/synthetic-avatar.jpg"))
  "Closed native channel member fixture with opaque decimal identities.")

(defmacro qq-guild-user-test-with-view (&rest body)
  "Evaluate BODY in a live Appkit member view bound as `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-guild-user-mode)
           (setq qq-guild-user--guild-id "9007199254740993"
                 qq-guild-user--native-id "144115219000000001")
           (let ((view (qq-guild-user--ensure-view)))
             (ignore view)
             ,@body))
       (qq-runtime-stop))))

(ert-deftest qq-guild-user-refresh-renders-only-native-channel-identity ()
  (let (requested)
    (qq-guild-user-test-with-view
      (cl-letf (((symbol-function 'qq-api-get-guild-member-profile)
                 (lambda (guild-id native-id callback &optional _errback)
                   (setq requested (list guild-id native-id))
                   (funcall callback (copy-tree qq-guild-user-test--profile))
                   'synthetic-request))
                ((symbol-function 'qq-state-guild)
                 (lambda (_guild-id)
                   '((name . "Synthetic Guild"))))
                ((symbol-function 'qq-media-url-preview-display-string)
                 (lambda (&rest _args) "[avatar]")))
        (qq-guild-user-refresh))
      (should (equal requested
                     '("9007199254740993" "144115219000000001")))
      (should (equal qq-guild-user--profile qq-guild-user-test--profile))
      (should (string-match-p "Synthetic Guild" (buffer-string)))
      (should (string-match-p "Synthetic member" (buffer-string)))
      (should (string-match-p "频道 ID 是 GPro tinyId，不是 QQ 号"
                              (buffer-string)))
      (should-not (string-match-p "私聊\|加好友\|QQ:" (buffer-string))))))

(ert-deftest qq-guild-user-open-avatar-uses-profile-url-and-shared-cache-key ()
  (let (opened)
    (with-temp-buffer
      (qq-guild-user-mode)
      (setq qq-guild-user--guild-id "9007199254740993"
            qq-guild-user--native-id "144115219000000001"
            qq-guild-user--profile (copy-tree qq-guild-user-test--profile))
      (cl-letf (((symbol-function 'qq-media-open-image-url)
                 (lambda (key url) (setq opened (list key url)))))
        (qq-guild-user-open-avatar))
      (should
       (equal opened
              '("guild-member-avatar:9007199254740993:144115219000000001"
                "https://example.invalid/synthetic-avatar.jpg"))))))

(ert-deftest qq-guild-user-view-id-keeps-opaque-native-identities ()
  (should
   (equal '(guild-user "9007199254740993" "144115219000000001")
          (qq-guild-user--view-id
           "9007199254740993" "144115219000000001"))))

(ert-deftest qq-guild-user-renamed-live-and-detached-buffer-is-reused ()
  (let ((qq-runtime--app nil)
        buffer first-view
        (calls 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments) candidate))
                    ((symbol-function 'qq-api-get-guild-member-profile)
                     (lambda (_guild-id _native-id callback &optional _errback)
                       (cl-incf calls)
                       (funcall callback
                                (copy-tree qq-guild-user-test--profile))
                       (list 'member-request calls))))
            (setq buffer
                  (qq-guild-user-open
                   "9007199254740993" "144115219000000001"))
            (with-current-buffer buffer
              (setq first-view (appkit-current-view))
              (rename-buffer "*renamed-guild-member*" t)
              (setq qq-guild-user--error "preserved live state"))
            (should
             (eq buffer
                 (qq-guild-user-open
                  "9007199254740993" "144115219000000001")))
            (with-current-buffer buffer
              (should (eq first-view (appkit-current-view)))
              (should (equal qq-guild-user--error "preserved live state"))
              (appkit-kill-view first-view)
              (should-not qq-guild-user--profile))
            (should
             (eq buffer
                 (qq-guild-user-open
                  "9007199254740993" "144115219000000001")))
            (with-current-buffer buffer
              (should-not (eq first-view (appkit-current-view)))
              (should (appkit-view-live-p (appkit-current-view))))
            (should (= calls 2))))
      (qq-runtime-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest qq-guild-user-runtime-replacement-resets-owned-work-via-setup ()
  (let ((qq-runtime--app nil)
        buffer old-view old-hook new-view new-hook
        cancelled
        (calls 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments) candidate))
                    ((symbol-function 'qq-api-get-guild-member-profile)
                     (lambda (_guild-id _native-id _callback
                              &optional _errback)
                       (cl-incf calls)
                       (when (= calls 2)
                         (should-not qq-guild-user--profile)
                         (should-not qq-guild-user--error))
                       (list 'member-request calls)))
                    ((symbol-function 'qq-api-cancel-request)
                     (lambda (request) (push request cancelled))))
            (setq buffer
                  (qq-guild-user-open
                   "9007199254740993" "144115219000000001"))
            (with-current-buffer buffer
              (setq old-view (appkit-current-view)
                    old-hook qq-guild-user--media-hook
                    qq-guild-user--profile '((display_name . "old account"))
                    qq-guild-user--error "old account error")
              (rename-buffer "*renamed-runtime-guild-member*" t))
            (qq-runtime-stop)
            (with-current-buffer buffer
              (should-not qq-guild-user--loading)
              (should-not qq-guild-user--profile)
              (should-not qq-guild-user--media-hook)
              ;; Simulate state retained by code outside the dead view.  The
              ;; replacement's :setup boundary must still discard it.
              (setq qq-guild-user--profile '((display_name . "stale"))
                    qq-guild-user--error "stale"
                    qq-guild-user--loading t
                    qq-guild-user--request 'retained-request
                    qq-guild-user--request-owner '(retained-owner)))
            (should
             (eq buffer
                 (qq-guild-user-open
                  "9007199254740993" "144115219000000001")))
            (with-current-buffer buffer
              (setq new-view (appkit-current-view)
                    new-hook qq-guild-user--media-hook)
              (should (appkit-view-live-p new-view))
              (should-not (eq old-view new-view))
              (should-not (eq old-hook new-hook))
              (should qq-guild-user--loading)
              (should-not qq-guild-user--profile)
              (should-not qq-guild-user--error))
            ;; Ordinary live reuse never runs :setup and keeps the active load
            ;; and its exact view-owned hook.
            (should
             (eq buffer
                 (qq-guild-user-open
                  "9007199254740993" "144115219000000001")))
            (with-current-buffer buffer
              (should (eq new-view (appkit-current-view)))
              (should (eq new-hook qq-guild-user--media-hook))
              (should qq-guild-user--loading))
            (should (= calls 2))
            (should (member '(member-request 1) cancelled))
            (should (member 'retained-request cancelled))))
      (qq-runtime-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest qq-guild-user-stale-and-dead-responses-are-inert ()
  (qq-guild-user-test-with-view
    (let (callbacks)
      (cl-letf (((symbol-function 'qq-api-get-guild-member-profile)
                 (lambda (_guild-id _native-id callback &optional _errback)
                   (push callback callbacks)
                   (list 'member-request (length callbacks))))
                ((symbol-function 'qq-api-cancel-request) #'ignore))
        (qq-guild-user-refresh)
        (let ((first (car callbacks)))
          (qq-guild-user-refresh)
          (let ((owner qq-guild-user--request-owner)
                (request qq-guild-user--request))
            (cl-letf (((symbol-function 'appkit-request-sync)
                       (lambda (&rest _arguments)
                         (ert-fail "stale member callback requested sync"))))
              (funcall first (copy-tree qq-guild-user-test--profile)))
            (should (eq owner qq-guild-user--request-owner))
            (should (eq request qq-guild-user--request)))))
      (let ((latest (car callbacks)))
        (appkit-kill-view view)
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "dead member callback requested sync"))))
          (funcall latest (copy-tree qq-guild-user-test--profile)))
        (should-not qq-guild-user--profile)
        (should-not qq-guild-user--request-owner)))))

(ert-deftest qq-guild-user-callback-requests-one-atomic-appkit-sync ()
  (qq-guild-user-test-with-view
    (let (success calls)
      (cl-letf (((symbol-function 'qq-api-get-guild-member-profile)
                 (lambda (_guild-id _native-id callback &optional _errback)
                   (setq success callback)
                   'member-request)))
        (qq-guild-user-refresh))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (candidate &rest options)
                     (push (cons candidate options) calls)))
                  ((symbol-function 'qq-guild-user-render)
                   (lambda () (ert-fail "member callback rendered directly")))
                  ((symbol-function 'appkit-invalidate)
                   (lambda (&rest _arguments)
                     (ert-fail "member callback split invalidation")))
                  ((symbol-function 'appkit-schedule-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "member callback scheduled directly"))))
          (funcall success (copy-tree qq-guild-user-test--profile)))
        (should (equal before (buffer-string)))
        (should (equal qq-guild-user--profile
                       qq-guild-user-test--profile))
        (should
         (equal calls (list (list view :structure t :part 'profile))))))))

(ert-deftest qq-guild-user-media-hook-targets-renamed-live-view ()
  (qq-guild-user-test-with-view
    (setq qq-guild-user--profile (copy-tree qq-guild-user-test--profile))
    (let ((hook qq-guild-user--media-hook)
          calls)
      (rename-buffer "*renamed-guild-member-media*" t)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (candidate &rest options)
                   (push (cons candidate options) calls))))
        (funcall hook (qq-guild-user--avatar-key)))
      (should
       (equal calls
              (list
               (list view :part 'profile
                     :resource (qq-guild-user--avatar-key)))))
      (appkit-kill-view view)
      (setq calls nil)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "dead member media hook requested sync"))))
        (funcall hook (qq-guild-user--avatar-key)))
      (should-not calls))))

(ert-deftest qq-guild-user-sync-preserves-stable-field-position-key ()
  (qq-guild-user-test-with-view
    (setq qq-guild-user--profile (copy-tree qq-guild-user-test--profile))
    (qq-guild-user--request-sync view)
    (qq-guild-user--sync-now view)
    (goto-char (point-min))
    (search-forward "频道 ID:")
    (let ((key (get-text-property (1- (point))
                                  qq-guild-user--position-property)))
      (should
       (equal key
              '(guild-user "9007199254740993" "144115219000000001"
                           field "频道 ID")))
      (setf (alist-get 'display_name qq-guild-user--profile)
            "Updated member")
      (qq-guild-user--request-sync view)
      (qq-guild-user--sync-now view)
      (should (equal key
                     (get-text-property (point)
                                        qq-guild-user--position-property))))))

(provide 'qq-guild-user-test)

;;; qq-guild-user-test.el ends here
