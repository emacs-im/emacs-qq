;;; qq-group-notices-test.el --- Tests for group notices -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-group-notices)

(defconst qq-group-notices-test--items
  '(((notice_id . "notice-one")
     (sender_id . "10001")
     (published_at . 1700000000)
     (title . "Maintenance")
     (text . "The group will be read-only tonight.")
     (images . (((id . "picture-one") (width . 640) (height . 480))))
     (read_count . 12)
     (read . t)
     (confirmation_required . t)
     (all_confirmed . :false)))
  "One closed group-notice fixture.")

(defmacro qq-group-notices-test-with-view (&rest body)
  "Evaluate BODY in a live Appkit group-notices view.

BODY may refer to the lexical variable `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-group-notices-mode)
           (setq qq-group-notices--group-id "20001")
           (let ((view (qq-group-notices--ensure-view)))
             ,@body))
       (qq-runtime-stop))))

(defun qq-group-notices-test--sync (view)
  "Synchronize pending group-notices presentation state for VIEW."
  (qq-group-notices--request-sync view)
  (qq-group-notices--sync-now view))

(ert-deftest qq-api-get-group-notices-validates-closed-response ()
  (let (action params result)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (candidate-action candidate-params callback
                        &optional _errback)
                 (setq action candidate-action
                       params candidate-params)
                 (funcall callback
                          `((data . ((group_id . "20001")
                                     (notices . ,qq-group-notices-test--items)))))
                 'request)))
      (should (eq (qq-api-get-group-notices
                   "20001" (lambda (notices) (setq result notices)))
                  'request))
      (should (equal action "emacs_get_group_notices"))
      (should (equal params '((group_id . "20001"))))
      (should (equal result qq-group-notices-test--items))
      (should-error (qq-api-get-group-notices 20001 #'ignore)
                    :type 'user-error))))

(ert-deftest qq-api-get-group-notices-rejects-numeric-sender-identity ()
  (let (reason)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          `((data . ((group_id . "20001")
                                     (notices .
                                      (((notice_id . "notice-one")
                                        (sender_id . 10001)
                                        (published_at . 1)
                                        (title)
                                        (text . "Synthetic")
                                        (images)
                                        (read_count)
                                        (read)
                                        (confirmation_required)
                                        (all_confirmed))))))))
                 'request)))
      (qq-api-get-group-notices
       "20001" #'ignore
       (lambda (_response candidate-reason) (setq reason candidate-reason)))
      (should (string-match-p "sender_id" reason)))))

(ert-deftest qq-group-notices-sync-shows-read-only-announcements ()
  (qq-group-notices-test-with-view
    (setq qq-group-notices--group-name "Emacs Users"
          qq-group-notices--items (copy-tree qq-group-notices-test--items))
    (qq-group-notices-test--sync view)
    (let ((text (buffer-string)))
      (should (string-match-p "Maintenance" text))
      (should (string-match-p "read-only tonight" text))
      (should (string-match-p "10001" text))
      (should (string-match-p "12 人已读" text))
      (should (string-match-p "需要确认" text))
      (should (string-match-p "图片 1 · 640×480" text))
      (goto-char (point-min))
      (search-forward "10001")
      (should (button-at (1- (point)))))))

(ert-deftest qq-group-notices-refresh-handles-synchronous-response ()
  (qq-group-notices-test-with-view
    (cl-letf (((symbol-function 'qq-api-get-group-notices)
               (lambda (_group-id callback &optional _errback)
                 (funcall callback (copy-tree qq-group-notices-test--items))
                 'request)))
      (qq-group-notices-refresh)
      (should (equal qq-group-notices--items qq-group-notices-test--items))
      (should-not qq-group-notices--loading)
      (should-not qq-group-notices--request)
      (should-not qq-group-notices--request-owner))))

(ert-deftest qq-group-notices-refresh-retains-content-and-reuses-node ()
  (qq-group-notices-test-with-view
    (setq qq-group-notices--items (copy-tree qq-group-notices-test--items))
    (qq-group-notices-test--sync view)
    (let* ((key '(notice "20001" "notice-one"))
           (node (gethash key qq-group-notices--node-table))
           success)
      (should node)
      (cl-letf (((symbol-function 'qq-api-get-group-notices)
                 (lambda (_group-id callback &optional _errback)
                   (setq success callback)
                   'notice-token)))
        (qq-group-notices-refresh))
      (should qq-group-notices--loading)
      (should (string-match-p "正在刷新群公告" (buffer-string)))
      (should (string-match-p "Maintenance" (buffer-string)))
      (let* ((updated (copy-tree qq-group-notices-test--items))
             (notice (car updated))
             (before (buffer-string)))
        (setf (alist-get 'text notice) "Updated announcement text.")
        (cl-letf (((symbol-function 'appkit-sync-invalidations)
                   (lambda (&rest _arguments)
                     (ert-fail "notice callback synchronized directly"))))
          (funcall success updated))
        (should (equal before (buffer-string)))
        (should (equal "Updated announcement text."
                       (alist-get 'text (car qq-group-notices--items))))
        (cl-letf (((symbol-function 'erase-buffer)
                   (lambda ()
                     (ert-fail "incremental notice sync erased the buffer"))))
          (appkit-sync-invalidations view))
        (should (eq node (gethash key qq-group-notices--node-table)))
        (should (string-match-p "Updated announcement text"
                                (buffer-string)))))))

(ert-deftest qq-group-notices-key-separates-identical-ids-across-groups ()
  (qq-group-notices-test-with-view
    (setq qq-group-notices--items (copy-tree qq-group-notices-test--items))
    (qq-group-notices-test--sync view)
    (let* ((first-key '(notice "20001" "notice-one"))
           (first-node (gethash first-key qq-group-notices--node-table))
           (second-items (copy-tree qq-group-notices-test--items)))
      (should first-node)
      (setf (alist-get 'title (car second-items)) "Other group notice")
      (setq qq-group-notices--group-id "20002"
            qq-group-notices--items second-items)
      (qq-group-notices-test--sync view)
      (let ((second-node
             (gethash '(notice "20002" "notice-one")
                      qq-group-notices--node-table)))
        (should second-node)
        (should-not (eq first-node second-node))
        (should-not (gethash first-key qq-group-notices--node-table))
        (should (string-match-p "Other group notice" (buffer-string)))))))

(ert-deftest qq-group-notices-stale-errback-is-silent-and-inert ()
  (qq-group-notices-test-with-view
    (let (first-errback errbacks)
      (cl-letf (((symbol-function 'qq-api-get-group-notices)
                 (lambda (_group-id _callback &optional errback)
                   (push errback errbacks)
                   (list 'notice-token (length errbacks))))
                ((symbol-function 'qq-api-cancel-request) #'ignore))
        (qq-group-notices-refresh)
        (setq first-errback (car errbacks))
        (qq-group-notices-refresh))
      (let ((current-owner qq-group-notices--request-owner)
            (current-request qq-group-notices--request))
        (cl-letf (((symbol-function 'qq-api--default-error)
                   (lambda (&rest _arguments)
                     (ert-fail "stale errback emitted a default error")))
                  ((symbol-function 'appkit-request-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "stale errback requested a presentation sync"))))
          (funcall first-errback nil "stale failure"))
        (should (eq current-owner qq-group-notices--request-owner))
        (should (eq current-request qq-group-notices--request))
        (should qq-group-notices--loading)
        (should-not qq-group-notices--error)))))

(ert-deftest qq-group-notices-dead-view-makes-late-response-inert ()
  (qq-group-notices-test-with-view
    (let (success)
      (cl-letf (((symbol-function 'qq-api-get-group-notices)
                 (lambda (_group-id callback &optional _errback)
                   (setq success callback)
                   'notice-token)))
        (qq-group-notices-refresh))
      (appkit-kill-view view)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "late notice response requested a dead sync"))))
        (funcall success (copy-tree qq-group-notices-test--items)))
      (should-not qq-group-notices--items)
      (should-not qq-group-notices--request-owner))))

(ert-deftest qq-group-notices-replacement-view-can-restart-cancelled-load ()
  (qq-group-notices-test-with-view
    (let ((calls 0)
          cancelled)
      (cl-letf (((symbol-function 'qq-api-get-group-notices)
                 (lambda (_group-id _callback &optional _errback)
                   (cl-incf calls)
                   (list 'notice-request calls)))
                ((symbol-function 'qq-api-cancel-request)
                 (lambda (request) (push request cancelled))))
        (setq qq-group-notices--items
              (copy-tree qq-group-notices-test--items))
        (qq-group-notices-refresh)
        (setq qq-group-notices--error "old account error")
        (should qq-group-notices--loading)
        (should qq-group-notices--items)
        (should qq-group-notices--error)
        (appkit-kill-view view)
        (should-not qq-group-notices--loading)
        (should-not qq-group-notices--request)
        (should-not qq-group-notices--request-owner)
        (should-not qq-group-notices--items)
        (should-not qq-group-notices--error)
        (should (= 1 (length cancelled)))
        (let ((replacement (qq-group-notices--ensure-view)))
          (should (appkit-view-live-p replacement))
          (qq-group-notices-refresh)
          (should (= 2 calls))
          (should qq-group-notices--loading))))))

(ert-deftest qq-group-notices-cross-runtime-refetches-without-old-content ()
  (let* ((buffer-name
          (generate-new-buffer-name " *qq-group-notices-runtime*"))
         (buffer (get-buffer-create buffer-name))
         (app-one (appkit-start-app 'qq :id 'notices-old-runtime))
         (app-two nil)
         (qq-runtime--app app-one)
         (old-items (copy-tree qq-group-notices-test--items))
         replacement-buffer
         callback
         cancelled
         calls)
    (setf (alist-get 'title (car old-items)) "OLD ACCOUNT ANNOUNCEMENT"
          (alist-get 'text (car old-items)) "OLD ACCOUNT PRIVATE CONTENT")
    (unwind-protect
        (cl-letf (((symbol-function 'qq-group-notices--buffer-name)
                   (lambda () buffer-name))
                  ((symbol-function 'pop-to-buffer)
                   (lambda (candidate &rest _arguments) candidate))
                  ((symbol-function 'qq-api-cancel-request)
                   (lambda (request) (push request cancelled))))
          (with-current-buffer buffer
            (qq-group-notices-mode)
            (setq qq-group-notices--group-id "20001"
                  qq-group-notices--group-name "Old Account Group")
            (let ((view-one (qq-group-notices--ensure-view)))
              (setq qq-group-notices--items old-items
                    qq-group-notices--error "OLD ACCOUNT ERROR"
                    qq-group-notices--loading t
                    qq-group-notices--request 'old-notice-request
                    qq-group-notices--request-owner '(old-notice-owner))
              (qq-group-notices-test--sync view-one)
              (should (string-match-p "OLD ACCOUNT ANNOUNCEMENT"
                                      (buffer-string)))
              ;; Reusing the same live view must not discard its active state.
              (should (eq view-one (qq-group-notices--ensure-view)))
              (should qq-group-notices--items)
              (appkit-stop-app app-one)
              (should-not (appkit-view-live-p view-one))
              (should-not qq-group-notices--items)
              (should-not qq-group-notices--error)
              (should-not qq-group-notices--loading)
              (should-not qq-group-notices--request)
              (should-not qq-group-notices--request-owner)
              (should-not
               (string-match-p "OLD ACCOUNT" (buffer-string)))
              (should (equal cancelled '(old-notice-request)))))
          (setq app-two (appkit-start-app 'qq :id 'notices-new-runtime)
                qq-runtime--app app-two)
          (cl-letf (((symbol-function 'qq-api-get-group-notices)
                     (lambda (group-id success &optional _failure)
                       (setq calls (1+ (or calls 0))
                             callback success)
                       (should (equal group-id "20001"))
                       'new-notice-request)))
            (setq replacement-buffer
                  (qq-group-notices-open "20001" "New Group"))
            (should (buffer-live-p replacement-buffer))
            (should-not (eq buffer replacement-buffer)))
          (with-current-buffer replacement-buffer
            (let ((view-two (appkit-current-view)))
              (should (appkit-view-live-p view-two))
              (should (= calls 1))
              (should qq-group-notices--loading)
              (should-not qq-group-notices--items)
              (should-not qq-group-notices--error)
              (should-not
               (string-match-p "OLD ACCOUNT" (buffer-string)))
              (funcall callback (copy-tree qq-group-notices-test--items))
              (qq-group-notices--sync-now view-two)
              (should (equal qq-group-notices--items
                             qq-group-notices-test--items))
              (should-not
               (string-match-p "OLD ACCOUNT" (buffer-string))))))
      (when (appkit-app-live-p app-one)
        (appkit-stop-app app-one))
      (when (appkit-app-live-p app-two)
        (appkit-stop-app app-two))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (buffer-live-p replacement-buffer)
        (kill-buffer replacement-buffer)))))

(provide 'qq-group-notices-test)

;;; qq-group-notices-test.el ends here
