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
    (let* ((key '(notice . "notice-one"))
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

(provide 'qq-group-notices-test)

;;; qq-group-notices-test.el ends here
