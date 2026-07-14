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

(ert-deftest qq-group-notices-render-shows-read-only-announcements ()
  (with-temp-buffer
    (qq-group-notices-mode)
    (setq qq-group-notices--group-id "20001"
          qq-group-notices--group-name "Emacs Users"
          qq-group-notices--items (copy-tree qq-group-notices-test--items))
    (qq-group-notices-render)
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
  (with-temp-buffer
    (qq-group-notices-mode)
    (setq qq-group-notices--group-id "20001")
    (cl-letf (((symbol-function 'qq-group-notices-render) #'ignore)
              ((symbol-function 'qq-api-get-group-notices)
               (lambda (_group-id callback &optional _errback)
                 (funcall callback (copy-tree qq-group-notices-test--items))
                 'request)))
      (qq-group-notices-refresh)
      (should (equal qq-group-notices--items qq-group-notices-test--items))
      (should-not qq-group-notices--loading)
      (should-not qq-group-notices--request)
      (should-not qq-group-notices--request-owner))))

(provide 'qq-group-notices-test)

;;; qq-group-notices-test.el ends here
