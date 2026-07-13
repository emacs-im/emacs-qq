;;; qq-search-test.el --- Tests for qq-search -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-search)

(defun qq-search-test--result (id time preview &optional sender)
  "Return a strict search result with ID, TIME, PREVIEW, and SENDER."
  `((chat . ((kind . "group") (group_id . "20001")))
    (message_id . ,id)
    (message_seq . ,id)
    (sent_at . ,time)
    (sender . ((user_id . "10001") (name . ,(or sender "Alice"))))
    (preview . ,preview)))

(defmacro qq-search-test-with-buffer (&rest body)
  "Evaluate BODY in an initialized search buffer."
  `(with-temp-buffer
     (qq-search-mode)
     (setq qq-search--session-key "group:20001")
     ,@body))

(ert-deftest qq-search-render-highlights-preview-only-and-has-no-open-button ()
  (qq-search-test-with-buffer
   (setq qq-search--query "Alice"
         qq-search--results
         (list (qq-search-test--result "11" 100 "Alice wrote this" "Alice"))
         qq-search--seen (make-hash-table :test #'equal)
         qq-search--next-cursor nil
         qq-search--status 'eof)
   (qq-search--render)
   (goto-char (point-min))
   (qq-search-next-result)
   (beginning-of-line)
   (search-forward "Alice")
   (should-not (eq (get-text-property (1- (point)) 'face) 'isearch))
   (search-forward "Alice")
   (let ((face (get-text-property (1- (point)) 'face)))
     (should (or (eq face 'isearch)
                 (and (listp face) (memq 'isearch face)))))
   (should-not (string-match-p "\\[Open\\]" (buffer-string)))
   (beginning-of-line)
   (should (equal (alist-get 'message_id (qq-search--result-at-point)) "11"))))

(ert-deftest qq-search-result-row-clicks-without-blanket-hover ()
  (qq-search-test-with-buffer
   (setq qq-search--query "needle"
         qq-search--results
         (list (qq-search-test--result "11" 100 "needle"))
         qq-search--seen (make-hash-table :test #'equal)
         qq-search--next-cursor nil
         qq-search--status 'eof)
   (qq-search--render)
   (goto-char (point-min))
   (should (qq-search-next-result))
   (beginning-of-line)
   (let ((button (button-at (point))))
     (should button)
     (should (eq (button-type button) 'appkit-ui-action-row-button))
     (should (eq (button-get button 'appkit-ui-action-row-object)
                 (qq-search--result-at-point)))
     (should (eq (key-binding [mouse-1]) #'push-button))
     ;; Mouse-1 must stay a direct button command instead of being
     ;; translated to the default Mouse-2 yank path.
     (should-not (mouse-on-link-p (point)))
     (should-not
      (text-property-not-all
       (line-beginning-position) (line-beginning-position 2)
       'mouse-face nil))
     (should-not (button-at (line-end-position)))
     (let (call)
       (cl-letf (((symbol-function 'qq-chat-open-message)
                  (lambda (&rest args) (setq call args))))
       (button-activate button)
       (should (equal call '("group:20001" "11" "needle"))))))))

(ert-deftest qq-search-return-does-not-open-result-from-terminating-newline ()
  (qq-search-test-with-buffer
   (setq qq-search--query "needle"
         qq-search--results
         (list (qq-search-test--result "11" 100 "needle"))
         qq-search--seen (make-hash-table :test #'equal)
         qq-search--next-cursor nil
         qq-search--status 'eof)
   (qq-search--render)
   (goto-char (point-min))
   (should (qq-search-next-result))
   (end-of-line)
   (should-not (button-at (point)))
   (should-error (qq-search-open-result) :type 'user-error)))

(ert-deftest qq-search-result-button-dispatches-real-primary-click ()
  (save-window-excursion
    (qq-search-test-with-buffer
     (let ((snowflake "900719925474099312345")
           call)
       (setq qq-search--query "needle"
             qq-search--results
             (list (qq-search-test--result "900719925474099300001"
                                           101 "first needle")
                   (qq-search-test--result snowflake 100 "clicked needle"))
             qq-search--seen (make-hash-table :test #'equal)
             qq-search--next-cursor nil
             qq-search--status 'eof)
       (qq-search--render)
       (switch-to-buffer (current-buffer))
       (goto-char (point-min))
       (should (qq-search-next-result))
       (should (qq-search-next-result))
       (let* ((clicked-pos (point))
              ;; The sixth POSN field is the buffer position consulted by
              ;; Emacs when resolving text-property keymaps for mouse events.
              (posn (list (selected-window) clicked-pos '(0 . 0)
                          0 nil clicked-pos))
              (down-event (list 'down-mouse-1 posn))
              (up-event (list 'mouse-1 posn))
              (mouse-1-click-follows-link 450))
         (cl-letf (((symbol-function 'qq-chat-open-message)
                    (lambda (&rest args) (setq call args))))
           (execute-kbd-macro (vector down-event up-event)))
         (should (equal call
                        (list "group:20001" snowflake "needle"))))))))

(ert-deftest qq-search-previous-at-point-min-is-bounded ()
  (qq-search-test-with-buffer
   (setq qq-search--query "x"
         qq-search--results
         (list (qq-search-test--result "11" 100 "x"))
         qq-search--seen (make-hash-table :test #'equal)
         qq-search--next-cursor nil
         qq-search--status 'eof)
   (qq-search--render)
   (goto-char (point-min))
   (should-not (qq-search-previous-result))
   (should (= (point) (point-min)))))

(ert-deftest qq-search-mode-has-whole-line-navigation-bindings ()
  (should (eq (lookup-key qq-search-mode-map (kbd "n"))
              #'qq-search-next-result))
  (should (eq (lookup-key qq-search-mode-map (kbd "p"))
              #'qq-search-previous-result))
  (should (eq (lookup-key qq-search-mode-map (kbd "RET"))
              #'qq-search-open-result))
  (should (eq (lookup-key qq-search-mode-map (kbd "m"))
              #'qq-search-load-more))
  (should (eq (lookup-key qq-search-mode-map (kbd "g"))
              #'qq-search-refresh))
  (should (eq (lookup-key qq-search-mode-map (kbd "s"))
              #'qq-search-search))
  (should (eq (lookup-key qq-search-mode-map (kbd "q")) #'quit-window)))

(ert-deftest qq-search-paginates-deduplicates-and-reaches-explicit-eof ()
  (qq-search-test-with-buffer
   (let ((first (qq-search-test--result "11" 100 "first"))
         (second (qq-search-test--result "10" 90 "second"))
         next-cursor)
     (cl-letf (((symbol-function 'qq-api-search-messages-start)
                (lambda (_session _query callback &optional _errback _limit)
                  (funcall callback
                           `((results . (,first))
                             (next_cursor . "cursor-1")))
                  'start-token))
               ((symbol-function 'qq-api-search-messages-next)
                (lambda (cursor callback &optional _errback)
                  (setq next-cursor cursor)
                  (funcall callback
                           `((results . (,first ,second))
                             (next_cursor)))
                  'next-token)))
       (qq-search-search "  first  ")
       (should (equal qq-search--query "first"))
       (should (equal qq-search--next-cursor "cursor-1"))
       (qq-search-load-more)
       (should (equal next-cursor "cursor-1"))
       (should-not qq-search--next-cursor)
       (should (eq qq-search--status 'eof))
       (should (equal (mapcar (lambda (result)
                               (alist-get 'message_id result))
                             qq-search--results)
                      '("11" "10")))
       (should (string-match-p "Loaded: 2" (buffer-string)))
       (should (string-match-p "End of results" (buffer-string)))))))

(ert-deftest qq-search-next-result-consumes-pages-until-a-new-row-arrives ()
  (qq-search-test-with-buffer
   (let ((first (qq-search-test--result "11" 100 "first"))
         (second (qq-search-test--result "10" 90 "second"))
         cursors)
     (cl-letf (((symbol-function 'qq-api-search-messages-start)
                (lambda (_session _query callback &optional _errback _limit)
                  (funcall callback
                           `((results . (,first))
                             (next_cursor . "cursor-1")))
                  'start-token))
               ((symbol-function 'qq-api-search-messages-next)
                (lambda (cursor callback &optional _errback)
                  (push cursor cursors)
                  (if (equal cursor "cursor-1")
                      (funcall callback
                               `((results . (,first))
                                 (next_cursor . "cursor-2")))
                    (funcall callback
                             `((results . (,second))
                               (next_cursor))))
                  'next-token)))
       (qq-search-search "first")
       (should (equal (alist-get 'message_id (qq-search--result-at-point))
                      "11"))
       (should (qq-search-next-result))
       (should (equal (nreverse cursors) '("cursor-1" "cursor-2")))
       (should (equal (alist-get 'message_id (qq-search--result-at-point))
                      "10"))
       (should-not qq-search--pending-next-key)
       (should (eq qq-search--status 'eof))))))

(ert-deftest qq-search-consumes-single-use-cursor-before-failure ()
  (qq-search-test-with-buffer
   (let ((next-calls 0))
     (cl-letf (((symbol-function 'qq-api-search-messages-start)
                (lambda (_session _query callback &optional _errback _limit)
                  (funcall callback
                           `((results . (,(qq-search-test--result
                                          "11" 100 "x")))
                             (next_cursor . "single-use")))
                  'start-token))
               ((symbol-function 'qq-api-search-messages-next)
                (lambda (_cursor _callback &optional errback)
                  (cl-incf next-calls)
                  (funcall errback nil "network")
                  'next-token)))
       (qq-search-search "x")
       (qq-search-load-more)
       (should (= next-calls 1))
       (should-not qq-search--next-cursor)
       (should-not qq-search--request-owner)
       (should (eq qq-search--status 'error))
       (should-error (qq-search-load-more) :type 'user-error)
       (should (= next-calls 1))))))

(ert-deftest qq-search-empty-page-can-retain-an-explicit-continuation ()
  (qq-search-test-with-buffer
   (cl-letf (((symbol-function 'qq-api-search-messages-start)
              (lambda (_session _query callback &optional _errback _limit)
                (funcall callback
                         '((results) (next_cursor . "empty-next")))
                'request)))
     (qq-search-search "missing")
     (should (eq qq-search--status 'ready))
     (should (equal qq-search--next-cursor "empty-next"))
     (should (string-match-p "m: load more" (buffer-string))))))

(ert-deftest qq-search-synchronous-dispatch-signal-clears-loading-owner ()
  (qq-search-test-with-buffer
   (cl-letf (((symbol-function 'qq-api-search-messages-start)
              (lambda (&rest _args) (error "transport unavailable"))))
     (should-not (qq-search-search "needle"))
     (should-not qq-search--request)
     (should-not qq-search--request-owner)
     (should (eq qq-search--status 'error))
     (should (string-match-p "transport unavailable" qq-search--error))
     (should (string-match-p "Error:" (buffer-string))))))

(ert-deftest qq-search-open-result-reuses-exact-around-jump-entrypoint ()
  (qq-search-test-with-buffer
   (let ((result (qq-search-test--result "11" 100 "needle")) call)
     (setq qq-search--query "needle"
           qq-search--results (list result)
           qq-search--seen (make-hash-table :test #'equal)
           qq-search--next-cursor nil
           qq-search--status 'eof)
     (qq-search--render)
     (cl-letf (((symbol-function 'qq-chat-open-message)
                (lambda (&rest args) (setq call args))))
       (qq-search-open-result)
       (should (equal call '("group:20001" "11" "needle")))))))

(provide 'qq-search-test)

;;; qq-search-test.el ends here
