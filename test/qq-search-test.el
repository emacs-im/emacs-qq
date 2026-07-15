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
  "Evaluate BODY in an initialized live Appkit search view.

BODY may refer to the lexical variable `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-search-mode)
           (setq qq-search--session-key "group:20001")
           (let ((view (qq-search--ensure-view)))
             ,@body))
       (qq-runtime-stop))))

(defun qq-search-test--sync (view)
  "Synchronize pending search presentation state for VIEW."
  (qq-search--request-sync view)
  (qq-search--sync-now view))

(ert-deftest qq-search-sync-highlights-preview-only-and-has-no-open-button ()
  (qq-search-test-with-buffer
   (setq qq-search--query "Alice"
         qq-search--results
         (list (qq-search-test--result "11" 100 "Alice wrote this" "Alice"))
         qq-search--seen (make-hash-table :test #'equal)
         qq-search--next-cursor nil
         qq-search--status 'eof)
   (qq-search-test--sync view)
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
   (qq-search-test--sync view)
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
   (qq-search-test--sync view)
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
       (qq-search-test--sync view)
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
   (qq-search-test--sync view)
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
                (lambda (session cursor projection callback &optional _errback)
                  (should (equal session "group:20001"))
                  (should (eq projection 'summary))
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
                (lambda (session cursor projection callback &optional _errback)
                  (should (equal session "group:20001"))
                  (should (eq projection 'summary))
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
                (lambda (session _cursor projection _callback &optional errback)
                  (should (equal session "group:20001"))
                  (should (eq projection 'summary))
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
     (qq-search-test--sync view)
     (goto-char (point-min))
     (should (qq-search-next-result))
     (cl-letf (((symbol-function 'qq-chat-open-message)
                (lambda (&rest args) (setq call args))))
       (qq-search-open-result)
       (should (equal call '("group:20001" "11" "needle")))))))

(ert-deftest qq-search-sync-reuses-result-nodes-without-incremental-erase ()
  (qq-search-test-with-buffer
    (let* ((first (qq-search-test--result "11" 100 "first"))
           (second (qq-search-test--result "10" 90 "second"))
           (first-key (cons 'result (qq-search--result-key first))))
      (setq qq-search--query "result"
            qq-search--results (list first)
            qq-search--results-tail (last qq-search--results)
            qq-search--seen (make-hash-table :test #'equal)
            qq-search--status 'ready
            qq-search--next-cursor "next")
      (puthash (qq-search--result-key first) t qq-search--seen)
      (qq-search-test--sync view)
      (let ((first-node (gethash first-key qq-search--node-table)))
        (should first-node)
        (qq-search--append-results (list second))
        (setq qq-search--status 'eof
              qq-search--next-cursor nil)
        (cl-letf (((symbol-function 'erase-buffer)
                   (lambda ()
                     (ert-fail "incremental search sync erased the buffer"))))
          (qq-search--request-sync view)
          (appkit-sync-invalidations view))
        (should (eq first-node (gethash first-key qq-search--node-table)))
        (should (string-match-p "Loaded: 2" (buffer-string)))
        (should (string-match-p "second" (buffer-string)))))))

(ert-deftest qq-search-status-keeps-semantic-position-when-page-is-inserted ()
  (qq-search-test-with-buffer
    (let ((first (qq-search-test--result "11" 100 "first"))
          (second (qq-search-test--result "10" 90 "second")))
      (setq qq-search--query "result"
            qq-search--results (list first)
            qq-search--results-tail (last qq-search--results)
            qq-search--seen (make-hash-table :test #'equal)
            qq-search--status 'ready
            qq-search--next-cursor "next")
      (puthash (qq-search--result-key first) t qq-search--seen)
      (qq-search-test--sync view)
      (goto-char
       (appkit-position-find-property-value
        (point-min) (point-max) 'qq-search-entry-key 'status))
      (qq-search--append-results (list second))
      (setq qq-search--status 'eof
            qq-search--next-cursor nil)
      (qq-search-test--sync view)
      (should (eq 'status
                  (get-text-property (point) 'qq-search-entry-key)))
      (should (equal 'header
                     (get-text-property (point-min) 'qq-search-entry-key))))))

(ert-deftest qq-search-open-reuses-renamed-view-across-session-replacement ()
  (let ((qq-runtime--app nil)
        (qq-search-buffer-name "*qq-search-rename-test*")
        callbacks
        cancelled
        first
        first-view)
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'qq-api-cancel-request)
                   (lambda (request) (push request cancelled)))
                  ((symbol-function 'qq-api-search-messages-start)
                   (lambda (session query callback &optional _errback _limit)
                     (push (list session query callback) callbacks)
                     (if (equal session "group:20001")
                         'first-request
                       'second-request))))
          (setq first (qq-search-open "group:20001" "first"))
          (setq first-view (with-current-buffer first (appkit-current-view)))
          (let ((first-owner
                 (with-current-buffer first qq-search--request-owner)))
            (with-current-buffer first
              (rename-buffer "*qq-search-renamed*" t))
            (let* ((second (qq-search-open "private:10002" "second"))
                   (second-owner
                    (with-current-buffer second qq-search--request-owner))
                   (first-callback (nth 2 (assoc "group:20001" callbacks)))
                   (second-callback (nth 2 (assoc "private:10002" callbacks))))
              (should (eq first second))
              (should (eq first-view
                          (with-current-buffer second (appkit-current-view))))
              (should (eq qq-search--view-id (appkit-view-id first-view)))
              (should-not (get-buffer qq-search-buffer-name))
              (should (equal cancelled '(first-request)))
              (with-current-buffer second
                (should (equal qq-search--session-key "private:10002"))
                (should (equal qq-search--query "second"))
                ;; Each dimension is necessary on its own: the old owner is
                ;; stale even under the current session, and the old session is
                ;; stale even when paired with the current owner.
                (should-not
                 (qq-search--request-current-p
                  first-view second "private:10002" first-owner))
                (should-not
                 (qq-search--request-current-p
                  first-view second "group:20001" second-owner)))
              (funcall first-callback
                       `((results . (,(qq-search-test--result
                                      "11" 100 "stale first session")))
                         (next_cursor)))
              (with-current-buffer second
                (should (eq qq-search--request-owner second-owner))
                (should-not qq-search--results)
                (should (eq qq-search--status 'loading)))
              (funcall second-callback '((results) (next_cursor)))
              (with-current-buffer second
                (should-not qq-search--request-owner)
                (should-not qq-search--results)
                (should (eq qq-search--status 'eof))))))
      (when (buffer-live-p first)
        (kill-buffer first))
      (qq-runtime-stop))))

(ert-deftest qq-search-detach-erases-renamed-buffer-before-runtime-replacement ()
  "A detached singleton must not retain rendered data from the old account."
  (let ((qq-runtime--app nil)
        (qq-search-buffer-name "*qq-search-detach-privacy-test*")
        buffer
        old-view
        (cancel-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer) #'ignore)
                  ((symbol-function 'qq-api-search-messages-start)
                   (lambda (_session _query _callback &optional _errback _limit)
                     'search-request))
                  ((symbol-function 'qq-api-cancel-request)
                   (lambda (_request)
                     (cl-incf cancel-count)
                     (when (= cancel-count 1)
                       (error "Synthetic cancellation failure")))))
          (setq buffer (qq-search-open "group:20001" "OLD QUERY SECRET"))
          (with-current-buffer buffer
            (setq old-view (appkit-current-view)
                  qq-search--results
                  (list (qq-search-test--result
                         "900719925474099312345" 100
                         "OLD RESULT SECRET" "OLD SENDER SECRET"))
                  qq-search--results-tail (last qq-search--results)
                  qq-search--seen (make-hash-table :test #'equal)
                  qq-search--status 'eof)
            (qq-search-test--sync old-view)
            (should (string-match-p "OLD RESULT SECRET" (buffer-string)))
            (rename-buffer "*qq-search-renamed-detached-privacy*" t))

          ;; Appkit intentionally leaves the user's buffer alive.  The view
          ;; release hook must therefore erase both presentation and model.
          (qq-runtime-stop)
          (should (buffer-live-p buffer))
          (with-current-buffer buffer
            (should-not (appkit-current-view))
            (should (equal (buffer-string) ""))
            (should-not qq-search--ewoc)
            (should-not qq-search--node-table)
            (should-not qq-search--session-key)
            (should-not qq-search--query)
            (should-not qq-search--results)
            (should-not qq-search--seen)
            (should-not qq-search--consumed-cursors))

          ;; A new runtime with the same stable app identity reclaims the
          ;; renamed detached buffer, but only after replacement setup has
          ;; crossed the account-clean boundary.
          (let ((replacement
                 (qq-search-open "private:10002" "NEW QUERY")))
            (should (eq replacement buffer))
            (with-current-buffer replacement
              (should-not (eq old-view (appkit-current-view)))
              (should (equal qq-search--session-key "private:10002"))
              (should (equal qq-search--query "NEW QUERY"))
              (should-not (string-match-p
                           "OLD \(?:QUERY\|RESULT\|SENDER\) SECRET"
                           (buffer-string))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop))))

(ert-deftest qq-search-api-callback-defers-presentation-to-view-sync ()
  (qq-search-test-with-buffer
    (let (success)
      (cl-letf (((symbol-function 'qq-api-search-messages-start)
                 (lambda (_session _query callback &optional _errback _limit)
                   (setq success callback)
                   'search-token)))
        (qq-search-search "needle"))
      (let ((before (buffer-string)))
        (should (string-match-p "Loading" before))
        (cl-letf (((symbol-function 'appkit-sync-invalidations)
                   (lambda (&rest _arguments)
                     (ert-fail "API callback synchronized presentation directly"))))
          (funcall success
                   `((results . (,(qq-search-test--result
                                   "11" 100 "needle")))
                     (next_cursor))))
        (should (= (length qq-search--results) 1))
        (should (equal before (buffer-string)))
        (appkit-sync-invalidations view)
        (should (string-match-p "needle" (buffer-string)))
        (should (string-match-p "End of results" (buffer-string)))))))

(ert-deftest qq-search-dead-view-makes-late-page-callback-inert ()
  (qq-search-test-with-buffer
    (let (success)
      (cl-letf (((symbol-function 'qq-api-search-messages-start)
                 (lambda (_session _query callback &optional _errback _limit)
                   (setq success callback)
                   'search-token)))
        (qq-search-search "needle"))
      (appkit-kill-view view)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "late callback requested sync for a dead view"))))
        (funcall success
                 `((results . (,(qq-search-test--result
                                 "11" 100 "stale")))
                   (next_cursor))))
      (should-not qq-search--results)
      (should-not qq-search--request-owner))))

(provide 'qq-search-test)

;;; qq-search-test.el ends here
