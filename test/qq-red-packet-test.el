;;; qq-red-packet-test.el --- Tests for QQ red packets -*- lexical-binding: t; -*-

;;; Commentary:

;; Regression coverage for native QQ red-packet protocol and views.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-red-packet)
(require 'qq-state)

(defconst qq-red-packet-test--message-id "7467703692092974645"
  "Opaque NT message identity used by red-packet fixtures.")

(defun qq-red-packet-test--presentation (title)
  "Return a native wallet presentation carrying TITLE."
  `((background . 100)
    (icon . 4)
    (title . ,title)
    (sub_title . "Open the packet")
    (content . "QQ red packet")
    (notice . ,(concat "[packet]" title))))

(defun qq-red-packet-test--segment ()
  "Return one closed native red-packet segment fixture."
  (copy-tree
   `((type . "wallet")
     (data
      (wallet_kind . "red-packet")
      (message_type . 2)
      (session_type . 1)
      (red_type . 2)
      (red_channel . 1)
      (grab_state . 0)
      (grabbed_amount . "0")
      (sender . ,(qq-red-packet-test--presentation ""))
      (receiver . ,(qq-red-packet-test--presentation "Best wishes"))))))

(defun qq-red-packet-test--detail ()
  "Return one closed native red-packet detail fixture."
  `((message_id . ,qq-red-packet-test--message-id)
    (send_order
     (sender_id . "10001")
     (sender_name . "Sender")
     (wishing . "Best wishes")
     (total_count . "2")
     (total_amount . "200")
     (channel . 1)
     (business_type . 2)
     (receiver_type . 1)
     (created_at . 1700000000)
     (expires_at . 1700086400)
     (state . 0)
     (received_count . "1")
     (received_amount . "100")
     (lucky_user_id . "0"))
    (receipts
     . [((user_id . "10002")
         (name . "Receiver")
         (amount . "100")
         (received_at . 1700000100))])))

(defmacro qq-red-packet-test-with-view (&rest body)
  "Evaluate BODY in a live Appkit packet view bound as `view'."
  (declare (indent 0) (debug t))
  `(let ((qq-runtime--app nil))
     (unwind-protect
         (with-temp-buffer
           (qq-red-packet-mode)
           (setq qq-red-packet--session-key "private:10002"
                 qq-red-packet--message-id qq-red-packet-test--message-id
                 qq-red-packet--segment (qq-red-packet-test--segment)
                 qq-red-packet--outgoing-p nil)
           (let ((view (qq-red-packet--ensure-view)))
             (ignore view)
             ,@body))
       (qq-runtime-stop))))

(ert-deftest qq-red-packet-preview-uses-native-presentation ()
  (should (equal "[packet]Best wishes"
                 (qq-state-message-preview-from-segments
                  (list (qq-red-packet-test--segment)))))
  (should
   (equal
    (qq-red-packet-test--segment)
    (qq-state--emacs-search-segment-to-internal
     `((kind . "wallet")
       (payload . ,(alist-get 'data (qq-red-packet-test--segment))))))))

(ert-deftest qq-red-packet-chat-card-is-wholly-interactive-without-open-label ()
  (let ((opened nil)
        (message `((server-id . ,qq-red-packet-test--message-id)
                   (self-p . nil))))
    (with-temp-buffer
      (setq-local qq-chat--session-key "private:10002")
      (cl-letf (((symbol-function 'qq-red-packet-open)
                 (lambda (&rest arguments) (setq opened arguments))))
        (qq-chat--insert-wallet-segment
         (qq-red-packet-test--segment) message nil nil)
        (should (string-match-p "QQ 红包" (buffer-string)))
        (should (string-match-p "Best wishes" (buffer-string)))
        (should-not (string-match-p "\\[Open\\]" (buffer-string)))
        (goto-char (point-min))
        (should (button-at (point)))
        (button-activate (button-at (point)))
        (should (equal opened
                       (list "private:10002"
                             qq-red-packet-test--message-id
                             (qq-red-packet-test--segment)
                             nil)))))))

(ert-deftest qq-wallet-transfer-card-does-not-expose-red-packet-action ()
  (let* ((segment (qq-red-packet-test--segment))
         (data (alist-get 'data segment)))
    (setf (alist-get 'message_type data) 1
          (alist-get 'wallet_kind data) "transfer"
          (alist-get 'red_type data) 0
          (alist-get 'content (alist-get 'receiver data)) "转账"
          (alist-get 'title (alist-get 'receiver data)) "¥ 0.10")
    (with-temp-buffer
      (setq-local qq-chat--session-key "private:10002")
      (qq-chat--insert-wallet-segment
       segment
       `((server-id . ,qq-red-packet-test--message-id) (self-p . nil))
       nil nil)
      (should (string-match-p "转账" (buffer-string)))
      (goto-char (point-min))
      (should-not (button-at (point))))))

(ert-deftest qq-password-red-packet-card-opens-native-page ()
  (let* ((segment (qq-red-packet-test--segment))
         (data (alist-get 'data segment))
         opened)
    (setf (alist-get 'wallet_kind data) "password-red-packet"
          (alist-get 'message_type data) 6
          (alist-get 'red_type data) 1
          (alist-get 'red_channel data) 32)
    (with-temp-buffer
      (setq-local qq-chat--session-key "private:10002")
      (cl-letf (((symbol-function 'qq-red-packet-open)
                 (lambda (&rest arguments) (setq opened arguments))))
        (qq-chat--insert-wallet-segment
         segment
         `((server-id . ,qq-red-packet-test--message-id) (self-p . nil))
         nil nil)
        (should (string-match-p "口令红包" (buffer-string)))
        (goto-char (point-min))
        (button-activate (button-at (point)))
        (should opened)))))

(ert-deftest qq-password-red-packet-page-does-not-request-unsupported-detail ()
  (let* ((segment (qq-red-packet-test--segment))
         (data (alist-get 'data segment))
         detail-called
         buffer)
    (setf (alist-get 'wallet_kind data) "password-red-packet"
          (alist-get 'message_type data) 6)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'qq-api-get-red-packet-detail)
                     (lambda (&rest _arguments) (setq detail-called t))))
            (setq buffer
                  (qq-red-packet-open
                   "private:10002" qq-red-packet-test--message-id
                   segment nil)))
          (should-not detail-called)
          (with-current-buffer buffer
            (should (string-match-p "领取口令红包" (buffer-string)))
            (should-not (string-match-p "刷新详情" (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest qq-unknown-wallet-card-is-not-interactive ()
  (let* ((segment (qq-red-packet-test--segment))
         (data (alist-get 'data segment)))
    (setf (alist-get 'wallet_kind data) "unknown"
          (alist-get 'message_type data) 99)
    (with-temp-buffer
      (setq-local qq-chat--session-key "private:10002")
      (qq-chat--insert-wallet-segment
       segment
       `((server-id . ,qq-red-packet-test--message-id) (self-p . nil))
       nil nil)
      (goto-char (point-min))
      (should-not (button-at (point))))))

(ert-deftest qq-api-password-red-packet-claim-validates-discriminant ()
  (let (result)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback
                          `((data . ((message_id . ,qq-red-packet-test--message-id)
                                     (interaction . "password")
                                     (password_message_id
                                      . "7467703692092974646"))))))))
      (qq-api-grab-red-packet
       "private:10002" qq-red-packet-test--message-id
       (lambda (value) (setq result value)))
      (should (equal (alist-get 'interaction result) "password")))))

(ert-deftest qq-api-red-packet-detail-validates-closed-response ()
  (let (captured-action captured-params result)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (action params callback &optional _errback)
                 (setq captured-action action
                       captured-params params)
                 (funcall callback `((data . ,(qq-red-packet-test--detail)))))))
      (qq-api-get-red-packet-detail
       "private:10002" qq-red-packet-test--message-id
       (lambda (value) (setq result value)))
      (should (equal captured-action "emacs_get_red_packet_detail"))
      (should
       (equal captured-params
              `((chat (kind . "private") (user_id . "10002"))
                (message_id . ,qq-red-packet-test--message-id))))
      (should (equal result (qq-red-packet-test--detail))))))

(ert-deftest qq-api-red-packet-detail-rejects-capability-leaks ()
  (let ((bad (append (qq-red-packet-test--detail)
                     '((authkey . "must-not-enter-client"))))
        failure)
    (cl-letf (((symbol-function 'qq-api-call)
               (lambda (_action _params callback &optional _errback)
                 (funcall callback `((data . ,bad))))))
      (qq-api-get-red-packet-detail
       "private:10002" qq-red-packet-test--message-id #'ignore
       (lambda (_response reason) (setq failure reason)))
      (should (string-match-p "invalid fields" failure)))))

(ert-deftest qq-red-packet-amount-formatting-does-not-use-floating-point ()
  (should (equal "¥0.00" (qq-red-packet--format-amount "0")))
  (should (equal "¥0.05" (qq-red-packet--format-amount "5")))
  (should (equal "¥2.00" (qq-red-packet--format-amount "200")))
  (should (equal "¥12345678901234567890.12"
                 (qq-red-packet--format-amount "1234567890123456789012"))))

(ert-deftest qq-red-packet-view-id-keeps-session-and-message-id-opaque ()
  (should
   (equal '(red-packet "private:10002" "7467703692092974645")
          (qq-red-packet--view-id
           "private:10002" "7467703692092974645"))))

(ert-deftest qq-red-packet-renamed-live-and-detached-buffer-is-reused ()
  (let ((qq-runtime--app nil)
        buffer first-view
        (calls 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments) candidate))
                    ((symbol-function 'qq-api-get-red-packet-detail)
                     (lambda (_session-key _message-id callback
                              &optional _errback)
                       (cl-incf calls)
                       (funcall callback (qq-red-packet-test--detail))
                       (list 'detail-request calls))))
            (setq buffer
                  (qq-red-packet-open
                   "private:10002" qq-red-packet-test--message-id
                   (qq-red-packet-test--segment) nil))
            (with-current-buffer buffer
              (setq first-view (appkit-current-view)
                    qq-red-packet--notice "preserved live notice")
              (rename-buffer "*renamed-red-packet*" t))
            (should
             (eq buffer
                 (qq-red-packet-open
                  "private:10002" qq-red-packet-test--message-id
                  (qq-red-packet-test--segment) nil)))
            (with-current-buffer buffer
              (should (eq first-view (appkit-current-view)))
              (should (equal qq-red-packet--detail
                             (qq-red-packet-test--detail)))
              (should (equal qq-red-packet--notice
                             "preserved live notice"))
              (appkit-kill-view first-view)
              (should-not qq-red-packet--detail)
              (should-not qq-red-packet--notice))
            (should
             (eq buffer
                 (qq-red-packet-open
                  "private:10002" qq-red-packet-test--message-id
                  (qq-red-packet-test--segment) nil)))
            (with-current-buffer buffer
              (should-not (eq first-view (appkit-current-view)))
              (should (appkit-view-live-p (appkit-current-view))))
            (should (= calls 2))))
      (qq-runtime-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest qq-red-packet-runtime-replacement-resets-owned-work-via-setup ()
  (let ((qq-runtime--app nil)
        buffer old-view new-view
        cancelled
        (calls 0))
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (candidate &rest _arguments) candidate))
                    ((symbol-function 'qq-api-get-red-packet-detail)
                     (lambda (_session-key _message-id _callback
                              &optional _errback)
                       (cl-incf calls)
                       (when (= calls 2)
                         (should-not qq-red-packet--detail)
                         (should-not qq-red-packet--error)
                         (should-not qq-red-packet--notice)
                         (should-not qq-red-packet--grabbing))
                       (list 'detail-request calls)))
                    ((symbol-function 'qq-api-cancel-request)
                     (lambda (request) (push request cancelled))))
            (setq buffer
                  (qq-red-packet-open
                   "private:10002" qq-red-packet-test--message-id
                   (qq-red-packet-test--segment) nil))
            (with-current-buffer buffer
              (setq old-view (appkit-current-view)
                    qq-red-packet--detail (qq-red-packet-test--detail)
                    qq-red-packet--error "old account error"
                    qq-red-packet--notice "old account notice"
                    qq-red-packet--grabbing t
                    qq-red-packet--grab-request 'old-grab-request
                    qq-red-packet--grab-owner '(old-grab-owner))
              (rename-buffer "*renamed-runtime-red-packet*" t))
            (qq-runtime-stop)
            (with-current-buffer buffer
              (should-not qq-red-packet--loading)
              (should-not qq-red-packet--grabbing)
              (should-not qq-red-packet--detail)
              ;; Force stale state back in; replacement :setup must clear and
              ;; cancel it before dispatching the new runtime's request.
              (setq qq-red-packet--detail (qq-red-packet-test--detail)
                    qq-red-packet--error "stale"
                    qq-red-packet--notice "stale"
                    qq-red-packet--loading t
                    qq-red-packet--request 'retained-detail-request
                    qq-red-packet--request-owner '(retained-detail-owner)
                    qq-red-packet--grabbing t
                    qq-red-packet--grab-request 'retained-grab-request
                    qq-red-packet--grab-owner '(retained-grab-owner)))
            (should
             (eq buffer
                 (qq-red-packet-open
                  "private:10002" qq-red-packet-test--message-id
                  (qq-red-packet-test--segment) nil)))
            (with-current-buffer buffer
              (setq new-view (appkit-current-view))
              (should (appkit-view-live-p new-view))
              (should-not (eq old-view new-view))
              (should qq-red-packet--loading)
              (should-not qq-red-packet--grabbing)
              (should-not qq-red-packet--detail)
              (should-not qq-red-packet--error)
              (should-not qq-red-packet--notice))
            ;; A second open finds the live registry entry and preserves its
            ;; active request instead of replaying :setup.
            (should
             (eq buffer
                 (qq-red-packet-open
                  "private:10002" qq-red-packet-test--message-id
                  (qq-red-packet-test--segment) nil)))
            (with-current-buffer buffer
              (should (eq new-view (appkit-current-view)))
              (should qq-red-packet--loading))
            (should (= calls 2))
            (dolist (request '((detail-request 1) old-grab-request
                               retained-detail-request retained-grab-request))
              (should (member request cancelled)))))
      (qq-runtime-stop)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest qq-red-packet-stale-and-dead-detail-responses-are-inert ()
  (qq-red-packet-test-with-view
    (let (callbacks)
      (cl-letf (((symbol-function 'qq-api-get-red-packet-detail)
                 (lambda (_session-key _message-id callback
                          &optional _errback)
                   (push callback callbacks)
                   (list 'detail-request (length callbacks))))
                ((symbol-function 'qq-api-cancel-request) #'ignore))
        (qq-red-packet-refresh)
        (let ((first (car callbacks)))
          (qq-red-packet-refresh)
          (let ((owner qq-red-packet--request-owner)
                (request qq-red-packet--request))
            (cl-letf (((symbol-function 'appkit-request-sync)
                       (lambda (&rest _arguments)
                         (ert-fail "stale detail callback requested sync"))))
              (funcall first (qq-red-packet-test--detail)))
            (should (eq owner qq-red-packet--request-owner))
            (should (eq request qq-red-packet--request)))))
      (let ((latest (car callbacks)))
        (appkit-kill-view view)
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "dead detail callback requested sync"))))
          (funcall latest (qq-red-packet-test--detail)))
        (should-not qq-red-packet--detail)
        (should-not qq-red-packet--request-owner)))))

(ert-deftest qq-red-packet-detail-callback-requests-one-atomic-appkit-sync ()
  (qq-red-packet-test-with-view
    (let (success calls)
      (cl-letf (((symbol-function 'qq-api-get-red-packet-detail)
                 (lambda (_session-key _message-id callback
                          &optional _errback)
                   (setq success callback)
                   'detail-request)))
        (qq-red-packet-refresh))
      (let ((before (buffer-string)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (candidate &rest options)
                     (push (cons candidate options) calls)))
                  ((symbol-function 'qq-red-packet-render)
                   (lambda () (ert-fail "detail callback rendered directly")))
                  ((symbol-function 'appkit-invalidate)
                   (lambda (&rest _arguments)
                     (ert-fail "detail callback split invalidation")))
                  ((symbol-function 'appkit-schedule-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "detail callback scheduled directly"))))
          (funcall success (qq-red-packet-test--detail)))
        (should (equal before (buffer-string)))
        (should (equal qq-red-packet--detail
                       (qq-red-packet-test--detail)))
        (should
         (equal calls (list (list view :structure t :part 'packet))))))))

(ert-deftest qq-red-packet-grab-callback-queues-post-sync-detail-refresh ()
  (qq-red-packet-test-with-view
    (let (grab-success
          (detail-calls 0))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'qq-api-grab-red-packet)
                 (lambda (_session-key _message-id callback
                          &optional _errback)
                   (setq grab-success callback)
                   'grab-request))
                ((symbol-function 'qq-api-get-red-packet-detail)
                 (lambda (_session-key _message-id _callback
                          &optional _errback)
                   (cl-incf detail-calls)
                   'detail-request)))
        (qq-red-packet-grab)
        (let ((before (buffer-string))
              (real-request-sync (symbol-function 'appkit-request-sync))
              calls)
          (cl-letf (((symbol-function 'appkit-request-sync)
                     (lambda (candidate &rest options)
                       (push (cons candidate options) calls)
                       (apply real-request-sync candidate options)))
                    ((symbol-function 'qq-red-packet-render)
                     (lambda () (ert-fail "grab callback rendered directly")))
                    ((symbol-function 'qq-red-packet-refresh)
                     (lambda () (ert-fail "grab callback refreshed directly"))))
            (funcall grab-success '((interaction . "normal")))
            (should (= detail-calls 0))
            (should (equal before (buffer-string)))
            (should (= 1 (length calls)))
            (should
             (equal (appkit-view-pending-events-snapshot view)
                    `((:type refresh-detail
                       :session-key "private:10002"
                       :message-id ,qq-red-packet-test--message-id)))))
          (qq-red-packet--sync-now view)
          (should (= detail-calls 1))
          (should qq-red-packet--loading)
          (should-not (appkit-view-pending-events-snapshot view)))))))

(ert-deftest qq-red-packet-dead-view-makes-grab-response-inert ()
  (qq-red-packet-test-with-view
    (let (success)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                ((symbol-function 'qq-api-grab-red-packet)
                 (lambda (_session-key _message-id callback
                          &optional _errback)
                   (setq success callback)
                   'grab-request)))
        (qq-red-packet-grab))
      (appkit-kill-view view)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (&rest _arguments)
                   (ert-fail "dead grab callback requested sync"))))
        (funcall success '((interaction . "normal"))))
      (should-not qq-red-packet--notice)
      (should-not qq-red-packet--grab-owner))))

(ert-deftest qq-red-packet-sync-preserves-stable-receipt-position-key ()
  (qq-red-packet-test-with-view
    (setq qq-red-packet--detail (qq-red-packet-test--detail))
    (qq-red-packet--request-sync view)
    (qq-red-packet--sync-now view)
    (goto-char (point-min))
    (search-forward "Receiver")
    (let ((key (get-text-property (1- (point))
                                  qq-red-packet--position-property)))
      (should
       (equal key
              `(red-packet "private:10002"
                           ,qq-red-packet-test--message-id
                           receipt "10002")))
      (setf (alist-get 'name
                       (aref (alist-get 'receipts qq-red-packet--detail) 0))
            "Updated receiver")
      (qq-red-packet--request-sync view)
      (qq-red-packet--sync-now view)
      (should (equal key
                     (get-text-property (point)
                                        qq-red-packet--position-property))))))

(provide 'qq-red-packet-test)

;;; qq-red-packet-test.el ends here
