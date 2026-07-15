;;; qq-red-packet-test.el --- Tests for QQ red packets -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-red-packet)
(require 'qq-state)

(defconst qq-red-packet-test--message-id "7467703692092974645")

(defun qq-red-packet-test--presentation (title)
  `((background . 100)
    (icon . 4)
    (title . ,title)
    (sub_title . "Open the packet")
    (content . "QQ red packet")
    (notice . ,(concat "[packet]" title))))

(defun qq-red-packet-test--segment ()
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

(provide 'qq-red-packet-test)

;;; qq-red-packet-test.el ends here
