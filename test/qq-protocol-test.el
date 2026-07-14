;;; qq-protocol-test.el --- Tests for qq-protocol -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'qq-protocol)

(ert-deftest qq-protocol-message-id-never-canonicalizes-numbers ()
  (should (equal (qq-protocol-optional-message-id "9007199254742007089")
                 "9007199254742007089"))
  (should-not (qq-protocol-optional-message-id nil))
  (should-error
   (qq-protocol-optional-message-id 9007199254742007089)))

(ert-deftest qq-protocol-json-true-distinguishes-decoded-false ()
  (dolist (value '(t 1 "true" "1" "yes"))
    (should (qq-protocol-json-true-p value)))
  (dolist (value '(nil :false :null 0 "false" "0" "no"))
    (should-not (qq-protocol-json-true-p value))))

(ert-deftest qq-protocol-group-uin-is-canonical-uint32-string ()
  (should (qq-protocol-group-uin-p "1"))
  (should (qq-protocol-group-uin-p "4294967295"))
  (dolist (value '("0" "01" "4294967296" 20001 nil))
    (should-not (qq-protocol-group-uin-p value))))

(ert-deftest qq-protocol-emacs-chat-locator-rejects-out-of-range-group-uin ()
  (should
   (qq-protocol-emacs-chat-locator-p
    '((kind . "group") (group_id . "4294967295"))))
  (should-not
   (qq-protocol-emacs-chat-locator-p
    '((kind . "group") (group_id . "4294967296")))))

(defun qq-protocol-test--read-state ()
  "Return one complete strict authoritative read-state payload."
  '((unread_count . 3)
    (first_unread
     . ((sequence . "30001")
        (message_id . "9007199254742007089")))
    (mentions
     . ((at_me
         . ((sequence . "30003")
            (message_id . "9007199254742007091")))
        (at_all . nil)))
    (latest
     . ((message_id . "9007199254742007094")
        (sequence . "30005")))))

(ert-deftest qq-protocol-emacs-session-locator-is-a-closed-union ()
  (dolist
      (locator
       '(((kind . "group") (group_id . "20001"))
         ((kind . "private") (user_id . "10001"))
         ((kind . "dataline")
          (peer_uid . "device-1") (variant . "desktop"))
         ((kind . "dataline")
          (peer_uid . "device-2") (variant . "mobile"))
         ((kind . "service") (peer_uid . "u_mail"))))
    (let ((validated
           (qq-protocol-validate-emacs-session-locator locator "event.chat")))
      (should (equal validated locator))
      (should-not (eq validated locator))))
  (dolist
      (locator
       '(nil
         ((kind . "group") (group_id . 20001))
         ((kind . "group") (group_id . "4294967296"))
         ((kind . "group") (group_id . "20001") (extra . t))
         ((kind . "private") (user_id . ""))
         ((kind . "dataline") (peer_uid . "device-1"))
         ((kind . "dataline")
          (peer_uid . "device-1") (variant . "tablet"))
         ((kind . "service") (peer_uid . ""))
         ((kind . "channel") (peer_uid . "opaque"))))
    (should-not (qq-protocol-emacs-session-locator-p locator))
    (should-error
     (qq-protocol-validate-emacs-session-locator locator "event.chat"))))

(ert-deftest qq-protocol-emacs-read-state-is-closed-and-lossless ()
  (let* ((read-state (qq-protocol-test--read-state))
         (validated
          (qq-protocol-validate-emacs-read-state read-state "event.read_state")))
    (should (equal validated read-state))
    (should-not (eq validated read-state))
    (should (equal "9007199254742007089"
                   (alist-get 'message_id
                              (alist-get 'first_unread validated)))))
  (should
   (qq-protocol-emacs-read-state-p
    '((unread_count . 0)
      (first_unread . nil)
      (mentions . ((at_me . nil) (at_all . nil)))
      (latest . ((message_id . "9"))))))
  (dolist
      (read-state
       '(((unread_count . -1)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 1.0)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 9007199254740992)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . ((sequence . "10") (message_id . 11)))
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . ((sequence . 10) (message_id . "11")))
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . ((sequence . "10")))
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . nil)
          (mentions . ((at_me . nil)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil) (extra . t)))
          (latest . nil))
         ((unread_count . 1)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . ((message_id . 12))))
         ((unread_count . 1)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . ((message_id . "12") (sequence . nil))))
         ((unread_count . 1)
          (first_unread . nil)
          (mentions . ((at_me . nil) (at_all . nil)))
          (latest . nil)
          (extra . t))))
    (should-not (qq-protocol-emacs-read-state-p read-state))
    (should-error
     (qq-protocol-validate-emacs-read-state read-state "event.read_state"))))

(ert-deftest qq-protocol-emacs-mark-read-result-is-a-closed-tagged-union ()
  (let ((state (qq-protocol-test--read-state)))
    (dolist
        (result
         `(((scope . "message")
            (read_through_message_id . "9007199254742007094")
            (read_state . ,state))
           ((scope . "session")
            (requested_message_id . "9007199254742007094")
            (read_state . ,state))))
      (let ((validated
             (qq-protocol-validate-emacs-mark-read-result
              result "emacs_mark_read response")))
        (should (equal validated result))
        (should-not (eq validated result))))
    (dolist
        (invalid
         `(((scope . "message")
            (requested_message_id . "9007199254742007094")
            (read_state . ,state))
           ((scope . "session")
            (read_through_message_id . "9007199254742007094")
            (read_state . ,state))
           ((scope . "message")
            (read_through_message_id . 9007199254742007094)
            (read_state . ,state))
           ((scope . "session")
            (requested_message_id . "9007199254742007094")
            (read_state . ,state)
            (extra . t))))
      (should-not (qq-protocol-emacs-mark-read-result-p invalid))
      (should-error
       (qq-protocol-validate-emacs-mark-read-result
        invalid "emacs_mark_read response")))))

(ert-deftest qq-protocol-emacs-read-state-notice-requires-exact-envelope ()
  (let* ((notice
          `((time . 1710000200)
            (self_id . 90001)
            (post_type . "notice")
            (notice_type . "emacs_read_state")
            (chat . ((kind . "group") (group_id . "20001")))
            (read_state . ,(qq-protocol-test--read-state))))
         (validated
          (qq-protocol-validate-emacs-read-state-notice notice "event")))
    (should (equal validated notice))
    (should-not (eq validated notice))
    (dolist
        (invalid
         (list
          (append notice '((extra . t)))
          (assq-delete-all 'time (copy-tree notice))
          (cons '(time . 0) (assq-delete-all 'time (copy-tree notice)))
          (cons '(self_id . "90001")
                (assq-delete-all 'self_id (copy-tree notice)))
          (cons '(post_type . "meta_event")
                (assq-delete-all 'post_type (copy-tree notice)))
          (cons '(notice_type . "read_state")
                (assq-delete-all 'notice_type (copy-tree notice)))))
      (should-not (qq-protocol-emacs-read-state-notice-p invalid))
      (should-error
       (qq-protocol-validate-emacs-read-state-notice invalid "event")))))

(ert-deftest qq-protocol-poke-recall-reference-is-closed-and-lossless ()
  (let* ((reference
          '((message_id . "9007199254742007089")
            (peer . ((chat_type . 1)
                     (peer_uid . "u_NT-private-uid")
                     (guild_id . "")))
            (valid_before . 2000000000)))
         (validated
          (qq-protocol-validate-poke-recall-reference reference "poke")))
    (should (equal validated reference))
    (should-not (eq validated reference))
    (dolist
        (invalid
         '(nil
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . ""))))
           ((message_id . 9007199254742007089)
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000))
           ((message_id . "0")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000))
           ((message_id . "01")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 3) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "") (guild_id . "")))
            (valid_before . 2000000000))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "g")))
            (valid_before . 2000000000))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")
                     (extra . t)))
            (valid_before . 2000000000))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 0))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . -1))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000.0))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . "2000000000"))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 9007199254740992))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (valid_before . 2000000000)
            (extra . t))))
      (should-not (qq-protocol-poke-recall-reference-p invalid))
      (should-error
       (qq-protocol-validate-poke-recall-reference invalid "poke")))))

(ert-deftest qq-protocol-poke-recall-expiry-includes-the-deadline ()
  (let ((reference
         '((message_id . "9007199254742007089")
           (peer . ((chat_type . 2)
                    (peer_uid . "20001")
                    (guild_id . "")))
           (valid_before . 200))))
    (should-not
     (qq-protocol-poke-recall-reference-expired-p reference 199))
    (should
     (qq-protocol-poke-recall-reference-expired-p reference 200))
    (should
     (qq-protocol-poke-recall-reference-expired-p reference 201))))

(provide 'qq-protocol-test)

;;; qq-protocol-test.el ends here
