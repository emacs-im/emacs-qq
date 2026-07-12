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
