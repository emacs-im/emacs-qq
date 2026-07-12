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
                     (guild_id . "")))))
         (validated
          (qq-protocol-validate-poke-recall-reference reference "poke")))
    (should (equal validated reference))
    (should-not (eq validated reference))
    (dolist
        (invalid
         '(nil
           ((message_id . 9007199254742007089)
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . ""))))
           ((message_id . "0")
             (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . ""))))
           ((message_id . "01")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . ""))))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 3) (peer_uid . "u_x") (guild_id . ""))))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "") (guild_id . ""))))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "g"))))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")
                     (extra . t))))
           ((message_id . "9007199254742007089")
            (peer . ((chat_type . 1) (peer_uid . "u_x") (guild_id . "")))
            (extra . t))))
      (should-not (qq-protocol-poke-recall-reference-p invalid))
      (should-error
       (qq-protocol-validate-poke-recall-reference invalid "poke")))))

(provide 'qq-protocol-test)

;;; qq-protocol-test.el ends here
