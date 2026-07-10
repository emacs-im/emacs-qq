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

(provide 'qq-protocol-test)

;;; qq-protocol-test.el ends here
