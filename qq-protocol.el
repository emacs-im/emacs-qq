;;; qq-protocol.el --- Wire value contracts for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Small, dependency-free decoders shared by the transport, API, and state
;; layers.  Identity values are intentionally not "normalized": accepting a
;; numeric NT message snowflake after JSON decoding would preserve an already
;; rounded value as if it were authoritative.

;;; Code:

(require 'subr-x)

(defun qq-protocol-message-id-p (value)
  "Return non-nil when VALUE is an original NT message snowflake string."
  (and (stringp value)
       (not (string-empty-p value))
       (string-match-p "\\`[0-9]+\\'" value)))

(defun qq-protocol-optional-message-id (value &optional context)
  "Return optional message-id VALUE after validating its wire representation.

Nil remains nil.  Every non-nil value must be an original decimal string.
CONTEXT is included in protocol errors."
  (cond
   ((null value) nil)
   ((qq-protocol-message-id-p value) value)
   (t
    (error "qq: %s requires message_id as an original decimal string, got %S"
           (or context "protocol payload") value))))

(defun qq-protocol-json-true-p (value)
  "Return non-nil only when wire VALUE explicitly represents JSON true."
  (or (eq value t)
      (and (numberp value) (not (zerop value)))
      (and (stringp value)
           (member (downcase value) '("true" "1" "yes")))))

(provide 'qq-protocol)

;;; qq-protocol.el ends here
