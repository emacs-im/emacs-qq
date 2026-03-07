;;; qq-ui.el --- Shared UI rendering primitives for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; QQ UI compatibility wrappers built directly on top of disco-ui.

;;; Code:

(require 'disco-ui)

(defalias 'qq-ui-insert-action-button #'disco-ui-insert-action-button)
(defalias 'qq-ui-insert-styled-line #'disco-ui-insert-styled-line)
(defalias 'qq-ui-prefix-state-p #'disco-ui-prefix-state-p)
(defalias 'qq-ui-make-prefix-state #'disco-ui-make-prefix-state)
(defalias 'qq-ui-prefix-state-current #'disco-ui-prefix-state-current)
(defalias 'qq-ui-prefix-state-rest #'disco-ui-prefix-state-rest)
(defalias 'qq-ui-prefix-state-consume #'disco-ui-prefix-state-consume)
(defalias 'qq-ui-prefix-string #'disco-ui-prefix-string)
(defalias 'qq-ui-apply-line-prefix #'disco-ui-apply-line-prefix)
(defalias 'qq-ui-insert-prefixed-lines #'disco-ui-insert-prefixed-lines)
(defalias 'qq-ui-render-list-view #'disco-ui-render-list-view)
(defalias 'qq-ui-append-face #'disco-ui-append-face)

(defvaralias 'qq-ui-card-indent-prefix 'disco-ui-card-indent-prefix)
(defvaralias 'qq-ui-card-indent-prefix-state 'disco-ui-card-indent-prefix-state)

(defun qq-ui-card-line-prefix (&optional indent)
  "Return a display-only card prefix string using disco-ui.

INDENT defaults to `qq-ui-card-indent-prefix'."
  (disco-ui-card-line-prefix :indent indent))

(defun qq-ui-card-prefix-state (&optional indent)
  "Return card line-prefix state using disco-ui.

INDENT defaults to `qq-ui-card-indent-prefix'."
  (disco-ui-card-prefix-state :indent indent))

(provide 'qq-ui)

;;; qq-ui.el ends here
