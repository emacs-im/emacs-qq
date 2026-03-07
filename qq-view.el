;;; qq-view.el --- Cursor/view preservation helpers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; QQ view compatibility wrappers built directly on top of disco-view.

;;; Code:

(require 'disco-view)

(defalias 'qq-view-capture-position #'disco-view-capture-position)
(defalias 'qq-view-restore-position #'disco-view-restore-position)
(defalias 'qq-view-render-preserving-position
  #'disco-view-render-preserving-position)
(defalias 'qq-view-render-list-spec #'disco-view-render-list-spec)
(defalias 'qq-view-render-list-spec-preserving-position
  #'disco-view-render-list-spec-preserving-position)
(defalias 'qq-view-list-spec-create #'disco-view-list-spec-create)
(defalias 'qq-view-insert-label-row #'disco-view-insert-label-row)
(defalias 'qq-view-insert-heading-line #'disco-view-insert-heading-line)
(defalias 'qq-view-insert-note-line #'disco-view-insert-note-line)
(defalias 'qq-view-insert-action-line #'disco-view-insert-action-line)
(defalias 'qq-view-one-line-row-create #'disco-view-one-line-row-create)
(defalias 'qq-view-canonicalize-number #'disco-view-canonicalize-number)
(defalias 'qq-view-truncate-fill #'disco-view-truncate-fill)
(defalias 'qq-view-elide-string #'disco-view-elide-string)
(defalias 'qq-view-current-column #'disco-view-current-column)
(defalias 'qq-view-move-to-column #'disco-view-move-to-column)
(defalias 'qq-view-one-line-column-widths #'disco-view-one-line-column-widths)
(defalias 'qq-view-insert-one-line-row #'disco-view-insert-one-line-row)

(provide 'qq-view)

;;; qq-view.el ends here
