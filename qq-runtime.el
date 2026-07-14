;;; qq-runtime.el --- Appkit session ownership for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Own the one default QQ application session used by emacs-qq buffers.
;; Protocol state remains in `qq-state'; appkit owns lifecycle and views.

;;; Code:

(require 'appkit-core)

(declare-function qq-transport-stop "qq-transport")

(defun qq-runtime--shutdown (_app)
  "Stop transport resources owned by the default QQ app session."
  (when (fboundp 'qq-transport-stop)
    (qq-transport-stop)))

(appkit-define-app-kind qq
  :shutdown #'qq-runtime--shutdown)

(defvar qq-runtime--app nil
  "Default live appkit session for emacs-qq.")

(defun qq-runtime-app ()
  "Return emacs-qq's live default appkit session."
  (unless (appkit-app-live-p qq-runtime--app)
    (setq qq-runtime--app
          (appkit-start-app 'qq :id 'default)))
  qq-runtime--app)

(defun qq-runtime-stop ()
  "Stop and forget emacs-qq's default appkit session."
  (when (appkit-app-p qq-runtime--app)
    (appkit-stop-app qq-runtime--app))
  (setq qq-runtime--app nil))

(provide 'qq-runtime)

;;; qq-runtime.el ends here
