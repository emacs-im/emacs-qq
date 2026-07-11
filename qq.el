;;; qq.el --- NapCat-backed QQ client for Emacs -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors
;; Keywords: comm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (websocket "1.16") (transient "0.3") (plz "0.8") (appkit "0.1") (disco "0"))

;;; Commentary:

;; emacs-qq provides a small but usable QQ client inside Emacs.
;;
;; Current MVP scope:
;; - connect to NapCat over OneBot websocket
;; - browse recent sessions in a disco-style root buffer
;; - open one chat buffer, fetch history, send text messages
;; - keep state updated from websocket events
;; - transient menus for root / chat / message / attach (telega/disco style)

;;; Code:

(require 'qq-customize)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-transport)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-forward)
(require 'qq-user-photo)
(require 'qq-user)
(require 'qq-group)
(require 'qq-root)
(require 'qq-transient)
(require 'qq-notifications)
(require 'qq-modes)

;;;###autoload
(defun qq ()
  "Start emacs-qq and open the root buffer."
  (interactive)
  (qq-runtime-app)
  (qq-root-open)
  (qq-connect))

;;;###autoload
(defun qq-connect ()
  "Start emacs-qq websocket transport."
  (interactive)
  (qq-transport-start))

;;;###autoload
(defun qq-disconnect ()
  "Stop emacs-qq websocket transport."
  (interactive)
  (qq-transport-stop))

;;;###autoload
(defun qq-refresh ()
  "Refresh runtime data from NapCat.

When transport is not connected yet, start it and wait for bootstrap.
When transport is already open, request a fresh snapshot immediately."
  (interactive)
  (if (qq-transport-running-p)
      (qq-api-refresh)
    (progn
      (qq-connect)
      (message "qq: connecting; initial refresh will run after lifecycle.connect"))))

;;;###autoload
(defun qq-reset-session-state ()
  "Clear in-memory transport and store state used by emacs-qq."
  (interactive)
  (qq-runtime-stop)
  (qq-state-reset)
  (message "qq: in-memory state reset"))

(provide 'qq)

;;; qq.el ends here
