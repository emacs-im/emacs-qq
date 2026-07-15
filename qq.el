;;; qq.el --- NapCat-backed QQ chat client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm
;; Version: 0.1.0
;; URL: https://github.com/0WD0/emacs-qq
;; Package-Requires: ((emacs "27.1") (websocket "1.16") (transient "0.7") (appkit "0.2.0"))

;;; Commentary:

;; emacs-qq provides a small but usable QQ client inside Emacs.
;;
;; Current MVP scope:
;; - connect to NapCat over OneBot websocket
;; - browse recent sessions in an appkit-backed root buffer
;; - open one chat buffer, fetch history, send text messages
;; - keep state updated from websocket events
;; - transient menus for root / chat / message / attachments

;;; Code:

(require 'subr-x)
(require 'qq-customize)
(require 'qq-runtime)
(require 'qq-state)
(require 'qq-transport)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-search)
(require 'qq-forward)
(require 'qq-red-packet)
(require 'qq-user-photo)
(require 'qq-user)
(require 'qq-guild-user)
(require 'qq-guild-channel)
(require 'qq-guild-forum)
(require 'qq-group)
(require 'qq-contacts)
(require 'qq-guilds)
(require 'qq-root)
(require 'qq-transient)
(require 'qq-notifications)
(require 'qq-modes)

(defconst qq--client-major-modes
  '(qq-chat-mode
    qq-contacts-mode
    qq-forward-mode
    qq-group-mode
    qq-group-notices-mode
    qq-guild-channel-mode
    qq-guild-forum-mode
    qq-guild-user-mode
    qq-guilds-mode
    qq-red-packet-mode
    qq-root-mode
    qq-search-mode
    qq-user-mode
    qq-user-photo-mode)
  "Major modes whose buffers contain account-scoped QQ client data.")

(defun qq--collect-client-buffers ()
  "Return live buffers owned by the current QQ client session.

The Appkit registry finds renamed views by ownership, while the explicit
major-mode list also finds legacy QQ buffers that are not Appkit views."
  (let (buffers)
    (when (appkit-app-p qq-runtime--app)
      (maphash
       (lambda (_id view)
         (when-let* ((buffer (and (appkit-view-p view)
                                  (appkit-view-buffer view))))
           (when (and (buffer-live-p buffer)
                      (not (memq buffer buffers)))
             (push buffer buffers))))
       (appkit-app-view-registry qq-runtime--app)))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (apply #'derived-mode-p qq--client-major-modes))
                 (not (memq buffer buffers)))
        (push buffer buffers)))
    (nreverse buffers)))

(defun qq--kill-client-buffer (buffer)
  "Kill account-scoped QQ BUFFER without allowing a query to retain it.

Normal kill hooks run first so legacy buffer-owned work is cancelled.  If a
broken hook signals, force the already-selected QQ buffer closed so generated
account data is not left visible."
  (when (buffer-live-p buffer)
    (condition-case error-data
        (with-current-buffer buffer
          (let ((kill-buffer-query-functions nil)
                (buffer-offer-save nil))
            (set-buffer-modified-p nil)
            (unless (kill-buffer buffer)
              (error "QQ buffer refused normal closure"))))
      (error
       (message "qq: buffer cleanup failed for %s; forcing close: %s"
                (buffer-name buffer) (error-message-string error-data))
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((kill-buffer-hook nil)
                 (kill-buffer-query-functions nil)
                 (buffer-offer-save nil))
             (set-buffer-modified-p nil)
             (unless (kill-buffer buffer)
               (error "QQ buffer refused forced closure")))))))))

(defun qq--kill-client-buffers (buffers)
  "Kill every live account-scoped QQ buffer in BUFFERS."
  (dolist (buffer buffers)
    (condition-case error-data
        (qq--kill-client-buffer buffer)
      (error
       ;; Continue closing the remaining client buffers.  If Emacs refuses to
       ;; kill this one even with hooks and queries disabled, at least remove
       ;; the old account's generated contents.
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((inhibit-read-only t))
             (widen)
             (erase-buffer)
             (set-buffer-modified-p nil))))
       (message "qq: could not close %s: %s"
                (if (buffer-live-p buffer) (buffer-name buffer) "QQ buffer")
                (error-message-string error-data))))))

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
  "Destructively log out of the in-memory emacs-qq session.

Transport and Appkit ownership are stopped first, then store reset hooks run,
and finally all account-scoped QQ buffers are closed.  Buffers are collected
before Appkit detaches renamed views; legacy QQ major modes are included too."
  (interactive)
  (let ((buffers (qq--collect-client-buffers)))
    (unwind-protect
        (unwind-protect
            (qq-runtime-stop)
          (qq-state-reset))
      ;; Include a QQ buffer created reentrantly by a reset hook, while keeping
      ;; the pre-stop registry snapshot needed for renamed Appkit views.
      (qq--kill-client-buffers
       (append buffers (qq--collect-client-buffers)))))
  (message "qq: session state reset; client buffers closed"))

(provide 'qq)

;;; qq.el ends here
