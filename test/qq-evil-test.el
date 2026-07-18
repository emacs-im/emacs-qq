;;; qq-evil-test.el --- Tests for emacs-qq Evil bindings -*- lexical-binding: t; -*-

(require 'ert)
(require 'qq)
(require 'evil)

(ert-deftest qq-evil-root-keeps-native-prefix-and-defines-actions ()
  (with-temp-buffer
    (qq-root-mode)
    (evil-normal-state)
    (should (eq (key-binding (kbd "RET"))
                #'qq-root-open-at-point))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g r"))
                #'qq-root-refresh))
    (should (eq (key-binding (kbd "g u"))
                #'qq-root-next-unread))
    (should (eq (key-binding (kbd "n"))
                #'evil-search-next))
    (evil-motion-state)
    (should (eq (key-binding (kbd "RET"))
                #'qq-root-open-at-point))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))))

(ert-deftest qq-evil-emacs-state-retains-the-ordinary-root-map ()
  (with-temp-buffer
    (qq-root-mode)
    (evil-emacs-state)
    (should (eq (key-binding (kbd "g")) #'qq-root-refresh))
    (should (eq (key-binding (kbd "n")) #'qq-root-button-forward))
    (should (eq (key-binding (kbd "RET"))
                #'qq-root-open-at-point))))

(ert-deftest qq-evil-read-only-surfaces-use-modal-action-keys ()
  (dolist (case
           '((qq-contacts-mode-map "RET" qq-contacts-open-at-point)
             (qq-contacts-mode-map "g j" qq-contacts-next-item)
             (qq-forward-mode-map "g r" qq-forward-refresh)
             (qq-forward-mode-map "g k" qq-forward-previous-message)
             (qq-guilds-mode-map "RET" appkit-directory-activate)
             (qq-guilds-mode-map "g j" appkit-directory-next-item)
             (qq-guild-forum-mode-map "RET" qq-guild-forum-open-post)
             (qq-guild-forum-post-mode-map
              "g k" appkit-discussion-previous-entry)
             (qq-user-mode-map "P" qq-user-open-photo-wall)
             (qq-group-mode-map "g n" qq-group-open-notices)
             (qq-red-packet-mode-map "c" qq-red-packet-grab)))
    (pcase-let ((`(,map-symbol ,key ,command) case))
      (with-temp-buffer
        (use-local-map (symbol-value map-symbol))
        (evil-normal-state)
        (should (eq (key-binding (kbd key)) command))
        (should (eq (key-binding (kbd "g g"))
                    #'evil-goto-first-line))))))

(ert-deftest qq-evil-chat-keeps-prefixes-and-the-composer-boundary ()
  (with-temp-buffer
    (use-local-map qq-chat-mode-map)
    (evil-normal-state)
    (should (eq (key-binding (kbd "RET")) #'evil-ret))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g r")) #'qq-chat-refresh))
    (qq-chat-timeline-mode 1)
    (should (eq (key-binding (kbd "r")) #'qq-chat-reply-to-message))
    (should (eq (key-binding (kbd "d d")) #'qq-chat-delete-message))
    (should (eq (key-binding (kbd "R")) #'qq-chat-forward-transient))
    (should (eq (key-binding (kbd "g q")) #'qq-chat-goto-reply))
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (qq-chat-timeline-mode -1)
    (should-not (eq (key-binding (kbd "r"))
                    #'qq-chat-reply-to-message))))

(ert-deftest qq-evil-chat-emacs-state-retains-timeline-single-keys ()
  (with-temp-buffer
    (use-local-map qq-chat-mode-map)
    (qq-chat-timeline-mode 1)
    (evil-emacs-state)
    (should (eq (key-binding (kbd "g")) #'qq-chat-goto-reply))
    (should (eq (key-binding (kbd "d")) #'qq-chat-delete-message))
    (should (eq (key-binding (kbd "f")) #'qq-chat-forward-transient))))

(ert-deftest qq-evil-applies-bindings-to-a-lazily-loaded-surface ()
  (unless (featurep 'qq-group-notices)
    (should (seq-some
             (lambda (entry)
               (eq (nth 1 entry) 'qq-group-notices-mode-map))
             appkit-evil--deferred-bindings)))
  (require 'qq-group-notices)
  (appkit-evil--after-load "qq-group-notices")
  (with-temp-buffer
    (use-local-map qq-group-notices-mode-map)
    (evil-normal-state)
    (should (eq (key-binding (kbd "g r"))
                #'qq-group-notices-refresh))
    (should (eq (key-binding (kbd "g g"))
                #'evil-goto-first-line))))

(provide 'qq-evil-test)

;;; qq-evil-test.el ends here
