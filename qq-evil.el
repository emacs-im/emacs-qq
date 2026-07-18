;;; qq-evil.el --- Native Evil bindings for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs-qq keeps its ordinary maps as the Emacs-state interface and defines a
;; separate modal vocabulary here.  In particular, application refresh and
;; navigation use `gr' and `gj'/`gk', leaving native Evil prefixes such as `gg'
;; intact.  This integration is optional and does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'qq-customize)

(declare-function qq-group-notices-button-backward "qq-group-notices" ())
(declare-function qq-group-notices-refresh "qq-group-notices" ())
(declare-function evil-set-initial-state "evil-core" (mode state))
(declare-function turn-off-evil-snipe-mode "evil-snipe" ())
(declare-function turn-off-evil-snipe-override-mode "evil-snipe" ())

(eval-when-compile
  (unless (require 'evil nil t)
    (defun evil-set-initial-state (&rest _args) nil)))

(defgroup qq-evil nil
  "Optional native Evil integration for emacs-qq."
  :group 'qq
  :prefix "qq-evil-")

(defcustom qq-evil-enable-integration t
  "If non-nil, install emacs-qq's Evil bindings automatically."
  :type 'boolean
  :group 'qq-evil)

(defcustom qq-evil-initial-state 'normal
  "Initial Evil state used for emacs-qq buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'qq-evil)

(defconst qq-evil--application-modes
  '(qq-chat-mode
    qq-contacts-mode
    qq-forward-mode
    qq-group-mode
    qq-group-notices-mode
    qq-guild-channel-mode
    qq-guild-forum-mode
    qq-guild-forum-post-mode
    qq-guild-user-mode
    qq-guilds-mode
    qq-red-packet-mode
    qq-root-mode
    qq-search-mode
    qq-user-mode
    qq-user-photo-mode)
  "Major modes participating in emacs-qq's Evil integration.")

(defconst qq-evil--readonly-maps
  '(qq-contacts-mode-map
    qq-forward-mode-map
    qq-group-mode-map
    qq-group-notices-mode-map
    qq-guild-channel-mode-map
    qq-guild-forum-mode-map
    qq-guild-forum-post-mode-map
    qq-guild-user-mode-map
    qq-guilds-mode-map
    qq-red-packet-mode-map
    qq-root-mode-map
    qq-search-mode-map
    qq-user-mode-map
    qq-user-photo-mode-map)
  "Read-only emacs-qq keymaps with standard modal quit semantics.")

(defconst qq-evil--application-states '(normal motion)
  "Evil states used by emacs-qq application bindings.")

(defun qq-evil--set-initial-states ()
  "Register `qq-evil-initial-state' for all QQ application modes."
  (when qq-evil-initial-state
    (dolist (mode qq-evil--application-modes)
      (evil-set-initial-state mode qq-evil-initial-state))))

(defun qq-evil--define-readonly-keys ()
  "Install shared and surface-specific read-only bindings."
  (dolist (map qq-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))

  (appkit-evil-define-keys qq-evil--application-states 'qq-root-mode-map
    (kbd "RET") #'qq-root-open-at-point
    (kbd "<return>") #'qq-root-open-at-point
    (kbd "g r") #'qq-root-refresh
    (kbd "s") #'qq-root-search
    (kbd "c") #'qq-contacts-open
    (kbd "g G") #'qq-guilds-open
    (kbd "o") #'qq-root-open-session
    (kbd "a") #'qq-root-open-avatar-at-point
    (kbd "i") #'qq-root-open-info-at-point
    (kbd "I") #'qq-root-open-self-user
    (kbd "TAB") #'qq-root-tab-dwim
    (kbd "<backtab>") #'qq-root-button-backward
    (kbd "g u") #'qq-root-next-unread
    (kbd "?") #'qq-root-transient)

  (appkit-evil-define-keys qq-evil--application-states 'qq-contacts-mode-map
    (kbd "RET") #'qq-contacts-open-at-point
    (kbd "<return>") #'qq-contacts-open-at-point
    (kbd "g r") #'qq-contacts-refresh
    (kbd "s") #'qq-contacts-search
    (kbd "f") #'qq-contacts-show-friends
    (kbd "g G") #'qq-contacts-show-groups
    (kbd "g I") #'qq-contacts-show-not-recent-groups
    (kbd "m") #'qq-contacts-open-at-point
    (kbd "i") #'qq-contacts-open-info-at-point
    (kbd "+") #'qq-contacts-add-friend-at-point
    (kbd "a") #'qq-contacts-open-avatar-at-point
    (kbd "Y") #'qq-contacts-copy-id-at-point
    (kbd "t") #'qq-contacts-toggle-category
    (kbd "g j") #'qq-contacts-next-item
    (kbd "g k") #'qq-contacts-previous-item
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-contacts-button-backward
    (kbd "g b") #'qq-contacts-open-root)

  (appkit-evil-define-keys qq-evil--application-states 'qq-forward-mode-map
    (kbd "RET") #'qq-forward-activate
    (kbd "<return>") #'qq-forward-activate
    (kbd "g r") #'qq-forward-refresh
    (kbd "g j") #'qq-forward-next-message
    (kbd "g k") #'qq-forward-previous-message)

  (appkit-evil-define-keys qq-evil--application-states 'qq-group-mode-map
    (kbd "g r") #'qq-group-refresh
    (kbd "m") #'qq-group-open-chat
    (kbd "a") #'qq-group-open-avatar
    (kbd "s") #'qq-group-search-members
    (kbd "g n") #'qq-group-open-notices
    (kbd "o") #'qq-group-open-owner
    (kbd "Y") #'qq-group-copy-id
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-group-button-backward)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-group-notices-mode-map
    (kbd "g r") #'qq-group-notices-refresh
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-group-notices-button-backward)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-guild-channel-mode-map
    (kbd "g r") #'qq-guild-channel-refresh)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-guild-forum-mode-map
    (kbd "RET") #'qq-guild-forum-open-post
    (kbd "<return>") #'qq-guild-forum-open-post
    (kbd "g r") #'qq-guild-forum-refresh
    (kbd "g j") #'qq-guild-forum-next-post
    (kbd "g k") #'qq-guild-forum-previous-post)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-guild-forum-post-mode-map
    (kbd "RET") #'qq-guild-forum-post-open-at-point
    (kbd "<return>") #'qq-guild-forum-post-open-at-point
    (kbd "g r") #'qq-guild-forum-post-refresh
    (kbd "g j") #'appkit-discussion-next-entry
    (kbd "g k") #'appkit-discussion-previous-entry)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-guild-user-mode-map
    (kbd "g r") #'qq-guild-user-refresh
    (kbd "a") #'qq-guild-user-open-avatar
    (kbd "Y") #'qq-guild-user-copy-id
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-guild-user-button-backward)

  (appkit-evil-define-keys qq-evil--application-states 'qq-guilds-mode-map
    (kbd "RET") #'appkit-directory-activate
    (kbd "<return>") #'appkit-directory-activate
    (kbd "g r") #'qq-guilds-refresh
    (kbd "s") #'qq-guilds-filter
    (kbd "TAB") #'appkit-directory-tab-dwim
    (kbd "<backtab>") #'appkit-directory-previous-item
    (kbd "g j") #'appkit-directory-next-item
    (kbd "g k") #'appkit-directory-previous-item
    (kbd "g u") #'appkit-directory-next-unread)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-red-packet-mode-map
    (kbd "g r") #'qq-red-packet-refresh
    (kbd "c") #'qq-red-packet-grab
    (kbd "TAB") #'forward-button)

  (appkit-evil-define-keys qq-evil--application-states 'qq-search-mode-map
    (kbd "RET") #'qq-search-open-result
    (kbd "<return>") #'qq-search-open-result
    (kbd "g r") #'qq-search-refresh
    (kbd "g j") #'qq-search-next-result
    (kbd "g k") #'qq-search-previous-result
    (kbd "m") #'qq-search-load-more
    (kbd "s") #'qq-search-search)

  (appkit-evil-define-keys qq-evil--application-states 'qq-user-mode-map
    (kbd "RET") #'qq-user-open-photo-at-point
    (kbd "<return>") #'qq-user-open-photo-at-point
    (kbd "g r") #'qq-user-refresh
    (kbd "m") #'qq-user-open-chat
    (kbd "l") #'qq-user-like
    (kbd "+") #'qq-user-add-friend
    (kbd "a") #'qq-user-open-avatar
    (kbd "P") #'qq-user-open-photo-wall
    (kbd "Y") #'qq-user-copy-id
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-user-button-backward)

  (appkit-evil-define-keys qq-evil--application-states
      'qq-user-photo-mode-map
    (kbd "RET") #'qq-user-photo-open-at-point
    (kbd "<return>") #'qq-user-photo-open-at-point
    (kbd "g r") #'qq-user-photo-refresh
    (kbd "TAB") #'forward-button
    (kbd "<backtab>") #'qq-user-photo-button-backward))

(defun qq-evil--define-chat-keys ()
  "Install chat-wide and timeline-only modal bindings."
  (appkit-evil-define-keys qq-evil--application-states 'qq-chat-mode-map
    (kbd "g r") #'qq-chat-refresh
    (kbd "g s") #'qq-chat-inplace-search
    (kbd "g n") #'qq-chat-search-next
    (kbd "g p") #'qq-chat-search-prev
    (kbd "g >") #'qq-chat-read-all
    (kbd "g x") #'qq-chat-goto-pop-message)

  ;; The timeline mode is inactive in the composer, so these bindings never
  ;; steal typed input.  `dd' follows telega's modal message-delete convention.
  (appkit-evil-define-keys qq-evil--application-states
      'qq-chat-timeline-mode-map
    (kbd "q") #'quit-window
    (kbd "r") #'qq-chat-reply-to-message
    (kbd "d d") #'qq-chat-delete-message
    (kbd "R") #'qq-chat-forward-transient
    (kbd "m") #'qq-chat-toggle-message-selection
    (kbd "U") #'qq-chat-clear-message-selection
    (kbd "o") #'qq-chat-open-resource-at-point
    (kbd "a") #'qq-chat-open-avatar-at-point
    (kbd "i") #'qq-chat-open-user-at-point
    (kbd "K") #'qq-chat-open-peer-info
    (kbd "g q") #'qq-chat-goto-reply
    (kbd "g x") #'qq-chat-goto-pop-message
    (kbd "P") #'qq-chat-poke-sender
    (kbd "!") #'qq-chat-react-to-message
    (kbd "?") #'qq-chat-transient)
  (add-hook 'qq-chat-timeline-mode-hook
            #'appkit-evil-normalize-keymaps))

(defun qq-evil--disable-snipe ()
  "Disable Evil Snipe in a read-only QQ application buffer."
  (when (fboundp 'turn-off-evil-snipe-mode)
    (turn-off-evil-snipe-mode))
  (when (fboundp 'turn-off-evil-snipe-override-mode)
    (turn-off-evil-snipe-override-mode)))

(defun qq-evil--install-snipe-hooks ()
  "Keep read-only QQ bindings above Evil Snipe's local overrides."
  (when qq-evil-enable-integration
    (dolist (mode (delq 'qq-chat-mode
                        (copy-sequence qq-evil--application-modes)))
      (add-hook (intern (format "%s-hook" mode)) #'qq-evil--disable-snipe))))

(defun qq-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing QQ application buffers."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (memq major-mode qq-evil--application-modes)
          (when (and (featurep 'evil-snipe)
                     (not (eq major-mode 'qq-chat-mode)))
            (qq-evil--disable-snipe))
          (appkit-evil-normalize-keymaps))))))

;;;###autoload
(defun qq-evil-setup ()
  "Install emacs-qq's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) qq-evil-enable-integration)
    (qq-evil--set-initial-states)
    (qq-evil--define-readonly-keys)
    (qq-evil--define-chat-keys)
    (when (featurep 'evil-snipe)
      (qq-evil--install-snipe-hooks))
    (qq-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (qq-evil-setup))

(with-eval-after-load 'evil-snipe
  (qq-evil--install-snipe-hooks))

(provide 'qq-evil)

;;; qq-evil.el ends here
