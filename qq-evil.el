;;; qq-evil.el --- Native Evil bindings for emacs-qq -*- lexical-binding: t; -*-

;;; Commentary:

;; emacs-qq keeps its ordinary maps as the Emacs-state interface.  This
;; optional integration keeps native Evil motions and defines only deliberate
;; application actions.  It does not depend on evil-collection.

;;; Code:

(require 'appkit-evil)
(require 'qq-customize)

(declare-function appkit-directory-activate "appkit-directory" ())
(declare-function appkit-directory-next-item "appkit-directory" ())
(declare-function appkit-directory-next-unread "appkit-directory" ())
(declare-function appkit-directory-previous-item "appkit-directory" ())
(declare-function appkit-directory-tab-dwim "appkit-directory" ())
(declare-function appkit-discussion-next-entry "appkit-discussion" ())
(declare-function appkit-discussion-previous-entry "appkit-discussion" ())
(declare-function qq-chat-clear-message-selection "qq-chat" (&optional quiet))
(declare-function qq-chat-delete-message "qq-chat" ())
(declare-function qq-chat-recall-message "qq-chat" ())
(declare-function qq-chat-forward-transient "qq-transient" (plan))
(declare-function qq-chat-goto-pop-message "qq-chat" ())
(declare-function qq-chat-goto-reply "qq-chat" (&optional message))
(declare-function qq-chat-inplace-search "qq-chat" (search-command))
(declare-function qq-chat-open-avatar-at-point "qq-chat" ())
(declare-function qq-chat-open-peer-info "qq-chat" ())
(declare-function qq-chat-open-resource-at-point "qq-chat" ())
(declare-function qq-chat-open-user-at-point "qq-chat" ())
(declare-function qq-chat-poke-sender "qq-chat" ())
(declare-function qq-chat-react-to-message "qq-chat" (&optional face-id message))
(declare-function qq-chat-read-all "qq-chat" ())
(declare-function qq-chat-refresh "qq-chat" ())
(declare-function qq-chat-reply-to-message "qq-chat" ())
(declare-function qq-chat-search-next "qq-chat" ())
(declare-function qq-chat-search-prev "qq-chat" ())
(declare-function qq-chat-toggle-message-selection "qq-chat" ())
(declare-function qq-chat-transient "qq-transient" ())
(declare-function qq-contacts-add-friend-at-point "qq-contacts" ())
(declare-function qq-contacts-button-backward "qq-contacts" ())
(declare-function qq-contacts-copy-id-at-point "qq-contacts" ())
(declare-function qq-contacts-next-item "qq-contacts" ())
(declare-function qq-contacts-open "qq-contacts" ())
(declare-function qq-contacts-open-at-point "qq-contacts" ())
(declare-function qq-contacts-open-avatar-at-point "qq-contacts" ())
(declare-function qq-contacts-open-info-at-point "qq-contacts" ())
(declare-function qq-contacts-open-root "qq-contacts" ())
(declare-function qq-contacts-previous-item "qq-contacts" ())
(declare-function qq-contacts-refresh "qq-contacts" ())
(declare-function qq-contacts-search "qq-contacts" (query &optional scope))
(declare-function qq-contacts-show-friends "qq-contacts" ())
(declare-function qq-contacts-show-groups "qq-contacts" ())
(declare-function qq-contacts-show-not-recent-groups "qq-contacts" ())
(declare-function qq-contacts-toggle-category "qq-contacts" (&optional category-id))
(declare-function qq-forward-next-message "qq-forward" (&optional count))
(declare-function qq-forward-previous-message "qq-forward" (&optional count))
(declare-function qq-forward-refresh "qq-forward" ())
(declare-function qq-group-button-backward "qq-group" ())
(declare-function qq-group-copy-id "qq-group" ())
(declare-function qq-group-notices-button-backward "qq-group-notices" ())
(declare-function qq-group-notices-refresh "qq-group-notices" ())
(declare-function qq-group-open-avatar "qq-group" ())
(declare-function qq-group-open-chat "qq-group" ())
(declare-function qq-group-open-notices "qq-group" ())
(declare-function qq-group-open-owner "qq-group" ())
(declare-function qq-group-refresh "qq-group" ())
(declare-function qq-group-search-members "qq-group" (&optional query))
(declare-function qq-guild-channel-refresh "qq-guild-channel" ())
(declare-function qq-guild-forum-next-post "qq-guild-forum" ())
(declare-function qq-guild-forum-open-post "qq-guild-forum" ())
(declare-function qq-guild-forum-post-open-at-point "qq-guild-forum-post" ())
(declare-function qq-guild-forum-post-refresh "qq-guild-forum-post" ())
(declare-function qq-guild-forum-previous-post "qq-guild-forum" ())
(declare-function qq-guild-forum-refresh "qq-guild-forum" ())
(declare-function qq-guild-user-button-backward "qq-guild-user" ())
(declare-function qq-guild-user-copy-id "qq-guild-user" ())
(declare-function qq-guild-user-open-avatar "qq-guild-user" ())
(declare-function qq-guild-user-refresh "qq-guild-user" ())
(declare-function qq-guilds-filter "qq-guilds" (query))
(declare-function qq-guilds-open "qq-guilds" ())
(declare-function qq-guilds-refresh "qq-guilds" ())
(declare-function qq-red-packet-grab "qq-red-packet" ())
(declare-function qq-red-packet-refresh "qq-red-packet" ())
(declare-function qq-root-button-backward "qq-root" ())
(declare-function qq-root-next-unread "qq-root" ())
(declare-function qq-root-open-at-point "qq-root" ())
(declare-function qq-root-open-avatar-at-point "qq-root" ())
(declare-function qq-root-open-info-at-point "qq-root" ())
(declare-function qq-root-open-self-user "qq-root" ())
(declare-function qq-root-open-session "qq-root" ())
(declare-function qq-root-refresh "qq-root" ())
(declare-function qq-root-search "qq-root" (&optional query))
(declare-function qq-root-tab-dwim "qq-root" ())
(declare-function qq-root-transient "qq-transient" ())
(declare-function qq-search-load-more "qq-search" ())
(declare-function qq-search-next-result "qq-search" ())
(declare-function qq-search-open-result "qq-search" ())
(declare-function qq-search-previous-result "qq-search" ())
(declare-function qq-search-refresh "qq-search" ())
(declare-function qq-search-search "qq-search" (query))
(declare-function qq-user-add-friend "qq-user" ())
(declare-function qq-user-button-backward "qq-user" ())
(declare-function qq-user-copy-id "qq-user" ())
(declare-function qq-user-like "qq-user" ())
(declare-function qq-user-open-avatar "qq-user" ())
(declare-function qq-user-open-chat "qq-user" ())
(declare-function qq-user-open-photo-at-point "qq-user" ())
(declare-function qq-user-open-photo-wall "qq-user" ())
(declare-function qq-user-photo-button-backward "qq-user-photo" ())
(declare-function qq-user-photo-open-at-point "qq-user-photo" ())
(declare-function qq-user-photo-refresh "qq-user-photo" ())
(declare-function qq-user-refresh "qq-user" ())

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

(defun qq-evil--set-initial-states ()
  "Register `qq-evil-initial-state' for all QQ application modes."
  (appkit-evil-set-initial-states
   qq-evil--application-modes qq-evil-initial-state))

(defun qq-evil--define-readonly-keys ()
  "Install shared and surface-specific read-only bindings."
  (dolist (map qq-evil--readonly-maps)
    (appkit-evil-define-readonly-keys map))

  (appkit-evil-map
    (:map qq-root-mode-map
     :nm
     "RET" #'qq-root-open-at-point
     "<return>" #'qq-root-open-at-point
     "g r" #'qq-root-refresh
     "s" #'qq-root-search
     "c" #'qq-contacts-open
     "g G" #'qq-guilds-open
     "o" #'qq-root-open-session
     "a" #'qq-root-open-avatar-at-point
     "i" #'qq-root-open-info-at-point
     "I" #'qq-root-open-self-user
     "TAB" #'qq-root-tab-dwim
     "<backtab>" #'qq-root-button-backward
     "?" #'qq-root-transient)
    (:map qq-contacts-mode-map
     :nm
     "RET" #'qq-contacts-open-at-point
     "<return>" #'qq-contacts-open-at-point
     "g r" #'qq-contacts-refresh
     "s" #'qq-contacts-search
     "f" #'qq-contacts-show-friends
     "g G" #'qq-contacts-show-groups
     "g I" #'qq-contacts-show-not-recent-groups
     "m" #'qq-contacts-open-at-point
     "i" #'qq-contacts-open-info-at-point
     "+" #'qq-contacts-add-friend-at-point
     "a" #'qq-contacts-open-avatar-at-point
     "Y" #'qq-contacts-copy-id-at-point
     "t" #'qq-contacts-toggle-category
     "TAB" #'forward-button
     "<backtab>" #'qq-contacts-button-backward
     "g b" #'qq-contacts-open-root)
    (:map qq-forward-mode-map
     :nm
     "g r" #'qq-forward-refresh)
    (:map qq-group-mode-map
     :nm
     "g r" #'qq-group-refresh
     "m" #'qq-group-open-chat
     "a" #'qq-group-open-avatar
     "s" #'qq-group-search-members
     "g n" #'qq-group-open-notices
     "o" #'qq-group-open-owner
     "Y" #'qq-group-copy-id
     "TAB" #'forward-button
     "<backtab>" #'qq-group-button-backward)
    (:map qq-group-notices-mode-map
     :nm
     "g r" #'qq-group-notices-refresh
     "TAB" #'forward-button
     "<backtab>" #'qq-group-notices-button-backward)
    (:map qq-guild-channel-mode-map
     :nm
     "g r" #'qq-guild-channel-refresh)
    (:map qq-guild-forum-mode-map
     :nm
     "RET" #'qq-guild-forum-open-post
     "<return>" #'qq-guild-forum-open-post
     "g r" #'qq-guild-forum-refresh)
    (:map qq-guild-forum-post-mode-map
     :nm
     "RET" #'qq-guild-forum-post-open-at-point
     "<return>" #'qq-guild-forum-post-open-at-point
     "g r" #'qq-guild-forum-post-refresh)
    (:map qq-guild-user-mode-map
     :nm
     "g r" #'qq-guild-user-refresh
     "a" #'qq-guild-user-open-avatar
     "Y" #'qq-guild-user-copy-id
     "TAB" #'forward-button
     "<backtab>" #'qq-guild-user-button-backward)
    (:map qq-guilds-mode-map
     :nm
     "RET" #'appkit-directory-activate
     "<return>" #'appkit-directory-activate
     "g r" #'qq-guilds-refresh
     "s" #'qq-guilds-filter
     "TAB" #'appkit-directory-tab-dwim
     "<backtab>" #'appkit-directory-previous-item)
    (:map qq-red-packet-mode-map
     :nm
     "g r" #'qq-red-packet-refresh
     "c" #'qq-red-packet-grab
     "TAB" #'forward-button)
    (:map qq-search-mode-map
     :nm
     "RET" #'qq-search-open-result
     "<return>" #'qq-search-open-result
     "g r" #'qq-search-refresh
     "m" #'qq-search-load-more
     "s" #'qq-search-search)
    (:map qq-user-mode-map
     :nm
     "RET" #'qq-user-open-photo-at-point
     "<return>" #'qq-user-open-photo-at-point
     "g r" #'qq-user-refresh
     "m" #'qq-user-open-chat
     "+" #'qq-user-add-friend
     "a" #'qq-user-open-avatar
     "P" #'qq-user-open-photo-wall
     "Y" #'qq-user-copy-id
     "TAB" #'forward-button
     "<backtab>" #'qq-user-button-backward)
    (:map qq-user-photo-mode-map
     :nm
     "RET" #'qq-user-photo-open-at-point
     "<return>" #'qq-user-photo-open-at-point
     "g r" #'qq-user-photo-refresh
     "TAB" #'forward-button
     "<backtab>" #'qq-user-photo-button-backward)))

(defun qq-evil--define-chat-keys ()
  "Install chat-wide and timeline-only modal bindings."
  ;; Follow Telega's message vocabulary on generated timeline content.  The
  ;; timeline map is inactive in the composer, so these keys never steal input.
  (appkit-evil-map
    (:map qq-chat-mode-map
     :nm
     "g r" #'qq-chat-refresh
     "g s" #'qq-chat-inplace-search
     "g n" #'qq-chat-search-next
     "g p" #'qq-chat-search-prev
     "g >" #'qq-chat-read-all
     "g x" #'qq-chat-goto-pop-message)
    (:map qq-chat-timeline-mode-map
     :nm
     "q" #'quit-window
     "r" #'qq-chat-reply-to-message
     "R" #'qq-chat-forward-transient
     "m" #'qq-chat-toggle-message-selection
     "U" #'qq-chat-clear-message-selection
     "a" #'qq-chat-open-avatar-at-point
     "i" #'qq-chat-open-user-at-point
     "K" #'qq-chat-open-peer-info
     "g q" #'qq-chat-goto-reply
     "g x" #'qq-chat-goto-pop-message
     "P" #'qq-chat-poke-sender
     "!" #'qq-chat-react-to-message
     "?" #'qq-chat-transient)))

(defun qq-evil--refresh-live-buffers ()
  "Refresh Evil projections in existing QQ application buffers."
  (appkit-evil-normalize-buffers qq-evil--application-modes))

;;;###autoload
(defun qq-evil-setup ()
  "Install emacs-qq's native Evil integration.
Safe to call multiple times."
  (interactive)
  (when (and (featurep 'evil) qq-evil-enable-integration)
    (qq-evil--set-initial-states)
    (qq-evil--define-readonly-keys)
    (qq-evil--define-chat-keys)
    (qq-evil--refresh-live-buffers)))

(with-eval-after-load 'evil
  (qq-evil-setup))

(provide 'qq-evil)

;;; qq-evil.el ends here
