;;; qq-customize.el --- Customization for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors
;; Keywords: comm

;;; Commentary:

;; User customization for emacs-qq.

;;; Code:

(require 'subr-x)
(require 'url-util)

(defgroup qq nil
  "QQ client for Emacs backed by NapCat."
  :group 'comm)

(defcustom qq-onebot-websocket-url "ws://127.0.0.1:3001/"
  "NapCat OneBot websocket endpoint used by emacs-qq."
  :type 'string
  :group 'qq)

(defcustom qq-onebot-token nil
  "OneBot access token for NapCat.

Use `qq-set-token' to set this for the current Emacs session.
When nil, emacs-qq falls back to `qq-onebot-token-env-var'."
  :type '(choice (const :tag "Unset" nil) string)
  :group 'qq)

(defcustom qq-onebot-token-env-var "NAPCAT_ONEBOT_TOKEN"
  "Environment variable used as OneBot token fallback."
  :type 'string
  :group 'qq)

(defcustom qq-recent-contact-count 50
  "Default amount of recent sessions requested during refresh."
  :type 'integer
  :group 'qq)

(defcustom qq-history-fetch-count 20
  "Default amount of messages fetched per history request."
  :type 'integer
  :group 'qq)

(defcustom qq-auto-mark-read t
  "When non-nil, mark a chat as read when its buffer is visible."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-show-recalled-messages nil
  "When non-nil, show recalled messages as stubs in the chat timeline.

Default nil matches telega (`telega-chat-show-deleted-messages-for' defaults
to nil): recalled rows stay in `qq-state' but are omitted from the EWOC.
When non-nil, render a \"[message recalled]\" placeholder (no action buttons).

Requires NapCat fork to mark messages with `recalled' / `recall_time'."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-show-unread-divider t
  "When non-nil, render an unread divider before the first unread message.

QQ only exposes `unread-count' (no last-read snowflake).  The first unread
row is approximated as the oldest among the last N non-self timeline
messages, where N is the session unread count."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-show-header-help nil
  "When non-nil, show the long keybinding help line in the chat EWOC header.

Default nil keeps the buffer header as title-only (telega-like).  Help remains
available via `C-c ?' / `describe-mode'."
  :type 'boolean
  :group 'qq)

(defface qq-msg-heading
  '((((class color) (background light))
     :background "gray92" :extend t)
    (((class color) (background dark))
     :background "gray20" :extend t)
    (t :inherit widget-single-line-field :extend t))
  "Face for message heading rows (avatar + sender + time).

Mirrors `telega-msg-heading'; intended for future appkit chatbuf reuse."
  :group 'qq)

(defface qq-msg-self-title
  '((t :weight bold :inherit font-lock-keyword-face))
  "Face for the current account's sender title in chat buffers."
  :group 'qq)

(defface qq-msg-user-title
  '((t :weight bold))
  "Face for other users' sender titles in chat buffers."
  :group 'qq)

(defface qq-msg-inline-reply
  '((t :inherit (qq-msg-heading shadow)))
  "Face for inline reply preview rows."
  :group 'qq)

(defface qq-msg-deleted
  '((t :inherit shadow :extend t :slant italic))
  "Face used for recalled message stubs."
  :group 'qq)

(defface qq-msg-date-separator
  '((t :inherit font-lock-doc-face :weight bold :extend t))
  "Face for day separator rows in the chat timeline."
  :group 'qq)

(defface qq-msg-unread-divider
  '((t :inherit warning :weight bold :extend t))
  "Face for the unread messages bar."
  :group 'qq)

(defface qq-msg-status
  '((t :inherit shadow))
  "Face for pending/failed/recalled status suffixes and timestamps."
  :group 'qq)

(defcustom qq-media-avatar-image-height 20
  "Pixel height used for inline avatar images in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-face-image-height 18
  "Pixel height used for inline QQ face images in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-preview-image-height 160
  "Maximum pixel height used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-preview-image-max-width 320
  "Maximum pixel width used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-download-directory
  (locate-user-emacs-file "qq-downloads/")
  "Directory used for QQ media downloads copied from NapCat resources."
  :type 'directory
  :group 'qq)

(defcustom qq-media-cache-directory
  (locate-user-emacs-file "qq-media-cache/")
  "Directory used for cached remote QQ media copies needed for inline rendering."
  :type 'directory
  :group 'qq)

(defcustom qq-self-message-dedupe-window 10
  "Seconds used to weakly dedupe self-message event echoes."
  :type 'integer
  :group 'qq)

(defcustom qq-transport-reconnect-delay 3
  "Base delay in seconds before reconnect attempts."
  :type 'number
  :group 'qq)

(defcustom qq-transport-reconnect-max-attempts nil
  "Maximum reconnect attempts before emacs-qq stops reconnecting.

Set to nil to allow unlimited reconnect attempts."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'qq)

(defun qq-set-token (token)
  "Set OneBot TOKEN for the current Emacs session."
  (interactive (list (read-passwd "NapCat OneBot token: ")))
  (setq qq-onebot-token token)
  (message "qq: token set for current session"))

(defun qq-current-token ()
  "Return the active OneBot token from customize or environment."
  (let ((custom-token (and (stringp qq-onebot-token)
                           (not (string-empty-p qq-onebot-token))
                           qq-onebot-token))
        (env-token (let ((raw (getenv qq-onebot-token-env-var)))
                     (and (stringp raw)
                          (not (string-empty-p raw))
                          raw))))
    (or custom-token env-token)))

(defun qq-build-websocket-url ()
  "Return `qq-onebot-websocket-url' with token query appended when needed."
  (let ((url qq-onebot-websocket-url)
        (token (qq-current-token)))
    (if (or (null token) (string-empty-p token))
        url
      (concat url
              (if (string-match-p "\\?" url) "&" "?")
              "access_token="
              (url-hexify-string token)))))

(provide 'qq-customize)

;;; qq-customize.el ends here
