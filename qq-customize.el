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

(defcustom qq-transport-request-timeout 30
  "Seconds before an unanswered NapCat action fails locally.

Nil disables request timeouts.  A timeout removes the request from the
transport pending table and invokes its error callback exactly once."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'qq)

(defcustom qq-chat-history-auto-load-threshold 2000
  "Character distance from buffer top that triggers an older-history page.

Set to nil to disable telega-style automatic loading near the top."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Characters"))
  :group 'qq)

(defcustom qq-chat-messages-pop-ring-size 50
  "Size of the chatbuf messages pop ring (telega `telega-chat-messages-pop-ring-size').

When jumping to a reply target (or any message), the previous message at
point is pushed so `qq-chat-goto-pop-message' (`x') can jump back."
  :type 'integer
  :group 'qq)

(defcustom qq-chat-jump-history-count nil
  "History page size when seeking a jump target (reply goto).

Nil means use `qq-history-fetch-count'.  NapCat `get_*_msg_history' with
`message_seq' set to the target snowflake loads one page from that cursor
(not iterative load-older from the buffer's oldest id)."
  :type '(choice (const :tag "Same as qq-history-fetch-count" nil)
                 (integer :tag "Count"))
  :group 'qq)

(defcustom qq-auto-mark-read t
  "When non-nil, mark a chat as read when its buffer is visible."
  :type 'boolean
  :group 'qq)

(defcustom qq-input-status-ttl 6
  "Seconds to keep a friend's chat-action (typing) after the last push.

Modeled after telega's ephemeral chat actions.  NapCat's
`notify/input_status' has no guaranteed stop packet, so the client
auto-clears unless a newer push refreshes it."
  :type 'integer
  :group 'qq)

(defcustom qq-chat-show-peer-actions t
  "When non-nil, show peer chat-actions (e.g. 正在输入) in chat footer and root.

Telega equivalent: footer `telega-chatbuf-footer-ins-prompt-delim' with
actions + root `telega-ins--chat-status' preferring actions."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-send-typing t
  "When non-nil, emit `set_input_status' while composing private-chat input.

Telega equivalent: `telega-chatbuf--set-action' \"Typing\" / \"Cancel\" driven
by input non-emptiness after commands."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-action-prefix ".. "
  "Prefix shown before peer action text in footer/root (telega typing symbol)."
  :type 'string
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

NapCat uses Linux QQ's `getABatchOfContactMsgBoxInfo' and resolves its first
unread msgSeq to an exact NT snowflake.  The unread-count approximation is
kept only as compatibility fallback for older NapCat builds."
  :type 'boolean
  :group 'qq)

(defcustom qq-chat-show-header-help nil
  "When non-nil, show the long keybinding help line in the chat EWOC header.

Default nil keeps the buffer header as title-only (telega-like).  Help remains
available via `C-c ?' / `describe-mode'."
  :type 'boolean
  :group 'qq)

;; Faces mirror telega-msg-* defaults (not a visual clone of every palette
;; trick).  Keep them ordinary so theming stays familiar.

(defface qq-msg-heading
  '((((class color) (background light))
     :background "gray90" :extend t)
    (((class color) (background dark))
     :background "gray20" :extend t)
    (t :inherit widget-single-line-field :extend t))
  "Face for message heading rows (avatar + sender + time).

Same defaults as `telega-msg-heading'."
  :group 'qq)

(defface qq-msg-self-title
  '((t :bold t))
  "Face for the current account's sender title.

Same defaults as `telega-msg-self-title'."
  :group 'qq)

(defface qq-msg-user-title
  '((t nil))
  "Face for other users' sender titles.

Same defaults as `telega-msg-user-title'."
  :group 'qq)

(defface qq-msg-inline-reply
  '((t :inherit (qq-msg-heading shadow)))
  "Face for inline reply preview rows.

Same idea as `telega-msg-inline-reply'."
  :group 'qq)

(defface qq-msg-poke
  '((t :inherit shadow))
  "Face for QQ gray-tip poke decorations.

Pokes use telega-style centered special-message chrome instead of the ordinary
message heading/body layout."
  :group 'qq)

(defface qq-msg-deleted
  '((t :inherit custom-invalid :extend t))
  "Face used for recalled message stubs.

Same defaults as `telega-msg-deleted'."
  :group 'qq)

(defface qq-msg-date-separator
  '((t :inherit shadow))
  "Face for day separator rows in the chat timeline."
  :group 'qq)

(defface qq-msg-unread-divider
  '((t :inherit shadow))
  "Face for the unread messages bar (telega unread bar)."
  :group 'qq)

(defface qq-msg-status
  '((t :inherit shadow))
  "Face for pending/failed/recalled status suffixes and timestamps."
  :group 'qq)

(defface qq-msg-reaction
  '((t :inherit button))
  "Face for a message reaction not selected by the current account."
  :group 'qq)

(defface qq-msg-reaction-chosen
  '((t :inherit (highlight bold)))
  "Face for a message reaction selected by the current account."
  :group 'qq)

(defcustom qq-media-avatar-image-height 20
  "Pixel height used for inline avatar images in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-face-image-height 18
  "Pixel height used for inline QQ face images in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-poke-image-height 28
  "Maximum pixel height used for decorative poke action images."
  :type 'integer
  :group 'qq)

(defcustom qq-media-default-emoji-directory
  "/opt/QQ/resources/app/resource/default-emojis"
  "Directory of LinuxQQ built-in base face PNGs (`<id>.png').

Used as the primary source for inline QQ faces so chat rendering does not
depend on NapCat `get_base_emoji' succeeding.  Falls back to the API when
a file is missing (newer / animated faces)."
  :type 'directory
  :group 'qq)

(defcustom qq-media-custom-face-count 1000
  "How many favorite custom faces to request from NapCat per fetch.

NapCat/`fetch_custom_face_info' only returns up to this many items
(`count' parameter).  The previous default of 96 truncated large
favorites libraries.  When the response is full (length = count),
`qq-media-refresh-custom-faces' automatically retries with a larger
count up to `qq-media-custom-face-count-max'."
  :type 'integer
  :group 'qq)

(defcustom qq-media-custom-face-count-max 5000
  "Upper bound when auto-expanding favorite-face fetches.

If a fetch returns exactly as many faces as requested, the client
retries with a larger `count' until it gets a short page or hits
this max."
  :type 'integer
  :group 'qq)

(defcustom qq-media-face-names-file
  (let* ((lib (or (locate-library "qq-customize.el")
                  (locate-library "qq-media.el")
                  load-file-name
                  buffer-file-name))
         ;; straight build often symlinks *.el into build/; resolve to the
         ;; real package dir so non-el data files (json) are found.
         (here (and lib (file-name-directory (file-truename lib)))))
    (expand-file-name "qq-face-names.json" (or here default-directory)))
  "JSON map of QQ face id → display name (e.g. \"178\" → \"/斜眼笑\").

Bundled with emacs-qq from NapCat `face_config.json'.  Used for plain-text
fallbacks and previews when the face image is not yet available."
  :type 'file
  :group 'qq)

(defcustom qq-media-preview-image-height 160
  "Maximum pixel height used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-preview-image-max-width 320
  "Maximum pixel width used for inline media previews in chat buffers."
  :type 'integer
  :group 'qq)

(defcustom qq-media-open-file-function #'find-file
  "Function used to open downloaded files and images.

The default keeps media inside Emacs, matching telega's
`telega-open-file-function'.  The function receives one local filename."
  :type 'function
  :group 'qq)

(defcustom qq-media-animate-gifs t
  "When non-nil, start GIF animation after opening it in `image-mode'."
  :type 'boolean
  :group 'qq)

(defcustom qq-media-video-player-command
  (and (executable-find "mpv") "mpv")
  "Command used to play QQ video files and URLs.

The default is mpv when available.  Unlike the old media path, an unset or
unrunnable command reports an error instead of opening a web browser."
  :type '(choice
          (const :tag "No video player" nil)
          (string :tag "Command line"))
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
