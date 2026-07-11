;;; qq-chat.el --- Chat buffers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Chat buffer modeled after disco.el's room view, while borrowing
;; telega.el-style naming and the most familiar chat input bindings.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'button)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'disco-chat-avatar)
(require 'appkit-chatbuf)
(require 'appkit-chat-timeline)
(require 'disco-ins)
(require 'disco-media)
(require 'disco-ui)
(require 'disco-view)
(require 'qq-api)
(require 'qq-customize)
(require 'qq-media)
(require 'qq-protocol)
(require 'qq-runtime)
(require 'qq-state)

;; `qq-forward' requires this module to reuse the message-body renderer, so
;; keep the reverse dependency lazy and avoid a load cycle.
(autoload 'qq-forward-segment-p "qq-forward")
(autoload 'qq-forward-insert-segment "qq-forward")
(autoload 'qq-forward-event-segment-to-internal "qq-forward")
(autoload 'qq-user-open "qq-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)

(declare-function qq-forward-segment-p "qq-forward" (segment))
(declare-function qq-forward-insert-segment
                  "qq-forward" (segment prefix-state properties))
(declare-function qq-forward-event-segment-to-internal
                  "qq-forward" (segment session-key))
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-api-forward-message
                  "qq-api" (message-id source-session-key target-session-key
                                        callback &optional errback))
(declare-function qq-api-send-forward-bundle
                  "qq-api" (source-session-key target-session-key message-ids
                                                callback &optional errback))
(declare-function qq-api-cancel-request "qq-api" (request-token))
(declare-function qq-api-send-poke
                  "qq-api" (session-key &optional target-id callback errback))
(declare-function qq-api-recall-poke
                  "qq-api" (message-id &optional callback errback))

(defvar-local qq-chat--session-key nil
  "Session key associated with the current chat buffer.")

(defvar-local qq-chat--my-action nil
  "Local outgoing chat-action for this buffer (telega `telega-chatbuf--my-action').

Non-nil means we currently advertise typing via `set_input_status'.")

(defvar-local qq-chat--last-search-query nil
  "Last in-chat search query.")

(defvar qq-chat-group-messages t
  "When non-nil, compact consecutive messages from the same sender.")

(defvar qq-chat-group-messages-timespan 300
  "Maximum time gap in seconds used for compact message grouping.")

(defvar-local qq-chat--fill-column nil
  "Cached telega-style timeline width for the active chat window.")

(defconst qq-chat--empty-placeholder :qq-chat-empty-placeholder
  "Sentinel EWOC payload used for the empty timeline note.")

(defconst qq-chat--history-gap-prefix "history-gap:"
  "Anchor prefix used by synthetic newer-history gap rows.")

(defun qq-chat--history-gap-message-p (message)
  "Return non-nil when MESSAGE is a synthetic history gap row."
  (and (listp message) (alist-get 'history-gap message)))

(defun qq-chat--history-gap-message (after-message-id)
  "Return synthetic gap row following AFTER-MESSAGE-ID."
  `((id . ,(concat qq-chat--history-gap-prefix (format "%s" after-message-id)))
    (history-gap . t)
    (gap-after-id . ,after-message-id)
    (self-p . t)))

(defvar-local qq-chat--history-loading nil
  "Non-nil while an older-history request is in flight for this chat.")

(defvar-local qq-chat--history-exhausted nil
  "Non-nil when older history is known to be exhausted for this chat.")

(defvar-local qq-chat--history-state nil
  "Telega-style history state plist for the current chat.

Keys are `:loading' (`older' or `newer'), `:older-loaded', `:newer-loaded',
`:newer-freezed', and `:gap-after-id'.")

(defvar-local qq-chat--initial-history-owner nil
  "Opaque owner token for the active initial-history request chain.")

(defvar-local qq-chat--initial-history-request nil
  "Transport request token currently owned by initial-history loading.")

(defun qq-chat--reset-history-state ()
  "Reset current buffer history paging state."
  (setq qq-chat--history-state
        (list :loading nil
              :older-loaded nil
              :newer-loaded t
              :newer-freezed nil
              :gap-after-id nil))
  (setq qq-chat--history-loading nil)
  (setq qq-chat--history-exhausted nil))

(defun qq-chat--history-get (property)
  "Return PROPERTY from current `qq-chat--history-state'."
  (plist-get qq-chat--history-state property))

(defun qq-chat--history-set (property value)
  "Set PROPERTY to VALUE in current `qq-chat--history-state'."
  (setq qq-chat--history-state
        (plist-put qq-chat--history-state property value))
  (when (eq property :loading)
    (setq qq-chat--history-loading (and value t)))
  (when (eq property :older-loaded)
    (setq qq-chat--history-exhausted (and value t)))
  value)

(defun qq-chat--set-history-gap (after-message-id)
  "Place a newer-history gap after AFTER-MESSAGE-ID, or clear it when nil."
  (qq-chat--history-set :gap-after-id after-message-id)
  (qq-chat--history-set :newer-freezed (and after-message-id t))
  (qq-chat--history-set :newer-loaded (not after-message-id)))

(defvar-local qq-chat--pending-jump-id nil
  "Server message id to jump to after history loads, or nil.

Mirrors telega's async `telega-chatbuf--goto-msg' when the target is not yet
loaded in the chatbuf.")

(defvar-local qq-chat--messages-pop-ring nil
  "Ring of message anchors for jump-back (telega `telega-chatbuf--messages-pop-ring').")

(defvar-local qq-chat--marked-message-anchors nil
  "Message anchors selected for one merged-forward operation.")

(defvar-local qq-chat--forward-request-active-p nil
  "Non-nil while one merged-forward request from this chat is in flight.")

(defvar qq-chat-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    ;; Message actions at point (telega-style single keys; never steal input).
    (define-key map (kbd "r") #'qq-chat-reply-to-message)
    (define-key map (kbd "d") #'qq-chat-delete-message)
    (define-key map (kbd "f") #'qq-chat-forward-message)
    (define-key map (kbd "M") #'qq-chat-toggle-forward-mark)
    (define-key map (kbd "F") #'qq-chat-forward-marked-messages)
    (define-key map (kbd "o") #'qq-chat-open-resource-at-point)
    (define-key map (kbd "a") #'qq-chat-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-chat-open-user-at-point)
    (define-key map (kbd "h") #'qq-chat-open-peer-info)
    (define-key map (kbd "g") #'qq-chat-goto-reply)
    (define-key map (kbd "x") #'qq-chat-goto-pop-message)
    (define-key map (kbd "P") #'qq-chat-send-poke)
    (define-key map (kbd "!") #'qq-chat-react-to-message)
    (define-key map (kbd "m") #'qq-chat-message-transient)
    (define-key map (kbd "?") #'qq-chat-transient)
    map)
  "Timeline-only keymap active when point is outside the draft region.

Single-key message actions (`r' reply, `d' recall, `!' react, `f' forward,
`M' mark, `F' forward marked, `o' open media, `a' avatar, `i' user,
`g' goto replied-to, `x' pop jump) and menus (`m'/`?') apply on the timeline.
They are inactive in the composer so typing is never stolen.")

(define-minor-mode qq-chat-timeline-mode
  "Buffer-local navigation bindings active outside the draft region."
  :init-value nil
  :lighter nil
  :keymap qq-chat-timeline-mode-map)

(defun qq-chat--session ()
  "Return current chat session object."
  (and qq-chat--session-key
       (qq-state-session qq-chat--session-key)))

(defun qq-chat--buffer-name (session-key)
  "Return canonical buffer name for SESSION-KEY."
  (format "*qq-chat:%s*"
          (or (alist-get 'title (qq-state-session session-key))
              session-key)))

(defun qq-chat--ensure-buffer-name ()
  "Rename current chat buffer to reflect latest session title."
  (when qq-chat--session-key
    (rename-buffer (qq-chat--buffer-name qq-chat--session-key) t)))

(defun qq-chat--buffer-width ()
  "Return current chat rendering width in columns."
  (max 72 (window-body-width (get-buffer-window (current-buffer) t))))

(defun qq-chat--header-line ()
  "Return dynamic header line for the active chat buffer.

Telega-like: title first, connection only when not connected, unread as a
compact badge.  Peer typing/actions live in the footer prompt delimiter
(see `qq-chat--input-footer-context-text'), not here.  Debug fields stay out
of the default chrome."
  (let* ((session (qq-chat--session))
         (title (or (alist-get 'title session) qq-chat--session-key))
         (status (qq-state-connection-status))
         (unread (or (alist-get 'unread-count session) 0))
         (status-part (if (eq status 'connected)
                          ""
                        (format "  [%s]" status)))
         (unread-part (if (> unread 0)
                          (format "  · %d unread" unread)
                        ""))
         (marked-count (length (qq-chat-marked-messages)))
         (marked-part (if (> marked-count 0)
                          (format "  · %d marked" marked-count)
                        "")))
    (format " %s%s%s%s" title status-part unread-part marked-part)))

(defun qq-chat--insert-read-only (text &optional face properties)
  "Insert read-only TEXT with optional FACE and PROPERTIES."
  (let ((start (point)))
    (insert text)
    (add-text-properties
     start (point)
     (append
      '(read-only t
        front-sticky t
        rear-nonsticky (read-only))
      properties
      (when face
        (list 'face face))))))

(defun qq-chat--format-time (timestamp)
  "Return display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%m-%d %H:%M:%S" (seconds-to-time timestamp))
    ""))

(defun qq-chat--format-time-short (timestamp)
  "Return compact display string for TIMESTAMP."
  (if (and timestamp (> timestamp 0))
      (format-time-string "%H:%M" (seconds-to-time timestamp))
    ""))

(defun qq-chat--render-window ()
  "Return the best live window currently displaying this chat buffer."
  (or (and (eq (window-buffer (selected-window)) (current-buffer))
           (selected-window))
      (let ((best nil)
            (best-width -1))
        (dolist (win (get-buffer-window-list (current-buffer) nil t) best)
          (let ((width (if (window-live-p win)
                           (window-width win 'remap)
                         -1)))
            (when (> width best-width)
              (setq best win
                    best-width width)))))))

(defun qq-chat--compute-fill-column (&optional window)
  "Compute telega-style timeline width for WINDOW."
  (disco-view-window-fill-column
   (or window (qq-chat--render-window))
   qq-chat-auto-fill-margin-columns))

(defun qq-chat--update-fill-column (&optional window)
  "Refresh and return the cached timeline width for WINDOW."
  (when-let* ((width (qq-chat--compute-fill-column window)))
    (setq-local qq-chat--fill-column width)
    width))

(defun qq-chat--line-fill-column ()
  "Return the usable timeline width for the current chat buffer."
  (or (and (integerp qq-chat--fill-column)
           (> qq-chat--fill-column 0)
           qq-chat--fill-column)
      (qq-chat--update-fill-column)
      (and (integerp fill-column) (> fill-column 0) fill-column)
      80))

(cl-defun qq-chat--insert-right-aligned-time
    (time &optional left-prefix-width (overflow-newline-p t))
  "Insert TIME at the shared disco timeline right edge.

LEFT-PREFIX-WIDTH reserves a display-only prefix applied by the caller.
OVERFLOW-NEWLINE-P controls whether an overlong row may move TIME to a new
line; nil keeps service rows such as poke strictly one-line."
  (when (and (stringp time) (not (string-empty-p time)))
    (disco-ins-insert-right-aligned-text
     time
     (qq-chat--line-fill-column)
     :face 'qq-msg-status
     :left-prefix-width left-prefix-width
     :overflow-newline-p overflow-newline-p)))

(defun qq-chat--message-day-key (message)
  "Return local calendar day key string for MESSAGE, or nil."
  (let ((timestamp (alist-get 'time message)))
    (when (and timestamp (> timestamp 0))
      (format-time-string "%Y-%m-%d" (seconds-to-time timestamp)))))

(defun qq-chat--message-day-label (day-key)
  "Return pretty date label for DAY-KEY (YYYY-MM-DD)."
  (if (not (stringp day-key))
      "Unknown date"
    (condition-case _
        (format-time-string "%A, %Y-%m-%d" (date-to-time (concat day-key "T00:00:00")))
      (error day-key))))

(defun qq-chat--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-chat--message-sender-display-parts (message)
  "Return sender display parts for MESSAGE as (PRIMARY . SECONDARY)."
  (let* ((session-key (or (alist-get 'session-key message)
                          qq-chat--session-key))
         (session-type (or (and session-key
                                (qq-state-session-key-type session-key))
                           (pcase (alist-get 'message-type message)
                             ("group" 'group)
                             (_ 'private))))
         (raw-event (alist-get 'raw-event message))
         (raw-sender (and (listp raw-event) (alist-get 'sender raw-event)))
         (sender-id (qq-chat--present-string (alist-get 'sender-id message)))
         (card (or (qq-chat--present-string (alist-get 'sender-card message))
                   (and (listp raw-sender)
                        (qq-chat--present-string (alist-get 'card raw-sender)))))
         (nickname (or (qq-chat--present-string (alist-get 'sender-nickname message))
                       (and (listp raw-sender)
                            (qq-chat--present-string (alist-get 'nickname raw-sender)))))
         (friend (and (eq session-type 'private)
                      sender-id
                      (qq-state-friend sender-id)))
         (remark (or (qq-chat--present-string (alist-get 'sender-remark message))
                     (and (listp friend)
                          (qq-chat--present-string (alist-get 'remark friend)))))
         (primary (if (eq session-type 'group)
                      (or card
                          nickname
                          (qq-chat--present-string (alist-get 'sender-name message))
                          sender-id
                          "unknown")
                    (or remark
                        nickname
                        (qq-chat--present-string (alist-get 'sender-name message))
                        sender-id
                        "unknown")))
         (secondary (or (if (eq session-type 'group)
                            (and card nickname
                                 (not (equal primary nickname))
                                 nickname)
                          (and remark nickname
                               (not (equal primary nickname))
                               nickname))
                        (qq-chat--present-string
                         (alist-get 'sender-secondary-name message)))))
    (cons primary secondary)))

(defun qq-chat--message-sender-name (message)
  "Return primary sender display name for MESSAGE."
  (car (qq-chat--message-sender-display-parts message)))

(defun qq-chat--message-sender-label (message)
  "Return best available sender label for MESSAGE."
  (let* ((parts (qq-chat--message-sender-display-parts message))
         (primary (car parts))
         (secondary (cdr parts)))
    (if secondary
        (format "%s • %s" primary secondary)
      primary)))

(defun qq-chat--insert-message-sender (message face)
  "Insert sender label for MESSAGE using FACE.

The primary name is shown first, with a telega-like secondary name trail when
available."
  (let* ((parts (qq-chat--message-sender-display-parts message))
         (primary (car parts))
         (secondary (cdr parts))
         (sender-id (alist-get 'sender-id message))
         (action (lambda ()
                   (if (and (qq-api-user-id-p sender-id)
                            (not (equal sender-id "0")))
                       (qq-user-open sender-id)
                     (user-error "qq: sender has no user profile"))))
         (help-echo "Open sender profile")
         (properties '(read-only t front-sticky t rear-nonsticky (read-only))))
    (disco-ui-insert-action-button
     primary
     action
     :face face
     :help-echo help-echo
     :properties properties)
    (when secondary
      (let ((separator-start (point)))
        (insert " • ")
        (add-text-properties separator-start (point)
                             (append properties '(face shadow))))
      (disco-ui-insert-action-button
       secondary
       action
       :face face
       :help-echo help-echo
       :properties properties))))

(defun qq-chat--same-sender-p (left right)
  "Return non-nil when LEFT and RIGHT messages share sender identity."
  (let ((left-id (alist-get 'sender-id left))
        (right-id (alist-get 'sender-id right)))
    (if (and left-id right-id)
        (equal left-id right-id)
      (let ((left-name (qq-chat--message-sender-name left))
            (right-name (qq-chat--message-sender-name right)))
        (and left-name right-name
             (equal left-name right-name))))))

(defun qq-chat--canonical-forward-segment (segment)
  "Translate SEGMENT at the root-event boundary, or return nil."
  (let ((type (and (listp segment) (alist-get 'type segment)))
        (data (and (listp segment) (alist-get 'data segment))))
    (and (or (equal type "forward")
             (and (equal type "card")
                  (equal (alist-get 'kind data) "forward")))
         (qq-forward-event-segment-to-internal
          segment qq-chat--session-key))))

(defun qq-chat--forward-segment-p (segment)
  "Return non-nil when SEGMENT translates to a forward record."
  (and (qq-chat--canonical-forward-segment segment) t))

(defun qq-chat--message-has-block-segments-p (message)
  "Return non-nil when MESSAGE contains block-like render segments."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (or (qq-chat--forward-segment-p segment)
                  (qq-chat--poke-segment-p segment)
                  (qq-chat--gray-tip-segment-p segment)
                  (qq-chat--mail-segment-p segment)
                  (qq-chat--card-segment-p segment)
                  (qq-chat--media-segment-p segment))
          (setq found t))
        (setq segments (cdr segments))))
    found))

(defun qq-chat--messages-compact-group-p (previous current)
  "Return non-nil when CURRENT should be compact-grouped under PREVIOUS."
  (and qq-chat-group-messages
       (listp previous)
       (listp current)
       (not (qq-chat--message-has-block-segments-p current))
       (qq-chat--same-sender-p previous current)
       (let ((previous-time (or (alist-get 'time previous) 0))
             (current-time (or (alist-get 'time current) 0)))
         (and (> previous-time 0)
              (> current-time 0)
              (<= (abs (- current-time previous-time))
                  (max 0 qq-chat-group-messages-timespan))))))

(defun qq-chat--status-suffix (message)
  "Return display suffix representing MESSAGE state."
  (pcase (alist-get 'status message)
    ('pending
     (propertize "  …" 'face 'qq-msg-status 'help-echo "pending"))
    ('failed
     (propertize (format "  !%s"
                         (or (alist-get 'error message) "failed"))
                 'face 'error
                 'help-echo (or (alist-get 'error message) "send failed")))
    ('recalled
     (propertize "  ↩" 'face 'qq-msg-status 'help-echo "recalled"))
    (_ "")))

(defun qq-chat--message-body (message)
  "Return body text for MESSAGE."
  (if (qq-state-message-recalled-p message)
      "[message recalled]"
    (or (qq-state-message-preview message) "")))

(defun qq-chat--message-anchor (message)
  "Return stable anchor value for MESSAGE.

Prefer the NapCat NT snowflake `server-id' (string) once known.  Pending
optimistic rows still use `local-id' until send succeeds and the node is
rekeyed (see `qq-chat--rekey-message-node-if-needed')."
  (or (alist-get 'server-id message)
      (alist-get 'local-id message)
      (alist-get 'id message)))

(defun qq-chat--message-reply-id (message)
  "Return reply target message id extracted from MESSAGE segments, or nil."
  (let ((segments (alist-get 'segments message))
        reply-id)
    (while (and segments (not reply-id))
      (let* ((segment (car segments))
             (type (alist-get 'type segment))
             (data (alist-get 'data segment)))
        (when (equal type "reply")
          (setq reply-id (or (alist-get 'id data)
                             (alist-get 'message_id data))))
        (setq segments (cdr segments))))
    (and reply-id (format "%s" reply-id))))

(defun qq-chat--message-line-properties (message anchor)
  "Return shared line properties for MESSAGE using ANCHOR."
  (list 'qq-chat-message-anchor anchor
        'qq-chat-message-id (alist-get 'server-id message)
        'qq-chat-message-local-id (alist-get 'local-id message)
        'qq-chat-session-key qq-chat--session-key
        'qq-chat-forward-marked (and (qq-chat--message-marked-p message) t)
        'read-only t
        'front-sticky '(read-only)
        'rear-nonsticky '(read-only)))

(defun qq-chat--message-at-point ()
  "Return message object under point in the current chat buffer, or nil."
  (let* ((anchor (or (get-text-property (point) 'qq-chat-message-anchor)
                     (get-text-property (line-beginning-position) 'qq-chat-message-anchor)))
         (messages (qq-state-session-messages qq-chat--session-key)))
    (seq-find
     (lambda (message)
       (equal (qq-chat--message-anchor message) anchor))
     messages)))

(defun qq-chat--message-forwardable-p (message)
  "Return non-nil when MESSAGE can be forwarded by its server ID."
  (and (listp message)
       (qq-api-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))))

(defun qq-chat--message-marked-p (message)
  "Return non-nil when MESSAGE is selected for merged forwarding."
  (member (qq-chat--message-anchor message) qq-chat--marked-message-anchors))

(defun qq-chat-marked-messages ()
  "Return selected messages in current timeline order."
  (seq-filter
   (lambda (message)
     (and (qq-chat--message-forwardable-p message)
          (qq-chat--message-marked-p message)))
   (qq-state-session-messages qq-chat--session-key)))

(defun qq-chat--forwardable-target-sessions ()
  "Return all private/group targets known from sessions and contact caches."
  (let ((by-key (make-hash-table :test #'equal))
        targets)
    (dolist (session (qq-state-sessions))
      (when (member (format "%s"
                            (or (alist-get 'type session)
                                (qq-state-session-key-type
                                 (alist-get 'key session))))
                    '("private" "group"))
        (puthash (alist-get 'key session) session by-key)))
    (dolist (friend (qq-state-friends))
      (when-let* ((id (alist-get 'user_id friend))
                  (key (qq-state-session-key 'private id)))
        (unless (gethash key by-key)
          (puthash
           key
           `((key . ,key)
             (type . private)
             (target-id . ,(format "%s" id))
             (peer-uin . ,(format "%s" id))
             (title . ,(or (qq-chat--present-string
                            (alist-get 'remark friend))
                           (qq-chat--present-string
                            (alist-get 'nickname friend))
                           (format "%s" id))))
           by-key))))
    (dolist (group (qq-state-groups))
      (when-let* ((id (alist-get 'group_id group))
                  (key (qq-state-session-key 'group id)))
        (unless (gethash key by-key)
          (puthash
           key
           `((key . ,key)
             (type . group)
             (target-id . ,(format "%s" id))
             (title . ,(or (qq-chat--present-string
                            (alist-get 'group_name group))
                           (format "%s" id))))
           by-key))))
    (maphash (lambda (_key target) (push target targets)) by-key)
    (sort targets
          (lambda (left right)
            (string-lessp (or (alist-get 'title left) "")
                          (or (alist-get 'title right) ""))))))

(defun qq-chat--read-forward-target ()
  "Read and return one private/group target session key."
  (let* ((sessions (qq-chat--forwardable-target-sessions))
         (choices
          (mapcar
           (lambda (session)
             (let ((key (alist-get 'key session)))
               (cons (format "%s  [%s]"
                             (or (alist-get 'title session) key)
                             key)
                     key)))
           sessions)))
    (unless choices
      (user-error "qq: no private/group forwarding target available"))
    (cdr (assoc (completing-read "Forward to: " choices nil t) choices))))

(defun qq-chat-toggle-forward-mark ()
  "Toggle merged-forward selection for the message at point."
  (interactive)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (anchor (qq-chat--message-anchor message)))
    (unless (qq-chat--message-forwardable-p message)
      (user-error "qq: this message cannot be forwarded"))
    (if (member anchor qq-chat--marked-message-anchors)
        (setq qq-chat--marked-message-anchors
              (delete anchor qq-chat--marked-message-anchors))
      (setq qq-chat--marked-message-anchors
            (append qq-chat--marked-message-anchors (list anchor))))
    (qq-chat--request-row-redisplay (list anchor))
    (qq-chat--header-line-update)
    (message "qq: %d message%s marked for forwarding"
             (length qq-chat--marked-message-anchors)
             (if (= (length qq-chat--marked-message-anchors) 1) "" "s"))))

(defun qq-chat-clear-forward-marks (&optional quiet)
  "Clear merged-forward message selection.

Suppress the status message when QUIET is non-nil."
  (interactive)
  (let ((anchors (copy-sequence qq-chat--marked-message-anchors)))
    (setq qq-chat--marked-message-anchors nil)
    (when anchors
      (qq-chat--request-row-redisplay anchors))
    (qq-chat--header-line-update)
    (unless quiet
      (message "qq: forwarding selection cleared"))))

(defun qq-chat-forward-message (&optional target-session-key)
  "Forward the message at point to TARGET-SESSION-KEY.

Interactively, prompt for a cached private or group session.  This uses the
fork-native single-forward request and deliberately creates no optimistic row,
because its result is only `{kind:single}'."
  (interactive)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (message-id (alist-get 'server-id message)))
    (unless (qq-chat--message-forwardable-p message)
      (user-error "qq: this message cannot be forwarded"))
    (let ((target (or target-session-key (qq-chat--read-forward-target))))
      (qq-api-forward-message
       message-id
       qq-chat--session-key
       target
       (lambda (_response)
         (message "qq: forwarded message to %s" target))))))

(defun qq-chat-forward-marked-messages (&optional target-session-key)
  "Send marked messages as one merged forward to TARGET-SESSION-KEY."
  (interactive)
  (when qq-chat--forward-request-active-p
    (user-error "qq: a merged-forward request is already in progress"))
  (let ((messages (qq-chat-marked-messages)))
    (unless messages
      (user-error "qq: no messages marked for forwarding"))
    (let* ((target (or target-session-key (qq-chat--read-forward-target)))
           (buffer (current-buffer))
           (sent-anchors (mapcar #'qq-chat--message-anchor messages)))
      (setq qq-chat--forward-request-active-p t)
      (condition-case error-data
          (qq-api-send-forward-bundle
           qq-chat--session-key
           target
           (mapcar (lambda (message) (alist-get 'server-id message)) messages)
           (lambda (_response)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq qq-chat--forward-request-active-p nil
                       qq-chat--marked-message-anchors
                       (seq-remove
                        (lambda (anchor) (member anchor sent-anchors))
                        qq-chat--marked-message-anchors))
                 (qq-chat--request-row-redisplay
                  sent-anchors)
                 (qq-chat--header-line-update)))
             (message "qq: forwarded %d messages to %s"
                      (length messages) target))
           (lambda (response reason)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq qq-chat--forward-request-active-p nil)
                 (qq-chat--header-line-update)))
             (qq-api--default-error response reason)))
        (error
         (setq qq-chat--forward-request-active-p nil)
         (qq-chat--header-line-update)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-chat--message-positions ()
  "Return list of message start positions in the current buffer."
  (when (appkit-chat-timeline-live-p)
    (delq nil
          (mapcar #'appkit-chat-timeline-key-position
                  (appkit-chat-timeline-keys)))))

(defun qq-chat--current-message-position ()
  "Return the start position of the message at point, or nil."
  (when (appkit-chat-timeline-live-p)
    (when-let* ((anchor (appkit-chat-timeline-key-at-point)))
      (appkit-chat-timeline-key-position anchor))))

(defun qq-chat-next-message ()
  "Move point to the next rendered message block."
  (interactive)
  (let* ((positions (qq-chat--message-positions))
         (current (or (qq-chat--current-message-position) (point)))
         (next (seq-find (lambda (pos) (> pos current)) positions)))
    (if next
        (goto-char next)
      (message "qq: no later message"))))

(defun qq-chat-previous-message ()
  "Move point to the previous rendered message block."
  (interactive)
  (let* ((positions (qq-chat--message-positions))
         (current (or (qq-chat--current-message-position) (point)))
         (previous nil))
    (dolist (pos positions)
      (when (< pos current)
        (setq previous pos)))
    (if previous
        (goto-char previous)
      (message "qq: no earlier message"))))

(defun qq-chat--current-draft-string ()
  "Return canonical draft as plain text."
  (appkit-chatbuf-string-plain-text (appkit-chatbuf-input-state)))

(defun qq-chat--sync-draft-from-buffer ()
  "Sync canonical draft from the editable buffer region."
  (let ((result (appkit-chatbuf-input-state-sync)))
    (when (plist-get result :changed-p)
      (qq-chat--maybe-update-my-action-from-input))
    (plist-get result :value)))

(defun qq-chat--reply-message ()
  "Return current reply target message from shared aux state, or nil."
  (let ((state (appkit-chatbuf-aux-state)))
    (and (eq (plist-get state :aux-type) 'reply)
         (plist-get state :aux-msg))))

(defun qq-chat--set-reply-message (message)
  "Set shared reply aux state to MESSAGE, or clear it when nil."
  (if message
      (appkit-chatbuf-aux-set
       (list :aux-type 'reply
             :aux-msg message
             :message-id (alist-get 'server-id message)))
    (appkit-chatbuf-aux-reset)))

(defun qq-chat--after-change (beg end _old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (appkit-chatbuf-after-change
   beg end
   :rendering-p (appkit-chatbuf-rendering-p)
   :sync-function #'qq-chat--sync-draft-from-buffer
   :prune-broken-objects t))

(defun qq-chat--update-context-mode ()
  "Enable timeline bindings only when point is outside the draft input."
  (let ((timeline-p (not (appkit-chatbuf-point-in-input-p))))
    (unless (eq qq-chat-timeline-mode timeline-p)
      (qq-chat-timeline-mode (if timeline-p 1 -1)))))

(defun qq-chat--flush-deferred-node-redisplay ()
  "Flush any node redisplay deferred while a region was active."
  (when (appkit-chat-timeline-live-p)
    (appkit-chat-timeline-flush-deferred)))

(defun qq-chat--maybe-auto-load-older ()
  "Load an older page when point approaches the timeline top."
  (when (and qq-chat-history-auto-load-threshold
             qq-chat--session-key
             (not (appkit-chatbuf-point-in-input-p))
             (< (point) (+ (point-min)
                           (max 0 qq-chat-history-auto-load-threshold)))
             (not (qq-chat--history-get :loading))
             (not (qq-chat--history-get :older-loaded)))
    (qq-chat-load-older-messages t)))

(defun qq-chat--post-command ()
  "Keep point inside the logical draft area when editing input."
  (unless (appkit-chatbuf-rendering-p)
    (appkit-chatbuf-post-command-clamp-point)
    (qq-chat--flush-deferred-node-redisplay)
    (qq-chat--update-context-mode)
    (qq-chat--maybe-auto-load-older)))

(defun qq-chat--prompt-text ()
  "Return visible prompt text for the current chat buffer."
  ">>> ")

(defun qq-chat--composer-visible-p ()
  "Return non-nil when the current session has a writable composer."
  (qq-state-session-sendable-p qq-chat--session-key))

(defun qq-chat--ensure-composer-visible ()
  "Signal a user error unless the current session has a composer."
  (unless (qq-chat--composer-visible-p)
    (user-error "qq: this session is read-only")))

(defun qq-chat--bind-input-region-from-footer ()
  "Bind the shared persistent tail input to canonical draft state."
  (appkit-chatbuf-init-state 32)
  (appkit-chatbuf-bind-input-region
   :visible-p (qq-chat--composer-visible-p)
   :prompt (qq-chat--prompt-text)
   :input-text (appkit-chatbuf-input-state)
   :post-bind-function #'appkit-chatbuf-input-apply-text-properties))

(defun qq-chat--header-text ()
  "Build EWOC header text for the current chat state.

Title lives only in `header-line-format' (`qq-chat--header-line'); do not
repeat it here.  EWOC header is reserved for history load state."
  (let* ((text
          (with-temp-buffer
            (cond
             ((eq (qq-chat--history-get :loading) 'newer)
              (disco-view-insert-note-line "(loading newer messages…)"))
             (qq-chat--history-loading
              (disco-view-insert-note-line "(loading older messages…)"))
             (qq-chat--history-exhausted
              (disco-view-insert-note-line "(older history exhausted)")))
            (when (> (buffer-size) 0)
              (insert "\n"))
            (buffer-string))))
    (when (> (length text) 0)
      (add-text-properties
       0 (length text)
       '(read-only t
         front-sticky (read-only)
         rear-nonsticky (read-only))
       text))
    text))

(defun qq-chat--action-indicator-text ()
  "Return telega-style peer action indicator for the current chat, or nil.

Mirrors `telega-chatbuf-footer-ins-prompt-delim' + `telega-ins--actions':
actions are shown on the footer delimiter line above the composer."
  (when (and qq-chat-show-peer-actions
             qq-chat--session-key)
    (when-let* ((text (qq-state-action-text qq-chat--session-key)))
      (propertize
       (format "(%s%s)" (or qq-chat-action-prefix ".. ") text)
       'face 'shadow))))

(defun qq-chat--set-my-action (action)
  "Set outgoing chatbuf ACTION like telega `telega-chatbuf--set-action'.

ACTION is `typing', `cancel', or nil.  Only private sessions send
`set_input_status' (NapCat limitation)."
  (let* ((want (cond
                ((eq action 'typing) 'typing)
                ((memq action '(cancel nil)) nil)
                ((equal action "Typing") 'typing)
                ((equal action "Cancel") nil)
                (t nil)))
         (session (qq-chat--session))
         (type (and session (alist-get 'type session)))
         (target (and session (alist-get 'target-id session))))
    (unless (eq qq-chat--my-action want)
      (setq qq-chat--my-action want)
      (when (and qq-chat-send-typing
                 (eq type 'private)
                 target)
        (qq-api-set-input-status target (if want 1 0))))))

(defun qq-chat--maybe-update-my-action-from-input ()
  "Advertise typing when private-chat draft is non-empty (telega parity)."
  (let ((input-p
         (not (string-empty-p
               (appkit-chatbuf-string-plain-text
                (appkit-chatbuf-input-state))))))
    (cond
     ((and (not qq-chat--my-action) input-p)
      (qq-chat--set-my-action 'typing))
     ((and qq-chat--my-action (not input-p))
      (qq-chat--set-my-action 'cancel)))))

(defun qq-chat--input-footer-context-text ()
  "Return dynamic footer text shown above the composer prompt.

Order (telega-inspired):
1. peer chat-action / typing indicator
2. reply aux (keeps its own faces/button; do not blanket-propertize
   or the cancel `[×]' loses its link face)."
  (let ((action-text (qq-chat--action-indicator-text))
        (reply-context (qq-chat--reply-context-text)))
    (concat
     "\n"
     (if action-text
         (concat action-text "\n")
       "")
     (if (string-empty-p reply-context)
         ""
       reply-context))))

(defun qq-chat--footer-text (&optional _draft)
  "Build read-only EWOC footer text for the current chat state."
  (let ((text (qq-chat--input-footer-context-text)))
    (add-text-properties
     0 (length text)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))
     text)
    text))

(defun qq-chat--first-unread-anchor (messages)
  "Return the exact first-unread anchor present in MESSAGES."
  (when qq-chat-show-unread-divider
    (let* ((session (qq-chat--session))
           (count (and session (alist-get 'unread-count session)))
           (exact (and (integerp count)
                       (> count 0)
                       (alist-get 'first-unread-message-id session))))
      (and exact
           (seq-some (lambda (message)
                       (equal exact (qq-chat--message-anchor message)))
                     messages)
           exact))))

(defun qq-chat--compute-message-render-context (previous-message message
                                                                 first-unread-anchor)
  "Project render context for MESSAGE after PREVIOUS-MESSAGE."
  (let* ((day-key (qq-chat--message-day-key message))
         (previous-day-key
          (and previous-message (qq-chat--message-day-key previous-message)))
         (anchor (qq-chat--message-anchor message)))
    (list :compact
          (and previous-message
               (qq-chat--messages-compact-group-p previous-message message)
               t)
          :insert-date
          (and day-key (not (equal day-key previous-day-key)) day-key)
          :insert-unread
          (and first-unread-anchor
               anchor
               (equal anchor first-unread-anchor)
               t))))

(defun qq-chat--message-media-cache-keys (message)
  "Return media cache keys that can affect MESSAGE rendering."
  (let ((sender-id (or (alist-get 'sender-id message)
                       (alist-get 'user-id message)))
        keys)
    (when sender-id
      (push (format "avatar:%s" sender-id) keys))
    (dolist (segment (alist-get 'segments message))
      (setq keys (nconc (qq-media-segment-cache-keys segment) keys)))
    (delete-dups (delq nil keys))))

(defun qq-chat--message-dependency-keys (message)
  "Return opaque resource keys that can change rendered MESSAGE."
  (let (dependencies)
    (when-let* ((reply-id (qq-chat--message-reply-id message)))
      (push (list :message reply-id) dependencies))
    (dolist (media-key (qq-chat--message-media-cache-keys message))
      (push (list :media media-key) dependencies))
    (delete-dups dependencies)))

(defun qq-chat--message-visible-in-timeline-p (message)
  "Return non-nil when MESSAGE belongs in the projected timeline."
  (or qq-chat-show-recalled-messages
      (not (qq-state-message-recalled-p message))))

(defun qq-chat--timeline-messages (&optional messages)
  "Return visible MESSAGES with the synthetic newer-history gap inserted."
  (let* ((visible
          (seq-filter #'qq-chat--message-visible-in-timeline-p
                      (or messages
                          (and qq-chat--session-key
                               (qq-state-session-messages qq-chat--session-key))
                          '())))
         (gap-after (qq-chat--history-get :gap-after-id)))
    (if (not gap-after)
        visible
      (let (result inserted)
        (dolist (message visible)
          (push message result)
          (when (equal gap-after (qq-chat--message-anchor message))
            (push (qq-chat--history-gap-message gap-after) result)
            (setq inserted t)))
        (if inserted (nreverse result) visible)))))

(defun qq-chat--project-timeline (messages)
  "Project visible QQ MESSAGES into shared timeline rows."
  (if (null messages)
      (list (appkit-chat-timeline-row-create
             :key qq-chat--empty-placeholder
             :payload qq-chat--empty-placeholder))
    (let ((first-unread (qq-chat--first-unread-anchor messages)))
      (appkit-chat-timeline-project
       messages
       #'qq-chat--message-anchor
       :context-function
       (lambda (previous message)
         (qq-chat--compute-message-render-context
          previous message first-unread))
       :dependencies-function #'qq-chat--message-dependency-keys))))

(defun qq-chat--ensure-timeline ()
  "Ensure current QQ chat owns one shared projected timeline."
  (qq-chat--ensure-view)
  (appkit-chat-timeline-ensure
   :printer #'qq-chat--row-printer
   :anchor-property 'qq-chat-message-anchor
   :header (qq-chat--header-text)
   :footer (qq-chat--footer-text)
   :after-mutation-function #'qq-chat--update-context-mode))

(defun qq-chat--view-id ()
  "Return the opaque appkit view id for the current QQ chat."
  (unless qq-chat--session-key
    (error "qq: chat buffer has no session key"))
  (list 'chat qq-chat--session-key))

(defun qq-chat--apply-state-event (event)
  "Apply queued QQ state EVENT inside an appkit sync transaction."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-mutation (plist-get event :mutation)))
    (pcase event-type
      ('message
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--apply-message-state-change event)
         (when (and qq-auto-mark-read
                    (get-buffer-window (current-buffer) t))
           (qq-chat-read-all))))
      ('history
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--header-line-update)
         (qq-chat--update-frame)
         (qq-chat--sync-timeline)))
      ('reset
       (qq-chat-render))
      ('session
       (when (equal event-session-key qq-chat--session-key)
         (if (eq event-mutation 'read)
             (qq-chat--apply-read-state-change)
           (qq-chat--header-line-update)
           (qq-chat--update-frame))))
      ('action
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--update-frame)))
      ('sessions-refreshed
       (qq-chat--header-line-update)
       (qq-chat--update-frame))
      ((or 'friends-refreshed 'groups-refreshed)
       (qq-chat--header-line-update)
       (qq-chat--update-frame)
       (qq-chat--sync-timeline
        :force-keys (appkit-chat-timeline-keys))))))

(defun qq-chat--sync-invalidations (view invalidations)
  "Synchronize current chat from coalesced appkit INVALIDATIONS."
  (let ((events (appkit-view-pending-events-snapshot view))
        (resources (appkit-invalidations-resource-keys invalidations))
        (entries (appkit-invalidations-entry-keys invalidations)))
    (dolist (event events)
      (when (appkit-view-live-p view)
        (qq-chat--apply-state-event event)))
    (when (appkit-view-live-p view)
      (appkit-view-acknowledge-events view (length events)))
    (when (appkit-view-live-p view)
      (cond
       ((and (null events)
             (or (appkit-invalidations-structure-p invalidations)
                 (appkit-invalidations-parts invalidations)
                 (appkit-invalidations-position-p invalidations)))
        (qq-chat-render))
       ((or resources entries)
        (qq-chat--header-line-update)
        (qq-chat--update-frame)
        (qq-chat--sync-timeline
         :force-keys entries
         :changed-resources resources))))))

(defun qq-chat--ensure-view ()
  "Return the live appkit view owning the current QQ chat buffer."
  (let* ((app (qq-runtime-app))
         (id (qq-chat--view-id))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal id (appkit-view-id current)))
      (setf (appkit-view-state current) qq-chat--session-key
            (appkit-view-sync-function current)
            #'qq-chat--sync-invalidations)
      current)
     ((appkit-view-live-p current)
      (error "qq: chat buffer belongs to a different appkit view"))
     (t
      (appkit-attach-view
       :app app
       :id id
       :state qq-chat--session-key
       :mode 'qq-chat-mode
       :sync-function #'qq-chat--sync-invalidations
       :parts '(frame timeline composer))))))

(defun qq-chat--header-line-update ()
  "Update chat header line and buffer name."
  (qq-chat--ensure-buffer-name)
  (setq-local header-line-format '(:eval (qq-chat--header-line)))
  (force-mode-line-update))

(defun qq-chat--update-frame ()
  "Synchronize QQ chat header, footer, prompt, and canonical composer."
  (qq-chat--ensure-timeline)
  (appkit-chat-timeline-set-frame
   (qq-chat--header-text)
   (qq-chat--footer-text)
   :bind-input-function #'qq-chat--bind-input-region-from-footer))

(cl-defun qq-chat--sync-timeline
    (&key (messages nil messages-p) force-keys changed-resources rekeys)
  "Synchronize QQ rows through the shared projected timeline controller."
  (qq-chat--ensure-timeline)
  (appkit-chat-timeline-sync
   (qq-chat--project-timeline
    (if messages-p messages (qq-chat--timeline-messages)))
   :force-keys force-keys
   :changed-resources changed-resources
   :rekeys rekeys))

(defun qq-chat--message-affects-composer-context-p (message-id)
  "Return non-nil when MESSAGE-ID is the active reply target."
  (equal message-id (appkit-chatbuf-aux-message-id)))

(defun qq-chat--refresh-composer-context-for-message (message-id)
  "Refresh the active reply composer context after MESSAGE-ID changes."
  (when (qq-chat--message-affects-composer-context-p message-id)
    (when-let* ((message (qq-chat--message-by-server-id message-id)))
      (let ((state (copy-sequence (appkit-chatbuf-aux-state))))
        (setq state (plist-put state :aux-msg message))
        (appkit-chatbuf-aux-set state)))
    (qq-chat--update-frame)))

(defun qq-chat--refresh-timeline-layout ()
  "Refresh projected QQ rows after display geometry changes."
  (when (appkit-chat-timeline-live-p)
    (appkit-chat-timeline-refresh)))

(defun qq-chat--on-window-size-change (&optional _frame)
  "Recompute chat width and refresh rows after window resizing."
  (when (eq major-mode 'qq-chat-mode)
    (when-let* ((window (qq-chat--render-window))
                (next (qq-chat--compute-fill-column window)))
      (when (and (> next 15)
                 (not (equal next qq-chat--fill-column)))
        (setq-local qq-chat--fill-column next
                    fill-column next)
        (qq-chat--refresh-timeline-layout)))))

(defun qq-chat--on-text-scale-change ()
  "Recompute pixel alignment after `text-scale-mode' changes."
  (when (eq major-mode 'qq-chat-mode)
    (setq-local qq-chat--fill-column nil)
    (when-let* ((window (qq-chat--render-window))
                (next (qq-chat--compute-fill-column window)))
      (setq-local qq-chat--fill-column next
                  fill-column next))
    (qq-chat--refresh-timeline-layout)))

(defun qq-chat--request-row-redisplay (anchors)
  "Redisplay projected ANCHORS, deferring while a region is active."
  (when (and anchors (appkit-chat-timeline-live-p))
    (appkit-chat-timeline-invalidate
     anchors :defer-while-mark-active t)))

(defun qq-chat--render-empty-placeholder ()
  "Insert the empty timeline placeholder row."
  (let ((start (point)))
    (disco-view-insert-note-line "No messages loaded yet.")
    (insert "\n")
    (add-text-properties
     start (point)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only)
       qq-chat-internal empty-placeholder))))

(defun qq-chat--render-history-gap (message)
  "Render synthetic newer-history gap MESSAGE."
  (let ((start (point))
        (anchor (qq-chat--message-anchor message)))
    (insert "\n  … newer messages are not loaded …  ")
    (insert-text-button
     "Load newer"
     'follow-link t
     'help-echo "Load the next newer history page"
     'action (lambda (_button) (qq-chat-load-newer-messages)))
    (insert "  ")
    (insert-text-button
     "Latest"
     'follow-link t
     'help-echo "Return to the latest cached messages"
     'action (lambda (_button) (qq-chat-return-to-latest)))
    (insert "\n\n")
    (add-text-properties
     start (point)
     (list 'read-only t
           'front-sticky '(read-only)
           'rear-nonsticky '(read-only)
           'qq-chat-message-anchor anchor
           'qq-chat-internal 'history-gap
           'face 'shadow))))

(defun qq-chat--row-printer (row)
  "EWOC pretty-printer for one projected QQ chat ROW."
  (let ((message (appkit-chat-timeline-row-payload row))
        (context (appkit-chat-timeline-row-context row)))
    (cond
   ((eq message qq-chat--empty-placeholder)
    (qq-chat--render-empty-placeholder))
   ((qq-chat--history-gap-message-p message)
    (qq-chat--render-history-gap message))
   (t
    (qq-chat--render-message message context)))))

(defun qq-chat--set-draft (text)
  "Set canonical draft TEXT and update the shared tail composer."
  (appkit-chatbuf-input-state-set text :reset-history-p t)
  (appkit-chatbuf-with-generated-update
    (appkit-chatbuf-input-replace (appkit-chatbuf-input-state))
    (appkit-chatbuf-input-apply-text-properties))
  (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))

(defun qq-chat--push-input-history (text)
  "Insert TEXT into input history when appropriate."
  (unless (appkit-chatbuf-input-has-objects-p)
    (appkit-chatbuf-input-history-push text)))

(defun qq-chat--guess-file-segment-type (path)
  "Return best QQ send segment type for local PATH."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond
     ((member ext '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")) "image")
     ((member ext '("mp4" "mov" "mkv" "webm" "avi" "flv")) "video")
     ((member ext '("amr" "mp3" "wav" "ogg" "m4a" "aac" "flac" "opus")) "record")
     (t "file"))))

(defun qq-chat--segment-object-label (segment)
  "Return visible input label for structured SEGMENT.

Face segments use the same inline image (or `/名称') as the timeline.
Favorite stickers (`image' with sub_type 1, or mface) try local thumbs."
  (let* ((type (or (alist-get 'type segment) "segment"))
         (data (alist-get 'data segment)))
    (pcase type
      ("face"
       (let ((id (or (alist-get 'id data) "?")))
         (qq-media-face-display-string id)))
      ("mface"
       (let* ((summary (or (alist-get 'summary data) "[商城表情]"))
              (file (or (alist-get 'file data)
                        (alist-get 'path data)))
              (image (and file
                          (qq-media-file-present-p file)
                          (qq-media--image-from-file
                           file
                           (max qq-media-face-image-height 32)))))
         (qq-media--image-display-string image (format "[mface:%s]" summary))))
      ("image"
       (let* ((summary (alist-get 'summary data))
              (sub (alist-get 'sub_type data))
              (file (or (alist-get 'file data)
                        (alist-get 'path data)))
              (sticker-p (member sub '(1 "1")))
              (image (and file
                          (qq-media-file-present-p file)
                          (qq-media--image-from-file
                           file
                           (if sticker-p
                               (max qq-media-face-image-height 32)
                             qq-media-face-image-height)))))
         (if (or sticker-p image)
             (qq-media--image-display-string
              image
              (or summary
                  (and file (file-name-nondirectory file))
                  "[image]"))
           (format "[image:%s]"
                   (or summary
                       (and file (file-name-nondirectory file))
                       "item")))))
      (_
       (let* ((path (or (alist-get 'path data)
                        (alist-get 'file data)
                        (alist-get 'name data)
                        "item"))
              (name (file-name-nondirectory (format "%s" path))))
         (format "[%s:%s]" type name))))))

(defun qq-chat--segment-input-object (segment)
  "Return chatbuf input object plist for outbound SEGMENT."
  (list :kind 'qq-segment
        :segment (copy-tree segment)
        :label (qq-chat--segment-object-label segment)))

(defun qq-chat--input-object-segment (object)
  "Return outbound QQ segment represented by chatbuf OBJECT, or nil."
  (when (eq (plist-get object :kind) 'qq-segment)
    (copy-tree (plist-get object :segment))))

(defun qq-chat--insert-input-segment-object (segment)
  "Insert outbound QQ SEGMENT into the current input region as one object."
  (qq-chat--ensure-composer-visible)
  (unless (appkit-chatbuf-point-in-input-p)
    (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
  (when (and (appkit-chatbuf-point-in-input-p)
             (> (point) (or (appkit-chatbuf-input-start-position) (point-min)))
             (let ((ch (char-before)))
               (and ch (not (memq ch '(32 9 10))))))
    (insert " "))
  (let* ((object (qq-chat--segment-input-object segment))
         (label (plist-get object :label)))
    (appkit-chatbuf-input-insert label :object object)
    (appkit-chatbuf-input-apply-text-properties)))

(defun qq-chat-attach-file (path &optional segment-type)
  "Insert local PATH into the chat input as a structured segment object.

When SEGMENT-TYPE is nil, infer the most useful QQ segment type from PATH."
  (interactive
   (list (read-file-name "Attach file: " nil nil t)
         nil))
  (unless (file-readable-p path)
    (user-error "qq: file is not readable: %s" path))
  (let* ((type (or segment-type (qq-chat--guess-file-segment-type path)))
         (segment `((type . ,type)
                    (data . ((file . ,path)
                             (name . ,(file-name-nondirectory path)))))))
    (qq-chat--insert-input-segment-object segment)
    (qq-chat--sync-draft-from-buffer)
    (message "qq: attached %s as %s"
             (file-name-nondirectory path)
             type)))

(defvar qq-chat--face-history nil
  "Minibuffer history for `qq-chat-attach-face'.")

(defun qq-chat--read-base-face-id (&optional prompt)
  "Prompt for and return one QQ base face id.

PROMPT defaults to \"QQ face: \".  Completion uses the existing QQ face
panel ordering, names, and local image affixation."
  (let ((choice
         (completing-read
          (or prompt "QQ face: ")
          (qq-media-face-completion-table)
          nil t nil 'qq-chat--face-history)))
    (or (qq-media-face-id-from-completion choice)
        (user-error "qq: not a face candidate: %s" choice))))

(defun qq-chat-attach-face (&optional face-id)
  "Insert a QQ base face (system emoji) into the chat composer.

With FACE-ID (string or number), insert that face directly.  Interactively,
prompt with completing-read over face names (`/斜眼笑') and ids, each
prefixed with the local face PNG when available.

Candidates stay in QQ face-id order (0, 1, 2, …) via completion
metadata (`display-sort-function' = identity), so Vertico/Icomplete
match QQ's base emoji panel order instead of history/length sort.

Sends as a structured OneBot `face' segment (not Unicode, not CQ text).
Bound via `qq-chat-attach-emoji' (`C-c C-e'); attach transient `e'."
  (interactive
   (list (qq-chat--read-base-face-id)))
  (let* ((id (format "%s" (or face-id
                              (user-error "qq: face id required"))))
         (segment `((type . "face")
                    (data . ((id . ,id))))))
    (qq-chat--insert-input-segment-object segment)
    (qq-chat--sync-draft-from-buffer)
    (message "qq: face %s (%s)"
             (or (qq-media-face-name id) id)
             id)))

(defvar qq-chat--custom-face-history nil
  "Minibuffer history for `qq-chat-attach-custom-face'.")

(defun qq-chat--insert-custom-face (face)
  "Insert favorite FACE alist into the composer as a sendable segment."
  (let ((segment (qq-media-custom-face-to-segment face)))
    (qq-chat--insert-input-segment-object segment)
    (qq-chat--sync-draft-from-buffer)
    (message "qq: favorite %s" (qq-media-custom-face-label face))))

(defun qq-chat--pick-custom-face (faces)
  "Completing-read among FACES and insert the chosen favorite.

Uses `qq-media-custom-face-completion-table' so candidates keep favorites
order and show local thumb previews (same treatment as base faces)."
  (unless faces
    (user-error "qq: no favorite custom faces (收藏表情为空)"))
  (let* ((choice (completing-read
                  "Favorite face: "
                  (qq-media-custom-face-completion-table faces)
                  nil t nil 'qq-chat--custom-face-history))
         (face (qq-media-custom-face-from-completion choice)))
    (unless face
      (user-error "qq: unknown favorite: %s" choice))
    (qq-chat--insert-custom-face face)))
(defun qq-chat-attach-custom-face (&optional force-refresh)
  "Insert a favorite custom face (收藏表情) into the chat composer.

Uses NapCat `fetch_custom_face_info'.  Personal favorites are sent as
image segments with `sub_type' 1; market favorites as mface when possible.

With prefix FORCE-REFRESH, re-fetch the list from NapCat.
Bound via `C-u C-c C-e' or attach transient `E'."
  (interactive "P")
  (let ((cached (and (not force-refresh) (qq-media-custom-faces))))
    (if cached
        (qq-chat--pick-custom-face cached)
      (message "qq: loading favorite faces…")
      (qq-media-refresh-custom-faces
       (lambda (faces)
         (condition-case err
             (qq-chat--pick-custom-face faces)
           (error (message "%s" (error-message-string err)))))
       (lambda (_response reason)
         (message "qq: failed to load favorites: %s" reason))))))

(defun qq-chat-attach-emoji (&optional custom-p)
  "Attach a QQ emoji into the composer.

Without prefix: system base face (`face' id, same as QQ 小黄脸).
With prefix CUSTOM-P (`C-u'): favorite custom face (收藏表情).

Keys: `C-c C-e' / `C-u C-c C-e'."
  (interactive "P")
  (if custom-p
      (qq-chat-attach-custom-face (equal custom-p '(16)))
    (call-interactively #'qq-chat-attach-face)))

(defun qq-chat--clipboard-temp-file (extension)
  "Return a unique temp file path with EXTENSION (including the leading dot)."
  (let ((dir (expand-file-name "clipboard" qq-media-cache-directory)))
    (make-directory dir t)
    (make-temp-file (expand-file-name "qq-clip-" dir) nil extension)))

(defun qq-chat--write-binary-temp-file (data extension)
  "Write binary DATA to a clipboard temp file with EXTENSION and return path."
  (let ((path (qq-chat--clipboard-temp-file extension))
        (coding-system-for-write 'binary))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert data))
    path))

(defun qq-chat--uri-list-local-paths (uri-list)
  "Parse URI-LIST (text/uri-list) into absolute local file paths."
  (let (paths)
    (dolist (uri (split-string (or uri-list "") "[\r\n\0]" t))
      (setq uri (string-trim uri))
      (when (and (not (string-empty-p uri))
                 (not (string-prefix-p "#" uri)))
        (cond
         ((string-match-p "\\`file:" uri)
          (require 'url-parse)
          (let* ((parsed (url-generic-parse-url uri))
                 (raw (url-filename parsed))
                 (path (and raw (url-unhex-string raw))))
            ;; url-filename keeps leading "/" on file:///path.
            (when (and (stringp path)
                       (file-exists-p path))
              (push (expand-file-name path) paths))))
         ((and (file-name-absolute-p uri) (file-exists-p uri))
          (push (expand-file-name uri) paths)))))
    (nreverse paths)))

(defun qq-chat--clipboard-selection (type)
  "Return CLIPBOARD selection for TYPE, or nil."
  (condition-case nil
      (let ((selection-coding-system
             (if (memq type '(image/png image/jpeg image/bmp))
                 'no-conversion
               selection-coding-system)))
        (gui-get-selection 'CLIPBOARD type))
    (error nil)))

(defun qq-chat--yank-media (mime-type data &optional as-file-p)
  "Attach clipboard/yank media DATA of MIME-TYPE into the chat input.

When AS-FILE-P is non-nil, force segment type `file' (telega C-u clipboard)."
  (unless (and (stringp data) (> (length data) 0))
    (user-error "qq: empty clipboard media"))
  (let* ((ext (pcase mime-type
                ((or 'image/png "image/png") ".png")
                ((or 'image/jpeg "image/jpeg") ".jpg")
                ((or 'image/bmp "image/bmp") ".bmp")
                (_ ".bin")))
         (path (qq-chat--write-binary-temp-file data ext))
         (type (if as-file-p "file" (qq-chat--guess-file-segment-type path))))
    (qq-chat-attach-file path type)
    path))

(defun qq-chat-attach-clipboard (&optional as-file-p)
  "Attach clipboard content to the chat composer (telega `C-c C-v').

Prefer order:
1. `text/uri-list' local files from the clipboard
2. image bytes (`image/png', `image/jpeg')
3. on Darwin, `pngpaste' if available

With `\\[universal-argument]' (AS-FILE-P), force segment type `file' instead of
image/video inference."
  (interactive "P")
  (cond
   ;; macOS: pngpaste is the reliable image path (same as telega).
   ((and (eq system-type 'darwin)
         (executable-find "pngpaste"))
    (let ((tmp (qq-chat--clipboard-temp-file ".png")))
      (unless (= 0 (call-process "pngpaste" nil nil nil tmp))
        (ignore-errors (delete-file tmp))
        (user-error "qq: no image in clipboard (pngpaste failed)"))
      (qq-chat-attach-file tmp (if as-file-p "file" "image"))))
   (t
    (let* ((targets (qq-chat--clipboard-selection 'TARGETS))
           (target-list (cond
                         ((stringp targets) (split-string targets))
                         ((listp targets) targets)
                         ((vectorp targets) (append targets nil))
                         (t nil)))
           (has-uri (cl-find "text/uri-list" target-list :test #'string-equal))
           (uris (and has-uri (qq-chat--clipboard-selection 'text/uri-list)))
           (paths (and uris (qq-chat--uri-list-local-paths uris))))
      (cond
       (paths
        (dolist (path paths)
          (qq-chat-attach-file
           path
           (when as-file-p "file"))))
       (t
        (let ((attached nil))
          (dolist (mime '(image/png image/jpeg image/bmp))
            (unless attached
              (when-let* ((data (qq-chat--clipboard-selection mime)))
                (qq-chat--yank-media mime data as-file-p)
                (setq attached t))))
          (unless attached
            (user-error "qq: no file or image in clipboard")))))))))

(defun qq-chat--current-input-segments ()
  "Parse current input region into outbound QQ message segments.

Follow telega's `telega-chatbuf--input-imcs' shape:

1. Split the input string by `appkit-chatbuf-input-object-property'
   (telega splits on `telega-attach' via `telega--split-by-text-prop').
2. Chunks with no object property → plain `text' segments (CJK included).
3. Chunks with an object → the structured segment (image/face/file/…).

With telega-style insert (object body + trailing spacer with
`rear-nonsticky t'), typed text after an attachment is a separate chunk and
is not swallowed into the image segment."
  (let* ((input (or (appkit-chatbuf-input-string) ""))
         (object-prop appkit-chatbuf-input-object-property)
         (chunks (appkit-chatbuf-split-by-text-property input object-prop))
         segments)
    (dolist (chunk chunks)
      (let ((object (and (not (string-empty-p chunk))
                         (get-text-property 0 object-prop chunk))))
        (if object
            (when-let* ((segment (qq-chat--input-object-segment object)))
              (push segment segments))
          (let ((text (substring-no-properties chunk)))
            ;; telega skips blank text chunks; keep pure whitespace only if
            ;; it is the sole content (otherwise trim attachment spacers).
            (unless (string-empty-p text)
              (push `((type . "text")
                      (data . ((text . ,text))))
                    segments))))))
    ;; Drop whitespace-only text segments that are only the telega-style
    ;; attachment spacer leftovers when mixed with real content.
    (setq segments (nreverse segments))
    (if (seq-find (lambda (s)
                    (or (not (equal (alist-get 'type s) "text"))
                        (not (string-blank-p
                              (or (alist-get 'text (alist-get 'data s)) "")))))
                  segments)
        (seq-filter
         (lambda (s)
           (or (not (equal (alist-get 'type s) "text"))
               (not (string-blank-p
                     (or (alist-get 'text (alist-get 'data s)) "")))))
         segments)
      segments)))

(defun qq-chat--composer-context-text (label message-id)
  "Return one composer context line for LABEL and MESSAGE-ID."
  (format "%s %s\n" label message-id))

(defun qq-chat--cancel-reply-button-string ()
  "Return telega-style close button for reply aux (`[×]').

Matches `telega-symbol-button-close' + `telega-link' face."
  (propertize
   "[×]"
   'face 'link
   'mouse-face 'highlight
   'help-echo "Cancel reply (C-c C-k)"
   'follow-link t
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-1]
               (lambda ()
                 (interactive)
                 (qq-chat-cancel-dwim)))
             (define-key map (kbd "RET")
               (lambda ()
                 (interactive)
                 (qq-chat-cancel-dwim)))
             map)
   'qq-chat-cancel-reply t))

(defun qq-chat--reply-context-text ()
  "Return extra context lines shown above the chat composer.

Telega-like aux bar:
  [×] Reply to Name
      short human preview

Never dump OneBot CQ / raw_message here — previews come from
`qq-state-message-preview' (segment-first)."
  (let ((message (qq-chat--reply-message)))
    (if-let* ((reply-id (alist-get 'server-id message))
              (name (or (car (qq-chat--message-sender-display-parts message))
                        "message"))
              (preview (string-trim (or (qq-state-message-preview message) "")))
              (preview (if (string-empty-p preview) "…" preview))
              (preview (truncate-string-to-width preview 64 nil nil t))
              (title (propertize (format "Reply to %s" name)
                                 'face 'qq-msg-title))
              (body (propertize (format "  %s" preview)
                                'face 'qq-msg-inline-reply)))
        (concat
         (qq-chat--cancel-reply-button-string)
         " "
         title
         "\n"
         body
         "\n")
      "")))

(defun qq-chat--insert-date-separator-row (day-label)
  "Insert a date separator row for DAY-LABEL."
  (disco-view-insert-note-line
   (format "-- %s --" day-label)
   :face 'qq-msg-date-separator))

(defun qq-chat--insert-unread-divider-row ()
  "Insert the unread separator row above the first unread message.

Label matches telega's unread bar wording (\"Unread Messages\")."
  (disco-view-insert-note-line
   "Unread Messages"
   :face 'qq-msg-unread-divider))

(defun qq-chat--message-by-server-id (server-id)
  "Return cached message with SERVER-ID in the current session, or nil."
  (when (and server-id qq-chat--session-key)
    (seq-find
     (lambda (message)
       (equal (format "%s" (or (alist-get 'server-id message) ""))
              (format "%s" server-id)))
     (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--message-position (server-id)
  "Return buffer position of SERVER-ID's projected row, or nil."
  (and server-id
       (appkit-chat-timeline-live-p)
       (appkit-chat-timeline-key-position (format "%s" server-id))))

(defun qq-chat--message-end-position (start)
  "Return end position of the message block starting at START."
  (or (and start
           (next-single-property-change
            start 'qq-chat-message-anchor nil (point-max)))
      start))

(defun qq-chat--highlight-region (start end)
  "Pulse region START..END (telega uses `pulse-momentary-highlight-region')."
  (when (and (number-or-marker-p start)
             (number-or-marker-p end)
             (< start end)
             (fboundp 'pulse-momentary-highlight-region))
    (with-no-warnings
      (pulse-momentary-highlight-region start end))))

(defun qq-chat--messages-pop-ring-last-p (message-id)
  "Return non-nil when MESSAGE-ID is already the top of the pop ring."
  (and (ring-p qq-chat--messages-pop-ring)
       (not (ring-empty-p qq-chat--messages-pop-ring))
       (equal (format "%s" (ring-ref qq-chat--messages-pop-ring 0))
              (format "%s" message-id))))

(defun qq-chat--messages-pop-ring-push (message-id)
  "Push MESSAGE-ID onto the jump-back ring (telega chatbuf pop ring)."
  (when (and message-id (ring-p qq-chat--messages-pop-ring))
    (let ((id (format "%s" message-id)))
      (unless (qq-chat--messages-pop-ring-last-p id)
        (ring-insert qq-chat--messages-pop-ring id)
        (message "qq: %s to jump back"
                 (or (key-description
                      (where-is-internal #'qq-chat-goto-pop-message
                                         qq-chat-timeline-mode-map t))
                     "x"))))))

(defun qq-chat--goto-loaded-message (server-id &optional highlight)
  "Goto SERVER-ID only if already displayed (telega `--goto-loaded-msg').

Return non-nil on success.  When HIGHLIGHT is non-nil, pulse the block."
  (when-let* ((id (and server-id (format "%s" server-id)))
              (pos (qq-chat--message-position id)))
    (goto-char pos)
    (when-let* ((win (get-buffer-window (current-buffer) t)))
      (set-window-point win pos)
      (with-selected-window win
        (goto-char pos)
        ;; telega-button--make-observable equivalent: keep target in view.
        (recenter)))
    (when highlight
      (qq-chat--highlight-region pos (qq-chat--message-end-position pos)))
    t))

(defun qq-chat--jump-history-count ()
  "Return history page size used when seeking a jump target."
  (max 1 (or qq-chat-jump-history-count qq-history-fetch-count)))

(defun qq-chat--finish-jump-if-loaded (target)
  "If TARGET is rendered, jump+highlight and clear pending jump.

Return non-nil on success."
  (when (and target (qq-chat--goto-loaded-message target t))
    (setq qq-chat--pending-jump-id nil)
    t))

(defun qq-chat--jump-fail (target &optional reason)
  "Clear pending jump for TARGET and report not found."
  (when (equal qq-chat--pending-jump-id target)
    (setq qq-chat--pending-jump-id nil))
  (message "qq: message not found%s"
           (if (and reason (not (string-empty-p reason)))
               (format " (%s)" reason)
             "")))

(defun qq-chat--seek-history-for-jump (session-key target buffer)
  "Load the fork-native history window centered on TARGET."
  (qq-api-fetch-history-around
   session-key
   target
   (lambda (meta)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (equal qq-chat--session-key session-key)
           (qq-chat--note-history-window meta)
           (unless (qq-chat--finish-jump-if-loaded target)
             (qq-chat--jump-fail target "around window omitted target"))))))
   (lambda (_response reason)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (equal qq-chat--session-key session-key)
           (qq-chat--jump-fail target reason)))))
   (qq-chat--jump-history-count)))

(defun qq-chat-goto-message (message-id &optional no-pop)
  "Goto MESSAGE-ID in the current chatbuf (telega `telega-chatbuf--goto-msg').

Push the message at point onto the pop ring unless NO-POP.  Already loaded
targets jump immediately; other targets use the fork-native
`get_msg_history_around' action exactly once."
  (interactive
   (list (or (get-text-property (point) 'qq-chat-reply-id)
             (qq-chat--message-reply-id (qq-chat--message-at-point))
             (read-string "Message id: "))))
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((id (and message-id (format "%s" message-id)))
        (session-key qq-chat--session-key)
        (buffer (current-buffer)))
    (unless (and id (not (string-empty-p id)))
      (user-error "qq: no message id to jump to"))
    ;; telega: put message at point into messages-pop-ring.
    (unless no-pop
      (when-let* ((at-point (qq-chat--message-at-point))
                  (cur-id (qq-chat--message-anchor at-point)))
        (unless (equal (format "%s" cur-id) id)
          (qq-chat--messages-pop-ring-push cur-id))))
    (if (qq-chat--goto-loaded-message id t)
        (setq qq-chat--pending-jump-id nil)
      (setq qq-chat--pending-jump-id id)
      (message "qq: loading…")
      (qq-chat--seek-history-for-jump session-key id buffer))))

(defun qq-chat-goto-reply (&optional message)
  "Goto the message that MESSAGE replies to (telega `telega-msg-goto-reply-to-message').

MESSAGE defaults to the message at point.  Bound to timeline `g'.  The ↪
reply preview line is also a button that calls this path."
  (interactive)
  (let* ((msg (or message
                  (qq-chat--message-at-point)
                  (user-error "qq: no message at point")))
         (reply-id (or (qq-chat--message-reply-id msg)
                       (user-error "qq: message is not a reply"))))
    (qq-chat-goto-message reply-id)))

(defun qq-chat-goto-pop-message ()
  "Pop a message from the jump ring and goto it (telega `telega-chatbuf-goto-pop-message').

Bound to timeline `x'."
  (interactive)
  (unless (and (ring-p qq-chat--messages-pop-ring)
               (not (ring-empty-p qq-chat--messages-pop-ring)))
    (user-error "qq: no messages to pop to"))
  (let ((id (ring-remove qq-chat--messages-pop-ring 0)))
    (message "qq: %d messages left in ring"
             (ring-length qq-chat--messages-pop-ring))
    ;; Avoid re-pushing current while popping (telega binds ring to nil).
    (let ((qq-chat--messages-pop-ring nil))
      (qq-chat-goto-message id 'no-pop))))

(defun qq-chat--insert-reply-preview-line (reply-id properties prefix-state)
  "Insert one inline reply preview line for REPLY-ID.

Telega uses `telega-ins--with-props' + `:action telega-msg-goto-reply-to-message'
on the reply header.  Here the line is a button (RET / mouse-1) that jumps to
REPLY-ID via `qq-chat-goto-message'."
  (let* ((source (qq-chat--message-by-server-id reply-id))
         (sender (and source (car (qq-chat--message-sender-display-parts source))))
         (preview (and source (string-trim (or (qq-state-message-preview source) ""))))
         (body (cond
                ((and sender preview (not (string-empty-p preview)))
                 (format "%s: %s" sender
                         (truncate-string-to-width preview 56 nil nil t)))
                ((and preview (not (string-empty-p preview)))
                 (truncate-string-to-width preview 64 nil nil t))
                (t (format "id %s" reply-id))))
         (reply-start (point))
         (target (format "%s" reply-id))
         (map (let ((map (make-sparse-keymap)))
                (set-keymap-parent map button-map)
                (define-key map [mouse-1]
                  (lambda ()
                    (interactive)
                    (qq-chat-goto-message target)))
                (define-key map (kbd "RET")
                  (lambda ()
                    (interactive)
                    (qq-chat-goto-message target)))
                map)))
    (insert (format "↪ %s\n" body))
    (add-text-properties
     reply-start (point)
     (append properties
             (list 'face 'qq-msg-inline-reply
                   'mouse-face 'highlight
                   'help-echo "Jump to replied message (telega-style; g / RET)"
                   'follow-link t
                   'keymap map
                   'button t
                   'category 'default-button
                   'action (lambda (_button)
                             (qq-chat-goto-message target))
                   'qq-chat-reply-id target
                   'qq-chat-reply-button t)))
    (disco-ui-apply-line-prefix reply-start (point) prefix-state)))

(defun qq-chat--face-segment-id (segment)
  "Return QQ base face id from face SEGMENT, or nil."
  (let* ((data (alist-get 'data segment))
         (raw (alist-get 'raw data)))
    (or (alist-get 'id data)
        (alist-get 'faceIndex data)
        (and (listp raw) (alist-get 'faceIndex raw)))))

(defun qq-chat--segment-inline-string (segment)
  "Return inline display string for SEGMENT, or nil for block-like segments.

Face segments render as inline images (LinuxQQ default-emojis / NapCat
base emoji), never as OneBot CQ text."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("text" (or (alist-get 'text data) ""))
      ("at"
       (let* ((target (and (alist-get 'qq data)
                           (format "%s" (alist-get 'qq data))))
              (kind (cond
                     ((equal target "all") 'at-all)
                     ((and target
                           (equal target (qq-state-self-user-id))) 'at-me)
                     (t 'ordinary)))
              (label (or (alist-get 'name data)
                         (and (eq kind 'at-all) "全体成员")
                         target
                         "mention")))
         (propertize (concat "@" label)
                     'face (if (memq kind '(at-me at-all))
                               'qq-msg-mention-self
                             'qq-msg-mention)
                     'qq-chat-mention-kind kind)))
      ("__unsupported"
       (qq-state-message-preview-from-segments (list segment)))
      ("face"
       (qq-media-face-display-string
        (or (qq-chat--face-segment-id segment) "?")))
      (_ nil))))

(defun qq-chat--mail-segment-p (segment)
  "Return non-nil when SEGMENT is a structured QQ Mail notification."
  (equal (alist-get 'type segment) "mail"))

(defun qq-chat--poke-segment-p (segment)
  "Return non-nil when SEGMENT is a QQ gray-tip poke decoration."
  (equal (alist-get 'type segment) "poke"))

(defun qq-chat--gray-tip-segment-p (segment)
  "Return non-nil when SEGMENT is a QQ JSON gray-tip notice."
  (equal (alist-get 'type segment) "gray-tip"))

(defun qq-chat--insert-gray-tip-message (message properties)
  "Insert centered JSON gray-tip MESSAGE using PROPERTIES."
  (let ((start (point)))
    (disco-ins-insert-divider-row
     (qq-chat--message-body message)
     'qq-msg-poke
     (qq-chat--line-fill-column))
    (add-text-properties start (point) properties)))

(defun qq-chat--insert-poke-message (message properties)
  "Insert decorative gray-tip MESSAGE using PROPERTIES.

Pokes deliberately bypass the ordinary message header/body layout.  Following
telega's special-message inserter, render one centered `( action )' row with
horizontal bars and keep the time at the far right.  QQ's native descriptive
tail stays on that same row; truncate the action before allowing it to collide
with the timestamp."
  (let* ((data (or (qq-state-poke-message-data message) '()))
         (actor (or (qq-chat--present-string (alist-get 'actor-name data))
                    (qq-chat--present-string (alist-get 'sender-name message))
                    "某人"))
         (target (qq-chat--present-string (alist-get 'target-name data)))
         (action (or (qq-chat--present-string (alist-get 'action data))
                     "戳了戳"))
         (detail (qq-chat--present-string (alist-get 'detail data)))
         (image-url (qq-chat--present-string (alist-get 'image-url data)))
         (image
          (if image-url
              (qq-media-url-preview-display-string
               (qq-media-poke-image-cache-key image-url)
               image-url
               "✦"
               qq-media-poke-image-height)
            "✦"))
         (time (qq-chat--format-time-short (alist-get 'time message)))
         (time-width (string-width time))
         (max-content-width
          (max 1 (- (qq-chat--line-fill-column) time-width 12)))
         (content
          (truncate-string-to-width
           (concat image " "
                   (string-join
                    (delq nil (list actor action target detail))
                    " "))
           max-content-width nil nil "…"))
         (core (concat "( " content " )"))
         (available (- (qq-chat--line-fill-column)
                       (string-width core)
                       time-width
                       8))
         (bar-count (max 0 (floor (/ (max 0 available) 2))))
         (bars (make-string bar-count ?-))
         (start (point))
         (poke-properties (append properties (list 'face 'qq-msg-poke))))
    (insert bars " " core " " bars)
    (qq-chat--insert-right-aligned-time time nil nil)
    (insert "\n")
    (add-text-properties start (point) poke-properties)
    (when (not (string-empty-p time))
      (save-excursion
        (goto-char (1- (point)))
        (let ((time-start (- (point) (length time))))
          (add-text-properties time-start (point)
                               (list 'face 'qq-msg-status)))))))

(defun qq-chat--card-segment-p (segment)
  "Return non-nil when SEGMENT is a normalized Ark rich card."
  (equal (alist-get 'type segment) "card"))

(defun qq-chat--insert-mail-segment (segment prefix-state properties)
  "Insert structured QQ Mail SEGMENT as a compact card."
  (let* ((data (alist-get 'data segment))
         (sender (alist-get 'sender data))
         (subject (alist-get 'subject data))
         (content (alist-get 'content data))
         (prompt (alist-get 'prompt data))
         (detail (or (alist-get 'detail data) "Open Mail"))
         (url (alist-get 'url data))
         (disco-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (disco-ui-card-prefix-state)))
    (disco-ui-insert-prefixed-lines
     card-prefix-state
     (if (and (stringp sender) (not (string-empty-p sender)))
         (format "Mail · %s" sender)
       "Mail")
     :face 'bold
     :properties properties)
    (when (and (stringp subject) (not (string-empty-p subject)))
      (disco-ui-insert-prefixed-lines
       card-prefix-state subject :properties properties))
    (when-let* ((body (cond
                       ((and (stringp content) (not (string-empty-p content))) content)
                       ((and (stringp prompt) (not (string-empty-p prompt))) prompt))))
      (disco-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties properties))
    (when (and (stringp url) (not (string-empty-p url)))
      (let ((start (point)))
        (disco-ui-insert-action-button
         (format "[%s]" detail)
         (lambda () (browse-url url t))
         :help-echo "Open this message in QQ Mail")
        (insert "\n")
        (disco-ui-apply-line-prefix start (point) card-prefix-state)
        (add-text-properties start (point) properties)))))

(defun qq-chat--card-kind-label (kind)
  "Return a short display label for normalized card KIND."
  (pcase kind
    ("miniapp" "Mini App")
    ("share" "Share")
    ("forward" "Chat History")
    ("forum" "Channel Post")
    ("announcement" "Group Announcement")
    ("contact" "Contact")
    (_ "Card")))

(defun qq-chat--insert-card-segment (segment prefix-state properties)
  "Insert normalized Ark rich-card SEGMENT."
  (let* ((data (alist-get 'data segment))
         (kind (alist-get 'kind data))
         (label (qq-chat--card-kind-label kind))
         (title (qq-chat--present-string (alist-get 'title data)))
         (content (qq-chat--present-string (alist-get 'content data)))
         (prompt (qq-chat--present-string (alist-get 'prompt data)))
         (source (qq-chat--present-string (alist-get 'source data)))
         (summary (qq-chat--present-string (alist-get 'summary data)))
         (url (qq-chat--present-string (alist-get 'url data)))
         (body (or content
                   (and (not title) prompt)))
         (open-action (and url (lambda () (browse-url url t))))
         (map (when open-action
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map button-map)
                  (define-key map (kbd "RET")
                    (lambda () (interactive) (funcall open-action)))
                  (define-key map [mouse-1]
                    (lambda () (interactive) (funcall open-action)))
                  map)))
         (card-properties
          (append
           properties
           (when open-action
             (list 'mouse-face 'highlight
                   'help-echo (format "Open this %s" (downcase label))
                   'follow-link t
                   'keymap map
                   'button t
                   'category 'default-button
                   'action (lambda (_button) (funcall open-action))))))
         (disco-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (disco-ui-card-prefix-state))
         (start (point)))
    (disco-ui-insert-prefixed-lines
     card-prefix-state
     (if source (format "%s · %s" label source) label)
     :face 'bold
     :properties card-properties)
    (when title
      (disco-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (when body
      (disco-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties card-properties))
    (when (and summary (not (equal summary body)))
      (disco-ui-insert-prefixed-lines
       card-prefix-state summary :face 'shadow :properties card-properties))
    (when open-action
      (add-text-properties start (point) card-properties))))

(defun qq-chat--media-segment-p (segment)
  "Return non-nil when SEGMENT should render as a media block."
  (member (alist-get 'type segment)
          '("image" "file" "record" "video" "mface")))

(defun qq-chat--segment-media-kind-label (segment)
  "Return short kind label for media SEGMENT."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("image" (if (alist-get 'emoji_id data) "Sticker" "Image"))
      ("file" (if (qq-media-imageish-file-segment-p segment) "Image" "File"))
      ("record" "Voice")
      ("video" "Video")
      ("mface" "Sticker")
      (_ "Media"))))

(defun qq-chat--segment-media-summary (segment)
  "Return one-line human summary for media SEGMENT.

Prefer a short name; never dump full URLs or CQ blobs into the timeline."
  (let* ((data (alist-get 'data segment))
         (type (alist-get 'type segment))
         (fallback (pcase type
                     ("image" "image")
                     ("mface" "sticker")
                     ("record" "voice")
                     ("video" "video")
                     ("file" "file")
                     (_ "media")))
         (summary (or (alist-get 'summary data)
                      (alist-get 'name data)
                      (alist-get 'file data))))
    (qq-state--short-media-label summary fallback)))

(defun qq-chat--format-byte-size (value)
  "Return human-readable string for byte VALUE, or nil."
  (let ((bytes (cond
                ((integerp value) value)
                ((stringp value) (truncate (string-to-number value)))
                (t nil))))
    (when (and bytes (> bytes 0))
      (cond
       ((>= bytes (* 1024 1024 1024))
        (format "%.1f GB" (/ bytes 1073741824.0)))
       ((>= bytes (* 1024 1024))
        (format "%.1f MB" (/ bytes 1048576.0)))
       ((>= bytes 1024)
        (format "%.1f KB" (/ bytes 1024.0)))
       (t (format "%d B" bytes))))))

(defun qq-chat--segment-media-meta-line (segment)
  "Return one quiet meta line for media SEGMENT.

Keep this short — size is useful; internal sub_type / emoji ids are not."
  (let* ((data (alist-get 'data segment))
         (size (qq-chat--format-byte-size (or (alist-get 'file_size data)
                                              (alist-get 'file-size data)))))
    (or size "")))

(defun qq-chat--segment-media-card-kind (segment)
  "Return shared media card kind for OneBot SEGMENT."
  (pcase (alist-get 'type segment)
    ("record" 'audio)
    ("mface" 'sticker)
    (_ (qq-media-segment-kind segment))))

(defun qq-chat--segment-media-card-context (segment &optional capabilities)
  "Adapt OneBot media SEGMENT to the shared card action protocol.

CAPABILITIES defaults to the centralized `qq-media' action/status model."
  (let* ((capabilities (or capabilities
                           (qq-media-segment-capabilities segment)))
         (url (plist-get capabilities :remote-url)))
    (disco-media-card-context-create
     :payload segment
     :kind (qq-chat--segment-media-card-kind segment)
     :title (qq-chat--segment-media-summary segment)
     :open-action (when (plist-get capabilities :open)
                    (lambda ()
                      (qq-media-segment-open segment)))
     :download-action (when (plist-get capabilities :download)
                        (lambda ()
                          (qq-media-segment-start-download segment)))
     :save-as-action (when (plist-get capabilities :save)
                       (lambda ()
                         (qq-media-segment-save-as segment)))
     :copy-url-action (when (plist-get capabilities :copy-url)
                        (lambda ()
                          (kill-new url)
                          (message "qq: copied media URL"))))))

(defun qq-chat--media-card-fallback-context ()
  "Return primary media context for the message at point."
  (when-let* ((message (ignore-errors (qq-chat--message-at-point)))
              (segment (qq-media-message-primary-segment message)))
    (qq-chat--segment-media-card-context segment)))

(defun qq-chat--insert-segment-media-line (segment prefix-state properties)
  "Insert one rich media card for SEGMENT using PREFIX-STATE and PROPERTIES."
  (let* ((kind-label (qq-chat--segment-media-kind-label segment))
         (meta (qq-chat--segment-media-meta-line segment))
         (capabilities (qq-media-segment-capabilities segment))
         (context (qq-chat--segment-media-card-context
                   segment capabilities))
         (prefix-state (let ((disco-ui-card-indent-prefix-state prefix-state))
                         (disco-ui-card-prefix-state))))
    (disco-ins-insert-media-card
     :kind (qq-chat--segment-media-card-kind segment)
     :title (qq-chat--segment-media-summary segment)
     :details (unless (string-empty-p meta) (list meta))
     :status (plist-get capabilities :status)
     :prefix prefix-state
     :title-face 'bold
     :meta-face 'shadow
     :properties properties
     :context context
     :open-help-echo (format "Open %s in Emacs or the configured player"
                             (downcase kind-label))
     :body-inserter
     (lambda (card-prefix-state)
       (when (qq-media-segment-preview-capable-p segment)
         (let ((preview-start (point))
               (preview (qq-media-segment-preview-image segment))
               (loading (qq-media-segment-preview-fetching-p segment))
               preview-end)
           (cond
            (preview
             (condition-case _
                 (progn
                   (disco-media-insert-image-slices
                    preview nil nil
                    (cond
                     ((equal (alist-get 'type segment) "mface") "[sticker]")
                     ((qq-media-videoish-segment-p segment) "[video]")
                     (t "[image]")))
                   (setq preview-end (point)))
               (error
                (insert "[preview unavailable]")))
             (when preview-end
               (disco-media-add-action-properties
                preview-start preview-end
                (lambda (&optional _event)
                  (interactive)
                  (disco-media-card-call-action 'open context))
                (format "Open %s" (downcase kind-label))))
             (insert "\n"))
            (loading
             (insert "[loading preview]\n"))
            (t
             (insert "[preview unavailable]\n")))
           (disco-ui-apply-line-prefix preview-start (point) card-prefix-state)
           (disco-ui-append-face preview-start (point) 'shadow)))))))

(defun qq-chat--insert-message-body (message prefix-state properties)
  "Insert MESSAGE content body using PREFIX-STATE and PROPERTIES."
  (let ((segments (alist-get 'segments message))
        (inline-parts nil))
    (cl-labels ((flush-inline ()
                  (when inline-parts
                    (disco-ui-insert-prefixed-lines
                     prefix-state
                     (mapconcat #'identity (nreverse inline-parts) "")
                     :properties properties)
                    (setq inline-parts nil))))
      (if (or (qq-state-message-recalled-p message)
              (null segments))
          (disco-ui-insert-prefixed-lines prefix-state (qq-chat--message-body message) :properties properties)
        (dolist (segment segments)
          (let ((type (alist-get 'type segment)))
            (unless (equal type "reply")
              (cond
               ((qq-chat--forward-segment-p segment)
                (flush-inline)
                (qq-forward-insert-segment
                 (qq-chat--canonical-forward-segment segment)
                 prefix-state properties))
               ((qq-chat--mail-segment-p segment)
                (flush-inline)
                (qq-chat--insert-mail-segment segment prefix-state properties))
               ((qq-chat--card-segment-p segment)
                (flush-inline)
                (qq-chat--insert-card-segment segment prefix-state properties))
               ((qq-chat--media-segment-p segment)
                (flush-inline)
                (qq-chat--insert-segment-media-line segment prefix-state properties))
               ((qq-chat--segment-inline-string segment)
                (push (qq-chat--segment-inline-string segment) inline-parts))
               (t
                (push (format "[%s]" (or type "segment")) inline-parts))))))
        (flush-inline)))))

(defun qq-chat--reaction-by-emoji-id (message emoji-id)
  "Return MESSAGE reaction matching EMOJI-ID, or nil."
  (seq-find
   (lambda (reaction)
     (equal (alist-get 'emoji-id reaction) (format "%s" emoji-id)))
   (qq-state-message-reactions message)))

(defun qq-chat--unicode-reaction-string (emoji-id)
  "Return Unicode character represented by decimal EMOJI-ID, or nil."
  (when (and (stringp emoji-id)
             (string-match-p "\\`[0-9]+\\'" emoji-id))
    (let* ((codepoint (string-to-number emoji-id))
           (character (and (<= 0 codepoint #x10ffff)
                           (not (<= #xd800 codepoint #xdfff))
                           (decode-char 'ucs codepoint))))
      (and character (char-to-string character)))))

(defun qq-chat--reaction-display-string (reaction)
  "Return inline display string for normalized REACTION."
  (let ((emoji-id (alist-get 'emoji-id reaction))
        (emoji-type (alist-get 'emoji-type reaction)))
    (if (equal emoji-type "2")
        (or (qq-chat--unicode-reaction-string emoji-id)
            (format "[emoji:%s]" emoji-id))
      (qq-media-face-display-string emoji-id))))

(defun qq-chat--message-reactable-p (message)
  "Return non-nil when MESSAGE can receive a group reaction."
  (and (listp message)
       (eq (alist-get 'type (qq-chat--session)) 'group)
       (qq-api-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))))

(defun qq-chat-toggle-message-reaction (message-id emoji-id)
  "Toggle EMOJI-ID on cached MESSAGE-ID, telega reaction-button style."
  (interactive
   (let* ((message (or (qq-chat--message-at-point)
                       (user-error "qq: no message at point")))
          (reaction (or (car (qq-state-message-reactions message))
                        (user-error "qq: message has no reaction to toggle"))))
     (list (alist-get 'server-id message)
           (alist-get 'emoji-id reaction))))
  (let* ((message (or (qq-chat--message-by-server-id message-id)
                      (user-error "qq: reaction message is not loaded")))
         (_reactable (or (qq-chat--message-reactable-p message)
                         (user-error "qq: reactions require a live group message")))
         (reaction (qq-chat--reaction-by-emoji-id message emoji-id))
         (set (not (and reaction (alist-get 'chosen-p reaction)))))
    (qq-api-set-message-emoji-like
     message-id emoji-id set
     (lambda (_response)
       (message "qq: reaction %s (%s)"
                (if set "added" "removed") emoji-id)))))

(defun qq-chat--insert-reaction-line (message prefix-state properties)
  "Insert shared disco reaction chips adapted for QQ MESSAGE."
  (let ((reactions (qq-state-message-reactions message))
        (message-id (alist-get 'server-id message)))
    (when reactions
      (when-let* ((span
                   (disco-ins-insert-reaction-line
                    reactions
                    :prefix prefix-state
                    :selected-face 'qq-msg-reaction-chosen
                    :unselected-face 'qq-msg-reaction
                    :label-function
                    (lambda (reaction)
                      (concat " "
                              (qq-chat--reaction-display-string reaction)
                              " "
                              (number-to-string
                               (or (alist-get 'count reaction) 0))
                              " "))
                    :selected-p-function
                    (lambda (reaction) (alist-get 'chosen-p reaction))
                    :action-function
                    (lambda (reaction)
                      (qq-chat-toggle-message-reaction
                       message-id (alist-get 'emoji-id reaction)))
                    :help-echo-function
                    (lambda (reaction)
                      (let ((emoji-id (alist-get 'emoji-id reaction)))
                        (format "%s %s"
                                (if (alist-get 'chosen-p reaction)
                                    "Remove reaction"
                                  "Add reaction")
                                (or (qq-media-face-name emoji-id)
                                    emoji-id)))))))
        (add-text-properties (car span) (cdr span) properties)))))

(defun qq-chat--insert-compact-message-body (message prefix-state properties short-time)
  "Insert a same-sender continuation body for MESSAGE.

Uses structured segments (so faces render as images).  SHORT-TIME is appended
on the first inline line when the body is pure inline content."
  (let ((segments (alist-get 'segments message))
        (inline-parts nil)
        (saw-block nil))
    (cl-labels
        ((flush-inline (&optional with-time)
           (when inline-parts
             (let* ((text (mapconcat #'identity (nreverse inline-parts) ""))
                    (start (point)))
               (insert (if (string-empty-p text) "(empty message)" text))
               (when (and with-time
                          (stringp short-time)
                          (not (string-empty-p short-time)))
                 (qq-chat--insert-right-aligned-time
                  short-time
                  (string-width
                   (or (disco-ui-prefix-state-current prefix-state) ""))
                  t))
               (insert "\n")
               (disco-ui-apply-line-prefix start (point) prefix-state)
               (add-text-properties start (point) properties)
               (setq inline-parts nil)))))
      (cond
       ((or (qq-state-message-recalled-p message) (null segments))
        (let* ((text (qq-chat--message-body message))
               (start (point)))
          (insert (if (string-empty-p text) "(empty message)" text))
          (when (and (stringp short-time) (not (string-empty-p short-time)))
            (qq-chat--insert-right-aligned-time
             short-time
             (string-width
              (or (disco-ui-prefix-state-current prefix-state) ""))
             t))
          (insert "\n")
          (disco-ui-apply-line-prefix start (point) prefix-state)
          (add-text-properties start (point) properties)))
       (t
        (dolist (segment segments)
          (let ((type (alist-get 'type segment)))
            (unless (equal type "reply")
              (cond
               ((qq-chat--forward-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-forward-insert-segment
                 (qq-chat--canonical-forward-segment segment)
                 prefix-state properties))
               ((qq-chat--mail-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-mail-segment
                 segment prefix-state properties))
               ((qq-chat--card-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-card-segment
                 segment prefix-state properties))
               ((qq-chat--media-segment-p segment)
                (flush-inline nil)
                (setq saw-block t)
                (qq-chat--insert-segment-media-line
                 segment prefix-state properties))
               ((qq-chat--segment-inline-string segment)
                (push (qq-chat--segment-inline-string segment) inline-parts))
               (t
                (push (format "[%s]" (or type "segment")) inline-parts))))))
        ;; Time rides on the first (only) inline flush when there is no media card.
        (flush-inline (not saw-block))
        (when (and saw-block
                   (stringp short-time)
                   (not (string-empty-p short-time))
                   (null inline-parts))
          (let ((start (point)))
            (qq-chat--insert-right-aligned-time
             short-time
             (string-width
              (or (disco-ui-prefix-state-current prefix-state) ""))
             t)
            (insert "\n")
            (disco-ui-apply-line-prefix start (point) prefix-state)
            (add-text-properties
             start (point)
             (append properties (list 'face 'qq-msg-status))))))))))

(defun qq-chat--set-pending-reply (message)
  "Set MESSAGE as the pending reply target in current chat buffer."
  (qq-chat--ensure-composer-visible)
  (let ((message-id (alist-get 'server-id message)))
    (unless message-id
      (user-error "qq: selected message has no server id"))
    (qq-chat--set-reply-message message)
    (qq-chat--update-frame)
    (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (message "qq: next message will reply to %s" message-id)))

(defun qq-chat--delete-message-internal (message)
  "Recall MESSAGE after confirmation."
  (let ((message-id (alist-get 'server-id message)))
    (unless message-id
      (user-error "qq: selected message has no server id"))
    (when (y-or-n-p (format "Recall message %s? " message-id))
      (if (qq-state-poke-message-p message)
          (qq-api-recall-poke message-id)
        (qq-api-delete-message message-id)))))

(defun qq-chat--message-title-face (message)
  "Return sender title face for MESSAGE."
  (if (alist-get 'self-p message)
      'qq-msg-self-title
    'qq-msg-user-title))

(defun qq-chat--message-avatar-prefixes (message)
  "Return shared telega-style two-line avatar prefixes for MESSAGE."
  (let* ((sender-id (alist-get 'sender-id message))
         (image (and sender-id
                     (not (equal (format "%s" sender-id) "0"))
                     (qq-media-avatar-image sender-id)))
         (prefixes
          (disco-chat-avatar-prefixes
           image "@"
           :pixel-size (disco-chat-avatar-two-line-pixel-size)
           :resize t))
         (avatar-properties
          (list 'mouse-face 'highlight
                'help-echo "Open sender avatar"
                'qq-chat-avatar-sender-id sender-id)))
    (dolist (key '(:header :first-body))
      (let ((prefix (copy-sequence (plist-get prefixes key))))
        (when (and (stringp prefix) (> (length prefix) 0))
          (add-text-properties 0 (length prefix) avatar-properties prefix))
        (setq prefixes (plist-put prefixes key prefix))))
    prefixes))

(defun qq-chat--render-message (message context)
  "Insert one formatted MESSAGE block using projected CONTEXT.

Visual model (telega-inspired; later appkit):
- optional date / unread bars above the node
- heading row: avatar + sender + status + time (`qq-msg-heading')
- optional reply preview (`qq-msg-inline-reply')
- body with light indent (no `>>'/`|' gutters)
- no per-message action button row (use `C-c m r/d/o/a' at point)"
  (let* ((anchor (qq-chat--message-anchor message))
         (start (point))
         (insert-date (plist-get context :insert-date))
         (insert-unread (plist-get context :insert-unread))
         (title-face (qq-chat--message-title-face message))
         (reply-id (qq-chat--message-reply-id message))
         (properties (qq-chat--message-line-properties message anchor))
         (status-suffix (qq-chat--status-suffix message))
         (compact (plist-get context :compact))
         (marked (qq-chat--message-marked-p message))
         (ordinary-message-p
          (not (or (qq-state-gray-tip-message-p message)
                   (qq-state-poke-message-p message))))
         (avatar-prefixes
          (and ordinary-message-p
               (qq-chat--message-avatar-prefixes message)))
         (header-prefix (or (plist-get avatar-prefixes :header) ""))
         (body-first-prefix
          (or (plist-get avatar-prefixes :first-body) "  "))
         (body-rest-prefix
          (or (plist-get avatar-prefixes :rest-body) "  "))
         (body-prefix-state
          (if compact
              (disco-ui-make-prefix-state body-rest-prefix body-rest-prefix)
            (disco-ui-make-prefix-state body-first-prefix body-rest-prefix)))
         (short-time (qq-chat--format-time-short (alist-get 'time message))))
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (qq-chat--insert-date-separator-row (qq-chat--message-day-label insert-date)))
    (when insert-unread
      (qq-chat--insert-unread-divider-row))
    (when marked
      (disco-ui-insert-prefixed-lines
       (disco-ui-make-prefix-state body-rest-prefix body-rest-prefix)
       "✓ selected for merged forwarding"
       :face 'warning
       :properties properties))
    (cond
     ((qq-state-gray-tip-message-p message)
      (qq-chat--insert-gray-tip-message message properties))
     ((and (qq-state-poke-message-p message)
           (not (qq-state-message-recalled-p message)))
      (qq-chat--insert-poke-message message properties))
     ((qq-state-message-recalled-p message)
      ;; Stub path for `qq-chat-show-recalled-messages' (telega deleted style).
      (let ((header-start (point)))
        (qq-chat--insert-message-sender message title-face)
        (insert status-suffix)
        (qq-chat--insert-right-aligned-time
         (qq-chat--format-time (alist-get 'time message))
         (string-width header-prefix)
         t)
        (insert "\n")
        (disco-ui-apply-line-prefix
         header-start (point)
         (disco-ui-make-prefix-state header-prefix body-rest-prefix))
        (add-text-properties
         header-start (point)
         (append properties (list 'face 'qq-msg-deleted)))
        (disco-ui-insert-prefixed-lines
         body-prefix-state
         (qq-chat--message-body message)
         :properties (append properties (list 'face 'qq-msg-deleted)))))
     (compact
      ;; Same-sender continuations still need segment-rich bodies (faces,
      ;; images, …).  Never dump plain `preview'/CQ text here — that is what
      ;; produced visible "[face:178]" while the image path already worked.
      (when reply-id
        (qq-chat--insert-reply-preview-line reply-id properties body-prefix-state))
      (qq-chat--insert-compact-message-body
       message body-prefix-state properties short-time))
     (t
      (let ((header-start (point)))
        (qq-chat--insert-message-sender message title-face)
        (insert status-suffix)
        (qq-chat--insert-right-aligned-time
         (qq-chat--format-time (alist-get 'time message))
         (string-width header-prefix)
         t)
        (insert "\n")
        (disco-ui-apply-line-prefix
         header-start (point)
         (disco-ui-make-prefix-state header-prefix body-rest-prefix))
        (disco-ui-append-face header-start (point) 'qq-msg-heading)
        (add-text-properties header-start (point) properties))
      (when reply-id
        (qq-chat--insert-reply-preview-line reply-id properties body-prefix-state))
      (qq-chat--insert-message-body message body-prefix-state properties)))
    (unless (or (qq-state-message-recalled-p message)
                (qq-state-poke-message-p message)
                (qq-state-gray-tip-message-p message))
      (qq-chat--insert-reaction-line message body-prefix-state properties))
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-chat--history-search (query forward)
  "Search chat history for QUERY in FORWARD direction.

Return non-nil on success."
  (let ((case-fold-search t)
        (history-end
         (max (point-min)
              (1- (or (appkit-chatbuf-input-start-position) (point-max))))))
    (save-restriction
      (narrow-to-region (point-min) (max (point-min) history-end))
      (if forward
          (or (search-forward query nil t)
              (progn
                (goto-char (point-min))
                (search-forward query nil t)))
        (or (search-backward query nil t)
            (progn
              (goto-char (point-max))
              (search-backward query nil t)))))))

(defun qq-chat-search (query)
  "Search current chat history for QUERY."
  (interactive (list (read-string "Search chat: " qq-chat--last-search-query)))
  (setq qq-chat--last-search-query query)
  (unless (and (stringp query) (not (string-empty-p query)))
    (user-error "qq: empty search query"))
  (goto-char (point-min))
  (unless (qq-chat--history-search query t)
    (message "qq: no match for %s" query)))

(defun qq-chat-search-next ()
  "Jump to the next match for the last chat search."
  (interactive)
  (unless (and qq-chat--last-search-query
               (not (string-empty-p qq-chat--last-search-query)))
    (user-error "qq: no active chat search"))
  (unless (qq-chat--history-search qq-chat--last-search-query t)
    (message "qq: no further match for %s" qq-chat--last-search-query)))

(defun qq-chat-search-prev ()
  "Jump to the previous match for the last chat search."
  (interactive)
  (unless (and qq-chat--last-search-query
               (not (string-empty-p qq-chat--last-search-query)))
    (user-error "qq: no active chat search"))
  (unless (qq-chat--history-search qq-chat--last-search-query nil)
    (message "qq: no previous match for %s" qq-chat--last-search-query)))

(defun qq-chat-render ()
  "Synchronize current chat frame and projected timeline from local state."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-view)
  (when (appkit-chatbuf-input-region-bounds)
    (appkit-chatbuf-input-state-sync :reset-history-p nil))
  (qq-chat--header-line-update)
  (qq-chat--update-frame)
  (qq-chat--sync-timeline))

(defun qq-chat-refresh ()
  "Refresh current chat contents from local state and NapCat history."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((gap-after (qq-chat--history-get :gap-after-id)))
    (qq-chat--reset-history-state)
    (when gap-after
      (qq-chat--set-history-gap gap-after)))
  (qq-chat-render)
  (qq-api-fetch-older-history qq-chat--session-key))

(defun qq-chat--note-history-window (meta)
  "Update newer-gap state after an around-window merge described by META."
  (let* ((session (qq-chat--session))
         (latest (or (alist-get 'read-latest-message-id session)
                     (alist-get 'last-message-id session)))
         (batch-ids (plist-get meta :batch-message-ids))
         (newest (plist-get meta :batch-newest-message-id)))
    (if (and latest newest (not (member latest batch-ids)))
        (qq-chat--set-history-gap newest)
      (qq-chat--set-history-gap nil))
    (qq-chat--update-frame)
    (qq-chat--sync-timeline)))

(defun qq-chat-load-newer-messages (&optional quiet)
  "Fill one page on the newer side of the current history gap."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((cursor (qq-chat--history-get :gap-after-id)))
    (cond
     ((not cursor)
      (unless quiet (message "qq: no newer history gap")))
     ((qq-chat--history-get :loading)
      (unless quiet (message "qq: history load already in progress")))
     (t
      (let ((session-key qq-chat--session-key)
            (buffer (current-buffer))
            (requested (max 1 qq-history-fetch-count))
            (point-anchor (and (not (appkit-chatbuf-point-in-input-p))
                               (get-text-property (point)
                                                  'qq-chat-message-anchor)))
            (point-anchor-offset 0))
        (when point-anchor
          (when-let* ((anchor-pos (qq-chat--message-position point-anchor)))
            (setq point-anchor-offset (- (point) anchor-pos))))
        (qq-chat--history-set :loading 'newer)
        (qq-api-fetch-history-page
         session-key cursor 'newer
         (lambda (meta)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (equal qq-chat--session-key session-key)
                 (qq-chat--history-set :loading nil)
                 (let* ((added (or (plist-get meta :added-count) 0))
                        (count (or (plist-get meta :message-count) 0))
                        (ids (plist-get meta :batch-message-ids))
                        (newest (plist-get meta :batch-newest-message-id))
                        (latest (or (alist-get 'read-latest-message-id
                                               (qq-chat--session))
                                    (alist-get 'last-message-id
                                               (qq-chat--session))))
                        (finished (or (<= count 1)
                                      (< count requested)
                                      (and latest (member latest ids))
                                      (equal newest cursor))))
                   (if finished
                       (qq-chat--set-history-gap nil)
                     (qq-chat--set-history-gap newest))
                   (qq-chat--update-frame)
                   (qq-chat--sync-timeline)
                   (when point-anchor
                     (when-let* ((anchor-pos (qq-chat--message-position point-anchor)))
                       (goto-char (+ anchor-pos point-anchor-offset))
                       (when-let* ((window (get-buffer-window buffer t)))
                         (set-window-point window (point)))))
                   (unless quiet
                     (if finished
                         (message "qq: newer history caught up")
                       (message "qq: loaded %d newer message%s"
                                added (if (= added 1) "" "s")))))))))
         (lambda (response reason)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (equal qq-chat--session-key session-key)
                 (qq-chat--history-set :loading nil)
                 (qq-chat--update-frame)
                 (qq-api--default-error response reason)))))
         requested))))))

(defun qq-chat-return-to-latest ()
  "Move point to the newest cached message without hiding a real history gap."
  (interactive)
  (let ((latest (or (alist-get 'read-latest-message-id (qq-chat--session))
                    (alist-get 'last-message-id (qq-chat--session)))))
    (if (and latest (qq-chat--goto-loaded-message latest nil))
        (message "qq: latest cached message")
      (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))))

(defun qq-chat-load-older-messages (&optional quiet)
  "Load one older history page for the current chat (telega/disco `M-<').

Uses the oldest cached snowflake `server-id' as NapCat `message_seq'.  Guards
against concurrent requests and marks history exhausted when NapCat returns no
new rows or reports the cursor missing."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   ((or qq-chat--history-exhausted (qq-chat--history-get :older-loaded))
    (unless quiet (message "qq: no older messages available")))
   ((qq-chat--history-get :loading)
    (unless quiet (message "qq: history load already in progress")))
   (t
    (let* ((session-key qq-chat--session-key)
           (before (qq-state-session-oldest-message-id session-key))
           (buffer (current-buffer)))
      (unless before
        (user-error "qq: no oldest message cursor; refresh first (C-c g)"))
      (qq-chat--history-set :loading 'older)
      (qq-api-fetch-older-history
       session-key
       before
       (lambda (meta)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (equal qq-chat--session-key session-key)
               (qq-chat--history-set :loading nil)
               (let ((added (or (plist-get meta :added-count) 0)))
                 (cond
                  ((<= added 0)
                   (qq-chat--history-set :older-loaded t)
                   (qq-chat--header-line-update)
                   (qq-chat--update-frame)
                   ;; Jump uses seek-at-target, not load-older chains.
                   (when qq-chat--pending-jump-id
                     (qq-chat--finish-jump-if-loaded qq-chat--pending-jump-id))
                   (unless (or quiet qq-chat--pending-jump-id)
                     (message "qq: reached beginning of history")))
                  (t
                   (qq-chat--header-line-update)
                   (qq-chat--update-frame)
                   (when qq-chat--pending-jump-id
                     (qq-chat--finish-jump-if-loaded qq-chat--pending-jump-id))
                   (unless (or quiet qq-chat--pending-jump-id)
                     (message "qq: loaded %d older message%s"
                              added
                              (if (= added 1) "" "s"))))))))))
       (lambda (response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (equal qq-chat--session-key session-key)
               (qq-chat--history-set :loading nil)
               (if (qq-api--history-exhausted-error-p response reason)
                   (progn
                     (qq-chat--history-set :older-loaded t)
                     (qq-chat--header-line-update)
                     (qq-chat--update-frame)
                     (when qq-chat--pending-jump-id
                       (qq-chat--finish-jump-if-loaded qq-chat--pending-jump-id))
                     (unless (or quiet qq-chat--pending-jump-id)
                       (message "qq: reached beginning of history")))
                 (qq-chat--header-line-update)
                 (qq-chat--update-frame)
                 (qq-api--default-error response reason)))))))))))

(defun qq-chat-send-message ()
  "Send current chat draft."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-composer-visible)
  (let* ((text (qq-chat--current-draft-string))
         (reply-message (qq-chat--reply-message))
         (reply-id (and reply-message
                        (alist-get 'server-id reply-message)))
         (content-segments (qq-chat--current-input-segments))
         (send-segments (append
                         (when reply-id
                           `(((type . "reply")
                              (data . ((id . ,(format "%s" reply-id)))))))
                         content-segments))
         (raw-message (unless (appkit-chatbuf-input-has-objects-p)
                        text)))
    (if (and (not (appkit-chatbuf-input-has-objects-p))
             (string-empty-p (string-trim text)))
        (message "qq: draft is empty")
      (qq-chat--push-input-history text)
      (appkit-chatbuf-input-state-clear :reset-history-p t)
      ;; telega: empty input after send → chatActionCancel
      (qq-chat--set-my-action 'cancel)
      (qq-chat--set-reply-message nil)
      (qq-chat--update-frame)
      (qq-api-send-message qq-chat--session-key send-segments raw-message))))

(defun qq-chat-return-dwim (arg)
  "Send current draft, or insert newline with prefix ARG."
  (interactive "P")
  (qq-chat--ensure-composer-visible)
  (if (not (appkit-chatbuf-point-in-input-p))
      (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (if arg
        (insert "\n")
      (qq-chat-send-message))))

(defun qq-chat-edit-draft ()
  "Move point to the editable draft area."
  (interactive)
  (qq-chat--ensure-composer-visible)
  (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))

(defun qq-chat-draft-prev ()
  "Replace draft with previous entry from input history."
  (interactive)
  (condition-case _err
      (progn
        (appkit-chatbuf-input-history-prev)
        (qq-chat--sync-draft-from-buffer)
        (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
    (user-error
     (user-error "qq: no previous inputs"))))

(defun qq-chat-draft-next ()
  "Move draft navigation toward more recent input history."
  (interactive)
  (condition-case _err
      (progn
        (appkit-chatbuf-input-history-next)
        (qq-chat--sync-draft-from-buffer)
        (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
    (user-error
     (user-error "qq: not currently browsing input history"))))

(defun qq-chat-clear-draft ()
  "Clear current draft and exit input history navigation."
  (interactive)
  (qq-chat--set-draft ""))

(defun qq-chat-reply-to-message ()
  "Reply to the message currently under point."
  (interactive)
  (qq-chat--set-pending-reply
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-delete-message ()
  "Recall the message currently under point."
  (interactive)
  (qq-chat--delete-message-internal
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-react-to-message (&optional face-id message)
  "Add QQ base FACE-ID as a reaction to MESSAGE at point.

Like telega's `!' action, interactive use opens the existing QQ face picker.
Clicking an existing reaction chip performs add/remove toggle instead."
  (interactive)
  (let* ((message (or message
                      (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (_reactable (or (qq-chat--message-reactable-p message)
                         (user-error "qq: reactions require a live group message")))
         (message-id (alist-get 'server-id message))
         (emoji-id (format "%s" (or face-id
                                    (qq-chat--read-base-face-id
                                     "React with QQ face: ")))))
    (qq-api-set-message-emoji-like
     message-id emoji-id t
     (lambda (_response)
       (message "qq: reaction added (%s)" emoji-id)))))

(defun qq-chat-open-resource-at-point ()
  "Open the exact media card at point, or the message's primary media."
  (interactive)
  (disco-media-card-open))

(defun qq-chat-open-avatar-at-point ()
  "Open sender avatar for the message at point."
  (interactive)
  (qq-media-open-message-avatar
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-open-user-at-point ()
  "Open the sender user page for the message at point."
  (interactive)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (user-id (alist-get 'sender-id message)))
    (unless (and (qq-api-user-id-p user-id) (not (equal user-id "0")))
      (user-error "qq: message sender has no user profile"))
    (qq-user-open user-id)))

(defun qq-chat-open-peer-user ()
  "Open the current private peer's user page."
  (interactive)
  (let* ((session (qq-state-session qq-chat--session-key))
         (type (alist-get 'type session))
         (user-id (or (alist-get 'peer-uin session)
                      (alist-get 'target-id session))))
    (unless (and (eq type 'private) (qq-api-user-id-p user-id))
      (user-error "qq: current chat has no user profile"))
    (qq-user-open user-id)))

(defun qq-chat-open-peer-info ()
  "Open the current private user or group profile page."
  (interactive)
  (let* ((session (or (qq-state-session qq-chat--session-key)
                      (user-error "qq: current chat has no session")))
         (type (alist-get 'type session))
         (target-id (or (and (eq type 'private) (alist-get 'peer-uin session))
                        (alist-get 'target-id session))))
    (pcase type
      ('private
       (unless (qq-api-user-id-p target-id)
         (user-error "qq: current chat has no user profile"))
       (qq-user-open target-id))
      ('group
       (unless (qq-api-group-id-p target-id)
         (user-error "qq: current chat has no group profile"))
       (qq-group-open target-id))
      (_ (user-error "qq: current chat has no profile page")))))

(defun qq-chat-cancel-dwim ()
  "Cancel reply context, or clear draft when no reply is pending.

Bound to `C-c C-k' (also ESC ESC / C-M-c).  Reply footer × is clickable."
  (interactive)
  (cond
   ((qq-chat--reply-message)
    (qq-chat--set-reply-message nil)
    (qq-chat--update-frame)
    (message "qq: reply target cleared"))
   ((or (appkit-chatbuf-input-history-active-p)
        (not (string-empty-p (string-trim (qq-chat--current-draft-string)))))
    (qq-chat-clear-draft)
    (message "qq: draft cleared"))
   (t
    (message "qq: nothing to cancel"))))

(defun qq-chat--apply-read-state-change ()
  "Synchronize unread-divider context after session read-state changes."
  (qq-chat--header-line-update)
  (qq-chat--sync-timeline)
  t)

(defun qq-chat-read-all ()
  "Mark the current chat as read."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-api-mark-session-read qq-chat--session-key))

(defun qq-chat-send-poke (&optional target-id)
  "Send a poke in the current chat.

For a private chat, poke the peer.  In a group, use the message at point as
the default target and prompt for a QQ number when point has no incoming
sender."
  (interactive)
  (let* ((session (qq-chat--session))
         (type (and session (alist-get 'type session)))
         (message (ignore-errors (qq-chat--message-at-point)))
         (message-target
          (and message
               (or (alist-get 'target-id message)
                   (and (not (alist-get 'self-p message))
                        (alist-get 'sender-id message)))))
         (target
          (if (eq type 'group)
              (or target-id message-target (read-string "Poke QQ: "))
            target-id)))
    (unless qq-chat--session-key
      (user-error "qq: this buffer is not bound to a session"))
    (when (and (eq type 'group)
               (string-empty-p (string-trim (or target ""))))
      (user-error "qq: group poke requires a target QQ"))
    (qq-api-send-poke
     qq-chat--session-key
     target
     (lambda (_response)
       (message "qq: poke sent")))))

(defvar qq-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c g") #'qq-chat-refresh)
    (define-key map (kbd "C-c /") #'qq-chat-search)
    (define-key map (kbd "C-c C-n") #'qq-chat-search-next)
    (define-key map (kbd "C-c C-p") #'qq-chat-search-prev)
    (define-key map (kbd "C-c r") #'qq-chat-read-all)
    (define-key map (kbd "C-c P") #'qq-chat-send-poke)
    (define-key map (kbd "C-c m") #'qq-chat-message-transient)
    (define-key map (kbd "C-c f") #'qq-chat-forward-message)
    (define-key map (kbd "C-c M") #'qq-chat-toggle-forward-mark)
    (define-key map (kbd "C-c F") #'qq-chat-forward-marked-messages)
    (define-key map (kbd "C-c i") #'qq-chat-open-peer-info)
    (define-key map (kbd "M-<") #'qq-chat-load-older-messages)
    (define-key map (kbd "M->") #'qq-chat-return-to-latest)
    (define-key map (kbd "RET") #'qq-chat-return-dwim)
    (define-key map (kbd "TAB") #'qq-chat-edit-draft)
    (define-key map (kbd "C-c '") #'qq-chat-edit-draft)
    (define-key map (kbd "M-p") #'qq-chat-draft-prev)
    (define-key map (kbd "M-n") #'qq-chat-draft-next)
    (define-key map (kbd "C-c C-f") #'qq-chat-attach-file)
    (define-key map (kbd "C-c C-v") #'qq-chat-attach-clipboard)
    (define-key map (kbd "C-c C-e") #'qq-chat-attach-emoji)
    (define-key map (kbd "C-c C-a") #'qq-chat-attach-transient)
    (define-key map (kbd "M-g n") #'qq-chat-next-message)
    (define-key map (kbd "M-g p") #'qq-chat-previous-message)
    (define-key map (kbd "C-c C-c") #'qq-chat-send-message)
    (define-key map (kbd "C-c C-k") #'qq-chat-cancel-dwim)
    ;; telega: ESC ESC / C-M-c also cancel reply-or-edit aux.
    (define-key map (kbd "\e\e") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-M-c") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-c ?") #'qq-chat-transient)
    map)
  "Keymap for `qq-chat-mode'.")

(define-derived-mode qq-chat-mode nil "QQ-Chat"
  "Major mode for emacs-qq chat buffers.

Message actions use point + keys (`r'/`d'/`!'/`o'/`a' on the timeline) or
`qq-chat-message-transient' (`C-c m' / timeline `m').  Chat-wide commands
  are in `qq-chat-transient' (`C-c ?' / timeline `?').
Attach from clipboard with `C-c C-v' (telega-style)."
  (appkit-chatbuf-mode-setup)
  (appkit-chatbuf-reset-state 32)
  (setq-local qq-chat--last-search-query nil)
  (setq-local qq-chat--marked-message-anchors nil)
  (setq-local qq-chat--forward-request-active-p nil)
  (setq-local qq-chat--fill-column nil)
  (setq-local disco-media-card-fallback-context-function
              #'qq-chat--media-card-fallback-context)
  (qq-chat--reset-history-state)
  (setq-local qq-chat--pending-jump-id nil)
  (setq-local qq-chat--messages-pop-ring
              (make-ring (max 1 qq-chat-messages-pop-ring-size)))
  ;; telega-style: M-x yank-media also drops images into the composer.
  (when (fboundp 'yank-media-handler)
    (funcall #'yank-media-handler
             '(image/png image/jpeg image/bmp)
             (lambda (mime-type data)
               (qq-chat--yank-media mime-type data nil))))
  (add-hook 'after-change-functions #'qq-chat--after-change nil t)
  (add-hook 'post-command-hook #'qq-chat--post-command nil t)
  (add-hook 'window-size-change-functions
            #'qq-chat--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook
            #'qq-chat--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'qq-chat--on-text-scale-change nil t)
  (qq-chat--update-context-mode))

(defun qq-chat--initial-history-request-current-p
    (buffer session-key owner)
  "Return non-nil when OWNER still owns BUFFER and SESSION-KEY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--initial-history-owner owner)))))

(defun qq-chat--complete-initial-history-load
    (buffer session-key owner &optional target meta)
  "Finish initial history load for BUFFER and SESSION-KEY.

When TARGET is non-nil, META describes an around window centered at the exact
first unread message."
  (when (qq-chat--initial-history-request-current-p
         buffer session-key owner)
    (with-current-buffer buffer
      (setq qq-chat--initial-history-owner nil
            qq-chat--initial-history-request nil)
      (when (and target meta)
        (qq-chat--note-history-window meta)
        (qq-chat--goto-loaded-message target nil))
      (when qq-auto-mark-read
        (qq-api-mark-session-read session-key)))))

(defun qq-chat--fail-initial-history-load
    (buffer session-key owner response reason)
  "Finish OWNER with an explicit initial-history failure."
  (when (qq-chat--initial-history-request-current-p
         buffer session-key owner)
    (with-current-buffer buffer
      (setq qq-chat--initial-history-owner nil
            qq-chat--initial-history-request nil))
    (qq-api--default-error response reason)))

(defun qq-chat--load-latest-initial-history (buffer session-key owner)
  "Load latest history for BUFFER, then complete read handling."
  (let ((request
         (qq-api-fetch-older-history
          session-key nil
          (lambda (_meta)
            (qq-chat--complete-initial-history-load
             buffer session-key owner))
          (lambda (response reason)
            (when (qq-chat--initial-history-request-current-p
                   buffer session-key owner)
              (qq-chat--complete-initial-history-load
               buffer session-key owner)
              (qq-api--default-error response reason))))))
    (when (qq-chat--initial-history-request-current-p
           buffer session-key owner)
      (with-current-buffer buffer
        (setq qq-chat--initial-history-request request)))
    request))

(defun qq-chat--load-initial-history (buffer session-key)
  "Load SESSION-KEY around its official QQ read position when available."
  (let ((owner (list 'initial-history session-key)))
    (with-current-buffer buffer
      (when qq-chat--initial-history-request
        (qq-api-cancel-request qq-chat--initial-history-request))
      (setq qq-chat--initial-history-owner owner
            qq-chat--initial-history-request nil))
    (let ((request
           (qq-api-fetch-session-read-state
            session-key
            (lambda (read-state)
              (when (qq-chat--initial-history-request-current-p
                     buffer session-key owner)
         (let* ((unread (or (alist-get 'unread_count read-state) 0))
                (first (alist-get 'first_unread read-state))
                (first-id (and (listp first)
                               (alist-get 'message_id first))))
           (if (and (> unread 0) first-id)
               (qq-api-fetch-history-around
                session-key first-id
                (lambda (meta)
                  (qq-chat--complete-initial-history-load
                   buffer session-key owner first-id meta))
                (lambda (response reason)
                  (qq-chat--fail-initial-history-load
                   buffer session-key owner response reason))
                (max qq-history-fetch-count (* 2 qq-history-fetch-count)))
             (qq-chat--load-latest-initial-history
              buffer session-key owner)))))
            (lambda (response reason)
              (qq-chat--fail-initial-history-load
               buffer session-key owner response reason)))))
      (when (qq-chat--initial-history-request-current-p
             buffer session-key owner)
        (with-current-buffer buffer
          (setq qq-chat--initial-history-request request)))
      request)))

(defun qq-chat-open (session-key)
  "Open chat for SESSION-KEY."
  (interactive)
  (unless session-key
    (user-error "qq: session key is required"))
  (qq-state-upsert-session session-key nil nil)
  (let* ((view
          (appkit-open-view
           :app (qq-runtime-app)
           :id (list 'chat session-key)
           :mode 'qq-chat-mode
           :buffer-name (qq-chat--buffer-name session-key)
           :state session-key
           :sync-function #'qq-chat--sync-invalidations
           :parts '(frame timeline composer)))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq qq-chat--session-key session-key)
      (qq-chat-render)
      (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
      (qq-chat--load-initial-history buffer session-key))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (qq-chat--on-window-size-change))))

(defun qq-chat--rerender-open-chats (&optional media-key)
  "Invalidate open chat rows affected by MEDIA-KEY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'qq-chat-mode)
                 (appkit-view-live-p (appkit-current-view)))
        (let ((view (appkit-current-view)))
          (if media-key
              (appkit-invalidate view :resource (list :media media-key))
            (appkit-invalidate view :entries (appkit-chat-timeline-keys)))
          (appkit-schedule-sync view))))))

(defun qq-chat--message-event-rekeys (event)
  "Return explicit local-to-server row rekeys described by EVENT."
  (let* ((message (plist-get event :message))
         (new-key (and (listp message) (qq-chat--message-anchor message)))
         (old-keys
          (delete-dups
           (delq nil
                 (list (plist-get event :previous-anchor)
                       (and (listp message) (alist-get 'local-id message))))))
         (old-key
          (seq-find (lambda (key)
                      (and (not (equal key new-key))
                           (appkit-chat-timeline-node key)))
                    old-keys)))
    (and old-key new-key (list (cons old-key new-key)))))

(defun qq-chat--apply-message-state-change (event)
  "Synchronize one QQ message EVENT through the canonical projection."
  (let* ((message (plist-get event :message))
         (anchor (or (plist-get event :message-anchor)
                     (and (listp message) (qq-chat--message-anchor message))))
         (previous-anchor (plist-get event :previous-anchor))
         (resources
          (mapcar (lambda (key) (list :message key))
                  (delete-dups (delq nil (list anchor previous-anchor)))))
         (rekeys (qq-chat--message-event-rekeys event)))
    (unless (or anchor previous-anchor)
      (error "qq: message state event has no stable anchor: %S" event))
    (qq-chat--header-line-update)
    (qq-chat--sync-timeline
     :changed-resources resources
     :rekeys rekeys)
    (when (or (qq-chat--message-affects-composer-context-p anchor)
              (qq-chat--message-affects-composer-context-p previous-anchor))
      (qq-chat--refresh-composer-context-for-message
       (or anchor previous-anchor)))))

(defun qq-chat--handle-state-change (event)
  "Queue state EVENT invalidations for open QQ chats."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type)))
    (when (memq event-type '(message history reset session action
                             sessions-refreshed friends-refreshed groups-refreshed))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and (derived-mode-p 'qq-chat-mode)
                     (appkit-view-live-p (appkit-current-view))
                     (or (memq event-type
                               '(reset sessions-refreshed
                                 friends-refreshed groups-refreshed))
                         (equal event-session-key qq-chat--session-key)))
            (let ((view (appkit-current-view)))
              (appkit-view-enqueue-event view event)
              (appkit-invalidate
               view :part (if (memq event-type '(session action sessions-refreshed))
                              'frame
                            'timeline))
              (appkit-schedule-sync view))))))))

(add-hook 'qq-media-cache-update-hook #'qq-chat--rerender-open-chats)
(add-hook 'qq-state-change-hook #'qq-chat--handle-state-change)

(provide 'qq-chat)

;;; qq-chat.el ends here
