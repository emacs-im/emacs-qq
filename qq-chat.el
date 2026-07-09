;;; qq-chat.el --- Chat buffers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Chat buffer modeled after disco.el's room view, while borrowing
;; telega.el-style naming and the most familiar chat input bindings.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'subr-x)
(require 'ewoc)
(require 'disco-chatbuf)
(require 'disco-media)
(require 'qq-api)
(require 'qq-customize)
(require 'qq-media)
(require 'qq-state)
(require 'qq-ui)
(require 'qq-view)

(defvar-local qq-chat--session-key nil
  "Session key associated with the current chat buffer.")

(defvar-local qq-chat--draft-input ""
  "Current unsent draft for the chat buffer.")

(defvar-local qq-chat--draft-input-rich ""
  "Current unsent draft for the chat buffer, preserving input object properties.")

(defvaralias 'qq-chat--input-marker 'disco-chatbuf--input-marker
  "Marker for the beginning of the editable input region.")

(defvaralias 'qq-chat--input-prompt-marker 'disco-chatbuf--prompt-marker
  "Marker for the beginning of the visible input prompt.")

(defvaralias 'qq-chat--input-ring 'disco-chatbuf--input-ring
  "Ring containing previously sent inputs.")

(defvaralias 'qq-chat--input-index 'disco-chatbuf--input-idx
  "Current index inside `qq-chat--input-ring', or nil when editing draft.")

(defvaralias 'qq-chat--input-pending 'disco-chatbuf--input-pending
  "Draft remembered before entering input history navigation.")

(defvar-local qq-chat--last-search-query nil
  "Last in-chat search query.")

(defvar qq-chat-group-messages t
  "When non-nil, compact consecutive messages from the same sender.")

(defvar qq-chat-group-messages-timespan 300
  "Maximum time gap in seconds used for compact message grouping.")

(defvar-local qq-chat--rendering nil
  "Non-nil while the chat buffer is being rendered.")

(defconst qq-chat--empty-placeholder :qq-chat-empty-placeholder
  "Sentinel EWOC payload used for the empty timeline note.")

(defvar-local qq-chat--ewoc nil
  "Persistent EWOC backing the current chat timeline.")

(defvar-local qq-chat--empty-node nil
  "EWOC node used for the empty timeline placeholder, or nil.")

(defvar-local qq-chat--message-node-table nil
  "Hash table of rendered message nodes keyed by message anchor.")

(defvar-local qq-chat--displayed-message-anchors nil
  "Rendered message anchors in timeline order.")

(defvar-local qq-chat--render-context-by-anchor nil
  "Render context plist table keyed by message anchor.")

(defvar-local qq-chat--deferred-node-anchors nil
  "Message anchors whose redisplay is deferred while a region is active.")

(defvar-local qq-chat--media-anchors-by-key nil
  "Hash table mapping media cache keys to affected message anchors.")

(defvar qq-chat-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Timeline-only keymap active when point is outside the draft region.")

(defvar qq-chat-message-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'qq-chat-reply-to-message)
    (define-key map (kbd "d") #'qq-chat-delete-message)
    (define-key map (kbd "o") #'qq-chat-open-resource-at-point)
    (define-key map (kbd "a") #'qq-chat-open-avatar-at-point)
    map)
  "Prefix map for message actions at point in `qq-chat-mode'.")

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
  "Return dynamic header line for the active chat buffer."
  (let* ((session (qq-chat--session))
         (title (or (alist-get 'title session) qq-chat--session-key))
         (target-id (alist-get 'target-id session))
         (status (qq-state-connection-status))
         (unread (or (alist-get 'unread-count session) 0)))
    (format " %s  [%s]  target:%s%s"
            title
            status
            target-id
            (if (> unread 0)
                (format "  unread:%d" unread)
              ""))))

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
         (action (lambda ()
                   (qq-media-open-message-avatar message)))
         (help-echo "Open sender avatar")
         (properties '(read-only t front-sticky t rear-nonsticky (read-only))))
    (qq-ui-insert-action-button
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
      (qq-ui-insert-action-button
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

(defun qq-chat--message-has-block-segments-p (message)
  "Return non-nil when MESSAGE contains block-like render segments."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (qq-chat--media-segment-p segment)
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
    ('pending " [pending]")
    ('failed (format " [failed: %s]" (or (alist-get 'error message) "send failed")))
    ('recalled " [recalled]")
    (_ "")))

(defun qq-chat--message-body (message)
  "Return body text for MESSAGE."
  (if (eq (alist-get 'status message) 'recalled)
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
        'read-only t
        'front-sticky '(read-only)
        'rear-nonsticky '(read-only)
        'mouse-face 'highlight))

(defun qq-chat--message-at-point ()
  "Return message object under point in the current chat buffer, or nil."
  (let* ((anchor (or (get-text-property (point) 'qq-chat-message-anchor)
                     (get-text-property (line-beginning-position) 'qq-chat-message-anchor)))
         (messages (qq-state-session-messages qq-chat--session-key)))
    (seq-find
     (lambda (message)
       (equal (qq-chat--message-anchor message) anchor))
     messages)))

(defun qq-chat--message-positions ()
  "Return list of message start positions in the current buffer."
  (let ((pos (point-min))
        result)
    (while (and pos (< pos (point-max)))
      (setq pos (text-property-not-all pos (point-max) 'qq-chat-message-anchor nil))
      (when pos
        (push pos result)
        (setq pos (next-single-property-change pos 'qq-chat-message-anchor nil (point-max)))))
    (nreverse result)))

(defun qq-chat--current-message-position ()
  "Return the start position of the message at point, or nil."
  (let ((anchor (or (get-text-property (point) 'qq-chat-message-anchor)
                    (get-text-property (line-beginning-position) 'qq-chat-message-anchor))))
    (and anchor
         (text-property-any (point-min) (point-max) 'qq-chat-message-anchor anchor))))

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

(defun qq-chat--input-region-bounds ()
  "Return current writable draft region as (START . END), or nil."
  (disco-chatbuf-input-region-bounds))

(defun qq-chat--input-start-position ()
  "Return draft input start position, or nil."
  (disco-chatbuf-input-start-position))

(defun qq-chat--input-prompt-start-position ()
  "Return prompt start position preceding current draft input, or nil."
  (disco-chatbuf-prompt-start-position))

(defun qq-chat--input-logical-end-position ()
  "Return logical draft end position for the tail input region."
  (disco-chatbuf-input-logical-end-position))

(defun qq-chat--point-in-input-p (&optional position)
  "Return non-nil when POSITION or point is inside the draft region."
  (disco-chatbuf-point-in-input-p position))

(defun qq-chat--point-in-prompt-p (&optional position)
  "Return non-nil when POSITION or point is inside the prompt glyph span."
  (disco-chatbuf-point-in-prompt-p position))

(defun qq-chat--apply-input-text-properties ()
  "Normalize current draft text properties after redraws and edits."
  (disco-chatbuf-input-apply-text-properties))

(defun qq-chat--current-draft-string ()
  "Return current draft text from the chat buffer."
  (if (and (not qq-chat--rendering)
           (qq-chat--input-region-bounds))
      (substring-no-properties (or (disco-chatbuf-input-string) ""))
    qq-chat--draft-input))

(defun qq-chat--capture-draft ()
  "Capture current editable draft into local draft state."
  (setq qq-chat--draft-input (qq-chat--current-draft-string))
  (setq qq-chat--draft-input-rich (or (disco-chatbuf-input-string) qq-chat--draft-input)))

(defun qq-chat--sync-draft-from-buffer ()
  "Sync draft state from the editable buffer region."
  (let* ((rich (or (disco-chatbuf-input-string) ""))
         (text (substring-no-properties rich)))
    (setq qq-chat--draft-input-rich rich)
    (unless (equal text qq-chat--draft-input)
      (setq qq-chat--draft-input text)
      (setq qq-chat--input-index nil)
      (setq qq-chat--input-pending nil))))

(defun qq-chat--reply-message ()
  "Return current reply target message from shared aux state, or nil."
  (and (eq (plist-get disco-chatbuf--aux-plist :aux-type) 'reply)
       (plist-get disco-chatbuf--aux-plist :aux-msg)))

(defun qq-chat--set-reply-message (message)
  "Set shared reply aux state to MESSAGE, or clear it when nil."
  (if message
      (disco-chatbuf-aux-set
       (list :aux-type 'reply
             :aux-msg message
             :message-id (alist-get 'server-id message)))
    (disco-chatbuf-aux-reset)))

(defun qq-chat--after-change (beg end _old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (disco-chatbuf-after-change
   beg end
   :rendering-p qq-chat--rendering
   :sync-function #'qq-chat--sync-draft-from-buffer
   :prune-broken-objects t))

(defun qq-chat--update-context-mode ()
  "Enable timeline bindings only when point is outside the draft input."
  (let ((timeline-p (not (qq-chat--point-in-input-p))))
    (unless (eq qq-chat-timeline-mode timeline-p)
      (qq-chat-timeline-mode (if timeline-p 1 -1)))))

(defun qq-chat--flush-deferred-node-redisplay ()
  "Flush any node redisplay deferred while a region was active."
  (when (and qq-chat--deferred-node-anchors
             (not mark-active))
    (let ((anchors (prog1 qq-chat--deferred-node-anchors
                     (setq qq-chat--deferred-node-anchors nil))))
      (qq-chat--invalidate-message-anchors-preserving-point anchors))))

(defun qq-chat--post-command ()
  "Keep point inside the logical draft area when editing input."
  (unless qq-chat--rendering
    (disco-chatbuf-post-command-clamp-point)
    (qq-chat--flush-deferred-node-redisplay)
    (qq-chat--update-context-mode)))

(defun qq-chat--capture-window-input-offsets ()
  "Return (WINDOW . OFFSET) pairs for windows currently in draft input."
  (let ((bounds (qq-chat--input-region-bounds))
        offsets)
    (when bounds
      (let ((start (car bounds))
            (end (cdr bounds)))
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (let ((window-point (window-point win)))
            (when (and (<= start window-point)
                       (<= window-point end))
              (push (cons win (- window-point start)) offsets))))))
    offsets))

(defun qq-chat--restore-window-input-offsets (offsets)
  "Restore window points in OFFSETS relative to current draft input start."
  (let ((start (qq-chat--input-start-position))
        (logical-end (qq-chat--input-logical-end-position)))
    (when (and (number-or-marker-p start)
               (number-or-marker-p logical-end))
      (dolist (entry offsets)
        (let ((win (car entry))
              (offset (cdr entry)))
          (when (and (window-live-p win)
                     (eq (window-buffer win) (current-buffer)))
            (set-window-point
             win
             (min logical-end
                  (max start (+ start offset))))))))))

(defun qq-chat--prompt-text ()
  "Return visible prompt text for the current chat buffer."
  ">>> ")

(defun qq-chat--clear-input-region-markers ()
  "Detach current chat composer markers during hard resets only."
  (disco-chatbuf-clear-prompt-and-input))

(defun qq-chat--bind-input-region-from-footer ()
  "Ensure the persistent tail input region exists and matches current draft."
  (disco-chatbuf-init-state 32)
  (let ((target-input (or qq-chat--draft-input-rich qq-chat--draft-input "")))
    (save-excursion
      (goto-char (point-max))
      (if (disco-chatbuf-prompt-button-live-p)
          (disco-chatbuf-prompt-update (qq-chat--prompt-text))
        (disco-chatbuf-install-prompt (qq-chat--prompt-text))))
    (qq-chat--apply-input-text-properties)
    (unless (equal (or (disco-chatbuf-input-string) "") target-input)
      (disco-chatbuf-input-set-text target-input)
      (qq-chat--apply-input-text-properties))))

(defun qq-chat--header-text ()
  "Build EWOC header text for the current chat state."
  (let* ((session (qq-chat--session))
         (title (or (alist-get 'title session) qq-chat--session-key))
         (text
          (with-temp-buffer
            (qq-view-insert-heading-line title)
            (qq-view-insert-note-line (qq-chat--header-help-text))
            (insert "\n")
            (buffer-string))))
    (add-text-properties
     0 (length text)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only))
     text)
    text))

(defun qq-chat--input-footer-context-text ()
  "Return dynamic footer text shown above the composer prompt."
  (let ((reply-context (qq-chat--reply-context-text)))
    (concat
     "\n"
     (if (string-empty-p reply-context)
         ""
       (propertize reply-context 'face 'font-lock-doc-face)))))

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

(defun qq-chat--message-render-context (message)
  "Return render context plist for MESSAGE, or nil when missing."
  (let ((anchor (qq-chat--message-anchor message)))
    (and anchor
         qq-chat--render-context-by-anchor
         (gethash anchor qq-chat--render-context-by-anchor))))

(defun qq-chat--compute-message-render-context (previous-message message)
  "Return render context for MESSAGE given PREVIOUS-MESSAGE."
  (let* ((day-key (qq-chat--message-day-key message))
         (previous-day-key (and previous-message
                                (qq-chat--message-day-key previous-message)))
         (insert-date (and day-key
                           (not (equal day-key previous-day-key))
                           day-key))
         (compact (and previous-message
                       (qq-chat--messages-compact-group-p previous-message message))))
    (list :compact (and compact t)
          :insert-date insert-date)))

(defun qq-chat--rebuild-render-contexts (messages)
  "Recompute full render contexts for MESSAGES in display order."
  (unless (hash-table-p qq-chat--render-context-by-anchor)
    (setq qq-chat--render-context-by-anchor (make-hash-table :test #'equal)))
  (clrhash qq-chat--render-context-by-anchor)
  (let ((previous-message nil))
    (dolist (message messages)
      (let ((anchor (qq-chat--message-anchor message)))
        (when anchor
          (puthash anchor
                   (qq-chat--compute-message-render-context previous-message message)
                   qq-chat--render-context-by-anchor)))
      (setq previous-message message))))

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

(defun qq-chat--rebuild-media-anchor-index (messages)
  "Recompute media cache key -> message anchor index for MESSAGES."
  (unless (hash-table-p qq-chat--media-anchors-by-key)
    (setq qq-chat--media-anchors-by-key (make-hash-table :test #'equal)))
  (clrhash qq-chat--media-anchors-by-key)
  (dolist (message messages)
    (when-let* ((anchor (qq-chat--message-anchor message)))
      (dolist (key (qq-chat--message-media-cache-keys message))
        (puthash key
                 (cons anchor (gethash key qq-chat--media-anchors-by-key))
                 qq-chat--media-anchors-by-key)))))

(defun qq-chat--media-anchors-for-key (media-key)
  "Return rendered message anchors affected by MEDIA-KEY in current buffer."
  (when (and (stringp media-key)
             (hash-table-p qq-chat--media-anchors-by-key))
    (delete-dups (copy-sequence (gethash media-key qq-chat--media-anchors-by-key)))))

(defun qq-chat--message-anchor-list (messages)
  "Return message anchors in display order, skipping missing anchors."
  (delq nil (mapcar #'qq-chat--message-anchor (or messages '()))))

(defun qq-chat--timeline-order-compatible-p (current-anchors target-anchors)
  "Return non-nil when CURRENT-ANCHORS can reconcile into TARGET-ANCHORS."
  (let ((current-set (make-hash-table :test #'equal))
        common-current
        common-target)
    (dolist (anchor current-anchors)
      (puthash anchor t current-set))
    (dolist (anchor current-anchors)
      (when (member anchor target-anchors)
        (push anchor common-current)))
    (dolist (anchor target-anchors)
      (when (gethash anchor current-set)
        (push anchor common-target)))
    (equal (nreverse common-current)
           (nreverse common-target))))

(defun qq-chat--neighbor-anchors-in-sequence (anchors target-anchor)
  "Return prev/current/next anchors around TARGET-ANCHOR within ANCHORS."
  (when-let* ((index (cl-position target-anchor anchors :test #'equal)))
    (delq nil
          (list (nth (1- index) anchors)
                (nth index anchors)
                (nth (1+ index) anchors)))))

(defun qq-chat--message-neighborhood-anchors (current-anchors target-anchors anchor)
  "Return anchors affected by ANCHOR moving from CURRENT-ANCHORS to TARGET-ANCHORS."
  (delete-dups
   (append (qq-chat--neighbor-anchors-in-sequence current-anchors anchor)
           (qq-chat--neighbor-anchors-in-sequence target-anchors anchor))))

(defun qq-chat--recompute-render-contexts-for-anchors (messages anchors)
  "Recompute render contexts in MESSAGES only for ANCHORS."
  (unless (hash-table-p qq-chat--render-context-by-anchor)
    (setq qq-chat--render-context-by-anchor (make-hash-table :test #'equal)))
  (let ((target-set (make-hash-table :test #'equal))
        (previous-message nil)
        seen)
    (dolist (anchor anchors)
      (when anchor
        (puthash anchor t target-set)))
    (dolist (message messages)
      (let ((anchor (qq-chat--message-anchor message)))
        (when (and anchor (gethash anchor target-set))
          (puthash anchor
                   (qq-chat--compute-message-render-context previous-message message)
                   qq-chat--render-context-by-anchor)
          (push anchor seen))
        (setq previous-message message)))
    (dolist (anchor anchors)
      (when (and anchor (not (member anchor seen)))
        (remhash anchor qq-chat--render-context-by-anchor)))))

(defun qq-chat--copy-render-contexts-for-anchors (anchors)
  "Return alist snapshot of current render contexts for ANCHORS."
  (let (snapshot)
    (dolist (anchor anchors)
      (when anchor
        (push (cons anchor
                    (copy-tree (gethash anchor qq-chat--render-context-by-anchor)))
              snapshot)))
    snapshot))

(defun qq-chat--changed-render-context-anchors (anchors old-snapshot)
  "Return anchors in ANCHORS whose render context changed from OLD-SNAPSHOT."
  (let (changed)
    (dolist (anchor anchors)
      (when anchor
        (let ((previous (alist-get anchor old-snapshot nil nil #'equal))
              (current (gethash anchor qq-chat--render-context-by-anchor)))
          (unless (equal previous current)
            (push anchor changed)))))
    (nreverse changed)))

(defun qq-chat--insert-message-node-before (message before-node)
  "Insert MESSAGE before BEFORE-NODE, or at end when BEFORE-NODE is nil."
  (when qq-chat--ewoc
    (let ((qq-chat--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (let* ((node (if before-node
                       (ewoc-enter-before qq-chat--ewoc before-node message)
                     (ewoc-enter-last qq-chat--ewoc message)))
             (anchor (qq-chat--message-anchor message)))
        (when (and node anchor qq-chat--message-node-table)
          (puthash anchor node qq-chat--message-node-table))
        node))))

(defun qq-chat--delete-message-node (anchor)
  "Delete EWOC node identified by ANCHOR.

Return non-nil when a node is removed."
  (let ((node (and anchor
                   qq-chat--message-node-table
                   (gethash anchor qq-chat--message-node-table))))
    (when (and node qq-chat--ewoc)
      (let ((qq-chat--rendering t)
            (inhibit-read-only t)
            (buffer-undo-list t))
        (ewoc-delete qq-chat--ewoc node)
        (remhash anchor qq-chat--message-node-table)
        t))))

(defun qq-chat--ensure-ewoc ()
  "Ensure current chat buffer owns one persistent EWOC instance."
  (unless (hash-table-p qq-chat--message-node-table)
    (setq qq-chat--message-node-table (make-hash-table :test #'equal)))
  (unless (hash-table-p qq-chat--render-context-by-anchor)
    (setq qq-chat--render-context-by-anchor (make-hash-table :test #'equal)))
  (unless (hash-table-p qq-chat--media-anchors-by-key)
    (setq qq-chat--media-anchors-by-key (make-hash-table :test #'equal)))
  (unless qq-chat--ewoc
    (let ((qq-chat--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (erase-buffer)
      (setq qq-chat--displayed-message-anchors nil)
      (setq qq-chat--empty-node nil)
      (setq qq-chat--ewoc
            (ewoc-create #'qq-chat--ewoc-printer
                         (qq-chat--header-text)
                         (qq-chat--footer-text)
                         t)))))

(defun qq-chat--header-line-update ()
  "Update chat header line and buffer name in telega-like fashion."
  (qq-chat--ensure-buffer-name)
  (setq-local header-line-format '(:eval (qq-chat--header-line)))
  (force-mode-line-update))

(defun qq-chat--apply-frame-update ()
  "Update current chat EWOC header/footer in place and rebind composer."
  (qq-chat--ensure-ewoc)
  (let ((qq-chat--rendering t)
        (inhibit-read-only t)
        (buffer-undo-list t))
    (qq-chat--clear-input-region-markers)
    (ewoc-set-hf qq-chat--ewoc
                 (qq-chat--header-text)
                 (qq-chat--footer-text)))
  (qq-chat--bind-input-region-from-footer))

(defun qq-chat--prompt-update ()
  "Update the persistent tail prompt/input without touching the timeline."
  (qq-chat--ensure-ewoc)
  (qq-chat--bind-input-region-from-footer))

(defun qq-chat--clear-empty-node ()
  "Remove the empty timeline placeholder node, if present."
  (when (and qq-chat--ewoc qq-chat--empty-node)
    (let ((qq-chat--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (ewoc-delete qq-chat--ewoc qq-chat--empty-node))
    (setq qq-chat--empty-node nil)))

(defun qq-chat--ensure-empty-node ()
  "Ensure the empty timeline placeholder node exists."
  (qq-chat--ensure-ewoc)
  (unless qq-chat--empty-node
    (let ((qq-chat--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (setq qq-chat--empty-node
            (ewoc-enter-last qq-chat--ewoc qq-chat--empty-placeholder)))))

(defun qq-chat--clear-timeline ()
  "Remove all currently rendered message nodes from the chat EWOC."
  (qq-chat--ensure-ewoc)
  (let ((qq-chat--rendering t)
        (inhibit-read-only t)
        (buffer-undo-list t))
    (ewoc-filter qq-chat--ewoc (lambda (_message) nil)))
  (setq qq-chat--displayed-message-anchors nil)
  (setq qq-chat--empty-node nil)
  (clrhash qq-chat--message-node-table)
  (clrhash qq-chat--render-context-by-anchor)
  (when (hash-table-p qq-chat--media-anchors-by-key)
    (clrhash qq-chat--media-anchors-by-key)))

(defun qq-chat--reconcile-timeline (messages)
  "Reconcile current chat EWOC nodes against MESSAGES in display order."
  (qq-chat--ensure-ewoc)
  (let* ((target-anchors (qq-chat--message-anchor-list messages))
         (current-anchors (or qq-chat--displayed-message-anchors '()))
         (context-anchors (delete-dups (append (copy-sequence current-anchors)
                                               (copy-sequence target-anchors))))
         (context-snapshot (qq-chat--copy-render-contexts-for-anchors context-anchors))
         (target-set (make-hash-table :test #'equal))
         touched-anchors)
    (when target-anchors
      (qq-chat--clear-empty-node))
    (dolist (anchor target-anchors)
      (puthash anchor t target-set))
    (unless (qq-chat--timeline-order-compatible-p current-anchors target-anchors)
      (qq-chat--clear-timeline)
      (setq current-anchors '()))
    (dolist (anchor current-anchors)
      (unless (gethash anchor target-set)
        (qq-chat--delete-message-node anchor)))
    (cl-loop for message in messages
             for index from 0 do
             (let* ((anchor (qq-chat--message-anchor message))
                    (node (and anchor (gethash anchor qq-chat--message-node-table))))
               (if node
                   (progn
                     (unless (equal (ewoc--node-data node) message)
                       (push anchor touched-anchors))
                     (let ((qq-chat--rendering t)
                           (inhibit-read-only t)
                           (buffer-undo-list t))
                       (ewoc-set-data node message)))
                 (let ((before-node
                        (cl-loop for later-anchor in (nthcdr (1+ index) target-anchors)
                                 for later-node = (gethash later-anchor qq-chat--message-node-table)
                                 when later-node return later-node)))
                   (push anchor touched-anchors)
                   (qq-chat--insert-message-node-before message before-node)))))
    (setq qq-chat--displayed-message-anchors target-anchors)
    (qq-chat--rebuild-render-contexts messages)
    (qq-chat--rebuild-media-anchor-index messages)
    (if target-anchors
        (qq-chat--clear-empty-node)
      (qq-chat--ensure-empty-node))
    (dolist (anchor
             (delete-dups
              (append touched-anchors
                      (qq-chat--changed-render-context-anchors
                       target-anchors context-snapshot))))
      (when-let* ((node (gethash anchor qq-chat--message-node-table)))
        (qq-chat--redisplay-node node)))))

(defun qq-chat--rekey-message-node-if-needed (message)
  "If MESSAGE was shown under local-id, rekey node/tables to server-id.

Return the new anchor when a rekey happened, else nil.  Needed when NapCat
returns the NT snowflake `message_id' after optimistic pending insert."
  (let* ((local-id (alist-get 'local-id message))
         (server-id (alist-get 'server-id message))
         (node (and local-id
                    server-id
                    (not (equal local-id server-id))
                    qq-chat--message-node-table
                    (gethash local-id qq-chat--message-node-table))))
    (when node
      (remhash local-id qq-chat--message-node-table)
      (puthash server-id node qq-chat--message-node-table)
      (setq qq-chat--displayed-message-anchors
            (mapcar (lambda (anchor)
                      (if (equal anchor local-id) server-id anchor))
                    (or qq-chat--displayed-message-anchors '())))
      (when (and (hash-table-p qq-chat--render-context-by-anchor)
                 (gethash local-id qq-chat--render-context-by-anchor))
        (puthash server-id
                 (gethash local-id qq-chat--render-context-by-anchor)
                 qq-chat--render-context-by-anchor)
        (remhash local-id qq-chat--render-context-by-anchor))
      (when (hash-table-p qq-chat--media-anchors-by-key)
        (maphash
         (lambda (key anchors)
           (puthash key
                    (mapcar (lambda (anchor)
                              (if (equal anchor local-id) server-id anchor))
                            anchors)
                    qq-chat--media-anchors-by-key))
         qq-chat--media-anchors-by-key))
      (setq qq-chat--deferred-node-anchors
            (mapcar (lambda (anchor)
                      (if (equal anchor local-id) server-id anchor))
                    (or qq-chat--deferred-node-anchors '())))
      server-id)))

(defun qq-chat--apply-single-message-change-partially (anchor messages)
  "Apply one ANCHOR change against MESSAGES incrementally.

Return non-nil when the persistent EWOC was patched successfully."
  (let* ((message-for-rekey
          (seq-find (lambda (message)
                      (or (equal (qq-chat--message-anchor message) anchor)
                          (equal (alist-get 'local-id message) anchor)
                          (equal (alist-get 'server-id message) anchor)))
                    messages))
         (_rekey (and message-for-rekey
                      (when-let* ((rekeyed (qq-chat--rekey-message-node-if-needed
                                            message-for-rekey)))
                        (setq anchor rekeyed)
                        rekeyed)))
         (current-anchors (or qq-chat--displayed-message-anchors '()))
         (target-anchors (qq-chat--message-anchor-list messages))
         (present-before (member anchor current-anchors))
         (present-after (member anchor target-anchors))
         (affected-anchors (qq-chat--message-neighborhood-anchors
                            current-anchors target-anchors anchor))
         (target-message (or message-for-rekey
                             (seq-find (lambda (message)
                                         (equal (qq-chat--message-anchor message)
                                                anchor))
                                       messages)))
         (context-snapshot (qq-chat--copy-render-contexts-for-anchors
                            affected-anchors)))
    (when (and anchor
               qq-chat--ewoc
               (or present-before present-after)
               (qq-chat--timeline-order-compatible-p current-anchors target-anchors))
      (qq-chat--mutate-timeline-preserving-point
       (lambda ()
         (when target-anchors
           (qq-chat--clear-empty-node))
         (cond
          ((and present-before (not present-after))
           (qq-chat--delete-message-node anchor))
          ((and present-after (not present-before) target-message)
           (let ((before-node
                  (cl-loop for later-anchor in (cdr (member anchor target-anchors))
                           for later-node = (gethash later-anchor qq-chat--message-node-table)
                           when later-node return later-node)))
             (qq-chat--insert-message-node-before target-message before-node)))
          ((and present-after target-message)
           (when-let* ((node (gethash anchor qq-chat--message-node-table)))
             (let ((qq-chat--rendering t)
                   (inhibit-read-only t)
                   (buffer-undo-list t))
               (ewoc-set-data node target-message)))))
         (setq qq-chat--displayed-message-anchors target-anchors)
         (qq-chat--recompute-render-contexts-for-anchors messages affected-anchors)
         (qq-chat--rebuild-media-anchor-index messages)
         (if target-anchors
             (qq-chat--clear-empty-node)
           (qq-chat--ensure-empty-node))
         (dolist (affected-anchor
                  (delete-dups
                   (append
                    (and present-after (list anchor))
                    (qq-chat--changed-render-context-anchors
                     affected-anchors context-snapshot))))
           (when-let* ((node (gethash affected-anchor qq-chat--message-node-table)))
             (qq-chat--redisplay-node node)))))
      t)))

(defun qq-chat--footer-start-position ()
  "Return EWOC footer start position for the current chat buffer, or nil."
  (and qq-chat--ewoc
       (ignore-errors
         (ewoc-location (ewoc--footer qq-chat--ewoc)))))

(defun qq-chat--footer-region-bounds ()
  "Return footer context bounds as (START . END), or nil when unavailable."
  (when-let* ((footer-start (qq-chat--footer-start-position))
              (prompt-start (qq-chat--input-prompt-start-position)))
    (when (<= footer-start prompt-start)
      (cons footer-start prompt-start))))

(defun qq-chat--footer-offset-at-position (&optional position)
  "Return footer-relative offset for POSITION or point, or nil."
  (when-let* ((bounds (qq-chat--footer-region-bounds)))
    (let ((pos (or position (point))))
      (when (and (<= (car bounds) pos)
                 (< pos (cdr bounds)))
        (- pos (car bounds))))))

(defun qq-chat--capture-footer-point-offset ()
  "Return point offset inside the footer context region, or nil."
  (qq-chat--footer-offset-at-position (point)))

(defun qq-chat--restore-footer-point-offset (offset)
  "Restore point inside the footer context region using OFFSET."
  (when (numberp offset)
    (when-let* ((bounds (qq-chat--footer-region-bounds)))
      (let ((start (car bounds))
            (end (cdr bounds)))
        (goto-char (min (max start (1- end))
                        (+ start offset)))))))

(defun qq-chat--capture-position-state (&optional position)
  "Capture chat-relative position state for POSITION or point."
  (let ((pos (or position (point))))
    (cond
     ((qq-chat--point-in-input-p pos)
      (when-let* ((input-start (qq-chat--input-start-position)))
        (list :zone 'input
              :offset (- pos input-start))))
     ((numberp (qq-chat--footer-offset-at-position pos))
      (list :zone 'footer
            :offset (qq-chat--footer-offset-at-position pos)))
     (t
      (save-excursion
        (goto-char pos)
        (list :zone 'message
              :snapshot (qq-view-capture-position
                         :anchor-property 'qq-chat-message-anchor
                         :preserve-window-start nil)))))))

(defun qq-chat--restore-position-state (state)
  "Restore point from STATE and return restored position."
  (pcase (plist-get state :zone)
    ('input
     (when-let* ((input-start (qq-chat--input-start-position))
                 (logical-end (qq-chat--input-logical-end-position)))
       (goto-char (min logical-end
                       (max input-start
                            (+ input-start (or (plist-get state :offset) 0)))))))
    ('footer
     (qq-chat--restore-footer-point-offset (plist-get state :offset)))
    (_
     (when-let* ((snapshot (plist-get state :snapshot)))
       (qq-view-restore-position snapshot))))
  (point))

(defun qq-chat--capture-mark-state ()
  "Capture active mark state for later redraw restoration, or nil."
  (when mark-active
    (qq-chat--capture-position-state (mark t))))

(defun qq-chat--restore-mark-state (state)
  "Restore active mark STATE after a redraw.

When STATE is nil, deactivate any transient region left by buffer mutation."
  (if (not state)
      (setq mark-active nil
            deactivate-mark t)
    (let ((mark-pos (save-excursion
                      (qq-chat--restore-position-state state)
                      (point))))
      (set-marker (mark-marker) mark-pos)
      (setq mark-active t
            deactivate-mark nil))))

(defun qq-chat--at-message-bottom-p ()
  "Return non-nil when point is at timeline bottom, outside draft input."
  (and (= (point) (point-max))
       (not (qq-chat--point-in-input-p))
       (not (numberp (qq-chat--capture-footer-point-offset)))))

(defun qq-chat--mutate-timeline-preserving-point (mutator)
  "Run MUTATOR while preserving reading/composer position.

Follow the same broad strategy as
`disco-room--mutate-timeline-preserving-point': keep composer cursor stable,
keep footer point stable, keep bottom-following buffers at the bottom, and
otherwise restore message position by semantic anchor."
  (let* ((window-input-offsets (qq-chat--capture-window-input-offsets))
         (mark-state (qq-chat--capture-mark-state))
         (input-start (qq-chat--input-start-position))
         (in-input (and (number-or-marker-p input-start)
                        (qq-chat--point-in-input-p)))
         (input-offset (and in-input (- (point) input-start)))
         (footer-offset (and (not in-input)
                             (qq-chat--capture-footer-point-offset)))
         (at-bottom (and (not in-input)
                         (not (numberp footer-offset))
                         (qq-chat--at-message-bottom-p)))
         (snapshot (and (not in-input)
                        (not (numberp footer-offset))
                        (not at-bottom)
                        (qq-view-capture-position
                         :anchor-property 'qq-chat-message-anchor
                         :preserve-window-start t))))
    (unwind-protect
        (progn
          (funcall mutator)
          (cond
           ((and in-input (numberp input-offset))
            (let ((new-start (qq-chat--input-start-position))
                  (logical-end (qq-chat--input-logical-end-position)))
              (when (and (number-or-marker-p new-start)
                         (number-or-marker-p logical-end))
                (goto-char (min logical-end
                                (max new-start (+ new-start input-offset)))))))
           ((numberp footer-offset)
            (qq-chat--restore-footer-point-offset footer-offset))
           (at-bottom
            (goto-char (point-max)))
           (snapshot
            (qq-view-restore-position snapshot))))
      (qq-chat--restore-window-input-offsets window-input-offsets)
      (qq-chat--restore-mark-state mark-state)
      (qq-chat--update-context-mode))))

(defun qq-chat--redisplay-node (node)
  "Redisplay a single EWOC NODE for the current chat buffer."
  (when (and qq-chat--ewoc node)
    (let ((qq-chat--rendering t)
          (inhibit-read-only t)
          (buffer-undo-list t))
      (save-excursion
        (ewoc-invalidate qq-chat--ewoc node)))))

(defun qq-chat--redisplay-message-anchors-preserving-point (anchors)
  "Redisplay ANCHORS using single-node EWOC invalidation."
  (when (and qq-chat--ewoc anchors)
    (qq-chat--mutate-timeline-preserving-point
     (lambda ()
       (dolist (anchor (delete-dups (delq nil (copy-sequence anchors))))
         (when-let* ((node (gethash anchor qq-chat--message-node-table)))
           (qq-chat--redisplay-node node)))))))

(defun qq-chat--invalidate-message-anchors-preserving-point (anchors)
  "Compatibility wrapper for anchor-local node redisplay."
  (qq-chat--redisplay-message-anchors-preserving-point anchors))

(defun qq-chat--chat-update (&rest parts)
  "Update dirty PARTS of the current chat buffer.

PARTS is a list containing any of `header-line', `header', `footer',
`prompt', or `timeline'.  When PARTS is empty, perform a full partitioned
update similar in spirit to `telega-chatbuf--chat-update'."
  (let* ((full-p (or (null parts) (memq 'full parts)))
         (initial-frame-p (or full-p
                              (null qq-chat--ewoc)
                              (not (disco-chatbuf-prompt-button-live-p))))
         (header-line-p (or full-p initial-frame-p (memq 'header-line parts)))
         (header-p (or full-p initial-frame-p (memq 'header parts)))
         (footer-p (or full-p initial-frame-p (memq 'footer parts)))
         (prompt-p (or full-p initial-frame-p (memq 'prompt parts)))
         (timeline-p (or full-p initial-frame-p (memq 'timeline parts)))
         (messages (and timeline-p
                        (qq-state-session-messages qq-chat--session-key))))
    (when header-line-p
      (qq-chat--header-line-update))
    (when (or header-p footer-p prompt-p timeline-p)
      (let ((qq-chat--rendering t))
        (qq-chat--mutate-timeline-preserving-point
         (lambda ()
           (qq-chat--ensure-ewoc)
           (when (or header-p footer-p)
             (qq-chat--apply-frame-update))
           (when (and prompt-p (not (or header-p footer-p)))
             (qq-chat--prompt-update))
           (when timeline-p
             (qq-chat--reconcile-timeline messages))))))))

(defun qq-chat--update-frame-preserving-point ()
  "Refresh chat header/footer/prompt while preserving message position."
  (qq-chat--chat-update 'header 'footer 'prompt))

(defun qq-chat--render-empty-placeholder ()
  "Insert the empty timeline placeholder row."
  (let ((start (point)))
    (qq-view-insert-note-line "No messages loaded yet.")
    (insert "\n")
    (add-text-properties
     start (point)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only)
       qq-chat-internal empty-placeholder))))

(defun qq-chat--ewoc-printer (message)
  "EWOC pretty-printer for one chat MESSAGE."
  (if (eq message qq-chat--empty-placeholder)
      (qq-chat--render-empty-placeholder)
    (qq-chat--render-message message)))

(defun qq-chat--set-draft (text)
  "Set current draft TEXT and refresh only the tail composer region."
  (setq qq-chat--draft-input text)
  (setq qq-chat--draft-input-rich (or text ""))
  (qq-chat--chat-update 'prompt)
  (goto-char (or (qq-chat--input-logical-end-position) (point-max))))

(defun qq-chat--push-input-history (text)
  "Insert TEXT into input history when appropriate."
  (unless (disco-chatbuf-input-has-objects-p)
    (disco-chatbuf-input-history-push text)))

(defun qq-chat--guess-file-segment-type (path)
  "Return best QQ send segment type for local PATH."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (cond
     ((member ext '("png" "jpg" "jpeg" "gif" "webp" "bmp" "svg")) "image")
     ((member ext '("mp4" "mov" "mkv" "webm" "avi" "flv")) "video")
     ((member ext '("amr" "mp3" "wav" "ogg" "m4a" "aac" "flac" "opus")) "record")
     (t "file"))))

(defun qq-chat--segment-object-label (segment)
  "Return visible input label for structured SEGMENT."
  (let* ((type (or (alist-get 'type segment) "segment"))
         (data (alist-get 'data segment))
         (path (or (alist-get 'path data)
                   (alist-get 'file data)
                   (alist-get 'name data)
                   "item"))
         (name (file-name-nondirectory path)))
    (format "[%s:%s]" type name)))

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
  (unless (qq-chat--point-in-input-p)
    (goto-char (or (qq-chat--input-logical-end-position) (point-max))))
  (when (and (qq-chat--point-in-input-p)
             (> (point) (or (qq-chat--input-start-position) (point-min)))
             (let ((ch (char-before)))
               (and ch (not (memq ch '(32 9 10))))))
    (insert " "))
  (let* ((object (qq-chat--segment-input-object segment))
         (label (plist-get object :label)))
    (disco-chatbuf-input-insert label :object object)
    (qq-chat--apply-input-text-properties)))

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

(defun qq-chat--current-input-segments ()
  "Parse current input region into outbound QQ message segments."
  (let* ((input (or (disco-chatbuf-input-string) ""))
         (object-prop disco-chatbuf-input-object-property)
         (limit (length input))
         (pos 0)
         segments)
    (while (< pos limit)
      (let ((object (get-text-property pos object-prop input)))
        (if object
            (let* ((next (or (next-single-property-change
                              pos object-prop input limit)
                             limit))
                   (segment (qq-chat--input-object-segment object)))
              (when segment
                (push segment segments))
              (setq pos next))
          (let* ((next (or (next-single-property-change
                            pos object-prop input limit)
                           limit))
                 (text (substring-no-properties input pos next)))
            (unless (string-empty-p text)
              (push `((type . "text")
                      (data . ((text . ,text))))
                    segments))
            (setq pos next)))))
    (nreverse segments)))

(defun qq-chat--composer-context-text (label message-id)
  "Return one composer context line for LABEL and MESSAGE-ID."
  (format "%s %s\n" label message-id))

(defun qq-chat--reply-context-text ()
  "Return extra context lines shown above the chat composer."
  (let ((message (qq-chat--reply-message)))
    (if-let* ((reply-id (alist-get 'server-id message))
              (preview (string-trim (or (qq-state-message-preview message) ""))))
        (concat
         (qq-chat--composer-context-text "Replying to" reply-id)
         (format "  %s\n"
                 (if (string-empty-p preview)
                     "(no preview)"
                   preview)))
      "")))

(defun qq-chat--header-help-text ()
  "Return header help text for chat actions."
  (concat
   "M-<: older/more   M-g n/p: message nav   C-c g: refresh   C-c /: search"
   "   C-c C-n/C-p: next/prev hit   C-c r: read all   C-c m r/d/o/a: message actions"
   "   C-c C-f: attach   RET/C-c C-c: send   type at >>>   M-p/M-n: history"
   "   timeline q: quit   C-c ?: help"))

(defun qq-chat--insert-date-separator-row (day-label)
  "Insert a date separator row for DAY-LABEL."
  (qq-view-insert-note-line (format "-- %s --" day-label) :face 'font-lock-doc-face))

(defun qq-chat--insert-reply-preview-line (reply-id properties prefix-state)
  "Insert one inline reply preview line for REPLY-ID.

PROPERTIES and PREFIX-STATE control styling for the inserted line."
  (let ((reply-start (point)))
    (insert (format "Reply to %s\n" reply-id))
    (add-text-properties reply-start (point) (append properties (list 'face 'shadow)))
    (qq-ui-apply-line-prefix reply-start (point) prefix-state)))

(defun qq-chat--segment-inline-string (segment)
  "Return inline display string for SEGMENT, or nil for block-like segments."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("text" (or (alist-get 'text data) ""))
      ("at" (concat "@" (or (alist-get 'name data)
                            (alist-get 'qq data)
                            "mention")))
      ("face" (qq-media-face-display-string (or (alist-get 'id data) "?")))
      (_ nil))))

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
  "Return one-line summary for media SEGMENT."
  (let* ((data (alist-get 'data segment))
         (summary (or (alist-get 'summary data)
                      (alist-get 'name data)
                      (alist-get 'file data)
                      (alist-get 'url data))))
    (if (and (stringp summary) (not (string-empty-p summary)))
        summary
      "(no label)")))

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
  "Return one meta line for media SEGMENT."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (pieces (delq nil
                       (list
                        (qq-chat--format-byte-size (or (alist-get 'file_size data)
                                                       (alist-get 'file-size data)))
                        (and (or (equal type "image")
                                 (qq-media-imageish-file-segment-p segment))
                             (alist-get 'sub_type data)
                             (format "sub=%s" (alist-get 'sub_type data)))
                        (and (equal type "mface")
                             (alist-get 'emoji_package_id data)
                             (format "pkg=%s" (alist-get 'emoji_package_id data)))
                        (and (or (equal type "mface")
                                 (equal type "image"))
                             (alist-get 'emoji_id data)
                             (format "emoji=%s" (alist-get 'emoji_id data)))))))
    (if pieces
        (string-join pieces "  ")
      "QQ media resource")))

(defun qq-chat--insert-prefixed-plain-line (prefix-state text properties &optional face)
  "Insert TEXT as one prefixed plain line using PREFIX-STATE and PROPERTIES."
  (let ((line-start (point)))
    (insert text "\n")
    (add-text-properties line-start (point)
                         (append properties
                                 (when face (list 'face face))))
    (qq-ui-apply-line-prefix line-start (point) prefix-state)))

(defun qq-chat--insert-message-action-button (label callback help-echo)
  "Insert one message action button with LABEL, CALLBACK and HELP-ECHO."
  (qq-ui-insert-action-button
   label
   callback
   :face 'link
   :help-echo help-echo
   :properties '(read-only t front-sticky t rear-nonsticky (read-only))))

(defun qq-chat--insert-segment-transfer-line (segment prefix-state properties)
  "Insert one transfer/action line for media SEGMENT."
  (let* ((download-state (qq-media-segment-download-state segment))
         (download-status (plist-get download-state :status))
         (download-path (plist-get download-state :path))
         (download-error (plist-get download-state :error))
         (local-file (qq-media-segment-local-file segment))
         (line-start (point))
         (inserted nil))
    (insert "transfer: ")
    (cond
     ((eq download-status 'downloading)
      (insert "[Downloading...]")
      (setq inserted t))
     (t
      (when local-file
        (qq-chat--insert-message-action-button
         "[Open Local]"
         (lambda ()
           (qq-media-segment-open-local segment))
         "Open the local cached/downloaded file")
        (setq inserted t))
      (pcase download-status
        ('downloaded
         (when inserted (insert " "))
         (qq-chat--insert-message-action-button
          "[Save As]"
          (lambda ()
            (qq-media-segment-save-as segment))
          "Copy media to a chosen location")
         (setq inserted t)
         (when (and (stringp download-path) (not (string-empty-p download-path)))
           (insert (format "  %s" (file-name-nondirectory download-path)))))
        ('error
         (when inserted (insert " "))
         (qq-chat--insert-message-action-button
          "[Retry]"
          (lambda ()
            (qq-media-segment-start-download segment))
          "Retry media download")
         (insert " ")
         (qq-chat--insert-message-action-button
          "[Save As]"
          (lambda ()
            (qq-media-segment-save-as segment))
          "Save media to a chosen location")
         (setq inserted t)
         (when (and (stringp download-error) (not (string-empty-p download-error)))
           (insert (format "  error=%s"
                           (truncate-string-to-width download-error 56 nil nil t)))))
        (_
         (when inserted (insert " "))
         (qq-chat--insert-message-action-button
          "[Download]"
          (lambda ()
            (qq-media-segment-start-download segment))
          "Download media into qq cache directory")
         (insert " ")
         (qq-chat--insert-message-action-button
          "[Save As]"
          (lambda ()
            (qq-media-segment-save-as segment))
          "Save media to a chosen location")
         (setq inserted t)))))
    (unless inserted
      (insert "[Unavailable]"))
    (insert "\n")
    (qq-ui-apply-line-prefix line-start (point) prefix-state)
    (add-text-properties line-start (point) (append properties (list 'face 'shadow)))))

(defun qq-chat--insert-segment-media-line (segment prefix-state properties)
  "Insert one rich media card for SEGMENT using PREFIX-STATE and PROPERTIES."
  (let* ((kind (qq-chat--segment-media-kind-label segment))
         (summary (qq-chat--segment-media-summary segment))
         (meta (qq-chat--segment-media-meta-line segment))
         (data (alist-get 'data segment))
         (url (alist-get 'url data))
         (prefix-state (let ((qq-ui-card-indent-prefix-state prefix-state))
                         (qq-ui-card-prefix-state))))
    (let ((title-start (point)))
      (insert (format "%s: %s\n" kind summary))
      (qq-ui-apply-line-prefix title-start (point) prefix-state)
      (add-text-properties title-start (point) (append properties (list 'face 'bold))))
    (let ((meta-start (point)))
      (insert meta "\n")
      (qq-ui-apply-line-prefix meta-start (point) prefix-state)
      (add-text-properties meta-start (point) (append properties (list 'face 'shadow))))
    (let ((action-start (point))
          (inserted nil))
      (when (qq-media-segment-playable-p segment)
        (qq-chat--insert-message-action-button
         "[Play]"
         (lambda ()
           (qq-media-segment-play segment))
         (format "Play %s" kind))
        (setq inserted t))
      (when inserted
        (insert " "))
      (qq-chat--insert-message-action-button
       "[Open]"
       (lambda ()
         (qq-media-segment-open segment))
       (format "Open %s" kind))
      (when (and (stringp url) (not (string-empty-p url)))
        (insert " ")
        (qq-chat--insert-message-action-button
         "[URL]"
         (lambda ()
           (browse-url url t))
         "Open resource URL"))
      (insert "\n")
      (qq-ui-apply-line-prefix action-start (point) prefix-state)
      (add-text-properties action-start (point) (append properties (list 'face 'shadow))))
    (qq-chat--insert-segment-transfer-line segment prefix-state properties)
    (when (qq-media-segment-preview-capable-p segment)
      (let ((preview-start (point))
            (preview (qq-media-segment-preview-image segment))
            (loading (qq-media-segment-preview-fetching-p segment)))
        (cond
         (preview
          (condition-case _
              (disco-media-insert-image-slices
               preview
               (and (stringp url) (not (string-empty-p url)) url)
               nil
               (if (equal (alist-get 'type segment) "mface")
                   "[sticker]"
                 "[image]"))
            (error
             (insert "[preview unavailable]")))
          (insert "\n"))
         (loading
          (insert "[loading preview]\n"))
         (t
          (insert "[preview unavailable]\n")))
        (qq-ui-apply-line-prefix preview-start (point) prefix-state)
        (add-text-properties preview-start (point) (append properties (list 'face 'shadow)))))))

(defun qq-chat--insert-message-body (message prefix-state properties)
  "Insert MESSAGE content body using PREFIX-STATE and PROPERTIES."
  (let ((segments (alist-get 'segments message))
        (inline-parts nil))
    (cl-labels ((flush-inline ()
                  (when inline-parts
                    (qq-ui-insert-prefixed-lines
                     prefix-state
                     (mapconcat #'identity (nreverse inline-parts) "")
                     :properties properties)
                    (setq inline-parts nil))))
      (if (or (eq (alist-get 'status message) 'recalled)
              (null segments))
          (qq-ui-insert-prefixed-lines prefix-state (qq-chat--message-body message) :properties properties)
        (dolist (segment segments)
          (let ((type (alist-get 'type segment)))
            (unless (equal type "reply")
              (cond
               ((qq-chat--media-segment-p segment)
                (flush-inline)
                (qq-chat--insert-segment-media-line segment prefix-state properties))
               ((qq-chat--segment-inline-string segment)
                (push (qq-chat--segment-inline-string segment) inline-parts))
               (t
                (push (format "[%s]" (or type "segment")) inline-parts))))))
        (flush-inline)))))

(defun qq-chat--message-openable-p (message)
  "Return non-nil when MESSAGE has an openable QQ media resource."
  (qq-media-message-has-openable-resource-p message))

(defun qq-chat--set-pending-reply (message)
  "Set MESSAGE as the pending reply target in current chat buffer."
  (let ((message-id (alist-get 'server-id message)))
    (unless message-id
      (user-error "qq: selected message has no server id"))
    (qq-chat--set-reply-message message)
    (qq-chat--chat-update 'footer)
    (goto-char (or (qq-chat--input-logical-end-position) (point-max)))
    (message "qq: next message will reply to %s" message-id)))

(defun qq-chat--delete-message-internal (message)
  "Recall MESSAGE after confirmation."
  (let ((message-id (alist-get 'server-id message)))
    (unless message-id
      (user-error "qq: selected message has no server id"))
    (when (y-or-n-p (format "Recall message %s? " message-id))
      (qq-api-delete-message message-id))))

(defun qq-chat--insert-message-actions (message prefix-state properties)
  "Insert per-message action buttons for MESSAGE using PREFIX-STATE."
  (let* ((server-id (alist-get 'server-id message))
         (has-resource (qq-chat--message-openable-p message))
         (action-start (point))
         (inserted nil))
    (when server-id
      (setq inserted t)
      (qq-chat--insert-message-action-button
       "[Reply]"
       (lambda ()
         (qq-chat--set-pending-reply message))
       (format "Reply to %s" server-id)))
    (when has-resource
      (when inserted (insert " "))
      (setq inserted t)
      (qq-chat--insert-message-action-button
       "[Open]"
       (lambda ()
         (qq-media-open-message-resource message))
       "Open primary media resource"))
    (when (alist-get 'sender-id message)
      (when inserted (insert " "))
      (setq inserted t)
      (qq-chat--insert-message-action-button
       "[Avatar]"
       (lambda ()
         (qq-media-open-message-avatar message))
       "Open sender avatar"))
    (when (and server-id (alist-get 'self-p message))
      (when inserted (insert " "))
      (setq inserted t)
      (qq-chat--insert-message-action-button
       "[Recall]"
       (lambda ()
         (qq-chat--delete-message-internal message))
       (format "Recall %s" server-id)))
    (when inserted
      (insert "\n")
      (qq-ui-apply-line-prefix action-start (point) prefix-state)
      (add-text-properties action-start (point) (append properties (list 'face 'shadow))))))

(defun qq-chat--render-message (message &optional previous-message)
  "Insert one formatted MESSAGE block.

When PREVIOUS-MESSAGE is non-nil and no stored render context is available,
fall back to computing compact grouping directly."
  (let* ((anchor (qq-chat--message-anchor message))
         (start (point))
         (context (or (qq-chat--message-render-context message)
                      (qq-chat--compute-message-render-context previous-message message)))
         (insert-date (plist-get context :insert-date))
         (self-p (alist-get 'self-p message))
         (author-face (if self-p 'font-lock-keyword-face 'default))
         (header-prefix (if self-p ">> " "-- "))
         (body-prefix (if self-p "   " "|  "))
         (reply-id (qq-chat--message-reply-id message))
         (properties (qq-chat--message-line-properties message anchor))
         (status-suffix (qq-chat--status-suffix message))
         (compact (plist-get context :compact))
         (header-prefix-state (qq-ui-make-prefix-state header-prefix body-prefix))
         (body-prefix-state (qq-ui-make-prefix-state body-prefix body-prefix))
         (body-text (qq-chat--message-body message))
         (short-time (qq-chat--format-time-short (alist-get 'time message))))
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (qq-chat--insert-date-separator-row (qq-chat--message-day-label insert-date)))
    (if compact
        (progn
          (when reply-id
            (qq-chat--insert-reply-preview-line reply-id properties body-prefix-state))
          (let* ((lines (split-string (if (string-empty-p body-text) "(empty message)" body-text) "\n" nil))
                 (first-line (or (car lines) "(empty message)"))
                 (rest-lines (cdr lines))
                 (content-start (point)))
            (insert first-line)
            (unless (string-empty-p short-time)
              (insert "  ")
              (let ((time-start (point)))
                (insert short-time)
                (add-text-properties time-start (point) (list 'face 'shadow))))
            (insert "\n")
            (qq-ui-apply-line-prefix content-start (point) body-prefix-state)
            (add-text-properties content-start (point) properties)
            (when rest-lines
              (let ((rest-start (point)))
                (qq-ui-insert-prefixed-lines body-prefix-state (string-join rest-lines "\n") :properties properties)
                (add-text-properties rest-start (point) properties))))
          (qq-chat--insert-message-actions message body-prefix-state properties))
      (let ((header-start (point)))
        (when-let* ((sender-id (alist-get 'sender-id message)))
          (let ((avatar-start (point)))
            (insert (qq-media-avatar-display-string sender-id) " ")
            (add-text-properties
             avatar-start
             (point)
             '(mouse-face highlight
               help-echo "Open sender avatar"))))
        (qq-chat--insert-message-sender message author-face)
        (insert status-suffix)
        (insert "  ")
        (let ((time-start (point)))
          (insert (qq-chat--format-time (alist-get 'time message)))
          (add-text-properties time-start (point) (list 'face 'shadow)))
        (insert "\n")
        (qq-ui-apply-line-prefix header-start (point) header-prefix-state)
        (add-text-properties header-start (point) properties))
      (when reply-id
        (qq-chat--insert-reply-preview-line reply-id properties body-prefix-state))
      (qq-chat--insert-message-body message body-prefix-state properties)
      (qq-chat--insert-message-actions message body-prefix-state properties))
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-chat--history-search (query forward)
  "Search chat history for QUERY in FORWARD direction.

Return non-nil on success."
  (let ((case-fold-search t)
        (history-end (max (point-min)
                          (1- (or (and (markerp qq-chat--input-marker)
                                       (marker-position qq-chat--input-marker))
                                  (point-max))))))
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

(defun qq-chat--render-internal ()
  "Apply a full partitioned update for the current chat buffer."
  (qq-chat--chat-update 'header-line 'header 'footer 'prompt 'timeline))

(defun qq-chat-render ()
  "Render current chat buffer from local state."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--capture-draft)
  (qq-chat--render-internal))

(defun qq-chat-refresh ()
  "Refresh current chat contents from local state and NapCat history."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat-render)
  (qq-api-fetch-history qq-chat--session-key))

(defun qq-chat-load-older-messages ()
  "Load older history page for the current chat."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-api-fetch-history qq-chat--session-key
                        (qq-state-session-oldest-message-id qq-chat--session-key)))

(defun qq-chat-send-message ()
  "Send current chat draft."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
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
         (raw-message (unless (disco-chatbuf-input-has-objects-p)
                        text)))
    (if (and (not (disco-chatbuf-input-has-objects-p))
             (string-empty-p (string-trim text)))
        (message "qq: draft is empty")
      (qq-chat--push-input-history text)
      (setq qq-chat--input-index nil)
      (setq qq-chat--input-pending nil)
      (setq qq-chat--draft-input "")
      (setq qq-chat--draft-input-rich "")
      (qq-chat--set-reply-message nil)
      (qq-chat--chat-update 'footer 'prompt)
      (qq-api-send-message qq-chat--session-key send-segments raw-message))))

(defun qq-chat-return-dwim (arg)
  "Send current draft, or insert newline with prefix ARG."
  (interactive "P")
  (if (not (qq-chat--point-in-input-p))
      (goto-char (or (qq-chat--input-logical-end-position) (point-max)))
    (if arg
        (insert "\n")
      (qq-chat-send-message))))

(defun qq-chat-edit-draft ()
  "Move point to the editable draft area."
  (interactive)
  (goto-char (or (qq-chat--input-logical-end-position) (point-max))))

(defun qq-chat-draft-prev ()
  "Replace draft with previous entry from input history."
  (interactive)
  (condition-case _err
      (progn
        (disco-chatbuf-input-history-prev)
        (qq-chat--sync-draft-from-buffer)
        (goto-char (or (qq-chat--input-logical-end-position) (point-max))))
    (user-error
     (user-error "qq: no previous inputs"))))

(defun qq-chat-draft-next ()
  "Move draft navigation toward more recent input history."
  (interactive)
  (condition-case _err
      (progn
        (disco-chatbuf-input-history-next)
        (qq-chat--sync-draft-from-buffer)
        (goto-char (or (qq-chat--input-logical-end-position) (point-max))))
    (user-error
     (user-error "qq: not currently browsing input history"))))

(defun qq-chat-clear-draft ()
  "Clear current draft and exit input history navigation."
  (interactive)
  (setq qq-chat--input-index nil)
  (setq qq-chat--input-pending nil)
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

(defun qq-chat-open-resource-at-point ()
  "Open the most relevant media resource from the message at point."
  (interactive)
  (qq-media-open-message-resource
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-open-avatar-at-point ()
  "Open sender avatar for the message at point."
  (interactive)
  (qq-media-open-message-avatar
   (or (qq-chat--message-at-point)
       (user-error "qq: no message at point"))))

(defun qq-chat-cancel-dwim ()
  "Cancel reply context, or clear draft when no reply is pending."
  (interactive)
  (cond
   ((qq-chat--reply-message)
    (qq-chat--set-reply-message nil)
    (qq-chat--chat-update 'footer)
    (message "qq: reply target cleared"))
   ((or qq-chat--input-index
        qq-chat--input-pending
        (not (string-empty-p (string-trim (qq-chat--current-draft-string)))))
    (qq-chat-clear-draft)
    (message "qq: draft cleared"))
   (t
    (message "qq: nothing to cancel"))))

(defun qq-chat-read-all ()
  "Mark the current chat as read."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-api-mark-session-read qq-chat--session-key))

(defalias 'qq-chat-mark-read #'qq-chat-read-all)
(defalias 'qq-chat-send #'qq-chat-send-message)
(defalias 'qq-chat-load-older #'qq-chat-load-older-messages)

(defvar qq-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c g") #'qq-chat-refresh)
    (define-key map (kbd "C-c /") #'qq-chat-search)
    (define-key map (kbd "C-c C-n") #'qq-chat-search-next)
    (define-key map (kbd "C-c C-p") #'qq-chat-search-prev)
    (define-key map (kbd "C-c r") #'qq-chat-read-all)
    (define-key map (kbd "C-c m") qq-chat-message-prefix-map)
    (define-key map (kbd "M-<") #'qq-chat-load-older-messages)
    (define-key map (kbd "RET") #'qq-chat-return-dwim)
    (define-key map (kbd "TAB") #'qq-chat-edit-draft)
    (define-key map (kbd "C-c '") #'qq-chat-edit-draft)
    (define-key map (kbd "M-p") #'qq-chat-draft-prev)
    (define-key map (kbd "M-n") #'qq-chat-draft-next)
    (define-key map (kbd "C-c C-f") #'qq-chat-attach-file)
    (define-key map (kbd "M-g n") #'qq-chat-next-message)
    (define-key map (kbd "M-g p") #'qq-chat-previous-message)
    (define-key map (kbd "C-c C-c") #'qq-chat-send-message)
    (define-key map (kbd "C-c C-k") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-c ?") #'describe-mode)
    map)
  "Keymap for `qq-chat-mode'.")

(define-derived-mode qq-chat-mode nil "QQ-Chat"
  "Major mode for emacs-qq chat buffers."
  (disco-chatbuf-mode-setup)
  (setq-local qq-chat--draft-input "")
  (setq-local qq-chat--draft-input-rich "")
  (qq-chat--clear-input-region-markers)
  (disco-chatbuf-init-state 32)
  (disco-chatbuf-aux-reset)
  (disco-chatbuf-input-options-reset)
  (setq-local qq-chat--last-search-query nil)
  (setq-local qq-chat--rendering nil)
  (setq-local qq-chat--ewoc nil)
  (setq-local qq-chat--empty-node nil)
  (setq-local qq-chat--message-node-table (make-hash-table :test #'equal))
  (setq-local qq-chat--displayed-message-anchors nil)
  (setq-local qq-chat--render-context-by-anchor (make-hash-table :test #'equal))
  (setq-local qq-chat--media-anchors-by-key (make-hash-table :test #'equal))
  (setq-local qq-chat--deferred-node-anchors nil)
  (add-hook 'after-change-functions #'qq-chat--after-change nil t)
  (add-hook 'post-command-hook #'qq-chat--post-command nil t)
  (qq-chat--update-context-mode))

(defun qq-chat-open (session-key)
  "Open chat for SESSION-KEY."
  (interactive)
  (unless session-key
    (user-error "qq: session key is required"))
  (qq-state-upsert-session session-key nil nil)
  (let ((buffer (get-buffer-create (qq-chat--buffer-name session-key))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-chat-mode)
        (qq-chat-mode))
      (setq qq-chat--session-key session-key)
      (qq-chat-render)
      (goto-char (or (qq-chat--input-logical-end-position) (point-max)))
      (qq-api-fetch-history session-key)
      (when qq-auto-mark-read
        (qq-chat-read-all)))
    (pop-to-buffer buffer)))

(defun qq-chat--merge-deferred-node-anchors (anchors)
  "Merge ANCHORS into deferred node redisplay state for current buffer."
  (setq qq-chat--deferred-node-anchors
        (delete-dups
         (append (copy-sequence (or qq-chat--deferred-node-anchors '()))
                 (copy-sequence (or anchors '()))))))

(defun qq-chat--request-node-redisplay (anchors)
  "Redisplay ANCHORS now, or defer while a region is active.

This intentionally avoids the old idle-queue approach.  In line with telega's
node redisplay and disco-room's partial mutation helpers, chat updates happen
immediately unless the user is actively selecting a region."
  (let ((effective-anchors (delete-dups (copy-sequence (or anchors '())))))
    (when effective-anchors
      (if mark-active
          (qq-chat--merge-deferred-node-anchors effective-anchors)
        (qq-chat--invalidate-message-anchors-preserving-point effective-anchors)))))

(defun qq-chat--rerender-open-chats (&optional media-key)
  "Refresh affected chat message nodes after media cache updates.

When MEDIA-KEY is non-nil, only invalidate rendered messages that depend on the
changed cache entry."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-chat-mode)
        (let ((anchors (or (and media-key
                                (qq-chat--media-anchors-for-key media-key))
                           qq-chat--displayed-message-anchors)))
          (qq-chat--request-node-redisplay anchors))))))

(defun qq-chat--handle-state-change (event)
  "Refresh affected chat buffers after state EVENT.

Follow telega/disco-like update rules: patch single message changes in the
persistent EWOC when possible, fall back to full render for broader timeline
changes, and only update the composer frame for metadata changes.

Prefer `qq-state' `:mutation' metadata when present:

- message create/update → single-node partial patch (full render fallback)
- history / reset → full render (batch timeline rebuild still coarse)
- session read → header-line only (unread); other session → header-line+header
- friends/groups refresh → header + timeline (sender titles may change)"
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-message (plist-get event :message))
        (event-mutation (plist-get event :mutation)))
    (when (memq event-type '(message history reset session
                             sessions-refreshed friends-refreshed groups-refreshed))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'qq-chat-mode)
            (cond
             ((eq event-type 'message)
              (when (equal event-session-key qq-chat--session-key)
                (qq-chat--header-line-update)
                (let* ((messages (qq-state-session-messages qq-chat--session-key))
                       (anchor (or (plist-get event :message-anchor)
                                   (qq-chat--message-anchor event-message)
                                   (plist-get event :previous-anchor))))
                  (unless (qq-chat--apply-single-message-change-partially
                           anchor messages)
                    (qq-chat-render)))
                (when (and qq-auto-mark-read
                           (get-buffer-window buffer t))
                  (qq-chat-read-all))))
             ((eq event-type 'history)
              (when (equal event-session-key qq-chat--session-key)
                (qq-chat-render)))
             ((eq event-type 'reset)
              (qq-chat-render))
             ((eq event-type 'session)
              (when (equal event-session-key qq-chat--session-key)
                (if (eq event-mutation 'read)
                    ;; Unread clear: header-line (and later optimistic divider).
                    (qq-chat--chat-update 'header-line)
                  (qq-chat--chat-update 'header-line 'header))))
             ((eq event-type 'sessions-refreshed)
              (qq-chat--chat-update 'header-line 'header))
             ((memq event-type '(friends-refreshed groups-refreshed))
              (qq-chat--chat-update 'header-line 'header 'timeline)))))))))

(add-hook 'qq-media-cache-update-hook #'qq-chat--rerender-open-chats)
(add-hook 'qq-state-change-hook #'qq-chat--handle-state-change)

(provide 'qq-chat)

;;; qq-chat.el ends here
