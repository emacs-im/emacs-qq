;;; qq-forward.el --- Merged-forward message viewer for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; QQ merged-forward records are trees, not ordinary timeline messages.  This
;; module keeps their transient messages local to a `special-mode' buffer and
;; deliberately does not merge them into `qq-state'.  Timeline rendering can
;; use the public segment predicates/inserter without introducing a load cycle:
;; `qq-chat' autoloads those entry points, while this module requires
;; `qq-chat' to reuse its body and media renderer.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'qq-api)
(require 'qq-chat)
(require 'qq-media)
(require 'qq-state)
(require 'disco-ui)
(require 'disco-view)

(declare-function qq-api-get-forward
                  "qq-api" (source callback &optional errback))
(declare-function qq-api--exact-object-keys-p
                  "qq-api" (object required &optional optional))
(declare-function qq-api--signal-schema-error
                  "qq-api" (protocol-p format-string &rest args))
(declare-function qq-api--validate-native-forward-message
                  "qq-api" (message context protocol-p))
(declare-function qq-api--validate-native-forward-segment
                  "qq-api" (segment context protocol-p))
(declare-function qq-api-chat-locator "qq-api" (session-key))
(declare-function qq-api-validate-forward-source
                  "qq-api" (source &optional context protocol-p))
(declare-function qq-api-validate-native-forward-messages
                  "qq-api" (messages &optional context protocol-p))
(declare-function qq-api-validate-resource-id
                  "qq-api" (value &optional context protocol-p))
(declare-function qq-chat--insert-message-body
                  "qq-chat" (message prefix-state properties))
(declare-function qq-chat--message-reply-id "qq-chat" (message))

(defvar-local qq-forward--buffer-key nil
  "Explicit remote reference or local inline identity for this viewer.")

(defvar-local qq-forward--lookup-id nil
  "Display identity from the native source represented by this viewer.")

(defvar-local qq-forward--lookup-kind nil
  "Native source kind: `message', `resource', or nil for local inline.")

(defvar-local qq-forward--source nil
  "Validated fork-native remote source for this viewer, or nil inline.")

(defvar-local qq-forward--raw-messages nil
  "Raw fork-native snapshots currently displayed by this forward viewer.")

(defvar-local qq-forward--messages nil
  "Viewer-local normalized messages rendered in the current buffer.")

(defvar-local qq-forward--loading nil
  "Non-nil while this forward viewer is awaiting an API response.")

(defvar-local qq-forward--loaded-p nil
  "Non-nil after this viewer has accepted an inline or remote result.")

(defvar-local qq-forward--error nil
  "Last loading error string for this forward viewer, or nil.")

(defvar-local qq-forward--generation 0
  "Monotonic request/render generation for this forward viewer.")

(defvar-local qq-forward--inline-content nil
  "Native inline snapshot array supplied by the parent forward segment.")

(defvar-local qq-forward--inline-p nil
  "Non-nil when `qq-forward--inline-content' is authoritative.

This separate flag distinguishes an explicit empty inline message array from
a remote reference that must be fetched.")

(defvar-local qq-forward--media-indexes-by-key nil
  "Hash table mapping media cache keys to lists of entry indexes.

Rebuilt on full `qq-forward--render'.  Media cache ticks use this to redisplay
only the affected entries (telega-msg-redisplay / qq-chat media-anchor style).")

(defun qq-forward--present-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun qq-forward--single-alist-p (value)
  "Return non-nil when VALUE is one symbol-keyed alist."
  (and (consp value)
       (consp (car value))
       (symbolp (car (car value)))))

(defun qq-forward--source-id (source)
  "Return identity string from validated native SOURCE."
  (pcase (alist-get 'kind source)
    ("message" (alist-get 'message_id source))
    ("resource" (alist-get 'resource_id source))))

(defun qq-forward--source-reference (source)
  "Return (KIND . ID) for validated native SOURCE."
  (pcase (alist-get 'kind source)
    ("message" (cons 'message (alist-get 'message_id source)))
    ("resource" (cons 'resource (alist-get 'resource_id source)))))

(defun qq-forward--validate-content (content &optional protocol-p)
  "Validate canonical nested forward CONTENT union."
  (unless (qq-api--single-alist-p content)
    (qq-api--signal-schema-error
     protocol-p "qq: forward content must be an object"))
  (pcase (alist-get 'kind content)
    ("inline"
     (unless (qq-api--exact-object-keys-p content '(kind messages))
       (qq-api--signal-schema-error
        protocol-p "qq: inline forward content has invalid fields"))
     (qq-api-validate-native-forward-messages
      (alist-get 'messages content) "inline forward messages" protocol-p))
    ("remote"
     (unless (qq-api--exact-object-keys-p content '(kind reference))
       (qq-api--signal-schema-error
        protocol-p "qq: remote forward content has invalid fields"))
     (qq-api-validate-forward-source
      (alist-get 'reference content) "remote forward reference" protocol-p))
    (_
     (qq-api--signal-schema-error
      protocol-p "qq: forward content has invalid kind")))
  (copy-tree content))

;;;###autoload
(defun qq-forward-segment-p (segment)
  "Return non-nil when SEGMENT is a canonical internal forward card."
  (condition-case nil
      (when (qq-forward--single-alist-p segment)
        (let ((type (alist-get 'type segment))
              (data (alist-get 'data segment)))
          (cond
           ((and (equal type "forward")
                 (qq-api--exact-object-keys-p data '(content)))
            (qq-forward--validate-content (alist-get 'content data))
            t)
           ((and (equal type "card")
                 (qq-api--exact-object-keys-p
                  data '(kind reference presentation))
                 (equal (alist-get 'kind data) "forward"))
            (qq-api--validate-native-forward-segment
             `((kind . "forward-card")
               (payload . ((reference . ,(alist-get 'reference data))
                           (presentation . ,(alist-get 'presentation data)))))
             "canonical forward card" t)
            t))))
    (error nil)))

(defun qq-forward--segment-source (segment)
  "Return remote native source from canonical SEGMENT, or nil for inline."
  (unless (qq-forward-segment-p segment)
    (user-error "qq: segment is not a canonical forward record"))
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment)))
    (if (equal type "forward")
        (let ((content (qq-forward--validate-content
                        (alist-get 'content data))))
          (when (equal (alist-get 'kind content) "remote")
            (qq-api-validate-forward-source
             (alist-get 'reference content)
             "forward segment reference")))
      (let ((source
             (qq-api-validate-forward-source
              (alist-get 'reference data)
              "forward-card reference")))
        (unless (equal (alist-get 'kind source) "resource")
          (user-error "qq: forward-card reference must be resource"))
        source))))

;;;###autoload
(defun qq-forward-reference-id (segment)
  "Return canonical remote identity from SEGMENT, or nil for inline."
  (when-let* ((source (qq-forward--segment-source segment)))
    (qq-forward--source-id source)))

(defun qq-forward--inline-cell (segment)
  "Return native inline messages cell from canonical SEGMENT."
  (when (and (qq-forward-segment-p segment)
             (equal (alist-get 'type segment) "forward"))
    (let ((content
           (qq-forward--validate-content
            (alist-get 'content (alist-get 'data segment)))))
      (when (equal (alist-get 'kind content) "inline")
        (assq 'messages content)))))

(defun qq-forward--inline-message-count (inline-content)
  "Return number of inline forward messages in INLINE-CONTENT, or nil.

Important: in Elisp `(listp nil)' is non-nil and `(length nil)' is 0.
Remote forward-cards have no inline snapshot, so callers must treat nil
as \"unknown\", not zero — otherwise the UI always shows
\"0 forwarded messages\"."
  (cond
   ((vectorp inline-content)
    (length inline-content))
   ((null inline-content)
    nil)
   ((proper-list-p inline-content)
    (length inline-content))
   (t nil)))

(defun qq-forward--count-from-presentation (presentation)
  "Best-effort message count from PRESENTATION summary text, or nil.

QQ Ark summaries often look like \"查看3条转发消息\" when the nested
snapshot is remote-only."
  (let ((summary (qq-forward--present-string
                  (and (listp presentation) (alist-get 'summary presentation)))))
    (cond
     ((and (stringp summary)
           (string-match "查看\\([0-9]+\\)条" summary))
      (string-to-number (match-string 1 summary)))
     ((and (stringp summary)
           (string-match "\\([0-9]+\\)\\s-*条转发" summary))
      (string-to-number (match-string 1 summary)))
     ((and (stringp summary)
           (string-match "\\([0-9]+\\)\\s-*forwarded" summary))
      (string-to-number (match-string 1 summary)))
     (t nil))))
(defun qq-forward--unsupported-segment (payload)
  "Return safe internal placeholder for unsupported native PAYLOAD."
  (let* ((summary (qq-forward--present-string
                   (and (listp payload) (alist-get 'summary payload))))
         (summary (or summary "unknown element"))
         (summary (replace-regexp-in-string "[\n\r\t ]+" " " summary)))
    `((type . "text")
      (data
       . ((text
           . ,(format "[unsupported QQ element: %s]"
                      (truncate-string-to-width summary 80 nil nil t))))))))

(defun qq-forward--native-video-data (payload)
  "Map validated native video PAYLOAD into the internal media shape."
  (let* ((remote (alist-get 'remote payload))
         (state (alist-get 'state remote)))
    `((file . ,(alist-get 'file payload))
      ,@(when (assq 'local_path payload)
          `((path . ,(alist-get 'local_path payload))))
      ,@(when (assq 'size payload)
          `((file_size . ,(alist-get 'size payload))))
      ,@(when (assq 'name payload)
          `((name . ,(alist-get 'name payload))))
      ,@(when (assq 'thumb payload)
          `((thumb . ,(alist-get 'thumb payload))))
      (remote_status . ,state)
      ,@(when (equal state "available")
          `((url . ,(alist-get 'url remote)))))))

(defun qq-forward-native-segment-to-internal (segment)
  "Validate native SEGMENT and map it to one internal timeline segment."
  (qq-api--validate-native-forward-segment
   segment "native forward segment" t)
  (let ((kind (alist-get 'kind segment))
        (payload (alist-get 'payload segment)))
    (pcase kind
      ("video"
       `((type . "video")
         (data . ,(qq-forward--native-video-data payload))))
      ("forward"
       `((type . "forward")
         (data . ((content . ,(copy-tree (alist-get 'content payload)))))))
      ("forward-card"
       `((type . "card")
         (data . ((kind . "forward")
                  (reference . ,(copy-tree
                                 (alist-get 'reference payload)))
                  (presentation . ,(copy-tree
                                    (alist-get 'presentation payload)))))))
      ("reply"
       `((type . "reply") (data . ,(copy-tree payload))))
      ("unsupported"
       (qq-forward--unsupported-segment payload))
      (_
       `((type . ,kind) (data . ,(copy-tree payload)))))))

(defun qq-forward--native-sender-fields (sender)
  "Return internal sender display tuple from native SENDER."
  (pcase (alist-get 'kind sender)
    ("user"
     (let ((id (alist-get 'user_id sender))
           (name (alist-get 'name sender)))
       (list id (or (qq-forward--present-string name) id "unknown")
             name nil)))
    ("anonymous"
     (let ((name (alist-get 'name sender)))
       (list nil (or (qq-forward--present-string name) "anonymous")
             name nil)))))

(defun qq-forward--normalized-message
    (source segments entry-id server-id time sender-fields state origin)
  "Build one viewer-local message from a validated native snapshot."
  (pcase-let ((`(,sender-id ,sender-name ,sender-nickname ,sender-card)
               sender-fields))
    (let* ((segments (copy-tree segments))
           (recalled (equal state "recalled"))
           (preview (if recalled
                        "[message recalled]"
                      (qq-state-message-preview-from-segments segments))))
      `((id . ,entry-id)
        (server-id . ,server-id)
        (time . ,time)
        (sender-id . ,sender-id)
        (sender-name . ,sender-name)
        (sender-secondary-name . nil)
        (sender-card . ,sender-card)
        (sender-nickname . ,sender-nickname)
        (sender-remark . nil)
        (self-p . nil)
        (status . ,(if recalled 'recalled 'received))
        (segments . ,(if recalled nil segments))
        (raw-message . ,preview)
        (preview . ,preview)
        (message-type . ,(alist-get 'kind origin))
        (origin . ,(copy-tree origin))
        (raw-event . ,(copy-tree source))))))

(defun qq-forward-native-message-to-internal (message)
  "Validate native MESSAGE and map it to viewer-local timeline shape."
  (qq-api--validate-native-forward-message
   message "native forward message" t)
  (let ((segments
         (mapcar #'qq-forward-native-segment-to-internal
                 (let ((value (alist-get 'segments message)))
                   (if (vectorp value) (append value nil) value)))))
    (qq-forward--normalized-message
     message
     segments
     (alist-get 'entry_id message)
     (alist-get 'message_id message)
     (alist-get 'sent_at message)
     (qq-forward--native-sender-fields (alist-get 'sender message))
     (alist-get 'state message)
     (alist-get 'origin message))))

(defun qq-forward--normalize-messages (raw-messages)
  "Validate and map native RAW-MESSAGES for the viewer."
  (mapcar #'qq-forward-native-message-to-internal
          (qq-api-validate-native-forward-messages
           raw-messages "forward viewer messages" t)))

(defun qq-forward--legacy-presentation (data)
  "Extract safe presentation fields from legacy Ark DATA."
  (let (presentation)
    (dolist (key '(source title content summary prompt))
      (when-let* ((value (alist-get key data))
                  (_ (stringp value)))
        (push (cons key value) presentation)))
    (nreverse presentation)))

;;;###autoload
(defun qq-forward-event-segment-to-internal (segment session-key)
  "Translate one root-chat legacy forward SEGMENT at the event boundary.

Canonical native-derived internal segments pass through unchanged.  Legacy
inline content is deliberately ignored: native snapshots are loaded through
the fork-native forward action using an explicit locator-qualified reference."
  (cond
   ((qq-forward-segment-p segment)
    (copy-tree segment))
   ((and (qq-forward--single-alist-p segment)
         (equal (alist-get 'type segment) "forward"))
    (let* ((data (alist-get 'data segment))
           (message-id
            (qq-api-validate-message-id
             (and (listp data) (alist-get 'message_id data))
             "legacy forward event"))
           (chat (qq-api-chat-locator session-key)))
      `((type . "forward")
        (data
         . ((content
             . ((kind . "remote")
                (reference
                 . ((kind . "message")
                    (message_id . ,message-id)
                    (chat . ,chat))))))))))
   ((and (qq-forward--single-alist-p segment)
         (equal (alist-get 'type segment) "card")
         (equal (alist-get 'kind (alist-get 'data segment)) "forward"))
    (let* ((data (alist-get 'data segment))
           (resource-id
            (qq-api-validate-resource-id
             (and (listp data) (alist-get 'res_id data))
             "legacy Ark forward event")))
      `((type . "card")
        (data
         . ((kind . "forward")
            (reference
             . ((kind . "resource") (resource_id . ,resource-id)))
            (presentation . ,(qq-forward--legacy-presentation data)))))))
   (t nil)))

(defun qq-forward--inline-buffer-key (segment)
  "Return UI-only buffer key for inline SEGMENT."
  (format "inline:%x" (sxhash-equal segment)))

(defun qq-forward--buffer-name (buffer-key)
  "Return deterministic forward viewer buffer name for BUFFER-KEY."
  (format "*qq-forward:%s*" buffer-key))

(defun qq-forward--format-time (timestamp)
  "Return display time for integer TIMESTAMP, or an empty string."
  (if (and (integerp timestamp) (> timestamp 0))
      (format-time-string "%Y-%m-%d %H:%M:%S"
                          (seconds-to-time timestamp))
    ""))

(defun qq-forward--entry-properties (message index)
  "Return text properties for normalized MESSAGE at INDEX."
  (list 'qq-forward-message message
        'qq-forward-entry-index index
        'rear-nonsticky '(qq-forward-message qq-forward-entry-index)))

(defun qq-forward--insert-message-body (message prefix-state properties)
  "Insert viewer MESSAGE body while preserving nested forward subtrees."
  (let ((ordinary nil))
    (cl-labels
        ((flush-ordinary
          ()
          (when ordinary
            (let ((body-message (copy-tree message)))
              (setf (alist-get 'segments body-message)
                    (nreverse ordinary))
              (qq-chat--insert-message-body
               body-message prefix-state properties))
            (setq ordinary nil))))
      (dolist (segment (alist-get 'segments message))
        (cond
         ((qq-forward-segment-p segment)
          (flush-ordinary)
          (qq-forward-insert-segment
           segment prefix-state properties))
         (t
          (push segment ordinary))))
      (flush-ordinary)
      (when (null (alist-get 'segments message))
        (qq-chat--insert-message-body message prefix-state properties)))))

(defun qq-forward--message-reply-target (message)
  "Return native reply target cons from normalized MESSAGE, or nil."
  (when-let* ((reply
               (seq-find (lambda (segment)
                           (equal (alist-get 'type segment) "reply"))
                         (alist-get 'segments message)))
              (target (alist-get 'target (alist-get 'data reply))))
    (pcase (alist-get 'kind target)
      ("entry" (cons 'entry (alist-get 'entry_id target)))
      ("unresolved" (cons 'unresolved (alist-get 'message_id target))))))

(defun qq-forward--message-by-target (target)
  "Return viewer-local message matching reply TARGET."
  (pcase (car target)
    ('entry
     (seq-find (lambda (message)
                 (equal (alist-get 'id message) (cdr target)))
               qq-forward--messages))
    ('unresolved nil)))

(defun qq-forward--insert-reply-preview-line
    (target prefix-state properties)
  "Insert viewer-local reply preview for native TARGET.

The target is resolved only inside `qq-forward--messages'; no transient
forwarded message is written to or looked up in `qq-state'."
  (let* ((target-id (cdr target))
         (source (qq-forward--message-by-target target))
         (sender (and source
                      (qq-forward--present-string
                       (alist-get 'sender-name source))))
         (preview (and source
                       (string-trim
                        (or (alist-get 'preview source)
                            (qq-state-message-preview source)
                            ""))))
         (body (cond
                ((and sender preview (not (string-empty-p preview)))
                 (format "%s: %s" sender
                         (truncate-string-to-width preview 56 nil nil t)))
                ((and preview (not (string-empty-p preview)))
                 (truncate-string-to-width preview 64 nil nil t))
                ((and (eq (car target) 'entry) target-id)
                 (format "entry %s" target-id))
                (target-id (format "unresolved message %s" target-id))
                (t "unresolved reply")))
         (target-index (and source
                            (cl-position source qq-forward--messages
                                         :test #'eq)))
         (start (point)))
    (insert (format "↪ %s\n" body))
    (let ((reply-properties
           (append properties
                   (list 'face 'qq-msg-inline-reply
                         'qq-forward-reply-id
                         target-id)
                   (when target-index
                     (let ((action
                            (lambda ()
                              (qq-forward--goto-entry target-index)))
                           (map (make-sparse-keymap)))
                       (set-keymap-parent map button-map)
                       (define-key map (kbd "RET")
                                   (lambda () (interactive) (funcall action)))
                       (define-key map [mouse-1]
                                   (lambda () (interactive) (funcall action)))
                       (list 'mouse-face 'highlight
                             'help-echo "Jump to replied forwarded message"
                             'follow-link t
                             'keymap map
                             'button t
                             'category 'default-button
                             'action (lambda (_button)
                                       (funcall action))))))))
      (add-text-properties start (point) reply-properties))
    (disco-ui-apply-line-prefix start (point) prefix-state)))

(defun qq-forward--insert-message (message index)
  "Insert normalized forward MESSAGE numbered INDEX.

Header matches the official forward list and qq-chat: avatar (when the
sender has a user id) + name + time.  Avatar image updates redisplay via
the existing media-key index (`avatar:USER-ID')."
  (let* ((start (point))
         (properties (qq-forward--entry-properties message index))
         (sender-id (alist-get 'sender-id message))
         (sender (or (qq-forward--present-string
                      (alist-get 'sender-name message))
                     "unknown"))
         (time (qq-forward--format-time (alist-get 'time message)))
         (prefix-state (disco-ui-make-prefix-state "  " "  ")))
    (let ((header-start (point)))
      ;; Same gate as qq-chat: any non-nil sender-id (string or number).
      ;; Anonymous / virtual senders keep sender-id nil and skip the glyph.
      (when sender-id
        (let ((avatar-start (point)))
          (insert (qq-media-avatar-display-string sender-id) " ")
          (add-text-properties
           avatar-start (point)
           '(mouse-face highlight
             help-echo "Open sender avatar"))))
      (insert sender)
      (unless (string-empty-p time)
        (insert "  ")
        (let ((time-start (point)))
          (insert time)
          (add-text-properties time-start (point) '(face shadow))))
      (insert "\n")
      (disco-ui-append-face header-start (point) 'qq-msg-heading)
      (add-text-properties header-start (point) properties))
    (when-let* ((target (qq-forward--message-reply-target message)))
      (qq-forward--insert-reply-preview-line
       target prefix-state properties))
    (qq-forward--insert-message-body message prefix-state properties)
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-forward--entry-positions ()
  "Return an alist mapping rendered entry indexes to buffer positions."
  (let ((position (point-min))
        (limit (point-max))
        (last-index :none)
        positions)
    (while (< position limit)
      (let ((index (get-text-property position 'qq-forward-entry-index)))
        (when (and (integerp index) (not (equal index last-index)))
          (push (cons index position) positions))
        (setq last-index index)
        (setq position
              (or (next-single-property-change
                   position 'qq-forward-entry-index nil limit)
                  limit))))
    (nreverse positions)))

(defun qq-forward--goto-entry (index)
  "Move point to rendered forward entry INDEX and return non-nil on success."
  (when-let* ((position (alist-get index (qq-forward--entry-positions))))
    (goto-char position)
    t))

(defun qq-forward--current-entry-index ()
  "Return entry index at point, accounting for point at buffer end."
  (or (get-text-property (point) 'qq-forward-entry-index)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'qq-forward-entry-index))))

(defun qq-forward--mutate-preserving-view (mutator &optional after-restore)
  "Run MUTATOR while preserving entry-relative point via disco-view.

Uses the same disco-view path as qq-chat, with
`qq-forward-entry-index' as the semantic anchor.  Do not reimplement
telega-save-cursor here."
  (disco-view-render-preserving-position
   mutator
   :anchor-property 'qq-forward-entry-index
   :preserve-window-start t
   :after-restore after-restore))

(defun qq-forward--rebuild-media-index ()
  "Rebuild media-key -> entry-index table for the current forward viewer."
  (unless (hash-table-p qq-forward--media-indexes-by-key)
    (setq qq-forward--media-indexes-by-key (make-hash-table :test #'equal)))
  (clrhash qq-forward--media-indexes-by-key)
  (cl-loop for message in qq-forward--messages
           for index from 0
           do (dolist (key (qq-chat--message-media-cache-keys message))
                (puthash key
                         (cons index
                               (gethash key qq-forward--media-indexes-by-key))
                         qq-forward--media-indexes-by-key))))

(defun qq-forward--indexes-for-media-key (media-key)
  "Return entry indexes affected by MEDIA-KEY in the current viewer."
  (when (and (stringp media-key)
             (hash-table-p qq-forward--media-indexes-by-key))
    (delete-dups
     (copy-sequence (gethash media-key qq-forward--media-indexes-by-key)))))

(defun qq-forward--entry-bounds (index)
  "Return (START . END) buffer bounds for rendered entry INDEX, or nil."
  (let* ((positions (qq-forward--entry-positions))
         (start (alist-get index positions)))
    (when start
      (let ((end
             (or (cl-loop for i from (1+ index)
                          below (1+ (length qq-forward--messages))
                          for pos = (alist-get i positions)
                          when pos return pos)
                 (point-max))))
        (cons start end)))))

(defun qq-forward--redisplay-entry (index)
  "Re-insert a single entry INDEX in place.

This is the forward-viewer analogue of `telega-msg-redisplay' /
`qq-chat--redisplay-node': mutate one message region, never erase the whole
buffer."
  (when-let* ((message (nth index qq-forward--messages))
              (bounds (qq-forward--entry-bounds index)))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end (cdr bounds)))
      (goto-char start)
      (delete-region start end)
      (qq-forward--insert-message message index)
      t)))

(defun qq-forward--redisplay-entries (indexes)
  "Redisplay entry INDEXES without a full buffer erase.

High indexes are processed first so earlier entry bounds remain valid while
regions are replaced.  Point/window are restored through disco-view.
After a real mutation, force-window-update like telega media
callbacks."
  (let* ((indexes (sort (delete-dups (delq nil (copy-sequence indexes))) #'<))
         changed)
    (when indexes
      (qq-forward--mutate-preserving-view
       (lambda ()
         (let ((inhibit-read-only t))
           (save-excursion
             (dolist (index (reverse indexes))
               (when (qq-forward--redisplay-entry index)
                 (setq changed t))))))
       (lambda ()
         (when changed
           (force-window-update (current-buffer)))))
      changed)))

(defun qq-forward--render-body ()
  "Erase and redraw the current forward viewer from buffer-local state.

Caller is responsible for view preservation via
`qq-forward--mutate-preserving-view'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Chat History\n" 'face 'bold))
    (insert (propertize
             (format "%s    n/p: navigate  RET: open nested  g: refresh  q: quit\n\n"
                     (pcase qq-forward--lookup-kind
                       ('message
                        (format "message_id: %s" qq-forward--lookup-id))
                       ('resource
                        (format "resource_id: %s" qq-forward--lookup-id))
                       (_ "inline content")))
             'face 'shadow))
    (cond
     (qq-forward--loading
      (insert (propertize "Loading chat history…\n" 'face 'shadow)))
     (qq-forward--error
      (insert (propertize (format "Unable to load chat history: %s\n"
                                  qq-forward--error)
                          'face 'error)))
     ((null qq-forward--messages)
      (insert (propertize "(empty chat history)\n" 'face 'shadow)))
     (t
      (cl-loop for message in qq-forward--messages
               for index from 0
               do (qq-forward--insert-message message index))))
    (qq-forward--rebuild-media-index)))

(defun qq-forward--render ()
  "Render the current `qq-forward-mode' buffer from buffer-local state.

Full erase is reserved for initial load, refresh, and structural state
changes.  Media cache ticks must use `qq-forward--redisplay-entries'
instead — full rebuilds are what made C-n/C-p feel hard-snapped.

View preservation goes through disco-view, not a local fork."
  (qq-forward--mutate-preserving-view #'qq-forward--render-body))

(defun qq-forward--request-valid-p (buffer source generation)
  "Return non-nil when async result still belongs to BUFFER and GENERATION."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-forward-mode)
              (equal qq-forward--source source)
              (= qq-forward--generation generation)))))

(defun qq-forward--error-text (response reason)
  "Return readable viewer error from RESPONSE and REASON."
  (format "%s"
          (or reason
              (and (listp response)
                   (or (alist-get 'message response)
                       (alist-get 'wording response)))
              "unknown error")))

(defun qq-forward--load-remote ()
  "Start a generation-guarded remote load for the current viewer."
  (let* ((buffer (current-buffer))
         (source (copy-tree qq-forward--source))
         (generation (cl-incf qq-forward--generation)))
    (unless source
      (user-error "qq: this viewer has no remote forward reference"))
    (setq qq-forward--loading t
          qq-forward--error nil)
    (qq-forward--render)
    (condition-case error-data
        (qq-api-get-forward
         source
         (lambda (raw-messages)
           (when (qq-forward--request-valid-p
                  buffer source generation)
             (with-current-buffer buffer
               (let ((messages (qq-forward--normalize-messages raw-messages)))
                 (setq qq-forward--raw-messages (copy-tree raw-messages)
                       qq-forward--messages messages
                       qq-forward--loaded-p t
                       qq-forward--loading nil
                       qq-forward--error nil)
                 (qq-forward--render)))))
         (lambda (response reason)
           (when (qq-forward--request-valid-p
                  buffer source generation)
             (with-current-buffer buffer
               (setq qq-forward--loading nil
                     qq-forward--error
                     (qq-forward--error-text response reason))
               (qq-forward--render)))))
      (error
       (when (qq-forward--request-valid-p
              buffer source generation)
         (setq qq-forward--loading nil
               qq-forward--error (error-message-string error-data))
         (qq-forward--render))))))

(defun qq-forward--load-inline ()
  "Render the authoritative inline subtree in the current viewer."
  (cl-incf qq-forward--generation)
  (setq qq-forward--raw-messages (copy-tree qq-forward--inline-content)
        qq-forward--messages
        (qq-forward--normalize-messages qq-forward--inline-content)
        qq-forward--loaded-p t
        qq-forward--loading nil
        qq-forward--error nil)
  (qq-forward--render))

(defun qq-forward-refresh ()
  "Refresh the current merged-forward viewer.

Inline subtrees are normalized again locally and never refetched.  Remote
records issue a fresh `emacs_get_forward' request."
  (interactive)
  (unless (derived-mode-p 'qq-forward-mode)
    (user-error "qq: not in a forward viewer"))
  (if qq-forward--inline-p
      (qq-forward--load-inline)
    (qq-forward--load-remote)))

(defun qq-forward--move-message (count)
  "Move COUNT entries in the current forward viewer."
  (let* ((positions (qq-forward--entry-positions))
         (total (length positions))
         (current (qq-forward--current-entry-index))
         (base (if (integerp current)
                   current
                 (if (> count 0) -1 total)))
         (target (+ base count)))
    (if (and (>= target 0) (< target total))
        (qq-forward--goto-entry target)
      (message "qq: no %s forwarded message"
               (if (> count 0) "next" "previous")))))

(defun qq-forward-next-message (&optional count)
  "Move to the next COUNT forwarded messages."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (qq-forward-previous-message (- count))
      (qq-forward--move-message count))))

(defun qq-forward-previous-message (&optional count)
  "Move to the previous COUNT forwarded messages."
  (interactive "p")
  (let ((count (or count 1)))
    (if (< count 0)
        (qq-forward-next-message (- count))
      (qq-forward--move-message (- count)))))

(defun qq-forward-activate ()
  "Activate the nested clickable forward block at point."
  (interactive)
  (cond
   ((button-at (point))
    (push-button (point)))
   ((get-text-property (point) 'qq-forward-segment)
    (qq-forward-open-segment
     (get-text-property (point) 'qq-forward-segment)))
   (t
    (user-error "qq: no nested chat history at point"))))

(defvar qq-forward-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'qq-forward-refresh)
    (define-key map (kbd "n") #'qq-forward-next-message)
    (define-key map (kbd "p") #'qq-forward-previous-message)
    (define-key map (kbd "RET") #'qq-forward-activate)
    map)
  "Keymap for `qq-forward-mode'.")

(define-derived-mode qq-forward-mode special-mode "QQ-Forward"
  "Major mode for one QQ merged-forward message tree."
  (setq-local truncate-lines nil)
  (setq-local qq-forward--media-indexes-by-key (make-hash-table :test #'equal))
  (setq-local header-line-format
              '(:eval
                (format " QQ Chat History · %s%s"
                        (or qq-forward--lookup-id "inline")
                        (if qq-forward--loading " · loading" "")))))

(defun qq-forward--open-buffer
    (buffer-key source inline-p inline-content)
  "Open BUFFER-KEY viewer for native SOURCE or INLINE-CONTENT."
  (when source
    (setq source
          (qq-api-validate-forward-source
           source "forward viewer source")))
  (when inline-p
    (setq inline-content
          (qq-api-validate-native-forward-messages
           inline-content "native forward inline content")))
  (unless (or inline-p source)
    (user-error "qq: this chat history has neither inline content nor source"))
  (let* ((reference (and source (qq-forward--source-reference source)))
         (lookup-kind (car reference))
         (lookup-id (cdr reference))
         (buffer (get-buffer-create (qq-forward--buffer-name buffer-key))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-forward-mode)
        (qq-forward-mode))
      (let ((changed (not (equal qq-forward--buffer-key buffer-key))))
        (setq qq-forward--buffer-key buffer-key
              qq-forward--source (copy-tree source)
              qq-forward--lookup-id lookup-id
              qq-forward--lookup-kind lookup-kind)
        (when changed
          (setq qq-forward--raw-messages nil
                qq-forward--messages nil
                qq-forward--loading nil
                qq-forward--loaded-p nil
                qq-forward--error nil
                qq-forward--generation 0))
        (if inline-p
            (progn
              (setq qq-forward--inline-p t
                    qq-forward--inline-content (copy-tree inline-content))
              (qq-forward--load-inline))
          (let ((was-inline qq-forward--inline-p))
            (setq qq-forward--inline-p nil
                  qq-forward--inline-content nil)
            (when was-inline
              (setq qq-forward--loaded-p nil)))
          (when (and (not qq-forward--loaded-p)
                     (not qq-forward--loading)
                     (null qq-forward--error))
            (qq-forward--load-remote)))))
    (pop-to-buffer buffer)
    buffer))

;;;###autoload
(defun qq-forward-open (source)
  "Open a remote merged-forward viewer for explicit native SOURCE."
  (let* ((source (qq-api-validate-forward-source
                  source "forward viewer source"))
         (reference (qq-forward--source-reference source))
         (buffer-key
          (format "%s:%s:%x"
                  (car reference) (cdr reference) (sxhash-equal source))))
    (qq-forward--open-buffer buffer-key source nil nil)))

;;;###autoload
(defun qq-forward-open-segment (segment)
  "Open canonical internal merged-forward SEGMENT."
  (interactive
   (list (get-text-property (point) 'qq-forward-segment)))
  (unless (qq-forward-segment-p segment)
    (user-error "qq: segment is not a canonical merged-forward record"))
  (let* ((inline-cell (qq-forward--inline-cell segment))
         (inline-p (and inline-cell t))
         (inline-content
          (and inline-cell
               (qq-api-validate-native-forward-messages
                (cdr inline-cell) "native forward inline content")))
         (source (qq-forward--segment-source segment))
         (reference (and source (qq-forward--source-reference source)))
         (buffer-key
          (if inline-p
              (qq-forward--inline-buffer-key segment)
            (and source
                 (format "%s:%s:%x"
                         (car reference) (cdr reference)
                         (sxhash-equal source))))))
    (qq-forward--open-buffer
     buffer-key source inline-p inline-content)))

;;;###autoload
(defun qq-forward-insert-segment (segment prefix-state properties)
  "Insert canonical forward SEGMENT as a whole-card action."
  (unless (qq-forward-segment-p segment)
    (error "qq: cannot render non-forward segment"))
  (let* ((data (alist-get 'data segment))
         (presentation
          (and (equal (alist-get 'type segment) "card")
               (alist-get 'presentation data)))
         (source-label
          (qq-forward--present-string (alist-get 'source presentation)))
         (title (qq-forward--present-string (alist-get 'title presentation)))
         (content-preview
          (qq-forward--present-string (alist-get 'content presentation)))
         (summary
          (qq-forward--present-string (alist-get 'summary presentation)))
         (prompt
          (qq-forward--present-string (alist-get 'prompt presentation)))
         (details (delete-dups
                   (delq nil (list content-preview summary prompt))))
         (inline-cell (qq-forward--inline-cell segment))
         (inline-content
          (and inline-cell
               (qq-api-validate-native-forward-messages
                (cdr inline-cell) "native forward inline content")))
         (count (or (qq-forward--inline-message-count inline-content)
                    (qq-forward--count-from-presentation presentation)))
         (source (qq-forward--segment-source segment))
         (reference (and source (qq-forward--source-reference source)))
         (open-action (lambda () (qq-forward-open-segment segment)))
         (map (let ((map (make-sparse-keymap)))
                (set-keymap-parent map button-map)
                (define-key map (kbd "RET")
                            (lambda () (interactive) (funcall open-action)))
                (define-key map [mouse-1]
                            (lambda () (interactive) (funcall open-action)))
                map))
         (card-properties
          (append properties
                  (list 'qq-forward-segment (copy-tree segment)
                        'mouse-face 'highlight
                        'help-echo
                        (if reference
                            (format "Open merged chat history (%s %s)"
                                    (if (eq (car reference) 'resource)
                                        "resource_id"
                                      "message_id")
                                    (cdr reference))
                          "Open inline merged chat history")
                        'follow-link t
                        'keymap map
                        'button t
                        'category 'default-button
                        'action (lambda (_button) (funcall open-action)))))
         (start (point))
         (disco-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (disco-ui-card-prefix-state)))
    (disco-ui-insert-prefixed-lines
     card-prefix-state
     (if source-label
         (format "Chat History · %s" source-label)
       "Chat History")
     :face 'bold
     :properties card-properties)
    (when title
      (disco-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (dolist (detail details)
      (unless (equal detail title)
        (disco-ui-insert-prefixed-lines
         card-prefix-state detail :face 'shadow :properties card-properties)))
    (when (and (numberp count) (> count 0))
      (disco-ui-insert-prefixed-lines
       card-prefix-state
       (format "%d forwarded message%s" count (if (= count 1) "" "s"))
       :face 'shadow
       :properties card-properties))
    (add-text-properties start (point) card-properties)
    (point)))

(defun qq-forward--handle-media-cache-update (&optional media-key)
  "Refresh affected forward entries after MEDIA-KEY cache updates.

Follow telega/qq-chat: never full-erase on media ticks.  Only redisplay
entries that depend on MEDIA-KEY.  Unrelated keys and foreign cache events
are no-ops so open forward viewers stop thrashing while the user navigates."
  (when (stringp media-key)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (derived-mode-p 'qq-forward-mode)
                     (not qq-forward--loading)
                     qq-forward--messages)
            (when-let* ((indexes (qq-forward--indexes-for-media-key media-key)))
              (qq-forward--redisplay-entries indexes))))))))

(add-hook 'qq-media-cache-update-hook #'qq-forward--handle-media-cache-update)

(provide 'qq-forward)

;;; qq-forward.el ends here
