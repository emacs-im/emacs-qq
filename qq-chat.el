;;; qq-chat.el --- Chat buffers for emacs-qq -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Chat buffer built on appkit's shared timeline and presentation primitives,
;; while borrowing
;; telega.el-style naming and the most familiar chat input bindings.

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'button)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-chat-avatar)
(require 'appkit-chatbuf)
(require 'appkit-chat-history)
(require 'appkit-chat-completion)
(require 'appkit-chat-timeline)
(require 'appkit-chat-ins)
(require 'appkit-media)
(require 'appkit-ui)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-completion)
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
(autoload 'qq-guild-user-open "qq-guild-user" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-search-open "qq-search" nil t)
(autoload 'qq-red-packet-open "qq-red-packet" nil t)
(autoload 'qq-chat-forward-transient "qq-transient" nil t)

(declare-function qq-forward-segment-p "qq-forward" (segment))
(declare-function qq-forward-insert-segment
                  "qq-forward" (segment prefix-state properties))
(declare-function qq-forward-event-segment-to-internal
                  "qq-forward" (segment session-key))
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-guild-user-open "qq-guild-user" (guild-id native-id))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-search-open "qq-search" (session-key &optional query))
(declare-function qq-red-packet-open
                  "qq-red-packet"
                  (session-key message-id segment outgoing-p))
(declare-function qq-chat-message-transient "qq-transient" (&rest args))
(declare-function qq-chat-attach-transient "qq-transient" (&rest args))
(declare-function qq-chat-transient "qq-transient" (&rest args))
(declare-function qq-chat-forward-transient "qq-transient" (&rest args))
(declare-function qq-api-forward-messages-individually
                  "qq-api" (source-session-key target-session-key message-ids
                                                callback &optional errback))
(declare-function qq-api-forward-messages-merged
                  "qq-api" (source-session-key target-session-key message-ids
                                                callback &optional errback))
(declare-function qq-api-cancel-request "qq-api" (request-token))
(declare-function qq-api-send-poke
                  "qq-api" (session-key target-id &optional callback errback))
(declare-function qq-api-recall-poke
                  "qq-api" (session-key recall-reference
                                         &optional callback errback))

(defvar-local qq-chat--session-key nil
  "Session key associated with the current chat buffer.")

(defvar-local qq-chat--my-action nil
  "Local outgoing chat-action for this buffer.

Non-nil means we currently advertise typing via `set_input_status'.  This
mirrors telega's `telega-chatbuf--my-action'.")

(defvar-local qq-chat--last-search-query nil
  "Last in-chat search query.")

(defvar-local qq-chat--search-results nil
  "Newest-to-oldest authoritative results for the active in-chat search.")

(defvar-local qq-chat--search-results-tail nil
  "Last cons cell of `qq-chat--search-results' for constant-time append.")

(defvar-local qq-chat--search-seen nil
  "Persistent result-key set for the active in-chat search.")

(defvar-local qq-chat--search-consumed-cursors nil
  "Active-query set of single-use continuation cursors already dispatched.")

(defvar-local qq-chat--search-index nil
  "Index of the active item in `qq-chat--search-results'.")

(defvar-local qq-chat--search-direction 'older
  "Initial authoritative search direction, `older' or `newer'.")

(defvar-local qq-chat--search-anchor nil
  "Search origin plist with exact message id and sequence, or nil.")

(defvar-local qq-chat--search-completed-p nil
  "Non-nil after the active query has produced a terminal outcome.")

(defvar-local qq-chat--search-next-cursor nil
  "Opaque continuation for the active in-chat search.")

(defvar-local qq-chat--search-request nil
  "Transport token for the active in-chat search request.")

(defvar-local qq-chat--search-owner nil
  "Opaque identity owning the active in-chat search request.")

(defvar-local qq-chat--search-highlight-overlays nil
  "Overlays highlighting matched message text, never surrounding UI.")

(defvar-local qq-chat--msg-filter nil
  "Active materialized message filter, or nil.

The plist owns `:title', `:query', newest-first rendering-snapshot `:items',
and the opaque single-use `:next-cursor'.  It lives beside, rather than inside,
AppKit's ordinary continuous-history window: canceling a filter reveals the
same normal projection unless preserving point requires an exact around fetch.")

(defvar-local qq-chat--filter-request nil
  "Transport token for the active materialized-filter page request.")

(defvar-local qq-chat--filter-owner nil
  "Opaque identity owning the active materialized-filter request.")

(defvar-local qq-chat--filter-sync-request nil
  "Settled filter callback awaiting its Appkit projection transaction.

The value is a plist containing the request OWNER and, when applicable, the
OWNER whose first-page semantic point must be restored after projection.")

(defvar-local qq-chat--filter-auto-load-p nil
  "Non-nil when Appkit should continue a duplicate-only filter page.")

(defvar-local qq-chat--callback-sync-request nil
  "Accepted asynchronous state awaiting presentation by its captured view.

The value is a plist containing `:view' and an ordered `:actions' list.
Actions run only after the accepted state has been projected by that exact
live Appkit view.")

(defvar-local qq-chat--send-sync-request nil
  "Failed-send composer state awaiting presentation by its captured view.")

(defvar qq-chat-group-messages t
  "When non-nil, compact consecutive messages from the same sender.")

(defvar qq-chat-group-messages-timespan 300
  "Maximum time gap in seconds used for compact message grouping.")

(defvar-local qq-chat--fill-column nil
  "Cached telega-style timeline width for the active chat window.")

(defconst qq-chat--empty-placeholder :qq-chat-empty-placeholder
  "Sentinel EWOC payload used for the empty timeline note.")

(defvar-local qq-chat--remote-latest-id nil
  "Newest exact server message id observed for this QQ session.

This NapCat/read-state frontier remains client-owned.  Protocol-independent
window edges, loading ownership, exhaustion, and stalls live in AppKit's
buffer-local continuous history controller.")

(defvar-local qq-chat--guild-history-start-sequence nil
  "Lowest native Guild sequence covered by the current history window.")

(defvar-local qq-chat--guild-history-end-sequence nil
  "Highest native Guild sequence covered by the current history window.")

(defvar-local qq-chat--guild-forum-next-cursor nil
  "Opaque native cursor for the next older QQ Guild forum page.")

(defvar-local qq-chat--initial-history-owner nil
  "Opaque owner token for the active initial-history request chain.")

(defvar-local qq-chat--initial-history-request nil
  "Transport request token currently owned by initial-history loading.")

(defvar-local qq-chat--open-message-owner nil
  "Opaque owner for an around-fetch started by `qq-chat-open-message'.")

(defvar-local qq-chat--open-message-request nil
  "Transport token owned by `qq-chat-open-message'.")

(defun qq-chat--reset-history-state ()
  "Reset current buffer history paging state."
  (appkit-chat-history-reset-state)
  (setq qq-chat--remote-latest-id nil
        qq-chat--guild-history-start-sequence nil
        qq-chat--guild-history-end-sequence nil
        qq-chat--guild-forum-next-cursor nil))

(defun qq-chat--guild-forum-session-p (session-key)
  "Return non-nil when SESSION-KEY is a QQ Guild forum channel."
  (when (eq (qq-state-session-key-type session-key) 'guild-channel)
    (let* ((identity (qq-state-session-key-identity session-key))
           (session (qq-state-session session-key))
           (channel
            (qq-state-guild-channel
             (alist-get 'guild-id identity)
             (alist-get 'channel-id identity))))
      (equal (or (alist-get 'channel-kind session)
                 (alist-get 'kind channel))
             "forum"))))

(defun qq-chat--set-history-window (first-message-id last-message-id)
  "Project one contiguous history window from FIRST-MESSAGE-ID through LAST.

Both identifiers remain opaque strings.  A nil FIRST-MESSAGE-ID leaves the
older edge unbounded.  A nil LAST-MESSAGE-ID attaches the window to the live
latest edge; otherwise it is the exact cursor used to fetch newer history."
  (appkit-chat-history-window-set first-message-id last-message-id))

(defun qq-chat--set-empty-history-window ()
  "Establish an authoritative empty window attached to live history."
  (appkit-chat-history-window-establish-empty))

(defun qq-chat--history-window-partial-p ()
  "Return non-nil when newer messages exist outside the projected window."
  (appkit-chat-history-window-partial-p))

(defun qq-chat--history-window-known-p ()
  "Return non-nil after this buffer established an exact history window."
  (appkit-chat-history-window-known-p))

(defun qq-chat--begin-around-history-window
    (&optional remote-latest-id owner)
  "Start an owned around-message window with REMOTE-LATEST-ID frontier.

When OWNER is nil, allocate a fresh opaque token.  Returning and comparing
the token by identity prevents stale callbacks without maintaining a numeric
generation counter."
  (setq owner
        (appkit-chat-history-request-begin
         'around (or owner (list 'history-window qq-chat--session-key))))
  (setq qq-chat--remote-latest-id
        (or remote-latest-id qq-chat--remote-latest-id))
  (appkit-chat-history-older-loaded-set nil)
  (appkit-chat-history-newer-stalled-clear)
  owner)

(defvar-local qq-chat--pending-jump-id nil
  "Server message id to jump to after history loads, or nil.

Mirrors telega's async `telega-chatbuf--goto-msg' when the target is not yet
loaded in the chatbuf.")

(defvar-local qq-chat--messages-pop-ring nil
  "Ring of message anchors for jump-back.

This mirrors telega's `telega-chatbuf--messages-pop-ring'.")

(cl-defstruct
    (qq-chat-message-selection
     (:constructor qq-chat--make-message-selection (anchor owner message)))
  "One stable message-selection membership.

OWNER is an opaque identity token.  A completed asynchronous operation may
remove this membership only while the same OWNER still belongs to ANCHOR."
  anchor
  owner
  message)

(defvar-local qq-chat--message-selection nil
  "Ordered list of `qq-chat-message-selection' memberships.")

(defvar-local qq-chat--forward-request nil
  "Cancelable transport token for the active forward request.")

(defvar-local qq-chat--forward-request-owner nil
  "Opaque owner of the active forward request from this chat buffer.")

(defvar-local qq-chat--forward-sync-request nil
  "Settled forward callback awaiting its Appkit presentation transaction.")

(defvar-local qq-chat--forward-plan-owner nil
  "Opaque runtime/account owner captured by newly created forward plans.")

(defvar qq-chat--last-forward-target-key nil
  "Most recent session key that accepted a forwarded message.")

(defvar qq-chat-forward-target-history nil
  "Minibuffer history for QQ forwarding destinations.")

(cl-defstruct
    (qq-chat-forward-plan
     (:constructor qq-chat--make-forward-plan
                   (buffer session-key anchors messages memberships
                           plan-owner)))
  "Immutable source snapshot passed through the forwarding transient."
  buffer
  session-key
  anchors
  messages
  memberships
  plan-owner)

(defvar-local qq-chat--send-restore-owner nil
  "Opaque owner allowed to restore the most recently cleared failed send.

Any later composer edit, reply change, or send revokes this ownership so a
stale network failure can never overwrite newer user input.")

(defvar-local qq-chat--last-read-target-id nil
  "Newest server message id submitted from this buffer's cursor.")

(defvar-local qq-chat--guild-read-request-p nil
  "Non-nil while this channel buffer is marking its native Guild peer read.")

(defvar qq-chat-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    ;; Message actions at point (telega-style single keys; never steal input).
    (define-key map (kbd "r") #'qq-chat-reply-to-message)
    (define-key map (kbd "d") #'qq-chat-delete-message)
    (define-key map (kbd "f") #'qq-chat-forward-transient)
    (define-key map (kbd "m") #'qq-chat-toggle-message-selection)
    (define-key map (kbd "U") #'qq-chat-clear-message-selection)
    (define-key map (kbd "o") #'qq-chat-open-resource-at-point)
    (define-key map (kbd "a") #'qq-chat-open-avatar-at-point)
    (define-key map (kbd "i") #'qq-chat-open-user-at-point)
    (define-key map (kbd "h") #'qq-chat-open-peer-info)
    (define-key map (kbd "g") #'qq-chat-goto-reply)
    (define-key map (kbd "x") #'qq-chat-goto-pop-message)
    (define-key map (kbd "P") #'qq-chat-poke-sender)
    (define-key map (kbd "!") #'qq-chat-react-to-message)
    (define-key map (kbd "?") #'qq-chat-transient)
    map)
  "Timeline-only keymap active when point is outside the draft region.

Single-key message actions (`r' reply, `d' recall, `!' react, `P' poke sender,
`f' forward, `m' select/unselect, `U' clear selection,
`o' open media, `a' avatar, `i' user,
`g' goto replied-to, `x' pop jump) and the `?' menu apply on the timeline.
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

(defun qq-chat--msg-filter-active-p ()
  "Return non-nil when this chat projects a materialized message filter."
  (and (plist-get qq-chat--msg-filter :active) t))

(defun qq-chat--msg-filter-title ()
  "Return the active message filter's concise human title."
  (or (plist-get qq-chat--msg-filter :title) "filter"))

(defun qq-chat--msg-filter-has-more-p ()
  "Return non-nil when the active filter owns another server page."
  (and (qq-chat--msg-filter-active-p)
       (stringp (plist-get qq-chat--msg-filter :next-cursor))
       (not (string-empty-p (plist-get qq-chat--msg-filter :next-cursor)))))

(defun qq-chat--msg-filter-status ()
  "Return one compact status string for the active message filter."
  (when (qq-chat--msg-filter-active-p)
    (let ((loaded (length (or (plist-get qq-chat--msg-filter :items) '()))))
      (format "Filter: %s · %d%s%s"
              (qq-chat--msg-filter-title)
              loaded
              (if (qq-chat--msg-filter-has-more-p) "+" "")
              (if qq-chat--filter-owner " · searching…" "")))))

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
  "Return the formatted header line for the active chat buffer.

Telega-like: title first, connection only when not connected, unread as a
compact badge.  Peer typing/actions live in the footer prompt delimiter
(see `qq-chat--input-footer-context-text'), not here.  Debug fields stay out
of the default chrome."
  (let* ((session (qq-chat--session))
         (title (or (alist-get 'title session) qq-chat--session-key))
         (status (qq-state-connection-status))
         (unread (or (alist-get 'unread-count session) 0))
         (status-part (if (memq status '(connected ready))
                          ""
                        (format "  [%s]" status)))
         (unread-part (if (> unread 0)
                          (format "  · %d unread" unread)
                        ""))
         (selected-count (length qq-chat--message-selection))
         (selected-part (if (> selected-count 0)
                            (format "  · %d selected" selected-count)
                          ""))
         (forward-part (if qq-chat--forward-request-owner
                           "  · forwarding…"
                         ""))
         (filter-part
          (if-let* ((status (qq-chat--msg-filter-status)))
              (format "  · %s" status)
            ""))
         (search-part
          (if (and qq-chat--last-search-query
                   (not (string-empty-p qq-chat--last-search-query)))
              (format "  · search \"%s\" %s%s"
                      (truncate-string-to-width
                       qq-chat--last-search-query 18 nil nil t)
                      (if qq-chat--search-index
                          (number-to-string (1+ qq-chat--search-index))
                        (if qq-chat--search-completed-p "0" "…"))
                      (if (or qq-chat--search-next-cursor
                              qq-chat--search-request
                              (and qq-chat--search-owner
                                   (plist-get qq-chat--search-owner :pending)))
                          "+" ""))
            "")))
    (format " %s%s%s%s%s%s%s" title status-part unread-part selected-part
            forward-part filter-part search-part)))

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
  (appkit-view-window-fill-column
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
  "Insert TIME at the shared appkit timeline right edge.

LEFT-PREFIX-WIDTH reserves a display-only prefix applied by the caller.
OVERFLOW-NEWLINE-P controls whether an overlong row may move TIME to a new
line; nil keeps service rows such as poke strictly one-line."
  (when (and (stringp time) (not (string-empty-p time)))
    (appkit-chat-ins-insert-right-aligned-text
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

(defun qq-chat--open-message-sender-profile (message)
  "Open MESSAGE sender in its exact native identity domain."
  (let ((session-key (or (alist-get 'session-key message)
                         qq-chat--session-key)))
    (if (and session-key
             (eq (qq-state-session-key-type session-key) 'guild-channel))
        (let* ((identity (qq-state-session-key-identity session-key))
               (guild-id (alist-get 'guild-id identity))
               (native-id (alist-get 'sender-native-id message)))
          (unless (and (qq-protocol--nonzero-decimal-string-p guild-id)
                       (qq-protocol--nonzero-decimal-string-p native-id))
            (user-error "qq: channel message sender has no native profile identity"))
          (qq-guild-user-open guild-id native-id))
      (let ((sender-id (alist-get 'sender-id message)))
        (unless (and (qq-api-user-id-p sender-id)
                     (not (equal sender-id "0")))
          (user-error "qq: sender has no user profile"))
        (qq-user-open sender-id)))))

(defun qq-chat--insert-message-sender (message face)
  "Insert sender label for MESSAGE using FACE.

The primary name is shown first, with a telega-like secondary name trail when
available."
  (let* ((parts (qq-chat--message-sender-display-parts message))
         (primary (car parts))
         (secondary (cdr parts))
         (action (lambda () (qq-chat--open-message-sender-profile message)))
         (help-echo "Open sender profile")
         (properties '(read-only t front-sticky t rear-nonsticky (read-only))))
    (appkit-ui-insert-action-button
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
      (appkit-ui-insert-action-button
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
                  (qq-chat--animated-face-segment-p segment)
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
  (qq-state-message-anchor message))

(defun qq-chat--authoritative-latest-message-id ()
  "Return the best exact latest-message id known for the current session."
  (let ((session (qq-chat--session)))
    (or (alist-get 'read-latest-message-id session)
        (alist-get 'last-message-id session))))

(defun qq-chat--history-batch-bounds (meta)
  "Return (OLDEST . NEWEST) exact ids for history batch META.

Derive ordering from the already normalized session timeline instead of the
wire array: native batch direction is not part of the client identity
contract.  An empty batch, or one not present in canonical state, has no
bounds."
  (let ((ids (plist-get meta :batch-message-ids))
        oldest
        newest)
    (when ids
      (let ((members (make-hash-table :test #'equal)))
        (dolist (id ids)
          (when id
            (puthash (format "%s" id) t members)))
        (dolist (message (qq-state-session-messages qq-chat--session-key))
          (when-let* ((id (alist-get 'server-id message))
                      ((gethash id members)))
            (unless oldest
              (setq oldest id))
            (setq newest id)))))
    (cons oldest newest)))

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
        'qq-chat-message-selected
        (and (qq-chat-message-selected-p message) t)
        'read-only t
        'front-sticky '(read-only)
        'rear-nonsticky '(read-only)))

(defun qq-chat--message-at-point (&optional position)
  "Return message object under POSITION or point, or nil."
  (let* ((position (or position (point)))
         (anchor (or (get-text-property position 'qq-chat-message-anchor)
                     (save-excursion
                       (goto-char position)
                       (get-text-property (line-beginning-position)
                                          'qq-chat-message-anchor))))
         (messages
          (if (qq-chat--msg-filter-active-p)
              (qq-chat--filtered-timeline-messages)
            (qq-state-session-messages qq-chat--session-key))))
    (seq-find
     (lambda (message)
       (equal (qq-chat--message-anchor message) anchor))
     messages)))

(defun qq-chat--latest-server-message ()
  "Return the newest loaded message carrying a canonical server id."
  (seq-find
   (lambda (message)
     (qq-api-message-id-p (alist-get 'server-id message)))
   (reverse (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--latest-visible-server-message ()
  "Return the newest server message in this buffer's contiguous window."
  (seq-find
   (lambda (message)
     (qq-api-message-id-p (alist-get 'server-id message)))
   (reverse (qq-chat--timeline-messages))))

(defun qq-chat--message-index (message-id messages)
  "Return MESSAGE-ID's zero-based position in ordered MESSAGES, or nil."
  (seq-position
   messages message-id
   (lambda (message candidate-id)
     (equal (alist-get 'server-id message) candidate-id))))

(defun qq-chat--message-id-after-p (candidate-id reference-id messages)
  "Return non-nil when CANDIDATE-ID follows REFERENCE-ID in MESSAGES.

Return nil when either id is absent, because an unknown timeline relation
must not be guessed."
  (let ((candidate-index (qq-chat--message-index candidate-id messages))
        (reference-index (qq-chat--message-index reference-id messages)))
    (and candidate-index reference-index
         (> candidate-index reference-index))))

(defun qq-chat--history-frontier-behind-batch-p
    (frontier-id newest-id batch-ids)
  "Return non-nil when canonical order proves FRONTIER-ID is stale.

NEWEST-ID is the normalized newest entry in the current history batch and
BATCH-IDS are the exact batch members.  A frontier present in the batch is
not stale: the batch itself may legitimately extend beyond the snapshot that
started the request.  Identifiers remain opaque; only their positions in the
canonical session timeline are compared."
  (and frontier-id
       newest-id
       (not (member frontier-id batch-ids))
       (qq-chat--message-id-after-p
        newest-id frontier-id
        (qq-state-session-messages qq-chat--session-key))))

(defun qq-chat--read-target-needed-p (message-id)
  "Return non-nil when MESSAGE-ID can advance the known read position."
  (let* ((messages (qq-state-session-messages qq-chat--session-key))
         (session (qq-state-session qq-chat--session-key))
         (unread-count (alist-get 'unread-count session))
         (first-unread (alist-get 'first-unread-message-id session))
         (read-latest (alist-get 'read-latest-message-id session)))
    (and
     (or (null qq-chat--last-read-target-id)
         (qq-chat--message-id-after-p
          message-id qq-chat--last-read-target-id messages))
     (cond
      ((and (integerp unread-count) (= unread-count 0) read-latest)
       (qq-chat--message-id-after-p message-id read-latest messages))
      ((and (integerp unread-count) (> unread-count 0) first-unread)
       (or (equal message-id first-unread)
           (qq-chat--message-id-after-p
            message-id first-unread messages)))
      (t t)))))

(defun qq-chat--mark-message-viewed (message &optional force)
  "Advance native read position through MESSAGE.

With FORCE, submit even when this buffer already requested the same target."
  (if (eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
      (let ((unread (alist-get 'unread-count
                               (qq-state-session qq-chat--session-key))))
        (when (and (integerp unread) (> unread 0)
                   (not qq-chat--guild-read-request-p))
          (let ((buffer (current-buffer)))
            (setq qq-chat--guild-read-request-p t)
            (qq-api-mark-guild-read
             qq-chat--session-key
             (lambda (_navigation)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq qq-chat--guild-read-request-p nil))))
             (lambda (response reason)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq qq-chat--guild-read-request-p nil)))
               (qq-api--default-error response reason))))))
    (when-let* ((message-id (alist-get 'server-id message))
                ((qq-api-message-id-p message-id))
                ((or force (qq-chat--read-target-needed-p message-id))))
      (setq qq-chat--last-read-target-id message-id)
      (qq-api-mark-message-read qq-chat--session-key message-id))))

(defun qq-chat--manage-read-position (&optional position)
  "Advance read state to the message represented by POSITION.

Point in the composer represents the newest loaded message, matching telega's
prompt behavior.  Point on the timeline represents that exact message."
  (when (and qq-auto-mark-read qq-chat--session-key)
    (let ((position (or position (point))))
      (qq-chat--mark-message-viewed
       (if (appkit-chatbuf-point-in-input-p position)
           (qq-chat--latest-visible-server-message)
         (qq-chat--message-at-point position))))))

(defun qq-chat--window-scroll (window _display-start)
  "Advance reads and load newer history from WINDOW's visible timeline edge."
  (when (window-live-p window)
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'qq-chat-mode)
        ;; The selected window's ordinary post-command path owns read state.
        ;; Indirect scrolling can move an inactive window without running it.
        (unless (eq window (selected-window))
          (qq-chat--manage-read-position (window-point window)))
        ;; Mouse-wheel and scroll-bar commands may move the viewport while
        ;; leaving point far above the newer edge.  Use AppKit's clamped
        ;; visible end for both selected and inactive windows.
        (when-let* ((visible-end
                     (appkit-chat-timeline-window-visible-end-position window)))
          (qq-chat--maybe-auto-load-newer visible-end))))))

(defun qq-chat--message-forwardable-p (message)
  "Return non-nil when MESSAGE can be forwarded by its server ID."
  (and (listp message)
       (qq-api-message-id-p (alist-get 'server-id message))
       (not (qq-state-message-recalled-p message))))

(defun qq-chat--forward-source-supported-p
    (&optional style session-key)
  "Return non-nil when SESSION-KEY may submit native forwards using STYLE.

STYLE is `individual', `merged', or nil when any supported style is enough.
DataLine desktop supports only individual forwarding; DataLine mobile supports
neither style."
  (condition-case nil
      (let* ((identity
              (qq-state-session-key-identity
               (or session-key qq-chat--session-key)))
             (type (alist-get 'type identity))
             (variant (alist-get 'variant identity)))
        (and
         (memq style '(nil individual merged))
         (pcase type
           ((or 'private 'group 'service) t)
           ('dataline
            (and (equal variant "desktop")
                 (memq style '(nil individual))))
           (_ nil))))
    (error nil)))

(defun qq-chat--validate-forward-source-session (style session-key)
  "Return SESSION-KEY when it supports native forwarding STYLE."
  (unless (qq-chat--forward-source-supported-p style session-key)
    (user-error "qq: %s forwarding is unavailable from %s"
                (or style 'native) session-key))
  session-key)

(defun qq-chat--message-selection-anchors ()
  "Return the selected message anchors in membership insertion order."
  (mapcar #'qq-chat-message-selection-anchor qq-chat--message-selection))

(defun qq-chat--message-selection-find (anchor)
  "Return the current selection membership for ANCHOR, or nil."
  (seq-find
   (lambda (membership)
     (equal anchor (qq-chat-message-selection-anchor membership)))
   qq-chat--message-selection))

(defun qq-chat--stable-message-order (messages)
  "Return MESSAGES in stable QQ timeline order.

Server time is compared first and exact per-session message sequence second.
When any message in one timestamp bucket lacks a sequence, that entire bucket
uses state insertion order instead.  This makes the comparator transitive
while retaining the only ordering evidence shared by every item.  The
decorated input position settles ties without treating snowflake IDs as
ordering numbers."
  (let ((sequence-complete-by-time (make-hash-table :test #'eql))
        (missing (make-symbol "missing"))
        (indexed
         (cl-loop for message in messages
                  for index from 0
                  collect (cons index message))))
    (dolist (item indexed)
      (let* ((message (cdr item))
             (time
              (qq-state--normalize-time (alist-get 'time message)))
             (previous
              (gethash time sequence-complete-by-time missing))
             (sequence-p
              (qq-protocol--nonzero-decimal-string-p
               (alist-get 'message-seq message))))
        (puthash time
                 (and (or (eq previous missing) previous) sequence-p)
                 sequence-complete-by-time)))
    (mapcar
     #'cdr
     (sort indexed
           (lambda (left right)
             (let ((left-message (cdr left))
                   (right-message (cdr right))
                   (left-sequence (alist-get 'message-seq (cdr left)))
                   (right-sequence (alist-get 'message-seq (cdr right)))
                   (left-time
                    (qq-state--normalize-time
                     (alist-get 'time (cdr left))))
                   (right-time
                    (qq-state--normalize-time
                     (alist-get 'time (cdr right)))))
               (cond
                ((< left-time right-time) t)
                ((> left-time right-time) nil)
                ((and (gethash left-time sequence-complete-by-time)
                      (not (equal left-sequence right-sequence)))
                 (< (qq-protocol-decimal-string-compare
                     left-sequence right-sequence)
                    0))
                ((qq-state--message-sort< left-message right-message) t)
                ((qq-state--message-sort< right-message left-message) nil)
                (t (< (car left) (car right))))))))))

(defun qq-chat--forward-message-candidates ()
  "Return materialized forward candidates in stable timeline order.

Canonical history remains available while a message filter is active.  The
filter-owned snapshot replaces a canonical object with the same anchor,
because that snapshot is the exact projection currently presented to the
user."
  (let ((messages (qq-state-session-messages qq-chat--session-key)))
    (when (qq-chat--msg-filter-active-p)
      (dolist (filtered (qq-chat--filtered-timeline-messages))
        (let ((anchor (qq-chat--message-anchor filtered)))
          (setq messages
                (seq-remove
                 (lambda (message)
                   (equal anchor (qq-chat--message-anchor message)))
                 messages))
          (setq messages (append messages (list filtered))))))
    (dolist (membership qq-chat--message-selection)
      (let ((anchor (qq-chat-message-selection-anchor membership))
            (snapshot (qq-chat-message-selection-message membership)))
        ;; Filter-only selections can outlive their private projection without
        ;; ever entering canonical history.  Re-project permanent tombstones
        ;; at this public boundary so a later recall cannot leave the captured
        ;; live snapshot forwardable.  Do not replace or mutate MEMBERSHIP:
        ;; its opaque owner remains the immutable async-completion identity.
        (when (listp snapshot)
          (setq snapshot
                (qq-state-message-apply-tombstones
                 qq-chat--session-key snapshot)))
        (when (and
               (listp snapshot)
               (not
                (seq-some
                 (lambda (message)
                   (equal anchor (qq-chat--message-anchor message)))
                 messages)))
          ;; A selected filter-only message owns its materialized snapshot.
          ;; Refreshing or closing the filter cannot erase that membership.
          (setq messages (append messages (list snapshot))))))
    (qq-chat--stable-message-order messages)))

(defun qq-chat-message-selected-p (message)
  "Return non-nil when normalized MESSAGE belongs to the selection set."
  (and (qq-chat--message-selection-find (qq-chat--message-anchor message)) t))

(defun qq-chat-selected-messages ()
  "Return selected, forwardable messages in stable timeline order."
  (seq-filter
   (lambda (message)
     (and (qq-chat--message-forwardable-p message)
          (qq-chat-message-selected-p message)))
   (qq-chat--forward-message-candidates)))

(defun qq-chat--prune-message-selection ()
  "Drop memberships whose authoritative candidate is no longer forwardable.

The state accessor intentionally returns a defensive deep copy, so only pay
that cost while a non-empty message selection needs validation.  Header
formatting then reads the buffer-local membership list in constant time.
Canonical candidates remain visible to this validation while a filter owns
the rendered projection; selection-owned snapshots bridge refresh and cancel
gaps before filter-only rows enter canonical history."
  (when qq-chat--message-selection
    (let ((forwardable (make-hash-table :test #'equal)))
      (dolist (message (qq-chat--forward-message-candidates))
        (when (qq-chat--message-forwardable-p message)
          (puthash (qq-chat--message-anchor message) t forwardable)))
      (setq qq-chat--message-selection
            (seq-filter
             (lambda (membership)
               (gethash (qq-chat-message-selection-anchor membership)
                        forwardable))
             qq-chat--message-selection)))))

(defun qq-chat--rekey-message-selection (previous-anchor anchor)
  "Move selection from PREVIOUS-ANCHOR to canonical ANCHOR."
  (when (and previous-anchor anchor (not (equal previous-anchor anchor)))
    (when-let* ((previous
                 (qq-chat--message-selection-find previous-anchor)))
      (if (qq-chat--message-selection-find anchor)
          ;; Preserve the canonical membership's newer opaque owner.
          (setq qq-chat--message-selection
                (delq previous qq-chat--message-selection))
        (setq qq-chat--message-selection
              (mapcar
               (lambda (membership)
                 (if (eq membership previous)
                     (let ((message
                            (copy-tree
                             (qq-chat-message-selection-message membership))))
                       (setf (alist-get 'server-id message) anchor
                             (alist-get 'id message) anchor)
                       (qq-chat--make-message-selection
                        anchor
                        (qq-chat-message-selection-owner membership)
                        message))
                   membership))
               qq-chat--message-selection))))))

(defun qq-chat--forwardable-target-sessions ()
  "Return all sendable forward targets known from sessions and contacts."
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
            (let ((left-title (or (alist-get 'title left) ""))
                  (right-title (or (alist-get 'title right) "")))
              (if (equal left-title right-title)
                  (string-lessp (alist-get 'key left)
                                (alist-get 'key right))
                (string-lessp left-title right-title)))))))

(defun qq-chat--forward-style-label (style)
  "Return the user-facing label for forward STYLE."
  (pcase style
    ('individual "逐条转发")
    ('merged "合并转发")
    (_ (error "qq: unknown forward style %S" style))))

(defun qq-chat--forward-target-type-label (session)
  "Return a concise destination type label for SESSION."
  (pcase (format "%s" (or (alist-get 'type session)
                            (qq-state-session-key-type
                             (alist-get 'key session))))
    ("private" "好友")
    ("group" "群聊")
    (_ "会话")))

(defun qq-chat--read-forward-target (style count)
  "Read a destination for forwarding COUNT messages using STYLE."
  (let* ((sessions (qq-chat--forwardable-target-sessions))
         (choices
          (mapcar
           (lambda (session)
             (let ((key (alist-get 'key session)))
               (cons (format "%s  · %s  [%s]"
                             (or (alist-get 'title session) key)
                             (qq-chat--forward-target-type-label session)
                             key)
                     key)))
           sessions))
         (default
          (car (rassoc qq-chat--last-forward-target-key choices))))
    (unless choices
      (user-error "qq: no forwarding destination is available"))
    (let ((selected
           (completing-read
            (format "%s %d 条消息到: "
                    (qq-chat--forward-style-label style) count)
            choices nil t nil 'qq-chat-forward-target-history default)))
      (or (cdr (assoc selected choices))
          (user-error "qq: invalid forwarding destination")))))

(defun qq-chat-toggle-message-selection ()
  "Toggle the message at point in the stable selection and move forward."
  (interactive)
  ;; Keep the direct `m' command behind the same closed source capability
  ;; gate as plan creation and submission.  Unsupported sessions must never
  ;; accumulate a selection that no forwarding style can consume.
  (qq-chat--validate-forward-source-session nil qq-chat--session-key)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (anchor (qq-chat--message-anchor message))
         (membership (qq-chat--message-selection-find anchor)))
    (unless (qq-chat--message-forwardable-p message)
      (user-error "qq: this message cannot be forwarded"))
    (if membership
        (setq qq-chat--message-selection
              (delq membership qq-chat--message-selection))
      (setq qq-chat--message-selection
            (append
             qq-chat--message-selection
             (list
              (qq-chat--make-message-selection
               anchor
               (list 'message-selection anchor)
               (copy-tree message))))))
    (qq-chat--request-row-redisplay (list anchor))
    (qq-chat--header-line-update)
    (message "qq: 已选择 %d 条消息"
             (length qq-chat--message-selection))
    ;; Match telega: repeated `m' walks through the timeline, but the final
    ;; message never drops point into the composer.
    (let ((position (qq-chat--current-message-position)))
      (when (and position
                 (seq-some (lambda (candidate) (> candidate position))
                           (qq-chat--message-positions)))
        (qq-chat-next-message)))))

(defun qq-chat-clear-message-selection (&optional quiet)
  "Clear the current message selection.

Suppress the status message when QUIET is non-nil."
  (interactive)
  (let ((anchors (qq-chat--message-selection-anchors)))
    (setq qq-chat--message-selection nil)
    (when anchors
      (qq-chat--request-row-redisplay anchors))
    (qq-chat--header-line-update)
    (unless quiet
      (message "qq: 已清除消息选择"))))

(defun qq-chat--current-forward-plan ()
  "Return a stable forward plan from selected messages or point."
  (qq-chat--validate-forward-source-session nil qq-chat--session-key)
  (qq-chat--prune-message-selection)
  (let* ((selection-p (and qq-chat--message-selection t))
         (messages
          (if selection-p
              (qq-chat-selected-messages)
            (let ((message (qq-chat--message-at-point)))
              (and message (list message)))))
         (anchors (mapcar #'qq-chat--message-anchor messages))
         (memberships
          (when selection-p
            (mapcar
             (lambda (anchor)
               (let ((membership
                      (qq-chat--message-selection-find anchor)))
                 (unless membership
                   (error "qq: selected message %s lost its membership"
                          anchor))
                 (cons anchor
                       (qq-chat-message-selection-owner membership))))
             anchors))))
    (unless messages
      (user-error "qq: select a message to forward"))
    (dolist (message messages)
      (unless (qq-chat--message-forwardable-p message)
        (user-error "qq: selected message %s cannot be forwarded"
                    (qq-chat--message-anchor message))))
    (qq-chat--make-forward-plan
     (current-buffer)
     qq-chat--session-key
     anchors
     (copy-tree messages)
     memberships
     qq-chat--forward-plan-owner)))

(defun qq-chat--forward-plan-messages (plan)
  "Return PLAN's immutable message snapshots in their original order."
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (let ((buffer (qq-chat-forward-plan-buffer plan))
        (session-key (qq-chat-forward-plan-session-key plan))
        (anchors (qq-chat-forward-plan-anchors plan))
        (messages (qq-chat-forward-plan-messages plan))
        (plan-owner (qq-chat-forward-plan-plan-owner plan)))
    (unless (buffer-live-p buffer)
      (user-error "qq: forwarding source buffer no longer exists"))
    (with-current-buffer buffer
      (unless (and (derived-mode-p 'qq-chat-mode)
                   (equal qq-chat--session-key session-key))
        (user-error "qq: forwarding source buffer changed sessions"))
      (unless (and plan-owner (eq plan-owner qq-chat--forward-plan-owner))
        (user-error "qq: forwarding plan belongs to a stale runtime"))
      (unless (and (consp messages)
                   (= (length anchors) (length messages)))
        (user-error "qq: forwarding plan has inconsistent messages"))
      (let ((projected
             (mapcar
              (lambda (message)
                (qq-state-message-apply-tombstones session-key message))
              messages)))
        (cl-loop for anchor in anchors
                 for message in projected
                 unless (and (equal anchor (qq-chat--message-anchor message))
                             (qq-chat--message-forwardable-p message))
                 do (user-error
                     "qq: forwarding plan message %s is invalid" anchor))
        (copy-tree projected)))))

(defun qq-chat--forward-request-current-p (buffer session-key owner)
  "Return non-nil when OWNER still owns BUFFER's forwarding request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (qq-chat--captured-view-current-p
               (plist-get owner :view))
              (eq qq-chat--forward-request-owner owner)))))

(defun qq-chat--cancel-forward-request ()
  "Cancel and forget the active forwarding request in this buffer."
  (let ((request qq-chat--forward-request))
    ;; Invalidate callback ownership before transport cancellation.  A
    ;; synchronous cancellation callback must already be stale, especially at
    ;; the runtime/account reset boundary.
    (setq qq-chat--forward-request nil
          qq-chat--forward-request-owner nil
          qq-chat--forward-sync-request nil)
    (when request
      (condition-case nil
          (qq-api-cancel-request request)
        (quit
         (setq quit-flag nil)
         nil)
        (error nil)))))

(defun qq-chat--forward-succeeded
    (buffer session-key owner anchors memberships style target _response)
  "Settle successful forward OWNER and remove captured MEMBERSHIPS."
  (when (qq-chat--forward-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view))
            removed)
        (dolist (snapshot memberships)
          (let* ((anchor (car snapshot))
                 (membership (qq-chat--message-selection-find anchor)))
            (when (and membership
                       (eq (qq-chat-message-selection-owner membership)
                           (cdr snapshot)))
              (setq qq-chat--message-selection
                    (delq membership qq-chat--message-selection))
              (push anchor removed))))
        (setq qq-chat--forward-request nil
              qq-chat--forward-request-owner nil)
        (when view
          (setq qq-chat--forward-sync-request
                (list :kind 'forward-settlement :owner owner :view view))
          (appkit-request-sync
           view :part 'frame :entries (nreverse removed)))))
    (setq qq-chat--last-forward-target-key target)
    (message "qq: 已%s %d 条消息到 %s"
             (qq-chat--forward-style-label style)
             (length anchors)
             target)))

(defun qq-chat--forward-failed (buffer session-key owner response reason)
  "Settle failed forward OWNER while preserving its message selection."
  (when (qq-chat--forward-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setq qq-chat--forward-request nil
              qq-chat--forward-request-owner nil)
        (when view
          (setq qq-chat--forward-sync-request
                (list :kind 'forward-settlement :owner owner :view view))
          (appkit-request-sync view :part 'frame))))
    (qq-api--default-error response reason)))

(defun qq-chat--submit-forward (style plan &optional target-session-key)
  "Submit PLAN using STYLE to TARGET-SESSION-KEY.

STYLE is `individual' or `merged'.  Cancellation and failures preserve the
selection; success removes only the immutable selection snapshot in PLAN."
  (unless (qq-chat-forward-plan-p plan)
    (user-error "qq: invalid forwarding plan"))
  (let* ((buffer (qq-chat-forward-plan-buffer plan))
         (session-key (qq-chat-forward-plan-session-key plan))
         (_validated-source
          (qq-chat--validate-forward-source-session style session-key))
         (messages (qq-chat--forward-plan-messages plan))
         (anchors (copy-sequence (qq-chat-forward-plan-anchors plan)))
         (memberships
          (copy-sequence (qq-chat-forward-plan-memberships plan)))
         ids target owner request request-installed-p)
    (with-current-buffer buffer
      (when qq-chat--forward-request-owner
        (user-error "qq: another forwarding request is already in progress"))
      (setq target
            (or target-session-key
                (qq-chat--read-forward-target style (length messages))))
      ;; The minibuffer permits process filters and recursive commands.  Treat
      ;; its return as a fresh ownership boundary: another request, a reset,
      ;; or a recall may have invalidated every pre-prompt observation.
      (when qq-chat--forward-request-owner
        (user-error "qq: another forwarding request is already in progress"))
      (qq-chat--validate-forward-source-session style session-key)
      (setq messages (qq-chat--forward-plan-messages plan)
            ids (mapcar (lambda (message) (alist-get 'server-id message))
                        messages)
            owner (list :kind 'forward
                        :style style
                        :session-key session-key
                        :target target
                        :anchors anchors
                        :view (qq-chat--ensure-view))
            qq-chat--forward-request-owner owner)
      (condition-case error-data
          (progn
            ;; Acquiring the owner and reflecting it in the header is part of
            ;; the pre-dispatch transaction.  A rendering error must not leave
            ;; an owner behind that permanently suppresses retries.
            (qq-chat--header-line-update)
            ;; Extend the transport's inhibited handoff through installation
            ;; of its returned token in this buffer.  A C-g arriving after the
            ;; socket handoff but before this assignment is deferred until the
            ;; request has a cancelable, callback-owned identity here.
            (let ((inhibit-quit t))
              (setq request
                    (funcall
                     (pcase style
                       ('individual #'qq-api-forward-messages-individually)
                       ('merged #'qq-api-forward-messages-merged)
                       (_ (error "qq: unknown forward style %S" style)))
                     session-key target ids
                     (apply-partially
                      #'qq-chat--forward-succeeded
                      buffer session-key owner anchors memberships style target)
                     (apply-partially
                      #'qq-chat--forward-failed buffer session-key owner)))
              ;; A test adapter or transport may settle synchronously.  Never
              ;; install its already-finished request token afterward.
              (when (qq-chat--forward-request-current-p
                     buffer session-key owner)
                (setq qq-chat--forward-request request
                      request-installed-p (and request t)))
              ;; Restoring `inhibit-quit' does not guarantee an immediate
              ;; check of `quit-flag'.  Turn a quit from this outer handoff
              ;; into one synchronous signal only after token installation,
              ;; so it cannot leak to an unrelated caller safe point.
              (when quit-flag
                (setq quit-flag nil)
                (signal 'quit nil)))
            request)
        ((error quit)
         ;; Once a non-nil token is installed, the action may already have
         ;; been delivered.  Keep ownership until its response, timeout, or
         ;; explicit cancellation; clearing it here would permit a duplicate
         ;; retry with unknown first-delivery status.
         (unless request-installed-p
           (when (eq qq-chat--forward-request-owner owner)
             (setq qq-chat--forward-request nil
                   qq-chat--forward-request-owner nil)
             (ignore-errors (qq-chat--header-line-update))))
         ;; An automatically delivered deferred quit may leave `quit-flag'
         ;; set.  Consume that flag before re-signalling exactly once.
         (when (eq (car error-data) 'quit)
           (setq quit-flag nil))
         (signal (car error-data) (cdr error-data)))))))

(defun qq-chat-forward-individually (&optional plan target-session-key)
  "Forward PLAN as individual messages to TARGET-SESSION-KEY."
  (interactive)
  (qq-chat--submit-forward
   'individual (or plan (qq-chat--current-forward-plan)) target-session-key))

(defun qq-chat-forward-merged (&optional plan target-session-key)
  "Forward PLAN as one merged-forward card to TARGET-SESSION-KEY."
  (interactive)
  (qq-chat--submit-forward
   'merged (or plan (qq-chat--current-forward-plan)) target-session-key))

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

(defun qq-chat--composer-boundary-valid-p ()
  "Return non-nil when live input is wholly after the EWOC footer."
  (let ((input (appkit-chatbuf-input-start-position)))
    (or (not (appkit-chat-timeline-live-p))
        (null input)
        (let ((footer (appkit-chat-timeline-footer-start-position))
              (prompt (appkit-chatbuf-prompt-start-position)))
          (and footer prompt (<= footer prompt input))))))

(defun qq-chat--canonical-input-contaminated-p ()
  "Return non-nil when canonical input contains timeline-owned text."
  (let ((state (appkit-chatbuf-input-state)))
    (and (> (length state) 0)
         (text-property-not-all
          0 (length state) 'qq-chat-message-anchor nil state))))

(defun qq-chat--assert-canonical-input-clean ()
  "Reject canonical input polluted with rendered timeline rows."
  (when (qq-chat--canonical-input-contaminated-p)
    (error "qq: canonical input contains rendered message rows")))

(defun qq-chat--sync-draft-from-buffer ()
  "Sync canonical draft from the editable buffer region."
  (let ((result
         (if (qq-chat--composer-boundary-valid-p)
             (appkit-chatbuf-input-state-sync)
           ;; Never let a transient/corrupted marker turn the timeline into a
           ;; canonical draft.  A later frame invariant will surface the bad
           ;; boundary without multiplying message text on every rebuild.
           (list :changed-p nil
                 :value (appkit-chatbuf-input-state)
                 :invalid-boundary-p t))))
    (when (plist-get result :changed-p)
      ;; Any real composer mutation revokes a cleared send's right to restore
      ;; its old draft after a late transport failure.
      (setq qq-chat--send-restore-owner nil)
      (qq-chat--maybe-update-my-action-from-input))
    (plist-get result :value)))

(defun qq-chat--reply-message ()
  "Return current reply target message from shared aux state, or nil."
  (let ((state (appkit-chatbuf-aux-state)))
    (and (eq (plist-get state :aux-type) 'reply)
         (plist-get state :aux-msg))))

(defun qq-chat--set-reply-message (message)
  "Set shared reply aux state to MESSAGE, or clear it when nil."
  (setq qq-chat--send-restore-owner nil)
  (if message
      (appkit-chatbuf-aux-set
       (list :aux-type 'reply
             :aux-msg message
             :message-id (alist-get 'server-id message)))
    (appkit-chatbuf-aux-reset)))

(defun qq-chat--after-change (beg end old-len)
  "Keep draft state synced after editable-region changes from BEG to END."
  (appkit-chatbuf-after-change
   beg end
   :old-length old-len
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
  (when (and qq-chat--session-key
             (not (appkit-chatbuf-point-in-input-p)))
    (if (qq-chat--msg-filter-active-p)
        (when (and (numberp qq-chat-history-auto-load-threshold)
                   (<= (point)
                       (+ (point-min)
                          (max 0 qq-chat-history-auto-load-threshold))))
          (when-let* ((view (qq-chat--live-current-view)))
            (qq-chat--request-callback-sync
             view (lambda () (qq-chat-filter-load-more t)))))
      (when (appkit-chat-history-autoload-older-p
             (point) (point-min) qq-chat-history-auto-load-threshold)
        (when-let* ((view (qq-chat--live-current-view)))
          (qq-chat--request-callback-sync
           view (lambda () (qq-chat-load-older-messages t))))))))

(defun qq-chat--maybe-auto-load-newer (&optional position)
  "Load a newer page when POSITION approaches the timeline footer."
  (let ((position (or position (point)))
        (footer (or (appkit-chat-timeline-footer-start-position)
                    (appkit-chatbuf-input-start-position)
                    (point-max))))
    (when (and qq-chat--session-key
               (not (qq-chat--msg-filter-active-p))
               (appkit-chat-history-autoload-newer-p
               position footer qq-chat-history-auto-load-threshold
               (appkit-chatbuf-composer-idle-p)))
      (when-let* ((view (qq-chat--live-current-view)))
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat-load-newer-messages t)))))))

(defun qq-chat--post-command ()
  "Keep point inside the logical draft area when editing input."
  (unless (appkit-chatbuf-rendering-p)
    (appkit-chatbuf-post-command-clamp-point)
    (when (and (appkit-chatbuf-point-in-input-p)
               (appkit-chatbuf-input-has-objects-p))
      (appkit-chatbuf-input-prune-broken-objects))
    (qq-chat--flush-deferred-node-redisplay)
    (qq-chat--update-context-mode)
    ;; A partial around-message window behaves like telega: approaching its
    ;; lower edge extends the continuous slice without inserting a gap row.
    (qq-chat--maybe-auto-load-newer)
    (qq-chat--maybe-auto-load-older)
    (qq-chat--manage-read-position)))

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
  (qq-chat--assert-canonical-input-clean)
  (appkit-chatbuf-init-state 32)
  (appkit-chatbuf-bind-input-region
   :visible-p (qq-chat--composer-visible-p)
   :prompt (qq-chat--prompt-text)
   :input-text (appkit-chatbuf-input-state)
   :post-bind-function #'appkit-chatbuf-input-apply-text-properties))

(defun qq-chat--header-text ()
  "Return the empty EWOC header used by the QQ chat timeline.

Like telega, paging state belongs to the prompt delimiter rather than a
persistent prose row above the message history."
  "")

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

(defun qq-chat--history-delimiter-line ()
  "Return a telega-style delimiter reflecting the current history edge."
  (let ((width (max 8 (or qq-chat--fill-column 60))))
    (if-let* ((status (qq-chat--msg-filter-status)))
        (let* ((label
                (truncate-string-to-width
                 (concat " " status " ") width nil nil "…"))
               (remaining (max 0 (- width (string-width label))))
               (left (/ remaining 2)))
          (propertize
           (concat (make-string left ?·)
                   label
                   (make-string (- remaining left) ?·))
           'face 'shadow))
      (appkit-chat-history-delimiter-string
       width :loading-text "加载中…"))))

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
     (qq-chat--history-delimiter-line)
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
  (let (keys)
    (when-let* ((avatar-key
                 (qq-media-message-avatar-cache-key message)))
      (push avatar-key keys))
    (dolist (part (alist-get 'parts
                             (qq-state-gray-tip-message-data message)))
      (when (equal (alist-get 'type part) "user")
        (push (format "avatar:%s" (alist-get 'user-id part)) keys)))
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

(defun qq-chat--history-window-slice (messages)
  "Return the one contiguous buffer-local slice of ordered MESSAGES."
  (if (appkit-chat-history-window-empty-p)
      ;; An authoritative empty remote window may still own optimistic local
      ;; rows created after the snapshot.  They become normal window entries
      ;; when their explicit local→server rekey seeds the first exact edge.
      (seq-filter (lambda (message)
                    (not (qq-api-message-id-p
                          (alist-get 'server-id message))))
                  messages)
    (let ((slice
           (appkit-chat-history-window-slice
            messages #'qq-chat--message-anchor)))
      (and (plist-get slice :valid-p)
           (plist-get slice :entries)))))

(defun qq-chat--filter-result-id (result)
  "Return RESULT's exact NT message id, or nil."
  (let ((id (and (listp result) (plist-get result :message-id))))
    (and (qq-api-message-id-p id) id)))

(defun qq-chat--filter-patch-replay-p (owner entry)
  "Return non-nil when OWNER may replay request-local patch ENTRY.

Recall is an irreversible tombstone.  Reaction patches are deltas or aggregate
observations and only belong to a response whose request started before the
notice was observed."
  (pcase (plist-get (plist-get entry :patch) :kind)
    ('recall t)
    ('emoji-like
     (let ((request-token (plist-get owner :observation-token))
           (event-token (plist-get entry :observation-token)))
       (unless (and (integerp request-token) (integerp event-token))
         (error "qq: reaction filter patch lacks exact observation tokens"))
       (> event-token request-token)))
    (kind (error "qq: unsupported filter message patch kind %S" kind))))

(defun qq-chat--normalize-filter-result (owner result)
  "Convert wire RESULT into a request-owned local item for OWNER."
  (let* ((id (alist-get 'message_id result))
         (message
          (qq-state-message-apply-tombstones
           qq-chat--session-key
           (qq-state-normalize-message-snapshot
            qq-chat--session-key result))))
    (unless (equal (alist-get 'server-id message) id)
      (error "qq: filter result message identity changed during normalization"))
    (dolist (entry (plist-get owner :patches))
      (when (and (equal (plist-get entry :message-id) id)
                 (qq-chat--filter-patch-replay-p owner entry))
        (setq message
              (qq-state-message-apply-patch
               message (plist-get entry :patch)))))
    (list :message-id id
          :message message)))

(defun qq-chat--apply-filter-message-patch
    (message-id patch observation-token)
  "Apply exact PATCH to filter-owned MESSAGE-ID without touching history.

OBSERVATION-TOKEN is the matching state event observation.  A pending request
keeps the patch only until its callback."
  (when (and (qq-chat--msg-filter-active-p)
             (qq-api-message-id-p message-id))
    (let ((kind (plist-get patch :kind))
          (patch-token (plist-get patch :observation-token)))
      (unless (memq kind '(recall emoji-like))
        (error "qq: unsupported filter message patch kind %S" kind))
      (unless (and (integerp observation-token)
                   (equal patch-token observation-token))
        (error "qq: message patch contradicts its state observation")))
    (when (and (listp qq-chat--filter-owner)
               (plist-get qq-chat--filter-owner :pending))
      (setf (plist-get qq-chat--filter-owner :patches)
            (nconc (plist-get qq-chat--filter-owner :patches)
                   (list (list :message-id message-id
                               :patch (copy-tree patch)
                               :observation-token observation-token)))))
    (let (changed items)
      (dolist (item (or (plist-get qq-chat--msg-filter :items) '()))
        (if (equal (qq-chat--filter-result-id item) message-id)
            (let ((updated (copy-sequence item)))
              (setq updated
                    (plist-put
                     updated :message
                     (qq-state-message-apply-patch
                      (plist-get item :message) patch)))
              (push updated items)
              (setq changed t))
          (push item items)))
      (when changed
        (setq items (nreverse items)
              qq-chat--msg-filter
              (plist-put (copy-sequence qq-chat--msg-filter) :items items))
        ;; An append owner captured the pre-request list.  Patch that private
        ;; snapshot too, so its eventual page cannot resurrect stale content.
        (when (and (listp qq-chat--filter-owner)
                   (plist-get qq-chat--filter-owner :pending))
          (setf (plist-get qq-chat--filter-owner :existing) items)))
      changed)))

(defun qq-chat--filtered-timeline-messages ()
  "Return active filter hits in native oldest-to-newest result order.

Search pages arrive newest-first.  Their filter-owned snapshots are the sole
projection baseline; canonical history may contain a stale object with the
same id or unrelated history islands and must not replace a search hit."
  (delq nil
        (mapcar
         (lambda (item) (plist-get item :message))
         (reverse (copy-sequence
                   (or (plist-get qq-chat--msg-filter :items) '()))))))

(defun qq-chat--timeline-messages (&optional messages)
  "Return visible messages in the current contiguous history window."
  (let ((projected
         (if (qq-chat--msg-filter-active-p)
             (qq-chat--filtered-timeline-messages)
           (when (qq-chat--history-window-known-p)
             (qq-chat--history-window-slice
              (or messages
                  (and qq-chat--session-key
                       (qq-state-session-messages qq-chat--session-key))
                  '()))))))
    (seq-filter #'qq-chat--message-visible-in-timeline-p projected)))

(defun qq-chat--project-timeline (messages)
  "Project visible QQ MESSAGES into shared timeline rows."
  (if (null messages)
      (let* ((state (cond
                     ((and (qq-chat--msg-filter-active-p)
                           qq-chat--filter-owner)
                      'searching)
                     ((qq-chat--msg-filter-active-p) 'no-match)
                     (t 'normal)))
             (placeholder (list :kind qq-chat--empty-placeholder
                                :state state)))
        (list (appkit-chat-timeline-row-create
               :key qq-chat--empty-placeholder
               :payload placeholder
               :context (list :state state))))
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

(defun qq-chat--live-current-view ()
  "Return this buffer's live canonical chat view, or nil when detached."
  (let ((view (appkit-current-view)))
    (and qq-chat--session-key
         (derived-mode-p 'qq-chat-mode)
         (appkit-view-live-p view)
         (eq (appkit-view-buffer view) (current-buffer))
         (equal (appkit-view-id view) (qq-chat--view-id))
         (equal (appkit-view-state view) qq-chat--session-key)
         view)))

(defun qq-chat--captured-view-current-p (view)
  "Return non-nil when VIEW is this buffer's live canonical chat view."
  (and view (eq view (qq-chat--live-current-view))))

(defun qq-chat--request-callback-sync (view &optional action)
  "Project accepted callback state through captured VIEW.

When ACTION is non-nil, run it inside VIEW's sync transaction after the state
projection.  A replacement or detached view is inert."
  (when (and (qq-chat--captured-view-current-p view)
             (or (null action) (functionp action)))
    (let ((request
           (if (eq view (plist-get qq-chat--callback-sync-request :view))
               qq-chat--callback-sync-request
             (list :view view :actions nil))))
      (when action
        (setf (plist-get request :actions)
              (append (plist-get request :actions) (list action))))
      (setq qq-chat--callback-sync-request request)
      (if action
          (appkit-request-sync
           view :structure t :parts '(timeline frame) :position t)
        (appkit-request-sync
         view :structure t :parts '(timeline frame))))))

(defun qq-chat--apply-state-event (event)
  "Apply queued QQ state EVENT inside an appkit sync transaction."
  (let ((event-session-key (plist-get event :session-key))
        (event-type (plist-get event :type))
        (event-mutation (plist-get event :mutation)))
    (pcase event-type
      ('message
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--apply-message-state-change event)))
      ('history
       (when (equal event-session-key qq-chat--session-key)
         (qq-chat--header-line-update)
         (qq-chat--update-frame)
         (qq-chat--sync-timeline)))
      ('connection
       (qq-chat--header-line-update))
      ('reset
       (qq-chat--clear-search-highlights)
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
       (qq-chat--header-line-update))
      ((or 'friends-refreshed 'groups-refreshed)
       (qq-chat--header-line-update)
       (qq-chat--sync-timeline
        :force-keys (appkit-chat-timeline-keys))))))

(defun qq-chat--sync-invalidations (view invalidations)
  "Synchronize current chat from coalesced appkit INVALIDATIONS."
  (let* ((events (appkit-view-pending-events-snapshot view))
         (parts (appkit-invalidations-parts invalidations))
         (geometry-p (memq 'geometry parts))
         (resources (appkit-invalidations-resource-keys invalidations))
         (entries (appkit-invalidations-entry-keys invalidations))
         (raw-forward-sync-request qq-chat--forward-sync-request)
         (forward-sync-request
          (and (or (null (plist-get raw-forward-sync-request :view))
                   (eq view (plist-get raw-forward-sync-request :view)))
               raw-forward-sync-request))
         (raw-filter-sync-request qq-chat--filter-sync-request)
         (filter-sync-request
          (and (or (null (plist-get raw-filter-sync-request :view))
                   (eq view (plist-get raw-filter-sync-request :view)))
               raw-filter-sync-request))
         (filter-auto-load-p qq-chat--filter-auto-load-p)
         (raw-callback-sync-request qq-chat--callback-sync-request)
         (callback-sync-request
          (and (eq view (plist-get raw-callback-sync-request :view))
               raw-callback-sync-request))
         (callback-actions
          (copy-sequence (plist-get callback-sync-request :actions)))
         (raw-send-sync-request qq-chat--send-sync-request)
         (send-sync-request
          (and (eq view (plist-get raw-send-sync-request :view))
               raw-send-sync-request))
         (filter-point-owner
          (plist-get filter-sync-request :point-owner))
         ;; Point ownership must be sampled before geometry or queued events
         ;; mutate generated content and advance the buffer modification tick.
         (filter-point-owned-p
          (and filter-point-owner
               (qq-chat--filter-point-state-current-p filter-point-owner))))
    (when geometry-p
      (when-let* ((window (qq-chat--render-window))
                  (next (qq-chat--compute-fill-column window))
                  (_ (and (integerp next) (> next 15))))
        (setq-local qq-chat--fill-column next
                    fill-column next)))
    ;; A failed send already restored canonical composer state.  Materialize
    ;; it before any full render can synchronize the still-empty tail back
    ;; over that authoritative state.
    (when send-sync-request
      (qq-chat--render-canonical-input))
    (dolist (event events)
      (when (appkit-view-live-p view)
        (qq-chat--apply-state-event event)))
    (when (appkit-view-live-p view)
      (let ((point-owner filter-point-owner)
            rendered-p)
        (cond
         ;; A materialized-filter completion changed buffer-local projection
         ;; state outside this transaction.  Reconcile it here even when its
         ;; invalidation coalesced with queued state events.
         ((or filter-sync-request callback-sync-request)
          (qq-chat-render)
          (setq rendered-p t))
         (geometry-p
          (qq-chat--sync-timeline
           :force-keys
           (and (appkit-chat-timeline-live-p)
                (appkit-chat-timeline-keys))
           :changed-resources resources)
          (qq-chat--update-frame))
         ;; Forward settlement owns precise row redraws plus frame state.  Do
         ;; not turn its `frame' request into a full chat render.
         (forward-sync-request
          (when (or resources entries)
            (qq-chat--sync-timeline
             :force-keys entries
             :changed-resources resources))
          (qq-chat--update-frame))
         (send-sync-request
          (qq-chat--update-frame))
         ((and (null events)
               (or (appkit-invalidations-structure-p invalidations)
                   (appkit-invalidations-parts invalidations)
                   (appkit-invalidations-position-p invalidations)))
          (qq-chat-render)
          (setq rendered-p t))
         ((or resources entries)
          (qq-chat--sync-timeline
           :force-keys entries
           :changed-resources resources)))
        ;; A full render reconciles structure and frame, but equal row payloads
        ;; are not reprinted.  Consume precise entry/resource invalidations too
        ;; so selection and dependency-only presentation changes are not lost.
        (when (and rendered-p (or geometry-p resources entries))
          (qq-chat--sync-timeline
           :force-keys
           (if geometry-p
               (delete-dups
                (append entries (appkit-chat-timeline-keys)))
             entries)
           :changed-resources resources))
        (when (and filter-sync-request filter-point-owned-p)
          (qq-chat--position-after-filter-first-page point-owner))
        ;; Forward settlement changes the mode-line header even when its frame
        ;; invalidation coalesced with an event-specific projection above.
        (when forward-sync-request
          (qq-chat--header-line-update))
        (when (eq raw-forward-sync-request qq-chat--forward-sync-request)
          (setq qq-chat--forward-sync-request nil))
        ;; Clear only the completion projected above.  If a reentrant callback
        ;; installed newer filter state, its own Appkit request still owns it.
        (when (eq raw-filter-sync-request qq-chat--filter-sync-request)
          (setq qq-chat--filter-sync-request nil))
        ;; A callback completion belongs to one captured view.  Clear stale
        ;; replacement-view state without ever running its post-sync actions.
        (when (eq raw-callback-sync-request qq-chat--callback-sync-request)
          (setq qq-chat--callback-sync-request nil))
        (when (eq raw-send-sync-request qq-chat--send-sync-request)
          (setq qq-chat--send-sync-request nil))
        (when filter-auto-load-p
          (setq qq-chat--filter-auto-load-p nil))
        ;; Do not lose queued events if any projection above fails.  Appkit
        ;; will merge the invalidation snapshot for retry on that path.
        (appkit-view-acknowledge-events view (length events))
        (when send-sync-request
          (goto-char
           (or (appkit-chatbuf-input-logical-end-position) (point-max))))
        (dolist (action callback-actions)
          (when (qq-chat--captured-view-current-p view)
            (funcall action)))
        ;; Duplicate-only pages retain a live cursor.  Continue only after the
        ;; accepted page has passed through the Appkit projection transaction,
        ;; never recursively from its transport callback.
        (when (and filter-auto-load-p
                   (qq-chat--msg-filter-active-p)
                   (not qq-chat--filter-owner)
                   (qq-chat--msg-filter-has-more-p))
          (qq-chat-filter-load-more t))))))

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
            #'qq-chat--sync-invalidations
            (appkit-view-parts current)
            '(frame timeline composer geometry))
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
       :parts '(frame timeline composer geometry))))))

(defun qq-chat--header-line-update ()
  "Update chat header line and buffer name."
  (qq-chat--ensure-buffer-name)
  (qq-chat--prune-message-selection)
  (setq-local header-line-format (qq-chat--header-line))
  (force-mode-line-update))

(defun qq-chat--update-frame ()
  "Synchronize QQ chat header, footer, prompt, and canonical composer."
  (qq-chat--ensure-timeline)
  (appkit-chat-timeline-set-frame
   (qq-chat--header-text)
   (qq-chat--footer-text)
   :bind-input-function #'qq-chat--bind-input-region-from-footer
   :composer-visible-p (qq-chat--composer-visible-p)))

(cl-defun qq-chat--sync-timeline
    (&key (messages nil messages-p) force-keys changed-resources rekeys)
  "Synchronize QQ rows through the shared projected timeline controller."
  (qq-chat--ensure-timeline)
  (prog1
      (appkit-chat-timeline-sync
       (qq-chat--project-timeline
        (if messages-p messages (qq-chat--timeline-messages)))
       :force-keys force-keys
       :changed-resources changed-resources
       :rekeys rekeys)
    (when (qq-chat--msg-filter-active-p)
      (qq-chat--highlight-filter-results))))

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

(cl-defun qq-chat-sync-timeline-geometry (&key reset force)
  "Synchronize shared QQ timeline geometry in the current buffer.

RESET forces an authoritative geometry sync even when the cached width still
matches.  FORCE refreshes projected rows at the same character width, which is
required after text scaling because pixel-aligned avatars can still change."
  (when-let* ((view (qq-chat--live-current-view)))
    (let* ((window (qq-chat--render-window))
           (next (and window (qq-chat--compute-fill-column window)))
           (valid (and (integerp next) (> next 15)))
           (changed (and valid (not (equal next qq-chat--fill-column)))))
      (when (or reset changed force)
        (appkit-request-sync view :part 'geometry :position t))
      (and valid next))))

(defun qq-chat--on-window-size-change (&optional _frame)
  "Recompute chat width and refresh rows after window resizing."
  (when (eq major-mode 'qq-chat-mode)
    (qq-chat-sync-timeline-geometry)))

(defun qq-chat--on-text-scale-change ()
  "Recompute pixel alignment after `text-scale-mode' changes."
  (when (eq major-mode 'qq-chat-mode)
    (qq-chat-sync-timeline-geometry :reset t :force t)))

(defun qq-chat--request-row-redisplay (anchors)
  "Redisplay projected ANCHORS, deferring while a region is active."
  (when (and anchors (appkit-chat-timeline-live-p))
    (appkit-chat-timeline-invalidate
     anchors :defer-while-mark-active t)))

(defun qq-chat--render-empty-placeholder (state)
  "Insert the empty timeline placeholder row for STATE."
  (let ((start (point)))
    (appkit-view-insert-note-line
     (pcase state
       ('searching "Searching messages…")
       ('no-match "No matching messages.")
       ('normal "No messages loaded yet.")
       (_ (error "qq: invalid empty timeline state %S" state))))
    (insert "\n")
    (add-text-properties
     start (point)
     '(read-only t
       front-sticky (read-only)
       rear-nonsticky (read-only)
       qq-chat-internal empty-placeholder))))

(defun qq-chat--row-printer (row)
  "EWOC pretty-printer for one projected QQ chat ROW."
  (let ((message (appkit-chat-timeline-row-payload row))
        (context (appkit-chat-timeline-row-context row)))
    (cond
   ((and (listp message)
         (eq (plist-get message :kind) qq-chat--empty-placeholder))
    (qq-chat--render-empty-placeholder
     (or (plist-get context :state)
         (error "qq: empty timeline row lacks state context"))))
   (t
    (qq-chat--render-message message context)))))

(defun qq-chat--render-canonical-input ()
  "Replace the live composer from canonical input state explicitly."
  (qq-chat--assert-canonical-input-clean)
  (appkit-chatbuf-with-generated-update
    (appkit-chatbuf-input-replace (appkit-chatbuf-input-state))
    (appkit-chatbuf-input-apply-text-properties)))

(defun qq-chat--set-draft (text)
  "Set canonical draft TEXT and update the shared tail composer."
  (setq qq-chat--send-restore-owner nil)
  (appkit-chatbuf-input-state-set text :reset-history-p t)
  (qq-chat--render-canonical-input)
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
      ("at"
       (let ((target (alist-get 'qq data))
             (name (alist-get 'name data)))
         (concat "@" (or name
                          (and (equal target "all") "全体成员")
                          target
                          "mention"))))
      ("face"
       (let ((id (or (alist-get 'id data) "?")))
         (qq-media-face-display-string id)))
      ("mface"
       (let* ((summary (or (alist-get 'summary data) "[商城表情]"))
              (file (or (alist-get 'file data)
                        (alist-get 'path data)))
              (image (and file
                          (appkit-media-file-present-p file)
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
                          (appkit-media-file-present-p file)
                          (if sticker-p
                              (qq-media--image-from-file
                               file (max qq-media-face-image-height 32))
                            (qq-media-composer-image-preview file))))
              (name (or (alist-get 'name data)
                        (and file (file-name-nondirectory file))
                        summary
                        "image"))
              (size (and file
                         (file-exists-p file)
                         (file-size-human-readable
                          (file-attribute-size (file-attributes file)))))
              (preview (and image
                            (qq-media--image-display-string image "▧"))))
         (cond
          (sticker-p
             (qq-media--image-display-string
              image
              (or summary
                  name
                  "[image]")))
          ((or preview file)
           (concat "[image] "
                   (if preview (concat preview " ") "")
                   (propertize name 'help-echo file)
                   (if size (format " (%s)" size) "")))
          (t
           (format "[image:%s]" (or summary "item"))))))
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

(defun qq-chat--insert-input-segment-object (segment &optional visible-label)
  "Insert outbound QQ SEGMENT into the composer as one object.

VISIBLE-LABEL overrides the segment-derived label, for example to retain a
favorite face's local thumbnail alongside its sendable mface payload."
  ;; Structured insertion inhibits ordinary modification hooks inside Appkit,
  ;; so revoke stale send restoration explicitly before changing the draft.
  (setq qq-chat--send-restore-owner nil)
  (qq-chat--ensure-composer-visible)
  (unless (appkit-chatbuf-point-in-input-p)
    (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
  ;; Appkit preserves the exact object start as a boundary-before position,
  ;; but an unexpected point inside an intangible object belongs after it.
  ;; Normalize before adding a leading separator so we never edit the old
  ;; object's protected interior on the way to inserting the new one.
  (when-let* ((bounds (appkit-chatbuf-input-object-bounds-at-point)))
    (unless (= (point) (car bounds))
      (goto-char (cdr bounds))))
  (when (and (appkit-chatbuf-point-in-input-p)
             (> (point) (or (appkit-chatbuf-input-start-position) (point-min)))
             (let ((ch (char-before)))
               (and ch (not (memq ch '(32 9 10))))))
    (insert " "))
  (let* ((object (qq-chat--segment-input-object segment))
         (object (if visible-label
                     (plist-put object :label visible-label)
                   object))
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

(defun qq-chat--read-base-face-id (&optional prompt)
  "Prompt for and return one QQ base face id.

PROMPT defaults to \"QQ face: \".  Completion uses the existing QQ face
panel ordering, names, and local image affixation."
  (qq-completion-read-base-face-id prompt))

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

(defun qq-chat--insert-custom-face (face)
  "Insert favorite FACE alist into the composer as a sendable segment."
  (let ((segment (qq-media-custom-face-to-segment face)))
    (qq-chat--insert-input-segment-object
     segment (qq-media-custom-face-display-string face))
    (qq-chat--sync-draft-from-buffer)
    (message "qq: favorite %s" (qq-media-custom-face-label face))))

(defun qq-chat--pick-custom-face (faces)
  "Completing-read among FACES and insert the chosen favorite.

Uses the shared Appkit candidate layer so favorites keep NapCat order and show
local thumb previews with the same treatment as composer completion."
  (unless faces
    (user-error "qq: no favorite custom faces (收藏表情为空)"))
  (qq-chat--insert-custom-face (qq-completion-read-custom-face faces)))

(defun qq-chat-attach-custom-face (&optional force-refresh)
  "Insert a favorite custom face (收藏表情) into the chat composer.

Uses NapCat `fetch_custom_face_info'.  Personal favorites are sent as
image segments with `sub_type' 1; market favorites as mface when possible.

  With prefix FORCE-REFRESH, re-fetch the list from NapCat.
Bound via `C-u C-c C-e' or attach transient `E'."
  (interactive "P")
  (let ((buffer (current-buffer))
        (session-key qq-chat--session-key)
        (view (qq-chat--ensure-view)))
    (unless (and (not force-refresh) (qq-media-custom-faces-loaded-p))
      (message "qq: loading favorite faces…"))
    (qq-media-ensure-custom-faces
     (lambda (faces)
       (when (and (buffer-live-p buffer) view)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (qq-chat--captured-view-current-p view))
             (qq-chat--request-callback-sync
              view
              (lambda ()
                (condition-case err
                    (qq-chat--pick-custom-face faces)
                  (error
                   (message "%s" (error-message-string err))))))))))
     (lambda (_response reason)
       (message "qq: failed to load favorites: %s" reason))
     force-refresh)))

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
  (appkit-view-insert-note-line
   (format "-- %s --" day-label)
   :face 'qq-msg-date-separator))

(defun qq-chat--insert-unread-divider-row ()
  "Insert the unread separator row above the first unread message.

Label matches telega's unread bar wording (\"Unread Messages\")."
  (appkit-view-insert-note-line
   "Unread Messages"
   :face 'qq-msg-unread-divider))

(defun qq-chat--message-by-server-id (server-id)
  "Return the projected message with SERVER-ID in the current session."
  (when (and server-id qq-chat--session-key)
    (seq-find
     (lambda (message)
       (equal (format "%s" (or (alist-get 'server-id message) ""))
              (format "%s" server-id)))
     (if (qq-chat--msg-filter-active-p)
         (qq-chat--filtered-timeline-messages)
       (qq-state-session-messages qq-chat--session-key)))))

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
  (qq-chat--cancel-initial-history-request)
  (let ((owner (qq-chat--begin-around-history-window))
        (view (qq-chat--ensure-view)))
    (qq-api-fetch-history-around
     session-key
     target
     (lambda (meta)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (qq-chat--captured-view-current-p view)
                      (appkit-chat-history-request-current-p owner))
             (appkit-chat-history-request-end owner)
             (qq-chat--note-history-window meta)
             (qq-chat--request-callback-sync
              view
              (lambda ()
                (unless (qq-chat--finish-jump-if-loaded target)
                  (qq-chat--jump-fail
                   target "around window omitted target"))))))))
     (lambda (_response reason)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when (and (equal qq-chat--session-key session-key)
                      (qq-chat--captured-view-current-p view)
                      (appkit-chat-history-request-current-p owner))
             (appkit-chat-history-request-end owner)
             (qq-chat--request-callback-sync
              view (lambda () (qq-chat--jump-fail target reason)))))))
     (qq-chat--jump-history-count))))

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
    (if (and (qq-chat--history-window-known-p)
             (qq-chat--goto-loaded-message id t))
        (setq qq-chat--pending-jump-id nil)
      (qq-chat--cancel-open-message-request)
      (setq qq-chat--pending-jump-id id)
      (message "qq: loading…")
      (qq-chat--seek-history-for-jump session-key id buffer))))

(defun qq-chat-goto-reply (&optional message)
  "Goto the message that MESSAGE replies to.

MESSAGE defaults to the message at point.  Bound to timeline `g'.  The ↪
reply preview line is also a button that calls this path, corresponding to
telega's `telega-msg-goto-reply-to-message'."
  (interactive)
  (let* ((msg (or message
                  (qq-chat--message-at-point)
                  (user-error "qq: no message at point")))
         (reply-id (or (qq-chat--message-reply-id msg)
                       (user-error "qq: message is not a reply"))))
    (qq-chat-goto-message reply-id)))

(defun qq-chat-goto-pop-message ()
  "Pop a message from the jump ring and goto it.

Bound to timeline `x', corresponding to telega's
`telega-chatbuf-goto-pop-message'."
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

Telega uses `telega-ins--with-props' together with its goto-reply action on the
reply header.  Here the line is a button (RET / mouse-1) that jumps to REPLY-ID
via `qq-chat-goto-message'."
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
    (appkit-ui-apply-line-prefix reply-start (point) prefix-state)))

(defun qq-chat--face-segment-id (segment)
  "Return QQ base face id from face SEGMENT, or nil."
  (let* ((data (alist-get 'data segment))
         (raw (alist-get 'raw data)))
    (or (alist-get 'id data)
        (alist-get 'faceIndex data)
        (and (listp raw) (alist-get 'faceIndex raw)))))

(defun qq-chat--animated-face-segment-p (segment)
  "Return non-nil when face SEGMENT identifies an animated base face."
  (when (equal (alist-get 'type segment) "face")
    (let* ((data (alist-get 'data segment))
           (raw (alist-get 'raw data))
           (face-type (or (alist-get 'face_type data)
                          (alist-get 'faceType data)
                          (and (listp raw) (alist-get 'faceType raw))))
           (sticker-id (or (alist-get 'sticker_id data)
                           (alist-get 'stickerId data)
                           (and (listp raw) (alist-get 'stickerId raw)))))
      (or (equal face-type 3)
          (and sticker-id
               (not (equal (format "%s" sticker-id) "0")))))))

(defun qq-chat--face-segment-description (segment)
  "Return the native display description from face SEGMENT, or nil."
  (let* ((data (alist-get 'data segment))
         (raw (alist-get 'raw data)))
    (or (alist-get 'description data)
        (alist-get 'faceText data)
        (alist-get 'face_text data)
        (and (listp raw) (alist-get 'faceText raw))
        (and (listp raw) (alist-get 'face_text raw)))))

(defun qq-chat--open-mention-user (user-id)
  "Open the profile for mentioned USER-ID."
  (unless (and (qq-api-user-id-p user-id)
               (not (equal user-id "0")))
    (user-error "qq: mention has no user profile"))
  (qq-user-open user-id))

(defun qq-chat--mention-display-string (data)
  "Return a telega-style interactive mention string from segment DATA."
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
                    "mention"))
         (text (concat "@" label))
         (profile-p (and (qq-api-user-id-p target)
                         (not (equal target "0"))))
         (display
          (if profile-p
              (buttonize
               text #'qq-chat--open-mention-user target
               (format "Open %s's profile (QQ %s)" label target))
            text)))
    (add-text-properties
     0 (length display)
     (list 'face (if (memq kind '(at-me at-all))
                     'qq-msg-mention-self
                   'qq-msg-mention)
           'qq-chat-mention-kind kind
           'qq-chat-mention-user-id (and profile-p target)
           'rear-nonsticky '(qq-chat-mention-kind
                             qq-chat-mention-user-id))
     display)
    display))

(defun qq-chat--segment-inline-string (segment)
  "Return inline display string for SEGMENT, or nil for block-like segments.

Face segments render as inline images (LinuxQQ default-emojis / NapCat
base emoji), never as OneBot CQ text."
  (let ((type (alist-get 'type segment))
        (data (alist-get 'data segment)))
    (pcase type
      ("text" (or (alist-get 'text data) ""))
      ("at"
       (qq-chat--mention-display-string data))
      ("__unsupported"
       (qq-state-message-preview-from-segments (list segment)))
      ("face"
       (qq-media-face-display-string
        (or (qq-chat--face-segment-id segment) "?")
        (qq-chat--face-segment-description segment)))
      (_ nil))))

(defun qq-chat--mail-segment-p (segment)
  "Return non-nil when SEGMENT is a structured QQ Mail notification."
  (equal (alist-get 'type segment) "mail"))

(defun qq-chat--wallet-segment-p (segment)
  "Return non-nil when SEGMENT is a native QQ wallet message."
  (equal (alist-get 'type segment) "wallet"))

(defun qq-chat--poke-segment-p (segment)
  "Return non-nil when SEGMENT is a QQ gray-tip poke decoration."
  (equal (alist-get 'type segment) "poke"))

(defun qq-chat--gray-tip-segment-p (segment)
  "Return non-nil when SEGMENT is a QQ gray-tip notice."
  (equal (alist-get 'type segment) "gray-tip"))

(defun qq-chat--insert-gray-tip-user (part)
  "Insert the telega-style interactive user represented by gray-tip PART."
  (let* ((user-id (alist-get 'user-id part))
         (name (alist-get 'name part))
         (label (concat (qq-media-avatar-display-string user-id) " " name))
         (action (lambda () (qq-user-open user-id))))
    (appkit-ui-insert-action-button
     label action
     :face 'qq-msg-user-title
     :help-echo (format "Open %s's profile" name)
     :properties
     (list 'read-only t
           'front-sticky t
           'rear-nonsticky '(read-only)
           'qq-chat-gray-tip-user-id user-id))))

(defun qq-chat--gray-tip-label (message)
  "Return MESSAGE's structured, interactive gray-tip label."
  (let ((parts (alist-get 'parts (qq-state-gray-tip-message-data message))))
    (if (null parts)
        (qq-chat--message-body message)
      (with-temp-buffer
        (let ((inhibit-read-only t))
          (dolist (part parts)
            (pcase (alist-get 'type part)
              ("text" (insert (alist-get 'text part)))
              ("user" (qq-chat--insert-gray-tip-user part))))
          (buffer-string))))))

(defun qq-chat--insert-gray-tip-message (message properties)
  "Insert centered gray-tip MESSAGE using PROPERTIES."
  (let ((start (point)))
    (appkit-chat-ins-insert-divider-row
     (qq-chat--gray-tip-label message)
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
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state
     (if (and (stringp sender) (not (string-empty-p sender)))
         (format "Mail · %s" sender)
       "Mail")
     :face 'bold
     :properties properties)
    (when (and (stringp subject) (not (string-empty-p subject)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state subject :properties properties))
    (when-let* ((body (cond
                       ((and (stringp content) (not (string-empty-p content))) content)
                       ((and (stringp prompt) (not (string-empty-p prompt))) prompt))))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties properties))
    (when (and (stringp url) (not (string-empty-p url)))
      (let ((start (point)))
        (appkit-ui-insert-action-button
         (format "[%s]" detail)
         (lambda () (browse-url url t))
         :help-echo "Open this message in QQ Mail")
        (insert "\n")
        (appkit-ui-apply-line-prefix start (point) card-prefix-state)
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
         (image-url (qq-chat--present-string (alist-get 'image data)))
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
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state))
         (start (point)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state
     (if source (format "%s · %s" label source) label)
     :face 'bold
     :properties card-properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (when image-url
      (let ((image-start (point)))
        (insert
         (qq-media-url-preview-display-string
          (format "card-image-url:%s" image-url)
          image-url "" qq-media-preview-image-height))
        (insert "\n")
        (appkit-ui-apply-line-prefix image-start (point) card-prefix-state)
        (add-text-properties image-start (point) card-properties)))
    (when body
      (appkit-ui-insert-prefixed-lines
       card-prefix-state body :face 'shadow :properties card-properties))
    (when (and summary (not (equal summary body)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state summary :face 'shadow :properties card-properties))
    (when open-action
      (add-text-properties start (point) card-properties))))

(defun qq-chat--insert-wallet-segment
    (segment message prefix-state properties)
  "Insert native wallet SEGMENT belonging to MESSAGE as one card."
  (let* ((data (alist-get 'data segment))
         (receiver (alist-get 'receiver data))
         (sender (alist-get 'sender data))
         (presentation
          (if (seq-some (lambda (key)
                          (qq-chat--present-string (alist-get key receiver)))
                        '(title sub_title content notice))
              receiver
            sender))
         (title (qq-chat--present-string (alist-get 'title presentation)))
         (subtitle (qq-chat--present-string
                    (alist-get 'sub_title presentation)))
         (content (qq-chat--present-string (alist-get 'content presentation)))
         (wallet-kind (alist-get 'wallet_kind data))
         (red-packet-p (member wallet-kind
                               '("red-packet" "password-red-packet")))
         (kind-label
          (pcase wallet-kind
            ("red-packet" "🧧 QQ 红包")
            ("password-red-packet" "🧧 口令红包")
            ("transfer" (if content (format "💳 %s" content) "💳 转账"))
            (_ (if content (format "💳 %s" content) "QQ 钱包"))))
         (message-id (alist-get 'server-id message))
         (outgoing-p (eq (alist-get 'self-p message) t))
         (open-action
          (and red-packet-p
               (qq-api-message-id-p message-id)
               (lambda ()
                 (qq-red-packet-open
                  qq-chat--session-key message-id segment outgoing-p))))
         (map (when open-action
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map button-map)
                  (define-key map (kbd "RET")
                    (lambda () (interactive) (funcall open-action)))
                  (define-key map [mouse-1]
                    (lambda () (interactive) (funcall open-action)))
                  map)))
         (card-properties
          (append properties
                  (when open-action
                    (list 'mouse-face 'highlight
                          'help-echo "查看 QQ 红包"
                          'follow-link t
                          'keymap map
                          'button t
                          'category 'default-button
                          'action (lambda (_button)
                                    (funcall open-action))))))
         (appkit-ui-card-indent-prefix-state prefix-state)
         (card-prefix-state (appkit-ui-card-prefix-state))
         (start (point)))
    (appkit-ui-insert-prefixed-lines
     card-prefix-state kind-label
     :face 'bold :properties card-properties)
    (when title
      (appkit-ui-insert-prefixed-lines
       card-prefix-state title :properties card-properties))
    (when subtitle
      (appkit-ui-insert-prefixed-lines
       card-prefix-state subtitle :face 'shadow :properties card-properties))
    (when (and content (not (equal content title)))
      (appkit-ui-insert-prefixed-lines
       card-prefix-state content :face 'shadow :properties card-properties))
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
    (appkit-media-card-context-create
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
         (prefix-state (let ((appkit-ui-card-indent-prefix-state prefix-state))
                         (appkit-ui-card-prefix-state))))
    (appkit-chat-ins-insert-media-card
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
                   (appkit-media-insert-image-slices
                    preview nil nil
                    (cond
                     ((equal (alist-get 'type segment) "mface") "[sticker]")
                     ((qq-media-videoish-segment-p segment) "[video]")
                     (t "[image]")))
                   (setq preview-end (point)))
               (error
                (insert "[preview unavailable]")))
             (when preview-end
               (appkit-media-add-action-properties
                preview-start preview-end
                (lambda (&optional _event)
                  (interactive)
                  (appkit-media-card-call-action 'open context))
                (format "Open %s" (downcase kind-label))))
             (insert "\n"))
            (loading
             (insert "[loading preview]\n"))
            (t
             (insert "[preview unavailable]\n")))
           (appkit-ui-apply-line-prefix preview-start (point) card-prefix-state)
           (appkit-ui-append-face preview-start (point) 'shadow)))))))

(defun qq-chat--insert-animated-face-segment (segment prefix-state properties)
  "Insert animated face SEGMENT below the avatar's two-line header."
  ;; Consume the avatar's normal-height first-body slice before inserting the
  ;; tall animation.  Otherwise line-prefix stretches that slice to the media
  ;; line and makes one avatar look like two vertically separated avatars.
  (let ((avatar-tail-start (point)))
    (insert " \n")
    (appkit-ui-apply-line-prefix avatar-tail-start (point) prefix-state)
    (add-text-properties avatar-tail-start (point) properties))
  (let ((animation-start (point)))
    (insert (or (qq-chat--segment-inline-string segment)
                (qq-state-message-preview-from-segments (list segment))))
    (insert "\n")
    (appkit-ui-apply-line-prefix animation-start (point) prefix-state)
    (add-text-properties animation-start (point) properties)))

(defun qq-chat--insert-message-body (message prefix-state properties)
  "Insert MESSAGE content body using PREFIX-STATE and PROPERTIES."
  (let ((segments (alist-get 'segments message))
        (inline-parts nil))
    (cl-labels ((flush-inline ()
                  (when inline-parts
                    (appkit-ui-insert-prefixed-lines
                     prefix-state
                     (mapconcat #'identity (nreverse inline-parts) "")
                     :properties (append properties
                                         '(qq-chat-search-text t)))
                    (setq inline-parts nil))))
      (if (or (qq-state-message-recalled-p message)
              (null segments))
          (appkit-ui-insert-prefixed-lines
           prefix-state (qq-chat--message-body message)
           :properties (append properties '(qq-chat-search-text t)))
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
               ((qq-chat--wallet-segment-p segment)
                (flush-inline)
                (qq-chat--insert-wallet-segment
                 segment message prefix-state properties))
               ((qq-chat--card-segment-p segment)
                (flush-inline)
                (qq-chat--insert-card-segment segment prefix-state properties))
               ((qq-chat--media-segment-p segment)
                (flush-inline)
                (qq-chat--insert-segment-media-line segment prefix-state properties))
               ((qq-chat--animated-face-segment-p segment)
                (flush-inline)
                (qq-chat--insert-animated-face-segment
                 segment prefix-state properties))
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

(defun qq-chat--message-reference (message)
  "Return MESSAGE's closed locator-qualified mutation reference.

The selected message must explicitly belong to this chat buffer; never pair a
buffer session with a detached message id after the fact."
  (unless (listp message)
    (user-error "qq: message reference requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (equal session-key qq-chat--session-key)
      (user-error "qq: selected message belongs to a different chat"))
    (qq-api-validate-message-reference
     `((message_id . ,message-id)
       (chat . ,(qq-api-chat-locator session-key)))
     "selected message reference")))

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
     (qq-chat--message-reference message) emoji-id set
     (lambda (_response)
       (message "qq: reaction %s (%s)"
                (if set "added" "removed") emoji-id)))))

(defun qq-chat--insert-reaction-line (message prefix-state properties)
  "Insert shared appkit reaction chips adapted for QQ MESSAGE."
  (let ((reactions (qq-state-message-reactions message))
        (message-id (alist-get 'server-id message)))
    (when reactions
      (when-let* ((span
                   (appkit-chat-ins-insert-reaction-line
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
                    (start (point))
                    body-end)
               (insert (if (string-empty-p text) "(empty message)" text))
               (setq body-end (point))
               (when (and with-time
                          (stringp short-time)
                          (not (string-empty-p short-time)))
                 (qq-chat--insert-right-aligned-time
                  short-time
                  (string-width
                   (or (appkit-ui-prefix-state-current prefix-state) ""))
                  t))
               (insert "\n")
               (appkit-ui-apply-line-prefix start (point) prefix-state)
               (add-text-properties start (point) properties)
               (add-text-properties start body-end '(qq-chat-search-text t))
               (setq inline-parts nil)))))
      (cond
       ((or (qq-state-message-recalled-p message) (null segments))
        (let* ((text (qq-chat--message-body message))
               (start (point))
               body-end)
          (insert (if (string-empty-p text) "(empty message)" text))
          (setq body-end (point))
          (when (and (stringp short-time) (not (string-empty-p short-time)))
            (qq-chat--insert-right-aligned-time
             short-time
             (string-width
              (or (appkit-ui-prefix-state-current prefix-state) ""))
             t))
          (insert "\n")
          (appkit-ui-apply-line-prefix start (point) prefix-state)
          (add-text-properties start (point) properties)
          (add-text-properties start body-end '(qq-chat-search-text t))))
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
              (or (appkit-ui-prefix-state-current prefix-state) ""))
             t)
            (insert "\n")
            (appkit-ui-apply-line-prefix start (point) prefix-state)
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
  (let* ((message-id (alist-get 'server-id message))
         (poke-p (qq-state-poke-message-p message))
         (recall-reference
          (and poke-p (qq-state-poke-recall-reference message))))
    (unless message-id
      (user-error "qq: selected message has no server id"))
    (when (and poke-p (null recall-reference))
      (user-error "qq: poke has no native recall reference"))
    (when (and poke-p
               (qq-protocol-poke-recall-reference-expired-p
                recall-reference))
      (user-error "qq: 戳一戳已超过 2 分钟撤回期限"))
    (when (y-or-n-p (format "Recall message %s? " message-id))
      (if poke-p
          (qq-api-recall-poke qq-chat--session-key recall-reference)
        (qq-api-delete-message (qq-chat--message-reference message))))))

(defun qq-chat--message-title-face (message)
  "Return sender title face for MESSAGE."
  (if (alist-get 'self-p message)
      'qq-msg-self-title
    'qq-msg-user-title))

(defun qq-chat--message-avatar-prefixes (message)
  "Return shared telega-style two-line avatar prefixes for MESSAGE."
  (let* ((sender-id (alist-get 'sender-id message))
         (image (qq-media-message-avatar-image message))
         (prefixes
          (appkit-chat-avatar-prefixes
           image "@"
           :pixel-size (appkit-chat-avatar-two-line-pixel-size)
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

(cl-defun qq-chat-message-layout
    (message &key compact (avatar-p t) selected-p)
  "Return shared presentation layout for MESSAGE.

COMPACT makes the first body line use the ordinary continuation prefix.
AVATAR-P controls whether the shared two-line avatar occupies the heading and
first body line.  SELECTED-P adds a stable mark to every visual line without
inserting a synthetic timeline row.  The returned plist owns a fresh mutable
body prefix state."
  (let* ((avatar-prefixes
          (and avatar-p (qq-chat--message-avatar-prefixes message)))
         (selection-prefix
          (if selected-p
              (propertize "▌ " 'face 'qq-msg-selected-marker)
            ""))
         (header-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :header) "")))
         (body-first-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :first-body) "  ")))
         (body-rest-prefix
          (concat selection-prefix
                  (or (plist-get avatar-prefixes :rest-body) "  "))))
    (list :header-prefix header-prefix
          :body-rest-prefix body-rest-prefix
          :body-prefix-state
          (if compact
              (appkit-ui-make-prefix-state
               body-rest-prefix body-rest-prefix)
            (appkit-ui-make-prefix-state
             body-first-prefix body-rest-prefix)))))

(cl-defun qq-chat-insert-message-heading
    (message properties layout
             &key (title-face nil title-face-p)
             (status-suffix nil status-suffix-p))
  "Insert the shared heading for MESSAGE and return its body prefix state.

PROPERTIES cover the generated heading.  LAYOUT must come from
`qq-chat-message-layout'.  TITLE-FACE and STATUS-SUFFIX default to the normal
QQ message presentation when omitted."
  (let* ((header-prefix (plist-get layout :header-prefix))
         (body-rest-prefix (plist-get layout :body-rest-prefix))
         (header-start (point)))
    (qq-chat--insert-message-sender
     message (if title-face-p
                 title-face
               (qq-chat--message-title-face message)))
    (insert (if status-suffix-p
                (or status-suffix "")
              (qq-chat--status-suffix message)))
    (qq-chat--insert-right-aligned-time
     (qq-chat--format-time (alist-get 'time message))
     (string-width header-prefix)
     t)
    (insert "\n")
    (appkit-ui-apply-line-prefix
     header-start (point)
     (appkit-ui-make-prefix-state header-prefix body-rest-prefix))
    (appkit-ui-append-face header-start (point) 'qq-msg-heading)
    (add-text-properties header-start (point) properties)
    (plist-get layout :body-prefix-state)))

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
         (selected (qq-chat-message-selected-p message))
         (ordinary-message-p
          (not (or (qq-state-gray-tip-message-p message)
                   (qq-state-poke-message-p message))))
         (layout
          (qq-chat-message-layout
           message :compact compact :avatar-p ordinary-message-p
           :selected-p selected))
         (header-prefix (plist-get layout :header-prefix))
         (body-rest-prefix (plist-get layout :body-rest-prefix))
         (body-prefix-state (plist-get layout :body-prefix-state))
         (short-time (qq-chat--format-time-short (alist-get 'time message)))
         content-start)
    (when (and (stringp insert-date) (not (string-empty-p insert-date)))
      (qq-chat--insert-date-separator-row (qq-chat--message-day-label insert-date)))
    (when insert-unread
      (qq-chat--insert-unread-divider-row))
    (setq content-start (point))
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
        (appkit-ui-apply-line-prefix
         header-start (point)
         (appkit-ui-make-prefix-state header-prefix body-rest-prefix))
        (add-text-properties
         header-start (point)
         (append properties (list 'face 'qq-msg-deleted)))
        (appkit-ui-insert-prefixed-lines
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
      (qq-chat-insert-message-heading
       message properties layout
       :title-face title-face
       :status-suffix status-suffix)
      (when reply-id
        (qq-chat--insert-reply-preview-line reply-id properties body-prefix-state))
      (qq-chat--insert-message-body message body-prefix-state properties)))
    ;; Gray tips and pokes bypass the ordinary heading/body layout.  Give their
    ;; single visual row the same stable selection stripe without manufacturing
    ;; a second timeline row.
    (when (and selected
               (or (qq-state-gray-tip-message-p message)
                   (qq-state-poke-message-p message)))
      (appkit-ui-apply-line-prefix
       content-start (point)
       (appkit-ui-make-prefix-state header-prefix header-prefix)))
    (unless (or (qq-state-message-recalled-p message)
                (qq-state-poke-message-p message)
                (qq-state-gray-tip-message-p message))
      (qq-chat--insert-reaction-line message body-prefix-state properties))
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun qq-chat--clear-search-highlights ()
  "Remove in-chat search overlays owned by this buffer."
  (mapc #'delete-overlay qq-chat--search-highlight-overlays)
  (setq qq-chat--search-highlight-overlays nil))

(defun qq-chat--add-search-text-highlights (message-id query)
  "Add literal QUERY highlights inside MESSAGE-ID's actual text only."
  (when-let* ((start (qq-chat--message-position message-id))
              (end (qq-chat--message-end-position start))
              (tokens (split-string query "[[:space:]]+" t))
              (regexp (regexp-opt tokens)))
    (let ((case-fold-search t))
      (save-excursion
        (goto-char start)
        (while (re-search-forward regexp end t)
          (let ((match-start (match-beginning 0))
                (match-end (match-end 0)))
            (when (and (get-text-property match-start 'qq-chat-search-text)
                       (>= (or (next-single-property-change
                                match-start 'qq-chat-search-text nil end)
                               end)
                           match-end))
              (let ((overlay (make-overlay match-start match-end nil t nil)))
                (overlay-put overlay 'face 'isearch)
                (overlay-put overlay 'evaporate t)
                (push overlay qq-chat--search-highlight-overlays)))))))))

(defun qq-chat--highlight-search-text (message-id query)
  "Replace owned highlights with QUERY matches inside MESSAGE-ID."
  (qq-chat--clear-search-highlights)
  (qq-chat--add-search-text-highlights message-id query))

(defun qq-chat--search-result-key (result)
  "Return deduplication key for authoritative search RESULT."
  (cons (qq-api-session-key-from-locator (alist-get 'chat result))
        (alist-get 'message_id result)))

(defun qq-chat--append-search-results (results session-key)
  "Append unseen RESULTS for SESSION-KEY and return number added."
  (let ((added 0))
    (dolist (result results)
      (let* ((key (qq-chat--search-result-key result))
             (result-session (car key)))
        (unless (equal result-session session-key)
          (error "qq: message search returned %s while searching %s"
                 result-session session-key))
        (unless (gethash key qq-chat--search-seen)
          (puthash key t qq-chat--search-seen)
          (let ((cell (list result)))
            (if qq-chat--search-results-tail
                (setcdr qq-chat--search-results-tail cell)
              (setq qq-chat--search-results cell))
            (setq qq-chat--search-results-tail cell))
          (cl-incf added))))
    added))

(defun qq-chat--search-request-current-p
    (buffer session-key owner &optional call-owner)
  "Return non-nil when OWNER still owns search in BUFFER/SESSION-KEY.

When CALL-OWNER is non-nil, it must also own the currently executing page."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--search-owner owner)
              (qq-chat--captured-view-current-p
               (plist-get owner :view))
              (or (null call-owner)
                  (eq (plist-get owner :call-owner) call-owner))))))

(defun qq-chat--cancel-search-request ()
  "Cancel the current in-chat search callback owner."
  (let ((request qq-chat--search-request))
    (setq qq-chat--search-request nil
          qq-chat--search-owner nil)
    (when request
      (condition-case nil
          (qq-api-cancel-request request)
        (quit
         (setq quit-flag nil)
         nil)
        (error nil)))))

(defun qq-chat--show-search-result (index)
  "Open and highlight authoritative search result at INDEX."
  (let* ((result (nth index qq-chat--search-results))
         (session-key (and result
                           (qq-api-session-key-from-locator
                            (alist-get 'chat result)))))
    (unless result
      (user-error "qq: no search result at index %s" index))
    (unless (equal session-key qq-chat--session-key)
      (error "qq: search result belongs to unexpected session %s" session-key))
    (setq qq-chat--search-index index)
    (qq-chat--header-line-update)
    (qq-chat-open-message session-key (alist-get 'message_id result)
                          qq-chat--last-search-query)))

(defun qq-chat--search-result-origin-order (result anchor)
  "Compare RESULT with search ANCHOR without coercing string identities.

Return -1 when RESULT is older, 0 at the same native sequence, and 1 when
newer.  Both sides must carry exact decimal kernel sequences."
  (let ((origin-seq (plist-get anchor :sequence))
        (result-seq (alist-get 'message_seq result)))
    (unless (and (qq-protocol--nonzero-decimal-string-p origin-seq)
                 (qq-protocol--nonzero-decimal-string-p result-seq))
      (error "qq: in-place search ordering requires native message sequences"))
    (qq-protocol-decimal-string-compare result-seq origin-seq)))

(defun qq-chat--initial-search-selection ()
  "Return initial result index, `need-more', or nil.

Search results are newest-to-oldest.  Message snowflakes are never compared;
the origin is located by exact id when it is itself a match.  Otherwise the
original decimal kernel sequence is compared as a string."
  (let* ((anchor qq-chat--search-anchor)
         (anchor-id (plist-get anchor :message-id))
         (anchor-index
          (and anchor-id
               (cl-position anchor-id qq-chat--search-results
                            :test #'equal
                            :key (lambda (result)
                                   (alist-get 'message_id result)))))
         (result-count (length qq-chat--search-results)))
    (cond
     ((null anchor)
      (cond ((> result-count 0) 0)
            (qq-chat--search-next-cursor 'need-more)))
     ((eq qq-chat--search-direction 'older)
      (cond
       ((and anchor-index (< (1+ anchor-index) result-count))
        (1+ anchor-index))
       (anchor-index
        (and qq-chat--search-next-cursor 'need-more))
       ((cl-position-if
         (lambda (result)
           (< (qq-chat--search-result-origin-order result anchor) 0))
         qq-chat--search-results))
       (qq-chat--search-next-cursor 'need-more)))
     (t
      ;; A newer match is immediately before an anchor that matched the
      ;; query.  Otherwise wait until the result stream crosses the origin,
      ;; then choose the last (therefore closest) strictly newer sequence.
      (cond
       (anchor-index (and (> anchor-index 0) (1- anchor-index)))
       (t
        (let ((newer-indices nil)
              (crossed nil)
              (index 0))
          (dolist (result qq-chat--search-results)
            (if (> (qq-chat--search-result-origin-order result anchor) 0)
                (setq newer-indices (cons index newer-indices))
              (setq crossed t))
            (cl-incf index))
          (cond
           ((or crossed (null qq-chat--search-next-cursor))
            (car newer-indices))
           (qq-chat--search-next-cursor 'need-more)))))))))

(defun qq-chat--finish-search-request (owner &optional message-text)
  "Release search OWNER and optionally display MESSAGE-TEXT."
  (when (eq qq-chat--search-owner owner)
    (setq qq-chat--search-request nil
          qq-chat--search-owner nil
          qq-chat--search-completed-p t)
    (when message-text (message "%s" message-text))))

(defun qq-chat--continue-search-request (owner)
  "Continue OWNER through an unconsumed server cursor."
  (qq-chat--issue-search-request owner t))

(defun qq-chat--search-page-succeeded
    (buffer session-key owner call-owner page)
  "Apply PAGE when exact OWNER and CALL-OWNER remain current."
  (when (qq-chat--search-request-current-p
         buffer session-key owner call-owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view))
            action)
        (setf (plist-get owner :pending) nil)
        (setq qq-chat--search-request nil)
        (condition-case error-data
            (let ((purpose (plist-get owner :purpose)))
              (qq-chat--append-search-results
               (alist-get 'results page) session-key)
              (setq qq-chat--search-next-cursor (alist-get 'next_cursor page))
              (pcase purpose
                ('initial
                 (let ((selection (qq-chat--initial-search-selection)))
                   (cond
                    ((integerp selection)
                     (qq-chat--finish-search-request owner)
                     (setq action
                           (lambda ()
                             (qq-chat--show-search-result selection))))
                    ((eq selection 'need-more)
                     (setq action
                           (lambda ()
                             (qq-chat--continue-search-request owner))))
                    (t
                     (qq-chat--finish-search-request
                      owner
                      (format "qq: no %s match for %s"
                              (if (eq qq-chat--search-direction 'newer)
                                  "newer" "older")
                              qq-chat--last-search-query))))))
                ('older
                 (let ((desired-index (plist-get owner :desired-index)))
                   (cond
                    ((nth desired-index qq-chat--search-results)
                     (qq-chat--finish-search-request owner)
                     (setq action
                           (lambda ()
                             (qq-chat--show-search-result desired-index))))
                    (qq-chat--search-next-cursor
                     (setq action
                           (lambda ()
                             (qq-chat--continue-search-request owner))))
                    (t
                     (qq-chat--finish-search-request
                      owner
                      (format "qq: no older match for %s"
                              qq-chat--last-search-query))))))))
          (error
           (setq qq-chat--search-next-cursor nil)
           (qq-chat--finish-search-request
            owner
            (format "qq: invalid message-search result: %s"
                    (error-message-string error-data)))))
        (qq-chat--request-callback-sync view action)))))

(defun qq-chat--search-page-failed
    (buffer session-key owner call-owner _response reason)
  "Report failure REASON for exact OWNER and CALL-OWNER."
  (when (qq-chat--search-request-current-p
         buffer session-key owner call-owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setf (plist-get owner :pending) nil)
        ;; A next cursor is single-use and was removed before dispatch.  Never
        ;; restore it after failure; an explicit new search is the only retry.
        (setq qq-chat--search-next-cursor nil)
        (qq-chat--finish-search-request
         owner (format "qq: message search failed: %s; search again to retry"
                       reason))
        (qq-chat--request-callback-sync view)))))

(defun qq-chat--issue-search-request (owner next-p)
  "Issue one page for overall search OWNER, continuing when NEXT-P."
  (let* ((buffer (current-buffer))
         (session-key qq-chat--session-key)
         (call-owner (list 'message-search-page))
         (cursor (and next-p qq-chat--search-next-cursor))
         request)
    (condition-case error-data
        (progn
          (when next-p
            (unless cursor
              (error "qq: message search has no continuation cursor"))
            (when (gethash cursor qq-chat--search-consumed-cursors)
              (setq qq-chat--search-next-cursor nil)
              (error "qq: message search repeated an already consumed cursor"))
            (puthash cursor t qq-chat--search-consumed-cursors)
            ;; Cursor capabilities are single-use.  Consume before dispatch so
            ;; a signal or errback cannot replay native searchMore.
            (setq qq-chat--search-next-cursor nil))
          (setf (plist-get owner :call-owner) call-owner
                (plist-get owner :pending) t)
          (setq qq-chat--search-request nil)
          (qq-chat--header-line-update)
          (message "qq: searching messages…")
          (setq request
                (if next-p
                    (qq-api-search-messages-next
                     session-key cursor 'summary
                     (lambda (page)
                       (qq-chat--search-page-succeeded
                        buffer session-key owner call-owner page))
                     (lambda (response reason)
                       (qq-chat--search-page-failed
                        buffer session-key owner call-owner response reason)))
                  (qq-api-search-messages-start
                   session-key qq-chat--last-search-query
                   (lambda (page)
                     (qq-chat--search-page-succeeded
                      buffer session-key owner call-owner page))
                   (lambda (response reason)
                     (qq-chat--search-page-failed
                      buffer session-key owner call-owner response reason)))))
          (when (and (qq-chat--search-request-current-p
                      buffer session-key owner call-owner)
                     (plist-get owner :pending))
            (setq qq-chat--search-request request))
          request)
      (error
       (setf (plist-get owner :pending) nil)
       (setq qq-chat--search-next-cursor nil)
       (qq-chat--finish-search-request
        owner
        (format "qq: message search dispatch failed: %s; search again to retry"
                (error-message-string error-data)))
       (qq-chat--header-line-update)
       nil))))

(defun qq-chat--start-search-request (purpose &optional desired-index)
  "Start a search request for PURPOSE and optional DESIRED-INDEX."
  (qq-chat--cancel-search-request)
  (let ((owner (list :session-key qq-chat--session-key
                     :view (qq-chat--ensure-view)
                     :purpose purpose
                     :desired-index desired-index
                     :call-owner nil
                     :pending nil)))
    (setq qq-chat--search-owner owner
          qq-chat--search-completed-p nil)
    (qq-chat--issue-search-request owner (eq purpose 'older))))

(defun qq-chat-search (query &optional forward-p)
  "Start authoritative in-place message search for QUERY.

Results come from Linux QQ through NapCat, never from wrapping a local buffer
text search.  FORWARD-P selects the closest newer result from the message at
point; otherwise select the closest older result."
  (interactive
   (list (read-string "QQ search (backward): " qq-chat--last-search-query)))
  (setq query (and (stringp query) (string-trim query)))
  (when (qq-chat--msg-filter-active-p)
    (user-error "qq: cancel the active message filter before in-place search"))
  (unless (and query (not (string-empty-p query)))
    (user-error "qq: empty search query"))
  (when (> (length query) 512)
    (user-error "qq: search query must be at most 512 characters"))
  (qq-chat--cancel-search-request)
  (qq-chat--clear-search-highlights)
  (let* ((origin (ignore-errors (qq-chat--message-at-point)))
         (origin-id (alist-get 'server-id origin))
         (origin-sequence (alist-get 'message-seq origin))
         (origin-sequence
          (and (qq-protocol--nonzero-decimal-string-p origin-sequence)
               origin-sequence)))
    (setq qq-chat--last-search-query query
        qq-chat--search-results nil
        qq-chat--search-results-tail nil
        qq-chat--search-seen (make-hash-table :test #'equal)
        qq-chat--search-consumed-cursors (make-hash-table :test #'equal)
        qq-chat--search-index nil
        qq-chat--search-next-cursor nil
        qq-chat--search-direction (if forward-p 'newer 'older)
        qq-chat--search-completed-p nil
        qq-chat--search-anchor
        (and (qq-api-message-id-p origin-id)
             origin-sequence
             (list :message-id origin-id
                   :sequence origin-sequence))))
  (qq-chat--header-line-update)
  (qq-chat--start-search-request 'initial))

(defun qq-chat-search-forward (query)
  "Start a telega-compatible forward invocation for QUERY."
  (interactive
   (list (read-string "QQ search (forward): " qq-chat--last-search-query)))
  (qq-chat-search query t))

(defun qq-chat-search-next ()
  "Jump to the newer result of the authoritative in-chat search."
  (interactive)
  (when (qq-chat--msg-filter-active-p)
    (user-error "qq: cancel the active message filter before in-place search"))
  (if (and (null qq-chat--search-index) qq-chat--search-owner)
      (message "qq: message search is already loading")
    (if (null qq-chat--search-index)
      (call-interactively #'qq-chat-search)
      (if (> qq-chat--search-index 0)
          (qq-chat--show-search-result (1- qq-chat--search-index))
        (message "qq: no newer match for %s" qq-chat--last-search-query)))))

(defun qq-chat-search-prev ()
  "Continue the authoritative in-chat search toward older messages."
  (interactive)
  (when (qq-chat--msg-filter-active-p)
    (user-error "qq: cancel the active message filter before in-place search"))
  (if (and (null qq-chat--search-index) qq-chat--search-owner)
      (message "qq: message search is already loading")
    (if (null qq-chat--search-index)
      (call-interactively #'qq-chat-search)
      (let ((next-index (1+ qq-chat--search-index)))
        (cond
         ((nth next-index qq-chat--search-results)
          (qq-chat--show-search-result next-index))
         (qq-chat--search-request
          (message "qq: message search is already loading"))
         (qq-chat--search-next-cursor
          (qq-chat--start-search-request 'older next-index))
         (t
          (message "qq: no older match for %s"
                   qq-chat--last-search-query)))))))

(defun qq-chat-search-results (&optional query)
  "Open paginated search results for the current chat and optional QUERY."
  (interactive)
  (qq-search-open qq-chat--session-key query))

(defun qq-chat--reset-search-state (&optional defer-presentation-p)
  "Cancel and clear the independent in-place search state.

When DEFER-PRESENTATION-P is non-nil, leave owned overlays for the next view
sync to remove.  Return non-nil when any search state or highlighting was
active."
  (let ((active-p (or qq-chat--last-search-query
                      qq-chat--search-request
                      qq-chat--search-owner
                      qq-chat--search-results
                      qq-chat--search-highlight-overlays)))
    (qq-chat--cancel-search-request)
    (unless defer-presentation-p
      (qq-chat--clear-search-highlights))
    (setq qq-chat--last-search-query nil
          qq-chat--search-results nil
          qq-chat--search-results-tail nil
          qq-chat--search-seen (make-hash-table :test #'equal)
          qq-chat--search-consumed-cursors (make-hash-table :test #'equal)
          qq-chat--search-index nil
          qq-chat--search-direction 'older
          qq-chat--search-anchor nil
          qq-chat--search-completed-p nil
          qq-chat--search-next-cursor nil)
    active-p))

(defun qq-chat-search-cancel ()
  "Cancel the independent in-place search without changing the timeline."
  (interactive)
  (let ((active-p (qq-chat--reset-search-state)))
    (qq-chat--header-line-update)
    (message (if active-p
                 "qq: message search canceled"
               "qq: no active message search"))))

(defconst qq-chat--message-filter-specs
  '(("search" . qq-chat-filter-search)
    ("hashtag" . qq-chat-filter-hashtag))
  "Materialized QQ message filters supported by the native fork protocol.")

(defun qq-chat--read-filter-command ()
  "Read and return one supported materialized-filter command."
  (let* ((completion-ignore-case t)
         (name (completing-read
                "Chat Messages Filter: "
                (mapcar #'car qq-chat--message-filter-specs)
                nil t)))
    (or (alist-get name qq-chat--message-filter-specs nil nil #'equal)
        (user-error "qq: unsupported message filter %s" name))))

(defun qq-chat-filter (filter-command)
  "Choose and activate materialized FILTER-COMMAND for this chat.

Like `telega-chatbuf-filter', `C-c /' is a require-match filter chooser.  A
selected entry may run a second reader, such as the query reader for `search'."
  (interactive (list (qq-chat--read-filter-command)))
  (unless (commandp filter-command 'for-interactive)
    (user-error "qq: invalid message filter command"))
  (call-interactively filter-command))

(defconst qq-chat--inplace-search-specs
  '(("query" . qq-chat-search))
  "In-place search commands exposed by the telega-style chooser.")

(defun qq-chat--read-inplace-search-command ()
  "Read and return one supported in-place search command."
  (let* ((completion-ignore-case t)
         (name (completing-read
                "In-place Search: "
                (mapcar #'car qq-chat--inplace-search-specs)
                nil t)))
    (or (alist-get name qq-chat--inplace-search-specs nil nil #'equal)
        (user-error "qq: unsupported in-place search %s" name))))

(defun qq-chat-inplace-search (search-command)
  "Choose and invoke in-place SEARCH-COMMAND for this chat."
  (interactive (list (qq-chat--read-inplace-search-command)))
  (unless (commandp search-command 'for-interactive)
    (user-error "qq: invalid in-place search command"))
  (call-interactively search-command))

(defun qq-chat--invalidate-normal-history-requests ()
  "Invalidate every normal-history request before materializing a filter.

Initial and exact-open transports have explicit tokens and are canceled.
Latest, older, newer, and any remaining around request are rejected through
AppKit's identity barrier.  The established normal window is not changed."
  (let ((barrier
         (appkit-chat-history-request-begin
          'filter-barrier (list 'filter-history-barrier))))
    (appkit-chat-history-request-end barrier))
  ;; Revoke controller ownership before transport cancellation: even a
  ;; reentrant cancel callback is already stale and cannot mutate the window.
  (qq-chat--cancel-initial-history-request)
  (qq-chat--cancel-open-message-request))

(defun qq-chat--filter-point-state ()
  "Return a semantic snapshot of the current chat point and buffer contents."
  (let* ((window (and (eq (window-buffer (selected-window)) (current-buffer))
                      (selected-window)))
         (position (if (and window (window-live-p window))
                       (window-point window)
                     (point)))
         (input-start (appkit-chatbuf-input-start-position))
         (anchor (or (get-text-property position 'qq-chat-message-anchor)
                     (save-excursion
                       (goto-char position)
                       (get-text-property (line-beginning-position)
                                          'qq-chat-message-anchor)))))
    (list :window window
          :tick (buffer-chars-modified-tick)
          :place
          (cond
           ((and input-start (appkit-chatbuf-point-in-input-p position))
            (list 'input (- position input-start)))
           (anchor
            (list 'message anchor
                  (if-let* ((anchor-position
                             (qq-chat--message-position anchor)))
                      (- position anchor-position)
                    0)))
           (t (list 'buffer position))))))

(defun qq-chat--filter-point-state-current-p (owner)
  "Return non-nil when point still matches OWNER's dispatch snapshot."
  (equal (plist-get owner :point-state)
         (qq-chat--filter-point-state)))

(defun qq-chat--position-after-filter-first-page (owner)
  "Restore OWNER's semantic origin or initial composer position."
  (unless (and (plist-get owner :origin-id)
               (qq-chat--goto-loaded-message
                (plist-get owner :origin-id) nil))
    (goto-char (or (appkit-chatbuf-input-start-position) (point-max)))))

(defun qq-chat--filter-request-current-p (buffer session-key owner)
  "Return non-nil when OWNER still owns BUFFER's filter request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (qq-chat--msg-filter-active-p)
              (qq-chat--captured-view-current-p
               (plist-get owner :view))
              (eq qq-chat--filter-owner owner)))))

(defun qq-chat--cancel-filter-request ()
  "Cancel and forget the current materialized-filter request."
  (let ((request qq-chat--filter-request)
        (owner qq-chat--filter-owner))
    (when (listp owner)
      (setf (plist-get owner :pending) nil)
      (setf (plist-get owner :patches) nil))
    (setq qq-chat--filter-request nil
          qq-chat--filter-owner nil
          qq-chat--filter-sync-request nil
          qq-chat--filter-auto-load-p nil)
    (when request
      (condition-case nil
          (qq-api-cancel-request request)
        (quit
         (setq quit-flag nil)
         nil)
        (error nil)))))

(defun qq-chat--merge-filter-results (existing page)
  "Append newest-first PAGE to EXISTING without duplicate message ids."
  (let ((seen (make-hash-table :test #'equal))
        merged)
    (dolist (result (append existing page))
      (let ((id (qq-chat--filter-result-id result)))
        (unless id
          (error "qq: materialized filter result lacks an exact message id"))
        (unless (gethash id seen)
          (puthash id t seen)
          (push result merged))))
    (nreverse merged)))

(defun qq-chat--highlight-filter-results ()
  "Highlight the active filter query only inside projected message bodies."
  (qq-chat--clear-search-highlights)
  (when-let* ((query (plist-get qq-chat--msg-filter :query)))
    (dolist (result (plist-get qq-chat--msg-filter :items))
      (qq-chat--add-search-text-highlights
       (qq-chat--filter-result-id result) query))))

(defun qq-chat--filter-page-succeeded
    (buffer session-key owner page)
  "Accept materialized filter PAGE when OWNER remains current."
  (when (qq-chat--filter-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let* ((view (plist-get owner :view))
             (page-items
              (mapcar (lambda (result)
                        (qq-chat--normalize-filter-result owner result))
                      (or (alist-get 'results page) '())))
             (existing (or (plist-get owner :existing) '()))
             (items
              (qq-chat--merge-filter-results
               existing page-items))
             (added (- (length items) (length existing)))
             (append-p (plist-get owner :append))
             (point-owned-p
              (and (not append-p)
                   (qq-chat--filter-point-state-current-p owner)))
             (filter (copy-sequence (plist-get owner :filter))))
        (setf (plist-get owner :pending) nil)
        (setf (plist-get owner :patches) nil)
        (setq qq-chat--filter-request nil
              qq-chat--filter-owner nil)
        (setq filter (plist-put filter :active t)
              filter (plist-put filter :items items)
              filter (plist-put filter :next-cursor
                                (alist-get 'next_cursor page)))
        (setq qq-chat--msg-filter filter)
        (setq qq-chat--filter-auto-load-p
              (and view
                   (= added 0)
                   (qq-chat--msg-filter-has-more-p)))
        (when view
          (setq qq-chat--filter-sync-request
                (list :owner owner
                      :point-owner (and point-owned-p owner)
                      :view view))
          (appkit-request-sync
           view
           :structure t
           :parts '(timeline frame)
           :position t))
        (cond
         ;; Empty and duplicate-only native pages are valid capabilities, not
         ;; end-of-results.  Consume their cursor until data or EOF arrives.
         ((and (= added 0) (qq-chat--msg-filter-has-more-p))
          nil)
         (append-p
          ;; AppKit already preserved semantic point/window position while
          ;; older rows were prepended.  Never force an append back to input.
          (message "qq: filter -> %s" (qq-chat--msg-filter-title)))
         (point-owned-p
          (message "qq: filter -> %s" (qq-chat--msg-filter-title)))
         (t
          (message "qq: filter -> %s" (qq-chat--msg-filter-title))))))))

(defun qq-chat--filter-page-failed
    (buffer session-key owner response reason)
  "Finish OWNER's materialized-filter request with explicit failure REASON."
  (when (qq-chat--filter-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setf (plist-get owner :pending) nil)
        (setf (plist-get owner :patches) nil)
        (setq qq-chat--filter-request nil
              qq-chat--filter-owner nil
              qq-chat--filter-auto-load-p nil
              qq-chat--msg-filter
              (plist-put qq-chat--msg-filter :next-cursor nil))
        (when view
          (setq qq-chat--filter-sync-request
                (list :owner owner :view view))
          (appkit-request-sync
           view :structure t :parts '(timeline frame) :position t)))
      (qq-api--default-error response reason))))

(defun qq-chat--run-filter (filter &optional append)
  "Run materialized FILTER, appending its next page when APPEND is non-nil."
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let* ((type (alist-get 'type
                          (qq-state-session-key-identity qq-chat--session-key)))
         (existing
          (if append (or (plist-get qq-chat--msg-filter :items) '()) '()))
         (cursor (and append (plist-get qq-chat--msg-filter :next-cursor))))
    (unless (memq type '(group private))
      (user-error "qq: message filters are unsupported for %s sessions" type))
    (when (and append (not (and (stringp cursor)
                                (not (string-empty-p cursor)))))
      (user-error "qq: no more filtered messages available"))
    (qq-chat--cancel-filter-request)
    (unless append
      (qq-chat--invalidate-normal-history-requests))
    (let* ((buffer (current-buffer))
           (session-key qq-chat--session-key)
           (origin (and (not append)
                        (ignore-errors (qq-chat--message-at-point))))
           (owner (list :filter (copy-sequence filter)
                        :existing existing
                        :append (and append t)
                        :view nil
                        :origin-id (and origin (alist-get 'server-id origin))
                        :point-state nil
                        :observation-token nil
                        :patches nil
                        :pending nil))
           request)
      ;; A cursor capability is single-use.  Remove it from visible state
      ;; before dispatch so a synchronous error cannot accidentally replay it.
      (setq filter (copy-sequence filter)
            filter (plist-put filter :active t)
            filter (plist-put filter :items existing)
            filter (plist-put filter :next-cursor nil)
            qq-chat--msg-filter filter
            qq-chat--filter-owner owner
            qq-chat--filter-request nil)
      (qq-chat-render)
      (setf (plist-get owner :view) (qq-chat--live-current-view))
      ;; Point ownership begins after rendering the searching projection.  A
      ;; later callback may restore the pre-filter semantic origin only while
      ;; the user has left this post-dispatch point and composer untouched.
      (setf (plist-get owner :point-state) (qq-chat--filter-point-state))
      ;; Capture the state clock at the actual transport boundary.  Notices
      ;; handled before this point are already part of any future snapshot and
      ;; must not be replayed as deltas over that snapshot.
      (setf (plist-get owner :observation-token)
            (qq-state-message-observation-token))
      (setf (plist-get owner :pending) t)
      (condition-case error-data
          (progn
            (setq request
                  (if append
                      (qq-api-filter-messages-next
                       session-key cursor
                       (lambda (page)
                         (qq-chat--filter-page-succeeded
                          buffer session-key owner page))
                       (lambda (response reason)
                         (qq-chat--filter-page-failed
                          buffer session-key owner response reason)))
                    (qq-api-filter-messages-start
                     session-key (plist-get filter :query)
                     (lambda (page)
                       (qq-chat--filter-page-succeeded
                        buffer session-key owner page))
                     (lambda (response reason)
                       (qq-chat--filter-page-failed
                        buffer session-key owner response reason)))))
            (when (and (qq-chat--filter-request-current-p
                        buffer session-key owner)
                       (plist-get owner :pending))
              (setq qq-chat--filter-request request))
            request)
        (error
         (qq-chat--filter-page-failed
          buffer session-key owner nil (error-message-string error-data))
         nil)))))

(defun qq-chat--activate-query-filter (query title)
  "Materialize authoritative QUERY under concise filter TITLE."
  (unless (and query (not (string-empty-p query)))
    (user-error "qq: empty filter query"))
  (when (> (length query) 512)
    (user-error "qq: filter query must be at most 512 characters"))
  (qq-chat--reset-search-state)
  (setq qq-chat--last-search-query nil)
  (qq-chat--run-filter (list :title title :query query)))

(defun qq-chat-filter-search (query)
  "Materialize current-chat messages matching authoritative QUERY."
  (interactive (list (read-string "Filter messages: "
                                  qq-chat--last-search-query)))
  (setq query (and (stringp query) (string-trim query)))
  (qq-chat--activate-query-filter query (format "search \"%s\"" query)))

(defun qq-chat-filter-hashtag (hashtag)
  "Materialize messages containing HASHTAG."
  (interactive (list (read-string "Hashtag: #")))
  (setq hashtag (string-trim (or hashtag "")))
  (when (string-empty-p hashtag)
    (user-error "qq: empty hashtag"))
  (let ((query (concat (unless (string-prefix-p "#" hashtag) "#") hashtag)))
    (qq-chat--activate-query-filter query (format "hashtag %s" query))))

(defun qq-chat-filter-refresh ()
  "Restart the active materialized filter from its authoritative first page."
  (interactive)
  (unless (qq-chat--msg-filter-active-p)
    (user-error "qq: no active message filter"))
  (qq-chat--run-filter
   (list :title (qq-chat--msg-filter-title)
         :query (plist-get qq-chat--msg-filter :query))))

(defun qq-chat-filter-load-more (&optional quiet)
  "Load one older materialized-filter page, if available."
  (interactive)
  (cond
   ((not (qq-chat--msg-filter-active-p))
    (unless quiet (message "qq: no active message filter")))
   (qq-chat--filter-owner
    (unless quiet (message "qq: filter search is already loading")))
   ((not (qq-chat--msg-filter-has-more-p))
    (unless quiet (message "qq: no more filtered messages available")))
   (t
    (qq-chat--run-filter qq-chat--msg-filter t))))

(defun qq-chat--deactivate-filter (&optional defer-presentation-p)
  "Clear the materialized filter without changing the normal history window."
  (qq-chat--cancel-filter-request)
  (setq qq-chat--msg-filter nil)
  (unless defer-presentation-p
    (qq-chat--clear-search-highlights)))

(defun qq-chat-filter-cancel ()
  "Cancel the active filter and restore the normal timeline.

When point names a filter-only message, preserve that semantic position by
opening one exact around-message window.  With no filter, remove only in-place
search highlights; `qq-chat-search-cancel' owns full result-state cleanup."
  (interactive)
  (if (not (qq-chat--msg-filter-active-p))
      (if qq-chat--search-highlight-overlays
          (progn
            (qq-chat--clear-search-highlights)
            (qq-chat--header-line-update)
            (message "qq: message search highlight cleared"))
        (message "qq: no active message filter or search highlight"))
    (let* ((session-key qq-chat--session-key)
           (message (ignore-errors (qq-chat--message-at-point)))
           (message-id (and message (alist-get 'server-id message))))
      (qq-chat--deactivate-filter)
      (qq-chat-render)
      (cond
       ((and message-id (qq-chat--goto-loaded-message message-id nil)))
       (message-id
        (qq-chat-open-message session-key message-id))
       ((not (qq-chat--history-window-known-p))
        (qq-chat--load-initial-history (current-buffer) session-key)))
      (message "qq: message filter canceled"))))

(defun qq-chat-render ()
  "Synchronize current chat frame and projected timeline from local state."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-view)
  (when (appkit-chatbuf-input-region-bounds)
    (appkit-chatbuf-input-state-sync :reset-history-p nil))
  (qq-chat--header-line-update)
  ;; Establish the EWOC rows before the trailing composer.  On an empty EWOC
  ;; the footer's tail boundary is not stable until first reconciliation;
  ;; binding input first can leave the prompt inside the message region.
  (qq-chat--sync-timeline)
  (qq-chat--update-frame))

(defun qq-chat-refresh ()
  "Refresh the active filter or rebuild the authoritative latest window."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   ((qq-chat--msg-filter-active-p)
    (qq-chat-filter-refresh))
   ((eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
    (qq-chat--load-initial-history
     (current-buffer) qq-chat--session-key))
   (t
    (qq-chat-return-to-latest t))))

(defun qq-chat--note-history-window (meta &optional remote-latest-id)
  "Record the one around-message history slice described by META.

REMOTE-LATEST-ID is captured before the around batch mutates the local cache;
when it is unknown, keep the newer edge partial rather than claiming that the
batch is authoritative latest history."
  (pcase-let* ((`(,oldest . ,newest) (qq-chat--history-batch-bounds meta))
               (batch-ids (plist-get meta :batch-message-ids))
               ;; `qq-chat--begin-around-history-window' seeds this state
               ;; before the request.  If it now differs from the optional
               ;; snapshot, a live event advanced the frontier in flight and
               ;; must win without numerically comparing snowflake ids.
               (latest (or qq-chat--remote-latest-id remote-latest-id))
               (at-latest (and latest
                               (or (member latest batch-ids)
                                   (equal latest newest))))
               (stale-frontier
                (and (not at-latest)
                     (qq-chat--history-frontier-behind-batch-p
                      latest newest batch-ids)))
               (frontier (cond
                          (at-latest newest)
                          (stale-frontier nil)
                          (t latest))))
    (setq qq-chat--remote-latest-id frontier)
    (appkit-chat-history-older-loaded-set nil)
    (when newest
      (qq-chat--set-history-window oldest (unless at-latest newest)))))

(defun qq-chat-load-newer-messages (&optional quiet)
  "Extend the current contiguous history window by one newer page."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((cursor (appkit-chat-history-window-last-key)))
    (cond
     ((qq-chat--msg-filter-active-p)
      (unless quiet
        (message "qq: filtered results only paginate toward older matches")))
     ((not (qq-chat--history-window-known-p))
      (if (appkit-chat-history-loading-p)
          (unless quiet (message "qq: initial history is still loading"))
        (qq-chat-return-to-latest)))
     ((not cursor)
      (unless quiet (message "qq: latest history is already loaded")))
     ((appkit-chat-history-loading-p)
      (unless quiet (message "qq: history load already in progress")))
     ((eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
      (qq-chat--load-initial-history
       (current-buffer) qq-chat--session-key))
     (t
      (qq-chat--cancel-initial-history-request)
      (let ((session-key qq-chat--session-key)
            (buffer (current-buffer))
            (view (qq-chat--ensure-view))
            (requested (max 1 qq-history-fetch-count))
            (owner (list 'newer-history qq-chat--session-key cursor))
            (point-anchor (and (not (appkit-chatbuf-point-in-input-p))
                               (get-text-property (point)
                                                  'qq-chat-message-anchor)))
            (point-anchor-offset 0))
        (when point-anchor
          (when-let* ((anchor-pos (qq-chat--message-position point-anchor)))
            (setq point-anchor-offset (- (point) anchor-pos))))
        (appkit-chat-history-request-begin 'newer owner)
        (when view
          (appkit-request-sync view :part 'frame))
        (qq-api-fetch-history-page
         session-key cursor 'newer
         (lambda (meta)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (qq-chat--captured-view-current-p view)
                          (equal qq-chat--session-key session-key)
                          (appkit-chat-history-request-current-p owner))
                 (appkit-chat-history-request-end owner)
                 (pcase-let* ((`(,_oldest . ,newest)
                                (qq-chat--history-batch-bounds meta))
                               (added (or (plist-get meta :added-count) 0))
                               (ids (plist-get meta :batch-message-ids))
                               (messages
                                (qq-state-session-messages session-key))
                               (latest qq-chat--remote-latest-id)
                               (reached-latest
                                (and latest
                                     (or (member latest ids)
                                         (equal latest newest))))
                               (stale-frontier
                                (and (not reached-latest)
                                     (qq-chat--history-frontier-behind-batch-p
                                      latest newest ids)))
                               (effective-latest
                                (unless stale-frontier latest))
                               (progressed
                                (and newest
                                     (qq-chat--message-id-after-p
                                      newest cursor messages)))
                               (finished
                                (or reached-latest
                                    (and (not progressed)
                                         (or (null effective-latest)
                                             (equal effective-latest cursor))))))
                   (when reached-latest
                     (setq qq-chat--remote-latest-id newest))
                   (when stale-frontier
                     (setq qq-chat--remote-latest-id nil))
                   (qq-chat--set-history-window
                    (appkit-chat-history-window-first-key)
                    (cond
                     (finished nil)
                     (progressed newest)
                     (t cursor)))
                   (when (and (not progressed) (not finished))
                     (appkit-chat-history-newer-stalled-set cursor))
                   (qq-chat--request-callback-sync
                    view
                    (and point-anchor
                         (lambda ()
                           (when-let* ((anchor-pos
                                        (qq-chat--message-position
                                         point-anchor)))
                             (goto-char (+ anchor-pos point-anchor-offset))
                             (when-let* ((window
                                          (get-buffer-window buffer t)))
                               (set-window-point window (point)))))))
                   (unless quiet
                     (if finished
                         (message "qq: newer history caught up")
                       (message "qq: loaded %d newer message%s"
                                added (if (= added 1) "" "s")))))))))
         (lambda (response reason)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (qq-chat--captured-view-current-p view)
                          (equal qq-chat--session-key session-key)
                          (appkit-chat-history-request-current-p owner))
                 (appkit-chat-history-request-end owner)
                 (qq-chat--request-callback-sync view)
                 (qq-api--default-error response reason)))))
         requested))))))

(defun qq-chat--goto-latest-window-end (&optional preferred-id)
  "Move to PREFERRED-ID in the latest window, or to its composer end."
  (let ((latest (or preferred-id
                    qq-chat--remote-latest-id
                    (qq-chat--authoritative-latest-message-id))))
    (unless (and latest (qq-chat--goto-loaded-message latest nil))
      (goto-char (or (appkit-chatbuf-input-start-position) (point-max))))))

(defun qq-chat--mark-latest-window-read (&optional message-id)
  "Mark MESSAGE-ID, or the newest visible server message, as read."
  (if-let* ((message (or (and message-id
                              (qq-chat--message-by-server-id message-id))
                         (qq-chat--latest-visible-server-message))))
      (qq-chat--mark-message-viewed message t)
    (user-error "qq: this chat has no server message to mark as read")))

(defun qq-chat--adopt-live-frontier-window
    (frontier-at-start observed-frontier)
  "Establish a minimal latest window from an in-flight live observation.

Return OBSERVED-FRONTIER when it differs from FRONTIER-AT-START and still
names a canonical cached server message.  A nil-cursor history response may
be empty because its snapshot preceded that websocket delivery; the single
live row is nevertheless a provably contiguous window attached to latest.
Older paging can extend its exact first edge normally."
  (when (and observed-frontier
             (not (equal observed-frontier frontier-at-start))
             (qq-chat--message-by-server-id observed-frontier))
    (setq qq-chat--remote-latest-id observed-frontier)
    (appkit-chat-history-older-loaded-set nil)
    (qq-chat--set-history-window observed-frontier nil)
    observed-frontier))

(defun qq-chat-return-to-latest (&optional force mark-read)
  "Rebuild the authoritative latest history window and move to its end.

With FORCE, refetch even when the current slice is already attached to live
history.  With MARK-READ, submit the exact latest string id only after that
window is available.  This follows telega's latest/read-all behavior rather
than jumping across an unfilled cached gap."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (when (qq-chat--msg-filter-active-p)
    (qq-chat--deactivate-filter)
    (qq-chat-render))
  (cond
   ((appkit-chat-history-loading-p)
    (message "qq: history load already in progress"))
   ((eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
    (qq-chat--load-initial-history
     (current-buffer) qq-chat--session-key))
   ((and (not force)
         (qq-chat--history-window-known-p)
         (not (qq-chat--history-window-partial-p)))
    (qq-chat--goto-latest-window-end)
    (when mark-read
      (qq-chat--mark-latest-window-read))
    (message "qq: latest history"))
   (t
    (qq-chat--cancel-initial-history-request)
    (let ((session-key qq-chat--session-key)
          (buffer (current-buffer))
          (view (qq-chat--ensure-view))
          (owner (list :kind 'latest-history
                       :session-key qq-chat--session-key
                       :frontier-at-start
                       qq-chat--remote-latest-id)))
      (appkit-chat-history-request-begin 'newer owner)
      (when view
        (appkit-request-sync view :part 'frame))
      (qq-api-fetch-older-history
       session-key nil
       (lambda (meta)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (pcase-let* ((`(,oldest . ,newest)
                              (qq-chat--history-batch-bounds meta))
                            (frontier-at-start
                             (plist-get owner :frontier-at-start))
                            (observed-frontier qq-chat--remote-latest-id)
                            (latest
                             (if (equal observed-frontier frontier-at-start)
                                 newest
                               observed-frontier)))
                 (if newest
                     (progn
                       (setq qq-chat--remote-latest-id latest)
                       (appkit-chat-history-older-loaded-set nil)
                       (qq-chat--set-history-window oldest nil)
                       (qq-chat--request-callback-sync
                        view
                        (lambda ()
                          (qq-chat--goto-latest-window-end latest)
                          (when mark-read
                            (qq-chat--mark-latest-window-read latest))
                          (message "qq: latest history loaded"))))
                   (if-let* ((live-frontier
                              (qq-chat--adopt-live-frontier-window
                               frontier-at-start observed-frontier)))
                       (qq-chat--request-callback-sync
                        view
                        (lambda ()
                          (qq-chat--goto-latest-window-end live-frontier)
                          (when mark-read
                            (qq-chat--mark-latest-window-read live-frontier))
                          (message
                           "qq: latest history followed live delivery")))
                     (setq qq-chat--remote-latest-id nil)
                     (qq-chat--set-empty-history-window)
                     (qq-chat--request-callback-sync
                      view
                      (lambda ()
                        (qq-chat--goto-latest-window-end)
                        (message
                         "qq: latest history returned no messages")))))))))
       (lambda (response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (qq-chat--request-callback-sync view)
               (qq-api--default-error response reason)))))))))))

(defun qq-chat--load-older-guild-messages (&optional quiet)
  "Extend the current Guild sequence range toward older messages."
  (unless (qq-protocol--decimal-string-p
           qq-chat--guild-history-start-sequence)
    (user-error "qq: Guild history range is not initialized; refresh first"))
  (if (equal qq-chat--guild-history-start-sequence "0")
      (progn
        (appkit-chat-history-older-loaded-set t)
        (unless quiet (message "qq: reached beginning of channel history")))
    (let* ((session-key qq-chat--session-key)
           (end-sequence
            (qq-chat--guild-sequence-offset
             qq-chat--guild-history-start-sequence -1))
           (start-sequence (qq-chat--guild-page-start end-sequence))
           (buffer (current-buffer))
           (view (qq-chat--ensure-view))
           (owner (list 'older-guild-history session-key start-sequence)))
      (appkit-chat-history-request-begin 'older owner)
      (when view
        (appkit-request-sync view :part 'frame))
      (qq-api-fetch-guild-message-range
       session-key start-sequence end-sequence
       (lambda (records)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (let* ((ids (qq-chat--guild-record-ids records))
                      (oldest (or (car ids)
                                  (appkit-chat-history-window-first-key))))
                 (setq qq-chat--guild-history-start-sequence start-sequence)
                 (appkit-chat-history-older-loaded-set
                  (equal start-sequence "0"))
                 (when oldest
                   (qq-chat--set-history-window oldest nil))
                 (qq-chat--request-callback-sync view)
                 (unless quiet
                   (message "qq: loaded %d older channel message%s"
                            (length records)
                            (if (= (length records) 1) "" "s"))))))))
       (lambda (response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (qq-chat--request-callback-sync view)
               (qq-api--default-error response reason)))))))))

(defun qq-chat--forum-history-anchors (session-key)
  "Return oldest and newest loaded forum anchors for SESSION-KEY."
  (let ((posts
         (seq-filter
          (lambda (message)
            (equal (alist-get 'message-type message) "guild-forum-post"))
          (qq-state-session-messages session-key))))
    (cons (qq-state-message-anchor (car posts))
          (qq-state-message-anchor (car (last posts))))))

(defun qq-chat--load-older-guild-forum-posts (&optional quiet)
  "Extend the current QQ Guild forum window by one opaque cursor page."
  (unless (qq-chat--guild-forum-session-p qq-chat--session-key)
    (user-error "qq: current channel is not a forum"))
  (if (appkit-chat-history-older-loaded-p)
      (unless quiet (message "qq: reached beginning of forum history"))
    (unless (qq-api-non-empty-string-p qq-chat--guild-forum-next-cursor)
      (user-error "qq: forum cursor is not initialized; refresh first"))
    (let* ((session-key qq-chat--session-key)
           (cursor qq-chat--guild-forum-next-cursor)
           (buffer (current-buffer))
           (view (qq-chat--ensure-view))
           (owner (list 'older-guild-forum session-key cursor)))
      (appkit-chat-history-request-begin 'older owner)
      (when view
        (appkit-request-sync view :part 'frame))
      (qq-api-fetch-guild-forum-page
       session-key cursor
       (lambda (page)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (setq qq-chat--guild-forum-next-cursor
                     (alist-get 'next_cursor page))
               (appkit-chat-history-older-loaded-set
                (eq (alist-get 'finished page) t))
               (pcase-let ((`(,oldest . ,_newest)
                            (qq-chat--forum-history-anchors session-key)))
                 (if oldest
                     (qq-chat--set-history-window oldest nil)
                   (qq-chat--set-empty-history-window)))
               (qq-chat--request-callback-sync view)
               (unless quiet
                 (message "qq: loaded %d older forum post%s"
                          (length (alist-get 'posts page))
                          (if (= (length (alist-get 'posts page)) 1)
                              "" "s")))))))
       (lambda (response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (qq-chat--request-callback-sync view)
               (qq-api--default-error response reason)))))))))

(defun qq-chat-load-older-messages (&optional quiet)
  "Extend the current contiguous history window by one older page.

Ordinary chats page by exact snowflake cursor.  Guild channels page by their
independent native sequence range."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (cond
   ((qq-chat--msg-filter-active-p)
    (qq-chat-filter-load-more quiet))
   ((not (qq-chat--history-window-known-p))
    (unless quiet (message "qq: history window is not initialized")))
   ((appkit-chat-history-older-loaded-p)
    (unless quiet (message "qq: no older messages available")))
   ((appkit-chat-history-loading-p)
    (unless quiet (message "qq: history load already in progress")))
   ((qq-chat--guild-forum-session-p qq-chat--session-key)
    (qq-chat--load-older-guild-forum-posts quiet))
   ((eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
    (qq-chat--load-older-guild-messages quiet))
   (t
    (qq-chat--cancel-initial-history-request)
    (let* ((session-key qq-chat--session-key)
           (before (or (appkit-chat-history-window-first-key)
                       (when-let* ((first (seq-find
                                           (lambda (message)
                                             (qq-api-message-id-p
                                              (alist-get 'server-id message)))
                                           (qq-chat--timeline-messages))))
                         (alist-get 'server-id first))
                       (qq-state-session-oldest-message-id session-key)))
           (buffer (current-buffer))
           (view (qq-chat--ensure-view))
           (owner (list 'older-history qq-chat--session-key before)))
      (unless before
        (user-error "qq: no oldest message cursor; refresh first (C-c g)"))
      (appkit-chat-history-request-begin 'older owner)
      (when view
        (appkit-request-sync view :part 'frame))
      (qq-api-fetch-older-history
       session-key
       before
       (lambda (meta)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (pcase-let* ((`(,oldest . ,_newest)
                              (qq-chat--history-batch-bounds meta))
                            (added (or (plist-get meta :added-count) 0))
                            (messages
                             (qq-state-session-messages session-key))
                            (progressed
                             (and oldest
                                  (qq-chat--message-id-after-p
                                   before oldest messages))))
                 (cond
                  ((not progressed)
                   (appkit-chat-history-older-loaded-set t)
                   (unless (or quiet qq-chat--pending-jump-id)
                     (message "qq: reached beginning of history")))
                  (t
                   (qq-chat--set-history-window
                    oldest (appkit-chat-history-window-last-key))
                   (unless (or quiet qq-chat--pending-jump-id)
                     (message "qq: loaded %d older message%s"
                              added
                              (if (= added 1) "" "s")))))
                 (let ((pending-jump qq-chat--pending-jump-id))
                   (qq-chat--request-callback-sync
                    view
                    (and pending-jump
                         (lambda ()
                           ;; Jump uses seek-at-target, not load-older chains.
                           (qq-chat--finish-jump-if-loaded
                            pending-jump))))))))))
       (lambda (response reason)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and (qq-chat--captured-view-current-p view)
                        (equal qq-chat--session-key session-key)
                        (appkit-chat-history-request-current-p owner))
               (appkit-chat-history-request-end owner)
               (if (qq-api--history-exhausted-error-p response reason)
                   (progn
                     (appkit-chat-history-older-loaded-set t)
                     (let ((pending-jump qq-chat--pending-jump-id))
                       (qq-chat--request-callback-sync
                        view
                        (and pending-jump
                             (lambda ()
                               (qq-chat--finish-jump-if-loaded
                                pending-jump)))))
                     (unless (or quiet qq-chat--pending-jump-id)
                       (message "qq: reached beginning of history")))
                 (qq-chat--request-callback-sync view)
                 (qq-api--default-error response reason)))))))))))

(defun qq-chat--restore-failed-send
    (buffer session-key owner draft-state aux-state
            &optional allow-partial-p captured-view)
  "Restore one failed send when OWNER still owns pristine BUFFER composer.

SESSION-KEY prevents a reused buffer from receiving another chat's draft.
DRAFT-STATE preserves rich input properties and AUX-STATE preserves its reply.
Normally the cleared composer must still be pristine.  ALLOW-PARTIAL-P is for
rolling back synchronous errors inside the destructive clear transaction; its
opaque OWNER must still be current.  Return non-nil only when restoration
happened."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (equal qq-chat--session-key session-key)
                 (eq qq-chat--send-restore-owner owner)
                 (or (null captured-view)
                     (qq-chat--captured-view-current-p captured-view))
                 (or allow-partial-p
                     (and (equal-including-properties
                           (appkit-chatbuf-input-state) "")
                          (null (appkit-chatbuf-aux-state)))))
        ;; Revoke before rendering so restoration cannot be mistaken for a
        ;; still-pristine send slot by a reentrant callback.
        (setq qq-chat--send-restore-owner nil)
        (appkit-chatbuf-input-state-set draft-state :reset-history-p t)
        (if aux-state
            (appkit-chatbuf-aux-set (copy-tree aux-state))
          (appkit-chatbuf-aux-reset))
        (if captured-view
            (progn
              (setq qq-chat--send-sync-request
                    (list :view captured-view :owner owner))
              (appkit-request-sync
               captured-view :part 'frame :position t))
          (qq-chat--render-canonical-input)
          (qq-chat--update-frame)
          (goto-char
           (or (appkit-chatbuf-input-logical-end-position) (point-max))))
        (qq-chat--maybe-update-my-action-from-input)
        t))))

(defun qq-chat-send-message ()
  "Send current chat draft."
  (interactive)
  (unless qq-chat--session-key
    (user-error "qq: this buffer is not bound to a session"))
  (qq-chat--ensure-composer-visible)
  ;; Never submit a stale canonical cache or a partially edited rich object.
  (appkit-chatbuf-input-prune-broken-objects)
  (qq-chat--sync-draft-from-buffer)
  (let* ((buffer (current-buffer))
         (session-key qq-chat--session-key)
         (view (qq-chat--ensure-view))
         (draft-state (appkit-chatbuf-input-state))
         (aux-state (copy-tree (appkit-chatbuf-aux-state)))
         (restore-owner (make-symbol "qq-chat-send-restore"))
         (text (qq-chat--current-draft-string))
         (reply-message (qq-chat--reply-message))
         (reply-id (and reply-message
                        (qq-api-validate-message-id
                         (alist-get 'server-id reply-message)
                         "reply draft")))
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
      (setq qq-chat--send-restore-owner restore-owner)
      (condition-case err
          (progn
            (appkit-chatbuf-input-state-clear :reset-history-p t)
            ;; telega: empty input after send → chatActionCancel
            (qq-chat--set-my-action 'cancel)
            ;; This is the send transaction's own clear, not a later user
            ;; reply change, so retain restore ownership across it.
            (appkit-chatbuf-aux-reset)
            (qq-chat--render-canonical-input)
            (qq-chat--update-frame)
            (qq-api-send-message
             session-key send-segments raw-message
             (lambda (_response)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (eq qq-chat--send-restore-owner restore-owner)
                     (setq qq-chat--send-restore-owner nil)))))
             (lambda (response reason)
               (let ((restored
                      (qq-chat--restore-failed-send
                       buffer session-key restore-owner draft-state aux-state
                       nil view)))
                 (qq-api--default-error
                  response
                  (if restored
                      (format "%s (draft restored)" (or reason "send failed"))
                    reason))))))
        (error
         (qq-chat--restore-failed-send
          buffer session-key restore-owner draft-state aux-state t)
         (signal (car err) (cdr err)))))))

(defun qq-chat-return-dwim (arg)
  "Complete an unresolved token, send draft, or insert newline with ARG.

An unresolved @member, /face, /favorite, or :emoji: token always owns RET.
This prevents a completion frontend with no preselected row from falling
through and sending the query as plain text.  `C-c RET' remains the explicit
way to send literal token text."
  (interactive "P")
  (qq-chat--ensure-composer-visible)
  (if (not (appkit-chatbuf-point-in-input-p))
      (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max)))
    (cond
     (arg
      (insert "\n"))
     ((qq-completion-token-at-point)
      (or (qq-completion-complete)
          (message "qq: no completion candidate; C-c RET sends literally")))
     (t
      (qq-chat-send-message)))))

(defun qq-chat-edit-draft ()
  "Move point to the editable draft area."
  (interactive)
  (qq-chat--ensure-composer-visible)
  (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))

(defun qq-chat-complete ()
  "Complete at composer point, or move to the composer from the timeline."
  (interactive)
  (if (appkit-chatbuf-point-in-input-p)
      (or (qq-completion-complete)
          (message "qq: no completion at point"))
    (qq-chat-edit-draft)))

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
         (emoji-id (format "%s" (or face-id
                                    (qq-chat--read-base-face-id
                                     "React with QQ face: ")))))
    (qq-api-set-message-emoji-like
     (qq-chat--message-reference message) emoji-id t
     (lambda (_response)
       (message "qq: reaction added (%s)" emoji-id)))))

(defun qq-chat-open-resource-at-point ()
  "Open the exact media card at point, or the message's primary media."
  (interactive)
  (appkit-media-card-open))

(defun qq-chat-open-avatar-at-point ()
  "Open sender avatar for the message at point."
  (interactive)
  (if-let* ((user-id (get-text-property
                      (point) 'qq-chat-gray-tip-user-id)))
      (qq-media-open-user-avatar user-id)
    (qq-media-open-message-avatar
     (or (qq-chat--message-at-point)
         (user-error "qq: no message at point")))))

(defun qq-chat-open-user-at-point ()
  "Open the sender user page for the message at point."
  (interactive)
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (gray-tip-user-id (get-text-property
                            (point) 'qq-chat-gray-tip-user-id)))
    (if gray-tip-user-id
        (progn
          (unless (and (qq-api-user-id-p gray-tip-user-id)
                       (not (equal gray-tip-user-id "0")))
            (user-error "qq: service message has no user profile"))
          (qq-user-open gray-tip-user-id))
      (qq-chat--open-message-sender-profile message))))

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
  "Return to authoritative latest history and mark it read.

This is the QQ counterpart of `telega-chatbuf-read-all': when an around
window is partial, fetch the real latest page before submitting its exact
opaque message id."
  (interactive)
  (if (eq (qq-state-session-key-type qq-chat--session-key) 'guild-channel)
      (qq-api-mark-guild-read
       qq-chat--session-key
       (lambda (_navigation) (message "qq: channel marked read")))
    (qq-chat-return-to-latest nil t)))

(defun qq-chat--poke-session (session-key)
  "Return poke-capable SESSION-KEY metadata, or signal `user-error'."
  (unless session-key
    (user-error "qq: this buffer is not bound to a session"))
  (let ((session (qq-state-session session-key)))
    (unless (memq (alist-get 'type session) '(private group))
      (user-error "qq: this conversation does not support pokes"))
    session))

(defun qq-chat--poke-target-id-p (target-id)
  "Return non-nil when TARGET-ID identifies a real QQ user."
  (and (qq-api-user-id-p target-id)
       (not (equal target-id "0"))))

(defun qq-chat--validate-poke-target (session target-id)
  "Return TARGET-ID when it is valid for poke-capable SESSION.

A private conversation can only target its peer or the current user.  Group
targets are established by the message-sender or native member-selection
paths before reaching this validator."
  (unless (qq-chat--poke-target-id-p target-id)
    (user-error "qq: poke target is not a valid QQ user"))
  (when (eq (alist-get 'type session) 'private)
    (let ((peer-id (alist-get 'target-id session))
          (self-id (qq-state-self-user-id)))
      (unless (or (equal target-id peer-id)
                  (and (qq-chat--poke-target-id-p self-id)
                       (equal target-id self-id)))
        (user-error "qq: private poke target must be the peer or self"))))
  target-id)

(defun qq-chat--send-poke-to (session-key target-id)
  "Poke TARGET-ID in SESSION-KEY using the strict peer/target contract."
  (qq-chat--validate-poke-target
   (qq-chat--poke-session session-key) target-id)
  (qq-api-send-poke
   session-key target-id
   (lambda (_response)
     (message "qq: poke sent"))))

(defun qq-chat--poke-sender-at-point ()
  "Return the QQ user ID of the message sender at point.

Only `sender-id' has this meaning.  In particular, a message's `target-id'
denotes its conversation peer (or a poke decoration target), so it must not
be used to infer the sender."
  (let* ((message (or (qq-chat--message-at-point)
                      (user-error "qq: no message at point")))
         (sender-id (alist-get 'sender-id message)))
    (unless (qq-chat--poke-target-id-p sender-id)
      (user-error "qq: message sender cannot be poked"))
    sender-id))

(defun qq-chat-poke-sender ()
  "Poke the sender of the message at point.

This works for both another user and the current user's own messages."
  (interactive)
  (qq-chat--send-poke-to
   qq-chat--session-key
   (qq-chat--poke-sender-at-point)))

(defun qq-chat-send-poke (&optional target-id)
  "Choose and poke TARGET-ID in the current chat.

Interactive private chats offer the peer and the current user.  Interactive
group chats search the native member directory and then require an explicit
member choice.  A non-nil programmatic TARGET-ID bypasses the chooser but is
still validated by the strict API contract."
  (interactive)
  (let* ((session-key qq-chat--session-key)
         (session (qq-chat--poke-session session-key)))
    (if target-id
        (qq-chat--send-poke-to session-key target-id)
      (let* ((buffer (current-buffer))
             (message (ignore-errors (qq-chat--message-at-point)))
             (sender-id (alist-get 'sender-id message))
             (initial-user-id
              (and (eq (alist-get 'type session) 'group)
                   (qq-chat--poke-target-id-p sender-id)
                   sender-id)))
        (qq-completion-read-poke-target
         session-key
         (lambda (selected-user-id)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (equal qq-chat--session-key session-key)
                 (qq-chat--send-poke-to session-key selected-user-id)))))
         initial-user-id)))))

(defvar qq-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") #'recenter-top-bottom)
    (define-key map (kbd "C-c g") #'qq-chat-refresh)
    ;; Follow native telega's filtering/search key family.  `C-c /' chooses a
    ;; materialized filter; C-r/C-s and M-g s/n/p remain in-place navigation.
    ;; The standalone paginated result page is an extension on `C-c M-/'.
    (define-key map (kbd "C-c /") #'qq-chat-filter)
    (define-key map (kbd "C-c M-/") #'qq-chat-search-results)
    (define-key map (kbd "C-c C-r") #'qq-chat-search)
    (define-key map (kbd "C-c C-s") #'qq-chat-search-forward)
    (define-key map (kbd "C-c C-c") #'qq-chat-filter-cancel)
    (define-key map (kbd "C-c C-n") #'qq-chat-search-next)
    (define-key map (kbd "C-c C-p") #'qq-chat-search-prev)
    (define-key map (kbd "C-c r") #'qq-chat-read-all)
    (define-key map (kbd "C-c P") #'qq-chat-send-poke)
    (define-key map (kbd "C-c m") #'qq-chat-message-transient)
    (define-key map (kbd "C-c f") #'qq-chat-forward-transient)
    (define-key map (kbd "C-c i") #'qq-chat-open-peer-info)
    ;; Keep M-</M-> as native Emacs beginning/end-of-buffer commands.
    ;; Telega reserves its authoritative latest/read-all action for M-g.
    (define-key map (kbd "RET") #'qq-chat-return-dwim)
    (define-key map (kbd "DEL") #'appkit-chatbuf-input-backward-delete)
    (define-key map (kbd "<backspace>") #'appkit-chatbuf-input-backward-delete)
    (define-key map (kbd "C-d") #'appkit-chatbuf-input-forward-delete)
    (define-key map (kbd "<delete>") #'appkit-chatbuf-input-forward-delete)
    (define-key map (kbd "TAB") #'qq-chat-complete)
    (define-key map (kbd "<tab>") #'qq-chat-complete)
    (define-key map (kbd "C-M-i") #'qq-chat-complete)
    (define-key map (kbd "C-c '") #'qq-chat-edit-draft)
    (define-key map (kbd "M-p") #'qq-chat-draft-prev)
    (define-key map (kbd "M-n") #'qq-chat-draft-next)
    (define-key map (kbd "C-c C-f") #'qq-chat-attach-file)
    (define-key map (kbd "C-c C-v") #'qq-chat-attach-clipboard)
    (define-key map (kbd "C-c C-e") #'qq-chat-attach-emoji)
    (define-key map (kbd "C-c C-a") #'qq-chat-attach-transient)
    (define-key map (kbd "M-g s") #'qq-chat-inplace-search)
    (define-key map (kbd "M-g n") #'qq-chat-search-next)
    (define-key map (kbd "M-g p") #'qq-chat-search-prev)
    (define-key map (kbd "M-g >") #'qq-chat-read-all)
    (define-key map (kbd "M-g r") #'qq-chat-read-all)
    (define-key map (kbd "M-g x") #'qq-chat-goto-pop-message)
    (define-key map (kbd "C-c RET") #'qq-chat-send-message)
    (define-key map (kbd "C-c C-k") #'qq-chat-cancel-dwim)
    ;; telega: ESC ESC / C-M-c also cancel reply-or-edit aux.
    (define-key map (kbd "\e\e") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-M-c") #'qq-chat-cancel-dwim)
    (define-key map (kbd "C-c ?") #'qq-chat-transient)
    map)
  "Keymap for `qq-chat-mode'.")

(define-derived-mode qq-chat-mode nil "QQ-Chat"
  "Major mode for emacs-qq chat buffers.

Message actions use point + keys (`r'/`d'/`!'/`P'/`o'/`a' on the timeline) or
`qq-chat-message-transient' (`C-c m' / timeline `m').  Chat-wide commands
are in `qq-chat-transient' (`C-c ?' / timeline `?').
Attach from clipboard with `C-c C-v' (telega-style)."
  (appkit-chatbuf-mode-setup)
  ;; Keep vertically sliced two-line avatars visually contiguous.
  (setq-local line-spacing 0)
  (appkit-chatbuf-reset-state 32)
  (qq-completion-setup)
  (setq-local qq-chat--last-search-query nil)
  (setq-local qq-chat--search-results nil)
  (setq-local qq-chat--search-results-tail nil)
  (setq-local qq-chat--search-seen (make-hash-table :test #'equal))
  (setq-local qq-chat--search-consumed-cursors
              (make-hash-table :test #'equal))
  (setq-local qq-chat--search-index nil)
  (setq-local qq-chat--search-direction 'older)
  (setq-local qq-chat--search-anchor nil)
  (setq-local qq-chat--search-completed-p nil)
  (setq-local qq-chat--search-next-cursor nil)
  (setq-local qq-chat--search-request nil)
  (setq-local qq-chat--search-owner nil)
  (setq-local qq-chat--search-highlight-overlays nil)
  (setq-local qq-chat--msg-filter nil)
  (setq-local qq-chat--filter-request nil)
  (setq-local qq-chat--filter-owner nil)
  (setq-local qq-chat--filter-sync-request nil)
  (setq-local qq-chat--filter-auto-load-p nil)
  (setq-local qq-chat--callback-sync-request nil)
  (setq-local qq-chat--send-sync-request nil)
  (setq-local qq-chat--message-selection nil)
  (setq-local qq-chat--forward-request nil)
  (setq-local qq-chat--forward-request-owner nil)
  (setq-local qq-chat--forward-sync-request nil)
  (setq-local qq-chat--forward-plan-owner (list 'forward-plan-owner))
  (setq-local qq-chat--last-read-target-id nil)
  (setq-local qq-chat--guild-read-request-p nil)
  (setq-local qq-chat--fill-column nil)
  (setq-local appkit-media-card-fallback-context-function
              #'qq-chat--media-card-fallback-context)
  (qq-chat--reset-history-state)
  (setq-local qq-chat--pending-jump-id nil)
  (setq-local qq-chat--open-message-owner nil)
  (setq-local qq-chat--open-message-request nil)
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
  (add-hook 'window-scroll-functions #'qq-chat--window-scroll nil t)
  (add-hook 'window-size-change-functions
            #'qq-chat--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook
            #'qq-chat--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'qq-chat--on-text-scale-change nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-search-request nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-filter-request nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-open-message-request nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-initial-history-request nil t)
  (add-hook 'kill-buffer-hook #'qq-chat--cancel-forward-request nil t)
  (qq-chat--update-context-mode))

(defun qq-chat--initial-history-request-current-p
    (buffer session-key owner)
  "Return non-nil when OWNER still owns BUFFER and SESSION-KEY."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--initial-history-owner owner)
              (qq-chat--captured-view-current-p
               (plist-get owner :view))
              (appkit-chat-history-request-current-p owner)))))

(defun qq-chat--cancel-initial-history-request ()
  "Cancel and forget this chat buffer's initial-history chain."
  (let ((request qq-chat--initial-history-request)
        (owner qq-chat--initial-history-owner))
    (when (appkit-chat-history-request-current-p owner)
      (appkit-chat-history-request-end owner))
    (setq qq-chat--initial-history-request nil
          qq-chat--initial-history-owner nil)
    (when request
      (qq-api-cancel-request request))))

(defun qq-chat--cancel-open-message-request ()
  "Cancel an around-fetch owned by `qq-chat-open-message'."
  (let ((request qq-chat--open-message-request)
        (owner qq-chat--open-message-owner))
    (when (appkit-chat-history-request-current-p owner)
      (appkit-chat-history-request-end owner))
    (setq qq-chat--open-message-request nil
          qq-chat--open-message-owner nil
          qq-chat--pending-jump-id nil)
    (when request
      (qq-api-cancel-request request))))

(defun qq-chat--open-message-request-current-p
    (buffer session-key owner)
  "Return non-nil when OWNER owns BUFFER's exact open-message request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (eq qq-chat--open-message-owner owner)
              (qq-chat--captured-view-current-p
               (plist-get owner :view))))))

(defun qq-chat--complete-initial-history-load
    (buffer session-key owner &optional target meta)
  "Finish initial history load for BUFFER and SESSION-KEY.

When TARGET is non-nil, META describes an around window centered at the exact
first unread message."
  (when (qq-chat--initial-history-request-current-p
         buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (appkit-chat-history-request-end owner)
        (setq qq-chat--initial-history-owner nil
              qq-chat--initial-history-request nil)
        (when (and target meta)
          (qq-chat--note-history-window
           meta (plist-get owner :remote-latest-id)))
        (qq-chat--request-callback-sync
         view
         (and target
              (lambda ()
                (qq-chat--goto-loaded-message target nil))))))))

(defun qq-chat--fail-initial-history-load
    (buffer session-key owner response reason)
  "Finish OWNER with an explicit initial-history failure."
  (when (qq-chat--initial-history-request-current-p
         buffer session-key owner)
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (appkit-chat-history-request-end owner)
        (setq qq-chat--initial-history-owner nil
              qq-chat--initial-history-request nil)
        (qq-chat--request-callback-sync view)))
    (qq-api--default-error response reason)))

(defun qq-chat--continue-initial-history-from-session
    (buffer session-key owner)
  "Continue initial-history OWNER from SESSION-KEY's accepted read state."
  (when (qq-chat--initial-history-request-current-p
         buffer session-key owner)
    (let* ((session (qq-state-session session-key))
           (unread (or (alist-get 'unread-count session) 0))
           (first-id (alist-get 'first-unread-message-id session))
           (session-latest (alist-get 'read-latest-message-id session))
           (frontier-at-start (plist-get owner :frontier-at-start))
           (observed-frontier
            (buffer-local-value 'qq-chat--remote-latest-id buffer))
           ;; A different buffer frontier means a live message arrived after
           ;; the HTTP read-state request started.  Preserve that observation;
           ;; otherwise the accepted kernel snapshot may advance stale cache.
           (latest (if (equal observed-frontier frontier-at-start)
                       session-latest
                     observed-frontier)))
      (setf (plist-get owner :remote-latest-id) latest)
      (if (and (> unread 0) first-id)
          (qq-chat--load-unread-initial-history
           buffer session-key owner first-id latest)
        (qq-chat--load-latest-initial-history buffer session-key owner)))))

(defun qq-chat--load-latest-initial-history (buffer session-key owner)
  "Load latest history for BUFFER, then complete read handling."
  (with-current-buffer buffer
    (setq qq-chat--remote-latest-id
          (plist-get owner :remote-latest-id)))
  (let ((request
         (qq-api-fetch-older-history
          session-key nil
          (lambda (meta)
            (when (qq-chat--initial-history-request-current-p
                   buffer session-key owner)
              (with-current-buffer buffer
                (pcase-let* ((`(,oldest . ,newest)
                               (qq-chat--history-batch-bounds meta))
                              (frontier-at-start
                               (plist-get owner :remote-latest-id))
                              (observed-frontier qq-chat--remote-latest-id)
                              (latest
                               (if (equal observed-frontier frontier-at-start)
                                   newest
                                 observed-frontier)))
                  (cond
                   (newest
                    (setq qq-chat--remote-latest-id latest)
                    (appkit-chat-history-older-loaded-set nil)
                    (qq-chat--set-history-window oldest nil))
                   ((qq-chat--adopt-live-frontier-window
                     frontier-at-start observed-frontier))
                   ;; An empty nil-cursor response authoritatively establishes
                   ;; an empty latest window even when canonical state retains
                   ;; unrelated history islands.  The first live create will
                   ;; seed its exact first edge.
                   (t
                    (setq qq-chat--remote-latest-id nil)
                    (qq-chat--set-empty-history-window))))))
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

(defun qq-chat--load-unread-initial-history
    (buffer session-key owner first-id remote-latest-id)
  "Load SESSION-KEY around FIRST-ID for initial-history OWNER.

The around-fetch replaces the completed read-state request as the single
cancelable request owned by BUFFER.  Synchronous callbacks may complete the
owner before the API function returns, in which case its stale return token is
never installed."
  (with-current-buffer buffer
    (qq-chat--begin-around-history-window remote-latest-id owner))
  (let ((request
         (qq-api-fetch-history-around
          session-key first-id
          (lambda (meta)
            (qq-chat--complete-initial-history-load
             buffer session-key owner first-id meta))
          (lambda (response reason)
            (qq-chat--fail-initial-history-load
             buffer session-key owner response reason))
          (max qq-history-fetch-count (* 2 qq-history-fetch-count)))))
    (when (qq-chat--initial-history-request-current-p
           buffer session-key owner)
      (with-current-buffer buffer
        (setq qq-chat--initial-history-request request)))
    request))

(defun qq-chat--load-initial-ordinary-history (buffer session-key)
  "Load SESSION-KEY around its official QQ read position when available."
  (let ((owner (list :kind 'initial-history
                     :session-key session-key
                     :view (with-current-buffer buffer
                             (qq-chat--ensure-view))
                     :remote-latest-id nil
                     :frontier-at-start nil
                     :read-observation-token
                     (qq-api-read-observation-start))))
    (with-current-buffer buffer
      (when qq-chat--initial-history-request
        (qq-api-cancel-request qq-chat--initial-history-request))
      (appkit-chat-history-window-clear)
      (setf (plist-get owner :frontier-at-start)
            qq-chat--remote-latest-id)
      (setq qq-chat--initial-history-owner owner
            qq-chat--initial-history-request nil)
      (appkit-chat-history-request-begin 'initial owner)
      (qq-chat--sync-timeline :messages nil)
      (qq-chat--update-frame))
    (let ((read-pending t)
          request)
      (setq request
            (qq-api-fetch-session-read-state
             session-key
             (lambda (read-state)
               (setq read-pending nil)
               (when (qq-chat--initial-history-request-current-p
                      buffer session-key owner)
                 (when (qq-api-read-observation-accept-p
                        session-key
                        (plist-get owner :read-observation-token))
                   (qq-state-apply-session-read-state session-key read-state))
                 ;; When the HTTP token lost, a newer notice/recent-contact
                 ;; observation has already populated the canonical session.
                 (qq-chat--continue-initial-history-from-session
                  buffer session-key owner)))
             (lambda (response reason)
               (setq read-pending nil)
               (qq-chat--fail-initial-history-load
                buffer session-key owner response reason))))
      ;; Only the still-pending read-state call owns this token.  A
      ;; synchronous callback may already have installed its child token.
      (when (and read-pending
                 (qq-chat--initial-history-request-current-p
                  buffer session-key owner))
        (with-current-buffer buffer
          (setq qq-chat--initial-history-request request)))
      request)))

(defun qq-chat--guild-sequence-offset (sequence delta)
  "Return decimal SEQUENCE shifted by integer DELTA, saturating at zero.

Guild message sequences are counters, not snowflake message identities.
Emacs integer arithmetic is arbitrary precision, so this never rounds them."
  (unless (and (qq-protocol--decimal-string-p sequence) (integerp delta))
    (error "qq: invalid Guild sequence arithmetic operands"))
  (number-to-string (max 0 (+ (string-to-number sequence) delta))))

(defun qq-chat--guild-page-start (end-sequence)
  "Return the inclusive page start ending at END-SEQUENCE."
  (qq-chat--guild-sequence-offset
   end-sequence (- 1 (max 1 qq-history-fetch-count))))

(defun qq-chat--guild-record-ids (records)
  "Return exact message ids from validated Guild RECORDS."
  (mapcar (lambda (record) (alist-get 'message_id record)) records))

(defun qq-chat--complete-initial-guild-navigation
    (buffer session-key owner start-sequence end-sequence records)
  "Finish initial Guild OWNER after loading RECORDS for an exact range."
  (when (qq-chat--initial-history-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (appkit-chat-history-request-end owner)
      (setq qq-chat--initial-history-owner nil
            qq-chat--initial-history-request nil)
      (let* ((ids (qq-chat--guild-record-ids records))
             (oldest (car ids))
             (latest (car (last ids))))
        (setq qq-chat--remote-latest-id latest
              qq-chat--guild-history-start-sequence start-sequence
              qq-chat--guild-history-end-sequence end-sequence)
        (appkit-chat-history-older-loaded-set
         (equal start-sequence "0"))
        (if oldest
            (qq-chat--set-history-window oldest nil)
          (qq-chat--set-empty-history-window))
        (qq-chat--request-callback-sync
         (plist-get owner :view)
         (lambda ()
           (goto-char
            (or (appkit-chatbuf-input-start-position) (point-max)))))))))

(defun qq-chat--load-initial-guild-range
    (buffer session-key owner end-sequence)
  "Load the latest Guild page ending at END-SEQUENCE for OWNER."
  (let ((start-sequence (qq-chat--guild-page-start end-sequence)))
    (if (equal end-sequence "0")
        (qq-chat--complete-initial-guild-navigation
         buffer session-key owner "0" "0" nil)
      (let ((pending t)
            request)
        (setq request
              (qq-api-fetch-guild-message-range
               session-key start-sequence end-sequence
               (lambda (records)
                 (setq pending nil)
                 (qq-chat--complete-initial-guild-navigation
                  buffer session-key owner start-sequence end-sequence records))
               (lambda (response reason)
                 (setq pending nil)
                 (qq-chat--fail-initial-history-load
                  buffer session-key owner response reason))))
        (when (and pending
                   (qq-chat--initial-history-request-current-p
                    buffer session-key owner))
          (with-current-buffer buffer
            (setq qq-chat--initial-history-request request)))
        request))))

(defun qq-chat--load-initial-guild-navigation (buffer session-key)
  "Load authoritative Guild navigation before showing live channel messages."
  (let ((owner (list :kind 'initial-guild-navigation
                     :session-key session-key
                     :view (with-current-buffer buffer
                             (qq-chat--ensure-view)))))
    (with-current-buffer buffer
      (when qq-chat--initial-history-request
        (qq-api-cancel-request qq-chat--initial-history-request))
      (appkit-chat-history-window-clear)
      (setq qq-chat--initial-history-owner owner
            qq-chat--initial-history-request nil)
      (appkit-chat-history-request-begin 'initial owner)
      (qq-chat--sync-timeline :messages nil)
      (qq-chat--update-frame))
    (let ((pending t)
          request)
      (setq request
            (qq-api-fetch-guild-navigation
             session-key
             (lambda (_navigation)
               (setq pending nil)
               (let* ((identity (qq-state-session-key-identity session-key))
                      (channel
                       (qq-state-guild-channel
                        (alist-get 'guild-id identity)
                        (alist-get 'channel-id identity)))
                      (latest (alist-get 'latest_sequence channel)))
                 (unless (qq-protocol--decimal-string-p latest)
                   (error "qq: Guild channel lacks latest_sequence"))
                 (qq-chat--load-initial-guild-range
                  buffer session-key owner latest)))
             (lambda (response reason)
               (setq pending nil)
               (qq-chat--fail-initial-history-load
                buffer session-key owner response reason))))
      (when (and pending
                 (qq-chat--initial-history-request-current-p
                  buffer session-key owner))
        (with-current-buffer buffer
          (setq qq-chat--initial-history-request request)))
      request)))

(defun qq-chat--complete-initial-guild-forum
    (buffer session-key owner page)
  "Finish initial Guild forum OWNER with authoritative PAGE."
  (when (qq-chat--initial-history-request-current-p buffer session-key owner)
    (with-current-buffer buffer
      (appkit-chat-history-request-end owner)
      (setq qq-chat--initial-history-owner nil
            qq-chat--initial-history-request nil
            qq-chat--guild-forum-next-cursor
            (alist-get 'next_cursor page)
            qq-chat--guild-history-start-sequence nil
            qq-chat--guild-history-end-sequence nil
            qq-chat--remote-latest-id nil)
      (appkit-chat-history-older-loaded-set
       (eq (alist-get 'finished page) t))
      (pcase-let ((`(,oldest . ,_newest)
                   (qq-chat--forum-history-anchors session-key)))
        (if oldest
            (qq-chat--set-history-window oldest nil)
          (qq-chat--set-empty-history-window)))
      (qq-chat--request-callback-sync
       (plist-get owner :view)
       (lambda ()
         (goto-char
          (or (appkit-chatbuf-input-start-position) (point-max))))))))

(defun qq-chat--load-initial-guild-forum (buffer session-key)
  "Load the newest native Feed page for forum SESSION-KEY."
  (let ((owner (list :kind 'initial-guild-forum
                     :session-key session-key
                     :view (with-current-buffer buffer
                             (qq-chat--ensure-view)))))
    (with-current-buffer buffer
      (when qq-chat--initial-history-request
        (qq-api-cancel-request qq-chat--initial-history-request))
      (appkit-chat-history-window-clear)
      (setq qq-chat--initial-history-owner owner
            qq-chat--initial-history-request nil
            qq-chat--guild-forum-next-cursor nil)
      (appkit-chat-history-request-begin 'initial owner)
      (qq-chat--sync-timeline :messages nil)
      (qq-chat--update-frame))
    (let ((pending t)
          request)
      (setq request
            (qq-api-fetch-guild-forum-page
             session-key ""
             (lambda (page)
               (setq pending nil)
               (qq-chat--complete-initial-guild-forum
                buffer session-key owner page))
             (lambda (response reason)
               (setq pending nil)
               (qq-chat--fail-initial-history-load
                buffer session-key owner response reason))))
      (when (and pending
                 (qq-chat--initial-history-request-current-p
                  buffer session-key owner))
        (with-current-buffer buffer
          (setq qq-chat--initial-history-request request)))
      request)))

(defun qq-chat--load-initial-history (buffer session-key)
  "Load the protocol-specific initial position for SESSION-KEY."
  (cond
   ((qq-chat--guild-forum-session-p session-key)
    (qq-chat--load-initial-guild-forum buffer session-key))
   ((eq (qq-state-session-key-type session-key) 'guild-channel)
    (qq-chat--load-initial-guild-navigation buffer session-key))
   (t
    (qq-chat--load-initial-ordinary-history buffer session-key))))

(defun qq-chat--open-buffer (session-key)
  "Create or reuse SESSION-KEY's chat view without loading history."
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
           :parts '(frame timeline composer geometry)))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (let ((fresh-p (null qq-chat--session-key)))
        (setq qq-chat--session-key session-key)
        (qq-completion-preload-members)
        (if fresh-p
            (progn
              ;; A fresh buffer has no proven contiguous window yet.  Do not
              ;; render every cache island while the initial/around request is
              ;; still choosing its exact slice.
              (appkit-chat-history-window-clear)
              (qq-chat--ensure-view)
              (qq-chat--header-line-update)
              (qq-chat--sync-timeline :messages nil)
              (qq-chat--update-frame))
          (qq-chat-render)))
      (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))
    buffer))

(defun qq-chat-open (session-key)
  "Open chat for SESSION-KEY and load its official initial position."
  (interactive)
  (let ((buffer (qq-chat--open-buffer session-key)))
    (with-current-buffer buffer
      (qq-chat--cancel-open-message-request)
      (qq-chat--load-initial-history buffer session-key))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (qq-chat--on-window-size-change))))

(defun qq-chat--finish-open-message (target query)
  "Jump to loaded TARGET and highlight its actual text matching QUERY."
  (setq qq-chat--pending-jump-id nil)
  (if (qq-chat--goto-loaded-message target nil)
      (progn
        (when (and (stringp query) (not (string-empty-p query)))
          (qq-chat--highlight-search-text target query))
        t)
    nil))

(defun qq-chat--open-message-succeeded
    (buffer session-key owner target query meta)
  "Finish TARGET around-fetch with META when OWNER remains current."
  (when (and (qq-chat--open-message-request-current-p buffer session-key owner)
             (with-current-buffer buffer
               (appkit-chat-history-request-current-p owner)))
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setf (plist-get owner :pending) nil)
        (setq qq-chat--open-message-request nil
              qq-chat--open-message-owner nil)
        (appkit-chat-history-request-end owner)
        (qq-chat--note-history-window meta)
        (qq-chat--request-callback-sync
         view
         (lambda ()
           (unless (qq-chat--finish-open-message target query)
             (qq-chat--jump-fail
              target "around window omitted target"))))))))

(defun qq-chat--open-message-failed
    (buffer session-key owner target _response reason)
  "Report TARGET around-fetch failure REASON for the current OWNER."
  (when (and (qq-chat--open-message-request-current-p buffer session-key owner)
             (with-current-buffer buffer
               (appkit-chat-history-request-current-p owner)))
    (with-current-buffer buffer
      (let ((view (plist-get owner :view)))
        (setf (plist-get owner :pending) nil)
        (setq qq-chat--open-message-request nil
              qq-chat--open-message-owner nil)
        (appkit-chat-history-request-end owner)
        (qq-chat--request-callback-sync
         view (lambda () (qq-chat--jump-fail target reason)))))))

(defun qq-chat-open-message (session-key message-id &optional query)
  "Open SESSION-KEY at exact MESSAGE-ID, optionally highlighting QUERY.

Unlike `qq-chat-open', this entry point never starts an initial-history load.
It cancels an existing initial load before using one owned around-fetch, so a
search-result jump cannot race a latest/read-position request."
  (setq message-id
        (qq-api-validate-message-id message-id "open searched message"))
  (let ((buffer (qq-chat--open-buffer session-key)))
    (with-current-buffer buffer
      (qq-chat--cancel-initial-history-request)
      (qq-chat--cancel-open-message-request)
      (qq-chat--cancel-search-request)
      (qq-chat--clear-search-highlights)
      (when (qq-chat--msg-filter-active-p)
        (qq-chat--deactivate-filter)
        (qq-chat-render))
      (setq qq-chat--pending-jump-id message-id))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (qq-chat--on-window-size-change)
      (unless (and (qq-chat--history-window-known-p)
                   (qq-chat--finish-open-message message-id query))
        (let* ((view (qq-chat--live-current-view))
               (owner (list :session-key session-key
                            :view view
                            :pending t))
               request)
          (qq-chat--begin-around-history-window nil owner)
          (setq qq-chat--open-message-owner owner)
          (setq request
                (qq-api-fetch-history-around
                 session-key message-id
                 (lambda (meta)
                   (qq-chat--open-message-succeeded
                    buffer session-key owner message-id query meta))
                 (lambda (response reason)
                   (qq-chat--open-message-failed
                    buffer session-key owner message-id response reason))
                 (qq-chat--jump-history-count)))
          (when (and (qq-chat--open-message-request-current-p
                      buffer session-key owner)
                     (plist-get owner :pending))
            (setq qq-chat--open-message-request request)))))
    buffer))

(defun qq-chat--rerender-open-chats (&optional media-key)
  "Invalidate open chat rows affected by MEDIA-KEY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when-let* ((view (qq-chat--live-current-view)))
        (if media-key
            (appkit-request-sync view :resource (list :media media-key))
          (when (appkit-chat-timeline-live-p)
            (appkit-request-sync
             view :entries (appkit-chat-timeline-keys))))))))

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

(defun qq-chat--message-event-advances-frontier-p (event)
  "Return non-nil when EVENT introduces a canonical server message.

Ordinary updates such as recall and emoji-like notices must never move the
remote frontier.  A create is a new remote row once it has a server id; an
update only qualifies when it explicitly promotes the message's own pending
local anchor to that server id."
  (let* ((message (plist-get event :message))
         (mutation (plist-get event :mutation))
         (source (plist-get event :source))
         (event-anchor (plist-get event :message-anchor))
         (previous-anchor (plist-get event :previous-anchor))
         (local-id (and (listp message) (alist-get 'local-id message)))
         (server-id (and (listp message) (alist-get 'server-id message))))
    (and (eq (plist-get event :type) 'message)
         (equal (plist-get event :session-key) qq-chat--session-key)
         (qq-api-message-id-p server-id)
         (equal event-anchor server-id)
         (or (and (eq mutation 'create)
                  (memq source '(event notice)))
             (and (eq mutation 'update)
                  (memq source '(event notice response))
                  previous-anchor
                  local-id
                  (equal previous-anchor local-id)
                  (not (equal previous-anchor server-id)))))))

(defun qq-chat--observe-message-frontier (event)
  "Advance this buffer's remote frontier from canonical message EVENT.

This is deliberately state-only and idempotent.  The state-change hook calls
it before scheduling AppKit invalidations, so an HTTP history callback in the
same event-loop turn cannot overwrite a websocket message merely because
redisplay has not processed the queued event yet."
  (let* ((message (plist-get event :message))
         (anchor (or (plist-get event :message-anchor)
                     (and (listp message)
                          (qq-chat--message-anchor message)))))
    (when (and (qq-chat--message-event-advances-frontier-p event)
               (qq-api-message-id-p anchor)
               (equal anchor
                      (alist-get 'server-id
                                 (qq-chat--latest-server-message))))
      (appkit-chat-history-window-seed-live anchor)
      (unless (equal anchor qq-chat--remote-latest-id)
        (appkit-chat-history-newer-stalled-clear))
      (setq qq-chat--remote-latest-id anchor))))

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
    ;; Idempotent with the eager observation in `qq-chat--handle-state-change'.
    (qq-chat--observe-message-frontier event)
    (qq-chat--rekey-message-selection previous-anchor anchor)
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
    (when (eq event-type 'reset)
      ;; The remembered destination belongs to the previous runtime/account,
      ;; not to any particular chat buffer.
      (setq qq-chat--last-forward-target-key nil
            qq-chat-forward-target-history nil))
    (when (memq event-type '(message history reset session action connection
                             sessions-refreshed friends-refreshed groups-refreshed))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'qq-chat-mode)
            ;; Reset is a hard ownership boundary even after runtime shutdown
            ;; has made the AppKit view non-live.  Rotate/clear opaque owners
            ;; before any best-effort cancellation can run callbacks.
            (when (eq event-type 'reset)
              (setq qq-chat--forward-plan-owner (list 'forward-plan-owner)
                    qq-chat--message-selection nil)
              (qq-chat--cancel-forward-request)
              (qq-chat--deactivate-filter t)
              (qq-chat--reset-search-state t))
            ;; Only UI projection requires a live view.  Ownership cleanup
            ;; above deliberately does not.
            (when (or (memq event-type
                            '(reset connection sessions-refreshed
                              friends-refreshed groups-refreshed))
                      (equal event-session-key qq-chat--session-key))
              (when-let* ((view (qq-chat--live-current-view)))
                (when (eq event-type 'message)
                  (qq-chat--observe-message-frontier event)
                  ;; Filter snapshots are private request state.  Observe their
                  ;; exact patch before queueing redisplay so a synchronous or
                  ;; already-ready filter callback cannot merge an older
                  ;; `:existing' snapshot over this notice.
                  (when-let* ((patch (plist-get event :message-patch)))
                    (qq-chat--apply-filter-message-patch
                     (plist-get event :message-anchor)
                     patch
                     (plist-get event :observation-token))))
                (appkit-view-enqueue-event view event)
                (appkit-request-sync
                 view :part (if (memq event-type
                                      '(session action connection
                                        sessions-refreshed))
                                'frame
                              'timeline))))))))))

(add-hook 'qq-media-cache-update-hook #'qq-chat--rerender-open-chats)
(add-hook 'qq-state-change-hook #'qq-chat--handle-state-change)

(provide 'qq-chat)

;;; qq-chat.el ends here
