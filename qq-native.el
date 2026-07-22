;;; qq-native.el --- Native emacs-qq product boundary -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Product-facing QQ operations live here.  As in telega's TDLib operation
;; layer, callers do not select an implementation: the WebSocket connection
;; and wire protocol stay behind the `qq-gateway-*' adapter modules.  A local
;; disconnect only detaches Emacs; account stop and logout remain explicit
;; lifecycle commands.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-gateway-directory)
(require 'qq-gateway-message)
(require 'qq-gateway-transport)
(require 'qq-protocol)
(require 'qq-state)

(cl-defstruct (qq-native-request
               (:constructor qq-native-request-create))
  "Opaque cancellable native request."
  token)

(defvar qq-native--bootstrap-owner nil
  "Account owner whose initial directory refresh is running or complete.")

(defvar qq-native--bootstrap-pending 0
  "Number of directory parts pending for the current account bootstrap.")

(defun qq-native--wrap-request (token)
  "Wrap opaque native TOKEN for safe cancellation."
  (and token
       (qq-native-request-create :token token)))

(defun qq-native-cancel-request (request)
  "Cancel local callback ownership for native REQUEST."
  (when (qq-native-request-p request)
    (qq-gateway-transport-cancel (qq-native-request-token request))))

(defun qq-native-running-p ()
  "Return non-nil when the native service transport is active."
  (qq-gateway-transport-running-p))

(defun qq-native-ready-p ()
  "Return non-nil when the native service accepts business requests."
  (qq-gateway-transport-ready-p))

(defun qq-native-group-id-p (value)
  "Return non-nil when VALUE is an exact native group UIN."
  (qq-gateway--canonical-decimal-p value))

(defun qq-native-user-id-p (value)
  "Return non-nil when VALUE is an exact native user UIN."
  (qq-gateway--canonical-decimal-p value))

(defun qq-native-connect ()
  "Connect to the native service without changing account lifecycle."
  (qq-native-activate)
  (qq-gateway-transport-start))

(defun qq-native-disconnect ()
  "Disconnect Emacs from the native service.

This never stops or logs out a managed QQ account."
  (qq-gateway-transport-stop))

(defun qq-native--default-error (_body reason)
  "Report a native service failure described by REASON."
  (message "qq: %s" (or reason "native request failed")))

(defun qq-native-refresh-friend-categories
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with categories.

ERRBACK receives the service response and reason.  REFRESH forces the
generation-local directory cache."
  (qq-native--wrap-request
   (qq-gateway-directory-refresh-friends
    callback (or errback #'qq-native--default-error) refresh)))

(defun qq-native-refresh-joined-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with them.

ERRBACK receives the service response and reason.  REFRESH forces the
generation-local directory cache."
  (qq-native--wrap-request
   (qq-gateway-directory-refresh-groups
    callback (or errback #'qq-native--default-error) refresh)))

(defun qq-native--group-profile-from-state (group-id)
  "Return a group-profile projection for exact GROUP-ID, or nil."
  (when-let* ((group (qq-state-group group-id)))
    `((group_id . ,group-id)
      (name . ,(alist-get 'group_name group))
      (remark . ,(alist-get 'group_remark group))
      (description . ,(alist-get 'description group))
      (announcement . ,(alist-get 'announcement group))
      (owner_id . ,(alist-get 'owner_id group))
      (member_count . ,(alist-get 'member_count group))
      (max_member_count . ,(alist-get 'max_member_count group))
      (active_member_count . ,(alist-get 'active_member_count group))
      (created_at . ,(alist-get 'created_at group))
      (joined_at . ,(alist-get 'joined_at group))
      (pinned . ,(alist-get 'pinned group))
      (mute . ,(copy-tree (alist-get 'mute group)))
      (join . ,(copy-tree (alist-get 'join group)))
      (self_permission . ,(alist-get 'self_permission group))
      (category . ,(copy-tree (alist-get 'category group)))
      (grade . ,(alist-get 'grade group))
      (certification . ,(copy-tree (alist-get 'certification group)))
      (school . ,(copy-tree (alist-get 'school group)))
      (location . ,(copy-tree (alist-get 'location group)))
      (has_custom_avatar . ,(alist-get 'has_custom_avatar group)))))

(defun qq-native-get-group (group-id callback &optional errback)
  "Fetch native GROUP-ID profile and call CALLBACK.

The projection is derived from its generation-owned joined-group cache; when
absent, one authoritative group refresh is performed first."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: group profile requires an exact group UIN"))
  (if-let* ((profile (qq-native--group-profile-from-state group-id)))
      (progn
        (when callback
          (funcall callback profile))
        nil)
    (qq-native-refresh-joined-groups
     (lambda (_groups)
       (if-let* ((profile
                  (qq-native--group-profile-from-state group-id)))
           (when callback
             (funcall callback profile))
         (funcall (or errback #'qq-native--default-error)
                  nil "group is not present in the selected account")))
     errback t)))

(defun qq-native-refresh ()
  "Refresh primary native directory data."
  (interactive)
  (list (qq-native-refresh-friend-categories)
        (qq-native-refresh-joined-groups)))

(defun qq-native--member-values (member)
  "Return non-empty locally searchable strings from MEMBER."
  (seq-filter
   (lambda (value) (and (stringp value) (not (string-empty-p value))))
   (mapcar (lambda (key) (alist-get key member))
           '(card nickname remark qid user_id))))

(defun qq-native--filter-members (members query limit)
  "Return MEMBERS matching QUERY, truncated to LIMIT."
  (let* ((needle (downcase (string-trim query)))
         (matches
          (if (string-empty-p needle)
              members
            (seq-filter
             (lambda (member)
               (seq-some
                (lambda (value)
                  (string-match-p
                   (regexp-quote needle) (downcase value)))
                (qq-native--member-values member)))
             members))))
    (copy-tree (seq-take matches limit))))

(defun qq-native-search-group-members
    (group-id query callback &optional errback limit)
  "Search native GROUP-ID members for QUERY.

CALLBACK receives at most LIMIT exact member projections.  Native
member pages are complete generation-owned snapshots, so repeated queries use
the local page after its first fetch.  ERRBACK receives the service response
and reason."
  (unless (stringp query)
    (user-error "qq: Group member search query must be a string"))
  (setq limit (or limit 200))
  (unless (and (integerp limit) (<= 1 limit 200))
    (user-error "qq: Group member search limit must be between 1 and 200"))
  (unless (qq-gateway--canonical-decimal-p group-id)
    (user-error "qq: Group member search requires an exact group UIN"))
  (if-let* ((page (qq-gateway-directory-group-member-page group-id)))
      (progn
        (qq-gateway--invoke
         callback
         (qq-native--filter-members
          (alist-get 'members page) query limit))
        nil)
    (qq-native--wrap-request
     (qq-gateway-directory-list-group-members
      group-id
      (lambda (members)
        (qq-gateway--invoke
         callback
         (qq-native--filter-members members query limit)))
      (or errback #'qq-native--default-error)))))

(defun qq-native--apply-group-setting (group-id field value)
  "Apply confirmed group FIELD VALUE for GROUP-ID to shared state."
  (let ((groups (qq-state-groups))
        changed)
    (setq groups
          (mapcar
           (lambda (group)
             (when (equal (alist-get 'group_id group) group-id)
               (setf (alist-get field group nil nil #'eq) value)
               (setq changed t))
             group)
           groups))
    (when changed
      (qq-state-apply-groups groups))
    changed))

(defun qq-native--group-setting-success
    (group-id field value callback receipt)
  "Apply one confirmed group setting and forward RECEIPT to CALLBACK."
  (qq-native--apply-group-setting group-id field value)
  (when callback
    (funcall callback receipt)))

(defun qq-native-set-group-name
    (group-id name &optional callback errback)
  "Set GROUP-ID's public NAME."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group name requires an exact group UIN"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: Group name must be a non-empty string"))
  (let ((success (apply-partially #'qq-native--group-setting-success
                                  group-id 'group_name name callback)))
    (qq-native--wrap-request
     (qq-gateway-directory-set-group-name
      group-id name success (or errback #'qq-native--default-error)))))

(defun qq-native-set-friend-pinned
    (user-id pinned &optional callback errback)
  "Set USER-ID's friend conversation PINNED state."
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Friend pinned state requires an exact user UIN"))
  (setq pinned (and pinned t))
  (qq-native--wrap-request
   (qq-gateway-directory-set-friend-pinned
    user-id pinned callback (or errback #'qq-native--default-error))))

(defun qq-native-set-presence (presence &optional callback errback)
  "Set the selected account's closed PRESENCE through the native service.

CALLBACK receives the exact acknowledgement.  The request is routed to the
locally selected managed account without changing its lifecycle phase."
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (let ((owner
         (or (qq-gateway-current-account-owner)
             (user-error "qq: Select a managed QQ account first"))))
    (qq-native--wrap-request
     (qq-gateway-account-set-presence
      (car owner) presence callback (or errback #'qq-native--default-error)))))

(defun qq-native-set-group-remark
    (group-id remark &optional callback errback)
  "Set or clear GROUP-ID's account-local REMARK."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group remark requires an exact group UIN"))
  (unless (stringp remark)
    (user-error "qq: Group remark must be a string"))
  (let ((success (apply-partially #'qq-native--group-setting-success
                                  group-id 'group_remark
                                  (and (not (string-empty-p remark)) remark)
                                  callback)))
    (qq-native--wrap-request
     (qq-gateway-directory-set-group-remark
      group-id remark success (or errback #'qq-native--default-error)))))

(defun qq-native-set-group-whole-mute
    (group-id enabled &optional callback errback)
  "Set GROUP-ID's whole-group mute state."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group whole mute requires an exact group UIN"))
  (setq enabled (and enabled t))
  (qq-native--wrap-request
   (qq-gateway-directory-set-group-whole-mute
    group-id enabled callback (or errback #'qq-native--default-error))))

(defun qq-native-set-group-pinned
    (group-id pinned &optional callback errback)
  "Set GROUP-ID's conversation PINNED state."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group pinned state requires an exact group UIN"))
  (setq pinned (and pinned t))
  (let ((success (apply-partially #'qq-native--group-setting-success
                                  group-id 'pinned
                                  (if pinned t :false) callback)))
    (qq-native--wrap-request
     (qq-gateway-directory-set-group-pinned
      group-id pinned success (or errback #'qq-native--default-error)))))

(defun qq-native-clock-in-group
    (group-id &optional callback errback)
  "Clock the selected account into GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group clock-in requires an exact group UIN"))
  (qq-native--wrap-request
   (qq-gateway-directory-clock-in-group
    group-id callback (or errback #'qq-native--default-error))))

(defun qq-native-get-group-at-all-remaining
    (group-id callback &optional errback)
  "Fetch GROUP-ID's live @all availability."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group @all quota requires an exact group UIN"))
  (qq-native--wrap-request
   (qq-gateway-directory-get-group-at-all-remaining
    group-id callback (or errback #'qq-native--default-error))))

(defun qq-native--group-leave-success (group-id callback receipt)
  "Remove confirmed GROUP-ID from loaded shared state, then forward RECEIPT."
  (when (qq-state-groups-loaded-p)
    (qq-state-apply-groups
     (seq-remove
      (lambda (group)
        (equal (alist-get 'group_id group) group-id))
      (qq-state-groups))))
  (when callback
    (funcall callback receipt)))

(defun qq-native-leave-group
    (group-id &optional callback errback)
  "Leave GROUP-ID without dismissing it."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group leave requires an exact group UIN"))
  (let ((success (apply-partially #'qq-native--group-leave-success
                                  group-id callback)))
    (qq-native--wrap-request
     (qq-gateway-directory-leave-group
      group-id success (or errback #'qq-native--default-error)))))

(defun qq-native-set-group-member-card
    (group-id user-id card &optional callback errback)
  "Set or clear USER-ID's CARD in GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group member card requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Group member card requires an exact user UIN"))
  (unless (stringp card)
    (user-error "qq: Group member card must be a string"))
  (qq-native--wrap-request
   (qq-gateway-directory-set-group-member-card
    group-id user-id card callback (or errback #'qq-native--default-error))))

(defun qq-native-set-group-member-special-title
    (group-id user-id special-title &optional callback errback)
  "Set or clear USER-ID's SPECIAL-TITLE in GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Special title requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Special title requires an exact user UIN"))
  (unless (stringp special-title)
    (user-error "qq: Special title must be a string"))
  (qq-native--wrap-request
   (qq-gateway-directory-set-group-member-special-title
    group-id user-id special-title callback
    (or errback #'qq-native--default-error))))

(defun qq-native-kick-group-member
    (group-id user-id reject-add-request &optional callback errback)
  "Remove USER-ID from GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group kick requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Group kick requires an exact user UIN"))
  (setq reject-add-request (and reject-add-request t))
  (qq-native--wrap-request
   (qq-gateway-directory-kick-group-member
    group-id user-id reject-add-request callback
    (or errback #'qq-native--default-error))))

(defun qq-native-send-message
    (session-key segments &optional raw-message callback errback)
  "Send SEGMENTS to SESSION-KEY through the native service.

RAW-MESSAGE is an optional optimistic rendering override.  The native
protocol accepts only its closed segment union, then promotes the pending row
from the later authoritative self event."
  (qq-gateway-message-send
   session-key segments raw-message callback
   (or errback #'qq-native--default-error)))

(defun qq-native-send-poke
    (session-key target-id &optional callback errback)
  "Poke TARGET-ID in SESSION-KEY through the native service."
  (qq-gateway-message-send-poke
   session-key target-id callback (or errback #'qq-native--default-error)))

(defun qq-native-set-message-reaction
    (message emoji-id set &optional callback errback)
  "Add or remove EMOJI-ID on normalized native group MESSAGE.

SET non-nil adds the reaction.  CALLBACK receives the successful service
receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Reaction requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key (stringp message-id))
      (user-error "qq: Reaction requires exact session and message identity"))
    (qq-gateway-message-set-reaction
     message emoji-id set callback (or errback #'qq-native--default-error))))

(defun qq-native-set-message-essence
    (message set &optional callback errback)
  "Set or remove normalized native group MESSAGE as essence.

SET non-nil marks the message as essence.  CALLBACK receives the successful
service receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Essence action requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-protocol-message-id-p message-id))
      (user-error "qq: Essence action requires exact group message identity"))
    (qq-gateway-message-set-essence
     message set callback (or errback #'qq-native--default-error))))

(defun qq-native-set-message-todo
    (message operation &optional callback errback)
  "Apply todo OPERATION to normalized native group MESSAGE.

OPERATION must be one of `set', `complete', or `cancel'.  CALLBACK receives
the successful closed receipt; ERRBACK receives failure details."
  (unless (listp message)
    (user-error "qq: Todo action requires a normalized message"))
  (unless (memq operation '(set complete cancel))
    (user-error "qq: Unknown message todo operation %S" operation))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key
                 (eq (qq-state-session-key-type session-key) 'group)
                 (qq-protocol-message-id-p message-id))
      (user-error "qq: Todo action requires exact group message identity"))
    (qq-gateway-message-set-todo
     message operation callback (or errback #'qq-native--default-error))))

(defun qq-native-recall-poke (message &optional callback errback)
  "Recall normalized poke MESSAGE through its native capability.

CALLBACK receives the successful response; ERRBACK receives failure details."
  (unless (qq-state-poke-message-p message)
    (user-error "qq: Poke recall requires a normalized poke message"))
  (let ((session-key (alist-get 'session-key message))
        (reference (qq-state-poke-recall-reference message)))
    (unless (and session-key reference)
      (user-error "qq: Poke has no native recall capability"))
    (ignore session-key reference)
    (qq-gateway-message-recall-poke
     message callback (or errback #'qq-native--default-error))))

(defun qq-native-recall-message (message &optional callback errback)
  "Recall normalized MESSAGE through the native service.

CALLBACK receives the successful response; ERRBACK receives the failure
response and reason."
  (unless (listp message)
    (user-error "qq: Recall requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key (stringp message-id))
      (user-error "qq: Recall requires exact session and message identity"))
    (qq-gateway-message-recall
     session-key message callback (or errback #'qq-native--default-error))))

(defun qq-native--message-at-sequence (session-key sequence)
  "Return SESSION-KEY message carrying exact SEQUENCE, or nil."
  (seq-find
   (lambda (message)
     (equal (alist-get 'message-seq message) sequence))
   (qq-state-session-messages session-key)))

(defun qq-native-history-frontier (session-key)
  "Return the native service's exact known history frontier.

SESSION-KEY identifies the private or group conversation.  The result is a
plist containing `:sequence' and,
when known, `:message-id'.  Group `latest_sequence' is authoritative and is
advanced by a newer live event.  Private history deliberately exposes only a
live sequence observed by this client projection because neither Lagrange nor
the QQ C2C history method provides a latest cursor.  `:empty-p' or
`:unavailable-reason' explains a result without a sequence."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target-id (alist-get 'target-id identity))
         (live (qq-gateway-message-live-frontier session-key))
         (live-sequence (alist-get 'sequence live)))
    (unless (memq kind '(private group))
      (user-error "qq: Native history supports private and group chats"))
    (pcase kind
      ('private
       (if live-sequence
           (list :sequence live-sequence
                 :message-id (alist-get 'message_id live)
                 :source 'live-event)
         (list :unavailable-reason 'private-latest-sequence)))
      ('group
       (let* ((group (qq-state-group target-id))
              (directory-sequence (alist-get 'latest_sequence group))
              sequence source)
         (when (and directory-sequence
                    (not (qq-gateway--canonical-decimal-p
                          directory-sequence t)))
           (error "qq: Native group latest_sequence is not exact"))
         (cond
          ((and live-sequence
                (or (null directory-sequence)
                    (qq-gateway--decimal-less-p
                     directory-sequence live-sequence)))
           (setq sequence live-sequence source 'live-event))
          (directory-sequence
           (setq sequence directory-sequence source 'group-directory)))
         (cond
          (sequence
           (let ((message
                  (or (and (equal sequence live-sequence) live)
                      (qq-native--message-at-sequence session-key sequence))))
             (list :sequence sequence
                   :message-id (or (alist-get 'message_id message)
                                   (alist-get 'server-id message))
                   :source source
                   :authoritative-p t)))
          (group
           (list :empty-p t :source 'group-directory :authoritative-p t))
          (t
           (list :unavailable-reason 'group-directory))))))))

(defun qq-native-history-range-before (start-sequence count)
  "Return a native range of COUNT messages before START-SEQUENCE, or nil at zero."
  (qq-gateway-message--validate-sequence
   start-sequence "Current history start sequence")
  (qq-gateway-message--validate-history-count count)
  (unless (equal start-sequence "0")
    (qq-gateway-message-history-range-ending-at
     (qq-gateway-message--decimal-subtract-small start-sequence 1)
     count)))

(defun qq-native-history-range-after
    (end-sequence count &optional maximum-sequence)
  "Return COUNT native messages after END-SEQUENCE, capped at MAXIMUM-SEQUENCE.

All sequence values stay canonical decimal strings.  Return nil when MAXIMUM
is already covered."
  (qq-gateway-message--validate-sequence
   end-sequence "Current history end sequence")
  (qq-gateway-message--validate-history-count count)
  (when maximum-sequence
    (qq-gateway-message--validate-sequence
     maximum-sequence "Known latest history sequence"))
  (unless (and maximum-sequence
               (not (qq-gateway--decimal-less-p
                     end-sequence maximum-sequence)))
    (let* ((start-sequence
            (qq-gateway-message--decimal-add-small end-sequence 1))
           (candidate-end
            (qq-gateway-message--decimal-add-small
             start-sequence (1- count)))
           (range-end
            (if (and maximum-sequence
                     (qq-gateway--decimal-less-p
                      maximum-sequence candidate-end))
                maximum-sequence
              candidate-end)))
      (qq-gateway-message--validate-history-range
       start-sequence range-end))))

(defun qq-native--history-meta (meta &rest properties)
  "Return native history META prefixed with PROPERTIES."
  (append properties (copy-sequence meta)))

(defun qq-native-fetch-history-range
    (session-key start-sequence end-sequence callback &optional errback properties)
  "Fetch one native history range for SESSION-KEY.

START-SEQUENCE and END-SEQUENCE are inclusive exact strings.  CALLBACK receives
merge metadata prefixed by optional plist PROPERTIES.  ERRBACK handles a
transport or protocol failure."
  (qq-native--wrap-request
   (qq-gateway-message-get-history
    session-key start-sequence end-sequence
    (lambda (meta)
      (qq-gateway--invoke
       callback
       (apply #'qq-native--history-meta meta properties)))
    (or errback #'qq-native--default-error))))

(defun qq-native-fetch-latest-history
    (session-key callback &optional errback count)
  "Fetch the native service's latest known history for SESSION-KEY.

Native group history uses the directory's exact latest sequence.  Native
private history uses only a live observed sequence; when none exists CALLBACK
receives metadata with `:history-frontier-unavailable' instead of a guessed
request.  ERRBACK handles failure and COUNT limits the requested page size."
  (let* ((frontier (qq-native-history-frontier session-key))
         (sequence (plist-get frontier :sequence)))
    (cond
     (sequence
      (pcase-let ((`(,start-sequence . ,end-sequence)
                   (qq-gateway-message-history-range-ending-at
                    sequence
                    (min 100 (max 1 (or count qq-history-fetch-count))))))
        (qq-native-fetch-history-range
         session-key start-sequence end-sequence callback errback
         (list :history-at-latest-p t :history-frontier frontier))))
     ((plist-get frontier :empty-p)
      (qq-gateway--invoke
       callback
       (qq-native--history-meta
        (list :session-key session-key
              :message-count 0 :added-count 0 :batch-message-ids nil)
        :history-at-latest-p t :history-at-oldest-p t
        :history-frontier frontier))
      nil)
     (t
      (qq-gateway--invoke
       callback
       (qq-native--history-meta
        (list :session-key session-key
              :message-count 0 :added-count 0 :batch-message-ids nil)
        :history-frontier-unavailable
        (plist-get frontier :unavailable-reason)
        :history-frontier frontier))
      nil))))

(defun qq-native-fetch-history-around
    (session-key message-id callback &optional errback count)
  "Fetch native history around exact MESSAGE-ID in SESSION-KEY."
  (let* ((message
          (seq-find
           (lambda (candidate)
             (equal (alist-get 'server-id candidate) message-id))
           (qq-state-session-messages session-key)))
         (sequence (alist-get 'message-seq message)))
    (if (not sequence)
        (qq-gateway--client-error
         (or errback #'qq-native--default-error)
         "history_sequence_unavailable"
         "Native history can seek only a cached message carrying sequence metadata")
      (pcase-let ((`(,start-sequence . ,end-sequence)
                   (qq-gateway-message-history-range-around
                    sequence
                    (min 100 (max 1 (or count qq-history-fetch-count))))))
        (qq-native-fetch-history-range
         session-key start-sequence end-sequence callback errback
         (list :history-target-message-id message-id))))))

(defun qq-native-history-exhausted-error-p (response reason)
  "Return non-nil when native RESPONSE and REASON mean history EOF."
  (ignore response reason)
  nil)

(defun qq-native-supports-p (capability)
  "Return non-nil when the native service supports product CAPABILITY."
  (memq capability
        '(contacts group-members group-settings group-member-settings
          group-moderation group-clock-in group-at-all-quota
          group-lifecycle presence
          send-text send-message face reply mention poke recall
          explicit-history)))

(defun qq-native-presence-capable-p ()
  "Return non-nil when the selected account can accept presence changes."
  (and (qq-native-ready-p)
       (qq-native-supports-p 'presence)
       (let ((account (qq-gateway-current-account)))
         (and account
              (equal (alist-get 'phase account) "online")
              (member "account.set_presence"
                      (qq-gateway-transport-capabilities))))))

(defun qq-native--bootstrap-complete (owner failed-p)
  "Complete one directory bootstrap part for OWNER, recording FAILED-P."
  (when (equal owner qq-native--bootstrap-owner)
    (if failed-p
        (setq qq-native--bootstrap-owner nil
              qq-native--bootstrap-pending 0)
      (setq qq-native--bootstrap-pending
            (max 0 (1- qq-native--bootstrap-pending))))))

(defun qq-native--bootstrap-success (owner _value)
  "Record one successful directory bootstrap part for OWNER."
  (qq-native--bootstrap-complete owner nil))

(defun qq-native--bootstrap-failure (owner _body reason)
  "Record failed directory bootstrap OWNER and report its REASON."
  (qq-native--bootstrap-complete owner t)
  (qq-native--default-error nil reason))

(defun qq-native--maybe-bootstrap (&rest _arguments)
  "Load missing directory state for the selected online native account."
  (when (qq-gateway-transport-ready-p)
    (let* ((account (qq-gateway-current-account))
           (owner (qq-gateway-current-account-owner))
           (friends-needed (not (qq-state-friend-categories-loaded-p)))
           (groups-needed (not (qq-state-groups-loaded-p)))
           (part-count (+ (if friends-needed 1 0)
                          (if groups-needed 1 0))))
      (when (and owner
                 (equal (alist-get 'phase account) "online")
                 (> part-count 0)
                 (not (equal owner qq-native--bootstrap-owner)))
        (setq qq-native--bootstrap-owner (copy-tree owner)
              qq-native--bootstrap-pending part-count)
        (when friends-needed
          (qq-native-refresh-friend-categories
           (apply-partially #'qq-native--bootstrap-success owner)
           (apply-partially #'qq-native--bootstrap-failure owner)))
        (when groups-needed
          (qq-native-refresh-joined-groups
           (apply-partially #'qq-native--bootstrap-success owner)
           (apply-partially #'qq-native--bootstrap-failure owner)))))))

(defun qq-native-activate ()
  "Make the selected native account own shared client projection state."
  (qq-gateway-message-activate-projection)
  (qq-native--maybe-bootstrap)
  t)

(defun qq-native-reset-session-state ()
  "Revoke account projection and request caches."
  (setq qq-native--bootstrap-owner nil
        qq-native--bootstrap-pending 0)
  (qq-gateway-directory-reset)
  (qq-gateway-message-revoke-projection))

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-native--maybe-bootstrap t)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-native--maybe-bootstrap t)

(provide 'qq-native)

;;; qq-native.el ends here
