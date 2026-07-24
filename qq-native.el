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
(require 'qq-gateway-attachment)
(require 'qq-gateway-conversation)
(require 'qq-gateway-directory)
(require 'qq-gateway-message)
(require 'qq-gateway-media)
(require 'qq-gateway-transport)
(require 'qq-native-request)
(require 'qq-protocol)
(require 'qq-state)

(cl-defstruct (qq-native--read-operation
               (:constructor qq-native--read-operation-create))
  "One stable read request with an in-flight leaf and queued successor."
  owner
  request
  message
  callback
  errback
  next-message
  next-callback
  next-errback
  token)

(defvar qq-native--bootstrap-owner nil
  "Account owner whose initial directory refresh is running or complete.")

(defvar qq-native--bootstrap-instance-id nil
  "Gateway instance whose selected-slot bootstrap is running or complete.")

(defvar qq-native--bootstrap-pending 0
  "Number of recent/directory parts pending for the current bootstrap.")

(defvar qq-native--bootstrap-token nil
  "Opaque identity of the current bootstrap attempt.")

(defvar qq-native--friend-directory-owner nil
  "Stable account slot that produced the displayed friend directory.")

(defvar qq-native--group-directory-owner nil
  "Stable account slot that produced the displayed group directory.")

(defvar qq-native--observed-account-id nil
  "Selected account slot whose phase is recorded by the native facade.")

(defvar qq-native--observed-account-phase nil
  "Last observed phase of `qq-native--observed-account-id'.")

(defvar qq-native--recent-resync-context nil
  "Selected slot/Gateway context with one lag resync currently in flight.")

(defvar qq-native--recent-request nil
  "Newest product request allowed to replace the recent-session projection.")

(defvar qq-native--read-operations (make-hash-table :test #'equal)
  "Newest in-flight and coalesced native read intent per session.")

(defun qq-native--revoke-read-operations (&rest _ignored)
  "Revoke every in-flight or coalesced read intent.

Late callbacks become inert because their operation token is no longer owned.
Pending transport requests are forgotten after ownership has been revoked, so
cancellation cannot reenter the old operation."
  (let (requests)
    (maphash
     (lambda (_session-key operation)
       (when-let* ((request (qq-native--read-operation-request operation)))
         (push request requests)))
     qq-native--read-operations)
    (setq qq-native--read-operations (make-hash-table :test #'equal))
    (dolist (request requests)
      (qq-native-cancel-request request))))

(defun qq-native--revoke-stale-read-operations (&rest _ignored)
  "Revoke read work when the selected stable account slot has changed."
  (let ((owner (qq-gateway-current-account-id))
        stale-p)
    (maphash
     (lambda (_session-key operation)
       (unless (equal owner
                      (qq-native--read-operation-owner operation))
         (setq stale-p t)))
     qq-native--read-operations)
    (when stale-p
      (qq-native--revoke-read-operations))))


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

(cl-defun qq-native--start-request
    (starter callback errback
             &key (owner nil owner-supplied-p))
  "Start asynchronous product work through STARTER.

CALLBACK and ERRBACK are product-facing leaf callbacks.  When omitted, OWNER
defaults to the stable selected account slot; an explicitly supplied nil marks
global work.  Omitting OWNER without a selected slot is a user error.
Return one uniform `qq-native-request'."
  (qq-native-request-start
   starter
   :callback callback
   :errback (or errback #'qq-native--default-error)
   :owner (if owner-supplied-p
              owner
            (or (qq-gateway-current-account-id)
                (user-error "qq: Select a QQ account first")))))

(defun qq-native--directory-refresh-success
    (kind owner callback value)
  "Record directory KIND observation OWNER, then call CALLBACK with VALUE."
  (when (equal owner (qq-gateway-current-account-id))
    (pcase kind
      ('friends
       (setq qq-native--friend-directory-owner (copy-sequence owner)))
      ('groups
       (setq qq-native--group-directory-owner (copy-sequence owner)))
      (_ (error "qq: unknown native directory kind %S" kind))))
  (when callback
    (funcall callback value)))

(defun qq-native-refresh-friend-categories
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with categories.

ERRBACK receives the service response and reason.  REFRESH forces the service's
contact cache."
  (let ((owner (or (qq-gateway-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-directory-refresh-friends success failure refresh))
     (apply-partially
      #'qq-native--directory-refresh-success 'friends owner callback)
     errback)))

(defun qq-native-refresh-joined-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with them.

ERRBACK receives the service response and reason.  REFRESH forces the service's
contact cache."
  (let ((owner (or (qq-gateway-current-account-id)
                   (user-error "qq: Select a QQ account first"))))
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-directory-refresh-groups success failure refresh))
     (apply-partially
      #'qq-native--directory-refresh-success 'groups owner callback)
     errback)))

(defun qq-native--recent-private-row-projectable-p (row account)
  "Return non-nil when private recent ROW can derive a UIN using ACCOUNT."
  (let* ((message (alist-get 'latest_message row))
         (sender (alist-get 'sender message))
         (recipient (alist-get 'recipient message))
         (sender-self (qq-gateway-message--endpoint-self-p sender account))
         (recipient-self
          (qq-gateway-message--endpoint-self-p recipient account))
         (peer
          (pcase (list (and sender-self t) (and recipient-self t))
            (`(t nil) recipient)
            (`(nil t) sender)
            (`(t t) recipient)
            (_ (error "qq: recent private endpoints do not identify account")))))
    (and (qq-gateway--canonical-decimal-p (alist-get 'uin peer)) t)))

(defun qq-native--recent-row-projectable-p (row account)
  "Return non-nil when recent ROW has a product session key for ACCOUNT."
  (let ((identity (alist-get 'conversation row)))
    (pcase (alist-get 'kind identity)
      ("group" t)
      ;; The identity may legitimately be UID-only.  The latest message
      ;; can still provide the peer UIN needed by the product session key.
      ("private" (qq-native--recent-private-row-projectable-p row account))
      ;; Temporary conversations are valid protocol rows but have no product
      ;; session-key/routing model yet.
      ("temporary" nil))))

(defun qq-native--recent-row-state-entry (page row account)
  "Return one state-domain entry for PAGE ROW and ACCOUNT."
  (let* ((account-id (alist-get 'account_id page))
         (identity (alist-get 'conversation row))
         (normalized
          (qq-gateway-message-normalize-snapshot
           (alist-get 'latest_message row)
           account-id
           account
           (eq (alist-get 'latest_message_recalled row) t)))
         (session-key (alist-get 'session-key normalized))
         (session-identity (qq-state-session-key-identity session-key)))
    (pcase (alist-get 'kind identity)
      ("private"
       (unless (and (eq (alist-get 'type session-identity) 'private)
                    (or (not (assq 'peer_uin identity))
                        (equal (alist-get 'peer_uin identity)
                               (alist-get 'target-id session-identity)))
                    (or (not (assq 'peer_uid identity))
                        (equal (alist-get 'peer_uid identity)
                               (alist-get 'peer-uid normalized))))
         (error "qq: recent private identity contradicts latest message")))
      ("group"
       (unless (and (eq (alist-get 'type session-identity) 'group)
                    (equal (alist-get 'group_uin identity)
                           (alist-get 'target-id session-identity)))
         (error "qq: recent group identity contradicts latest message"))))
    (list
     :session-key session-key
     :message normalized
     :activity-revision (alist-get 'activity_revision row)
     :pinned-known-p (and (assq 'pinned row) t)
     :pinned (alist-get 'pinned row))))

(defun qq-native--apply-recent-page (page observation-token)
  "Normalize and apply recent PAGE for OBSERVATION-TOKEN."
  (let ((account (qq-gateway-current-account)))
    (unless (and account
                 (equal (alist-get 'account_id account)
                        (alist-get 'account_id page)))
      (error "qq: recent page no longer belongs to the selected account slot"))
    (qq-state-apply-recent-conversations
     (seq-keep
      (lambda (row)
        (when (qq-native--recent-row-projectable-p row account)
          (qq-native--recent-row-state-entry page row account)))
      (alist-get 'conversations page))
     observation-token)))

(defun qq-native-refresh-recent-conversations
    (&optional callback errback limit)
  "Refresh the selected account slot's native recent conversations.

This operation is slot-scoped: a restart of the same managed account does not
invalidate its result.  CALLBACK receives the resulting state session list;
ERRBACK receives a service/client error.  LIMIT defaults at the Gateway
adapter boundary."
  (let ((account-id (or (qq-gateway-current-account-id)
                        (user-error "qq: Select a QQ account first")))
        (observation-token (qq-state-session-summary-observation-start))
        (error-fn (or errback #'qq-native--default-error))
        request)
    (when qq-native--recent-request
      (qq-native-cancel-request qq-native--recent-request))
    (setq qq-native--recent-request nil)
    (setq request
          (qq-native--start-request
           (lambda (success failure)
             (qq-gateway-conversation-list-recent
              account-id
              :callback success
              :errback failure
              :limit limit))
           (lambda (page)
             (when (eq request qq-native--recent-request)
               (setq qq-native--recent-request nil))
             (condition-case error-data
                 (qq-gateway--invoke
                  callback
                  (qq-native--apply-recent-page page observation-token))
               (error
                (let ((reason (error-message-string error-data)))
                  (qq-gateway--invoke
                   error-fn
                   `((code . "client_projection_failed")
                     (message . ,reason))
                   reason)))))
           (lambda (body reason)
             (when (eq request qq-native--recent-request)
               (setq qq-native--recent-request nil))
             (qq-gateway--invoke error-fn body reason))
           :owner account-id))
    (when (qq-native-request-active-p request)
      (setq qq-native--recent-request request))
    request))

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

The projection is derived from its selected-slot joined-group cache; when
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
  "Refresh primary native recent and directory data."
  (interactive)
  (let* ((online-p (equal (alist-get 'phase (qq-gateway-current-account))
                          "online"))
         (contacts-p (and online-p (qq-native-supports-p 'contacts))))
    (delq nil
          (list
           (when (qq-native-supports-p 'recent-conversations)
             (qq-native-refresh-recent-conversations))
           (when contacts-p (qq-native-refresh-friend-categories))
           (when contacts-p (qq-native-refresh-joined-groups))))))

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

CALLBACK receives at most LIMIT exact member projections.  Native member pages
are complete selected-slot snapshots, so repeated queries use the local page
after its first fetch.  ERRBACK receives the service response and reason."
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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-directory-list-group-members
        group-id
        (lambda (members)
          (funcall success
                   (qq-native--filter-members members query limit)))
        failure))
     callback errback)))

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
    (group-id field value success receipt)
  "Apply one confirmed group setting and forward RECEIPT to SUCCESS."
  (qq-native--apply-group-setting group-id field value)
  (funcall success receipt))

(defun qq-native-set-group-name
    (group-id name &optional callback errback)
  "Set GROUP-ID's public NAME."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group name requires an exact group UIN"))
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: Group name must be a non-empty string"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-name
      group-id name
      (apply-partially #'qq-native--group-setting-success
                       group-id 'group_name name success)
      failure))
   callback errback))

(defun qq-native-set-friend-pinned
    (user-id pinned &optional callback errback)
  "Set USER-ID's friend conversation PINNED state."
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Friend pinned state requires an exact user UIN"))
  (setq pinned (and pinned t))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-friend-pinned
      user-id pinned success failure))
   callback errback))

(defun qq-native-set-presence (presence &optional callback errback)
  "Set the selected account's PRESENCE through the native service.

CALLBACK receives the exact acknowledgement.  The request is routed to the
locally selected managed account without changing its lifecycle phase."
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (let ((account-id
         (or (qq-gateway-current-account-id)
             (user-error "qq: Select a managed QQ account first"))))
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-account-set-presence
        account-id presence success failure))
     callback errback)))

(defun qq-native-set-group-remark
    (group-id remark &optional callback errback)
  "Set or clear GROUP-ID's account-local REMARK."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group remark requires an exact group UIN"))
  (unless (stringp remark)
    (user-error "qq: Group remark must be a string"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-remark
      group-id remark
      (apply-partially
       #'qq-native--group-setting-success
       group-id 'group_remark
       (and (not (string-empty-p remark)) remark)
       success)
      failure))
   callback errback))

(defun qq-native-set-group-whole-mute
    (group-id enabled &optional callback errback)
  "Set GROUP-ID's whole-group mute state."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group whole mute requires an exact group UIN"))
  (setq enabled (and enabled t))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-whole-mute
      group-id enabled success failure))
   callback errback))

(defun qq-native-set-group-pinned
    (group-id pinned &optional callback errback)
  "Set GROUP-ID's conversation PINNED state."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group pinned state requires an exact group UIN"))
  (setq pinned (and pinned t))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-pinned
      group-id pinned
      (apply-partially #'qq-native--group-setting-success
                       group-id 'pinned (if pinned t :false) success)
      failure))
   callback errback))

(defun qq-native-clock-in-group
    (group-id &optional callback errback)
  "Clock the selected account into GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group clock-in requires an exact group UIN"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-clock-in-group group-id success failure))
   callback errback))

(defun qq-native-get-group-at-all-remaining
    (group-id callback &optional errback)
  "Fetch GROUP-ID's live @all availability."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group @all quota requires an exact group UIN"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-get-group-at-all-remaining
      group-id success failure))
   callback errback))

(defun qq-native--group-leave-success (group-id success receipt)
  "Remove confirmed GROUP-ID from state, then forward RECEIPT to SUCCESS."
  (when (qq-state-groups-loaded-p)
    (qq-state-apply-groups
     (seq-remove
      (lambda (group)
        (equal (alist-get 'group_id group) group-id))
      (qq-state-groups))))
  (funcall success receipt))

(defun qq-native-leave-group
    (group-id &optional callback errback)
  "Leave GROUP-ID without dismissing it."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group leave requires an exact group UIN"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-leave-group
      group-id
      (apply-partially #'qq-native--group-leave-success group-id success)
      failure))
   callback errback))

(defun qq-native-set-group-member-card
    (group-id user-id card &optional callback errback)
  "Set or clear USER-ID's CARD in GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group member card requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Group member card requires an exact user UIN"))
  (unless (stringp card)
    (user-error "qq: Group member card must be a string"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-member-card
      group-id user-id card success failure))
   callback errback))

(defun qq-native-set-group-member-special-title
    (group-id user-id special-title &optional callback errback)
  "Set or clear USER-ID's SPECIAL-TITLE in GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Special title requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Special title requires an exact user UIN"))
  (unless (stringp special-title)
    (user-error "qq: Special title must be a string"))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-set-group-member-special-title
      group-id user-id special-title success failure))
   callback errback))

(defun qq-native-kick-group-member
    (group-id user-id reject-add-request &optional callback errback)
  "Remove USER-ID from GROUP-ID."
  (unless (qq-native-group-id-p group-id)
    (user-error "qq: Group kick requires an exact group UIN"))
  (unless (qq-native-user-id-p user-id)
    (user-error "qq: Group kick requires an exact user UIN"))
  (setq reject-add-request (and reject-add-request t))
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-directory-kick-group-member
      group-id user-id reject-add-request success failure))
   callback errback))

(defun qq-native--local-media-plan (segment index)
  "Return a preparation plan for local media SEGMENT at INDEX, or nil.

Image and record segments carrying an opaque `attachment_id' are already
protocol-ready.  URL-only media deliberately fails: the native service accepts
immutable staged bytes, not a URL that could change before upload."
  (let ((kind (alist-get 'type segment)))
    (when (member kind '("image" "record"))
      (let* ((data (alist-get 'data segment))
             (attachment-id (and (listp data)
                                 (alist-get 'attachment_id data)))
             (file (and (listp data)
                        (or (alist-get 'file data)
                            (alist-get 'path data))))
             (summary (and (listp data) (alist-get 'summary data)))
             (sub-type (and (listp data) (alist-get 'sub_type data))))
        (cond
         (attachment-id nil)
         ((and (stringp file) (not (string-empty-p file)))
          (let ((path (expand-file-name file)))
            (unless (and (file-regular-p path) (file-readable-p path))
              (user-error
               "qq: %s source is not a readable regular file: %s"
               (capitalize kind) path))
            (if (equal kind "image")
                (let ((summary (or summary "[图片]"))
                      (sub-type (or sub-type 0)))
                  (unless (and (qq-gateway--non-empty-string-p summary)
                               (<= (length (string-to-list summary)) 128)
                               (not (string-match-p "[[:cntrl:]]" summary)))
                    (user-error
                     "qq: Image summary must be 1–128 printable characters"))
                  (unless (and (integerp sub-type)
                               (<= 0 sub-type #xffffffff))
                    (user-error "qq: Image sub-type must be an unsigned integer"))
                  (list :index index :kind kind :path path
                        :summary summary :sub-type sub-type))
              (list :index index :kind kind :path path))))
         (t
          (user-error
           "qq: Native %s sending requires a local file or prepared attachment"
           kind)))))))

(defun qq-native--release-send-resource (resource-id)
  "Best-effort release one send-pipeline RESOURCE-ID."
  (when resource-id
    (condition-case error-data
        (qq-gateway-resource-release
         resource-id nil
         (lambda (_body reason)
           (message "qq: staged media cleanup failed: %s" reason)))
      ((error quit)
       (message "qq: staged media cleanup failed: %s"
                (error-message-string error-data))))))

(defun qq-native--release-send-attachment (attachment-id)
  "Best-effort release one unused send-pipeline ATTACHMENT-ID."
  (when attachment-id
    (condition-case error-data
        (qq-gateway-attachment-release
         attachment-id nil
         (lambda (_body reason)
           (message "qq: prepared media cleanup failed: %s" reason)))
      ((error quit)
       (message "qq: prepared media cleanup failed: %s"
                (error-message-string error-data))))))

(defun qq-native--send-message-with-local-media
    (session-key segments plans raw-message callback errback)
  "Resolve local media PLANS, then send SEGMENTS to SESSION-KEY.

Staging and preparation may run concurrently, but the immutable segment order
is retained.  This composite request owns preparation operations until they
produce ready attachments, then owns those attachments until `message.send'
accepts them.  Cancellation or failure releases everything still owned here."
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (optimistic-segments (copy-tree segments))
         (resolved (vconcat (copy-tree segments)))
         (remaining (length plans))
         (operations nil)
         (attachment-ids nil)
         (active t)
         send-token
         request)
    (cl-labels
        ((cancel-operation
           (operation)
           (condition-case error-data
               (qq-gateway-attachment-cancel-operation operation)
             ((error quit)
              (message "qq: local media operation cleanup failed: %s"
                       (error-message-string error-data)))))
         (release-attachments
           ()
           (let ((owned-attachment-ids (delete-dups attachment-ids)))
             (setq attachment-ids nil)
             (dolist (attachment-id owned-attachment-ids)
               (qq-native--release-send-attachment attachment-id))))
         (cancel-send
           ()
           (when send-token
             (let ((token send-token))
               (setq send-token nil)
               (condition-case error-data
                   (qq-gateway-transport-cancel token)
                 ((error quit)
                  (message "qq: local media send cancellation failed: %s"
                           (error-message-string error-data)))))))
         (cleanup
           ()
           (let ((owned-operations operations))
             (setq operations nil)
             (cancel-send)
             (dolist (operation owned-operations)
               (cancel-operation operation))
             (release-attachments)))
         (finish
           (success-p body value)
           (when active
             (setq active nil
                   send-token nil)
             (if success-p
                 ;; Successful `message.send' has claimed every attachment.
                 (setq attachment-ids nil
                       operations nil)
               ;; Release is idempotent when the service already moved an
               ;; attachment into its Sending state.
               (cleanup))
             (if success-p
                 (qq-native-request-finish request)
               (qq-native-request-fail request))
             (if success-p
                 (qq-native-request--invoke callback value)
               (qq-native-request--invoke errback body value))))
         (send-failed
           (body reason)
           (finish nil body reason))
         (send-succeeded
           (receipt)
           (finish t nil receipt))
         (dispatch
           ()
           (if (not (equal owner (qq-gateway-current-account-id)))
               (qq-native-cancel-request request)
             (condition-case error-data
                 (let ((token
                        (qq-gateway-message-send
                         session-key (append resolved nil) raw-message
                         #'send-succeeded #'send-failed
                         optimistic-segments)))
                   ;; Nil is paired with a synchronous preflight failure.
                   ;; Accepted requests complete later on the event loop.
                   (when (and active token)
                     (setq send-token token)
                     (setf (qq-native-request-token request) token)))
               (error
                (send-failed nil (error-message-string error-data))))))
         (media-ready
           (plan attachment)
           (let ((attachment-id (alist-get 'attachment_id attachment))
                 (resource-id (alist-get 'resource_id attachment)))
             (if (not active)
                 (progn
                   (qq-native--release-send-attachment attachment-id)
                   (qq-native--release-send-resource resource-id))
               (push attachment-id attachment-ids)
               ;; The prepared attachment keeps the bytes alive, so the
               ;; composite never needs to retain the extra resource lease.
               (qq-native--release-send-resource resource-id)
               (unless (equal owner (qq-gateway-current-account-id))
                 (qq-native-cancel-request request))
               (when active
                 (aset resolved (plist-get plan :index)
                       `((type . ,(plist-get plan :kind))
                         (data . ((attachment_id . ,attachment-id)))))
                 (setq remaining (1- remaining))
                 (when (= remaining 0)
                   (dispatch))))))
         (cancel
           ()
           (when active
             (setq active nil)
             (cleanup)))
         (abort-startup
           ()
           (when active
             (setq active nil)
             (cleanup))
           (when request
             (qq-native-request-fail request))))
      (condition-case error-data
          (progn
            (setq request (qq-native-request-create owner #'cancel))
            (dolist (plan plans)
              (when active
                (let* ((ready (apply-partially #'media-ready plan))
                       (operation
                        (pcase (plist-get plan :kind)
                          ("image"
                           (qq-gateway-attachment-stage-and-prepare-image
                            session-key (plist-get plan :path)
                            (plist-get plan :summary)
                            (plist-get plan :sub-type)
                            ready #'send-failed))
                          ("record"
                           (qq-gateway-attachment-stage-and-prepare-record
                            session-key (plist-get plan :path)
                            ready #'send-failed))
                          (_ (error "qq: Unknown local media plan")))))
                  (when (qq-gateway-attachment-operation-active-p operation)
                    (if active
                        (push operation operations)
                      (cancel-operation operation))))))
            request)
        ((error quit)
         (abort-startup)
         (signal (car error-data) (cdr error-data)))))))

(defun qq-native-send-message
    (session-key segments &optional raw-message callback errback)
  "Send SEGMENTS to SESSION-KEY through the native service.

Local image paths are copied into the service Resource Store and local PCM WAV
records are first derived into message-ready Tencent Silk.  Both are prepared
for the selected account and conversation, then replaced by opaque attachment
IDs before the wire request is sent.  RAW-MESSAGE is an
optional optimistic rendering override.  The pending row is promoted only by
the later authoritative self event."
  (let ((plans
         (cl-loop for segment in segments
                  for index from 0
                  for plan = (qq-native--local-media-plan segment index)
                  when plan collect plan))
        (error-fn (or errback #'qq-native--default-error)))
    (if plans
        (qq-native--send-message-with-local-media
         session-key segments plans raw-message callback error-fn)
      (qq-native--start-request
       (lambda (success failure)
         (qq-gateway-message-send
          session-key segments raw-message success failure))
       callback error-fn))))

(defun qq-native-send-poke
    (session-key target-id &optional callback errback)
  "Poke TARGET-ID in SESSION-KEY through the native service."
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-message-send-poke
      session-key target-id success failure))
   callback errback))

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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-message-set-reaction
        message emoji-id set success failure))
     callback errback)))

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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-message-set-essence message set success failure))
     callback errback)))

(defun qq-native-set-message-todo
    (message operation &optional callback errback)
  "Apply todo OPERATION to normalized native group MESSAGE.

OPERATION must be one of `set', `complete', or `cancel'.  CALLBACK receives
the successful receipt; ERRBACK receives failure details."
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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-message-set-todo message operation success failure))
     callback errback)))

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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-message-recall-poke message success failure))
     callback errback)))

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
    (qq-native--start-request
     (lambda (success failure)
       (qq-gateway-message-recall
        session-key message success failure))
     callback errback)))

(defun qq-native--read-operation-current-p (session-key operation)
  "Return non-nil when OPERATION still owns SESSION-KEY."
  (eq operation (gethash session-key qq-native--read-operations)))

(defun qq-native--read-message-after-p (candidate reference)
  "Return non-nil when CANDIDATE follows REFERENCE in their session timeline.

Read coalescing is a client projection concern.  It therefore compares stable
message identities in the cached timeline instead of depending on native
sequence metadata."
  (let ((candidate-session (alist-get 'session-key candidate))
        (reference-session (alist-get 'session-key reference))
        (candidate-id (alist-get 'server-id candidate))
        (reference-id (alist-get 'server-id reference)))
    (when (and candidate-session
               (equal candidate-session reference-session)
               (qq-protocol-message-id-p candidate-id)
               (qq-protocol-message-id-p reference-id))
      (let* ((messages (qq-state-session-messages candidate-session))
             (candidate-position
              (cl-position candidate-id messages
                           :key (lambda (message)
                                  (alist-get 'server-id message))
                           :test #'equal))
             (reference-position
              (cl-position reference-id messages
                           :key (lambda (message)
                                  (alist-get 'server-id message))
                           :test #'equal)))
        (and candidate-position
             reference-position
             (> candidate-position reference-position))))))

(defun qq-native--cancel-read-operation (session-key operation)
  "Revoke OPERATION, its queued intent, and its transport token."
  (when (qq-native--read-operation-current-p session-key operation)
    (remhash session-key qq-native--read-operations))
  (let ((token (qq-native--read-operation-token operation)))
    (setf (qq-native--read-operation-token operation) nil
          (qq-native--read-operation-next-message operation) nil
          (qq-native--read-operation-next-callback operation) nil
          (qq-native--read-operation-next-errback operation) nil)
    (when token
      (qq-gateway-transport-cancel token))))

(defun qq-native--advance-read-operation
    (session-key operation success-p)
  "Advance OPERATION after one leaf settles with SUCCESS-P.

The stable composite request remains active while a queued read intent exists
and becomes terminal only after the actual queue is empty."
  (if-let* ((message (qq-native--read-operation-next-message operation)))
      (progn
        (setf (qq-native--read-operation-message operation) message
              (qq-native--read-operation-callback operation)
              (qq-native--read-operation-next-callback operation)
              (qq-native--read-operation-errback operation)
              (qq-native--read-operation-next-errback operation)
              (qq-native--read-operation-next-message operation) nil
              (qq-native--read-operation-next-callback operation) nil
              (qq-native--read-operation-next-errback operation) nil)
        ;; Publish the successor before leaf delivery so callback reentry sees
        ;; the stable composite request still in flight.
        (qq-native--dispatch-read-operation session-key operation))
    (remhash session-key qq-native--read-operations)
    (if success-p
        (qq-native-request-finish
         (qq-native--read-operation-request operation))
      (qq-native-request-fail
       (qq-native--read-operation-request operation)))))

(defun qq-native--settle-read-leaf
    (session-key operation success-p body value)
  "Settle OPERATION's current read report with BODY and VALUE."
  (when (qq-native--read-operation-current-p session-key operation)
    (let* ((request (qq-native--read-operation-request operation))
           (callback
            (if success-p
                (qq-native--read-operation-callback operation)
              (or (qq-native--read-operation-errback operation)
                  #'qq-native--default-error))))
      (setf (qq-native--read-operation-token operation) nil)
      (if (not (qq-native-request--owner-current-p request))
          (qq-native-cancel-request request)
        (qq-native--advance-read-operation session-key operation success-p)
        (if success-p
            (qq-native-request--invoke callback value)
          (qq-native-request--invoke callback body value))))))

(defun qq-native--dispatch-read-operation (session-key operation)
  "Dispatch OPERATION's current leaf for SESSION-KEY."
  (let* ((request (qq-native--read-operation-request operation))
         (token
          (qq-gateway-message-mark-read
           (qq-native--read-operation-message operation)
           (lambda (receipt)
             (qq-native--settle-read-leaf
              session-key operation t nil receipt))
           (lambda (body reason)
             (qq-native--settle-read-leaf
              session-key operation nil body reason)))))
    ;; A nil token is paired with a synchronous failure callback.  Every
    ;; non-nil token completes later through the transport event loop.
    (when (and token
               (qq-native--read-operation-current-p session-key operation)
               (qq-native-request-active-p request))
      (setf (qq-native--read-operation-token operation) token))
    request))

(defun qq-native--start-mark-message-read (message callback errback)
  "Start one native read report for exact MESSAGE."
  (let* ((session-key (alist-get 'session-key message))
         (owner (qq-gateway-current-account-id))
         request
         operation)
    (condition-case error-data
        (progn
          (setq operation
                (qq-native--read-operation-create
                 :owner (copy-sequence owner)
                 :message message
                 :callback callback
                 :errback errback))
          (setq request
                (qq-native-request-create
                 owner
                 (lambda ()
                   (qq-native--cancel-read-operation session-key operation))))
          (setf (qq-native--read-operation-request operation) request)
          (puthash session-key operation qq-native--read-operations)
          (qq-native--dispatch-read-operation session-key operation)
          request)
      ((error quit)
        (when request
          (qq-native-cancel-request request))
        (signal (car error-data) (cdr error-data))))))

(defun qq-native-mark-message-read (message &optional callback errback)
  "Advance native read state through normalized MESSAGE.

Only one report per session is in flight and at most one newest message intent
is queued.  CALLBACK belongs only to an intent that is actually dispatched;
duplicate, older, and superseded intents do not accumulate waiters.  ERRBACK
receives failure details."
  (unless (qq-native-message-read-capable-p message)
    (user-error
     "qq: Native read report requires a current stable message reference"))
  (let* ((session-key (alist-get 'session-key message))
         (owner (qq-gateway-current-account-id))
         (operation (gethash session-key qq-native--read-operations)))
    (when (and operation
               (not (equal owner
                           (qq-native--read-operation-owner operation))))
      (qq-native--revoke-read-operations)
      (setq operation nil))
    (cond
     ((null operation)
      (qq-native--start-mark-message-read message callback errback))
     ((not (qq-native--read-message-after-p
            message (qq-native--read-operation-message operation)))
      (qq-native--read-operation-request operation))
     (t
      (let ((next (qq-native--read-operation-next-message operation)))
        (when (or (null next)
                  (qq-native--read-message-after-p message next))
          (setf (qq-native--read-operation-next-message operation) message
                (qq-native--read-operation-next-callback operation) callback
                (qq-native--read-operation-next-errback operation) errback))
        (qq-native--read-operation-request operation))))))

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
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-message-get-history
      session-key start-sequence end-sequence
      (lambda (meta)
        (funcall success
                 (apply #'qq-native--history-meta meta properties)))
      failure))
   callback errback))

(defun qq-native-fetch-private-history-page
    (session-key cursor callback &optional errback count properties)
  "Fetch one private roaming-history page for SESSION-KEY.

CURSOR is nil for the initial server-clock request or the exact continuation
cursor returned by the previous page.  CALLBACK receives merge metadata
prefixed by optional PROPERTIES."
  (qq-native--start-request
   (lambda (success failure)
     (qq-gateway-message-get-private-history
      session-key cursor
      (lambda (meta)
        (funcall success
                 (apply #'qq-native--history-meta meta properties)))
      failure
      (min 100 (max 1 (or count qq-history-fetch-count)))))
   callback errback))

(defun qq-native-fetch-latest-history
    (session-key callback &optional errback count)
  "Fetch the native service's latest known history for SESSION-KEY.

Native group history uses the directory's exact latest sequence.  Native
private history starts from the service clock through `SsoGetRoamMsg' and does
not require a live message sequence.  ERRBACK handles failure and COUNT limits
the requested page size."
  (let* ((kind (qq-state-session-key-type session-key))
         (frontier (qq-native-history-frontier session-key))
         (sequence (plist-get frontier :sequence)))
    (cond
     ((eq kind 'private)
      (qq-native-fetch-private-history-page
       session-key nil callback errback count
       (list :history-at-latest-p t)))
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

(defconst qq-native--capability-methods
  '((recent-conversations "conversation.list_recent")
    (contacts "contact.list_friends" "contact.list_groups")
    (avatar "contact.get_user_avatar")
    (group-members "contact.list_group_members")
    (group-settings "group.set_name" "group.set_remark"
                    "group.set_whole_mute" "group.set_pinned")
    (group-member-settings "group.set_member_card"
                           "group.set_member_special_title")
    (group-moderation "group.kick_member")
    (group-clock-in "group.clock_in")
    (group-at-all-quota "group.get_at_all_remaining")
    (group-lifecycle "group.leave")
    (presence "account.set_presence")
    (send-text "message.send")
    (send-message "message.send")
    (face "message.send")
    (reply "message.send")
    (mention "message.send")
    (poke "message.poke")
    (recall "message.recall")
    (explicit-history "message.get_history" "message.get_private_history")
    (read-receipt "message.mark_read"))
  "Product capabilities and every required negotiated Gateway method.")

(defun qq-native-implemented-p (capability)
  "Return non-nil when this client implements product CAPABILITY."
  (and (assq capability qq-native--capability-methods) t))

(defun qq-native-supports-p (capability)
  "Return non-nil when negotiated methods support product CAPABILITY."
  (when-let* ((spec (assq capability qq-native--capability-methods)))
    (let ((methods (qq-gateway-transport-capabilities)))
      (cl-every (lambda (method) (member method methods)) (cdr spec)))))

(defun qq-native-message-read-capable-p (message)
  "Return non-nil when MESSAGE is a current, stable read reference."
  (let ((account (qq-gateway-current-account)))
    (and (qq-native-ready-p)
         (qq-native-supports-p 'read-receipt)
         (equal (alist-get 'phase account) "online")
         (qq-gateway-message-read-capable-p message))))

(defun qq-native-presence-capable-p ()
  "Return non-nil when the selected account can accept presence changes."
  (and (qq-native-ready-p)
       (qq-native-supports-p 'presence)
       (let ((account (qq-gateway-current-account)))
         (and account
              (equal (alist-get 'phase account) "online")))))

(defun qq-native--bootstrap-complete (token failed-p)
  "Complete one bootstrap part owned by opaque TOKEN.

FAILED-P revokes the whole bootstrap context so a later typed account change
may retry it."
  (when (eq token qq-native--bootstrap-token)
    (if failed-p
        (setq qq-native--bootstrap-instance-id nil
              qq-native--bootstrap-owner nil
              qq-native--bootstrap-pending 0
              qq-native--bootstrap-token nil)
      (setq qq-native--bootstrap-pending
            (max 0 (1- qq-native--bootstrap-pending))))
    t))

(defun qq-native--bootstrap-success (token _value)
  "Record one successful bootstrap part owned by TOKEN."
  (qq-native--bootstrap-complete token nil))

(defun qq-native--bootstrap-failure (token _body reason)
  "Record failed bootstrap TOKEN and report REASON."
  (when (qq-native--bootstrap-complete token t)
    (qq-native--default-error nil reason)))

(defun qq-native--invalidate-directory-bootstrap ()
  "Invalidate Native Session-sensitive observations without blanking state."
  (setq qq-native--bootstrap-instance-id nil
        qq-native--bootstrap-owner nil
        qq-native--bootstrap-pending 0
        qq-native--bootstrap-token nil
        qq-native--friend-directory-owner nil
        qq-native--group-directory-owner nil))

(defun qq-native--observe-account-context (&optional force)
  "Observe the account phase and invalidate session-sensitive caches when needed.

FORCE marks a typed Gateway ready boundary.  Otherwise stable account identity
changes and transitions into or out of `online' are the lifecycle boundaries;
no public Native Session identity or counter is required."
  (let* ((account (qq-gateway-current-account))
         (account-id (and account (alist-get 'account_id account)))
         (phase (and account (alist-get 'phase account)))
         (slot-changed-p
          (not (equal account-id qq-native--observed-account-id)))
         (online-boundary-p
          (and (not slot-changed-p)
               (not (equal phase qq-native--observed-account-phase))
               (or (equal phase "online")
                   (equal qq-native--observed-account-phase "online")))))
    (when (or force slot-changed-p online-boundary-p)
      (qq-native--invalidate-directory-bootstrap))
    (setq qq-native--observed-account-id
          (and account-id (copy-sequence account-id))
          qq-native--observed-account-phase
          (and phase (copy-sequence phase)))))

(defun qq-native--maybe-bootstrap (&rest _arguments)
  "Load recent state and live directories for the selected account slot."
  (qq-native--observe-account-context)
  (when (qq-gateway-transport-ready-p)
    (let* ((account (qq-gateway-current-account))
           (owner (qq-gateway-current-account-id))
           (instance-id (qq-gateway-transport-gateway-instance-id))
           (online-p (equal (alist-get 'phase account) "online"))
           (recent-needed (qq-native-supports-p 'recent-conversations))
           (contacts-p (and online-p (qq-native-supports-p 'contacts)))
           (friends-current-p
            (equal owner qq-native--friend-directory-owner))
           (groups-current-p
            (equal owner qq-native--group-directory-owner))
           (friends-needed
            (and contacts-p
                 (or (not (qq-state-friend-categories-loaded-p))
                     (not friends-current-p))))
           (groups-needed
            (and contacts-p
                 (or (not (qq-state-groups-loaded-p))
                     (not groups-current-p))))
           ;; Preserve the old projection while asking the newly-online
           ;; Native Session to rebuild its source cache.
           (friends-refresh
            (and friends-needed
                 (qq-state-friend-categories-loaded-p)
                 (not friends-current-p)))
           (groups-refresh
            (and groups-needed
                 (qq-state-groups-loaded-p)
                 (not groups-current-p)))
           (part-count (+ (if recent-needed 1 0)
                          (if friends-needed 1 0)
                          (if groups-needed 1 0))))
      (when (and owner instance-id
                 (> part-count 0)
                 (not (and (equal owner qq-native--bootstrap-owner)
                           (equal instance-id
                                  qq-native--bootstrap-instance-id))))
        (let ((token (list 'qq-native-bootstrap)))
          (setq qq-native--bootstrap-instance-id (copy-sequence instance-id)
                qq-native--bootstrap-owner (copy-sequence owner)
                qq-native--bootstrap-pending part-count
                qq-native--bootstrap-token token)
          (when recent-needed
            (qq-native-refresh-recent-conversations
             (apply-partially #'qq-native--bootstrap-success token)
             (apply-partially #'qq-native--bootstrap-failure token)))
          (when friends-needed
            (qq-native-refresh-friend-categories
             (apply-partially #'qq-native--bootstrap-success token)
             (apply-partially #'qq-native--bootstrap-failure token)
             friends-refresh))
          (when groups-needed
            (qq-native-refresh-joined-groups
             (apply-partially #'qq-native--bootstrap-success token)
             (apply-partially #'qq-native--bootstrap-failure token)
             groups-refresh)))))))

(defun qq-native--handle-ready (_instance-id)
  "Start a fresh selected-slot bootstrap after typed Gateway ready."
  ;; Account replacement publishes before this hook.  Discard the prior
  ;; connection's completed context even when the service instance survived,
  ;; because transient conversation events may have arrived while detached.
  (setq qq-native--recent-resync-context nil)
  (qq-native--observe-account-context t)
  (qq-native--maybe-bootstrap))

(defun qq-native--handle-account-registry-change (reason _account-id)
  "Bootstrap after account registry REASON other than typed ready."
  (unless (eq reason 'ready)
    (qq-native--maybe-bootstrap)))

(defun qq-native--recent-resync-complete (context failed-p &rest arguments)
  "Release lag resync CONTEXT; report failure from ARGUMENTS when FAILED-P."
  (when (equal context qq-native--recent-resync-context)
    (setq qq-native--recent-resync-context nil))
  (when failed-p
    (qq-native--default-error nil (cadr arguments))))

(defun qq-native--handle-desync (&rest _arguments)
  "Coalesce a recent-conversation resync after transient event loss."
  (when (and (qq-gateway-transport-ready-p)
             (qq-native-supports-p 'recent-conversations))
    (when-let* ((account-id (qq-gateway-current-account-id))
                (instance-id (qq-gateway-transport-gateway-instance-id)))
      (let ((context (list instance-id account-id)))
        (unless (equal context qq-native--recent-resync-context)
          ;; Publish ownership before starting because a preflight failure
          ;; reports through the error callback before the starter returns.
          (setq qq-native--recent-resync-context (copy-tree context))
          (qq-native-refresh-recent-conversations
           (apply-partially
            #'qq-native--recent-resync-complete context nil)
           (apply-partially
            #'qq-native--recent-resync-complete context t)))))))

(defun qq-native-activate ()
  "Make the selected native account own shared client projection state."
  (qq-gateway-message-activate-projection)
  (qq-native--maybe-bootstrap)
  t)

(defun qq-native-reset-session-state ()
  "Revoke account projection and request caches."
  (setq qq-native--bootstrap-instance-id nil
        qq-native--bootstrap-owner nil
        qq-native--bootstrap-pending 0
        qq-native--bootstrap-token nil
        qq-native--friend-directory-owner nil
        qq-native--group-directory-owner nil
        qq-native--observed-account-id nil
        qq-native--observed-account-phase nil
        qq-native--recent-resync-context nil
        qq-native--recent-request nil)
  (qq-native--revoke-read-operations)
  (qq-native-request-revoke-all)
  (qq-gateway-attachment-reset)
  (qq-gateway-media-reset)
  (qq-gateway-resource-reset)
  (qq-gateway-directory-reset)
  (qq-gateway-message-revoke-projection))

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-native--revoke-read-operations)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-native--revoke-stale-read-operations)
(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-native-request-revoke-stale)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-native-request-revoke-stale)
(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-native--maybe-bootstrap t)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-native--handle-account-registry-change t)
(add-hook 'qq-gateway-ready-hook #'qq-native--handle-ready t)
(add-hook 'qq-gateway-desync-hook #'qq-native--handle-desync t)

(provide 'qq-native)

;;; qq-native.el ends here
