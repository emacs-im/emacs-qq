;;; qq-gateway-directory.el --- Native Gateway contact projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Strict selected-account adapters for the Gateway friend, group, and
;; group-member methods.  Only the selected stable account slot may replace
;; shared QQ directory state.  Group-member pages are native cache data and
;; also enrich exact UID/UIN routing knowledge.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-gateway-message)
(require 'qq-gateway-rpc)
(require 'qq-state)

(defvar qq-gateway-directory--active-requests
  (make-hash-table :test #'equal)
  "Newest owned directory request record for each projected resource.")
(defvar qq-gateway-directory--member-pages
  (make-hash-table :test #'equal)
  "Selected-account group-member pages keyed by exact group UIN.")
(defvar qq-gateway-directory--cache-owner nil
  "Stable account ID owning native directory caches.")
(defvar qq-gateway-directory--observed-account-id nil
  "Selected account whose phase is recorded for cache invalidation.")
(defvar qq-gateway-directory--observed-account-phase nil
  "Last observed phase of `qq-gateway-directory--observed-account-id'.")

(cl-defstruct (qq-gateway-directory--request-record
               (:constructor qq-gateway-directory--request-record-create))
  "Exactly-once lifecycle for one newest-owned directory resource request."
  resource
  owner
  (state 'active)
  transport-token
  errback)

(defun qq-gateway-directory--assert-unique (items key context)
  "Return ITEMS after asserting unique KEY values in CONTEXT."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (item items)
      (let ((value (alist-get key item)))
        (when (gethash value seen)
          (error "qq: Gateway %s duplicates %s %s" context key value))
        (puthash value t seen))))
  items)

(defun qq-gateway-directory--check-friend-relationships (result)
  "Return RESULT after checking relations owned by the friend projection."
  (let ((categories (alist-get 'categories result))
        (friends (alist-get 'friends result))
        (category-ids (make-hash-table :test #'equal)))
    (qq-gateway-directory--assert-unique
     categories 'id "friend categories")
    (dolist (category categories)
      (puthash (alist-get 'id category) t category-ids))
    (qq-gateway-directory--assert-unique friends 'uid "friend list")
    (qq-gateway-directory--assert-unique friends 'uin "friend list")
    (dolist (friend friends)
      (unless (gethash (alist-get 'category_id friend) category-ids)
        (error "qq: Gateway friend belongs to an unknown category"))))
  result)

(defun qq-gateway-directory--friend-to-state (friend category-name)
  "Project native FRIEND with CATEGORY-NAME to shared directory shape."
  `((user_id . ,(alist-get 'uin friend))
    (uid . ,(alist-get 'uid friend))
    (nickname . ,(alist-get 'nickname friend))
    (remark . ,(alist-get 'remark friend))
    (category_id . ,(alist-get 'category_id friend))
    (category_name . ,category-name)
    (personal_sign . ,(alist-get 'personal_sign friend))
    (qid . ,(alist-get 'qid friend))
    (age . ,(alist-get 'age friend))
    (gender . ,(alist-get 'gender friend))))

(defun qq-gateway-directory--friends-to-state (result)
  "Convert friend-list RESULT to ordered shared categories."
  (let ((friends (alist-get 'friends result)))
    (mapcar
     (lambda (category)
       (let* ((category-id (alist-get 'id category))
              (category-name (alist-get 'name category))
              (members
               (delq
                nil
                (mapcar
                 (lambda (friend)
                   (when (= (alist-get 'category_id friend) category-id)
                     (qq-gateway-directory--friend-to-state
                      friend category-name)))
                 friends))))
         `((category_id . ,category-id)
           (sort_id . ,(alist-get 'sort_id category))
           (name . ,category-name)
           (member_count . ,(alist-get 'member_count category))
           (friends . ,members))))
     (alist-get 'categories result))))

(defun qq-gateway-directory--preflight-peer-identities (owner identities)
  "Preflight UID/UIN IDENTITIES for projected account OWNER."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (identity identities)
      (let ((uid (car identity))
            (uin (cdr identity)))
        (when-let* ((page-uin (gethash uid seen)))
          (unless (equal page-uin uin)
            (error "qq: Gateway directory maps one UID to multiple UINs")))
        (puthash uid uin seen)
        (qq-gateway-message--validate-peer-identity owner uid uin))))
  identities)

(defun qq-gateway-directory--project-friends (result owner)
  "Project friend-list RESULT for OWNER into shared state."
  (qq-gateway-directory--check-friend-relationships result)
  (let* ((friends (alist-get 'friends result))
         (identities
          (mapcar (lambda (friend)
                    (cons (alist-get 'uid friend) (alist-get 'uin friend)))
                  friends))
         (categories (qq-gateway-directory--friends-to-state result)))
    (qq-gateway-directory--preflight-peer-identities owner identities)
    (qq-state-apply-friend-categories categories)
    (dolist (identity identities)
      (qq-gateway-message--remember-peer-identity
       owner (car identity) (cdr identity)))
    categories))

(defun qq-gateway-directory--group-to-state (group account)
  "Project native GROUP using selected ACCOUNT identity."
  `((group_id . ,(alist-get 'group_uin group))
    (group_name . ,(alist-get 'name group))
    (group_remark . ,(alist-get 'remark group))
    (owner_uid . ,(alist-get 'owner_uid group))
    (member_count . ,(alist-get 'member_count group))
    (max_member_count . ,(alist-get 'member_max group))
    (created_at . ,(alist-get 'created_time group))
    (description . ,(alist-get 'description group))
    (question . ,(alist-get 'question group))
    (announcement . ,(alist-get 'announcement group))
    (last_speak_time . ,(alist-get 'last_speak_time group))
    (latest_sequence . ,(and (alist-get 'latest_sequence group)
                             (format "%s" (alist-get 'latest_sequence group))))
    (self_permission
     . ,(and (alist-get 'owner_uid group)
             (equal (alist-get 'owner_uid group) (alist-get 'uid account))
             "owner"))))

(defun qq-gateway-directory--project-groups (result _owner)
  "Project joined-group RESULT into shared state."
  (let ((account (qq-gateway-current-account))
        groups)
    (qq-gateway-directory--assert-unique
     (alist-get 'groups result) 'group_uin "group list")
    (setq groups
          (mapcar (lambda (group)
                    (qq-gateway-directory--group-to-state group account))
                  (alist-get 'groups result)))
    (qq-state-apply-groups groups)
    groups))

(defun qq-gateway-directory--permission-role (permission)
  "Return UI role for exact native PERMISSION, or nil when unknown."
  (pcase (alist-get 'kind permission)
    ((or "member" "owner" "admin") (alist-get 'kind permission))))

(defun qq-gateway-directory--member-to-state (member group-uin group-name)
  "Project native MEMBER for GROUP-UIN and GROUP-NAME."
  `((user_id . ,(alist-get 'uin member))
    (uid . ,(alist-get 'uid member))
    (nickname . ,(alist-get 'nickname member))
    (card . ,(alist-get 'member_card member))
    (remark)
    (qid)
    (title . ,(alist-get 'special_title member))
    (role . ,(qq-gateway-directory--permission-role
              (alist-get 'permission member)))
    (robot)
    (group_id . ,group-uin)
    (group_name . ,group-name)
    (level . ,(alist-get 'level member))
    (join_timestamp . ,(alist-get 'join_timestamp member))
    (last_message_timestamp . ,(alist-get 'last_message_timestamp member))
    (shut_up_timestamp . ,(alist-get 'shut_up_timestamp member))
    (native_permission
     . ,(qq-gateway-value-copy (alist-get 'permission member)))
    (is_friend . ,(and (qq-state-friend-categories-loaded-p)
                       (qq-state-friend (alist-get 'uin member))
                       t))))

(defun qq-gateway-directory--enrich-group-from-members
    (group-uin raw-members)
  "Enrich cached GROUP-UIN with exact ownership from RAW-MEMBERS."
  (when-let* ((group (qq-state-group group-uin)))
    (let* ((self-uin (alist-get 'uin (qq-gateway-current-account)))
           (owner
            (seq-find
             (lambda (member)
               (equal (alist-get 'kind (alist-get 'permission member))
                      "owner"))
             raw-members))
           (self
            (seq-find (lambda (member)
                        (equal (alist-get 'uin member) self-uin))
                      raw-members))
           (self-role
            (and self
                 (qq-gateway-directory--permission-role
                  (alist-get 'permission self)))))
      (when owner
        (setf (alist-get 'owner_id group nil nil #'eq)
              (alist-get 'uin owner))
        (setf (alist-get 'owner_uid group nil nil #'eq)
              (alist-get 'uid owner)))
      (when self-role
        (setf (alist-get 'self_permission group nil nil #'eq) self-role))
      (qq-state-apply-groups
       (mapcar (lambda (candidate)
                 (if (equal (alist-get 'group_id candidate) group-uin)
                     group
                   candidate))
               (qq-state-groups))))))

(defun qq-gateway-directory--project-members (result owner group-uin)
  "Project GROUP-UIN member RESULT for OWNER and return mapped members."
  (let* ((group (qq-state-group group-uin))
         (group-name (and group (alist-get 'group_name group)))
         (raw-members (alist-get 'members result))
         (identities
          (mapcar (lambda (member)
                    (cons (alist-get 'uid member) (alist-get 'uin member)))
                  raw-members))
         (members
          (mapcar (lambda (member)
                    (qq-gateway-directory--member-to-state
                     member group-uin group-name))
                  raw-members))
         (page
          `((group_id . ,group-uin)
            (members . ,members)
            (member_count . ,(alist-get 'member_count result))
            (member_list_change_sequence
             . ,(alist-get 'member_list_change_sequence result))
            (member_card_sequence
             . ,(alist-get 'member_card_sequence result)))))
    (qq-gateway-directory--assert-unique
     raw-members 'uid "group-member list")
    (qq-gateway-directory--assert-unique
     raw-members 'uin "group-member list")
    (qq-gateway-directory--preflight-peer-identities owner identities)
    (dolist (identity identities)
      (qq-gateway-message--remember-peer-identity
       owner (car identity) (cdr identity)))
    (puthash group-uin (qq-gateway-value-copy page)
             qq-gateway-directory--member-pages)
    (qq-gateway-directory--enrich-group-from-members group-uin raw-members)
    members))

(defun qq-gateway-directory--request-active-p (request)
  "Return non-nil when directory REQUEST still owns asynchronous work."
  (and (qq-gateway-directory--request-record-p request)
       (eq (qq-gateway-directory--request-record-state request) 'active)))

(defun qq-gateway-directory--request-current-p (request)
  "Return non-nil when REQUEST still owns its resource and account slot."
  (and (qq-gateway-directory--request-active-p request)
       (eq request
           (gethash (qq-gateway-directory--request-record-resource request)
                    qq-gateway-directory--active-requests))
       (equal (qq-gateway-directory--request-record-owner request)
              qq-gateway-directory--cache-owner)
       (equal (qq-gateway-directory--request-record-owner request)
              (qq-gateway-current-account-id))))

(defun qq-gateway-directory--finish-request (request state)
  "Move active directory REQUEST to terminal STATE exactly once."
  (when (qq-gateway-directory--request-active-p request)
    (setf (qq-gateway-directory--request-record-state request) state
          (qq-gateway-directory--request-record-transport-token request) nil)
    (let ((resource
           (qq-gateway-directory--request-record-resource request)))
      (when (eq request
                (gethash resource qq-gateway-directory--active-requests))
        (remhash resource qq-gateway-directory--active-requests)))
    t))

(defun qq-gateway-directory--cancel-transport (token)
  "Best-effort cancel directory transport TOKEN."
  (when token
    (condition-case error-data
        (qq-gateway-transport-cancel token)
      (error
       (message "qq: directory request cancellation failed: %s"
                (error-message-string error-data))))))

(defun qq-gateway-directory--cancel-request
    (request &optional code message)
  "Cancel active REQUEST and optionally report client CODE and MESSAGE."
  (when (qq-gateway-directory--request-active-p request)
    (let ((token
           (qq-gateway-directory--request-record-transport-token request))
          (errback (qq-gateway-directory--request-record-errback request)))
      ;; Revoke before transport or user code: both may synchronously reenter.
      (qq-gateway-directory--finish-request request 'cancelled)
      (qq-gateway-directory--cancel-transport token)
      (when code
        (qq-gateway-rpc-client-error
         errback code "%s" (or message "Gateway directory request cancelled")))
      t)))

(defun qq-gateway-directory--cancel-resource
    (resource &optional code message)
  "Cancel the current request for RESOURCE."
  (let ((request (gethash resource qq-gateway-directory--active-requests)))
    (when (qq-gateway-directory--request-record-p request)
      (qq-gateway-directory--cancel-request request code message))))

(defun qq-gateway-directory--cancel-all-requests
    (&optional code message)
  "Cancel all active directory requests, optionally reporting CODE and MESSAGE."
  (let (requests)
    (maphash (lambda (_resource request) (push request requests))
             qq-gateway-directory--active-requests)
    ;; Publish revocation before cancellation callbacks can start replacements.
    (clrhash qq-gateway-directory--active-requests)
    (dolist (request requests)
      (when (qq-gateway-directory--request-record-p request)
        (qq-gateway-directory--cancel-request request code message)))))

(defun qq-gateway-directory--set-cache-owner
    (owner &optional code message)
  "Make stable OWNER own directory caches, cancelling the previous owner."
  (unless (equal owner qq-gateway-directory--cache-owner)
    ;; Install the new owner before cancellation can reenter.
    (setq qq-gateway-directory--cache-owner
          (and owner (copy-sequence owner)))
    (qq-gateway-directory--cancel-all-requests
     (or code "account_selection_changed")
     (or message "Selected QQ account changed during directory request"))
    (clrhash qq-gateway-directory--member-pages))
  owner)

(defun qq-gateway-directory--invalidate-native-session-cache (code message)
  "Cancel Native Session-sensitive work with CODE and MESSAGE.
Preserve stable account directories."
  (qq-gateway-directory--cancel-all-requests code message)
  (clrhash qq-gateway-directory--member-pages))

(defun qq-gateway-directory-reset ()
  "Revoke native directory request ownership and member caches."
  (setq qq-gateway-directory--cache-owner nil
        qq-gateway-directory--observed-account-id nil
        qq-gateway-directory--observed-account-phase nil)
  (qq-gateway-directory--cancel-all-requests
   "gateway_reset" "Gateway directory state was reset")
  (clrhash qq-gateway-directory--member-pages)
  nil)

(defun qq-gateway-directory--synchronize-account-context (reason)
  "Synchronize directory ownership and lifecycle caches for registry REASON."
  (let* ((account (qq-gateway-current-account))
         (account-id (and account (alist-get 'account_id account)))
         (phase (and account (alist-get 'phase account)))
         (slot-changed-p
          (not (equal account-id
                      qq-gateway-directory--observed-account-id)))
         (online-boundary-p
          (and (not slot-changed-p)
               (not (equal phase
                           qq-gateway-directory--observed-account-phase))
               (or (equal phase "online")
                   (equal qq-gateway-directory--observed-account-phase
                          "online")))))
    (cond
     ((not (equal account-id qq-gateway-directory--cache-owner))
      (qq-gateway-directory--set-cache-owner account-id))
     ((or (eq reason 'ready) slot-changed-p online-boundary-p)
      (qq-gateway-directory--invalidate-native-session-cache
       (if (eq reason 'ready) "gateway_resynchronized"
         "native_session_changed")
       (if (eq reason 'ready)
           "Gateway directory state was resynchronized"
         "QQ Native Session changed during directory request"))))
    (setq qq-gateway-directory--observed-account-id
          (and account-id (copy-sequence account-id))
          qq-gateway-directory--observed-account-phase
          (and phase (copy-sequence phase)))))

(defun qq-gateway-directory--handle-account-selection (&rest _arguments)
  "Revoke directory caches after a selected account change."
  (qq-gateway-directory--synchronize-account-context 'selection))

(defun qq-gateway-directory--handle-account-registry-change
    (reason _account-id)
  "Refresh directory lifecycle observations after account registry REASON."
  (qq-gateway-directory--synchronize-account-context reason))

(defun qq-gateway-directory--begin-request (resource owner errback)
  "Publish and return the newest request record for RESOURCE and OWNER."
  (let* ((previous
          (gethash resource qq-gateway-directory--active-requests))
         (request
          (qq-gateway-directory--request-record-create
           :resource resource :owner (copy-sequence owner)
           :errback errback)))
    ;; Publish first so a predecessor's errback sees its replacement.
    (puthash resource request qq-gateway-directory--active-requests)
    (when (qq-gateway-directory--request-record-p previous)
      (qq-gateway-directory--cancel-request
       previous "superseded_request"
       "Gateway directory request was superseded"))
    request))

(defun qq-gateway-directory--request
    (resource method params projector callback errback)
  "Run one newest-owned directory RESOURCE request using native METHOD.

PARAMS are sent as-is.  PROJECTOR receives the owned domain result and exact
account owner.  CALLBACK receives the projected value; ERRBACK follows
Gateway error conventions."
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner))
         (_cache (qq-gateway-directory--set-cache-owner owner))
         (request
          (qq-gateway-directory--begin-request resource owner errback)))
    (condition-case error-data
        (when (qq-gateway-directory--request-current-p request)
          (let ((token
                 (qq-gateway-rpc-call
                  method (append `((account_id . ,owner)) params)
                  :current-p
                  (lambda ()
                    (qq-gateway-directory--request-current-p request))
                  :stale-code "superseded_request"
                  :stale-message
                  "Gateway directory request was superseded or changed owner"
                  :projector
                  (lambda (result) (funcall projector result owner))
                  :callback
                  (lambda (value)
                    (when (qq-gateway-directory--finish-request
                           request 'settled)
                      (qq-gateway--invoke callback value)))
                  :errback
                  (lambda (body reason)
                    (when (qq-gateway-directory--finish-request
                           request 'failed)
                      (qq-gateway--invoke errback body reason))))))
            (when (and token
                       (qq-gateway-directory--request-current-p request))
              (setf
               (qq-gateway-directory--request-record-transport-token request)
               token))
            token))
      ((error quit)
       (qq-gateway-directory--finish-request request 'failed)
       (signal (car error-data) (cdr error-data))))))

(defun qq-gateway-directory-refresh-friends
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with ordered categories.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its contact cache."
  (interactive
   (list (lambda (categories)
           (message "qq: loaded %d Gateway friend categories"
                    (length categories)))
         (lambda (_body reason)
           (message "qq: Gateway friend refresh failed: %s" reason))
         (and current-prefix-arg t)))
  (qq-gateway-directory--request
   'friends "contact.list_friends"
   `((refresh . ,(if refresh t :false)))
   #'qq-gateway-directory--project-friends
   callback errback))

(defun qq-gateway-directory-refresh-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with mapped groups.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its contact cache."
  (interactive
   (list (lambda (groups)
           (message "qq: loaded %d Gateway groups" (length groups)))
         (lambda (_body reason)
           (message "qq: Gateway group refresh failed: %s" reason))
         (and current-prefix-arg t)))
  (qq-gateway-directory--request
   'groups "contact.list_groups"
   `((refresh . ,(if refresh t :false)))
   #'qq-gateway-directory--project-groups
   callback errback))

(defun qq-gateway-directory-set-friend-pinned
    (friend-uin pinned &optional callback errback)
  "Set FRIEND-UIN's conversation PINNED state through the native service."
  (unless (qq-gateway--canonical-decimal-p friend-uin)
    (user-error "qq: Friend pinned state requires an exact decimal UIN"))
  (setq pinned (if pinned t :false))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner)))
    (qq-gateway-rpc-call
     "friend.set_pinned"
     `((account_id . ,owner)
       (friend_uin . ,friend-uin)
       (pinned . ,pinned))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during friend setting"
     :callback callback
     :errback errback)))

(defun qq-gateway-directory--set-group-setting
    (method group-uin field value callback errback)
  "Send group setting METHOD with FIELD and VALUE for GROUP-UIN."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group setting requires an exact decimal group UIN"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner)))
    (qq-gateway-rpc-call
     method
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (,field . ,value))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during group setting"
     :callback callback
     :errback errback)))

(defun qq-gateway-directory-set-group-name
    (group-uin name &optional callback errback)
  "Set GROUP-UIN's public NAME through the native service."
  (unless (and (stringp name) (not (string-empty-p name)))
    (user-error "qq: Group name must be a non-empty string"))
  (qq-gateway-directory--set-group-setting
   "group.set_name" group-uin 'name name callback errback))

(defun qq-gateway-directory-set-group-remark
    (group-uin remark &optional callback errback)
  "Set or clear GROUP-UIN's account-local REMARK through the Gateway."
  (unless (stringp remark)
    (user-error "qq: Group remark must be a string"))
  (qq-gateway-directory--set-group-setting
   "group.set_remark" group-uin 'remark remark callback errback))

(defun qq-gateway-directory-set-group-whole-mute
    (group-uin enabled &optional callback errback)
  "Set GROUP-UIN's whole-group mute state through the native service."
  (setq enabled (and enabled t))
  (qq-gateway-directory--set-group-setting
   "group.set_whole_mute" group-uin 'enabled
   (if enabled t :false) callback errback))

(defun qq-gateway-directory-set-group-pinned
    (group-uin pinned &optional callback errback)
  "Set GROUP-UIN's conversation PINNED state through the Gateway."
  (setq pinned (and pinned t))
  (qq-gateway-directory--set-group-setting
   "group.set_pinned" group-uin 'pinned
   (if pinned t :false) callback errback))

(defun qq-gateway-directory-clock-in-group
    (group-uin &optional callback errback)
  "Clock the selected QQ account into exact GROUP-UIN."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group clock-in requires an exact group UIN"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner)))
    (qq-gateway-rpc-call
     "group.clock_in"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during group clock-in"
     :callback callback
     :errback errback)))

(defun qq-gateway-directory-get-group-at-all-remaining
    (group-uin &optional callback errback)
  "Fetch live @all availability and quotas for exact GROUP-UIN."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group @all quota requires an exact group UIN"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner)))
    (qq-gateway-rpc-call
     "group.get_at_all_remaining"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during group @all query"
     :callback callback
     :errback errback)))

(defun qq-gateway-directory-leave-group
    (group-uin &optional callback errback)
  "Leave exact GROUP-UIN through the selected native QQ account.

This method cannot dismiss a group.  A successful receipt revokes cached
member data and any older group or member request that could reintroduce the
departed group."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group leave requires an exact group UIN"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner))
         (_cache (qq-gateway-directory--set-cache-owner owner)))
    (qq-gateway-rpc-call
     "group.leave"
     `((account_id . ,owner)
       (group_uin . ,group-uin))
     :current-p
     (lambda ()
       (and (equal owner (qq-gateway-current-account-id))
            (equal owner qq-gateway-directory--cache-owner)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during group leave"
     :projector
     (lambda (receipt)
       (qq-gateway-directory--cancel-resource
        'groups "superseded_request"
        "Gateway group list was invalidated by leaving a group")
       (qq-gateway-directory--cancel-resource
        (cons 'group-members group-uin) "superseded_request"
        "Gateway group-member list was invalidated by leaving the group")
       (remhash group-uin qq-gateway-directory--member-pages)
       receipt)
     :callback callback
     :errback errback)))

(defun qq-gateway-directory--apply-group-member-setting
    (group-uin target-uin field value)
  "Apply confirmed FIELD VALUE to cached TARGET-UIN in GROUP-UIN.

The directory never creates an incomplete page or member from a mutation
receipt."
  (when-let* ((page (gethash group-uin
                             qq-gateway-directory--member-pages))
              (member
               (seq-find
                (lambda (candidate)
                  (equal (alist-get 'user_id candidate) target-uin))
                (alist-get 'members page))))
    (setf (alist-get field member nil nil #'eq)
          (and (not (string-empty-p value)) value))))

(defun qq-gateway-directory--set-group-member-setting
    (method group-uin target-uin field cache-field value callback errback)
  "Send group-member setting METHOD and update its projected CACHE-FIELD."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group-member setting requires an exact group UIN"))
  (unless (qq-gateway--canonical-decimal-p target-uin)
    (user-error "qq: Group-member setting requires an exact target UIN"))
  (unless (stringp value)
    (user-error "qq: Group-member setting value must be a string"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner)))
    (qq-gateway-rpc-call
     method
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (target_uin . ,target-uin)
       (,field . ,value))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message
     "Selected QQ account changed during group-member setting"
     :projector
     (lambda (receipt)
       (qq-gateway-directory--apply-group-member-setting
        group-uin target-uin cache-field value)
       receipt)
     :callback callback
     :errback errback)))

(defun qq-gateway-directory-set-group-member-card
    (group-uin target-uin card &optional callback errback)
  "Set or clear TARGET-UIN's CARD in GROUP-UIN through the Gateway."
  (qq-gateway-directory--set-group-member-setting
   "group.set_member_card" group-uin target-uin 'card 'card card
   callback errback))

(defun qq-gateway-directory-set-group-member-special-title
    (group-uin target-uin special-title &optional callback errback)
  "Set or clear TARGET-UIN's SPECIAL-TITLE in GROUP-UIN through Gateway."
  (qq-gateway-directory--set-group-member-setting
   "group.set_member_special_title" group-uin target-uin
   'special_title 'title special-title callback errback))

(defun qq-gateway-directory--remove-group-member (group-uin target-uin)
  "Remove cached TARGET-UIN from GROUP-UIN exactly once.

Return non-nil only when an existing member was removed.  An absent page or
member is left untouched, and no incomplete directory state is invented."
  (when-let* ((page (gethash group-uin
                             qq-gateway-directory--member-pages))
              (members (alist-get 'members page))
              (member
               (seq-find
                (lambda (candidate)
                  (equal (alist-get 'user_id candidate) target-uin))
                members)))
    (setf (alist-get 'members page nil nil #'eq) (delq member members))
    (setf (alist-get 'member_count page nil nil #'eq)
          (max 0 (1- (alist-get 'member_count page))))
    t))

(defun qq-gateway-directory-kick-group-member
    (group-uin target-uin reject-add-request &optional callback errback)
  "Remove TARGET-UIN from GROUP-UIN through the native service."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group kick requires an exact group UIN"))
  (unless (qq-gateway--canonical-decimal-p target-uin)
    (user-error "qq: Group kick requires an exact target UIN"))
  (let* ((owner (or (qq-gateway-current-account-id)
                    (user-error "qq: Select a QQ account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner))
         (wire-reject (if reject-add-request t :false)))
    (qq-gateway-rpc-call
     "group.kick_member"
     `((account_id . ,owner)
       (group_uin . ,group-uin)
       (target_uin . ,target-uin)
       (reject_add_request . ,wire-reject))
     :current-p (lambda () (equal owner (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during group-member kick"
     :projector
     (lambda (receipt)
       (qq-gateway-directory--remove-group-member group-uin target-uin)
       receipt)
     :callback callback
     :errback errback)))

(defun qq-gateway-directory-list-group-members
    (group-uin callback &optional errback refresh)
  "List exact GROUP-UIN members and call CALLBACK with mapped members.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Native Session to replace its member cache."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group UIN must be an exact decimal string"))
  (qq-gateway-directory--request
   (cons 'group-members group-uin) "contact.list_group_members"
   `((group_uin . ,group-uin)
     (refresh . ,(if refresh t :false)))
   (lambda (result owner)
     (qq-gateway-directory--project-members result owner group-uin))
   callback errback))

(defun qq-gateway-directory-group-member-page (group-uin)
  "Return the selected account's cached member page for exact GROUP-UIN."
  (qq-gateway-value-copy
   (gethash group-uin qq-gateway-directory--member-pages)))

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-gateway-directory--handle-account-selection)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-gateway-directory--handle-account-registry-change)

(provide 'qq-gateway-directory)

;;; qq-gateway-directory.el ends here
