;;; qq-gateway-directory.el --- Native Gateway contact projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Strict account/generation-scoped adapters for the Gateway friend, group,
;; and group-member methods.  Only the selected account may replace shared QQ
;; directory state.  Group-member pages remain generation-owned native cache
;; data and also enrich exact UID/UIN routing knowledge.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-gateway-message)
(require 'qq-state)

(defvar qq-gateway-directory--request-counter 0)
(defvar qq-gateway-directory--active-requests
  (make-hash-table :test #'equal)
  "Newest directory request token for each projected resource.")
(defvar qq-gateway-directory--member-pages
  (make-hash-table :test #'equal)
  "Selected-generation group-member pages keyed by exact group UIN.")
(defvar qq-gateway-directory--cache-owner nil
  "Exact `(ACCOUNT-ID . GENERATION)' owning native directory caches.")

(defun qq-gateway-directory--uint32-p (value)
  "Return non-nil when VALUE is an unsigned 32-bit integer."
  (and (integerp value) (<= 0 value) (<= value #xffffffff)))

(defun qq-gateway-directory--int32-p (value)
  "Return non-nil when VALUE is a signed 32-bit integer."
  (and (integerp value) (<= (- #x80000000) value) (< value #x80000000)))

(defun qq-gateway-directory--uint64-p (value)
  "Return non-nil when VALUE is an unsigned 64-bit integer."
  (and (integerp value)
       (<= 0 value)
       (<= value #xffffffffffffffff)))

(defun qq-gateway-directory--optional-string-p (value)
  "Return non-nil when VALUE is nil or a string."
  (or (null value) (stringp value)))

(defun qq-gateway-directory--validate-owner (result owner keys context)
  "Validate RESULT ownership and exact KEYS for OWNER in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p result keys)
    (error "qq: Gateway %s result has invalid fields" context))
  (unless (and (equal (alist-get 'account_id result) (car owner))
               (equal (alist-get 'generation result) (cdr owner)))
    (error "qq: Gateway %s result owner contradicts request" context))
  result)

(defun qq-gateway-directory--validate-category (category context)
  "Validate friend CATEGORY in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p
           category '(id name member_count sort_id))
    (error "qq: Gateway %s has invalid fields" context))
  (unless (qq-gateway-directory--int32-p (alist-get 'id category))
    (error "qq: Gateway %s.id must be int32" context))
  (unless (stringp (alist-get 'name category))
    (error "qq: Gateway %s.name must be string" context))
  (dolist (key '(member_count sort_id))
    (unless (qq-gateway-directory--uint32-p (alist-get key category))
      (error "qq: Gateway %s.%s must be uint32" context key)))
  category)

(defun qq-gateway-directory--validate-friend (friend context)
  "Validate native FRIEND in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p
           friend
           '(uid uin category_id nickname remark personal_sign qid age gender))
    (error "qq: Gateway %s has invalid fields" context))
  (unless (qq-gateway--non-empty-string-p (alist-get 'uid friend))
    (error "qq: Gateway %s.uid must be opaque string" context))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'uin friend))
    (error "qq: Gateway %s.uin must be exact decimal string" context))
  (unless (qq-gateway-directory--int32-p (alist-get 'category_id friend))
    (error "qq: Gateway %s.category_id must be int32" context))
  (dolist (key '(nickname remark personal_sign qid))
    (unless (qq-gateway-directory--optional-string-p (alist-get key friend))
      (error "qq: Gateway %s.%s must be string or null" context key)))
  (dolist (key '(age gender))
    (let ((value (alist-get key friend)))
      (unless (or (null value) (qq-gateway-directory--uint32-p value))
        (error "qq: Gateway %s.%s must be uint32 or null" context key))))
  friend)

(defun qq-gateway-directory--validate-friends-result (result owner)
  "Validate closed friend-list RESULT for OWNER."
  (qq-gateway-directory--validate-owner
   result owner '(account_id generation friends categories) "friend-list")
  (let ((categories (alist-get 'categories result))
        (friends (alist-get 'friends result))
        (seen-categories (make-hash-table :test #'eql))
        (seen-uids (make-hash-table :test #'equal))
        (seen-uins (make-hash-table :test #'equal)))
    (unless (and (proper-list-p categories) (proper-list-p friends))
      (error "qq: Gateway friend-list members must be arrays"))
    (cl-loop
     for category in categories
     for index from 0
     do
     (progn
       (qq-gateway-directory--validate-category
        category (format "friend-list.categories[%d]" index))
       (let ((category-id (alist-get 'id category)))
         (when (gethash category-id seen-categories)
           (error "qq: Gateway friend-list duplicates category %s" category-id))
         (puthash category-id t seen-categories))))
    (cl-loop
     for friend in friends
     for index from 0
     do
     (progn
       (qq-gateway-directory--validate-friend
        friend (format "friend-list.friends[%d]" index))
       (let ((category-id (alist-get 'category_id friend))
             (uid (alist-get 'uid friend))
             (uin (alist-get 'uin friend)))
         (unless (gethash category-id seen-categories)
           (error "qq: Gateway friend belongs to unknown category %s"
                  category-id))
         (when (gethash uid seen-uids)
           (error "qq: Gateway friend-list duplicates UID %s" uid))
         (when (gethash uin seen-uins)
           (error "qq: Gateway friend-list duplicates UIN %s" uin))
         (puthash uid t seen-uids)
         (puthash uin t seen-uins)))))
  (copy-tree result))

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
  "Convert validated friend-list RESULT to ordered shared categories."
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
  "Project validated friend-list RESULT for OWNER into shared state."
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

(defun qq-gateway-directory--validate-group (group context)
  "Validate native GROUP in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p
           group
           '(group_uin owner_uid name member_count member_max created_time
             description question announcement remark last_speak_time
             latest_sequence))
    (error "qq: Gateway %s has invalid fields" context))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'group_uin group))
    (error "qq: Gateway %s.group_uin must be exact decimal string" context))
  (unless (or (null (alist-get 'owner_uid group))
              (qq-gateway--non-empty-string-p (alist-get 'owner_uid group)))
    (error "qq: Gateway %s.owner_uid must be opaque string or null" context))
  (unless (stringp (alist-get 'name group))
    (error "qq: Gateway %s.name must be string" context))
  (dolist (key '(member_count member_max created_time))
    (unless (qq-gateway-directory--uint32-p (alist-get key group))
      (error "qq: Gateway %s.%s must be uint32" context key)))
  (when (> (alist-get 'member_count group) (alist-get 'member_max group))
    (error "qq: Gateway %s member count exceeds capacity" context))
  (dolist (key '(description question announcement remark))
    (unless (qq-gateway-directory--optional-string-p (alist-get key group))
      (error "qq: Gateway %s.%s must be string or null" context key)))
  (let ((last-speak (alist-get 'last_speak_time group))
        (latest (alist-get 'latest_sequence group)))
    (unless (or (null last-speak)
                (qq-gateway-directory--uint64-p last-speak))
      (error "qq: Gateway %s.last_speak_time must be uint64 or null" context))
    (unless (or (null latest) (qq-gateway-directory--uint32-p latest))
      (error "qq: Gateway %s.latest_sequence must be uint32 or null" context)))
  group)

(defun qq-gateway-directory--validate-groups-result (result owner)
  "Validate closed joined-group RESULT for OWNER."
  (qq-gateway-directory--validate-owner
   result owner '(account_id generation groups) "group-list")
  (let ((groups (alist-get 'groups result))
        (seen (make-hash-table :test #'equal)))
    (unless (proper-list-p groups)
      (error "qq: Gateway group-list groups must be an array"))
    (cl-loop
     for group in groups
     for index from 0
     do
     (progn
       (qq-gateway-directory--validate-group
        group (format "group-list.groups[%d]" index))
       (let ((group-uin (alist-get 'group_uin group)))
         (when (gethash group-uin seen)
           (error "qq: Gateway group-list duplicates group UIN %s" group-uin))
         (puthash group-uin t seen)))))
  (copy-tree result))

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
  "Project validated joined-group RESULT into shared state."
  (let ((account (qq-gateway-current-account))
        groups)
    (setq groups
          (mapcar (lambda (group)
                    (qq-gateway-directory--group-to-state group account))
                  (alist-get 'groups result)))
    (qq-state-apply-groups groups)
    groups))

(defun qq-gateway-directory--validate-permission (permission context)
  "Validate native member PERMISSION in CONTEXT."
  (pcase (alist-get 'kind permission)
    ((or "member" "owner" "admin")
     (unless (qq-gateway--exact-object-keys-p permission '(kind))
       (error "qq: Gateway %s permission has invalid fields" context)))
    ("unknown"
     (unless (and (qq-gateway--exact-object-keys-p permission '(kind code))
                  (qq-gateway-directory--uint32-p
                   (alist-get 'code permission)))
       (error "qq: Gateway %s unknown permission is malformed" context)))
    (_ (error "qq: Gateway %s permission has unknown kind" context)))
  permission)

(defun qq-gateway-directory--validate-member (member context)
  "Validate native group MEMBER in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p
           member
           '(uid uin nickname member_card special_title level permission
             join_timestamp last_message_timestamp shut_up_timestamp))
    (error "qq: Gateway %s has invalid fields" context))
  (unless (qq-gateway--non-empty-string-p (alist-get 'uid member))
    (error "qq: Gateway %s.uid must be opaque string" context))
  (unless (qq-gateway--canonical-decimal-p (alist-get 'uin member))
    (error "qq: Gateway %s.uin must be exact decimal string" context))
  (unless (stringp (alist-get 'nickname member))
    (error "qq: Gateway %s.nickname must be string" context))
  (dolist (key '(member_card special_title))
    (unless (qq-gateway-directory--optional-string-p (alist-get key member))
      (error "qq: Gateway %s.%s must be string or null" context key)))
  (dolist (key '(level join_timestamp last_message_timestamp shut_up_timestamp))
    (unless (qq-gateway-directory--uint32-p (alist-get key member))
      (error "qq: Gateway %s.%s must be uint32" context key)))
  (qq-gateway-directory--validate-permission
   (alist-get 'permission member) context)
  member)

(defun qq-gateway-directory--validate-members-result
    (result owner group-uin)
  "Validate closed member-list RESULT for OWNER and GROUP-UIN."
  (qq-gateway-directory--validate-owner
   result owner
   '(account_id generation group_uin members member_count
     member_list_change_sequence member_card_sequence)
   "group-member-list")
  (unless (equal (alist-get 'group_uin result) group-uin)
    (error "qq: Gateway group-member-list group contradicts request"))
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (error "qq: Gateway group-member-list group UIN is invalid"))
  (dolist (key '(member_count member_list_change_sequence member_card_sequence))
    (unless (qq-gateway-directory--uint32-p (alist-get key result))
      (error "qq: Gateway group-member-list %s must be uint32" key)))
  ;; The server-reported count is metadata, not a page-length invariant:
  ;; membership may change while the Account Runtime walks pagination.
  (let ((members (alist-get 'members result))
        (seen-uids (make-hash-table :test #'equal))
        (seen-uins (make-hash-table :test #'equal)))
    (unless (proper-list-p members)
      (error "qq: Gateway group-member-list members must be an array"))
    (cl-loop
     for member in members
     for index from 0
     do
     (progn
       (qq-gateway-directory--validate-member
        member (format "group-member-list.members[%d]" index))
       (let ((uid (alist-get 'uid member))
             (uin (alist-get 'uin member)))
         (when (gethash uid seen-uids)
           (error "qq: Gateway group-member-list duplicates UID %s" uid))
         (when (gethash uin seen-uins)
           (error "qq: Gateway group-member-list duplicates UIN %s" uin))
         (puthash uid t seen-uids)
         (puthash uin t seen-uins)))))
  (copy-tree result))

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
    (native_permission . ,(copy-tree (alist-get 'permission member)))
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

(defun qq-gateway-directory--project-members (result owner)
  "Project validated group-member RESULT for OWNER and return mapped members."
  (let* ((group-uin (alist-get 'group_uin result))
         (group (qq-state-group group-uin))
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
    (qq-gateway-directory--preflight-peer-identities owner identities)
    (dolist (identity identities)
      (qq-gateway-message--remember-peer-identity
       owner (car identity) (cdr identity)))
    (puthash group-uin (copy-tree page)
             qq-gateway-directory--member-pages)
    (qq-gateway-directory--enrich-group-from-members group-uin raw-members)
    members))

(defun qq-gateway-directory--set-cache-owner (owner)
  "Make exact OWNER own native directory request and member caches."
  (unless (equal owner qq-gateway-directory--cache-owner)
    (setq qq-gateway-directory--cache-owner (copy-tree owner))
    (clrhash qq-gateway-directory--active-requests)
    (clrhash qq-gateway-directory--member-pages))
  owner)

(defun qq-gateway-directory-reset ()
  "Revoke native directory request ownership and member caches."
  (setq qq-gateway-directory--cache-owner nil)
  (clrhash qq-gateway-directory--active-requests)
  (clrhash qq-gateway-directory--member-pages)
  nil)

(defun qq-gateway-directory--handle-account-context (&rest _arguments)
  "Revoke directory caches after a selected account context change."
  (if (eq qq-backend 'gateway)
      (qq-gateway-directory--set-cache-owner
       (qq-gateway-current-account-owner))
    (qq-gateway-directory-reset)))

(defun qq-gateway-directory--begin-request (resource owner)
  "Return and register the newest request token for RESOURCE and OWNER."
  (let ((token (list :resource resource :owner (copy-tree owner)
                     :serial (cl-incf qq-gateway-directory--request-counter))))
    (puthash resource token qq-gateway-directory--active-requests)
    token))

(defun qq-gateway-directory--request-current-p (resource token owner)
  "Return non-nil when RESOURCE still belongs to TOKEN and OWNER."
  (and (eq token (gethash resource qq-gateway-directory--active-requests))
       (equal owner qq-gateway-directory--cache-owner)
       (equal owner (qq-gateway-current-account-owner))))

(defun qq-gateway-directory--finish-request (resource token)
  "Forget RESOURCE only when it is still owned by TOKEN."
  (when (eq token (gethash resource qq-gateway-directory--active-requests))
    (remhash resource qq-gateway-directory--active-requests)
    t))

(defun qq-gateway-directory--request
    (resource method params validator projector callback errback)
  "Run one newest-owned directory RESOURCE request using native METHOD.

PARAMS are sent as-is.  VALIDATOR and PROJECTOR receive the result and exact
owner.  CALLBACK receives the projected value; ERRBACK follows Gateway error
conventions."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a Gateway account first")))
         (_projection (qq-gateway-message--ensure-projection-owner owner))
         (_cache (qq-gateway-directory--set-cache-owner owner))
         (token (qq-gateway-directory--begin-request resource owner)))
    (qq-gateway--send
     method (append `((account_id . ,(car owner))) params)
     (lambda (raw-result)
       (if (not (qq-gateway-directory--request-current-p
                 resource token owner))
           (qq-gateway--client-error
            errback "superseded_request"
            "Gateway directory request was superseded or changed owner")
         (condition-case error-data
             (let* ((result (funcall validator raw-result owner))
                    (value (funcall projector result owner)))
               (qq-gateway-directory--finish-request resource token)
               (qq-gateway--invoke callback value))
           (error
            (qq-gateway-directory--finish-request resource token)
            (qq-gateway--client-error
             errback "invalid_gateway_result" "%s"
             (error-message-string error-data))))))
     (lambda (body reason)
       (qq-gateway-directory--finish-request resource token)
       (qq-gateway--invoke errback body reason)))))

(defun qq-gateway-directory-refresh-friends
    (&optional callback errback refresh)
  "Refresh native friends and call CALLBACK with ordered categories.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Account Runtime to replace its generation-local contact cache."
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
   #'qq-gateway-directory--validate-friends-result
   #'qq-gateway-directory--project-friends
   callback errback))

(defun qq-gateway-directory-refresh-groups
    (&optional callback errback refresh)
  "Refresh native joined groups and call CALLBACK with mapped groups.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Account Runtime to replace its generation-local contact cache."
  (interactive
   (list (lambda (groups)
           (message "qq: loaded %d Gateway groups" (length groups)))
         (lambda (_body reason)
           (message "qq: Gateway group refresh failed: %s" reason))
         (and current-prefix-arg t)))
  (qq-gateway-directory--request
   'groups "contact.list_groups"
   `((refresh . ,(if refresh t :false)))
   #'qq-gateway-directory--validate-groups-result
   #'qq-gateway-directory--project-groups
   callback errback))

(defun qq-gateway-directory-list-group-members
    (group-uin callback &optional errback refresh)
  "List exact GROUP-UIN members and call CALLBACK with mapped members.

ERRBACK receives a Gateway error body and reason.  When REFRESH is non-nil,
force the Account Runtime to replace its generation-local member cache."
  (unless (qq-gateway--canonical-decimal-p group-uin)
    (user-error "qq: Group UIN must be an exact decimal string"))
  (qq-gateway-directory--request
   (cons 'group-members group-uin) "contact.list_group_members"
   `((group_uin . ,group-uin)
     (refresh . ,(if refresh t :false)))
   (lambda (result owner)
     (qq-gateway-directory--validate-members-result
      result owner group-uin))
   #'qq-gateway-directory--project-members
   callback errback))

(defun qq-gateway-directory-group-member-page (group-uin)
  "Return selected-generation cached member page for exact GROUP-UIN."
  (copy-tree (gethash group-uin qq-gateway-directory--member-pages)))

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-gateway-directory--handle-account-context)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-gateway-directory--handle-account-context)

(provide 'qq-gateway-directory)

;;; qq-gateway-directory.el ends here
