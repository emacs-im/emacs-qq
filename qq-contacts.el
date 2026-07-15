;;; qq-contacts.el --- Native QQ contacts and joined groups -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A persistent, keyed-EWOC directory for the complete native friend-category
;; and joined-group snapshots.  The recent-session root remains a compact
;; activity view; this buffer is the authoritative place to find peers which
;; are absent from that recent snapshot.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-view)
(require 'qq-api)
(require 'qq-media)
(require 'qq-runtime)
(require 'qq-state)

(autoload 'qq-chat-open "qq-chat" nil t)
(autoload 'qq-group-open "qq-group" nil t)
(autoload 'qq-root-open "qq-root" nil t)
(autoload 'qq-user-open "qq-user" nil t)

(declare-function qq-chat-open "qq-chat" (session-key))
(declare-function qq-group-open "qq-group" (group-id))
(declare-function qq-root-open "qq-root" ())
(declare-function qq-user-open "qq-user" (user-id))
(declare-function qq-user-open-search-result "qq-user" (result))
(declare-function qq-user-add-friend "qq-user" ())

(defgroup qq-contacts nil
  "Native friend and joined-group directory for emacs-qq."
  :group 'qq)

(defface qq-contacts-category
  '((t :inherit header-line :weight semi-bold :extend t))
  "Face for friend-category rows."
  :group 'qq-contacts)

(defface qq-contacts-section
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for directory section headings."
  :group 'qq-contacts)

(defface qq-contacts-navigation-button
  '((t :inherit mode-line-inactive :weight semi-bold
       :box (:line-width -1 :style released-button)))
  "Face for inactive directory navigation buttons."
  :group 'qq-contacts)

(defface qq-contacts-navigation-button-selected
  '((t :inherit mode-line-emphasis :weight bold
       :box (:line-width -1 :style pressed-button)))
  "Face for the selected directory navigation button."
  :group 'qq-contacts)

(defcustom qq-contacts-margin-columns 1
  "Columns reserved at the right edge of contact rows."
  :type 'integer
  :group 'qq-contacts)

(defconst qq-contacts-buffer-name "*qq-contacts*"
  "Name of the singleton QQ contacts buffer.")

(defconst qq-contacts--icon-slot-width 4
  "Width reserved for friend and group avatars.")

(cl-defstruct (qq-contacts--entry
               (:constructor qq-contacts--entry-create))
  key
  type
  object
  title
  count
  expanded
  view
  query
  width)

(defvar qq-contacts-search-history nil
  "Minibuffer history for native QQ directory searches.")

(defvar qq-contacts-search-scope-history nil
  "Minibuffer history for QQ directory search scopes.")

(defvar-local qq-contacts--ewoc nil)
(defvar-local qq-contacts--node-table nil)
(defvar-local qq-contacts--view 'friends)
(defvar-local qq-contacts--previous-view 'friends)
(defvar-local qq-contacts--query nil)
(defvar-local qq-contacts--search-scope 'all)
(defvar-local qq-contacts--collapsed-categories nil)
(defvar-local qq-contacts--fill-column nil)
(defvar-local qq-contacts--header-line-cache "")
(defvar-local qq-contacts--rendering nil)
(defvar-local qq-contacts--render-pending nil)
(defvar-local qq-contacts--dirty nil)
(defvar-local qq-contacts--pending-force-keys nil)
(defvar-local qq-contacts--loading nil)
(defvar-local qq-contacts--error nil)
(defvar-local qq-contacts--refresh-owner nil)
(defvar-local qq-contacts--refresh-pending 0)
(defvar-local qq-contacts--refresh-parts nil)
(defvar-local qq-contacts--friend-request nil)
(defvar-local qq-contacts--group-request nil)
(defvar-local qq-contacts--search-owner nil)
(defvar-local qq-contacts--search-pending nil)
(defvar-local qq-contacts--search-errors nil)
(defvar-local qq-contacts--search-friends nil)
(defvar-local qq-contacts--search-groups nil)
(defvar-local qq-contacts--search-strangers nil)
(defvar-local qq-contacts--search-members nil)
(defvar-local qq-contacts--member-group-id nil)
(defvar-local qq-contacts--search-friend-cursor nil)
(defvar-local qq-contacts--search-group-cursor nil)
(defvar-local qq-contacts--search-stranger-cursor nil)
(defvar-local qq-contacts--search-member-cursor nil)
(defvar-local qq-contacts--search-friend-request nil)
(defvar-local qq-contacts--search-group-request nil)
(defvar-local qq-contacts--search-stranger-request nil)
(defvar-local qq-contacts--search-member-request nil)

(defconst qq-contacts--view-id 'contacts
  "Appkit identity of the singleton contacts directory view.")

(defun qq-contacts--present-string (value)
  "Return non-empty string VALUE, or nil."
  (and (stringp value) (not (string-empty-p value)) value))

(defun qq-contacts--friend-name (friend)
  "Return the best display name for FRIEND."
  (or (qq-contacts--present-string (alist-get 'remark friend))
      (qq-contacts--present-string (alist-get 'nickname friend))
      (alist-get 'user_id friend)))

(defun qq-contacts--group-name (group)
  "Return the best display name for GROUP."
  (or (qq-contacts--present-string (alist-get 'group_remark group))
      (qq-contacts--present-string (alist-get 'group_name group))
      (qq-contacts--present-string (alist-get 'remark group))
      (qq-contacts--present-string (alist-get 'name group))
      (alist-get 'group_id group)))

(defun qq-contacts--navigation-entry ()
  "Return the navigation entry for the current view."
  (qq-contacts--entry-create
   :key 'navigation
   :type 'navigation
   :view qq-contacts--view
   :query qq-contacts--query
   :width qq-contacts--fill-column))

(defun qq-contacts--category-entry (category)
  "Return one friend CATEGORY entry."
  (let ((category-id (alist-get 'category_id category)))
    (qq-contacts--entry-create
     :key (cons 'category category-id)
     :type 'category
     :object category
     :title (alist-get 'name category)
     :count (length (alist-get 'friends category))
     :expanded (not (gethash category-id qq-contacts--collapsed-categories))
     :width qq-contacts--fill-column)))

(defun qq-contacts--friend-entry (friend)
  "Return one FRIEND entry."
  (qq-contacts--entry-create
   :key (cons 'friend (alist-get 'user_id friend))
   :type 'friend
   :object friend
   :width qq-contacts--fill-column))

(defun qq-contacts--group-entry (group)
  "Return one joined GROUP entry."
  (qq-contacts--entry-create
   :key (cons 'group (alist-get 'group_id group))
   :type 'group
   :object group
   :width qq-contacts--fill-column))

(defun qq-contacts--member-entry (member)
  "Return one native group MEMBER search entry."
  (qq-contacts--entry-create
   :key (cons 'member (alist-get 'user_id member))
   :type 'member
   :object member
   :width qq-contacts--fill-column))

(defun qq-contacts--stranger-entry (stranger)
  "Return one global QQ user search entry for STRANGER."
  (qq-contacts--entry-create
   :key (cons 'stranger (alist-get 'user_id stranger))
   :type 'stranger
   :object stranger
   :width qq-contacts--fill-column))

(defun qq-contacts--section-entry (key title count)
  "Return section KEY with TITLE and COUNT."
  (qq-contacts--entry-create
   :key (cons 'section key)
   :type 'section
   :title title
   :count count
   :width qq-contacts--fill-column))

(defun qq-contacts--note-entry (key title &optional type)
  "Return status note KEY with TITLE and optional TYPE."
  (qq-contacts--entry-create
   :key (cons 'note key)
   :type (or type 'note)
   :title title
   :width qq-contacts--fill-column))

(defun qq-contacts--load-more-entry (kind label)
  "Return a load-more entry for search result KIND with LABEL."
  (qq-contacts--entry-create
   :key (cons 'load-more kind)
   :type 'load-more
   :object kind
   :title label
   :width qq-contacts--fill-column))

(defun qq-contacts--search-error (kind)
  "Return the current native search error for KIND."
  (alist-get kind qq-contacts--search-errors))

(defun qq-contacts--search-group-object (result)
  "Project native group-search RESULT into one directory row object."
  (let ((group (alist-get 'group result)))
    `((group_id . ,(alist-get 'group_id group))
      (group_name . ,(alist-get 'name group))
      (group_remark . ,(alist-get 'remark group))
      (member_count . ,(alist-get 'member_count group))
      (self_permission . ,(alist-get 'self_permission group))
      (search_hits . ,(copy-tree (alist-get 'hits group)))
      (matched_discussions . ,(copy-tree (alist-get 'discussions result)))
      (matched_members . ,(copy-tree (alist-get 'member_profiles result)))
      (matched_member_cards . ,(copy-tree (alist-get 'member_cards result)))
      (recall_reason . ,(alist-get 'recall_reason result)))))

(defun qq-contacts--group-recent-p (group)
  "Return non-nil when GROUP has an entry in the recent-session snapshot."
  (qq-state-session-recent-p
   (qq-state-session-key 'group (alist-get 'group_id group))))

(defun qq-contacts--project-friends ()
  "Project the ordered friend-category view."
  (let ((categories (qq-state-friend-categories))
        entries)
    (push (qq-contacts--navigation-entry) entries)
    (dolist (category categories)
      (let ((category-entry (qq-contacts--category-entry category)))
        (push category-entry entries)
        (when (qq-contacts--entry-expanded category-entry)
          (dolist (friend (alist-get 'friends category))
            (push (qq-contacts--friend-entry friend) entries)))))
    (unless categories
      (push (qq-contacts--note-entry
             'empty-friends
             (cond
              (qq-contacts--loading "正在加载好友分组…")
              ((not (qq-state-friend-categories-loaded-p))
               "好友分组尚未加载。")
              (t "好友分组为空。")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-groups (&optional not-recent-p)
  "Project joined groups, restricted to non-recent groups when NOT-RECENT-P."
  (let* ((all-groups (qq-state-groups))
         (groups (if not-recent-p
                     (seq-remove #'qq-contacts--group-recent-p all-groups)
                   all-groups))
         entries)
    (push (qq-contacts--navigation-entry) entries)
    (dolist (group groups)
      (push (qq-contacts--group-entry group) entries))
    (unless groups
      (push (qq-contacts--note-entry
             (if not-recent-p 'empty-not-recent 'empty-groups)
             (cond
              (qq-contacts--loading "正在加载群列表…")
              ((not (qq-state-groups-loaded-p)) "群列表尚未加载。")
              (not-recent-p "所有已加入群都出现在当前近期会话快照中。")
              (t "已加入群列表为空。")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-search ()
  "Project exact native friend, group-chat, and global-user results."
  (let ((friends qq-contacts--search-friends)
        (groups qq-contacts--search-groups)
        (strangers qq-contacts--search-strangers)
        entries)
    (push (qq-contacts--navigation-entry) entries)
    (when friends
      (push (qq-contacts--section-entry 'friends "好友" (length friends)) entries)
      (dolist (friend friends)
        (push (qq-contacts--friend-entry friend) entries)))
    (when (memq 'friends qq-contacts--search-pending)
      (push (qq-contacts--note-entry 'searching-friends "正在搜索好友…") entries))
    (when-let* ((reason (qq-contacts--search-error 'friends)))
      (push (qq-contacts--note-entry 'search-friends-error reason 'error-note)
            entries))
    (when qq-contacts--search-friend-cursor
      (push (qq-contacts--load-more-entry 'friends "加载更多好友") entries))
    (when groups
      (push (qq-contacts--section-entry 'groups "群聊" (length groups)) entries)
      (dolist (group groups)
        (push (qq-contacts--group-entry group) entries)))
    (when (memq 'groups qq-contacts--search-pending)
      (push (qq-contacts--note-entry 'searching-groups "正在搜索群聊…") entries))
    (when-let* ((reason (qq-contacts--search-error 'groups)))
      (push (qq-contacts--note-entry 'search-groups-error reason 'error-note)
            entries))
    (when qq-contacts--search-group-cursor
      (push (qq-contacts--load-more-entry 'groups "加载更多群聊") entries))
    (when strangers
      (push (qq-contacts--section-entry
             'strangers "全网 QQ 用户" (length strangers))
            entries)
      (dolist (stranger strangers)
        (push (qq-contacts--stranger-entry stranger) entries)))
    (when (memq 'strangers qq-contacts--search-pending)
      (push (qq-contacts--note-entry 'searching-strangers
                                     "正在搜索全网 QQ 用户…")
            entries))
    (when-let* ((reason (qq-contacts--search-error 'strangers)))
      (push (qq-contacts--note-entry 'search-strangers-error
                                     reason 'error-note)
            entries))
    (when qq-contacts--search-stranger-cursor
      (push (qq-contacts--load-more-entry
             'strangers "加载更多全网 QQ 用户")
            entries))
    (unless (or friends groups strangers qq-contacts--search-pending
                qq-contacts--search-errors
                qq-contacts--search-friend-cursor
                qq-contacts--search-group-cursor
                qq-contacts--search-stranger-cursor)
      (push (qq-contacts--note-entry
             'empty-search
             (format "%s中没有与 “%s” 匹配的结果。"
                     (qq-contacts--search-scope-label
                      qq-contacts--search-scope)
                     (or qq-contacts--query "")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-members ()
  "Project exact native group-member search results."
  (let (entries)
    (push (qq-contacts--navigation-entry) entries)
    (when qq-contacts--search-members
      (push (qq-contacts--section-entry
             'members "群成员" (length qq-contacts--search-members))
            entries)
      (dolist (member qq-contacts--search-members)
        (push (qq-contacts--member-entry member) entries)))
    (when (memq 'members qq-contacts--search-pending)
      (push (qq-contacts--note-entry 'searching-members "正在搜索群成员…")
            entries))
    (when-let* ((reason (qq-contacts--search-error 'members)))
      (push (qq-contacts--note-entry 'search-members-error reason 'error-note)
            entries))
    (when qq-contacts--search-member-cursor
      (push (qq-contacts--load-more-entry 'members "加载更多群成员") entries))
    (unless (or qq-contacts--search-members qq-contacts--search-pending
                qq-contacts--search-errors
                qq-contacts--search-member-cursor)
      (push (qq-contacts--note-entry
             'empty-member-search
             (format "群 %s 中没有与 “%s” 匹配的成员。"
                     (or qq-contacts--member-group-id "")
                     (or qq-contacts--query "")))
            entries))
    (nreverse entries)))

(defun qq-contacts--project-entries ()
  "Return the current ordered directory projection."
  (let ((entries
         (pcase qq-contacts--view
           ('friends (qq-contacts--project-friends))
           ('groups (qq-contacts--project-groups))
           ('not-recent (qq-contacts--project-groups t))
           ('search (qq-contacts--project-search))
           ('members (qq-contacts--project-members))
           (_ (error "qq: unknown contacts view %S" qq-contacts--view)))))
    (when qq-contacts--error
      (setq entries
            (append entries
                    (list (qq-contacts--note-entry
                           'refresh-error qq-contacts--error 'error-note)))))
    entries))

(defun qq-contacts--selected-navigation-face (view)
  "Return navigation face for VIEW."
  (if (or (eq qq-contacts--view view)
          (and (eq view 'search) (eq qq-contacts--view 'members)))
      'qq-contacts-navigation-button-selected
    'qq-contacts-navigation-button))

(defun qq-contacts--insert-navigation-button (label view action help)
  "Insert navigation LABEL for VIEW, invoking ACTION with HELP text."
  (appkit-ui-insert-action-button
   label action
   :face (qq-contacts--selected-navigation-face view)
   :help-echo help))

(defun qq-contacts--insert-navigation (_entry)
  "Insert the directory navigation row."
  (insert " ")
  (qq-contacts--insert-navigation-button
   " 好友分组 " 'friends #'qq-contacts-show-friends "显示完整好友分组 (f)")
  (insert "  ")
  (qq-contacts--insert-navigation-button
   " 全部群 " 'groups #'qq-contacts-show-groups "显示所有已加入群 (G)")
  (insert "  ")
  (qq-contacts--insert-navigation-button
   " 未在近期 " 'not-recent #'qq-contacts-show-not-recent-groups
   "显示未出现在当前近期会话快照中的群 (I)")
  (insert "  ")
  (qq-contacts--insert-navigation-button
   " 搜索… " 'search
   (lambda () (call-interactively #'qq-contacts-search))
   "选择范围并搜索好友、群聊或全网 QQ 用户 (/)")
  (insert "  ")
  (appkit-ui-insert-action-button
   " 刷新 " #'qq-contacts-refresh
   :face 'qq-contacts-navigation-button
   :help-echo "从 Linux QQ 刷新精确通讯录快照 (g)")
  (insert "\n\n"))

(defun qq-contacts--insert-category (entry)
  "Insert friend-category ENTRY."
  (let ((start (point))
        (category-id (alist-get 'category_id (qq-contacts--entry-object entry))))
    (insert " " (if (qq-contacts--entry-expanded entry) "▾" "▸") " "
            (or (qq-contacts--entry-title entry) "未命名分组")
            (format "  (%d)\n" (or (qq-contacts--entry-count entry) 0)))
    (add-text-properties
     start (point)
     (list 'face 'qq-contacts-category
           'qq-contacts-key (qq-contacts--entry-key entry)
           'qq-contacts-row-type 'category
           'qq-contacts-category-id category-id))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 折叠/展开好友分组"
     :mouse-face 'highlight)))

(defun qq-contacts--friend-preview (friend)
  "Return secondary line text for FRIEND."
  (let ((nickname (qq-contacts--present-string (alist-get 'nickname friend)))
        (remark (qq-contacts--present-string (alist-get 'remark friend)))
        (qid (qq-contacts--present-string (alist-get 'qid friend)))
        (category (qq-contacts--present-string
                   (alist-get 'category_name friend)))
        (user-id (alist-get 'user_id friend)))
    (string-join
     (delq nil
           (list (and nickname (not (equal nickname remark)) nickname)
                 (and qid (format "QID %s" qid))
                 (format "QQ %s" user-id)
                 category))
     " · ")))

(defun qq-contacts--insert-friend (entry)
  "Insert actionable friend ENTRY."
  (let* ((friend (qq-contacts--entry-object entry))
         (user-id (alist-get 'user_id friend))
         (start (point)))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-avatar-cached-display-string user-id)))
      :context (qq-contacts--friend-name friend)
      :preview (qq-contacts--friend-preview friend)
      :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'friend
            'qq-contacts-object friend
            'qq-contacts-item-id user-id)
      :help-echo "mouse-1 or RET: 打开私聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开私聊"
     :mouse-face 'highlight)))

(defun qq-contacts--permission-label (permission)
  "Return concise label for native group PERMISSION."
  (pcase permission
    ("owner" "群主")
    ("admin" "管理员")
    ("member" "成员")
    (_ "")))

(defun qq-contacts--search-hit-label (hits)
  "Return a concise description of non-empty group-search HITS."
  (let (labels)
    (dolist (spec '((group_id . "群号") (name . "群名") (remark . "备注")))
      (when (alist-get (car spec) hits)
        (push (cdr spec) labels)))
    (when labels
      (format "命中%s" (string-join (nreverse labels) "、")))))

(defun qq-contacts--matched-label (prefix values field)
  "Return PREFIX plus up to three non-empty FIELD values from VALUES."
  (let ((names
         (seq-take
          (delq nil
                (mapcar (lambda (value)
                          (qq-contacts--present-string
                           (alist-get field value)))
                        values))
          3)))
    (when names
      (format "%s%s" prefix (string-join names "、")))))

(defun qq-contacts--group-preview (group)
  "Return secondary line text for GROUP."
  (let ((name (qq-contacts--present-string (alist-get 'group_name group)))
        (remark (qq-contacts--present-string (alist-get 'group_remark group)))
        (group-id (alist-get 'group_id group))
        (member-count (alist-get 'member_count group))
        (matched-members (alist-get 'matched_members group))
        (matched-discussions (alist-get 'matched_discussions group))
        (matched-member-cards (alist-get 'matched_member_cards group))
        (reason (qq-contacts--present-string
                 (alist-get 'recall_reason group))))
    (string-join
     (delq nil
           (list (and name (not (equal name remark)) name)
                 (format "群 %s" group-id)
                 (and (integerp member-count)
                      (format "%d 位成员" member-count))
                 (qq-contacts--search-hit-label
                  (alist-get 'search_hits group))
                 (qq-contacts--matched-label
                  "命中讨论组 " matched-discussions 'name)
                 (when matched-members
                   (format
                    "命中成员 %s"
                    (string-join
                     (seq-take
                      (mapcar
                       (lambda (member)
                         (or (qq-contacts--present-string
                              (alist-get 'card member))
                             (qq-contacts--present-string
                              (alist-get 'remark member))
                             (qq-contacts--present-string
                              (alist-get 'nickname member))
                             (alist-get 'user_id member)))
                       matched-members)
                      3)
                     "、")))
                 (qq-contacts--matched-label
                  "命中群名片 " matched-member-cards 'card)
                 (and reason (format "匹配原因 %s" reason))))
     " · ")))

(defun qq-contacts--insert-group (entry)
  "Insert actionable joined-group ENTRY."
  (let* ((group (qq-contacts--entry-object entry))
         (group-id (alist-get 'group_id group))
         (start (point)))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-group-avatar-cached-display-string group-id)))
      :context (qq-contacts--group-name group)
      :context-trail (qq-contacts--permission-label
                      (alist-get 'self_permission group))
      :context-trail-face 'shadow
      :preview (qq-contacts--group-preview group)
      :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'group
            'qq-contacts-object group
            'qq-contacts-item-id group-id)
      :help-echo "mouse-1 or RET: 打开群聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开群聊"
     :mouse-face 'highlight)))

(defun qq-contacts--member-name (member)
  "Return the best exact display name for group MEMBER."
  (or (qq-contacts--present-string (alist-get 'card member))
      (qq-contacts--present-string (alist-get 'remark member))
      (qq-contacts--present-string (alist-get 'nickname member))
      (alist-get 'user_id member)))

(defun qq-contacts--member-preview (member)
  "Return secondary line text for native group MEMBER."
  (let ((card (qq-contacts--present-string (alist-get 'card member)))
        (remark (qq-contacts--present-string (alist-get 'remark member)))
        (nickname (qq-contacts--present-string (alist-get 'nickname member))))
    (string-join
     (delq nil
           (list (and nickname (not (equal nickname card)) nickname)
                 (and remark (not (member remark (list card nickname))) remark)
                 (format "QQ %s" (alist-get 'user_id member))
                 (and (eq (alist-get 'is_friend member) t) "好友")))
     " · ")))

(defun qq-contacts--insert-member (entry)
  "Insert actionable native group MEMBER search ENTRY."
  (let* ((member (qq-contacts--entry-object entry))
         (user-id (alist-get 'user_id member))
         (start (point)))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-avatar-cached-display-string user-id)))
      :context (qq-contacts--member-name member)
      :context-trail (qq-contacts--present-string
                      (alist-get 'group_name member))
      :context-trail-face 'shadow
      :preview (qq-contacts--member-preview member)
      :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'member
            'qq-contacts-object member
            'qq-contacts-item-id user-id)
      :help-echo "mouse-1 or RET: 打开私聊")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开私聊"
     :mouse-face 'highlight)))

(defun qq-contacts--stranger-name (stranger)
  "Return the best display name for global user STRANGER."
  (or (qq-contacts--present-string (alist-get 'nickname stranger))
      (alist-get 'user_id stranger)))

(defun qq-contacts--insert-stranger (entry)
  "Insert actionable global QQ user ENTRY."
  (let* ((stranger (qq-contacts--entry-object entry))
         (user-id (alist-get 'user_id stranger))
         (start (point)))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter (lambda ()
                       (insert
                        (qq-media-avatar-cached-display-string user-id)))
      :context (qq-contacts--stranger-name stranger)
      :context-trail "全网用户"
      :context-trail-face 'shadow
      :preview (format "QQ %s · 打开资料页后可添加好友" user-id)
      :line-properties
      (list 'qq-contacts-key (qq-contacts--entry-key entry)
            'qq-contacts-row-type 'stranger
            'qq-contacts-object stranger
            'qq-contacts-item-id user-id)
      :help-echo "mouse-1 or RET: 打开用户资料")
     :indent 2
     :width (or (qq-contacts--entry-width entry) 80)
     :icon-slot-width qq-contacts--icon-slot-width
     :context-width-spec '(0.45 18 42))
    (appkit-ui-make-action-row
     start (point) entry #'qq-contacts--activate-entry
     :help-echo "mouse-1 or RET: 打开用户资料"
     :mouse-face 'highlight)))

(defun qq-contacts--insert-section (entry)
  "Insert search section ENTRY."
  (let ((start (point)))
    (insert (format "%s  (%d)\n"
                    (or (qq-contacts--entry-title entry) "")
                    (or (qq-contacts--entry-count entry) 0)))
    (add-text-properties
     start (point)
     (list 'face 'qq-contacts-section
           'qq-contacts-key (qq-contacts--entry-key entry)))))

(defun qq-contacts--insert-note (entry)
  "Insert status note ENTRY."
  (appkit-view-insert-note-line
   (or (qq-contacts--entry-title entry) "")
   :face (if (eq (qq-contacts--entry-type entry) 'error-note)
             'error
           'shadow)
   :line-properties (list 'qq-contacts-key (qq-contacts--entry-key entry))))

(defun qq-contacts--insert-load-more (entry)
  "Insert a real pagination button for search ENTRY."
  (let ((kind (qq-contacts--entry-object entry))
        (start (point)))
    (insert "  ")
    (appkit-ui-insert-action-button
     (format " %s " (or (qq-contacts--entry-title entry) "加载更多"))
     (pcase kind
       ('friends #'qq-contacts-load-more-friends)
       ('groups #'qq-contacts-load-more-groups)
       ('strangers #'qq-contacts-load-more-strangers)
       ('members #'qq-contacts-load-more-members)
       (_ (error "qq: unknown contacts pagination kind %S" kind)))
     :face 'qq-contacts-navigation-button
     :help-echo "继续精确的原生搜索")
    (insert "\n")
    (add-text-properties
     start (point)
     (list 'qq-contacts-key (qq-contacts--entry-key entry)))))

(defun qq-contacts--ewoc-printer (entry)
  "Insert one directory ENTRY."
  (pcase (qq-contacts--entry-type entry)
    ('navigation (qq-contacts--insert-navigation entry))
    ('category (qq-contacts--insert-category entry))
    ('friend (qq-contacts--insert-friend entry))
    ('group (qq-contacts--insert-group entry))
    ('member (qq-contacts--insert-member entry))
    ('stranger (qq-contacts--insert-stranger entry))
    ('section (qq-contacts--insert-section entry))
    ('load-more (qq-contacts--insert-load-more entry))
    ((or 'note 'error-note) (qq-contacts--insert-note entry))
    (type (error "qq: unknown contacts entry type %S" type))))

(defun qq-contacts--usable-width ()
  "Return current directory row width."
  (or (when-let* ((widths
                   (delq nil
                         (mapcar
                          (lambda (window)
                            (appkit-view-window-fill-column
                             window qq-contacts-margin-columns))
                          (get-buffer-window-list (current-buffer) nil t)))))
        (apply #'min widths))
      qq-contacts--fill-column
      80))

(defun qq-contacts--view-label ()
  "Return human-readable label for the selected directory view."
  (pcase qq-contacts--view
    ('friends "好友分组")
    ('groups "全部已加入群")
    ('not-recent "未在近期会话中的群")
    ('search (format "%s · “%s”"
                     (qq-contacts--search-scope-label
                      qq-contacts--search-scope)
                     (or qq-contacts--query "")))
    ('members (format "群 %s 的成员 · “%s”"
                      (or qq-contacts--member-group-id "")
                      (or qq-contacts--query "")))
    (_ "通讯录")))

(defconst qq-contacts--search-scope-choices
  '(("全部" . all)
    ("通讯录（好友＋已加入群）" . contacts)
    ("仅好友" . friends)
    ("仅群聊" . groups)
    ("全网 QQ 用户" . strangers))
  "User-facing search range names and their internal scopes.")

(defun qq-contacts--search-scope-label (scope)
  "Return the user-facing label for search SCOPE."
  (or (car (rassq scope qq-contacts--search-scope-choices))
      (error "qq: unknown contacts search scope %S" scope)))

(defun qq-contacts--read-search-scope ()
  "Read and return one explicit directory search scope."
  (let* ((labels (mapcar #'car qq-contacts--search-scope-choices))
         (label (completing-read "搜索范围: " labels nil t nil
                                 'qq-contacts-search-scope-history
                                 (car labels))))
    (or (cdr (assoc label qq-contacts--search-scope-choices))
        (user-error "qq: unknown directory search scope"))))

(defun qq-contacts--search-kinds (scope)
  "Return independent native search kinds selected by SCOPE."
  (pcase scope
    ('all '(friends groups strangers))
    ('contacts '(friends groups))
    ('friends '(friends))
    ('groups '(groups))
    ('strangers '(strangers))
    (_ (user-error "qq: invalid directory search scope %S" scope))))

(defun qq-contacts--refresh-header-line ()
  "Refresh cached directory header text."
  (setq qq-contacts--header-line-cache
        (format " QQ 通讯录 · %s  %d 位好友 · %d 个群%s"
                (qq-contacts--view-label)
                (qq-state-friend-count)
                (qq-state-group-count)
                (if qq-contacts--loading " · 正在刷新" "")))
  (force-mode-line-update))

(defun qq-contacts--queue-force-keys (keys)
  "Retain stable row KEYS until their next successful redraw."
  (dolist (key keys)
    (cl-pushnew key qq-contacts--pending-force-keys :test #'equal)))

(defun qq-contacts--invalidate-keys (keys)
  "Redraw existing EWOC rows identified by stable KEYS."
  (let ((snapshot
         (appkit-position-capture
          :anchor-property 'qq-contacts-key
          :preserve-window-start t))
        succeeded)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t)
                (buffer-undo-list t))
            (with-silent-modifications
              (dolist (key keys)
                (appkit-ewoc-invalidate-key
                 qq-contacts--ewoc qq-contacts--node-table key))))
          (setq succeeded t))
      (when snapshot
        (ignore-errors (appkit-position-restore snapshot)))
      (unless succeeded
        (qq-contacts--queue-force-keys keys)
        (setq qq-contacts--dirty t)))))

(defun qq-contacts--reconcile (&optional force-keys)
  "Reconcile the persistent directory, forcing FORCE-KEYS."
  (qq-contacts--queue-force-keys force-keys)
  (if qq-contacts--rendering
      (setq qq-contacts--render-pending t)
    (let ((qq-contacts--rendering t)
          (forced qq-contacts--pending-force-keys)
          succeeded
          (snapshot
           (appkit-position-capture
            :anchor-property 'qq-contacts-key
            :preserve-window-start t)))
      (setq qq-contacts--pending-force-keys nil)
      (unwind-protect
          (progn
            (setq qq-contacts--fill-column (qq-contacts--usable-width))
            (let ((inhibit-read-only t)
                  (buffer-undo-list t))
              (with-silent-modifications
                (setq qq-contacts--node-table
                      (appkit-ewoc-reconcile
                       qq-contacts--ewoc
                       (qq-contacts--project-entries)
                       #'qq-contacts--entry-key
                       :force-keys forced))))
            (setq qq-contacts--dirty nil)
            (qq-contacts--refresh-header-line)
            (dolist (window (get-buffer-window-list (current-buffer) nil t))
              (qq-contacts--ensure-window-avatars window))
            (setq succeeded t))
        (when snapshot
          (ignore-errors (appkit-position-restore snapshot)))
        (unless succeeded
          (qq-contacts--queue-force-keys forced)
          (setq qq-contacts--dirty t))
        (setq qq-contacts--rendering nil))
      (when qq-contacts--render-pending
        (setq qq-contacts--render-pending nil)
        (qq-contacts--reconcile)))))

(defun qq-contacts--displayed-p ()
  "Return non-nil when the directory has a live display window."
  (window-live-p (get-buffer-window (current-buffer) t)))

(defun qq-contacts--live-current-view ()
  "Return this buffer's live contacts view without creating one."
  (let ((view (appkit-current-view)))
    (and (derived-mode-p 'qq-contacts-mode)
         (appkit-view-live-p view)
         (equal qq-contacts--view-id (appkit-view-id view))
         view)))

(defun qq-contacts--ensure-window-avatars (window)
  "Start avatar work only for directory rows visible in WINDOW."
  (when (and (qq-contacts--live-current-view)
             (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (let ((position (window-start window))
          (limit (or (window-end window t) (point-max)))
          (seen (make-hash-table :test #'equal)))
      (while (< position limit)
        (let* ((row-type (get-text-property position 'qq-contacts-row-type))
               (object (get-text-property position 'qq-contacts-object))
               (identity
                (pcase row-type
                  ((or 'friend 'member 'stranger)
                   (alist-get 'user_id object))
                  ('group (alist-get 'group_id object)))))
          (when (and identity (not (gethash (cons row-type identity) seen)))
            (puthash (cons row-type identity) t seen)
            (condition-case error-data
                (pcase row-type
                  ((or 'friend 'member)
                   (qq-media-avatar-image identity))
                  ('stranger
                   (qq-media-url-preview-image
                    (format "avatar:%s" identity)
                    (alist-get 'avatar_url object)
                    qq-media-avatar-image-height))
                  ('group (qq-media-group-avatar-image identity)))
              (error
               (message "qq: failed to prepare directory avatar %s: %s"
                        identity (error-message-string error-data)))))
          (setq position
                (next-single-property-change
                 position 'qq-contacts-row-type nil limit)))))))

(defun qq-contacts--window-scroll (window _display-start)
  "Start media for newly visible directory rows after WINDOW scrolls."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer)))
    (qq-contacts--ensure-window-avatars window)))

(defun qq-contacts--ensure-view ()
  "Return the live Appkit view owning the current contacts buffer."
  (let* ((app (qq-runtime-app))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal qq-contacts--view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-contacts--sync-invalidations
            (appkit-view-parts current) '(directory))
     current)
     ((appkit-view-live-p current)
      (error "QQ: contacts buffer belongs to a different Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id qq-contacts--view-id
              :mode 'qq-contacts-mode
              :sync-function #'qq-contacts--sync-invalidations
              :parts '(directory))))
        (qq-contacts--setup-view view)
        view)))))

(defun qq-contacts--cancel-buffer-work (buffer)
  "Cancel asynchronous contacts work still owned by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-contacts-mode)
        (qq-contacts--cancel-refresh)
        (qq-contacts--cancel-search)))))

(defun qq-contacts--setup-view (view)
  "Register lifecycle cleanup for newly attached contacts VIEW."
  (appkit-register-handle
   view 'function
   (apply-partially #'qq-contacts--cancel-buffer-work
                    (appkit-view-buffer view))))

(defun qq-contacts--queue-view-sync (view &optional force-keys)
  "Queue one coalesced directory sync for live VIEW.

FORCE-KEYS identifies existing rows whose presentation resources changed."
  (when (appkit-view-live-p view)
    (if force-keys
        (appkit-invalidate view :entries force-keys)
      (appkit-invalidate view :structure t :part 'directory))
    (appkit-schedule-sync view)))

(defun qq-contacts--request-reconcile (&optional force-keys)
  "Request a coalesced directory sync, forcing FORCE-KEYS when non-nil."
  (qq-contacts--queue-view-sync (qq-contacts--ensure-view) force-keys))

(defun qq-contacts--sync-invalidations (view invalidations)
  "Consume coalesced Appkit INVALIDATIONS for contacts VIEW."
  (when (appkit-view-live-p view)
    (let ((force-keys (appkit-invalidations-entry-keys invalidations))
          (full-p (or (appkit-invalidations-structure-p invalidations)
                      (appkit-invalidations-parts invalidations)
                      (appkit-invalidations-position-p invalidations))))
      (when (or full-p force-keys)
        (if (qq-contacts--displayed-p)
            (appkit-with-content-update view
              (let ((width (qq-contacts--usable-width)))
                (if (and (not full-p)
                         force-keys
                         (not qq-contacts--dirty)
                         (not qq-contacts--rendering)
                         (= width (or qq-contacts--fill-column 0)))
                    (qq-contacts--invalidate-keys force-keys)
                  (qq-contacts--reconcile force-keys))))
          (qq-contacts--queue-force-keys force-keys)
          (setq qq-contacts--dirty t))))))

(defun qq-contacts--window-buffer-change (window)
  "Flush deferred updates when WINDOW displays the directory."
  (when-let* ((view (qq-contacts--live-current-view)))
    (when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
      (let ((width (qq-contacts--usable-width)))
        (when (or qq-contacts--dirty
                  qq-contacts--pending-force-keys
                  (/= width (or qq-contacts--fill-column 0)))
          (qq-contacts--queue-view-sync view))))))

(defun qq-contacts--window-size-change (&optional _frame)
  "Reflow this directory after a visible window size change."
  (when-let* ((view (qq-contacts--live-current-view)))
    (when (qq-contacts--displayed-p)
      (let ((width (qq-contacts--usable-width)))
        (when (/= width (or qq-contacts--fill-column 0))
          (qq-contacts--queue-view-sync view))))))

(defun qq-contacts--set-view (view)
  "Select directory VIEW and reconcile."
  (when (and (memq qq-contacts--view '(search members))
             (not (memq view '(search members))))
    (qq-contacts--cancel-search)
    (qq-contacts--clear-search-results))
  (unless (memq view '(search members))
    (setq qq-contacts--query nil))
  (setq qq-contacts--view view)
  (qq-contacts--request-reconcile))

(defun qq-contacts-show-friends ()
  "Show authoritative friend categories."
  (interactive)
  (qq-contacts--set-view 'friends))

(defun qq-contacts-show-groups ()
  "Show all authoritative joined groups."
  (interactive)
  (qq-contacts--set-view 'groups))

(defun qq-contacts-show-not-recent-groups ()
  "Show joined groups absent from the current recent-session snapshot."
  (interactive)
  (qq-contacts--set-view 'not-recent))

(defun qq-contacts--clear-search-results ()
  "Clear all result pages and cursors owned by the current search."
  (setq qq-contacts--search-errors nil
        qq-contacts--search-friends nil
        qq-contacts--search-groups nil
        qq-contacts--search-strangers nil
        qq-contacts--search-members nil
        qq-contacts--member-group-id nil
        qq-contacts--search-friend-cursor nil
        qq-contacts--search-group-cursor nil
        qq-contacts--search-stranger-cursor nil
        qq-contacts--search-member-cursor nil))

(defun qq-contacts--cancel-search ()
  "Cancel transport requests owned by the current native search."
  (setq qq-contacts--search-owner nil
        qq-contacts--search-pending nil)
  (when qq-contacts--search-friend-request
    (qq-api-cancel-request qq-contacts--search-friend-request))
  (when qq-contacts--search-group-request
    (qq-api-cancel-request qq-contacts--search-group-request))
  (when qq-contacts--search-stranger-request
    (qq-api-cancel-request qq-contacts--search-stranger-request))
  (when qq-contacts--search-member-request
    (qq-api-cancel-request qq-contacts--search-member-request))
  (setq qq-contacts--search-friend-request nil
        qq-contacts--search-group-request nil
        qq-contacts--search-stranger-request nil
        qq-contacts--search-member-request nil))

(defun qq-contacts--search-current-p (buffer owner kind)
  "Return non-nil when OWNER still owns search KIND in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-contacts-mode)
              (appkit-view-live-p (appkit-current-view))
              (eq owner qq-contacts--search-owner)
              (memq kind qq-contacts--search-pending)))))

(defconst qq-contacts--contact-search-identities
  '((kind user_id) (kind uid))
  "Independent exact identities carried by each native contact result.")

(defun qq-contacts--append-search-items
    (old new identity-fields &optional nullable-identity-fields)
  "Append NEW search items to OLD using unique IDENTITY-FIELDS.

An opaque native continuation must never repeat an identity already returned
by an earlier page.  Reject duplicates instead of silently replacing the old
row, because replacement would conceal a broken cursor snapshot.

Each element of IDENTITY-FIELDS is a field tuple that independently identifies
an item.  Contact pages therefore prove both `(kind,user_id)' and `(kind,uid)',
while group pages prove `(group_id)'.  A tuple listed in
NULLABLE-IDENTITY-FIELDS is skipped when any of its fields is nil; non-nil
values still have to be unique."
  (let ((identities
         (mapcar (lambda (fields)
                   (list fields
                         (make-hash-table :test #'equal)
                         (make-hash-table :test #'equal)))
                 identity-fields)))
    (cl-labels
        ((identity-key (item fields source)
           (let ((key (mapcar (lambda (field) (alist-get field item)) fields)))
             (if (cl-every #'identity key)
                 key
               (unless (member fields nullable-identity-fields)
                 (error "qq: %s search result lacks identity fields %s"
                        source (mapconcat #'symbol-name fields ",")))
               nil))))
      (dolist (item old)
        (dolist (identity identities)
          (let* ((fields (nth 0 identity))
                 (seen-old (nth 1 identity))
                 (key (identity-key item fields "existing")))
            (when key
              (when (gethash key seen-old)
                (error "qq: existing search results duplicate %s identity %S"
                       (mapconcat #'symbol-name fields ",") key))
              (puthash key t seen-old)))))
      (dolist (item new)
        (dolist (identity identities)
          (let* ((fields (nth 0 identity))
                 (seen-old (nth 1 identity))
                 (seen-new (nth 2 identity))
                 (key (identity-key item fields "continuation")))
            (when key
              (when (gethash key seen-old)
                (error "qq: search continuation repeated %s identity %S across pages"
                       (mapconcat #'symbol-name fields ",") key))
              (when (gethash key seen-new)
                (error "qq: search continuation duplicated %s identity %S"
                       (mapconcat #'symbol-name fields ",") key))
              (puthash key t seen-new))))))
    (append (copy-sequence old) (copy-tree new))))

(defun qq-contacts--finish-search-page (buffer owner kind append-p page)
  "Apply native search PAGE for KIND owned by OWNER in BUFFER.

When APPEND-P is non-nil, merge PAGE after prior pages by exact identity."
  (when (qq-contacts--search-current-p buffer owner kind)
    (with-current-buffer buffer
      (let ((items (alist-get 'results page))
            (cursor (alist-get 'next_cursor page))
            prepared
            prepared-p)
        ;; Prepare the whole page before changing any buffer state.  A repeated
        ;; cross-page identity is a terminal cursor violation, but it should
        ;; settle the loading row as an ordinary visible search error rather
        ;; than leaving the section permanently pending.
        (condition-case error-data
            (progn
              (setq prepared
                    (pcase kind
                      ('friends
                       (qq-contacts--append-search-items
                        (and append-p qq-contacts--search-friends)
                        items qq-contacts--contact-search-identities))
                      ('groups
                       (qq-contacts--append-search-items
                        (and append-p qq-contacts--search-groups)
                        (mapcar #'qq-contacts--search-group-object items)
                        '((group_id))))
                      ('strangers
                       (qq-contacts--append-search-items
                        (and append-p qq-contacts--search-strangers)
                        items '((user_id) (uid)) '((uid))))
                      ('members
                       (qq-contacts--append-search-items
                        (and append-p qq-contacts--search-members)
                        items qq-contacts--contact-search-identities))
                      (_ (error "qq: unknown native directory search kind %S"
                                kind)))
                    prepared-p t))
          (error
           (qq-contacts--fail-search-page
            buffer owner kind nil (error-message-string error-data))))
        (when prepared-p
          (pcase kind
            ('friends
             (setq qq-contacts--search-friends prepared
                   qq-contacts--search-friend-cursor cursor
                   qq-contacts--search-friend-request nil))
            ('groups
             (setq qq-contacts--search-groups prepared
                   qq-contacts--search-group-cursor cursor
                   qq-contacts--search-group-request nil))
            ('strangers
             (setq qq-contacts--search-strangers prepared
                   qq-contacts--search-stranger-cursor cursor
                   qq-contacts--search-stranger-request nil))
            ('members
             (setq qq-contacts--search-members prepared
                   qq-contacts--search-member-cursor cursor
                   qq-contacts--search-member-request nil)))
          (setq qq-contacts--search-errors
                (assq-delete-all kind qq-contacts--search-errors)
                qq-contacts--search-pending
                (delq kind qq-contacts--search-pending))
          (qq-contacts--request-reconcile))))))

(defun qq-contacts--fail-search-page (buffer owner kind _response reason)
  "Record native search failure REASON for KIND owned by OWNER in BUFFER."
  (when (qq-contacts--search-current-p buffer owner kind)
    (with-current-buffer buffer
      (pcase kind
        ('friends
         (setq qq-contacts--search-friend-request nil
               qq-contacts--search-friend-cursor nil))
        ('groups
         (setq qq-contacts--search-group-request nil
               qq-contacts--search-group-cursor nil))
        ('strangers
         (setq qq-contacts--search-stranger-request nil
               qq-contacts--search-stranger-cursor nil))
        ('members
         (setq qq-contacts--search-member-request nil
               qq-contacts--search-member-cursor nil)))
      (setq qq-contacts--search-errors
            (cons (cons kind reason)
                  (assq-delete-all kind qq-contacts--search-errors))
            qq-contacts--search-pending
            (delq kind qq-contacts--search-pending))
      (qq-contacts--request-reconcile))))

(defun qq-contacts--issue-search-request (kind cursor append-p)
  "Issue native search KIND, continuing opaque CURSOR when non-nil.

APPEND-P controls whether the resulting page extends existing entries."
  (let* ((buffer (current-buffer))
         (owner qq-contacts--search-owner)
         (query qq-contacts--query)
         (success (apply-partially #'qq-contacts--finish-search-page
                                   buffer owner kind append-p))
         (failure (apply-partially #'qq-contacts--fail-search-page
                                   buffer owner kind)))
    (condition-case error-data
        (let ((request
               (pcase kind
                 ('friends
                  (if cursor
                      (qq-api-search-contacts-next
                       'friends cursor query success failure nil 50)
                    (qq-api-search-contacts-start
                     'friends query success failure nil 50)))
                 ('groups
                  (if cursor
                      (qq-api-search-group-chats-next
                       cursor query success failure 'default 50 nil)
                    (qq-api-search-group-chats-start
                     query success failure 'default 50 nil)))
                 ('strangers
                  (if cursor
                      (qq-api-search-strangers-next
                       cursor query success failure 50)
                    (qq-api-search-strangers-start
                     query success failure 50)))
                 ('members
                  (if cursor
                      (qq-api-search-contacts-next
                       'group-members cursor query success failure
                       qq-contacts--member-group-id 50)
                    (qq-api-search-contacts-start
                     'group-members query success failure
                     qq-contacts--member-group-id 50)))
                 (_ (error "qq: unknown native directory search kind %S" kind)))))
          (when (qq-contacts--search-current-p buffer owner kind)
            (pcase kind
              ('friends (setq qq-contacts--search-friend-request request))
              ('groups (setq qq-contacts--search-group-request request))
              ('strangers
               (setq qq-contacts--search-stranger-request request))
              ('members (setq qq-contacts--search-member-request request)))))
      (error
       (qq-contacts--fail-search-page
        buffer owner kind nil (error-message-string error-data))))))

(defun qq-contacts-search (query &optional scope)
  "Search exact native directory SCOPE for QUERY.

SCOPE is one of `all', `contacts', `friends', `groups', or `strangers'."
  (interactive
   (let ((selected (qq-contacts--read-search-scope)))
     (list (read-string
            (format "在%s中搜索: "
                    (qq-contacts--search-scope-label selected))
            qq-contacts--query 'qq-contacts-search-history)
           selected)))
  (setq scope (or scope 'all))
  (let ((kinds (qq-contacts--search-kinds scope)))
    (setq query (string-trim (or query "")))
    ;; Validate the strictest selected owner before cancelling or replacing the
    ;; current search.  The API repeats this check at the transport boundary.
    (when (and (not (string-empty-p query)) (memq 'strangers kinds))
      (qq-api--stranger-search-owner-params query 50))
    (qq-contacts--ensure-view)
    (if (string-empty-p query)
        (when (memq qq-contacts--view '(search members))
          (qq-contacts-clear-search))
      (unless (memq qq-contacts--view '(search members))
        (setq qq-contacts--previous-view qq-contacts--view))
      (qq-contacts--cancel-search)
      (qq-contacts--clear-search-results)
      (setq qq-contacts--query query
            qq-contacts--search-scope scope
            qq-contacts--view 'search
            qq-contacts--search-owner
            (list 'native-directory-search scope query)
            qq-contacts--search-pending (copy-sequence kinds))
      (qq-contacts--request-reconcile)
      (dolist (kind kinds)
        (qq-contacts--issue-search-request kind nil nil)))))

(defun qq-contacts--load-more (kind)
  "Load the next exact native search page for KIND."
  (qq-contacts--ensure-view)
  (unless (and (memq qq-contacts--view '(search members))
               qq-contacts--search-owner)
    (user-error "qq: there is no active directory search"))
  (let ((cursor (pcase kind
                  ('friends qq-contacts--search-friend-cursor)
                  ('groups qq-contacts--search-group-cursor)
                  ('strangers qq-contacts--search-stranger-cursor)
                  ('members qq-contacts--search-member-cursor))))
    (unless (qq-api-non-empty-string-p cursor)
      (user-error "qq: this search section has no next page"))
    (when (memq kind qq-contacts--search-pending)
      (user-error "qq: this search section is already loading"))
    (setq qq-contacts--search-errors
          (assq-delete-all kind qq-contacts--search-errors)
          qq-contacts--search-pending
          (cons kind qq-contacts--search-pending))
    (pcase kind
      ('friends (setq qq-contacts--search-friend-cursor nil))
      ('groups (setq qq-contacts--search-group-cursor nil))
      ('strangers (setq qq-contacts--search-stranger-cursor nil))
      ('members (setq qq-contacts--search-member-cursor nil)))
    (qq-contacts--request-reconcile)
    (qq-contacts--issue-search-request kind cursor t)))

(defun qq-contacts-load-more-friends ()
  "Load the next native friend-search page."
  (interactive)
  (qq-contacts--load-more 'friends))

(defun qq-contacts-load-more-groups ()
  "Load the next native group-chat-search page."
  (interactive)
  (qq-contacts--load-more 'groups))

(defun qq-contacts-load-more-strangers ()
  "Load the next native global-user search page."
  (interactive)
  (qq-contacts--load-more 'strangers))

(defun qq-contacts-load-more-members ()
  "Load the next native group-member-search page."
  (interactive)
  (qq-contacts--load-more 'members))

;;;###autoload
(defun qq-contacts-search-group-members (group-id query)
  "Open exact native member search for GROUP-ID and QUERY."
  (interactive
   (list (read-string "群号: ")
         (read-string "搜索群成员: " nil 'qq-contacts-search-history)))
  (unless (qq-api-group-id-p group-id)
    (user-error "qq: group member search requires a canonical uint32 group UIN"))
  (setq query (string-trim (or query "")))
  (when (string-empty-p query)
    (user-error "qq: group member search query cannot be empty"))
  (let ((buffer (get-buffer-create qq-contacts-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-contacts-mode)
        (qq-contacts-mode))
      (qq-contacts--ensure-view)
      (unless (memq qq-contacts--view '(search members))
        (setq qq-contacts--previous-view qq-contacts--view))
      (qq-contacts--cancel-search)
      (qq-contacts--clear-search-results)
      (setq qq-contacts--view 'members
            qq-contacts--query query
            qq-contacts--member-group-id group-id
            qq-contacts--search-owner
            (list 'native-group-member-search group-id query)
            qq-contacts--search-pending '(members))
      (qq-contacts--request-reconcile)
      (qq-contacts--issue-search-request 'members nil nil))
    (pop-to-buffer buffer)
    buffer))

(defun qq-contacts-clear-search ()
  "Clear directory search and restore its prior view."
  (interactive)
  (when (memq qq-contacts--view '(search members))
    (qq-contacts--cancel-search)
    (qq-contacts--clear-search-results)
    (setq qq-contacts--query nil
          qq-contacts--search-scope 'all
          qq-contacts--view
          (if (memq qq-contacts--previous-view '(search members))
              'friends
            qq-contacts--previous-view))
    (qq-contacts--request-reconcile)))

(defun qq-contacts--line-property (property)
  "Return line PROPERTY at point."
  (or (get-text-property (point) property)
      (get-text-property (line-beginning-position) property)))

(defun qq-contacts-toggle-category (&optional category-id)
  "Toggle friend CATEGORY-ID or the category at point."
  (interactive)
  (setq category-id
        (or category-id (qq-contacts--line-property 'qq-contacts-category-id)))
  (unless (integerp category-id)
    (user-error "qq: point is not on a friend category"))
  (if (gethash category-id qq-contacts--collapsed-categories)
      (remhash category-id qq-contacts--collapsed-categories)
    (puthash category-id t qq-contacts--collapsed-categories))
  (qq-contacts--request-reconcile))

(defun qq-contacts--activate-entry (entry)
  "Activate exact directory ENTRY stored by an action row."
  (pcase (qq-contacts--entry-type entry)
    ('category
     (qq-contacts-toggle-category
      (alist-get 'category_id (qq-contacts--entry-object entry))))
    ((or 'friend 'member)
     (qq-chat-open
      (qq-state-session-key
       'private (alist-get 'user_id (qq-contacts--entry-object entry)))))
    ('stranger
     (qq-user-open-search-result (qq-contacts--entry-object entry)))
    ('group
     (qq-chat-open
      (qq-state-session-key
       'group (alist-get 'group_id (qq-contacts--entry-object entry)))))
    (_ (user-error "qq: this directory row is not actionable"))))

(defun qq-contacts-open-at-point ()
  "Open or toggle the exact directory row at point."
  (interactive)
  (if-let* ((button (button-at (point))))
      (push-button button)
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ('category (qq-contacts-toggle-category))
      ((or 'friend 'member)
       (let ((friend (qq-contacts--object-at-point)))
         (qq-chat-open
          (qq-state-session-key 'private (alist-get 'user_id friend)))))
      ('stranger
       (qq-user-open-search-result (qq-contacts--object-at-point)))
      ('group
       (let ((group (qq-contacts--object-at-point)))
         (qq-chat-open
          (qq-state-session-key 'group (alist-get 'group_id group)))))
      (_ (user-error "qq: point is not on a directory item")))))

(defun qq-contacts-mouse-open-at-point (event)
  "Open the directory row selected by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (qq-contacts-open-at-point))

(defun qq-contacts--object-at-point ()
  "Return the exact directory object represented at point."
  (or (qq-contacts--line-property 'qq-contacts-object)
      (user-error "qq: point is not on a directory item")))

(defun qq-contacts-open-info-at-point ()
  "Open native user or group details for the item at point."
  (interactive)
  (let ((object (qq-contacts--object-at-point)))
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ((or 'friend 'member) (qq-user-open (alist-get 'user_id object)))
      ('stranger (qq-user-open-search-result object))
      ('group (qq-group-open (alist-get 'group_id object)))
      (_ (user-error "qq: point has no profile page")))))

(defun qq-contacts-open-avatar-at-point ()
  "Open the native avatar for the item at point."
  (interactive)
  (let ((object (qq-contacts--object-at-point)))
    (pcase (qq-contacts--line-property 'qq-contacts-row-type)
      ((or 'friend 'member)
       (qq-media-open-user-avatar (alist-get 'user_id object)))
      ('stranger
       (qq-media-open-image-url
        (format "avatar:%s" (alist-get 'user_id object))
        (alist-get 'avatar_url object)))
      ('group (qq-media-open-group-avatar (alist-get 'group_id object)))
      (_ (user-error "qq: point has no avatar")))))

(defun qq-contacts-copy-id-at-point ()
  "Copy the exact QQ or group identity at point."
  (interactive)
  (let* ((object (qq-contacts--object-at-point))
         (id (pcase (qq-contacts--line-property 'qq-contacts-row-type)
               ((or 'friend 'member 'stranger)
                (alist-get 'user_id object))
               ('group (alist-get 'group_id object)))))
    (unless (stringp id)
      (user-error "qq: point has no exact identity"))
    (kill-new id)
    (message "qq: copied %s" id)))

(defun qq-contacts-add-friend-at-point ()
  "Open the global user at point and begin its add-friend flow."
  (interactive)
  (unless (eq (qq-contacts--line-property 'qq-contacts-row-type) 'stranger)
    (user-error "qq: point is not on a global QQ user result"))
  (qq-user-open-search-result (qq-contacts--object-at-point))
  (qq-user-add-friend))

(defun qq-contacts--item-positions ()
  "Return ordered buffer positions of actionable directory rows."
  (let ((position (point-min)) positions)
    (while (< position (point-max))
      (when (get-text-property position 'qq-contacts-item-id)
        (push position positions))
      (setq position
            (next-single-property-change
             position 'qq-contacts-item-id nil (point-max))))
    (nreverse (seq-uniq positions #'=))))

(defun qq-contacts--move-item (direction)
  "Move to next item in DIRECTION, where positive means forward."
  (let* ((positions (qq-contacts--item-positions))
         (origin (line-beginning-position))
         (target
          (if (> direction 0)
              (seq-find (lambda (position) (> position origin)) positions)
            (car (last (seq-filter
                        (lambda (position) (< position origin)) positions))))))
    (if target
        (goto-char target)
      (message "qq: no %s directory item"
               (if (> direction 0) "next" "previous")))))

(defun qq-contacts-next-item ()
  "Move to the next friend or group row."
  (interactive)
  (qq-contacts--move-item 1))

(defun qq-contacts-previous-item ()
  "Move to the previous friend or group row."
  (interactive)
  (qq-contacts--move-item -1))

(defun qq-contacts-button-backward ()
  "Move to the previous real directory button."
  (interactive)
  (forward-button -1))

(defun qq-contacts-open-root ()
  "Return to the emacs-qq root buffer."
  (interactive)
  (qq-root-open))

(defun qq-contacts--cancel-refresh ()
  "Cancel requests owned by the current directory refresh."
  (setq qq-contacts--refresh-owner nil
        qq-contacts--refresh-pending 0
        qq-contacts--refresh-parts nil
        qq-contacts--loading nil)
  (when qq-contacts--friend-request
    (qq-api-cancel-request qq-contacts--friend-request))
  (when qq-contacts--group-request
    (qq-api-cancel-request qq-contacts--group-request))
  (setq qq-contacts--friend-request nil
        qq-contacts--group-request nil))

(defun qq-contacts--refresh-current-p (buffer owner)
  "Return non-nil when OWNER still owns refresh in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-contacts-mode)
              (appkit-view-live-p (appkit-current-view))
              (eq owner qq-contacts--refresh-owner)))))

(defun qq-contacts--refresh-part-current-p (buffer owner kind)
  "Return non-nil when OWNER still owns refresh KIND in BUFFER."
  (and (qq-contacts--refresh-current-p buffer owner)
       (with-current-buffer buffer
         (memq kind qq-contacts--refresh-parts))))

(defun qq-contacts--finish-refresh-part (buffer owner kind &optional reason)
  "Finish one refresh KIND owned by OWNER in BUFFER, recording REASON."
  (when (qq-contacts--refresh-part-current-p buffer owner kind)
    (with-current-buffer buffer
      (pcase kind
        ('friends (setq qq-contacts--friend-request nil))
        ('groups (setq qq-contacts--group-request nil)))
      (when reason
        (setq qq-contacts--error
              (if qq-contacts--error
                  (concat qq-contacts--error " · " reason)
                reason)))
      (setq qq-contacts--refresh-parts
            (delq kind qq-contacts--refresh-parts))
      (setq qq-contacts--refresh-pending
            (length qq-contacts--refresh-parts))
      (when (= qq-contacts--refresh-pending 0)
        (setq qq-contacts--refresh-owner nil
              qq-contacts--loading nil))
      (qq-contacts--request-reconcile))))

(defun qq-contacts-refresh ()
  "Refresh exact friend categories and joined groups from Linux QQ."
  (interactive)
  (qq-contacts--ensure-view)
  (qq-contacts--cancel-refresh)
  (let ((buffer (current-buffer))
        (owner (list 'contacts-refresh (float-time))))
    (setq qq-contacts--refresh-owner owner
          qq-contacts--refresh-pending 2
          qq-contacts--refresh-parts '(friends groups)
          qq-contacts--loading t
          qq-contacts--error nil)
    (qq-contacts--request-reconcile)
    (condition-case error-data
        (let ((request
               (qq-api-refresh-friend-categories
                (lambda (_categories)
                  (qq-contacts--finish-refresh-part
                   buffer owner 'friends))
                (lambda (_response reason)
                  (qq-contacts--finish-refresh-part
                   buffer owner 'friends reason)))))
          (when (qq-contacts--refresh-part-current-p buffer owner 'friends)
            (setq qq-contacts--friend-request request)))
      (error
       (qq-contacts--finish-refresh-part
        buffer owner 'friends (error-message-string error-data))))
    (condition-case error-data
        (let ((request
               (qq-api-refresh-joined-groups
                (lambda (_groups)
                  (qq-contacts--finish-refresh-part
                   buffer owner 'groups))
                (lambda (_response reason)
                  (qq-contacts--finish-refresh-part
                   buffer owner 'groups reason)))))
          (when (qq-contacts--refresh-part-current-p buffer owner 'groups)
            (setq qq-contacts--group-request request)))
      (error
       (qq-contacts--finish-refresh-part
        buffer owner 'groups (error-message-string error-data))))))

(defun qq-contacts--handle-state-change (event)
  "Invalidate the open directory after relevant state EVENT."
  (when (memq (plist-get event :type)
              '(reset friends-refreshed groups-refreshed sessions-refreshed))
    (when-let* ((buffer (get-buffer qq-contacts-buffer-name)))
      (with-current-buffer buffer
        (let ((view (appkit-current-view)))
          (when (and (derived-mode-p 'qq-contacts-mode)
                     (appkit-view-live-p view))
            (qq-contacts--queue-view-sync view)))))))

(defun qq-contacts--handle-media-cache-update (media-key)
  "Invalidate the directory row identified by avatar MEDIA-KEY."
  (when (stringp media-key)
    (let (keys)
      (cond
       ((string-match "\\`avatar:\\([1-9][0-9]*\\)\\'" media-key)
        (setq keys (list (cons 'friend (match-string 1 media-key))
                         (cons 'member (match-string 1 media-key))
                         (cons 'stranger (match-string 1 media-key)))))
      ((string-match "\\`group-avatar:\\([1-9][0-9]*\\)\\'" media-key)
        (setq keys (list (cons 'group (match-string 1 media-key))))))
      (when keys
        (let ((buffer (get-buffer qq-contacts-buffer-name)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (let ((view (appkit-current-view)))
                (when (and (derived-mode-p 'qq-contacts-mode)
                           (appkit-view-live-p view))
                  (qq-contacts--queue-view-sync view keys))))))))))

(defvar qq-contacts-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-contacts-refresh)
    (define-key map (kbd "f") #'qq-contacts-show-friends)
    (define-key map (kbd "G") #'qq-contacts-show-groups)
    (define-key map (kbd "I") #'qq-contacts-show-not-recent-groups)
    (define-key map (kbd "/") #'qq-contacts-search)
    (define-key map (kbd "s") #'qq-contacts-search)
    (define-key map (kbd "C-c C-k") #'qq-contacts-clear-search)
    (define-key map (kbd "RET") #'qq-contacts-open-at-point)
    (define-key map (kbd "m") #'qq-contacts-open-at-point)
    (define-key map (kbd "i") #'qq-contacts-open-info-at-point)
    (define-key map (kbd "+") #'qq-contacts-add-friend-at-point)
    (define-key map (kbd "a") #'qq-contacts-open-avatar-at-point)
    (define-key map (kbd "w") #'qq-contacts-copy-id-at-point)
    (define-key map (kbd "t") #'qq-contacts-toggle-category)
    (define-key map (kbd "n") #'qq-contacts-next-item)
    (define-key map (kbd "p") #'qq-contacts-previous-item)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-contacts-button-backward)
    (define-key map (kbd "b") #'qq-contacts-open-root)
    (define-key map [mouse-1] #'qq-contacts-mouse-open-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-contacts-mode'.")

(define-derived-mode qq-contacts-mode special-mode "QQ-Contacts"
  "Major mode for the native QQ contacts directory."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local qq-contacts--node-table (make-hash-table :test #'equal))
  (setq-local qq-contacts--collapsed-categories
              (make-hash-table :test #'eql))
  (setq-local qq-contacts--view 'friends)
  (setq-local qq-contacts--previous-view 'friends)
  (setq-local qq-contacts--query nil)
  (setq-local qq-contacts--search-scope 'all)
  (setq-local qq-contacts--fill-column nil)
  (setq-local qq-contacts--header-line-cache "")
  (setq-local qq-contacts--rendering nil)
  (setq-local qq-contacts--render-pending nil)
  (setq-local qq-contacts--dirty nil)
  (setq-local qq-contacts--pending-force-keys nil)
  (setq-local qq-contacts--loading nil)
  (setq-local qq-contacts--error nil)
  (setq-local qq-contacts--refresh-owner nil)
  (setq-local qq-contacts--refresh-pending 0)
  (setq-local qq-contacts--refresh-parts nil)
  (setq-local qq-contacts--friend-request nil)
  (setq-local qq-contacts--group-request nil)
  (setq-local qq-contacts--search-owner nil)
  (setq-local qq-contacts--search-pending nil)
  (setq-local qq-contacts--search-errors nil)
  (setq-local qq-contacts--search-friends nil)
  (setq-local qq-contacts--search-groups nil)
  (setq-local qq-contacts--search-strangers nil)
  (setq-local qq-contacts--search-members nil)
  (setq-local qq-contacts--member-group-id nil)
  (setq-local qq-contacts--search-friend-cursor nil)
  (setq-local qq-contacts--search-group-cursor nil)
  (setq-local qq-contacts--search-stranger-cursor nil)
  (setq-local qq-contacts--search-member-cursor nil)
  (setq-local qq-contacts--search-friend-request nil)
  (setq-local qq-contacts--search-group-request nil)
  (setq-local qq-contacts--search-stranger-request nil)
  (setq-local qq-contacts--search-member-request nil)
  (setq-local header-line-format 'qq-contacts--header-line-cache)
  (setq-local revert-buffer-function
              (lambda (&rest _ignored) (qq-contacts-refresh)))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local qq-contacts--ewoc
                (ewoc-create #'qq-contacts--ewoc-printer nil nil t)))
  (add-hook 'window-buffer-change-functions
            #'qq-contacts--window-buffer-change nil t)
  (add-hook 'window-size-change-functions
            #'qq-contacts--window-size-change nil t)
  (add-hook 'window-scroll-functions #'qq-contacts--window-scroll nil t)
  (add-hook 'change-major-mode-hook #'qq-contacts--cancel-refresh nil t)
  (add-hook 'change-major-mode-hook #'qq-contacts--cancel-search nil t)
  (add-hook 'kill-buffer-hook #'qq-contacts--cancel-refresh nil t)
  (add-hook 'kill-buffer-hook #'qq-contacts--cancel-search nil t))

;;;###autoload
(defun qq-contacts-open ()
  "Open the persistent native QQ contacts directory."
  (interactive)
  (let* ((app (qq-runtime-app))
         (fresh-p (null (appkit-view-for-id app qq-contacts--view-id)))
         (view
          (appkit-open-view
           :app app
           :id qq-contacts--view-id
           :mode 'qq-contacts-mode
           :buffer-name qq-contacts-buffer-name
           :sync-function #'qq-contacts--sync-invalidations
           :parts '(directory)
           :setup #'qq-contacts--setup-view
           :select t))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (appkit-invalidate view :structure t :part 'directory)
      (if fresh-p
          (appkit-sync-invalidations view)
        (appkit-schedule-sync view))
      (when (and (not qq-contacts--loading)
                 (or (not (qq-state-friend-categories-loaded-p))
                     (not (qq-state-groups-loaded-p)))
                 (eq (qq-state-connection-status) 'ready))
        (qq-contacts-refresh)))
    buffer))

(add-hook 'qq-state-change-hook #'qq-contacts--handle-state-change)
(add-hook 'qq-media-cache-update-hook #'qq-contacts--handle-media-cache-update)

(provide 'qq-contacts)

;;; qq-contacts.el ends here
