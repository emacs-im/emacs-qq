;;; qq-completion.el --- Composer completion for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; QQ-specific providers and protocol insertion built on appkit's generic chat
;; completion substrate.  Group members come from the strict fork-native
;; `emacs_search_group_members' action; faces remain structured send segments.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'qq-api)
(require 'qq-media)
(require 'qq-state)

(declare-function qq-chat--insert-input-segment-object
                  "qq-chat" (segment &optional visible-label))
(declare-function qq-chat--sync-draft-from-buffer "qq-chat" ())

(defvar-local qq-chat--session-key)

(defconst qq-completion--cache-miss (make-symbol "qq-completion-cache-miss"))

(defvar-local qq-completion--member-cache nil
  "Query -> native group member list for the current chat buffer.")

(defvar-local qq-completion--member-pending nil
  "Query -> request metadata for in-flight native member searches.")

(defvar-local qq-completion--custom-face-pending nil
  "Current room-owned favorite-face cache request, or nil.")

(defvar qq-completion--face-history nil
  "History for shared QQ base-face completion readers.")

(defvar qq-completion--custom-face-history nil
  "History for shared QQ favorite-face completion readers.")

(defvar qq-completion--poke-target-history nil
  "History for strict poke-target candidate readers.")

(defvar qq-completion--poke-search-history nil
  "History for native group-member poke searches.")

(defvar-local qq-completion--poke-request nil
  "Active asynchronous poke-target search owner, or nil.

The value is a private plist carrying the transport token, scheduled picker
timer, and session identity.  It is deliberately separate from
`qq-completion--member-pending', whose continuation reopens composer
completion rather than choosing a command argument.")

(defun qq-completion--group-id ()
  "Return current group id, or nil outside a group chat buffer."
  (let ((session (and qq-chat--session-key
                      (qq-state-session qq-chat--session-key))))
    (when (eq (alist-get 'type session) 'group)
      (or (alist-get 'target-id session)
          (qq-state-session-key-target-id qq-chat--session-key)))))

(defun qq-completion--member-token ()
  "Return current group @mention token, or nil."
  (and (qq-completion--group-id)
       (appkit-chat-completion-token-bounds ?@)))

(defun qq-completion-member-token-at-point ()
  "Return unresolved native group @mention token at point, or nil.

Accepted mentions are structured composer objects and therefore never match
this function.  It is intended for submit guards such as
`qq-chat-return-dwim'."
  (qq-completion--member-token))

(defun qq-completion--face-token ()
  "Return the current `/face' composer token, or nil."
  (appkit-chat-completion-token-bounds ?/))

(defun qq-completion--unicode-emoji-token ()
  "Return the current `:emoji:' composer token, or nil."
  (appkit-chat-completion-delimited-token-bounds ?:))

(defun qq-completion--member-field-values (member)
  "Return searchable non-empty values from native MEMBER."
  (seq-filter
   (lambda (value) (and (stringp value) (not (string-empty-p value))))
   (mapcar (lambda (key) (alist-get key member))
           '(card nickname remark qid user_id))))

(defun qq-completion--member-display-name (member)
  "Return preferred visible name for native MEMBER."
  (or (seq-find
       (lambda (value) (and (stringp value) (not (string-empty-p value))))
       (mapcar (lambda (key) (alist-get key member))
               '(card nickname remark qid user_id)))
      "QQ member"))

(defun qq-completion--member-role-label (role)
  "Return compact Chinese label for native member ROLE."
  (pcase role
    ("owner" "群主")
    ("admin" "管理员")
    ("stranger" "陌生人")
    (_ nil)))

(defun qq-completion--member-annotation (member)
  "Return rich completion annotation for native MEMBER."
  (let* ((display (qq-completion--member-display-name member))
         (nickname (alist-get 'nickname member))
         (user-id (alist-get 'user_id member))
         (title (alist-get 'title member))
         (role (qq-completion--member-role-label (alist-get 'role member)))
         (parts
          (delq nil
                (list (and (stringp nickname)
                           (not (string-empty-p nickname))
                           (not (equal nickname display))
                           nickname)
                      role
                      (and (stringp title) (not (string-empty-p title)) title)
                      (and (alist-get 'robot member) "机器人")
                      (and user-id (format "QQ %s" user-id))))))
    (if parts (concat "  " (string-join parts " · ")) "")))

(defun qq-completion--member-prefix (member)
  "Return avatar prefix for native MEMBER."
  (let ((user-id (alist-get 'user_id member)))
    (if user-id
        (concat (qq-media-avatar-display-string user-id) " ")
      "  ")))

(defun qq-completion--member-candidates (members)
  "Return appkit candidates for native MEMBERS, preserving server order."
  (let ((seen (make-hash-table :test #'equal))
        candidates)
    (dolist (member members)
      (let* ((display (qq-completion--member-display-name member))
             (base (concat "@" display))
             (count (1+ (gethash base seen 0)))
             (user-id (alist-get 'user_id member))
             (label (if (= count 1)
                        base
                      (format "%s · %s" base user-id)))
             (segment
              `((type . "at")
                (data . ((qq . ,user-id) (name . ,display))))))
        (puthash base count seen)
        (push
         (appkit-chat-completion-candidate-create
          :label label
          :value `(:kind member :member ,member :segment ,segment)
          :search-terms (qq-completion--member-field-values member)
          :prefix (lambda (_candidate)
                    (qq-completion--member-prefix member))
          :annotation (lambda (_candidate)
                        (qq-completion--member-annotation member)))
         candidates)))
    (nreverse candidates)))

(defun qq-completion--filter-members (members query)
  "Return MEMBERS locally matching QUERY."
  (let ((needle (downcase (string-trim (or query "")))))
    (if (string-empty-p needle)
        members
      (seq-filter
       (lambda (member)
         (seq-some
          (lambda (value)
            (string-match-p (regexp-quote needle) (downcase value)))
          (qq-completion--member-field-values member)))
       members))))

(defun qq-completion--request-current-p (buffer session-key group-id)
  "Return non-nil when BUFFER still owns SESSION-KEY and GROUP-ID."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key session-key)
              (equal (qq-completion--group-id) group-id)))))

(defun qq-completion--maybe-reopen-member-completion
    (buffer session-key group-id query)
  "Reopen member completion if BUFFER still has QUERY at point."
  (when (qq-completion--request-current-p buffer session-key group-id)
    (when-let* ((window (get-buffer-window buffer t)))
      (with-selected-window window
        (when-let* ((token (qq-completion--member-token)))
          (when (equal query (plist-get token :query))
            (completion-at-point)))))))

(defun qq-completion--request-members (query &optional reopen)
  "Request native group members for QUERY.

When REOPEN is non-nil, reopen completion after a still-current response."
  (let* ((group-id (qq-completion--group-id))
         (pending (and group-id
                       (gethash query qq-completion--member-pending))))
    (when group-id
      (if pending
          (when reopen
            (puthash query (plist-put pending :reopen t)
                     qq-completion--member-pending))
        (let ((buffer (current-buffer))
              (session-key qq-chat--session-key)
              (owner (list :reopen reopen)))
          (puthash query owner qq-completion--member-pending)
          (qq-api-search-group-members
           group-id query
           (lambda (members)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (and (hash-table-p qq-completion--member-pending)
                            (eq owner
                                (gethash query
                                         qq-completion--member-pending)))
                   (remhash query qq-completion--member-pending)
                   (when (qq-completion--request-current-p
                          buffer session-key group-id)
                     (puthash query members qq-completion--member-cache)
                     (when (plist-get owner :reopen)
                       (run-at-time
                        0 nil #'qq-completion--maybe-reopen-member-completion
                        buffer session-key group-id query)))))))
           (lambda (_response reason)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (and (hash-table-p qq-completion--member-pending)
                            (eq owner
                                (gethash query
                                         qq-completion--member-pending)))
                   (remhash query qq-completion--member-pending)
                   (when (qq-completion--request-current-p
                          buffer session-key group-id)
                     (message "qq: failed to search group members: %s"
                              reason))))))
           200))))))

(defun qq-completion--cached-members (query)
  "Return cached members for QUERY, or `qq-completion--cache-miss'."
  (gethash query qq-completion--member-cache qq-completion--cache-miss))

(defun qq-completion--member-fallback (query)
  "Return locally filtered broad member cache for QUERY."
  (let ((broad (qq-completion--cached-members "")))
    (unless (eq broad qq-completion--cache-miss)
      (qq-completion--filter-members broad query))))

(defun qq-completion-member-capf ()
  "CAPF for native QQ group @mentions."
  (when-let* ((token (qq-completion--member-token)))
    (let* ((query (plist-get token :query))
           (cached (qq-completion--cached-members query))
           (members (if (eq cached qq-completion--cache-miss)
                        (qq-completion--member-fallback query)
                      cached)))
      (when (eq cached qq-completion--cache-miss)
        (qq-completion--request-members query))
      (when members
        (appkit-chat-completion-capf
         (plist-get token :start)
         (plist-get token :end)
         (qq-completion--member-candidates members)
         :insert-function #'qq-completion--insert-protocol-candidate
         :suffix " "
         :sync-function #'qq-chat--sync-draft-from-buffer)))))

(defun qq-completion--poke-user-id-p (value)
  "Return non-nil when VALUE is a canonical nonzero poke user id."
  (and (qq-api-user-id-p value)
       (not (equal value "0"))))

(defun qq-completion--poke-session-current-p
    (buffer session-key type target-id)
  "Return non-nil when BUFFER still owns canonical SESSION-KEY fields."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((session (qq-state-session session-key)))
           (and (derived-mode-p 'qq-chat-mode)
                (equal qq-chat--session-key session-key)
                (eq (alist-get 'type session) type)
                (equal (alist-get 'target-id session) target-id))))))

(defun qq-completion--poke-request-current-p (buffer owner)
  "Return non-nil when OWNER still owns BUFFER's poke-target request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq qq-completion--poke-request owner)
              (qq-completion--poke-session-current-p
               buffer (plist-get owner :session-key)
               'group (plist-get owner :group-id))))))

(defun qq-completion--clear-poke-owner (buffer owner)
  "Forget OWNER in BUFFER without disturbing a newer poke request."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (eq qq-completion--poke-request owner)
        (setq qq-completion--poke-request nil)))))

(defun qq-completion--cancel-poke-request ()
  "Cancel and forget the current buffer's poke-target search."
  (when qq-completion--poke-request
    (when-let* ((timer (plist-get qq-completion--poke-request :timer)))
      (when (timerp timer)
        (cancel-timer timer)))
    (when-let* ((token (plist-get qq-completion--poke-request :token)))
      (qq-api-cancel-request token)))
  (setq qq-completion--poke-request nil))

(defun qq-completion--poke-candidate-user-id (candidate)
  "Return canonical original user id carried by CANDIDATE."
  (let* ((value (appkit-chat-completion-candidate-value candidate))
         (member (plist-get value :member))
         (user-id (or (plist-get value :user-id)
                      (and (listp member) (alist-get 'user_id member)))))
    (unless (qq-completion--poke-user-id-p user-id)
      (error "qq: poke target candidate has invalid user id: %S" user-id))
    user-id))

(defun qq-completion--private-poke-candidate (user-id name relation)
  "Return a rich private poke candidate for USER-ID, NAME, and RELATION."
  (unless (qq-completion--poke-user-id-p user-id)
    (error "qq: private poke candidate has invalid user id: %S" user-id))
  (let ((user-id user-id)
        (name name)
        (relation relation))
    (appkit-chat-completion-candidate-create
     :label (format "%s · %s" relation name)
     :value `(:kind poke-target :user-id ,user-id)
     :search-terms (delete-dups (list relation name user-id))
     :prefix (lambda (_candidate)
               (concat (qq-media-avatar-display-string user-id) " "))
     :annotation (format "  QQ %s" user-id))))

(defun qq-completion--private-poke-candidates (session)
  "Return the strict peer/self poke candidates for private SESSION."
  (let* ((peer-id (alist-get 'target-id session))
         (self-info (qq-state-self-info))
         (self-id (qq-state-self-user-id))
         (peer-name (or (alist-get 'title session) peer-id))
         (self-name (or (alist-get 'nickname self-info) self-id)))
    (unless (qq-completion--poke-user-id-p peer-id)
      (user-error "qq: private poke requires a canonical peer id"))
    (unless (qq-completion--poke-user-id-p self-id)
      (user-error "qq: private poke requires loaded self identity"))
    (if (equal peer-id self-id)
        (list (qq-completion--private-poke-candidate
               self-id self-name "Me"))
      (list (qq-completion--private-poke-candidate
             peer-id peer-name "Peer")
            (qq-completion--private-poke-candidate
             self-id self-name "Me")))))

(defun qq-completion--read-private-poke-target
    (buffer session-key session callback initial-user-id)
  "Read a strict private poke target and call CALLBACK with its user id."
  (let* ((candidates (qq-completion--private-poke-candidates session))
         (initial-candidate
          (and initial-user-id
               (seq-find
                (lambda (candidate)
                  (equal (qq-completion--poke-candidate-user-id candidate)
                         initial-user-id))
                candidates))))
    (when (and initial-user-id (null initial-candidate))
      (user-error "qq: private poke target must be the peer or self"))
    (let ((candidate
           (appkit-chat-completion-read
            "Poke target: " candidates
            :history 'qq-completion--poke-target-history
            :initial-input
            (and initial-candidate
                 (appkit-chat-completion-candidate-label initial-candidate)))))
      (unless (qq-completion--poke-session-current-p
               buffer session-key 'private (alist-get 'target-id session))
        (user-error "qq: poke target belongs to a stale chat session"))
      (funcall callback
               (qq-completion--poke-candidate-user-id candidate)))))

(defun qq-completion--group-poke-candidates (members)
  "Return strict rich poke candidates for native group MEMBERS."
  (dolist (member members)
    (unless (qq-completion--poke-user-id-p (alist-get 'user_id member))
      (error "qq: group poke search returned invalid user id: %S"
             (alist-get 'user_id member))))
  (qq-completion--member-candidates members))

(defun qq-completion--present-group-poke-targets (buffer owner members)
  "Present native MEMBERS for current group poke request OWNER in BUFFER."
  (if (not (qq-completion--poke-request-current-p buffer owner))
      (qq-completion--clear-poke-owner buffer owner)
    (with-current-buffer buffer
      (setf (plist-get owner :timer) nil)
      (if (active-minibuffer-window)
          (setf (plist-get owner :timer)
                (run-at-time
                 0.2 nil #'qq-completion--present-group-poke-targets
                 buffer owner members))
        (unwind-protect
            (condition-case nil
                (let* ((candidates
                        (qq-completion--group-poke-candidates members))
                       (candidate
                        (appkit-chat-completion-read
                         "Poke group member: " candidates
                         :history 'qq-completion--poke-target-history))
                       (user-id
                        (qq-completion--poke-candidate-user-id candidate)))
                  (when (qq-completion--poke-request-current-p buffer owner)
                    (funcall (plist-get owner :callback) user-id)))
              (quit nil))
          (qq-completion--clear-poke-owner buffer owner))))))

(defun qq-completion--group-poke-search-succeeded (buffer owner members)
  "Accept native MEMBERS for current group poke request OWNER in BUFFER."
  (if (not (qq-completion--poke-request-current-p buffer owner))
      (qq-completion--clear-poke-owner buffer owner)
    (with-current-buffer buffer
      (setf (plist-get owner :completed-p) t
            (plist-get owner :token) nil)
      (if (null members)
          (progn
            (setq qq-completion--poke-request nil)
            (message "qq: no matching group member"))
        (setf (plist-get owner :timer)
              (run-at-time
               0 nil #'qq-completion--present-group-poke-targets
               buffer owner (copy-tree members)))))))

(defun qq-completion--group-poke-search-failed
    (buffer owner _response reason)
  "Finish failed group poke request OWNER in BUFFER with REASON."
  (if (not (qq-completion--poke-request-current-p buffer owner))
      (qq-completion--clear-poke-owner buffer owner)
    (with-current-buffer buffer
      (setf (plist-get owner :completed-p) t
            (plist-get owner :token) nil)
      (setq qq-completion--poke-request nil)
      (message "qq: failed to search group members: %s" reason))))

(defun qq-completion--read-group-poke-target
    (buffer session-key session callback initial-user-id)
  "Start a strict native group-member search and continue through CALLBACK."
  (let* ((group-id (alist-get 'target-id session))
         (query
          (string-trim
           (read-string "Search group member: " initial-user-id
                        'qq-completion--poke-search-history))))
    (unless (qq-api-group-id-p group-id)
      (user-error "qq: group poke requires a canonical group id"))
    (when (string-empty-p query)
      (user-error "qq: group poke requires a non-empty member search"))
    (let* ((owner
            (list :session-key session-key
                  :group-id group-id
                  :callback callback
                  :token nil
                  :timer nil
                  :completed-p nil))
           token)
      (setq qq-completion--poke-request owner)
      (condition-case error-data
          (setq token
                (qq-api-search-group-members
                 group-id query
                 (lambda (members)
                   (qq-completion--group-poke-search-succeeded
                    buffer owner members))
                 (lambda (response reason)
                   (qq-completion--group-poke-search-failed
                    buffer owner response reason))
                 200))
        (error
         (when (eq qq-completion--poke-request owner)
           (setq qq-completion--poke-request nil))
         (signal (car error-data) (cdr error-data))))
      ;; Test doubles and local transports may complete synchronously.
      (when (and (qq-completion--poke-request-current-p buffer owner)
                 (not (plist-get owner :completed-p)))
        (setf (plist-get owner :token) token))
      owner)))

(defun qq-completion-read-poke-target
    (session-key callback &optional initial-user-id)
  "Choose a strict poke target for SESSION-KEY, then call CALLBACK.

Private chats synchronously choose between the canonical peer and self.
Group chats first read a non-empty native member-search query, then schedule a
rich require-match picker after `qq-api-search-group-members' succeeds.
INITIAL-USER-ID, when non-nil, must be an original decimal string and seeds the
private selection or native group search.  No typed text is ever accepted as a
target without a matching canonical candidate."
  (unless (functionp callback)
    (error "qq: poke target callback must be a function"))
  (when (and initial-user-id
             (not (qq-completion--poke-user-id-p initial-user-id)))
    (user-error "qq: initial poke target must be an original QQ string"))
  (let* ((buffer (current-buffer))
         (session (qq-state-session session-key))
         (type (and session (alist-get 'type session))))
    (unless (and session
                 (derived-mode-p 'qq-chat-mode)
                 (equal qq-chat--session-key session-key))
      (user-error "qq: poke target reader requires the current chat session"))
    (qq-completion--cancel-poke-request)
    (pcase type
      ('private
       (qq-completion--read-private-poke-target
        buffer session-key session callback initial-user-id))
      ('group
       (qq-completion--read-group-poke-target
        buffer session-key session callback initial-user-id))
      (_
       (user-error "qq: poke target selection is unsupported for %s sessions"
                   type)))))

(defun qq-completion--base-face-candidates ()
  "Return shared completion candidates for QQ base faces."
  (mapcar
   (lambda (label)
     (let ((id (qq-media-face-id-from-completion label)))
       (appkit-chat-completion-candidate-create
        :label label
        :value `(:kind base-face
                 :segment ((type . "face") (data . ((id . ,id)))))
        :search-terms (list id (or (qq-media-face-name id) ""))
        :prefix (lambda (_candidate)
                  (qq-media--face-completion-prefix id)))))
   (qq-media-face-completion-candidates)))

(defconst qq-completion--custom-face-query-prefixes
  '("fav" "favorite" "sticker" "收藏" "表情包")
  "Explicit `/PREFIX' values that open visual favorite-face completion.")

(defun qq-completion--custom-face-token ()
  "Return the current explicit `/fav' favorite-face token, or nil."
  (when-let* ((token (qq-completion--face-token))
              (query (downcase (plist-get token :query)))
              ((seq-some (lambda (prefix)
                           (string-prefix-p prefix query))
                         qq-completion--custom-face-query-prefixes)))
    token))

(defun qq-completion--candidate-matches-token-query-p (candidate token)
  "Return non-nil when CANDIDATE can satisfy TOKEN's current query.

The match deliberately includes protocol candidate search terms, so numeric QQ
face ids and normalized Unicode names remain actionable while arbitrary paths,
URLs, colon prose, and emoticons do not capture RET."
  (let* ((query (downcase (or (plist-get token :query) "")))
         (terms (appkit-chat-completion-candidate-search-terms candidate))
         (values
          (cons (appkit-chat-completion-candidate-label candidate)
                (cond
                 ((stringp terms) (list terms))
                 ((listp terms) terms)
                 (t nil)))))
    (seq-some
     (lambda (value)
       (and (stringp value)
            (string-match-p (regexp-quote query) (downcase value))))
     values)))

(defun qq-completion-token-at-point ()
  "Return the unresolved QQ composer token at point, or nil.

The result includes a `:kind' of `member', `favorite-face', `face', or
`unicode-emoji'.  Submit dispatchers use syntax ownership rather than frontend
popup state, so a completion with no preselected candidate cannot fall through
and send literal token text accidentally."
  (let (token kind)
    (cond
     ((setq token (qq-completion--member-token))
      (setq kind 'member))
     ((setq token (qq-completion--custom-face-token))
      (setq kind 'favorite-face))
     ((and (setq token (qq-completion--face-token))
           (seq-some
            (lambda (candidate)
              (qq-completion--candidate-matches-token-query-p candidate token))
            (qq-completion--base-face-candidates)))
      (setq kind 'face))
     ((and (setq token (qq-completion--unicode-emoji-token))
           (seq-some
            (lambda (candidate)
              (qq-completion--candidate-matches-token-query-p candidate token))
            (appkit-chat-emoji-candidates)))
      (setq kind 'unicode-emoji)))
    (when (and token kind)
      (plist-put token :kind kind))))

(defun qq-completion--custom-face-request-current-p (buffer owner)
  "Return non-nil when BUFFER still owns favorite-face request OWNER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq qq-completion--custom-face-pending owner)
              (derived-mode-p 'qq-chat-mode)
              (equal qq-chat--session-key (plist-get owner :session-key))))))

(defun qq-completion--custom-face-request-owner-p (buffer owner)
  "Return non-nil when OWNER still owns BUFFER's favorite-face request."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (eq qq-completion--custom-face-pending owner))))

(defun qq-completion--clear-custom-face-owner (buffer owner)
  "Forget OWNER in BUFFER without disturbing a newer favorite-face request."
  (when (qq-completion--custom-face-request-owner-p buffer owner)
    (with-current-buffer buffer
      (setq qq-completion--custom-face-pending nil))
    t))

(defun qq-completion--maybe-reopen-custom-face-completion
    (buffer owner)
  "Reopen `/fav' completion in BUFFER for request OWNER."
  (let ((current-p
         (qq-completion--custom-face-request-current-p buffer owner))
        (query (plist-get owner :query)))
    (when (qq-completion--clear-custom-face-owner buffer owner)
      (when (and current-p
                 (get-buffer-window buffer t))
        (with-selected-window (get-buffer-window buffer t)
          (when-let* ((token (qq-completion--custom-face-token)))
            (when (equal query (plist-get token :query))
              (completion-at-point))))))))

(defun qq-completion--request-custom-faces (query &optional reopen)
  "Load favorite faces for QUERY and optionally REOPEN completion."
  (if qq-completion--custom-face-pending
      (progn
        (setf (plist-get qq-completion--custom-face-pending :query) query)
        (when reopen
          (setf (plist-get qq-completion--custom-face-pending :reopen) t)))
    (let* ((buffer (current-buffer))
           (owner (list :session-key qq-chat--session-key
                        :query query
                        :reopen reopen)))
      (setq qq-completion--custom-face-pending owner)
      (qq-media-ensure-custom-faces
       (lambda (_faces)
         (cond
          ((not (qq-completion--custom-face-request-owner-p buffer owner)))
          ((not (qq-completion--custom-face-request-current-p buffer owner))
           (qq-completion--clear-custom-face-owner buffer owner))
          ((plist-get owner :reopen)
           (run-at-time
            0 nil #'qq-completion--maybe-reopen-custom-face-completion
            buffer owner))
          (t
           (qq-completion--clear-custom-face-owner buffer owner))))
       (lambda (_response reason)
         (let ((current-p
                (qq-completion--custom-face-request-current-p buffer owner)))
           (when (qq-completion--clear-custom-face-owner buffer owner)
             (when current-p
               (message "qq: failed to load favorite faces: %s"
                        reason)))))))))

(defun qq-completion-face-capf ()
  "CAPF for `/名称' base faces and explicit `/fav' favorite faces."
  (when-let* ((token (qq-completion--face-token)))
    (let* ((custom-p (qq-completion--custom-face-token))
           (query (plist-get token :query))
           (candidates
            (if custom-p
                (when (qq-media-custom-faces-loaded-p)
                  (qq-completion--custom-face-candidates
                   (qq-media-custom-faces)))
              (qq-completion--base-face-candidates))))
      (when (and custom-p (not (qq-media-custom-faces-loaded-p)))
        (qq-completion--request-custom-faces query))
      (when candidates
        (appkit-chat-completion-capf
         (plist-get token :start)
         (plist-get token :end)
         candidates
         :insert-function #'qq-completion--insert-protocol-candidate
         :sync-function #'qq-chat--sync-draft-from-buffer)))))

(defun qq-completion-unicode-emoji-capf ()
  "CAPF for shared standard Unicode `:emoji_name:' completion."
  (appkit-chat-emoji-capf
   :sync-function #'qq-chat--sync-draft-from-buffer))

(defun qq-completion--custom-face-candidates (faces)
  "Return shared completion candidates for favorite FACES."
  (cl-loop for face in (seq-filter #'qq-media-custom-face-sendable-p faces)
           for index from 0
           collect
           (let ((current (copy-tree face)))
             (appkit-chat-completion-candidate-create
              :label (qq-media-custom-face-label current index)
              :value `(:kind custom-face :face ,current)
              :search-terms
              (let* ((desc (alist-get 'desc current))
                     (desc (and (stringp desc) (string-trim desc))))
                (delq nil
                      (append
                       qq-completion--custom-face-query-prefixes
                       (and (not (string-empty-p (or desc "")))
                            (mapcar (lambda (prefix)
                                      (concat prefix desc))
                                    qq-completion--custom-face-query-prefixes))
                       (list desc
                             (alist-get 'md5 current)
                             (and (alist-get 'emo_id current)
                                  (format "%s"
                                          (alist-get 'emo_id current)))))))
              :prefix (lambda (_candidate)
                        (qq-media--custom-face-completion-prefix current))))))

(defun qq-completion-read-base-face-id (&optional prompt)
  "Read a QQ base face and return its string id."
  (let* ((candidate
          (appkit-chat-completion-read
           (or prompt "QQ face: ")
           (qq-completion--base-face-candidates)
           :history 'qq-completion--face-history))
         (value (appkit-chat-completion-candidate-value candidate))
         (segment (plist-get value :segment)))
    (alist-get 'id (alist-get 'data segment))))

(defun qq-completion-read-custom-face (faces)
  "Read and return one favorite face from FACES."
  (let* ((candidate
          (appkit-chat-completion-read
           "Favorite face: "
           (qq-completion--custom-face-candidates faces)
           :history 'qq-completion--custom-face-history))
         (value (appkit-chat-completion-candidate-value candidate)))
    (plist-get value :face)))

(defun qq-completion--insert-protocol-candidate (candidate)
  "Insert protocol segment carried by appkit CANDIDATE."
  (let* ((value (appkit-chat-completion-candidate-value candidate))
         (kind (plist-get value :kind))
         (face (and (eq kind 'custom-face) (plist-get value :face)))
         (segment (if face
                      (qq-media-custom-face-to-segment face)
                    (plist-get value :segment)))
         (visible-label
          (when face (qq-media-custom-face-display-string face))))
    (unless segment
      (error "qq: completion candidate has no protocol segment"))
    (qq-chat--insert-input-segment-object segment visible-label)))

(defun qq-completion-complete ()
  "Complete the current QQ composer token.

Cold native member searches are started asynchronously and reopen completion
only while buffer, session, and token still match."
  (interactive)
  (when (appkit-chatbuf-point-in-input-p)
    (cond
     ((and (qq-completion--custom-face-token)
           (not (qq-media-custom-faces-loaded-p)))
      (qq-completion--request-custom-faces
       (plist-get (qq-completion--custom-face-token) :query) t)
      (message "qq: loading favorite faces…")
      t)
     ((and (qq-completion--custom-face-token)
           (qq-media-custom-faces-loaded-p)
           (null (qq-media-custom-faces)))
      (message "qq: favorite faces are empty")
      t)
     ((if-let* ((token (qq-completion--member-token))
                (query (plist-get token :query))
                ((eq (qq-completion--cached-members query)
                     qq-completion--cache-miss))
                ((null (qq-completion--member-fallback query))))
          (progn
            (qq-completion--request-members query t)
            (message "qq: loading matching group members…")
            t)
        (appkit-chat-completion-complete))))))

(defun qq-completion-preload-members ()
  "Warm the broad native member cache for the current group chat."
  (when (and (qq-completion--group-id)
             (eq (qq-completion--cached-members "")
                 qq-completion--cache-miss))
    (qq-completion--request-members "")))

(defun qq-completion-setup ()
  "Initialize shared QQ composer completion in the current chat buffer."
  (qq-completion--cancel-poke-request)
  (setq-local qq-completion--member-cache (make-hash-table :test #'equal))
  (setq-local qq-completion--member-pending (make-hash-table :test #'equal))
  (setq-local qq-completion--custom-face-pending nil)
  (setq-local qq-completion--poke-request nil)
  (add-hook 'kill-buffer-hook #'qq-completion--cancel-poke-request nil t)
  (add-hook 'change-major-mode-hook #'qq-completion--cancel-poke-request nil t)
  (appkit-chat-completion-setup
   :capf-functions '(qq-completion-member-capf
                     qq-completion-face-capf
                     qq-completion-unicode-emoji-capf)
   :append t))

(provide 'qq-completion)

;;; qq-completion.el ends here
