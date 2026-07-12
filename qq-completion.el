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
(require 'qq-api)
(require 'qq-media)
(require 'qq-state)

(declare-function qq-chat--insert-input-segment-object "qq-chat" (segment))
(declare-function qq-chat--sync-draft-from-buffer "qq-chat" ())

(defvar-local qq-chat--session-key)

(defconst qq-completion--cache-miss (make-symbol "qq-completion-cache-miss"))

(defvar-local qq-completion--member-cache nil
  "Query -> native group member list for the current chat buffer.")

(defvar-local qq-completion--member-pending nil
  "Query -> request metadata for in-flight native member searches.")

(defvar qq-completion--face-history nil
  "History for shared QQ base-face completion readers.")

(defvar qq-completion--custom-face-history nil
  "History for shared QQ favorite-face completion readers.")

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
              (session-key qq-chat--session-key))
          (puthash query (list :reopen reopen) qq-completion--member-pending)
          (qq-api-search-group-members
           group-id query
           (lambda (members)
             (when (qq-completion--request-current-p
                    buffer session-key group-id)
               (with-current-buffer buffer
                 (let ((request (gethash query qq-completion--member-pending)))
                   (remhash query qq-completion--member-pending)
                   (puthash query members qq-completion--member-cache)
                   (when (plist-get request :reopen)
                     (run-at-time
                      0 nil #'qq-completion--maybe-reopen-member-completion
                      buffer session-key group-id query))))))
           (lambda (_response reason)
             (when (qq-completion--request-current-p
                    buffer session-key group-id)
               (with-current-buffer buffer
                 (remhash query qq-completion--member-pending)
                 (message "qq: failed to search group members: %s" reason))))
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
         :sync-function #'qq-chat--sync-draft-from-buffer)))))

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

(defun qq-completion-face-capf ()
  "CAPF for `/名称' QQ base faces."
  (when-let* ((token (appkit-chat-completion-token-bounds ?/)))
    (appkit-chat-completion-capf
     (plist-get token :start)
     (plist-get token :end)
     (qq-completion--base-face-candidates)
     :insert-function #'qq-completion--insert-protocol-candidate
     :sync-function #'qq-chat--sync-draft-from-buffer)))

(defun qq-completion--custom-face-candidates (faces)
  "Return shared completion candidates for favorite FACES."
  (cl-loop for face in faces
           for index from 0
           collect
           (let ((current (copy-tree face)))
             (appkit-chat-completion-candidate-create
              :label (qq-media-custom-face-label current index)
              :value `(:kind custom-face :face ,current
                       :segment ,(qq-media-custom-face-to-segment current))
              :search-terms
              (delq nil (list (alist-get 'desc current)
                              (alist-get 'md5 current)
                              (and (alist-get 'emo_id current)
                                   (format "%s" (alist-get 'emo_id current)))))
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
         (segment (plist-get value :segment)))
    (unless segment
      (error "qq: completion candidate has no protocol segment"))
    (qq-chat--insert-input-segment-object segment)))

(defun qq-completion-complete ()
  "Complete the current QQ composer token.

Cold native member searches are started asynchronously and reopen completion
only while buffer, session, and token still match."
  (interactive)
  (when (appkit-chatbuf-point-in-input-p)
    (if-let* ((token (qq-completion--member-token))
              (query (plist-get token :query))
              ((eq (qq-completion--cached-members query)
                   qq-completion--cache-miss))
              ((null (qq-completion--member-fallback query))))
        (progn
          (qq-completion--request-members query t)
          (message "qq: loading matching group members…")
          t)
      (appkit-chat-completion-complete))))

(defun qq-completion-preload-members ()
  "Warm the broad native member cache for the current group chat."
  (when (and (qq-completion--group-id)
             (eq (qq-completion--cached-members "")
                 qq-completion--cache-miss))
    (qq-completion--request-members "")))

(defun qq-completion-setup ()
  "Initialize shared QQ composer completion in the current chat buffer."
  (setq-local qq-completion--member-cache (make-hash-table :test #'equal))
  (setq-local qq-completion--member-pending (make-hash-table :test #'equal))
  (appkit-chat-completion-setup
   :capf-functions '(qq-completion-member-capf qq-completion-face-capf)
   :append t))

(provide 'qq-completion)

;;; qq-completion.el ends here
