;;; qq-backend.el --- Explicit emacs-qq backend dispatch -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Product-facing operations are dispatched through one explicit Elisp
;; backend selection.  OneBot and the native Gateway may both exist as
;; transports, but only the selected backend owns shared `qq-state'.  Gateway
;; disconnect only detaches Emacs; account stop and logout remain explicit
;; lifecycle commands.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-customize)
(require 'qq-api)
(require 'qq-gateway-directory)
(require 'qq-gateway-message)
(require 'qq-gateway-transport)
(require 'qq-state)
(require 'qq-transport)

(cl-defstruct (qq-backend-request
               (:constructor qq-backend-request-create))
  "Opaque cancellable request retaining its originating backend."
  backend
  token)

(defvar qq-backend--gateway-bootstrap-owner nil
  "Gateway owner whose initial directory refresh is running or complete.")

(defvar qq-backend--gateway-bootstrap-pending 0
  "Number of directory parts pending for the current Gateway bootstrap.")

(defun qq-backend--validate (backend)
  "Return BACKEND when it names a supported protocol backend."
  (unless (memq backend '(onebot gateway))
    (user-error "qq: Unknown backend %S" backend))
  backend)

(defun qq-backend-active-p (backend)
  "Return non-nil when BACKEND owns the interactive client projection."
  (eq (qq-backend--validate backend) qq-backend))

(defun qq-backend--wrap-request (backend token)
  "Wrap originating BACKEND and opaque TOKEN for safe cancellation."
  (and token
       (qq-backend-request-create :backend backend :token token)))

(defun qq-backend-cancel-request (request)
  "Cancel local callback ownership for backend REQUEST."
  (when (qq-backend-request-p request)
    (pcase (qq-backend-request-backend request)
      ('onebot (qq-api-cancel-request (qq-backend-request-token request)))
      ('gateway
       (qq-gateway-transport-cancel (qq-backend-request-token request))))))

(defun qq-backend-running-p ()
  "Return non-nil when the selected backend transport is active."
  (pcase (qq-backend--validate qq-backend)
    ('onebot (qq-transport-running-p))
    ('gateway (qq-gateway-transport-running-p))))

(defun qq-backend-ready-p ()
  "Return non-nil when the selected backend accepts business requests."
  (pcase (qq-backend--validate qq-backend)
    ('onebot (and (qq-transport-running-p)
                  (memq (qq-state-connection-status) '(open ready))))
    ('gateway (qq-gateway-transport-ready-p))))

(defun qq-backend-connect ()
  "Connect the selected backend without changing remote account lifecycle."
  (qq-backend-activate)
  (pcase qq-backend
    ('onebot (qq-transport-start))
    ('gateway (qq-gateway-transport-start))))

(defun qq-backend-disconnect ()
  "Disconnect the selected backend's Emacs transport.

For the native Gateway this never stops or logs out a managed QQ account."
  (pcase (qq-backend--validate qq-backend)
    ('onebot (qq-transport-stop))
    ('gateway (qq-gateway-transport-stop))))

(defun qq-backend--default-gateway-error (_body reason)
  "Report a native Gateway failure described by REASON."
  (message "qq: %s" (or reason "native Gateway request failed")))

(defun qq-backend-refresh-friend-categories
    (&optional callback errback refresh)
  "Refresh selected-backend friends and call CALLBACK with categories.

ERRBACK receives the backend response and reason.  REFRESH forces the native
Gateway's generation-local cache; OneBot directory snapshots are always
authoritative refreshes."
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-backend--wrap-request
      'onebot (qq-api-refresh-friend-categories callback errback)))
    ('gateway
     (qq-backend--wrap-request
      'gateway
      (qq-gateway-directory-refresh-friends
       callback (or errback #'qq-backend--default-gateway-error) refresh)))))

(defun qq-backend-refresh-joined-groups
    (&optional callback errback refresh)
  "Refresh selected-backend joined groups and call CALLBACK with them.

ERRBACK receives the backend response and reason.  REFRESH forces the native
Gateway's generation-local cache; OneBot directory snapshots are always
authoritative refreshes."
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-backend--wrap-request
      'onebot (qq-api-refresh-joined-groups callback errback)))
    ('gateway
     (qq-backend--wrap-request
      'gateway
      (qq-gateway-directory-refresh-groups
       callback (or errback #'qq-backend--default-gateway-error) refresh)))))

(defun qq-backend-refresh ()
  "Refresh primary data supported by the selected backend."
  (interactive)
  (pcase (qq-backend--validate qq-backend)
    ('onebot (qq-api-refresh))
    ('gateway
     (list (qq-backend-refresh-friend-categories)
           (qq-backend-refresh-joined-groups)))))

(defun qq-backend--gateway-member-values (member)
  "Return non-empty locally searchable strings from Gateway MEMBER."
  (seq-filter
   (lambda (value) (and (stringp value) (not (string-empty-p value))))
   (mapcar (lambda (key) (alist-get key member))
           '(card nickname remark qid user_id))))

(defun qq-backend--filter-gateway-members (members query limit)
  "Return Gateway MEMBERS matching QUERY, truncated to LIMIT."
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
                (qq-backend--gateway-member-values member)))
             members))))
    (copy-tree (seq-take matches limit))))

(defun qq-backend-search-group-members
    (group-id query callback &optional errback limit)
  "Search selected-backend GROUP-ID members for QUERY.

CALLBACK receives at most LIMIT exact member projections.  Native Gateway
member pages are complete generation-owned snapshots, so repeated queries use
the local page after its first fetch.  ERRBACK receives the backend response
and reason."
  (unless (stringp query)
    (user-error "qq: Group member search query must be a string"))
  (setq limit (or limit 200))
  (unless (and (integerp limit) (<= 1 limit 200))
    (user-error "qq: Group member search limit must be between 1 and 200"))
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-backend--wrap-request
      'onebot
      (qq-api-search-group-members group-id query callback errback limit)))
    ('gateway
     (unless (qq-gateway--canonical-decimal-p group-id)
       (user-error "qq: Group member search requires an exact group UIN"))
     (if-let* ((page (qq-gateway-directory-group-member-page group-id)))
         (progn
           (qq-gateway--invoke
            callback
            (qq-backend--filter-gateway-members
             (alist-get 'members page) query limit))
           nil)
       (qq-backend--wrap-request
        'gateway
        (qq-gateway-directory-list-group-members
         group-id
         (lambda (members)
           (qq-gateway--invoke
            callback
            (qq-backend--filter-gateway-members members query limit)))
         (or errback #'qq-backend--default-gateway-error)))))))

(defun qq-backend--gateway-text (segments)
  "Return text represented by native-sendable SEGMENTS.

The current Gateway method deliberately supports text only.  Unsupported rich
segments fail before optimistic timeline state is inserted."
  (unless (and (proper-list-p segments) segments)
    (user-error "qq: Native Gateway requires at least one text segment"))
  (let (parts)
    (dolist (segment segments)
      (unless (and (qq-gateway--exact-object-keys-p segment '(type data))
                   (equal (alist-get 'type segment) "text")
                   (qq-gateway--exact-object-keys-p
                    (alist-get 'data segment) '(text))
                   (stringp (alist-get 'text (alist-get 'data segment))))
        (user-error
         "qq: Native Gateway currently sends plain text without reply or rich segments"))
      (push (alist-get 'text (alist-get 'data segment)) parts))
    (let ((text (mapconcat #'identity (nreverse parts) "")))
      (when (string-empty-p text)
        (user-error "qq: Native Gateway text message must not be empty"))
      text)))

(defun qq-backend-send-message
    (session-key segments &optional raw-message callback errback)
  "Send SEGMENTS to SESSION-KEY through the selected backend.

RAW-MESSAGE retains the OneBot optimistic rendering override.  The native
Gateway currently accepts text-only segments and promotes its pending row from
the later authoritative self event."
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-api-send-message
      session-key segments raw-message callback errback))
    ('gateway
     (qq-gateway-message-send-text
      session-key (qq-backend--gateway-text segments) callback
      (or errback #'qq-backend--default-gateway-error)))))

(defun qq-backend-recall-message (message &optional callback errback)
  "Recall normalized MESSAGE through the selected backend.

CALLBACK receives the successful backend response; ERRBACK receives the
backend failure response and reason."
  (unless (listp message)
    (user-error "qq: Recall requires a normalized message"))
  (let ((session-key (alist-get 'session-key message))
        (message-id (alist-get 'server-id message)))
    (unless (and session-key (stringp message-id))
      (user-error "qq: Recall requires exact session and message identity"))
    (pcase (qq-backend--validate qq-backend)
      ('onebot
       (let ((reference
              `((message_id . ,message-id)
                (chat . ,(qq-api-chat-locator session-key)))))
         (if (or callback errback)
             (qq-api-delete-message reference callback errback)
           (qq-api-delete-message reference))))
      ('gateway
       (qq-gateway-message-recall
        session-key message callback
        (or errback #'qq-backend--default-gateway-error))))))

(defun qq-backend-supports-p (capability)
  "Return non-nil when selected backend supports product CAPABILITY."
  (pcase (qq-backend--validate qq-backend)
    ('onebot t)
    ('gateway
     (memq capability
           '(contacts group-members send-text recall explicit-history)))))

(defun qq-backend--gateway-bootstrap-complete (owner failed-p)
  "Complete one Gateway bootstrap part for OWNER, recording FAILED-P."
  (when (equal owner qq-backend--gateway-bootstrap-owner)
    (if failed-p
        (setq qq-backend--gateway-bootstrap-owner nil
              qq-backend--gateway-bootstrap-pending 0)
      (setq qq-backend--gateway-bootstrap-pending
            (max 0 (1- qq-backend--gateway-bootstrap-pending))))))

(defun qq-backend--gateway-bootstrap-success (owner _value)
  "Record one successful Gateway directory bootstrap part for OWNER."
  (qq-backend--gateway-bootstrap-complete owner nil))

(defun qq-backend--gateway-bootstrap-failure (owner _body reason)
  "Record failed Gateway bootstrap OWNER and report its REASON."
  (qq-backend--gateway-bootstrap-complete owner t)
  (qq-backend--default-gateway-error nil reason))

(defun qq-backend--maybe-bootstrap-gateway (&rest _arguments)
  "Load missing directory state for the selected online Gateway account."
  (when (and (eq qq-backend 'gateway)
             (qq-gateway-transport-ready-p))
    (let* ((account (qq-gateway-current-account))
           (owner (qq-gateway-current-account-owner))
           (friends-needed (not (qq-state-friend-categories-loaded-p)))
           (groups-needed (not (qq-state-groups-loaded-p)))
           (part-count (+ (if friends-needed 1 0)
                          (if groups-needed 1 0))))
      (when (and owner
                 (equal (alist-get 'phase account) "online")
                 (> part-count 0)
                 (not (equal owner qq-backend--gateway-bootstrap-owner)))
        (setq qq-backend--gateway-bootstrap-owner (copy-tree owner)
              qq-backend--gateway-bootstrap-pending part-count)
        (when friends-needed
          (qq-backend-refresh-friend-categories
           (apply-partially #'qq-backend--gateway-bootstrap-success owner)
           (apply-partially #'qq-backend--gateway-bootstrap-failure owner)))
        (when groups-needed
          (qq-backend-refresh-joined-groups
           (apply-partially #'qq-backend--gateway-bootstrap-success owner)
           (apply-partially #'qq-backend--gateway-bootstrap-failure owner)))))))

(defun qq-backend-activate ()
  "Make the selected backend own shared client projection state."
  (pcase (qq-backend--validate qq-backend)
    ('onebot (qq-gateway-message-deactivate-projection))
    ('gateway
     (qq-gateway-message-activate-projection)
     (qq-backend--maybe-bootstrap-gateway)))
  qq-backend)

(defun qq-backend-reset-session-state ()
  "Revoke backend-private projection and request caches."
  (setq qq-backend--gateway-bootstrap-owner nil
        qq-backend--gateway-bootstrap-pending 0)
  (qq-gateway-directory-reset)
  (qq-gateway-message-revoke-projection))

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-backend--maybe-bootstrap-gateway t)
(add-hook 'qq-gateway-accounts-changed-hook
          #'qq-backend--maybe-bootstrap-gateway t)

(provide 'qq-backend)

;;; qq-backend.el ends here
