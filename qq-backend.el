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

(defun qq-backend--message-at-sequence (session-key sequence)
  "Return SESSION-KEY message carrying exact SEQUENCE, or nil."
  (seq-find
   (lambda (message)
     (equal (alist-get 'message-seq message) sequence))
   (qq-state-session-messages session-key)))

(defun qq-backend-history-frontier (session-key)
  "Return the selected backend's exact known history frontier.

For the native Gateway the result is a plist containing `:sequence' and,
when known, `:message-id'.  Group `latest_sequence' is authoritative and is
advanced by a newer live event.  Private history deliberately exposes only a
live sequence observed by this Gateway projection because neither Lagrange nor
the QQ C2C history method provides a latest cursor.  `:empty-p' or
`:unavailable-reason' explains a result without a sequence."
  (pcase (qq-backend--validate qq-backend)
    ('onebot nil)
    ('gateway
     (let* ((identity (qq-state-session-key-identity session-key))
            (kind (alist-get 'type identity))
            (target-id (alist-get 'target-id identity))
            (live (qq-gateway-message-live-frontier session-key))
            (live-sequence (alist-get 'sequence live)))
       (unless (memq kind '(private group))
         (user-error "qq: Native Gateway history supports private and group chats"))
       (pcase kind
         ('private
          (if live-sequence
              (list :backend 'gateway
                    :sequence live-sequence
                    :message-id (alist-get 'message_id live)
                    :source 'live-event)
            (list :backend 'gateway
                  :unavailable-reason 'private-latest-sequence)))
         ('group
          (let* ((group (qq-state-group target-id))
                 (directory-sequence (alist-get 'latest_sequence group))
                 sequence source)
            (when (and directory-sequence
                       (not (qq-gateway--canonical-decimal-p
                             directory-sequence t)))
              (error "qq: Gateway group latest_sequence is not exact"))
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
                         (qq-backend--message-at-sequence
                          session-key sequence))))
                (list :backend 'gateway
                      :sequence sequence
                      :message-id (or (alist-get 'message_id message)
                                      (alist-get 'server-id message))
                      :source source
                      :authoritative-p t)))
             (group
              (list :backend 'gateway :empty-p t
                    :source 'group-directory :authoritative-p t))
             (t
              (list :backend 'gateway
                    :unavailable-reason 'group-directory))))))))))

(defun qq-backend-history-range-before (start-sequence count)
  "Return native history range before exact START-SEQUENCE, or nil at zero."
  (qq-gateway-message--validate-sequence
   start-sequence "Current history start sequence")
  (qq-gateway-message--validate-history-count count)
  (unless (equal start-sequence "0")
    (qq-gateway-message-history-range-ending-at
     (qq-gateway-message--decimal-subtract-small start-sequence 1)
     count)))

(defun qq-backend-history-range-after
    (end-sequence count &optional maximum-sequence)
  "Return native history range after END-SEQUENCE, optionally capped at MAXIMUM.

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

(defun qq-backend--gateway-history-meta (meta &rest properties)
  "Return Gateway history META prefixed with backend PROPERTIES."
  (append (list :backend 'gateway) properties (copy-sequence meta)))

(defun qq-backend-fetch-history-range
    (session-key start-sequence end-sequence callback &optional errback properties)
  "Fetch one native Gateway history range for SESSION-KEY.

START-SEQUENCE and END-SEQUENCE are inclusive exact strings.  CALLBACK receives
merge metadata prefixed by optional plist PROPERTIES.  The returned request is
tagged for cancellation across later backend changes."
  (unless (eq (qq-backend--validate qq-backend) 'gateway)
    (user-error "qq: Explicit sequence ranges belong to the native Gateway"))
  (qq-backend--wrap-request
   'gateway
   (qq-gateway-message-get-history
    session-key start-sequence end-sequence
    (lambda (meta)
      (qq-gateway--invoke
       callback
       (apply #'qq-backend--gateway-history-meta meta properties)))
    (or errback #'qq-backend--default-gateway-error))))

(defun qq-backend-fetch-latest-history
    (session-key callback &optional errback count)
  "Fetch the selected backend's latest known history for SESSION-KEY.

Native group history uses the directory's exact latest sequence.  Native
private history uses only a live observed sequence; when none exists CALLBACK
receives metadata with `:history-frontier-unavailable' instead of a guessed
request."
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-backend--wrap-request
      'onebot
      (qq-api-fetch-older-history
       session-key nil callback errback count)))
    ('gateway
     (let* ((frontier (qq-backend-history-frontier session-key))
            (sequence (plist-get frontier :sequence)))
       (cond
        (sequence
         (pcase-let ((`(,start-sequence . ,end-sequence)
                      (qq-gateway-message-history-range-ending-at
                       sequence
                       (min 100 (max 1 (or count qq-history-fetch-count))))))
           (qq-backend-fetch-history-range
            session-key start-sequence end-sequence callback errback
            (list :history-at-latest-p t :history-frontier frontier))))
        ((plist-get frontier :empty-p)
         (qq-gateway--invoke
          callback
          (qq-backend--gateway-history-meta
           (list :session-key session-key
                 :message-count 0 :added-count 0 :batch-message-ids nil)
           :history-at-latest-p t :history-at-oldest-p t
           :history-frontier frontier))
         nil)
        (t
         (qq-gateway--invoke
          callback
          (qq-backend--gateway-history-meta
           (list :session-key session-key
                 :message-count 0 :added-count 0 :batch-message-ids nil)
           :history-frontier-unavailable
           (plist-get frontier :unavailable-reason)
           :history-frontier frontier))
         nil))))))

(defun qq-backend-fetch-history-around
    (session-key message-id callback &optional errback count)
  "Fetch selected-backend history around exact MESSAGE-ID in SESSION-KEY."
  (pcase (qq-backend--validate qq-backend)
    ('onebot
     (qq-backend--wrap-request
      'onebot
      (qq-api-fetch-history-around
       session-key message-id callback errback count)))
    ('gateway
     (let* ((message
             (seq-find
              (lambda (candidate)
                (equal (alist-get 'server-id candidate) message-id))
              (qq-state-session-messages session-key)))
            (sequence (alist-get 'message-seq message)))
       (if (not sequence)
           (qq-gateway--client-error
            (or errback #'qq-backend--default-gateway-error)
            "history_sequence_unavailable"
            "Native Gateway can seek only a cached message carrying sequence metadata")
         (pcase-let ((`(,start-sequence . ,end-sequence)
                      (qq-gateway-message-history-range-around
                       sequence
                       (min 100 (max 1 (or count qq-history-fetch-count))))))
           (qq-backend-fetch-history-range
            session-key start-sequence end-sequence callback errback
            (list :history-target-message-id message-id))))))))

(defun qq-backend-history-exhausted-error-p (response reason)
  "Return non-nil when selected backend RESPONSE and REASON mean history EOF."
  (and (eq (qq-backend--validate qq-backend) 'onebot)
       (qq-api--history-exhausted-error-p response reason)))

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
