;;; qq-gateway-conversation.el --- Native recent-conversation adapter -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Closed, account-slot-owned adapter for `conversation.list_recent'.  This
;; first-stage module deliberately stops at a validated domain page: timeline
;; normalization and selected-account state projection belong to the next
;; layer. Exact Native Session capabilities remain entirely inside the Gateway;
;; this wire boundary is owned by the stable account slot and Gateway instance.

;;; Code:

(require 'cl-lib)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-gateway-message)
(require 'qq-gateway-rpc)
(require 'qq-gateway-transport)
(require 'qq-gateway-wire)

(defconst qq-gateway-conversation-max-recent-limit 500
  "Largest page accepted by `conversation.list_recent'.")

(cl-defstruct (qq-gateway-conversation--request
               (:constructor qq-gateway-conversation--request-create))
  "Local ownership of one recent-conversation request."
  account-id
  gateway-instance-id
  transport-token
  errback)

(defvar qq-gateway-conversation--active-request nil
  "Newest locally owned recent-conversation request, or nil.")

(defun qq-gateway-conversation--json-boolean-p (value)
  "Return non-nil when VALUE is one exact decoded JSON boolean."
  (memq value '(t :false)))

(defun qq-gateway-conversation--validate-identity (identity context)
  "Validate recent conversation IDENTITY in CONTEXT and return a domain copy."
  (unless (qq-gateway-wire-object-p identity)
    (error "qq: Gateway %s conversation identity must be an object" context))
  (pcase (alist-get 'kind identity)
    ("private"
     (unless (qq-gateway-wire-closed-object-p
              identity '(kind) '(peer_uin peer_uid))
       (error "qq: Gateway %s private identity has invalid fields" context))
     (let ((uin-entry (assq 'peer_uin identity))
           (uid-entry (assq 'peer_uid identity)))
       (unless (or uin-entry uid-entry)
         (error "qq: Gateway %s private identity has no peer" context))
       (when (and uin-entry
                  (not (qq-gateway--canonical-decimal-p (cdr uin-entry))))
         (error "qq: Gateway %s private peer_uin is malformed" context))
       (when (and uid-entry
                  (not (qq-gateway--non-empty-string-p (cdr uid-entry))))
         (error "qq: Gateway %s private peer_uid is malformed" context))))
    ("group"
     (unless (and (qq-gateway-wire-exact-object-keys-p
                   identity '(kind group_uin))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'group_uin identity)))
       (error "qq: Gateway %s group identity is malformed" context)))
    ("temporary"
     (unless (qq-gateway-wire-closed-object-p
              identity '(kind)
              '(peer_uin peer_uid from_tiny_id to_tiny_id))
       (error "qq: Gateway %s temporary identity has invalid fields" context))
     (let ((entries
            (delq nil
                  (mapcar (lambda (key) (assq key identity))
                          '(peer_uin peer_uid from_tiny_id to_tiny_id)))))
       (unless entries
         (error "qq: Gateway %s temporary identity has no route" context))
       (dolist (key '(peer_uin from_tiny_id to_tiny_id))
         (when-let* ((entry (assq key identity)))
           (unless (qq-gateway--canonical-decimal-p (cdr entry))
             (error "qq: Gateway %s temporary %s is malformed"
                    context key))))
       (when-let* ((entry (assq 'peer_uid identity)))
         (unless (qq-gateway--non-empty-string-p (cdr entry))
           (error "qq: Gateway %s temporary peer_uid is malformed" context)))))
    (_ (error "qq: Gateway %s conversation identity has unknown kind" context)))
  (qq-gateway-wire-domain-copy identity))

(defun qq-gateway-conversation--validate-message-snapshot (message)
  "Validate one recent MESSAGE through the canonical message codec adapter.

TODO: move the closed snapshot codec out of the projection-heavy message
module.  Until then, keep this explicit adapter so the recent protocol never
copies or weakens that schema."
  (qq-gateway-message--validate-message message))

(defun qq-gateway-conversation--validate-identity-message
    (identity message context)
  "Reject a recent IDENTITY that contradicts its latest MESSAGE in CONTEXT."
  (let ((identity-kind (alist-get 'kind identity))
        (conversation (alist-get 'conversation message)))
    (pcase identity-kind
      ("private"
       (unless (equal (alist-get 'kind conversation) "private")
         (error "qq: Gateway %s private identity contradicts latest message"
                context)))
      ("group"
       (unless (and (equal (alist-get 'kind conversation) "group")
                    (equal (alist-get 'group_uin identity)
                           (alist-get 'group_uin conversation)))
         (error "qq: Gateway %s group identity contradicts latest message"
                context)))
      ("temporary"
       (unless (equal (alist-get 'kind conversation) "temp")
         (error "qq: Gateway %s temporary identity contradicts latest message"
                context))
       ;; The projection may retain route facts learned from an older message,
       ;; so an absent latest-message field is not a contradiction.  Two
       ;; present values, however, must denote the same exact route.
       (dolist (key '(from_tiny_id to_tiny_id))
         (when (and (assq key identity) (assq key conversation)
                    (not (equal (alist-get key identity)
                                (alist-get key conversation))))
           (error "qq: Gateway %s temporary route contradicts latest message"
                  context))))))
  message)

(defun qq-gateway-conversation--validate-read-cursor (cursor context)
  "Validate recent read CURSOR in CONTEXT."
  (unless (qq-gateway-wire-closed-object-p
           cursor
           '(read_through_message_id read_through_sequence)
           '(server_read_sequence))
    (error "qq: Gateway %s read cursor has invalid fields" context))
  (let ((message-id (alist-get 'read_through_message_id cursor))
        (sequence (alist-get 'read_through_sequence cursor))
        (server-entry (assq 'server_read_sequence cursor)))
    (unless (qq-gateway--canonical-decimal-p message-id)
      (error "qq: Gateway %s read cursor message_id is malformed" context))
    (unless (qq-gateway--canonical-decimal-p sequence)
      (error "qq: Gateway %s read cursor sequence is malformed" context))
    (when server-entry
      (let ((server-sequence (cdr server-entry)))
        (unless (and (qq-gateway--canonical-decimal-p server-sequence)
                     (not (qq-gateway--decimal-less-p
                           server-sequence sequence)))
          (error "qq: Gateway %s server read cursor regresses" context)))))
  (qq-gateway-wire-domain-copy cursor))

(defun qq-gateway-conversation--identity-key (identity message)
  "Return a closed duplicate-detection key for IDENTITY and latest MESSAGE."
  (pcase (alist-get 'kind identity)
    ("private"
     (list "private" (alist-get 'peer_uid identity)
           (alist-get 'peer_uin identity)))
    ("group" (list "group" (alist-get 'group_uin identity)))
    ("temporary"
     (list "temporary"
           (alist-get 'peer_uid identity)
           (alist-get 'peer_uin identity)
           (alist-get 'from_tiny_id identity)
           (alist-get 'to_tiny_id identity)
           (alist-get 'sub_type message)))))

(defun qq-gateway-conversation--validate-row
    (row index seen-identities seen-revisions)
  "Validate recent ROW at INDEX.

SEEN-IDENTITIES and SEEN-REVISIONS reject duplicate closed projections."
  (let ((context (format "recent conversations[%d]" index)))
    (unless (qq-gateway-wire-closed-object-p
             row
             '(conversation activity_revision latest_message
               latest_message_recalled)
             '(pinned read_cursor))
      (error "qq: Gateway %s has invalid fields" context))
    (let* ((identity
           (qq-gateway-conversation--validate-identity
             (alist-get 'conversation row) context))
           (revision (alist-get 'activity_revision row))
           (message
            (qq-gateway-conversation--validate-message-snapshot
             (alist-get 'latest_message row)))
           (recalled (alist-get 'latest_message_recalled row))
           (pinned-entry (assq 'pinned row))
           (read-entry (assq 'read_cursor row))
           (read-cursor
            (and read-entry
                 (qq-gateway-conversation--validate-read-cursor
                  (cdr read-entry) context)))
           (identity-key
            (qq-gateway-conversation--identity-key identity message)))
      (unless (qq-gateway--canonical-decimal-p revision)
        (error "qq: Gateway %s activity_revision is malformed" context))
      (when (gethash revision seen-revisions)
        (error "qq: Gateway recent page duplicates activity_revision %s"
               revision))
      (puthash revision t seen-revisions)
      (unless (qq-gateway-conversation--json-boolean-p recalled)
        (error "qq: Gateway %s recalled flag must be boolean" context))
      (when (and pinned-entry
                 (not (qq-gateway-conversation--json-boolean-p
                       (cdr pinned-entry))))
        (error "qq: Gateway %s pinned flag must be boolean" context))
      (qq-gateway-conversation--validate-identity-message
       identity message context)
      (when (gethash identity-key seen-identities)
        (error "qq: Gateway recent page duplicates conversation identity"))
      (puthash identity-key t seen-identities)
      `((conversation . ,identity)
        (activity_revision . ,revision)
        (latest_message . ,message)
        (latest_message_recalled . ,recalled)
        ,@(when pinned-entry `((pinned . ,(cdr pinned-entry))))
        ,@(when read-entry `((read_cursor . ,read-cursor)))))))

(defun qq-gateway-conversation--validate-page (page requested-account-id)
  "Validate a closed recent-conversation PAGE for REQUESTED-ACCOUNT-ID."
  (unless (qq-gateway-wire-exact-object-keys-p
           page '(account_id conversations truncated))
    (error "qq: Gateway recent-conversation page has invalid fields"))
  (let ((account-id (alist-get 'account_id page))
        (truncated (alist-get 'truncated page)))
    (unless (and (qq-gateway--non-empty-string-p account-id)
                 (equal account-id requested-account-id))
      (error "qq: Gateway recent-conversation page slot contradicts request"))
    (unless (qq-gateway-conversation--json-boolean-p truncated)
      (error "qq: Gateway recent-conversation truncated flag must be boolean"))
    (let ((seen-identities (make-hash-table :test #'equal))
          (seen-revisions (make-hash-table :test #'equal))
          rows)
      (cl-loop
       for row in (qq-gateway-wire-array
                   (alist-get 'conversations page)
                   "Gateway recent conversations")
       for index from 0
       do (push (qq-gateway-conversation--validate-row
                 row index seen-identities seen-revisions)
                rows))
      `((account_id . ,account-id)
        (conversations . ,(nreverse rows))
        (truncated . ,truncated)))))

(defun qq-gateway-conversation--normalize-limit (limit)
  "Return validated recent-conversation LIMIT."
  (setq limit (or limit qq-recent-contact-count))
  (unless (and (integerp limit)
               (<= 1 limit qq-gateway-conversation-max-recent-limit))
    (user-error "qq: Recent conversation limit must be between 1 and %d"
                qq-gateway-conversation-max-recent-limit))
  limit)

(defun qq-gateway-conversation--request-current-p (request)
  "Return non-nil when REQUEST still owns recent-conversation delivery."
  (eq request qq-gateway-conversation--active-request))

(defun qq-gateway-conversation--request-context-current-p (request)
  "Return non-nil when REQUEST still belongs to the selected account slot."
  (and (equal (qq-gateway-conversation--request-account-id request)
              (qq-gateway-current-account-id))
       (equal (qq-gateway-conversation--request-gateway-instance-id request)
              (qq-gateway-transport-gateway-instance-id))))

(defun qq-gateway-conversation--finish-request (request)
  "Release REQUEST only when it still owns delivery."
  (when (qq-gateway-conversation--request-current-p request)
    (setq qq-gateway-conversation--active-request nil)
    t))

(defun qq-gateway-conversation--claim-request (request)
  "Make REQUEST the single delivery owner and settle its predecessor.

Return non-nil only when REQUEST still owns delivery after a predecessor's
possibly reentrant error callback has run."
  (let ((previous qq-gateway-conversation--active-request))
    ;; Publish the replacement first.  If PREVIOUS's errback starts another
    ;; request reentrantly, the caller can observe that REQUEST lost ownership
    ;; and must not send it afterwards.
    (setq qq-gateway-conversation--active-request request)
    (when previous
      (when-let* ((token
                   (qq-gateway-conversation--request-transport-token previous)))
        (qq-gateway-transport-cancel token))
      (qq-gateway--client-error
       (qq-gateway-conversation--request-errback previous)
       "superseded_request"
       "Gateway recent-conversation request was superseded"))
    (qq-gateway-conversation--request-current-p request)))

(defun qq-gateway-conversation-reset (&rest _ignored)
  "Revoke and cancel local recent-conversation request ownership."
  (when-let* ((request qq-gateway-conversation--active-request))
    (setq qq-gateway-conversation--active-request nil)
    (when-let* ((token
                 (qq-gateway-conversation--request-transport-token request)))
      (qq-gateway-transport-cancel token)))
  nil)

(defun qq-gateway-conversation-cancel (transport-token)
  "Cancel TRANSPORT-TOKEN and revoke matching adapter ownership.

Native product requests must use this function instead of cancelling the raw
transport token directly, so the adapter cannot retain a false active marker."
  (let ((request qq-gateway-conversation--active-request))
    (when (and request
               (equal transport-token
                      (qq-gateway-conversation--request-transport-token request)))
      (setq qq-gateway-conversation--active-request nil))
    (qq-gateway-transport-cancel transport-token)))

(defun qq-gateway-conversation--project-page (request page)
  "Release current REQUEST ownership and return validated PAGE."
  (unless (qq-gateway-conversation--finish-request request)
    (error "qq: Gateway recent-conversation request lost ownership"))
  page)

(defun qq-gateway-conversation--deliver-error (request body reason)
  "Deliver Gateway BODY and REASON only when REQUEST is still current."
  (when (qq-gateway-conversation--request-current-p request)
    (qq-gateway-conversation--finish-request request)
    (if (qq-gateway-conversation--request-context-current-p request)
        (qq-gateway--invoke
         (qq-gateway-conversation--request-errback request) body reason)
      (qq-gateway--client-error
       (qq-gateway-conversation--request-errback request)
       "superseded_request"
       "Gateway recent-conversation request changed account slot"))))

(defun qq-gateway-conversation-list-recent
    (&optional callback errback limit)
  "Request the selected account slot's validated recent-conversation page.

CALLBACK receives a closed domain page but this adapter performs no `qq-state'
projection.  ERRBACK receives the standard Gateway body and reason.  LIMIT
defaults to `qq-recent-contact-count' and must be between 1 and 500.

Only one request owns delivery.  A newer call cancels and settles the older
one.  Selection or Gateway-instance changes reject a late response; restarting
the same selected account slot does not, because this operation is slot-owned."
  (setq limit (qq-gateway-conversation--normalize-limit limit))
  (let ((account-id (or (qq-gateway-current-account-id)
                        (user-error "qq: Select a QQ account first")))
        (instance-id (qq-gateway-transport-gateway-instance-id)))
    (unless (qq-gateway--non-empty-string-p instance-id)
      (user-error "qq: Gateway instance identity is unavailable"))
    (let* ((request
            (qq-gateway-conversation--request-create
             :account-id (copy-sequence account-id)
             :gateway-instance-id (copy-sequence instance-id)
             :errback errback))
           (params `((account_id . ,account-id) (limit . ,limit)))
           transport-token)
      ;; Register before sending: typed-RPC preflight errors may settle the
      ;; request synchronously.
      (when (qq-gateway-conversation--claim-request request)
        (condition-case error-data
            (setq transport-token
                  (qq-gateway-rpc-call
                   "conversation.list_recent" params
                   :current-p
                   (lambda ()
                     (and
                      (qq-gateway-conversation--request-current-p request)
                      (qq-gateway-conversation--request-context-current-p
                       request)))
                   :stale-code "superseded_request"
                   :stale-message
                   "Gateway recent-conversation request changed account slot"
                   :decoder
                   (lambda (raw-page)
                     (qq-gateway-conversation--validate-page
                      raw-page
                      (qq-gateway-conversation--request-account-id request)))
                   :projector
                   (apply-partially
                    #'qq-gateway-conversation--project-page request)
                   :callback callback
                   :errback
                   (apply-partially
                    #'qq-gateway-conversation--deliver-error request)))
          (error
           (qq-gateway-conversation--finish-request request)
           (signal (car error-data) (cdr error-data))))
        (if (qq-gateway-conversation--request-current-p request)
            (setf (qq-gateway-conversation--request-transport-token request)
                  transport-token)
          ;; A synchronous callback may cancel or supersede REQUEST before the
          ;; transport starter returns its token.  Do not orphan that token.
          (when transport-token
            (qq-gateway-transport-cancel transport-token)))
        transport-token))))

(defalias 'qq-gateway-conversation-refresh-recent
  #'qq-gateway-conversation-list-recent)

(add-hook 'qq-gateway-current-account-changed-hook
          #'qq-gateway-conversation-reset)

(provide 'qq-gateway-conversation)
;;; qq-gateway-conversation.el ends here
