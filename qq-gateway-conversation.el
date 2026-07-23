;;; qq-gateway-conversation.el --- Native recent-conversation adapter -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Closed, account-addressed adapter for `conversation.list_recent'.  This
;; module deliberately stops at a validated domain page: request ownership,
;; timeline normalization, and selected-account state projection belong to the
;; native product layer.

;;; Code:

(require 'cl-lib)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-gateway-message)
(require 'qq-gateway-rpc)
(require 'qq-gateway-wire)

(defconst qq-gateway-conversation-max-recent-limit 500
  "Largest page accepted by `conversation.list_recent'.")

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
             '(pinned))
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
        ,@(when pinned-entry `((pinned . ,(cdr pinned-entry))))))))

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

(cl-defun qq-gateway-conversation-list-recent
    (account-id &key callback errback limit)
  "Request ACCOUNT-ID's validated recent-conversation page.

CALLBACK receives a closed domain page but this adapter performs no request
ownership or `qq-state' projection.  ERRBACK receives the standard Gateway
body and reason.  LIMIT defaults to `qq-recent-contact-count' and must be
between 1 and 500.  Return the opaque token from the typed RPC boundary."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Recent conversations require an account slot"))
  (setq limit (qq-gateway-conversation--normalize-limit limit))
  (let* ((requested-account-id (copy-sequence account-id))
         (params `((account_id . ,requested-account-id) (limit . ,limit))))
    (qq-gateway-rpc-call
     "conversation.list_recent" params
     :decoder
     (lambda (raw-page)
       (qq-gateway-conversation--validate-page
        raw-page requested-account-id))
     :callback callback
     :errback errback)))

(provide 'qq-gateway-conversation)
;;; qq-gateway-conversation.el ends here
