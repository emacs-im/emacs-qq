;;; qq-gateway-conversation.el --- Native recent-conversation adapter -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Account-addressed adapter for `conversation.list_recent'.  Rust owns the
;; response schema.  This module checks only relationships needed by the Emacs
;; projection before handing the domain page to the native product layer.

;;; Code:

(require 'cl-lib)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-gateway-message)
(require 'qq-gateway-rpc)

(defconst qq-gateway-conversation-max-recent-limit 500
  "Largest page accepted by `conversation.list_recent'.")

(defun qq-gateway-conversation--assert-identity-message
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
  "Return a projection duplicate key for IDENTITY and latest MESSAGE."
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

(defun qq-gateway-conversation--check-row
    (row index seen-identities)
  "Check projection relationships for recent ROW at INDEX.

SEEN-IDENTITIES rejects two rows that would own the same local session."
  (let ((context (format "recent conversations[%d]" index)))
    (let* ((identity
            (alist-get 'conversation row))
           (message (alist-get 'latest_message row))
           (identity-key
            (qq-gateway-conversation--identity-key identity message)))
      (qq-gateway-conversation--assert-identity-message
       identity message context)
      (when (gethash identity-key seen-identities)
        (error "qq: Gateway recent page duplicates conversation identity"))
      (puthash identity-key t seen-identities)
      row)))

(defun qq-gateway-conversation--check-page (page)
  "Check client projection relationships in recent-conversation PAGE."
  (let ((seen-identities (make-hash-table :test #'equal)))
    (cl-loop
     for row in (alist-get 'conversations page)
     for index from 0
     do (qq-gateway-conversation--check-row row index seen-identities)))
  page)

(defun qq-gateway-conversation--normalize-limit (limit)
  "Return normalized recent-conversation LIMIT."
  (setq limit (or limit qq-recent-contact-count))
  (unless (and (integerp limit)
               (<= 1 limit qq-gateway-conversation-max-recent-limit))
    (user-error "qq: Recent conversation limit must be between 1 and %d"
                qq-gateway-conversation-max-recent-limit))
  limit)

(cl-defun qq-gateway-conversation-list-recent
    (account-id &key callback errback limit)
  "Request ACCOUNT-ID's recent-conversation domain page.

CALLBACK receives a domain page but this adapter performs no request
ownership or `qq-state' projection.  ERRBACK receives the standard Gateway
body and reason.  LIMIT defaults to `qq-recent-contact-count' and must be
between 1 and 500.  Return the opaque token from the typed RPC boundary."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Recent conversations require an account slot"))
  (setq limit (qq-gateway-conversation--normalize-limit limit))
  (let ((params `((account_id . ,account-id) (limit . ,limit))))
    (qq-gateway-rpc-call
     "conversation.list_recent" params
     :projector #'qq-gateway-conversation--check-page
     :callback callback
     :errback errback)))

(provide 'qq-gateway-conversation)
;;; qq-gateway-conversation.el ends here
