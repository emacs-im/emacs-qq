;;; qq-gateway-attachment.el --- Native prepared attachments -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Projection and upload-ahead helpers for Gateway Prepared Attachments.
;; Staged resources remain account-neutral; attachments bind
;; those immutable bytes to one stable account slot, one conversation, and one
;; media use.  Native MsgInfo, upload keys, Highway tickets, and local paths
;; never enter this projection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway-dispatch)
(require 'qq-gateway)
(require 'qq-gateway-rpc)
(require 'qq-gateway-resource)
(require 'qq-gateway-transport)
(require 'qq-gateway-wire)
(require 'qq-state)

(defvar qq-gateway-attachment-changed-hook nil
  "Hook called with REASON and ATTACHMENT-ID after projection changes.")

(defvar qq-gateway-attachment-desync-hook nil
  "Hook called with an unsolicited attachment stream error body.")

(defvar qq-gateway-attachment--attachments (make-hash-table :test #'equal))
(defvar qq-gateway-attachment--order nil)
(defvar qq-gateway-attachment--gateway-instance-id nil)
(defvar qq-gateway-attachment--refresh-owner nil
  "Identity of the newest authoritative attachment registry refresh.")
(defvar qq-gateway-attachment--resync-request-id nil
  "Identity of the in-flight automatic attachment registry resync.")

(cl-defstruct (qq-gateway-attachment-operation
               (:constructor qq-gateway-attachment-operation-create))
  "One client-side stage-and-prepare operation.

The service owns byte copying and QQ upload work.  This object owns only the
request callbacks and temporary resource/attachment identities created on
behalf of one caller."
  active-p
  request-id
  source-resource-id
  resource-id
  attachment-id
  resource-watch
  attachment-watch)

(defun qq-gateway-attachment--id-p (value)
  "Return non-nil when VALUE is a canonical opaque attachment identity."
  (and (stringp value)
       (string-match-p
        (concat
         "\\`att-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-"
         "[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
        value)))

(defun qq-gateway-attachment (attachment-id)
  "Return a copy of Prepared ATTACHMENT-ID, or nil."
  (qq-gateway-value-copy
   (and attachment-id
        (gethash attachment-id qq-gateway-attachment--attachments))))

(defun qq-gateway-attachments ()
  "Return copied Prepared Attachment snapshots in authoritative order."
  (delq nil
        (mapcar #'qq-gateway-attachment qq-gateway-attachment--order)))

(defun qq-gateway-attachment--clear (reason)
  "Clear the projected attachment registry for REASON."
  (let ((changed (or qq-gateway-attachment--order
                     (> (hash-table-count
                         qq-gateway-attachment--attachments) 0)))
        (resync-marker qq-gateway-attachment--resync-request-id))
    (qq-gateway-rpc-cancel-latest
     'qq-gateway-attachment--refresh-owner "superseded_request"
     "Gateway attachment refresh context was cleared")
    (when (eq resync-marker qq-gateway-attachment--resync-request-id)
      (setq qq-gateway-attachment--resync-request-id nil))
    (setq qq-gateway-attachment--attachments (make-hash-table :test #'equal)
          qq-gateway-attachment--order nil)
    (when changed
      (qq-gateway--run-hook
       'qq-gateway-attachment-changed-hook reason nil))))

(defun qq-gateway-attachment-reset ()
  "Forget the client attachment projection without mutating service state."
  (setq qq-gateway-attachment--gateway-instance-id nil)
  (qq-gateway-attachment--clear 'reset))

(defun qq-gateway-attachment--replace (snapshots reason)
  "Atomically replace attachments with SNAPSHOTS for REASON."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((attachment-id (alist-get 'attachment_id snapshot)))
        (when (gethash attachment-id next)
          (error "qq: Gateway attachment list duplicates %s" attachment-id))
        (puthash attachment-id snapshot next)
        (push attachment-id order)))
    (setq qq-gateway-attachment--attachments next
          qq-gateway-attachment--order (nreverse order))
    (qq-gateway--run-hook 'qq-gateway-attachment-changed-hook reason nil)
    (qq-gateway-attachments)))

(defun qq-gateway-attachment--phase-rank (phase)
  "Return lifecycle rank for attachment PHASE."
  (pcase phase
    ("queued" 0)
    ("negotiating" 1)
    ("uploading" 2)
    ("ready" 3)
    ("sending" 4)
    ((or "failed" "consumed" "canceled") 5)
    (_ -1)))

(defun qq-gateway-attachment--transition-p (from to)
  "Return non-nil when attachment phase FROM may transition to TO."
  (or (equal from to)
      (member (cons from to)
              '(("queued" . "negotiating")
                ("queued" . "failed")
                ("queued" . "canceled")
                ("negotiating" . "uploading")
                ("negotiating" . "ready")
                ("negotiating" . "failed")
                ("negotiating" . "canceled")
                ("uploading" . "ready")
                ("uploading" . "failed")
                ("uploading" . "canceled")
                ("ready" . "sending")
                ("ready" . "canceled")
                ("sending" . "consumed")
                ("sending" . "canceled")))))

(defun qq-gateway-attachment--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same attachment."
  (cl-every (lambda (key)
              (equal (alist-get key left) (alist-get key right)))
            '(attachment_id resource_id account_id conversation use
              bytes_total created_at)))

(defun qq-gateway-attachment--upsert (raw-snapshot reason)
  "Merge RAW-SNAPSHOT for REASON without regressing its lifecycle."
  (let* ((snapshot raw-snapshot)
         (attachment-id (alist-get 'attachment_id snapshot))
         (existing (gethash attachment-id
                            qq-gateway-attachment--attachments)))
    (when (and existing
               (not (qq-gateway-attachment--same-identity-p existing snapshot)))
      (error "qq: Gateway attachment identity changed for %s" attachment-id))
    (cond
     ((null existing)
      (setq qq-gateway-attachment--order
            (append qq-gateway-attachment--order (list attachment-id))))
     ((< (qq-gateway-attachment--phase-rank (alist-get 'phase snapshot))
         (qq-gateway-attachment--phase-rank (alist-get 'phase existing)))
      (setq snapshot existing))
     ((not (qq-gateway-attachment--transition-p
            (alist-get 'phase existing) (alist-get 'phase snapshot)))
      (error "qq: Gateway attachment lifecycle transition is invalid"))
     ((< (alist-get 'updated_at snapshot) (alist-get 'updated_at existing))
      (setq snapshot existing))
     ((and (equal (alist-get 'phase existing) "uploading")
           (equal (alist-get 'phase snapshot) "uploading")
           (qq-gateway--decimal-less-p
            (alist-get 'bytes_done snapshot) (alist-get 'bytes_done existing)))
      (setq snapshot existing))
     ((and (member (alist-get 'phase existing)
                   '("failed" "consumed" "canceled"))
           (not (equal snapshot existing)))
      (error "qq: Gateway attachment terminal snapshot changed")))
    (unless (equal snapshot existing)
      (puthash attachment-id snapshot qq-gateway-attachment--attachments)
      (qq-gateway--run-hook
       'qq-gateway-attachment-changed-hook reason attachment-id))
    (qq-gateway-value-copy snapshot)))

(defun qq-gateway-attachment-refresh (&optional callback errback reason)
  "Fetch the authoritative Prepared Attachment registry."
  (qq-gateway-rpc-latest-call
   'qq-gateway-attachment--refresh-owner "attachment.list" nil
   :projector
   (lambda (result)
     (qq-gateway-attachment--replace
      (alist-get 'attachments result) (or reason 'resync)))
   :callback callback
   :errback errback))

(defun qq-gateway-attachment--conversation-params (session-key)
  "Return target-scoped attachment conversation for SESSION-KEY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (kind (alist-get 'type identity))
         (target (alist-get 'target-id identity)))
    (pcase kind
      ('private `((kind . "private") (peer_uin . ,target)))
      ('group `((kind . "group") (group_uin . ,target)))
      (_ (user-error
          "qq: Prepared attachments support only private or group chats")))))

(defun qq-gateway-attachment--prepare
    (session-key resource-id use media-name callback errback)
  "Prepare RESOURCE-ID for SESSION-KEY with USE.

MEDIA-NAME is used only in local errors.  CALLBACK receives the queued
snapshot; later progress is projected through
`qq-gateway-attachment-changed-hook'."
  (let* ((account-id (or (qq-gateway-current-account-id)
                         (user-error "qq: Select a QQ account first")))
         (resource (qq-gateway-resource resource-id))
         (conversation
          (qq-gateway-attachment--conversation-params session-key)))
    (unless (and resource (equal (alist-get 'phase resource) "ready"))
      (user-error "qq: %s preparation requires a ready staged resource"
                  media-name))
    (qq-gateway-rpc-call
     "attachment.prepare"
     `((account_id . ,account-id)
       (resource_id . ,resource-id)
       (conversation . ,conversation)
       (use . ,use))
     :current-p
     (lambda () (equal account-id (qq-gateway-current-account-id)))
     :stale-code "invalid_gateway_result"
     :stale-message "Selected QQ account changed during attachment preparation"
     :projector
     (lambda (result)
       (qq-gateway-attachment--upsert
        (alist-get 'attachment result) 'prepare-response))
     :callback callback
     :errback errback)))

(defun qq-gateway-attachment-prepare-image
    (session-key resource-id &optional summary sub-type callback errback)
  "Prepare staged RESOURCE-ID as an image for SESSION-KEY.

CALLBACK receives the queued snapshot.  Progress and completion are
projected through `qq-gateway-attachment-changed-hook'."
  (qq-gateway-attachment--prepare
   session-key resource-id
   `((kind . "image")
     (summary . ,(or summary "[图片]"))
     (sub_type . ,(or sub-type 0)))
   "Image" callback errback))

(defun qq-gateway-attachment-prepare-record
    (session-key resource-id &optional callback errback)
  "Prepare staged RESOURCE-ID as native Silk for SESSION-KEY.

The staged resource must already contain message-ready Tencent Silk.  Audio
conversion creates a separate derived resource and is not implicit here.
CALLBACK receives the queued snapshot."
  (qq-gateway-attachment--prepare
   session-key resource-id '((kind . "record")) "Record" callback errback))

(defun qq-gateway-attachment-status
    (attachment-id &optional callback errback)
  "Fetch ATTACHMENT-ID and merge it into the local projection."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-gateway-rpc-call
   "attachment.status" `((attachment_id . ,attachment-id))
   :projector
   (lambda (result)
     (qq-gateway-attachment--upsert
      (alist-get 'attachment result) 'status))
   :callback callback
   :errback errback))

(defun qq-gateway-attachment-release
    (attachment-id &optional callback errback)
  "Idempotently release or cancel Prepared ATTACHMENT-ID."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-gateway-rpc-call
   "attachment.release" `((attachment_id . ,attachment-id))
   :callback callback
   :errback errback))

(defun qq-gateway-attachment--await
    (attachment-id callback errback)
  "Wait until projected ATTACHMENT-ID becomes ready or terminal.

Return a `qq-gateway-watch' that removes this local observer without changing
service state."
  (let (observer watch)
    (setq watch
          (qq-gateway-watch-create
           :active-p t
           :cancel-function
           (lambda ()
             (remove-hook 'qq-gateway-attachment-changed-hook observer))))
    (setq observer
          (lambda (_reason changed-id)
            (when (and (qq-gateway-watch-active-p watch)
                       (or (null changed-id)
                           (equal changed-id attachment-id)))
              (let ((snapshot (qq-gateway-attachment attachment-id)))
                (cond
                 ((null snapshot)
                  (qq-gateway-watch-cancel watch)
                  (qq-gateway--client-error
                   errback "attachment_disappeared"
                   "Prepared attachment disappeared during upload"))
                 ((equal (alist-get 'phase snapshot) "ready")
                  (qq-gateway-watch-cancel watch)
                  (qq-gateway--invoke callback snapshot))
                 ((member (alist-get 'phase snapshot)
                          '("failed" "consumed" "canceled"))
                  (qq-gateway-watch-cancel watch)
                  (let ((problem (alist-get 'error snapshot)))
                    (qq-gateway--client-error
                     errback
                     (or (alist-get 'code problem) "attachment_canceled")
                     "%s"
                     (or (alist-get 'message problem)
                         (format "Prepared attachment became %s before use"
                                 (alist-get 'phase snapshot)))))))))))
    (add-hook 'qq-gateway-attachment-changed-hook observer)
    (funcall observer 'initial attachment-id)
    watch))

(defun qq-gateway-attachment--cancel-local-work (operation)
  "Cancel callback ownership and observers held by OPERATION."
  (when-let* ((request-id
               (qq-gateway-attachment-operation-request-id operation)))
    (qq-gateway-transport-cancel request-id)
    (setf (qq-gateway-attachment-operation-request-id operation) nil))
  (when-let* ((watch
               (qq-gateway-attachment-operation-resource-watch operation)))
    (qq-gateway-watch-cancel watch)
    (setf (qq-gateway-attachment-operation-resource-watch operation) nil))
  (when-let* ((watch
               (qq-gateway-attachment-operation-attachment-watch operation)))
    (qq-gateway-watch-cancel watch)
    (setf (qq-gateway-attachment-operation-attachment-watch operation) nil)))

(defun qq-gateway-attachment--release-created (operation)
  "Best-effort release service objects created for OPERATION."
  (when-let* ((attachment-id
               (qq-gateway-attachment-operation-attachment-id operation)))
    (setf (qq-gateway-attachment-operation-attachment-id operation) nil)
    (condition-case nil
        (qq-gateway-attachment-release attachment-id)
      (error nil)))
  (let ((resource-ids
         (delete-dups
          (delq nil
                (list
                 (qq-gateway-attachment-operation-resource-id operation)
                 (qq-gateway-attachment-operation-source-resource-id
                  operation))))))
    (setf (qq-gateway-attachment-operation-resource-id operation) nil
          (qq-gateway-attachment-operation-source-resource-id operation) nil)
    (dolist (resource-id resource-ids)
      (condition-case nil
          (qq-gateway-resource-release resource-id)
        (error nil)))))

(defun qq-gateway-attachment-cancel-operation (operation)
  "Cancel unfinished stage-and-prepare OPERATION idempotently.

This only revokes client callbacks and objects created by this operation.  It
never stops an account or the long-lived service."
  (when (and (qq-gateway-attachment-operation-p operation)
             (qq-gateway-attachment-operation-active-p operation))
    (setf (qq-gateway-attachment-operation-active-p operation) nil)
    (qq-gateway-attachment--cancel-local-work operation)
    (qq-gateway-attachment--release-created operation)
    t))

(defun qq-gateway-attachment--fail-operation
    (operation errback body reason)
  "Settle OPERATION as failed and invoke ERRBACK with BODY and REASON."
  (when (qq-gateway-attachment-operation-active-p operation)
    (setf (qq-gateway-attachment-operation-active-p operation) nil)
    (qq-gateway-attachment--cancel-local-work operation)
    (qq-gateway-attachment--release-created operation)
    (qq-gateway--invoke errback body reason)))

(defun qq-gateway-attachment--stage-transform-and-prepare
    (path media-name transform-phase transform prepare callback errback)
  "Stage PATH, run TRANSFORM, then PREPARE it for one target.

MEDIA-NAME and TRANSFORM-PHASE describe lifecycle errors.  TRANSFORM receives
the ready source resource plus success and failure callbacks.  PREPARE receives
the final ready resource ID plus success and failure callbacks.  CALLBACK runs
only after the Prepared Attachment reaches `ready'.  Return a cancellable
local operation that owns every service object created before that handoff."
  (let* ((path (expand-file-name path))
         (account-id (or (qq-gateway-current-account-id)
                         (user-error "qq: Select a QQ account first")))
         (operation
          (qq-gateway-attachment-operation-create :active-p t)))
    (cl-labels
        ((fail
          (body reason)
          (qq-gateway-attachment--fail-operation
           operation errback body reason))
         (release-source
          ()
          (when-let* ((resource-id
                       (qq-gateway-attachment-operation-source-resource-id
                        operation)))
            (setf
             (qq-gateway-attachment-operation-source-resource-id operation)
             nil)
            (condition-case nil
                (qq-gateway-resource-release resource-id)
              (error nil))))
         (start-request
          (thunk)
          (condition-case error-data
              (let ((request-id (funcall thunk)))
                (when (and request-id
                           (qq-gateway-attachment-operation-active-p
                            operation))
                  (setf
                   (qq-gateway-attachment-operation-request-id operation)
                   request-id)))
            (error
             (fail nil (error-message-string error-data)))))
         (await-resource
          (resource-id ready-callback)
          (let ((watch
                 (qq-gateway-resource-await-ready
                  resource-id ready-callback #'fail)))
            (when (qq-gateway-watch-active-p watch)
              (setf
               (qq-gateway-attachment-operation-resource-watch operation)
               watch))))
         (await-attachment
          (attachment)
          (when (qq-gateway-attachment-operation-active-p operation)
            (let ((attachment-id (alist-get 'attachment_id attachment)))
              (setf (qq-gateway-attachment-operation-request-id operation) nil
                    (qq-gateway-attachment-operation-attachment-id operation)
                    attachment-id)
              (let ((watch
                     (qq-gateway-attachment--await
                      attachment-id
                      (lambda (ready)
                        (when
                            (qq-gateway-attachment-operation-active-p operation)
                          (if (not (equal account-id
                                          (qq-gateway-current-account-id)))
                              (fail
                               nil
                               (format
                                "Selected QQ account changed during %s preparation"
                                media-name))
                            (setf
                             (qq-gateway-attachment-operation-active-p operation)
                             nil
                             (qq-gateway-attachment-operation-attachment-watch
                              operation)
                             nil)
                            (qq-gateway--invoke callback ready))))
                      #'fail)))
                (when (qq-gateway-watch-active-p watch)
                  (setf
                   (qq-gateway-attachment-operation-attachment-watch operation)
                   watch))))))
         (prepare-final
          (_resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (setf (qq-gateway-attachment-operation-resource-watch
                   operation)
                  nil)
            (if (not (equal account-id (qq-gateway-current-account-id)))
                (fail
                 nil
                 (format "Selected QQ account changed during %s"
                         transform-phase))
              (start-request
               (lambda ()
                 (funcall
                  prepare
                  (qq-gateway-attachment-operation-resource-id operation)
                  #'await-attachment #'fail))))))
         (transform-complete
          (resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (let* ((resource-id (alist-get 'resource_id resource))
                   (source-id
                    (qq-gateway-attachment-operation-source-resource-id
                     operation)))
              (setf (qq-gateway-attachment-operation-request-id operation) nil
                    (qq-gateway-attachment-operation-resource-id operation)
                    resource-id)
              (when (equal source-id resource-id)
                (setf
                 (qq-gateway-attachment-operation-source-resource-id operation)
                 nil))
              (if (not (equal account-id (qq-gateway-current-account-id)))
                  (fail
                   nil
                   (format "Selected QQ account changed during %s"
                           transform-phase))
                ;; A distinct derived resource no longer depends on the staged
                ;; source once the transform request has returned.
                (release-source)
                (await-resource resource-id #'prepare-final)))))
         (source-ready
          (resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (setf (qq-gateway-attachment-operation-resource-watch
                   operation)
                  nil)
            (if (not (equal account-id (qq-gateway-current-account-id)))
                (fail
                 nil
                 (format "Selected QQ account changed during %s staging"
                         media-name))
              (start-request
               (lambda ()
                 (funcall transform resource #'transform-complete #'fail))))))
         (stage-complete
          (resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (let* ((resource-id (alist-get 'resource_id resource))
                   (source-id
                    (qq-gateway-attachment-operation-source-resource-id
                     operation)))
              (setf (qq-gateway-attachment-operation-request-id operation) nil
                    (qq-gateway-attachment-operation-source-resource-id
                     operation)
                    resource-id)
              (when (and source-id (not (equal source-id resource-id)))
                (error "qq: Resource stage changed identity within one operation"))
              (await-resource resource-id #'source-ready)))))
      (condition-case error-data
          (let ((request-id
                 (qq-gateway-resource-stage-local
                  path (file-name-nondirectory path) nil
                  #'stage-complete #'fail)))
            (when (and request-id
                       (qq-gateway-attachment-operation-active-p operation))
              (setf (qq-gateway-attachment-operation-request-id operation)
                    request-id)))
        ((error quit)
         (setf (qq-gateway-attachment-operation-active-p operation) nil)
         (qq-gateway-attachment--release-created operation)
         (signal (car error-data) (cdr error-data))))
      operation)))

(defun qq-gateway-attachment-stage-and-prepare-image
    (session-key path &optional summary sub-type callback errback)
  "Stage local PATH and prepare one ready image for SESSION-KEY.

CALLBACK runs only after the Prepared Attachment reaches `ready'.  Return a
cancellable local operation; canceling it also best-effort releases every
resource or attachment already created for it."
  (qq-gateway-attachment--stage-transform-and-prepare
   path "image" "image staging"
   (lambda (resource success _failure)
     (funcall success resource))
   (lambda (resource-id success failure)
     (qq-gateway-attachment-prepare-image
      session-key resource-id summary sub-type success failure))
   callback errback))

(defun qq-gateway-attachment-stage-and-prepare-record
    (session-key path &optional callback errback)
  "Stage PCM WAV at PATH and prepare one native record for SESSION-KEY.

The account-neutral source is derived into a distinct Tencent Silk resource.
Once derivation succeeds, the temporary WAV resource is released while the
Silk resource proceeds through target-scoped attachment preparation.  Return a
cancellable operation with the same ownership semantics as the image helper."
  (qq-gateway-attachment--stage-transform-and-prepare
   path "record" "record derivation"
   (lambda (resource success failure)
     (qq-gateway-resource-derive-record
      (alist-get 'resource_id resource) nil success failure))
   (lambda (resource-id success failure)
     (qq-gateway-attachment-prepare-record
      session-key resource-id success failure))
   callback errback))

(defun qq-gateway-attachment-assert-sendable
    (attachment-id session-key account-id &optional expected-use)
  "Return ATTACHMENT-ID after strict SESSION-KEY and ACCOUNT-ID checks.

ACCOUNT-ID is a stable account-slot identity.  EXPECTED-USE defaults to image
and may be \"record\"."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Media segment lacks an opaque attachment ID"))
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Prepared attachment requires a stable account ID"))
  (let ((expected-use (or expected-use "image"))
        (snapshot (qq-gateway-attachment attachment-id))
        (conversation
         (qq-gateway-attachment--conversation-params session-key)))
    (unless (member expected-use '("image" "record"))
      (error "qq: Unknown prepared attachment use %S" expected-use))
    (unless snapshot
      (user-error "qq: Prepared attachment %s is not projected" attachment-id))
    (unless (equal (alist-get 'phase snapshot) "ready")
      (user-error "qq: Prepared attachment %s is not ready" attachment-id))
    (unless (equal (alist-get 'account_id snapshot) account-id)
      (user-error "qq: Prepared attachment belongs to another account"))
    (unless (equal (alist-get 'conversation snapshot) conversation)
      (user-error "qq: Prepared attachment belongs to another conversation"))
    (unless (equal (alist-get 'kind (alist-get 'use snapshot)) expected-use)
      (user-error "qq: Prepared attachment is not %s" expected-use))
    attachment-id))

(defun qq-gateway-attachment--request-resync (reason)
  "Request one authoritative attachment refresh for REASON."
  (qq-gateway-rpc-request-single-flight
   'qq-gateway-attachment--resync-request-id 'attachment-resync
   (lambda (success failure)
     (qq-gateway-attachment-refresh success failure reason))
   "attachment"))

(defun qq-gateway-attachment--handle-ready (instance-id)
  "Synchronize attachments after Gateway ready INSTANCE-ID."
  (unless (qq-gateway--non-empty-string-p instance-id)
    (error "qq: Gateway ready instance identity is malformed"))
  (unless (equal instance-id qq-gateway-attachment--gateway-instance-id)
    (qq-gateway-attachment--clear 'gateway-changed))
  (setq qq-gateway-attachment--gateway-instance-id
        (qq-gateway-value-copy instance-id))
  (if (qq-gateway--method-available-p "attachment.list")
      (qq-gateway-attachment--request-resync 'ready)
    (qq-gateway-attachment--clear 'capability-unavailable)))

(defun qq-gateway-attachment--handle-event (event data)
  "Project native service attachment EVENT with DATA."
  (pcase event
    ("attachment.changed"
     (qq-gateway-attachment--upsert
      (alist-get 'attachment data) 'changed))
    (_ (error "qq: Unowned Gateway attachment event %s" event))))

(defun qq-gateway-attachment--handle-protocol-error (body)
  "Resynchronize after unsolicited attachment stream error BODY."
  (when (equal (alist-get 'code body) "attachment_event_stream_lagged")
    (qq-gateway--run-hook
     'qq-gateway-attachment-desync-hook
     body)
    (qq-gateway-attachment--request-resync 'resync)))

(add-hook 'qq-gateway-ready-hook #'qq-gateway-attachment--handle-ready)
(qq-gateway-dispatch-register-event
 "attachment.changed" #'qq-gateway-attachment--handle-event)
(qq-gateway-dispatch-register-error
 "attachment_event_stream_lagged"
 #'qq-gateway-attachment--handle-protocol-error)

(provide 'qq-gateway-attachment)

;;; qq-gateway-attachment.el ends here
