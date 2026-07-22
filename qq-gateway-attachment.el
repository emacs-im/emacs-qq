;;; qq-gateway-attachment.el --- Native prepared attachments -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Strict projection and upload-ahead helpers for Gateway Prepared
;; Attachments.  Staged resources remain account-neutral; attachments bind
;; those immutable bytes to one account generation, one conversation, and one
;; media use.  Native MsgInfo, upload keys, Highway tickets, and local paths
;; never enter this projection.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway)
(require 'qq-gateway-resource)
(require 'qq-gateway-transport)
(require 'qq-state)

(defconst qq-gateway-attachment--phases
  '("queued" "negotiating" "uploading" "ready"
    "failed" "sending" "consumed" "canceled")
  "Closed Prepared Attachment phases implemented by this client.")

(defvar qq-gateway-attachment-changed-hook nil
  "Hook called with REASON and ATTACHMENT-ID after projection changes.")

(defvar qq-gateway-attachment-desync-hook nil
  "Hook called with an unsolicited attachment stream error body.")

(defvar qq-gateway-attachment--attachments (make-hash-table :test #'equal))
(defvar qq-gateway-attachment--order nil)
(defvar qq-gateway-attachment--gateway-instance-id nil)
(defvar qq-gateway-attachment--resync-request-id nil)

(cl-defstruct (qq-gateway-attachment-operation
               (:constructor qq-gateway-attachment-operation-create))
  "One client-side stage-and-prepare operation.

The service owns byte copying and QQ upload work.  This object owns only the
request callbacks and temporary resource/attachment identities created on
behalf of one caller."
  active-p
  request-id
  resource-id
  attachment-id
  resource-wait-cancel
  attachment-wait-cancel)

(defun qq-gateway-attachment--id-p (value)
  "Return non-nil when VALUE is a canonical opaque attachment identity."
  (and (stringp value)
       (string-match-p
        (concat
         "\\`att-[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-"
         "[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\'")
        value)))

(defun qq-gateway-attachment--uint32-p (value)
  "Return non-nil when VALUE is an unsigned 32-bit integer."
  (and (integerp value) (<= 0 value) (<= value #xffffffff)))

(defun qq-gateway-attachment--timestamp-p (value)
  "Return non-nil when VALUE is a non-negative integer timestamp."
  (and (integerp value) (<= 0 value)))

(defun qq-gateway-attachment--boolean-p (value)
  "Return non-nil when VALUE is a decoded JSON boolean."
  (memq value '(t :false)))

(defun qq-gateway-attachment--decimal-less-or-equal-p (left right)
  "Return non-nil when canonical decimal LEFT is at most RIGHT."
  (or (equal left right) (qq-gateway--decimal-less-p left right)))

(defun qq-gateway-attachment--validate-problem (problem)
  "Validate and return attachment PROBLEM or nil."
  (when problem
    (unless (and (qq-gateway--exact-object-keys-p problem '(code message))
                 (qq-gateway--non-empty-string-p (alist-get 'code problem))
                 (qq-gateway--non-empty-string-p (alist-get 'message problem)))
      (error "qq: Gateway attachment problem is malformed")))
  problem)

(defun qq-gateway-attachment--validate-conversation (conversation)
  "Validate and copy attachment CONVERSATION."
  (unless (listp conversation)
    (error "qq: Gateway attachment conversation must be an object"))
  (pcase (alist-get 'kind conversation)
    ("private"
     (let ((uin (alist-get 'peer_uin conversation))
           (uid (alist-get 'peer_uid conversation)))
       (unless (or (and (qq-gateway--exact-object-keys-p
                         conversation '(kind peer_uin))
                        (qq-gateway--canonical-decimal-p uin))
                   (and (qq-gateway--exact-object-keys-p
                         conversation '(kind peer_uid))
                        (qq-gateway--non-empty-string-p uid)))
         (error "qq: Gateway private attachment conversation is malformed"))))
    ("group"
     (unless (and (qq-gateway--exact-object-keys-p
                   conversation '(kind group_uin))
                  (qq-gateway--canonical-decimal-p
                   (alist-get 'group_uin conversation)))
       (error "qq: Gateway group attachment conversation is malformed")))
    (_ (error "qq: Gateway attachment conversation has unknown kind")))
  (copy-tree conversation))

(defun qq-gateway-attachment--validate-use (use)
  "Validate and copy attachment USE."
  (pcase (alist-get 'kind use)
    ("image"
     (unless (and (qq-gateway--exact-object-keys-p
                   use '(kind summary sub_type))
                  (qq-gateway--non-empty-string-p (alist-get 'summary use))
                  (<= (length (string-to-list (alist-get 'summary use))) 128)
                  (not (string-match-p
                        "[[:cntrl:]]" (alist-get 'summary use)))
                  (qq-gateway-attachment--uint32-p
                   (alist-get 'sub_type use)))
       (error "qq: Gateway image attachment use is malformed")))
    ("record"
     (unless (qq-gateway--exact-object-keys-p use '(kind))
       (error "qq: Gateway record attachment use is malformed")))
    (_ (error "qq: Gateway attachment use has unknown kind")))
  (copy-tree use))

(defun qq-gateway-attachment--validate-snapshot (snapshot)
  "Validate and copy one closed Prepared Attachment SNAPSHOT."
  (unless (qq-gateway--exact-object-keys-p
           snapshot
           '(attachment_id resource_id account_id generation conversation use
             phase bytes_done bytes_total fast_path created_at updated_at error))
    (error "qq: Gateway attachment snapshot has invalid fields"))
  (let ((attachment-id (alist-get 'attachment_id snapshot))
        (resource-id (alist-get 'resource_id snapshot))
        (account-id (alist-get 'account_id snapshot))
        (generation (alist-get 'generation snapshot))
        (phase (alist-get 'phase snapshot))
        (bytes-done (alist-get 'bytes_done snapshot))
        (bytes-total (alist-get 'bytes_total snapshot))
        (fast-path (alist-get 'fast_path snapshot))
        (created-at (alist-get 'created_at snapshot))
        (updated-at (alist-get 'updated_at snapshot))
        (problem (alist-get 'error snapshot)))
    (unless (qq-gateway-attachment--id-p attachment-id)
      (error "qq: Gateway attachment_id must be an opaque att- UUID"))
    (unless (qq-gateway-resource--opaque-id-p resource-id)
      (error "qq: Gateway attachment resource_id is malformed"))
    (unless (qq-gateway--non-empty-string-p account-id)
      (error "qq: Gateway attachment account_id is malformed"))
    (unless (qq-gateway--canonical-decimal-p generation)
      (error "qq: Gateway attachment generation must be positive decimal text"))
    (qq-gateway-attachment--validate-conversation
     (alist-get 'conversation snapshot))
    (qq-gateway-attachment--validate-use (alist-get 'use snapshot))
    (unless (member phase qq-gateway-attachment--phases)
      (error "qq: Gateway attachment has unknown phase"))
    (unless (and (qq-gateway--canonical-decimal-p bytes-done t)
                 (qq-gateway--canonical-decimal-p bytes-total)
                 (qq-gateway-attachment--decimal-less-or-equal-p
                  bytes-done bytes-total))
      (error "qq: Gateway attachment byte progress is malformed"))
    (unless (and (qq-gateway-attachment--timestamp-p created-at)
                 (qq-gateway-attachment--timestamp-p updated-at)
                 (<= created-at updated-at))
      (error "qq: Gateway attachment timestamps are malformed"))
    (qq-gateway-attachment--validate-problem problem)
    (pcase phase
      ((or "queued" "negotiating" "uploading")
       (when (or fast-path problem)
         (error "qq: Gateway active attachment carries terminal metadata")))
      ("ready"
       (unless (and (qq-gateway-attachment--boolean-p fast-path)
                    (null problem)
                    (if (eq fast-path t)
                        (equal bytes-done "0")
                      (equal bytes-done bytes-total)))
         (error "qq: Gateway ready attachment has contradictory metadata")))
      ("failed"
       (unless (and problem (null fast-path))
         (error "qq: Gateway failed attachment lacks an exclusive error")))
      ((or "sending" "consumed")
       (unless (and (qq-gateway-attachment--boolean-p fast-path)
                    (null problem))
         (error "qq: Gateway sent attachment has contradictory metadata")))
      ("canceled"
       (unless (and (or (null fast-path)
                        (qq-gateway-attachment--boolean-p fast-path))
                    (null problem))
         (error "qq: Gateway canceled attachment is malformed"))))
    (copy-tree snapshot)))

(defun qq-gateway-attachment (attachment-id)
  "Return a copy of Prepared ATTACHMENT-ID, or nil."
  (copy-tree (and attachment-id
                  (gethash attachment-id
                           qq-gateway-attachment--attachments))))

(defun qq-gateway-attachments ()
  "Return copied Prepared Attachment snapshots in authoritative order."
  (delq nil
        (mapcar #'qq-gateway-attachment qq-gateway-attachment--order)))

(defun qq-gateway-attachment--clear (reason)
  "Clear the projected attachment registry for REASON."
  (let ((changed (or qq-gateway-attachment--order
                     (> (hash-table-count
                         qq-gateway-attachment--attachments) 0))))
    (setq qq-gateway-attachment--attachments (make-hash-table :test #'equal)
          qq-gateway-attachment--order nil
          qq-gateway-attachment--resync-request-id nil)
    (when changed
      (qq-gateway--run-hook
       'qq-gateway-attachment-changed-hook reason nil))))

(defun qq-gateway-attachment-reset ()
  "Forget the client attachment projection without mutating service state."
  (setq qq-gateway-attachment--gateway-instance-id nil)
  (qq-gateway-attachment--clear 'reset))

(defun qq-gateway-attachment--replace (snapshots reason)
  "Atomically replace attachments with SNAPSHOTS for REASON."
  (unless (listp snapshots)
    (error "qq: Gateway attachment list must be an array"))
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (raw snapshots)
      (let* ((snapshot (qq-gateway-attachment--validate-snapshot raw))
             (attachment-id (alist-get 'attachment_id snapshot)))
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
            '(attachment_id resource_id account_id generation conversation use
              bytes_total created_at)))

(defun qq-gateway-attachment--upsert (raw-snapshot reason)
  "Merge RAW-SNAPSHOT for REASON without regressing its lifecycle."
  (let* ((snapshot (qq-gateway-attachment--validate-snapshot raw-snapshot))
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
    (unless (eq snapshot existing)
      (puthash attachment-id snapshot qq-gateway-attachment--attachments)
      (qq-gateway--run-hook
       'qq-gateway-attachment-changed-hook reason attachment-id))
    (copy-tree snapshot)))

(defun qq-gateway-attachment--validate-single-result (result context)
  "Validate and return attachment in RESULT for CONTEXT."
  (unless (qq-gateway--exact-object-keys-p result '(attachment))
    (error "qq: Gateway %s result has invalid fields" context))
  (qq-gateway-attachment--validate-snapshot
   (alist-get 'attachment result)))

(defun qq-gateway-attachment--validate-list-result (result)
  "Validate and return attachments carried by attachment.list RESULT."
  (unless (qq-gateway--exact-object-keys-p result '(attachments))
    (error "qq: Gateway attachment.list result has invalid fields"))
  (let ((attachments (alist-get 'attachments result nil nil #'eq)))
    (unless (listp attachments)
      (error "qq: Gateway attachment.list attachments must be an array"))
    attachments))

(defun qq-gateway-attachment-refresh (&optional callback errback reason)
  "Fetch the authoritative Prepared Attachment registry."
  (qq-gateway--send
   "attachment.list" nil
   (lambda (result)
     (condition-case error-data
         (let ((attachments
                (qq-gateway-attachment--replace
                 (qq-gateway-attachment--validate-list-result result)
                 (or reason 'resync))))
           (setq qq-gateway-attachment--resync-request-id nil)
           (qq-gateway--invoke callback attachments))
       (error
        (setq qq-gateway-attachment--resync-request-id nil)
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   (lambda (body failure)
     (setq qq-gateway-attachment--resync-request-id nil)
     (qq-gateway--invoke errback body failure))))

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
  "Prepare RESOURCE-ID for SESSION-KEY with closed USE.

MEDIA-NAME is used only in local errors.  CALLBACK receives the validated
queued snapshot; later progress is projected through
`qq-gateway-attachment-changed-hook'."
  (let* ((owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a QQ account first")))
         (resource (qq-gateway-resource resource-id))
         (conversation
          (qq-gateway-attachment--conversation-params session-key)))
    (unless (and resource (equal (alist-get 'phase resource) "ready"))
      (user-error "qq: %s preparation requires a ready staged resource"
                  media-name))
    (qq-gateway-attachment--validate-use use)
    (qq-gateway--send
     "attachment.prepare"
     `((account_id . ,(car owner))
       (resource_id . ,resource-id)
       (conversation . ,conversation)
       (use . ,use))
     (lambda (result)
       (condition-case error-data
           (let ((snapshot
                  (qq-gateway-attachment--validate-single-result
                   result "attachment.prepare")))
             (unless (and (equal (alist-get 'phase snapshot) "queued")
                          (equal (alist-get 'resource_id snapshot) resource-id)
                          (equal (alist-get 'account_id snapshot) (car owner))
                          (equal (alist-get 'generation snapshot) (cdr owner))
                          (equal (alist-get 'conversation snapshot) conversation)
                          (equal (alist-get 'use snapshot) use))
               (error "qq: Gateway attachment.prepare response contradicts request"))
             (setq snapshot
                   (qq-gateway-attachment--upsert snapshot 'prepare-response))
             (qq-gateway--invoke callback snapshot))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

(defun qq-gateway-attachment-prepare-image
    (session-key resource-id &optional summary sub-type callback errback)
  "Prepare staged RESOURCE-ID as an image for SESSION-KEY.

CALLBACK receives the validated queued snapshot.  Progress and completion are
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
CALLBACK receives the validated queued snapshot."
  (qq-gateway-attachment--prepare
   session-key resource-id '((kind . "record")) "Record" callback errback))

(defun qq-gateway-attachment-status
    (attachment-id &optional callback errback)
  "Fetch ATTACHMENT-ID and merge it into the local projection."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-gateway--send
   "attachment.status" `((attachment_id . ,attachment-id))
   (lambda (result)
     (condition-case error-data
         (let ((snapshot
                (qq-gateway-attachment--validate-single-result
                 result "attachment.status")))
           (unless (equal attachment-id (alist-get 'attachment_id snapshot))
             (error "qq: Gateway attachment.status identity contradicts request"))
           (setq snapshot (qq-gateway-attachment--upsert snapshot 'status))
           (qq-gateway--invoke callback snapshot))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

(defun qq-gateway-attachment-release
    (attachment-id &optional callback errback)
  "Idempotently release or cancel Prepared ATTACHMENT-ID."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Attachment ID must be an opaque att- UUID"))
  (qq-gateway--send
   "attachment.release" `((attachment_id . ,attachment-id))
   (lambda (result)
     (condition-case error-data
         (progn
           (unless (and (qq-gateway--exact-object-keys-p
                         result '(attachment_id released))
                        (equal (alist-get 'attachment_id result) attachment-id)
                        (memq (alist-get 'released result) '(t :false)))
             (error "qq: Gateway attachment.release receipt is malformed"))
           (qq-gateway--invoke callback (copy-tree result)))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

(defun qq-gateway-attachment--await
    (attachment-id callback errback)
  "Wait until projected ATTACHMENT-ID becomes ready or terminal.

Return a function that removes this local observer without changing service
state."
  (let (observer finished)
    (setq observer
          (lambda (_reason changed-id)
            (when (and (not finished)
                       (or (null changed-id)
                           (equal changed-id attachment-id)))
              (let ((snapshot (qq-gateway-attachment attachment-id)))
                (cond
                 ((null snapshot)
                  (setq finished t)
                  (remove-hook 'qq-gateway-attachment-changed-hook observer)
                  (qq-gateway--client-error
                   errback "attachment_disappeared"
                   "Prepared attachment disappeared during upload"))
                 ((equal (alist-get 'phase snapshot) "ready")
                  (setq finished t)
                  (remove-hook 'qq-gateway-attachment-changed-hook observer)
                  (qq-gateway--invoke callback snapshot))
                 ((member (alist-get 'phase snapshot)
                          '("failed" "consumed" "canceled"))
                  (setq finished t)
                  (remove-hook 'qq-gateway-attachment-changed-hook observer)
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
    (lambda ()
      (unless finished
        (setq finished t)
        (remove-hook 'qq-gateway-attachment-changed-hook observer)))))

(defun qq-gateway-attachment--await-resource
    (resource-id callback errback)
  "Wait until staged RESOURCE-ID becomes ready or terminal.

Return a function that removes this local observer without changing service
state."
  (let (observer finished)
    (setq observer
          (lambda (_reason changed-id)
            (when (and (not finished)
                       (or (null changed-id) (equal changed-id resource-id)))
              (let ((resource (qq-gateway-resource resource-id)))
                (cond
                 ((null resource)
                  (setq finished t)
                  (remove-hook 'qq-gateway-resource-changed-hook observer)
                  (qq-gateway--client-error
                   errback "resource_disappeared"
                   "Staged image resource disappeared"))
                 ((equal (alist-get 'phase resource) "ready")
                  (setq finished t)
                  (remove-hook 'qq-gateway-resource-changed-hook observer)
                  (qq-gateway--invoke callback resource))
                 ((member (alist-get 'phase resource) '("failed" "released"))
                  (setq finished t)
                  (remove-hook 'qq-gateway-resource-changed-hook observer)
                  (let ((problem (alist-get 'error resource)))
                    (qq-gateway--client-error
                     errback
                     (or (alist-get 'code problem) "resource_released")
                     "%s"
                     (or (alist-get 'message problem)
                         "Staged image resource was released")))))))))
    (add-hook 'qq-gateway-resource-changed-hook observer)
    (funcall observer 'initial resource-id)
    (lambda ()
      (unless finished
        (setq finished t)
        (remove-hook 'qq-gateway-resource-changed-hook observer)))))

(defun qq-gateway-attachment--cancel-local-work (operation)
  "Cancel callback ownership and observers held by OPERATION."
  (when-let* ((request-id
               (qq-gateway-attachment-operation-request-id operation)))
    (when (stringp request-id)
      (qq-gateway-transport-cancel request-id))
    (setf (qq-gateway-attachment-operation-request-id operation) nil))
  (when-let* ((cancel
               (qq-gateway-attachment-operation-resource-wait-cancel
                operation)))
    (when (functionp cancel)
      (funcall cancel))
    (setf (qq-gateway-attachment-operation-resource-wait-cancel operation)
          nil))
  (when-let* ((cancel
               (qq-gateway-attachment-operation-attachment-wait-cancel
                operation)))
    (when (functionp cancel)
      (funcall cancel))
    (setf (qq-gateway-attachment-operation-attachment-wait-cancel operation)
          nil)))

(defun qq-gateway-attachment--release-created (operation)
  "Best-effort release service objects created for OPERATION."
  (when-let* ((attachment-id
               (qq-gateway-attachment-operation-attachment-id operation)))
    (setf (qq-gateway-attachment-operation-attachment-id operation) nil)
    (condition-case nil
        (qq-gateway-attachment-release attachment-id)
      (error nil)))
  (when-let* ((resource-id
               (qq-gateway-attachment-operation-resource-id operation)))
    (setf (qq-gateway-attachment-operation-resource-id operation) nil)
    (condition-case nil
        (qq-gateway-resource-release resource-id)
      (error nil))))

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

(defun qq-gateway-attachment-stage-and-prepare-image
    (session-key path &optional summary sub-type callback errback)
  "Stage local PATH and prepare one ready image for SESSION-KEY.

CALLBACK runs only after the Prepared Attachment reaches `ready'.  The local
path is used solely by `resource.stage_local' and is not retained in either
service projection.  Return a cancellable local operation; canceling it also
best-effort releases any resource or attachment already created for it."
  (let* ((path (expand-file-name path))
         (owner (or (qq-gateway-current-account-owner)
                    (user-error "qq: Select a QQ account first")))
         (operation
          (qq-gateway-attachment-operation-create :active-p t)))
    (cl-labels
        ((fail
          (body reason)
          (qq-gateway-attachment--fail-operation
           operation errback body reason))
         (await-attachment
          (attachment)
          (when (qq-gateway-attachment-operation-active-p operation)
            (let* ((attachment-id (alist-get 'attachment_id attachment))
                   (marker (list 'attachment-wait)))
              (setf (qq-gateway-attachment-operation-request-id operation) nil
                    (qq-gateway-attachment-operation-attachment-id operation)
                    attachment-id
                    (qq-gateway-attachment-operation-attachment-wait-cancel
                     operation)
                    marker)
              (let ((cancel
                     (qq-gateway-attachment--await
                      attachment-id
                      (lambda (ready)
                        (when
                            (qq-gateway-attachment-operation-active-p operation)
                          (if (not (equal owner
                                          (qq-gateway-current-account-owner)))
                              (fail nil
                                    "QQ account generation changed during image preparation")
                            (setf
                             (qq-gateway-attachment-operation-active-p operation)
                             nil
                             (qq-gateway-attachment-operation-attachment-wait-cancel
                              operation)
                             nil)
                            (qq-gateway--invoke callback ready))))
                      #'fail)))
                (when (eq marker
                          (qq-gateway-attachment-operation-attachment-wait-cancel
                           operation))
                  (setf
                   (qq-gateway-attachment-operation-attachment-wait-cancel
                    operation)
                   cancel))))))
         (prepare
          (_resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (setf (qq-gateway-attachment-operation-resource-wait-cancel
                   operation)
                  nil)
            (if (not (equal owner (qq-gateway-current-account-owner)))
                (fail nil "QQ account generation changed during image staging")
              (let ((marker (list 'attachment-prepare)))
                (setf (qq-gateway-attachment-operation-request-id operation)
                      marker)
                (condition-case error-data
                    (let ((request-id
                           (qq-gateway-attachment-prepare-image
                            session-key
                            (qq-gateway-attachment-operation-resource-id
                             operation)
                            summary sub-type #'await-attachment #'fail)))
                      (when (eq marker
                                (qq-gateway-attachment-operation-request-id
                                 operation))
                        (setf
                         (qq-gateway-attachment-operation-request-id operation)
                         request-id)))
                  (error
                   (fail nil (error-message-string error-data))))))))
         (stage-complete
          (resource)
          (when (qq-gateway-attachment-operation-active-p operation)
            (let* ((resource-id (alist-get 'resource_id resource))
                   (marker (list 'resource-wait)))
              (setf (qq-gateway-attachment-operation-request-id operation) nil
                    (qq-gateway-attachment-operation-resource-id operation)
                    resource-id
                    (qq-gateway-attachment-operation-resource-wait-cancel
                     operation)
                    marker)
              (let ((cancel
                     (qq-gateway-attachment--await-resource
                      resource-id #'prepare #'fail)))
                (when (eq marker
                          (qq-gateway-attachment-operation-resource-wait-cancel
                           operation))
                  (setf
                   (qq-gateway-attachment-operation-resource-wait-cancel
                    operation)
                   cancel)))))))
      (let ((marker (list 'resource-stage)))
        (setf (qq-gateway-attachment-operation-request-id operation) marker)
        (condition-case error-data
            (let ((request-id
                   (qq-gateway-resource-stage-local
                    path (file-name-nondirectory path) nil
                    #'stage-complete #'fail)))
              (when (eq marker
                        (qq-gateway-attachment-operation-request-id operation))
                (setf (qq-gateway-attachment-operation-request-id operation)
                      request-id)))
          (error
           (setf (qq-gateway-attachment-operation-active-p operation) nil)
           (qq-gateway-attachment--release-created operation)
           (signal (car error-data) (cdr error-data)))))
      operation)))

(defun qq-gateway-attachment-assert-sendable
    (attachment-id session-key owner &optional expected-use)
  "Return ATTACHMENT-ID after strict SESSION-KEY and OWNER checks.

EXPECTED-USE defaults to image and may be \"record\"."
  (unless (qq-gateway-attachment--id-p attachment-id)
    (user-error "qq: Media segment lacks an opaque attachment ID"))
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
    (unless (and (equal (alist-get 'account_id snapshot) (car owner))
                 (equal (alist-get 'generation snapshot) (cdr owner)))
      (user-error "qq: Prepared attachment belongs to another account generation"))
    (unless (equal (alist-get 'conversation snapshot) conversation)
      (user-error "qq: Prepared attachment belongs to another conversation"))
    (unless (equal (alist-get 'kind (alist-get 'use snapshot)) expected-use)
      (user-error "qq: Prepared attachment is not %s" expected-use))
    attachment-id))

(defun qq-gateway-attachment--request-resync (reason)
  "Request one authoritative attachment refresh for REASON."
  (unless qq-gateway-attachment--resync-request-id
    (let ((marker (list 'attachment-resync)))
      (setq qq-gateway-attachment--resync-request-id marker)
      (let ((request-id
             (qq-gateway-attachment-refresh
              nil
              (lambda (_body failure)
                (message "qq: Gateway attachment resync failed: %s" failure))
              reason)))
        (when (eq qq-gateway-attachment--resync-request-id marker)
          (setq qq-gateway-attachment--resync-request-id request-id))))))

(defun qq-gateway-attachment--handle-event (event data)
  "Project native service attachment EVENT with DATA."
  (condition-case error-data
      (pcase event
        ("gateway.ready"
         (unless (qq-gateway--exact-object-keys-p
                  data '(gateway_instance_id accounts))
           (error "qq: Gateway.ready data has invalid fields"))
         (let ((instance-id (alist-get 'gateway_instance_id data)))
           (unless (qq-gateway--non-empty-string-p instance-id)
             (error "qq: Gateway.ready instance identity is malformed"))
           (unless (equal instance-id
                          qq-gateway-attachment--gateway-instance-id)
             (qq-gateway-attachment--clear 'gateway-changed))
           (setq qq-gateway-attachment--gateway-instance-id instance-id)
           (if (qq-gateway--method-available-p "attachment.list")
               (qq-gateway-attachment--request-resync 'ready)
             (qq-gateway-attachment--clear 'capability-unavailable))))
        ("attachment.changed"
         (unless (qq-gateway--exact-object-keys-p data '(attachment))
           (error "qq: Gateway attachment.changed data has invalid fields"))
         (qq-gateway-attachment--upsert
          (alist-get 'attachment data) 'changed)))
    (error
     (qq-gateway-transport--protocol-violation
      "Malformed %s event: %s" event
      (error-message-string error-data)))))

(defun qq-gateway-attachment--handle-protocol-error (body)
  "Resynchronize after unsolicited attachment stream error BODY."
  (when (equal (alist-get 'code body) "attachment_event_stream_lagged")
    (qq-gateway--run-hook
     'qq-gateway-attachment-desync-hook (copy-tree body))
    (qq-gateway-attachment--request-resync 'resync)))

(add-hook 'qq-gateway-transport-event-hook
          #'qq-gateway-attachment--handle-event)
(add-hook 'qq-gateway-transport-protocol-error-hook
          #'qq-gateway-attachment--handle-protocol-error)

(provide 'qq-gateway-attachment)

;;; qq-gateway-attachment.el ends here
