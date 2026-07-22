;;; qq-gateway-resource.el --- Native Gateway resource projection -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Strict client-side projection for Gateway-owned immutable staged resources.
;; Local paths are accepted only by the explicit staging command and are never
;; retained in resource snapshots.  Resource identities remain account-neutral
;; and survive websocket reconnects; `resource.list' is the resync boundary.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway)

(defconst qq-gateway-resource--phases
  '("staging" "ready" "failed" "released")
  "Closed staged-resource phases implemented by this client.")

(defvar qq-gateway-resource-changed-hook nil
  "Hook called with REASON and RESOURCE-ID after resource projection changes.")

(defvar qq-gateway-resource-desync-hook nil
  "Hook called with an unsolicited resource stream error body.")

(defvar qq-gateway-resource--resources (make-hash-table :test #'equal))
(defvar qq-gateway-resource--order nil)
(defvar qq-gateway-resource--gateway-instance-id nil)
(defvar qq-gateway-resource--resync-request-id nil)

(defun qq-gateway-resource--opaque-id-p (value)
  "Return non-nil when VALUE is an opaque staged-resource identity."
  (and (stringp value)
       (string-prefix-p "res-" value)
       (> (length value) 4)
       (not (string-match-p "[[:space:][:cntrl:]]" value))))

(defun qq-gateway-resource--hex-digest-p (value length)
  "Return non-nil when VALUE is a lowercase hexadecimal digest of LENGTH."
  (and (stringp value)
       (= (length value) length)
       (string-match-p (format "\\`[0-9a-f]\\{%d\\}\\'" length) value)))

(defun qq-gateway-resource--timestamp-p (value)
  "Return non-nil when VALUE is a non-negative integer timestamp."
  (and (integerp value) (<= 0 value)))

(defun qq-gateway-resource--validate-digests (digests)
  "Validate and return staged-resource DIGESTS or nil."
  (when digests
    (unless (and (qq-gateway--exact-object-keys-p
                  digests '(sha256 sha1 md5))
                 (qq-gateway-resource--hex-digest-p
                  (alist-get 'sha256 digests) 64)
                 (qq-gateway-resource--hex-digest-p
                  (alist-get 'sha1 digests) 40)
                 (qq-gateway-resource--hex-digest-p
                  (alist-get 'md5 digests) 32))
      (error "qq: Gateway resource digests are malformed")))
  digests)

(defun qq-gateway-resource--validate-problem (problem)
  "Validate and return staged-resource PROBLEM or nil."
  (when problem
    (unless (and (qq-gateway--exact-object-keys-p problem '(code message))
                 (qq-gateway--non-empty-string-p (alist-get 'code problem))
                 (qq-gateway--non-empty-string-p (alist-get 'message problem)))
      (error "qq: Gateway resource problem is malformed")))
  problem)

(defun qq-gateway-resource--safe-name-p (value)
  "Return non-nil when VALUE is one safe staged-resource display basename."
  (and (qq-gateway--non-empty-string-p value)
       (<= (string-bytes value) 255)
       (not (member value '("." "..")))
       (not (string-match-p "[/\\[:cntrl:]]" value))))

(defun qq-gateway-resource--validate-snapshot (snapshot)
  "Validate and copy one closed staged-resource SNAPSHOT."
  (unless (qq-gateway--exact-object-keys-p
           snapshot
           '(resource_id phase suggested_name size media_type digests
             created_at updated_at expires_at error))
    (error "qq: Gateway resource snapshot has invalid fields"))
  (let ((resource-id (alist-get 'resource_id snapshot))
        (phase (alist-get 'phase snapshot))
        (name (alist-get 'suggested_name snapshot))
        (size (alist-get 'size snapshot))
        (media-type (alist-get 'media_type snapshot))
        (digests (alist-get 'digests snapshot))
        (created-at (alist-get 'created_at snapshot))
        (updated-at (alist-get 'updated_at snapshot))
        (expires-at (alist-get 'expires_at snapshot))
        (problem (alist-get 'error snapshot)))
    (unless (qq-gateway-resource--opaque-id-p resource-id)
      (error "qq: Gateway resource_id must be an opaque res- identity"))
    (unless (member phase qq-gateway-resource--phases)
      (error "qq: Gateway resource has unknown phase"))
    (unless (qq-gateway-resource--safe-name-p name)
      (error "qq: Gateway resource suggested_name is unsafe"))
    (unless (qq-gateway--canonical-decimal-p size t)
      (error "qq: Gateway resource size must be exact decimal text"))
    (unless (or (null media-type)
                (qq-gateway--non-empty-string-p media-type))
      (error "qq: Gateway resource media_type must be string or null"))
    (qq-gateway-resource--validate-digests digests)
    (qq-gateway-resource--validate-problem problem)
    (unless (and (qq-gateway-resource--timestamp-p created-at)
                 (qq-gateway-resource--timestamp-p updated-at)
                 (<= created-at updated-at)
                 (or (null expires-at)
                     (and (qq-gateway-resource--timestamp-p expires-at)
                          (<= created-at expires-at))))
      (error "qq: Gateway resource timestamps are malformed"))
    (pcase phase
      ("staging"
       (when (or media-type digests problem)
         (error "qq: Gateway staging resource carries terminal metadata")))
      ("ready"
       (unless (and digests (null problem))
         (error "qq: Gateway ready resource lacks digests or carries error")))
      ("failed"
       (unless (and problem (null media-type) (null digests))
         (error "qq: Gateway failed resource has contradictory metadata")))
      ("released"
       (when problem
         (error "qq: Gateway released resource must not carry an error"))))
    (copy-tree snapshot)))

(defun qq-gateway-resource (resource-id)
  "Return a copy of staged RESOURCE-ID, or nil."
  (copy-tree (and resource-id
                  (gethash resource-id qq-gateway-resource--resources))))

(defun qq-gateway-resources ()
  "Return copied staged-resource snapshots in authoritative order."
  (delq nil (mapcar #'qq-gateway-resource qq-gateway-resource--order)))

(defun qq-gateway-resource--clear (reason)
  "Clear the projected registry for REASON."
  (let ((changed (or qq-gateway-resource--order
                     (> (hash-table-count qq-gateway-resource--resources) 0))))
    (setq qq-gateway-resource--resources (make-hash-table :test #'equal)
          qq-gateway-resource--order nil
          qq-gateway-resource--resync-request-id nil)
    (when changed
      (qq-gateway--run-hook 'qq-gateway-resource-changed-hook reason nil))))

(defun qq-gateway-resource--replace (snapshots reason)
  "Atomically replace resources with SNAPSHOTS for REASON."
  (unless (listp snapshots)
    (error "qq: Gateway resource list must be an array"))
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (raw snapshots)
      (let* ((snapshot (qq-gateway-resource--validate-snapshot raw))
             (resource-id (alist-get 'resource_id snapshot)))
        (when (gethash resource-id next)
          (error "qq: Gateway resource list duplicates %s" resource-id))
        (puthash resource-id snapshot next)
        (push resource-id order)))
    (setq qq-gateway-resource--resources next
          qq-gateway-resource--order (nreverse order))
    (qq-gateway--run-hook 'qq-gateway-resource-changed-hook reason nil)
    (qq-gateway-resources)))

(defun qq-gateway-resource--phase-rank (phase)
  "Return monotonic lifecycle rank for resource PHASE."
  (pcase phase
    ("staging" 0)
    ((or "ready" "failed") 1)
    ("released" 2)
    (_ -1)))

(defun qq-gateway-resource--same-identity-p (left right)
  "Return non-nil when LEFT and RIGHT describe the same immutable resource."
  (cl-every (lambda (key)
              (equal (alist-get key left) (alist-get key right)))
            '(resource_id suggested_name size created_at)))

(defun qq-gateway-resource--upsert (raw-snapshot reason)
  "Merge RAW-SNAPSHOT for REASON without regressing its lifecycle."
  (let* ((snapshot (qq-gateway-resource--validate-snapshot raw-snapshot))
         (resource-id (alist-get 'resource_id snapshot))
         (existing (gethash resource-id qq-gateway-resource--resources)))
    (when (and existing
               (not (qq-gateway-resource--same-identity-p existing snapshot)))
      (error "qq: Gateway resource identity changed for %s" resource-id))
    (let ((incoming-rank (qq-gateway-resource--phase-rank
                          (alist-get 'phase snapshot)))
          (existing-rank (and existing
                              (qq-gateway-resource--phase-rank
                               (alist-get 'phase existing)))))
      (cond
       ((null existing)
        (setq qq-gateway-resource--order
              (append qq-gateway-resource--order (list resource-id))))
       ((< incoming-rank existing-rank)
        (setq snapshot existing))
       ((and (= incoming-rank existing-rank)
             (not (equal (alist-get 'phase snapshot)
                         (alist-get 'phase existing))))
        (error "qq: Gateway resource changed between terminal phases"))
       ((< (alist-get 'updated_at snapshot)
           (alist-get 'updated_at existing))
        (setq snapshot existing))
       ((and (= incoming-rank existing-rank)
             (not (equal snapshot existing)))
        (error "qq: Gateway resource terminal snapshot changed"))
       ((and (> incoming-rank existing-rank)
             (not (or (equal (alist-get 'phase snapshot) "released")
                      (and (equal (alist-get 'phase existing) "staging")
                           (member (alist-get 'phase snapshot)
                                   '("ready" "failed"))))))
        (error "qq: Gateway resource lifecycle transition is invalid")))
      (unless (eq snapshot existing)
        (puthash resource-id snapshot qq-gateway-resource--resources)
        (qq-gateway--run-hook
         'qq-gateway-resource-changed-hook reason resource-id))
      (copy-tree snapshot))))

(defun qq-gateway-resource--remove (resource-id reason)
  "Remove opaque RESOURCE-ID from the projection for REASON."
  (unless (qq-gateway-resource--opaque-id-p resource-id)
    (error "qq: Gateway resource.removed identity is malformed"))
  (when (gethash resource-id qq-gateway-resource--resources)
    (remhash resource-id qq-gateway-resource--resources)
    (setq qq-gateway-resource--order
          (delete resource-id qq-gateway-resource--order))
    (qq-gateway--run-hook
     'qq-gateway-resource-changed-hook reason resource-id)
    t))

(defun qq-gateway-resource--validate-single-result (result context)
  "Validate and return the resource carried by RESULT in CONTEXT."
  (unless (qq-gateway--exact-object-keys-p result '(resource))
    (error "qq: Gateway %s result has invalid fields" context))
  (qq-gateway-resource--validate-snapshot (alist-get 'resource result)))

(defun qq-gateway-resource--validate-list-result (result)
  "Validate and return resources carried by resource.list RESULT."
  (unless (qq-gateway--exact-object-keys-p result '(resources))
    (error "qq: Gateway resource.list result has invalid fields"))
  (let ((resources (alist-get 'resources result nil nil #'eq)))
    (unless (listp resources)
      (error "qq: Gateway resource.list resources must be an array"))
    resources))

(defun qq-gateway-resource-refresh (&optional callback errback reason)
  "Fetch the authoritative staged-resource registry.

CALLBACK receives copied snapshots.  ERRBACK follows the Gateway transport
error convention.  REASON defaults to `resync'."
  (qq-gateway--send
   "resource.list" nil
   (lambda (result)
     (condition-case error-data
         (let ((resources
                (qq-gateway-resource--replace
                 (qq-gateway-resource--validate-list-result result)
                 (or reason 'resync))))
           (setq qq-gateway-resource--resync-request-id nil)
           (qq-gateway--invoke callback resources))
       (error
        (setq qq-gateway-resource--resync-request-id nil)
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   (lambda (body failure)
     (setq qq-gateway-resource--resync-request-id nil)
     (qq-gateway--invoke errback body failure))))

(defun qq-gateway-resource-stage-local
    (path &optional suggested-name expected-sha256 callback errback)
  "Copy local PATH into Gateway-owned immutable resource storage.

SUGGESTED-NAME is an optional safe display basename.  EXPECTED-SHA256, when
non-nil, must be a lowercase hexadecimal digest.  CALLBACK receives the
validated staging snapshot; ERRBACK follows the Gateway convention."
  (let* ((path (expand-file-name path))
         (attributes (file-attributes path 'string)))
    (unless (and attributes (file-regular-p path))
      (user-error "qq: Resource source is not a regular file: %s" path))
    (when (and suggested-name
               (not (qq-gateway-resource--safe-name-p suggested-name)))
      (user-error "qq: Resource suggested name must be a safe basename"))
    (when (and expected-sha256
               (not (qq-gateway-resource--hex-digest-p expected-sha256 64)))
      (user-error "qq: Expected SHA-256 must be lowercase hexadecimal"))
    (qq-gateway--send
     "resource.stage_local"
     `((path . ,path)
       (suggested_name . ,suggested-name)
       (expected
        . ((size . ,(number-to-string (file-attribute-size attributes)))
           (sha256 . ,expected-sha256))))
     (lambda (result)
       (condition-case error-data
           (let ((snapshot
                  (qq-gateway-resource--validate-single-result
                   result "resource.stage_local")))
             (unless (equal (alist-get 'phase snapshot) "staging")
               (error "qq: Gateway stage response is not staging"))
             (setq snapshot
                   (qq-gateway-resource--upsert snapshot 'stage-response))
             (qq-gateway--invoke callback snapshot))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

(defun qq-gateway-resource-status
    (resource-id &optional callback errback)
  "Fetch RESOURCE-ID and merge it into the local projection."
  (unless (qq-gateway-resource--opaque-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-gateway--send
   "resource.status" `((resource_id . ,resource-id))
   (lambda (result)
     (condition-case error-data
         (let ((snapshot
                (qq-gateway-resource--validate-single-result
                 result "resource.status")))
           (unless (equal resource-id (alist-get 'resource_id snapshot))
             (error "qq: Gateway resource.status identity contradicts request"))
           (setq snapshot (qq-gateway-resource--upsert snapshot 'status))
           (qq-gateway--invoke callback snapshot))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

(defun qq-gateway-resource-release
    (resource-id &optional callback errback)
  "Idempotently release staged RESOURCE-ID."
  (unless (qq-gateway-resource--opaque-id-p resource-id)
    (user-error "qq: Resource ID must be an opaque res- identity"))
  (qq-gateway--send
   "resource.release" `((resource_id . ,resource-id))
   (lambda (result)
     (condition-case error-data
         (progn
           (unless (and (qq-gateway--exact-object-keys-p
                         result '(resource_id released))
                        (equal (alist-get 'resource_id result) resource-id)
                        (eq (alist-get 'released result) t))
             (error "qq: Gateway resource.release receipt is malformed"))
           (qq-gateway--invoke callback (copy-tree result)))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

(defun qq-gateway-resource--request-resync (reason)
  "Request one authoritative resource refresh for REASON."
  (unless qq-gateway-resource--resync-request-id
    (let ((marker (list 'resource-resync)))
      (setq qq-gateway-resource--resync-request-id marker)
      (let ((request-id
             (qq-gateway-resource-refresh
              nil
              (lambda (_body failure)
                (message "qq: Gateway resource resync failed: %s" failure))
              reason)))
        (when (eq qq-gateway-resource--resync-request-id marker)
          (setq qq-gateway-resource--resync-request-id request-id))))))

(defun qq-gateway-resource--handle-event (event data)
  "Project native Gateway resource EVENT with DATA."
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
                          qq-gateway-resource--gateway-instance-id)
             (qq-gateway-resource--clear 'gateway-changed))
           (setq qq-gateway-resource--gateway-instance-id instance-id)
           (if (qq-gateway--method-available-p "resource.list")
               (qq-gateway-resource--request-resync 'ready)
             (qq-gateway-resource--clear 'capability-unavailable))))
        ("resource.changed"
         (unless (qq-gateway--exact-object-keys-p data '(resource))
           (error "qq: Gateway resource.changed data has invalid fields"))
         (qq-gateway-resource--upsert
          (alist-get 'resource data) 'changed))
        ("resource.removed"
         (unless (qq-gateway--exact-object-keys-p data '(resource_id))
           (error "qq: Gateway resource.removed data has invalid fields"))
         (qq-gateway-resource--remove
          (alist-get 'resource_id data) 'removed)))
    (error
     (qq-gateway-transport--protocol-violation
      "Malformed %s event: %s" event
      (error-message-string error-data)))))

(defun qq-gateway-resource--handle-protocol-error (body)
  "Resynchronize after unsolicited resource stream error BODY."
  (when (equal (alist-get 'code body) "resource_event_stream_lagged")
    (qq-gateway--run-hook
     'qq-gateway-resource-desync-hook (copy-tree body))
    (qq-gateway-resource--request-resync 'resync)))

(add-hook 'qq-gateway-transport-event-hook
          #'qq-gateway-resource--handle-event)
(add-hook 'qq-gateway-transport-protocol-error-hook
          #'qq-gateway-resource--handle-protocol-error)

(provide 'qq-gateway-resource)

;;; qq-gateway-resource.el ends here
