;;; qq-account.el --- QQ account lifecycle client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Client-side projection of nt-gateway's long-lived multi-account registry.
;; The selected account is local Emacs UI state; all managed accounts continue
;; running independently when the websocket or Emacs process disconnects.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'qq-rpc)
(require 'qq-server)
(require 'qq-protocol)

(defvar qq-account-registry-changed-hook nil
  "Hook called with REASON and ACCOUNT-ID after the local registry changes.

REASON is one of `ready', `resync', `changed', `response', or `removed'.
ACCOUNT-ID is nil for an authoritative full-registry replacement.")

(defvar qq-account-selection-changed-hook nil
  "Hook called with OLD-ACCOUNT-ID and NEW-ACCOUNT-ID after selection changes.")

(defvar qq-account-registry-ready-hook nil
  "Hook called with GATEWAY-INSTANCE-ID after `gateway.ready' is projected.

The account registry has already been authoritatively replaced when handlers
run.  Consumers may use this typed synchronization boundary to refresh other
Gateway-owned registries without inspecting raw transport events.")

(defvar qq-account-desync-hook nil
  "Hook called with a protocol error body after transient events were lost.")

(defvar qq-account-projection-resync-hook nil
  "Hook called with PROJECTION and BODY after projection events were lost.

PROJECTION is one of \"resources\", \"attachments\", or \"remote_media\".
The account projection is resynchronized by this registry itself and is not
dispatched through this hook.  BODY is the owned event or error body that
reported the loss.  Consumers filter on PROJECTION and refresh their own
Gateway-owned registry from an authoritative snapshot.")

(defconst qq-account--foreign-projections
  '("resources" "attachments" "remote_media")
  "Runtime projections owned by registries other than the account registry.")

(defvar qq-account--accounts (make-hash-table :test #'equal))
(defvar qq-account--account-order nil)
(defvar qq-account--current-account-id nil)
(defvar qq-account--gateway-instance-id nil)
(defvar qq-account--refresh-owner nil
  "Identity of the newest authoritative account registry refresh.")
(defvar qq-account--resync-request-id nil
  "Identity of the in-flight automatic account registry resync.")

(defun qq-account--run-hook (hook &rest arguments)
  "Run each function on HOOK with ARGUMENTS, isolating consumer errors."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function (mapcar #'qq-server-value-copy hook-arguments))
       (error
        (message "qq: Gateway client hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-account--invoke (callback &rest arguments)
  "Invoke CALLBACK with owned copies of ARGUMENTS."
  (apply #'qq-rpc-invoke callback arguments))

(defun qq-account--client-error (errback code format-string &rest arguments)
  "Invoke ERRBACK with client CODE.

FORMAT-STRING and ARGUMENTS produce the human-readable failure text."
  (apply #'qq-rpc-client-error
         errback code format-string arguments))

(defun qq-account--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (qq-server-wire-exact-object-keys-p object keys))

(defun qq-account--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun qq-account--canonical-decimal-p (value &optional allow-zero)
  "Return non-nil when VALUE is a canonical decimal string.

When ALLOW-ZERO is non-nil, the exact string `0' is accepted."
  (and (stringp value)
       (if allow-zero
           (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)
         (string-match-p "\\`[1-9][0-9]*\\'" value))))

(defconst qq-account--max-uint64-decimal
  "18446744073709551615"
  "Largest exact unsigned 64-bit integer accepted on the Gateway wire.")

(defun qq-account--decimal-less-p (left right)
  "Return non-nil when canonical decimal LEFT is less than RIGHT.

The comparison never coerces either protocol value to an Emacs number."
  (or (< (length left) (length right))
      (and (= (length left) (length right))
           (string-lessp left right))))

(defun qq-account--uint64-decimal-p (value &optional allow-zero)
  "Return non-nil when VALUE is a canonical uint64 decimal string.

VALUE is compared as decimal text and is never coerced to an Emacs number.
By default zero is rejected; when ALLOW-ZERO is non-nil, only the exact
canonical string `0' additionally qualifies."
  (and (qq-account--canonical-decimal-p value allow-zero)
       (not (qq-account--decimal-less-p
             qq-account--max-uint64-decimal value))))

(defun qq-account-get (account-id)
  "Return a copy of managed ACCOUNT-ID's snapshot, or nil."
  (qq-server-value-copy
   (and account-id (gethash account-id qq-account--accounts))))

(defun qq-account-list ()
  "Return managed account snapshots in authoritative Gateway order."
  (delq nil
        (mapcar (lambda (account-id)
                  (qq-account-get account-id))
                qq-account--account-order)))

(defun qq-account-current-id ()
  "Return the account selected by this Emacs client, or nil."
  (and qq-account--current-account-id
       (copy-sequence qq-account--current-account-id)))

(defun qq-account-current ()
  "Return a copy of this Emacs client's selected account snapshot."
  (qq-account-get qq-account--current-account-id))

(defun qq-account--set-current-account (account-id)
  "Select ACCOUNT-ID locally and publish an exact selection change."
  (when account-id
    (unless (gethash account-id qq-account--accounts)
      (user-error "qq: QQ account does not exist: %s" account-id)))
  (unless (equal account-id qq-account--current-account-id)
    (let ((old qq-account--current-account-id))
      (setq qq-account--current-account-id
            (and account-id (copy-sequence account-id)))
      (qq-account--run-hook
       'qq-account-selection-changed-hook old account-id)))
  account-id)

(defun qq-account--account-display-name (snapshot)
  "Return a completion label for account SNAPSHOT."
  (let ((label (alist-get 'label snapshot))
        (uin (alist-get 'uin snapshot))
        (account-id (alist-get 'account_id snapshot))
        (phase (alist-get 'phase snapshot)))
    (format "%s%s — %s [%s]"
            (or label uin "Unbound account")
            (if (and label uin) (format " (%s)" uin) "")
            phase account-id)))

(defun qq-account--read-account-id (&optional prompt)
  "Read a managed account ID using PROMPT."
  (let ((accounts (qq-account-list)))
    (unless accounts
      (user-error "qq: Gateway has no managed accounts"))
    (let* ((choices
            (mapcar (lambda (snapshot)
                      (cons (qq-account--account-display-name snapshot)
                            (alist-get 'account_id snapshot)))
                    accounts))
           (default
            (when-let* ((current (qq-account-current)))
              (qq-account--account-display-name current))))
      (cdr (assoc
            (completing-read (or prompt "QQ account: ")
                             choices nil t nil nil default)
            choices)))))

;;;###autoload
(defun qq-account-select (account-id)
  "Select managed ACCOUNT-ID for this Emacs client's chat projection."
  (interactive (list (qq-account--read-account-id "Select QQ account: ")))
  (qq-account--set-current-account account-id)
  (when (called-interactively-p 'interactive)
    (message "qq: selected QQ account %s" account-id))
  account-id)

(defun qq-account--maybe-select-single-account ()
  "Select the sole managed account when no explicit selection remains."
  (unless (and qq-account--current-account-id
               (gethash qq-account--current-account-id qq-account--accounts))
    (qq-account--set-current-account
     (and (= (hash-table-count qq-account--accounts) 1)
          (car qq-account--account-order)))))

(defun qq-account--replace-accounts (snapshots reason instance-id)
  "Replace the registry with SNAPSHOTS for REASON and INSTANCE-ID."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((account-id (alist-get 'account_id snapshot)))
        (when (gethash account-id next)
          (error "qq: Duplicate QQ account ID %s" account-id))
        (puthash account-id snapshot next)
        (push account-id order)))
    (setq qq-account--accounts next
          qq-account--account-order (nreverse order)
          qq-account--gateway-instance-id
          (and instance-id (copy-sequence instance-id)))
    (qq-account--maybe-select-single-account)
    (qq-account--run-hook 'qq-account-registry-changed-hook reason nil)
    (qq-account-list)))

(defun qq-account--upsert-account (raw-snapshot reason)
  "Merge domain account RAW-SNAPSHOT for REASON."
  (let* ((snapshot raw-snapshot)
         (account-id (alist-get 'account_id snapshot))
         (existing (gethash account-id qq-account--accounts)))
    (unless existing
      (setq qq-account--account-order
            (append qq-account--account-order (list account-id))))
    (unless (equal existing snapshot)
      (puthash account-id snapshot qq-account--accounts)
      (qq-account--maybe-select-single-account)
      (qq-account--run-hook
       'qq-account-registry-changed-hook reason account-id))
    snapshot))

(defun qq-account--remove-account (account-id reason)
  "Remove ACCOUNT-ID locally for REASON and return its old snapshot."
  (let ((old (gethash account-id qq-account--accounts)))
    (when old
      (remhash account-id qq-account--accounts)
      (setq qq-account--account-order
            (delete account-id qq-account--account-order))
      (qq-account--maybe-select-single-account)
      (qq-account--run-hook
       'qq-account-registry-changed-hook reason account-id))
    old))

(defun qq-account-refresh-accounts (&optional callback errback reason)
  "Fetch the authoritative managed-account registry.

CALLBACK receives the copied account list.  ERRBACK follows the transport
error convention.  REASON defaults to `resync'."
  (qq-rpc-latest-call
   'qq-account--refresh-owner "account.list" nil
   :projector
   (lambda (result)
     (qq-account--replace-accounts
      (alist-get 'accounts result) (or reason 'resync)
      (qq-server-gateway-instance-id)))
   :callback callback
   :errback errback))

(defun qq-account--account-command
    (method account-id callback errback &optional remove-p)
  "Send account METHOD for ACCOUNT-ID and update the local registry.

CALLBACK receives the domain account snapshot and ERRBACK receives a protocol
error body plus reason.

When REMOVE-P is non-nil, remove the returned snapshot instead of merging it."
  (unless (qq-account--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-rpc-call
   method `((account_id . ,account-id))
   :projector
   (lambda (snapshot)
     (if remove-p
         (qq-account--remove-account account-id 'removed)
       (qq-account--upsert-account snapshot 'response))
     snapshot)
   :callback callback
   :errback errback))

(defun qq-account--interactive-success (snapshot)
  "Report interactive success represented by account SNAPSHOT."
  (message "qq: account %s is %s"
           (alist-get 'account_id snapshot)
           (alist-get 'phase snapshot)))

(defun qq-account--interactive-error (_body reason)
  "Report interactive Gateway failure REASON."
  (message "qq: Gateway request failed: %s" reason))

(defun qq-device--interactive-reset-success (_receipt)
  "Report successful explicit Device Profile reset."
  (message "qq: default QQ Device Profile was reset; saved EasyLogin records are invalid"))

;;;###autoload
(defun qq-device-reset (&optional callback errback)
  "Explicitly reset the shared default QQ Device Profile.

Every Managed Account must first have no Native Session.  Success rotates the
MachineGuid and invalidates every Login Record bound to the previous device.
CALLBACK receives t; ERRBACK receives a failure body and reason."
  (interactive
   (progn
     (unless
         (yes-or-no-p
          "Reset the shared QQ device and invalidate all quick logins? ")
       (user-error "qq: Device Profile reset cancelled"))
     (list #'qq-device--interactive-reset-success
           #'qq-account--interactive-error)))
  (when-let* ((active
               (seq-find
                (lambda (account)
                  (not (member (alist-get 'phase account)
                               '("stopped" "logged_out" "failed"))))
                (qq-account-list))))
    (user-error "qq: Stop account %s before resetting the shared device"
                (alist-get 'account_id active)))
  (qq-rpc-call
   "device.reset" nil
   :projector
   (lambda (result)
     (unless (qq-account--exact-object-keys-p result nil)
       (error "qq: Gateway returned an invalid Device Reset receipt"))
     t)
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-create (label &optional callback errback)
  "Create a persistent QQ account slot with optional LABEL.

CALLBACK receives its snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (let ((value (string-trim (read-string "Account label (optional): "))))
           (and (not (string-empty-p value)) value))
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (when label
    (unless (and (qq-account--non-empty-string-p label)
                 (equal label (string-trim label))
                 (<= (length label) 128))
      (user-error "qq: Account label must be trimmed and at most 128 characters")))
  (qq-rpc-call
   "account.create" (if label `((label . ,label)) '((label)))
   :projector
   (lambda (snapshot)
     (setq snapshot (qq-account--upsert-account snapshot 'response))
     ;; Creating a slot is an explicit local choice, unlike discovering
     ;; several pre-existing slots in gateway.ready.
     (qq-account--set-current-account (alist-get 'account_id snapshot))
     snapshot)
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-status (account-id &optional callback errback)
  "Fetch managed ACCOUNT-ID's current snapshot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-account--read-account-id)
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (qq-account--account-command
   "account.status" account-id callback errback))

;;;###autoload
(defun qq-account-set-presence
    (account-id presence &optional callback errback)
  "Set PRESENCE for managed ACCOUNT-ID.

CALLBACK receives an acknowledgement carrying the account ID and
requested presence.  This command does not change the account lifecycle phase
or store presence in the local account snapshot."
  (unless (qq-account--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (unless (qq-account-get account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-rpc-call
   "account.set_presence"
   `((account_id . ,account-id) (presence . ,presence))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-start (account-id &optional callback errback)
  "Start ACCOUNT-ID's Native Session.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-account--read-account-id "Start account: ")
         #'qq-account--interactive-success
         #'qq-account--interactive-error))
  (qq-account--account-command
   "account.start" account-id callback errback))

(defun qq-account--login-command
    (method account-id params secret callback errback)
  "Send login METHOD for ACCOUNT-ID with PARAMS and copied SECRET.

PARAMS is called with the copied secret.  CALLBACK receives the account
snapshot; ERRBACK receives a failure body and reason."
  (unless (qq-account--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (let ((secret-copy (copy-sequence secret)))
    (unwind-protect
        (qq-rpc-call
         method (append `((account_id . ,account-id))
                        (funcall params secret-copy))
         :projector
         (lambda (snapshot)
           (qq-account--upsert-account snapshot 'response))
         :callback callback
         :errback errback)
      (clear-string secret-copy))))

(defun qq-account--project-quick-login-accounts (result)
  "Validate RESULT and return its ordered EasyLogin account metadata."
  (unless (and (qq-account--exact-object-keys-p result '(accounts))
               (listp (alist-get 'accounts result)))
    (error "qq: Gateway returned an invalid quick-login account list"))
  (let ((seen-uins (make-hash-table :test #'equal))
        accounts)
    (dolist (account (alist-get 'accounts result))
      (unless
          (and
           (qq-account--exact-object-keys-p
            account '(uin uid generated_at_unix))
           (qq-account--uint64-decimal-p (alist-get 'uin account))
           (qq-account--non-empty-string-p (alist-get 'uid account))
           (integerp (alist-get 'generated_at_unix account))
           (>= (alist-get 'generated_at_unix account) 0))
        (error "qq: Gateway returned invalid quick-login account metadata"))
      (let ((uin (alist-get 'uin account)))
        (when (gethash uin seen-uins)
          (error "qq: Gateway returned duplicate quick-login UIN %s" uin))
        (puthash uin t seen-uins))
      (push (qq-server-value-copy account) accounts))
    (nreverse accounts)))

;;;###autoload
(defun qq-account-login-list (&optional callback errback)
  "List identities with reusable native EasyLogin credentials.

CALLBACK receives ordered non-secret account metadata.  ERRBACK receives a
failure body and reason.  The Gateway advertises this operation only when its
encrypted credential store is available."
  (interactive
   (list (lambda (accounts)
           (message "qq: %d quick-login account%s available"
                    (length accounts)
                    (if (= (length accounts) 1) "" "s")))
         #'qq-account--interactive-error))
  (qq-rpc-call
   "account.login.list" nil
   :projector #'qq-account--project-quick-login-accounts
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-login-password
    (account-id uin password &optional qimei callback errback)
  "Begin password login for ACCOUNT-ID using exact UIN and PASSWORD.

QIMEI is optional.  The caller's PASSWORD string is not retained or mutated.
CALLBACK receives the account snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let* ((account-id (qq-account--read-account-id "Login account: "))
          (snapshot (qq-account-get account-id)))
     (list account-id
           (read-string "QQ UIN: " (alist-get 'uin snapshot))
           (read-passwd "QQ password: ") nil
           #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (unless (qq-account--uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (qq-account--non-empty-string-p password)
    (user-error "qq: Password must not be empty"))
  (unless (or (null qimei) (qq-account--non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (qq-account--login-command
   "account.login.password" account-id
   (lambda (secret)
     `((uin . ,uin) (password . ,secret)
       ,@(when qimei `((qimei . ,qimei)))))
   password callback errback))

;;;###autoload
(defun qq-account-login-quick
    (account-id uin &optional qimei callback errback)
  "Begin native EasyLogin for ACCOUNT-ID using the stored identity for UIN.

QIMEI is optional.  CALLBACK receives the account snapshot; ERRBACK receives
a failure body and reason.  No reusable credential material crosses the
Gateway protocol."
  (unless (qq-account--uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (or (null qimei) (qq-account--non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (unless (qq-account--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-rpc-call
   "account.login.quick"
   `((account_id . ,account-id)
     (uin . ,uin)
     ,@(when qimei `((qimei . ,qimei))))
   :projector
   (lambda (snapshot)
     (qq-account--upsert-account snapshot 'response))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-account-login-captcha
    (account-id challenge-id ticket rand-str sid &optional callback errback)
  "Continue ACCOUNT-ID's captcha CHALLENGE-ID.

TICKET, RAND-STR, and SID carry the proof.  CALLBACK receives the account
snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let* ((account-id (qq-account--read-account-id "Captcha account: "))
          (challenge (alist-get 'challenge (qq-account-get account-id))))
     (unless (equal (alist-get 'kind challenge) "captcha")
       (user-error "qq: Selected account has no captcha challenge"))
     (list account-id (alist-get 'challenge_id challenge)
           (read-passwd "Captcha ticket: ")
           (read-string "Captcha randStr: ")
           (read-string "Captcha sid: " (alist-get 'sid challenge))
           #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (dolist (value (list challenge-id ticket rand-str sid))
    (unless (qq-account--non-empty-string-p value)
      (user-error "qq: Captcha proof fields must be non-empty strings")))
  (qq-account--login-command
   "account.login.captcha" account-id
   (lambda (secret)
     `((challenge_id . ,challenge-id) (ticket . ,secret)
       (rand_str . ,rand-str) (sid . ,sid)))
   ticket callback errback))

;;;###autoload
(defun qq-account-stop (account-id &optional callback errback)
  "Stop ACCOUNT-ID's Native Session without logging out of QQ.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Stop Native Session: ")))
     (unless (yes-or-no-p
              (format "Stop Native Session %s without QQ logout? " account-id))
       (user-error "qq: Account stop cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.stop" account-id callback errback))

;;;###autoload
(defun qq-account-logout (account-id &optional callback errback)
  "Explicitly log ACCOUNT-ID out of QQ while retaining its managed slot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Log out account: ")))
     (unless (yes-or-no-p (format "Log QQ account %s out of QQ? " account-id))
       (user-error "qq: Account logout cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.logout" account-id callback errback))

;;;###autoload
(defun qq-account-remove (account-id &optional callback errback)
  "Delete stopped ACCOUNT-ID and its Gateway-managed local material.

CALLBACK receives the removed snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let ((account-id (qq-account--read-account-id "Remove account slot: ")))
     (unless (yes-or-no-p
              (format "Permanently remove QQ account slot %s? " account-id))
       (user-error "qq: Account removal cancelled"))
     (list account-id #'qq-account--interactive-success
           #'qq-account--interactive-error)))
  (qq-account--account-command
   "account.remove" account-id callback errback t))

(defun qq-account--handle-event (event data)
  "Project native service EVENT with DATA into the account registry."
  (pcase event
    ("gateway.ready"
     (let ((instance-id (alist-get 'gateway_instance_id data))
           (accounts (alist-get 'accounts data)))
       (let ((resync-marker qq-account--resync-request-id))
         (qq-rpc-cancel-latest
          'qq-account--refresh-owner "superseded_request"
          "Gateway account refresh was superseded by gateway.ready")
         (when (eq resync-marker qq-account--resync-request-id)
           (setq qq-account--resync-request-id nil)))
       (let ((projected
              (qq-account--replace-accounts accounts 'ready instance-id)))
         (qq-account--run-hook 'qq-account-registry-ready-hook instance-id)
         projected)))
    ("account.changed"
     (qq-account--upsert-account data 'changed))
    ("account.removed"
     (qq-account--remove-account
      (alist-get 'account_id data) 'removed))
    (_ (error "qq: Unowned Gateway account event %s" event))))

(defun qq-account--resync-accounts (body)
  "Resynchronize the account registry after transient event loss BODY."
  (qq-account--run-hook 'qq-account-desync-hook body)
  (qq-rpc-request-single-flight
   'qq-account--resync-request-id 'account-resync
   (lambda (success failure)
     (qq-account-refresh-accounts success failure 'resync))
   "QQ account"))

(defun qq-account--handle-protocol-error (body)
  "Handle unsolicited Gateway protocol error BODY.

A websocket-level event stream lag may have dropped events for every runtime
projection, so all projection owners resynchronize."
  (when (equal (alist-get 'code body) "event_stream_lagged")
    (qq-account--resync-accounts body)
    (dolist (projection qq-account--foreign-projections)
      (qq-account--run-hook
       'qq-account-projection-resync-hook projection body))))

(defun qq-account--handle-resync-required (_event data)
  "Resynchronize one runtime projection after lossy DATA."
  (let ((projection (alist-get 'projection data)))
    (unless (and (qq-account--exact-object-keys-p data '(projection skipped))
                 (qq-account--non-empty-string-p projection)
                 (qq-account--non-empty-string-p (alist-get 'skipped data)))
      (error "qq: Malformed runtime.resync_required event"))
    (cond
     ((equal projection "accounts")
      (qq-account--resync-accounts data))
     ((member projection qq-account--foreign-projections)
      (qq-account--run-hook
       'qq-account-projection-resync-hook projection data))
     (t
      (error "qq: Unowned runtime projection %s requires resync"
             projection)))))

(dolist (event '("gateway.ready" "account.changed" "account.removed"))
  (qq-rpc-register-event event #'qq-account--handle-event))
(qq-rpc-register-event
 "runtime.resync_required" #'qq-account--handle-resync-required)
(qq-rpc-register-error
 "event_stream_lagged" #'qq-account--handle-protocol-error)

(provide 'qq-account)

;;; qq-account.el ends here
