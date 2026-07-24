;;; qq-gateway.el --- Native QQ account lifecycle client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Client-side projection of nt-gateway's long-lived multi-account registry.
;; The selected account is local Emacs UI state; all managed accounts continue
;; running independently when the websocket or Emacs process disconnects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway-dispatch)
(require 'qq-gateway-rpc)
(require 'qq-gateway-transport)
(require 'qq-gateway-wire)
(require 'qq-protocol)

(defvar qq-gateway-accounts-changed-hook nil
  "Hook called with REASON and ACCOUNT-ID after the local registry changes.

REASON is one of `ready', `resync', `changed', `response', or `removed'.
ACCOUNT-ID is nil for an authoritative full-registry replacement.")

(defvar qq-gateway-current-account-changed-hook nil
  "Hook called with OLD-ACCOUNT-ID and NEW-ACCOUNT-ID after selection changes.")

(defvar qq-gateway-ready-hook nil
  "Hook called with GATEWAY-INSTANCE-ID after `gateway.ready' is projected.

The account registry has already been authoritatively replaced when handlers
run.  Consumers may use this typed synchronization boundary to refresh other
Gateway-owned registries without inspecting raw transport events.")

(defvar qq-gateway-desync-hook nil
  "Hook called with a protocol error body after transient events were lost.")

(defvar qq-gateway--accounts (make-hash-table :test #'equal))
(defvar qq-gateway--account-order nil)
(defvar qq-gateway--current-account-id nil)
(defvar qq-gateway--gateway-instance-id nil)
(defvar qq-gateway--refresh-owner nil
  "Identity of the newest authoritative account registry refresh.")
(defvar qq-gateway--resync-request-id nil
  "Identity of the in-flight automatic account registry resync.")

(cl-defstruct (qq-gateway-watch
               (:constructor qq-gateway-watch-create))
  "Cancellable local observer of a Gateway projection."
  active-p
  cancel-function)

(defun qq-gateway-watch-cancel (watch)
  "Detach active projection WATCH exactly once."
  (when (and (qq-gateway-watch-p watch)
             (qq-gateway-watch-active-p watch))
    (let ((cancel (qq-gateway-watch-cancel-function watch)))
      (setf (qq-gateway-watch-active-p watch) nil
            (qq-gateway-watch-cancel-function watch) nil)
      (when cancel
        (funcall cancel))
      t)))

(defun qq-gateway--run-hook (hook &rest arguments)
  "Run each function on HOOK with ARGUMENTS, isolating consumer errors."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function (mapcar #'qq-gateway-value-copy hook-arguments))
       (error
        (message "qq: Gateway client hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-gateway--invoke (callback &rest arguments)
  "Invoke CALLBACK with owned copies of ARGUMENTS."
  (apply #'qq-gateway-rpc-invoke callback arguments))

(defun qq-gateway--client-error (errback code format-string &rest arguments)
  "Invoke ERRBACK with client CODE.

FORMAT-STRING and ARGUMENTS produce the human-readable failure text."
  (apply #'qq-gateway-rpc-client-error
         errback code format-string arguments))

(defun qq-gateway--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (qq-gateway-wire-exact-object-keys-p object keys))

(defun qq-gateway--non-empty-string-p (value)
  "Return non-nil when VALUE is a non-empty string."
  (and (stringp value) (not (string-empty-p value))))

(defun qq-gateway--canonical-decimal-p (value &optional allow-zero)
  "Return non-nil when VALUE is a canonical decimal string.

When ALLOW-ZERO is non-nil, the exact string `0' is accepted."
  (and (stringp value)
       (if allow-zero
           (string-match-p "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" value)
         (string-match-p "\\`[1-9][0-9]*\\'" value))))

(defconst qq-gateway--max-uint64-decimal
  "18446744073709551615"
  "Largest exact unsigned 64-bit integer accepted on the Gateway wire.")

(defun qq-gateway--decimal-less-p (left right)
  "Return non-nil when canonical decimal LEFT is less than RIGHT.

The comparison never coerces either protocol value to an Emacs number."
  (or (< (length left) (length right))
      (and (= (length left) (length right))
           (string-lessp left right))))

(defun qq-gateway--uint64-decimal-p (value &optional allow-zero)
  "Return non-nil when VALUE is a canonical uint64 decimal string.

VALUE is compared as decimal text and is never coerced to an Emacs number.
By default zero is rejected; when ALLOW-ZERO is non-nil, only the exact
canonical string `0' additionally qualifies."
  (and (qq-gateway--canonical-decimal-p value allow-zero)
       (not (qq-gateway--decimal-less-p
             qq-gateway--max-uint64-decimal value))))

(defun qq-gateway-account (account-id)
  "Return a copy of managed ACCOUNT-ID's snapshot, or nil."
  (qq-gateway-value-copy
   (and account-id (gethash account-id qq-gateway--accounts))))

(defun qq-gateway-accounts ()
  "Return managed account snapshots in authoritative Gateway order."
  (delq nil
        (mapcar (lambda (account-id)
                  (qq-gateway-account account-id))
                qq-gateway--account-order)))

(defun qq-gateway-current-account-id ()
  "Return the account selected by this Emacs client, or nil."
  (and qq-gateway--current-account-id
       (copy-sequence qq-gateway--current-account-id)))

(defun qq-gateway-current-account ()
  "Return a copy of this Emacs client's selected account snapshot."
  (qq-gateway-account qq-gateway--current-account-id))

(defun qq-gateway--set-current-account (account-id)
  "Select ACCOUNT-ID locally and publish an exact selection change."
  (when account-id
    (unless (gethash account-id qq-gateway--accounts)
      (user-error "qq: QQ account does not exist: %s" account-id)))
  (unless (equal account-id qq-gateway--current-account-id)
    (let ((old qq-gateway--current-account-id))
      (setq qq-gateway--current-account-id
            (and account-id (copy-sequence account-id)))
      (qq-gateway--run-hook
       'qq-gateway-current-account-changed-hook old account-id)))
  account-id)

(defun qq-gateway--account-display-name (snapshot)
  "Return a completion label for account SNAPSHOT."
  (let ((label (alist-get 'label snapshot))
        (uin (alist-get 'uin snapshot))
        (account-id (alist-get 'account_id snapshot))
        (phase (alist-get 'phase snapshot)))
    (format "%s%s — %s [%s]"
            (or label uin "Unbound account")
            (if (and label uin) (format " (%s)" uin) "")
            phase account-id)))

(defun qq-gateway--read-account-id (&optional prompt)
  "Read a managed account ID using PROMPT."
  (let ((accounts (qq-gateway-accounts)))
    (unless accounts
      (user-error "qq: Gateway has no managed accounts"))
    (let* ((choices
            (mapcar (lambda (snapshot)
                      (cons (qq-gateway--account-display-name snapshot)
                            (alist-get 'account_id snapshot)))
                    accounts))
           (default
            (when-let* ((current (qq-gateway-current-account)))
              (qq-gateway--account-display-name current))))
      (cdr (assoc
            (completing-read (or prompt "QQ account: ")
                             choices nil t nil nil default)
            choices)))))

;;;###autoload
(defun qq-gateway-account-select (account-id)
  "Select managed ACCOUNT-ID for this Emacs client's chat projection."
  (interactive (list (qq-gateway--read-account-id "Select QQ account: ")))
  (qq-gateway--set-current-account account-id)
  (when (called-interactively-p 'interactive)
    (message "qq: selected QQ account %s" account-id))
  account-id)

(defun qq-gateway--maybe-select-single-account ()
  "Select the sole managed account when no explicit selection remains."
  (unless (and qq-gateway--current-account-id
               (gethash qq-gateway--current-account-id qq-gateway--accounts))
    (qq-gateway--set-current-account
     (and (= (hash-table-count qq-gateway--accounts) 1)
          (car qq-gateway--account-order)))))

(defun qq-gateway--replace-accounts (snapshots reason instance-id)
  "Replace the registry with SNAPSHOTS for REASON and INSTANCE-ID."
  (let ((next (make-hash-table :test #'equal))
        order)
    (dolist (snapshot snapshots)
      (let ((account-id (alist-get 'account_id snapshot)))
        (when (gethash account-id next)
          (error "qq: Duplicate QQ account ID %s" account-id))
        (puthash account-id snapshot next)
        (push account-id order)))
    (setq qq-gateway--accounts next
          qq-gateway--account-order (nreverse order)
          qq-gateway--gateway-instance-id
          (and instance-id (copy-sequence instance-id)))
    (qq-gateway--maybe-select-single-account)
    (qq-gateway--run-hook 'qq-gateway-accounts-changed-hook reason nil)
    (qq-gateway-accounts)))

(defun qq-gateway--upsert-account (raw-snapshot reason)
  "Merge domain account RAW-SNAPSHOT for REASON."
  (let* ((snapshot raw-snapshot)
         (account-id (alist-get 'account_id snapshot))
         (existing (gethash account-id qq-gateway--accounts)))
    (unless existing
      (setq qq-gateway--account-order
            (append qq-gateway--account-order (list account-id))))
    (unless (equal existing snapshot)
      (puthash account-id snapshot qq-gateway--accounts)
      (qq-gateway--maybe-select-single-account)
      (qq-gateway--run-hook
       'qq-gateway-accounts-changed-hook reason account-id))
    snapshot))

(defun qq-gateway--remove-account (account-id reason)
  "Remove ACCOUNT-ID locally for REASON and return its old snapshot."
  (let ((old (gethash account-id qq-gateway--accounts)))
    (when old
      (remhash account-id qq-gateway--accounts)
      (setq qq-gateway--account-order
            (delete account-id qq-gateway--account-order))
      (qq-gateway--maybe-select-single-account)
      (qq-gateway--run-hook
       'qq-gateway-accounts-changed-hook reason account-id))
    old))

(defun qq-gateway--method-available-p (method)
  "Compatibility wrapper checking whether Gateway advertises METHOD."
  (qq-gateway-rpc-method-available-p method))

(defun qq-gateway-refresh-accounts (&optional callback errback reason)
  "Fetch the authoritative managed-account registry.

CALLBACK receives the copied account list.  ERRBACK follows the transport
error convention.  REASON defaults to `resync'."
  (qq-gateway-rpc-latest-call
   'qq-gateway--refresh-owner "account.list" nil
   :projector
   (lambda (result)
     (qq-gateway--replace-accounts
      (alist-get 'accounts result) (or reason 'resync)
      (qq-gateway-transport-gateway-instance-id)))
   :callback callback
   :errback errback))

(defun qq-gateway--account-command
    (method account-id callback errback &optional remove-p)
  "Send account METHOD for ACCOUNT-ID and update the local registry.

CALLBACK receives the domain account snapshot and ERRBACK receives a protocol
error body plus reason.

When REMOVE-P is non-nil, remove the returned snapshot instead of merging it."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-gateway-rpc-call
   method `((account_id . ,account-id))
   :projector
   (lambda (snapshot)
     (if remove-p
         (qq-gateway--remove-account account-id 'removed)
       (qq-gateway--upsert-account snapshot 'response))
     snapshot)
   :callback callback
   :errback errback))

(defun qq-gateway--interactive-success (snapshot)
  "Report interactive success represented by account SNAPSHOT."
  (message "qq: account %s is %s"
           (alist-get 'account_id snapshot)
           (alist-get 'phase snapshot)))

(defun qq-gateway--interactive-error (_body reason)
  "Report interactive Gateway failure REASON."
  (message "qq: Gateway request failed: %s" reason))

;;;###autoload
(defun qq-gateway-account-create (label &optional callback errback)
  "Create a persistent QQ account slot with optional LABEL.

CALLBACK receives its snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (let ((value (string-trim (read-string "Account label (optional): "))))
           (and (not (string-empty-p value)) value))
         #'qq-gateway--interactive-success
         #'qq-gateway--interactive-error))
  (when label
    (unless (and (qq-gateway--non-empty-string-p label)
                 (equal label (string-trim label))
                 (<= (length label) 128))
      (user-error "qq: Account label must be trimmed and at most 128 characters")))
  (qq-gateway-rpc-call
   "account.create" (if label `((label . ,label)) '((label)))
   :projector
   (lambda (snapshot)
     (setq snapshot (qq-gateway--upsert-account snapshot 'response))
     ;; Creating a slot is an explicit local choice, unlike discovering
     ;; several pre-existing slots in gateway.ready.
     (qq-gateway--set-current-account (alist-get 'account_id snapshot))
     snapshot)
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-gateway-account-status (account-id &optional callback errback)
  "Fetch managed ACCOUNT-ID's current snapshot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-gateway--read-account-id)
         #'qq-gateway--interactive-success
         #'qq-gateway--interactive-error))
  (qq-gateway--account-command
   "account.status" account-id callback errback))

;;;###autoload
(defun qq-gateway-account-set-presence
    (account-id presence &optional callback errback)
  "Set PRESENCE for managed ACCOUNT-ID.

CALLBACK receives an acknowledgement carrying the account ID and
requested presence.  This command does not change the account lifecycle phase
or store presence in the local account snapshot."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (unless (qq-gateway-account account-id)
    (user-error "qq: QQ account does not exist: %s" account-id))
  (qq-gateway-rpc-call
   "account.set_presence"
   `((account_id . ,account-id) (presence . ,presence))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-gateway-account-start (account-id &optional callback errback)
  "Start ACCOUNT-ID's Native Session.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (list (qq-gateway--read-account-id "Start account: ")
         #'qq-gateway--interactive-success
         #'qq-gateway--interactive-error))
  (qq-gateway--account-command
   "account.start" account-id callback errback))

(defun qq-gateway--login-command
    (method account-id params secret callback errback)
  "Send login METHOD for ACCOUNT-ID with PARAMS and copied SECRET.

PARAMS is called with the copied secret.  CALLBACK receives the account
snapshot; ERRBACK receives a failure body and reason."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (let ((secret-copy (copy-sequence secret)))
    (unwind-protect
        (qq-gateway-rpc-call
         method (append `((account_id . ,account-id))
                        (funcall params secret-copy))
         :projector
         (lambda (snapshot)
           (qq-gateway--upsert-account snapshot 'response))
         :callback callback
         :errback errback)
      (clear-string secret-copy))))

(defun qq-gateway--project-quick-login-accounts (result)
  "Validate RESULT and return its ordered EasyLogin account metadata."
  (unless (and (qq-gateway--exact-object-keys-p result '(accounts))
               (listp (alist-get 'accounts result)))
    (error "qq: Gateway returned an invalid quick-login account list"))
  (let ((seen-uins (make-hash-table :test #'equal))
        accounts)
    (dolist (account (alist-get 'accounts result))
      (unless
          (and
           (qq-gateway--exact-object-keys-p
            account '(uin uid generated_at_unix))
           (qq-gateway--uint64-decimal-p (alist-get 'uin account))
           (qq-gateway--non-empty-string-p (alist-get 'uid account))
           (integerp (alist-get 'generated_at_unix account))
           (>= (alist-get 'generated_at_unix account) 0))
        (error "qq: Gateway returned invalid quick-login account metadata"))
      (let ((uin (alist-get 'uin account)))
        (when (gethash uin seen-uins)
          (error "qq: Gateway returned duplicate quick-login UIN %s" uin))
        (puthash uin t seen-uins))
      (push (qq-gateway-value-copy account) accounts))
    (nreverse accounts)))

;;;###autoload
(defun qq-gateway-account-login-list (&optional callback errback)
  "List identities with reusable native EasyLogin credentials.

CALLBACK receives ordered non-secret account metadata.  ERRBACK receives a
failure body and reason.  The Gateway advertises this operation only when its
encrypted credential store is available."
  (interactive
   (list (lambda (accounts)
           (message "qq: %d quick-login account%s available"
                    (length accounts)
                    (if (= (length accounts) 1) "" "s")))
         #'qq-gateway--interactive-error))
  (qq-gateway-rpc-call
   "account.login.list" nil
   :projector #'qq-gateway--project-quick-login-accounts
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-gateway-account-login-password
    (account-id uin password &optional qimei callback errback)
  "Begin password login for ACCOUNT-ID using exact UIN and PASSWORD.

QIMEI is optional.  The caller's PASSWORD string is not retained or mutated.
CALLBACK receives the account snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let* ((account-id (qq-gateway--read-account-id "Login account: "))
          (snapshot (qq-gateway-account account-id)))
     (list account-id
           (read-string "QQ UIN: " (alist-get 'uin snapshot))
           (read-passwd "QQ password: ") nil
           #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (unless (qq-gateway--uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (qq-gateway--non-empty-string-p password)
    (user-error "qq: Password must not be empty"))
  (unless (or (null qimei) (qq-gateway--non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (qq-gateway--login-command
   "account.login.password" account-id
   (lambda (secret)
     `((uin . ,uin) (password . ,secret)
       ,@(when qimei `((qimei . ,qimei)))))
   password callback errback))

;;;###autoload
(defun qq-gateway-account-login-quick
    (account-id uin &optional qimei callback errback)
  "Begin native EasyLogin for ACCOUNT-ID using the stored identity for UIN.

QIMEI is optional.  CALLBACK receives the account snapshot; ERRBACK receives
a failure body and reason.  No reusable credential material crosses the
Gateway protocol."
  (unless (qq-gateway--uint64-decimal-p uin)
    (user-error "qq: UIN must be a canonical nonzero uint64 string"))
  (unless (or (null qimei) (qq-gateway--non-empty-string-p qimei))
    (user-error "qq: QIMEI must be a non-empty string or nil"))
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-gateway-rpc-call
   "account.login.quick"
   `((account_id . ,account-id)
     (uin . ,uin)
     ,@(when qimei `((qimei . ,qimei))))
   :projector
   (lambda (snapshot)
     (qq-gateway--upsert-account snapshot 'response))
   :callback callback
   :errback errback))

;;;###autoload
(defun qq-gateway-account-login-captcha
    (account-id challenge-id ticket rand-str sid &optional callback errback)
  "Continue ACCOUNT-ID's captcha CHALLENGE-ID.

TICKET, RAND-STR, and SID carry the proof.  CALLBACK receives the account
snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let* ((account-id (qq-gateway--read-account-id "Captcha account: "))
          (challenge (alist-get 'challenge (qq-gateway-account account-id))))
     (unless (equal (alist-get 'kind challenge) "captcha")
       (user-error "qq: Selected account has no captcha challenge"))
     (list account-id (alist-get 'challenge_id challenge)
           (read-passwd "Captcha ticket: ")
           (read-string "Captcha randStr: ")
           (read-string "Captcha sid: " (alist-get 'sid challenge))
           #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (dolist (value (list challenge-id ticket rand-str sid))
    (unless (qq-gateway--non-empty-string-p value)
      (user-error "qq: Captcha proof fields must be non-empty strings")))
  (qq-gateway--login-command
   "account.login.captcha" account-id
   (lambda (secret)
     `((challenge_id . ,challenge-id) (ticket . ,secret)
       (rand_str . ,rand-str) (sid . ,sid)))
   ticket callback errback))

;;;###autoload
(defun qq-gateway-account-login-unusual-device
    (account-id challenge-id device-sig-hex &optional callback errback)
  "Continue ACCOUNT-ID's unusual-device CHALLENGE-ID using DEVICE-SIG-HEX.

CALLBACK receives the account snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let* ((account-id (qq-gateway--read-account-id "Unusual-device account: "))
          (challenge (alist-get 'challenge (qq-gateway-account account-id))))
     (unless (equal (alist-get 'kind challenge) "unusual_device")
       (user-error "qq: Selected account has no unusual-device challenge"))
     (list account-id (alist-get 'challenge_id challenge)
           (read-passwd "Device sig (hex): ")
           #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (unless (qq-gateway--non-empty-string-p challenge-id)
    (user-error "qq: Challenge ID must be a non-empty string"))
  (unless (and (stringp device-sig-hex)
               (string-match-p
                "\\`\\(?:[[:xdigit:]][[:xdigit:]]\\)+\\'" device-sig-hex))
    (user-error "qq: Device sig must contain a non-empty even number of hex digits"))
  (qq-gateway--login-command
   "account.login.unusual_device" account-id
   (lambda (secret)
     `((challenge_id . ,challenge-id) (device_sig_hex . ,secret)))
   device-sig-hex callback errback))

;;;###autoload
(defun qq-gateway-account-stop (account-id &optional callback errback)
  "Stop ACCOUNT-ID's Native Session without logging out of QQ.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-gateway--read-account-id "Stop Native Session: ")))
     (unless (yes-or-no-p
              (format "Stop Native Session %s without QQ logout? " account-id))
       (user-error "qq: Account stop cancelled"))
     (list account-id #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (qq-gateway--account-command
   "account.stop" account-id callback errback))

;;;###autoload
(defun qq-gateway-account-logout (account-id &optional callback errback)
  "Explicitly log ACCOUNT-ID out of QQ while retaining its managed slot.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-gateway--read-account-id "Log out account: ")))
     (unless (yes-or-no-p (format "Log QQ account %s out of QQ? " account-id))
       (user-error "qq: Account logout cancelled"))
     (list account-id #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (qq-gateway--account-command
   "account.logout" account-id callback errback))

;;;###autoload
(defun qq-gateway-account-remove (account-id &optional callback errback)
  "Delete stopped ACCOUNT-ID and its Gateway-managed local material.

CALLBACK receives the removed snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let ((account-id (qq-gateway--read-account-id "Remove account slot: ")))
     (unless (yes-or-no-p
              (format "Permanently remove QQ account slot %s? " account-id))
       (user-error "qq: Account removal cancelled"))
     (list account-id #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (qq-gateway--account-command
   "account.remove" account-id callback errback t))

;;;###autoload
(defun qq-gateway-connect ()
  "Connect to the long-lived native service without starting an account."
  (interactive)
  (qq-gateway-transport-start))

;;;###autoload
(defun qq-gateway-disconnect ()
  "Disconnect Emacs from Gateway without stopping or logging out accounts."
  (interactive)
  (qq-gateway-transport-stop))

(defun qq-gateway--handle-event (event data)
  "Project native service EVENT with DATA into the account registry."
  (pcase event
    ("gateway.ready"
     (let ((instance-id (alist-get 'gateway_instance_id data))
           (accounts (alist-get 'accounts data)))
       (let ((resync-marker qq-gateway--resync-request-id))
         (qq-gateway-rpc-cancel-latest
          'qq-gateway--refresh-owner "superseded_request"
          "Gateway account refresh was superseded by gateway.ready")
         (when (eq resync-marker qq-gateway--resync-request-id)
           (setq qq-gateway--resync-request-id nil)))
       (let ((projected
              (qq-gateway--replace-accounts accounts 'ready instance-id)))
         (qq-gateway--run-hook 'qq-gateway-ready-hook instance-id)
         projected)))
    ("account.changed"
     (qq-gateway--upsert-account data 'changed))
    ("account.removed"
     (qq-gateway--remove-account
      (alist-get 'account_id data) 'removed))
    (_ (error "qq: Unowned Gateway account event %s" event))))

(defun qq-gateway--handle-protocol-error (body)
  "Handle unsolicited Gateway protocol error BODY."
  (when (equal (alist-get 'code body) "event_stream_lagged")
    (qq-gateway--run-hook 'qq-gateway-desync-hook body)
    (qq-gateway-rpc-request-single-flight
     'qq-gateway--resync-request-id 'account-resync
     (lambda (success failure)
       (qq-gateway-refresh-accounts success failure 'resync))
     "QQ account")))

(dolist (event '("gateway.ready" "account.changed" "account.removed"))
  (qq-gateway-dispatch-register-event event #'qq-gateway--handle-event))
(qq-gateway-dispatch-register-error
 "event_stream_lagged" #'qq-gateway--handle-protocol-error)

(provide 'qq-gateway)

;;; qq-gateway.el ends here
