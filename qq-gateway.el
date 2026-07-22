;;; qq-gateway.el --- Native Gateway account lifecycle client -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; Client-side projection of nt-gateway's long-lived multi-account registry.
;; The selected account is local Emacs UI state; all managed accounts continue
;; running independently when the websocket or Emacs process disconnects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'qq-gateway-transport)
(require 'qq-protocol)

(defconst qq-gateway--account-phases
  '("stopped" "starting" "login_required" "logging_in" "online"
    "reconnecting" "stopping" "logged_out" "failed")
  "Closed account phase vocabulary for native Gateway protocol v1.")

(defvar qq-gateway-accounts-changed-hook nil
  "Hook called with REASON and ACCOUNT-ID after the local registry changes.

REASON is one of `ready', `resync', `changed', `response', or `removed'.
ACCOUNT-ID is nil for an authoritative full-registry replacement.")

(defvar qq-gateway-current-account-changed-hook nil
  "Hook called with OLD-ACCOUNT-ID and NEW-ACCOUNT-ID after selection changes.")

(defvar qq-gateway-desync-hook nil
  "Hook called with a protocol error body after transient events were lost.")

(defvar qq-gateway--accounts (make-hash-table :test #'equal))
(defvar qq-gateway--account-order nil)
(defvar qq-gateway--current-account-id nil)
(defvar qq-gateway--gateway-instance-id nil)
(defvar qq-gateway--resync-request-id nil)

(defun qq-gateway--run-hook (hook &rest arguments)
  "Run each function on HOOK with ARGUMENTS, isolating consumer errors."
  (apply
   #'run-hook-wrapped hook
   (lambda (function &rest hook-arguments)
     (condition-case error-data
         (apply function hook-arguments)
       (error
        (message "qq: Gateway client hook %s failed in %S: %s"
                 hook function (error-message-string error-data))))
     nil)
   arguments))

(defun qq-gateway--invoke (callback &rest arguments)
  "Invoke CALLBACK with ARGUMENTS while isolating ordinary errors."
  (when callback
    (condition-case error-data
        (apply callback arguments)
      (error
       (message "qq: Gateway account callback failed: %s"
                (error-message-string error-data))))))

(defun qq-gateway--client-error (errback code format-string &rest arguments)
  "Invoke ERRBACK with client CODE.

FORMAT-STRING and ARGUMENTS produce the human-readable failure text."
  (let ((reason (apply #'format format-string arguments)))
    (qq-gateway--invoke
     errback `((code . ,code) (message . ,reason)) reason)
    nil))

(defun qq-gateway--exact-object-keys-p (object keys)
  "Return non-nil when alist OBJECT has exactly symbol KEYS."
  (and (listp object)
       (cl-every (lambda (entry)
                   (and (consp entry) (symbolp (car entry))))
                 object)
       (equal
        (sort (mapcar #'car object)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right))))
        (sort (copy-sequence keys)
              (lambda (left right)
                (string-lessp (symbol-name left) (symbol-name right)))))))

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

(defun qq-gateway--decimal-less-p (left right)
  "Return non-nil when canonical decimal LEFT is less than RIGHT.

The comparison never coerces either protocol value to an Emacs number."
  (or (< (length left) (length right))
      (and (= (length left) (length right))
           (string-lessp left right))))

(defun qq-gateway--validate-optional-string (value context)
  "Validate optional string VALUE for CONTEXT."
  (unless (or (null value) (stringp value))
    (error "qq: %s must be a string or null" context)))

(defun qq-gateway--validate-challenge (challenge)
  "Validate and copy a login CHALLENGE, or return nil."
  (when challenge
    (unless (listp challenge)
      (error "qq: Gateway account challenge must be an object or null"))
    (let ((kind (alist-get 'kind challenge)))
      (pcase kind
        ("captcha"
         (unless (qq-gateway--exact-object-keys-p
                  challenge '(kind challenge_id url sid))
           (error "qq: Gateway captcha challenge has invalid fields"))
         (qq-gateway--validate-optional-string
          (alist-get 'url challenge) "captcha URL")
         (qq-gateway--validate-optional-string
          (alist-get 'sid challenge) "captcha sid"))
        ("new_device"
         (unless (qq-gateway--exact-object-keys-p
                  challenge '(kind challenge_id jump_url))
           (error "qq: Gateway new-device challenge has invalid fields"))
         (qq-gateway--validate-optional-string
          (alist-get 'jump_url challenge) "new-device jump URL"))
        ("unusual_device"
         (unless (qq-gateway--exact-object-keys-p
                  challenge '(kind challenge_id))
           (error "qq: Gateway unusual-device challenge has invalid fields")))
        (_ (error "qq: Gateway account challenge has unknown kind %S" kind)))
      (unless (qq-gateway--non-empty-string-p
               (alist-get 'challenge_id challenge))
        (error "qq: Gateway challenge_id must be a non-empty string"))
      (copy-tree challenge))))

(defun qq-gateway--validate-problem (problem)
  "Validate and copy account PROBLEM, or return nil."
  (when problem
    (unless (and (qq-gateway--exact-object-keys-p problem '(code message))
                 (qq-gateway--non-empty-string-p (alist-get 'code problem))
                 (qq-gateway--non-empty-string-p (alist-get 'message problem)))
      (error "qq: Gateway account problem is malformed"))
    (copy-tree problem)))

(defun qq-gateway--validate-account (snapshot)
  "Validate and copy one closed managed-account SNAPSHOT."
  (unless (qq-gateway--exact-object-keys-p
           snapshot
           '(account_id label phase uin uid generation challenge problem))
    (error "qq: Gateway account snapshot has invalid fields"))
  (let ((account-id (alist-get 'account_id snapshot))
        (label (alist-get 'label snapshot nil nil #'eq))
        (phase (alist-get 'phase snapshot))
        (uin (alist-get 'uin snapshot nil nil #'eq))
        (uid (alist-get 'uid snapshot nil nil #'eq))
        (generation (alist-get 'generation snapshot)))
    (unless (qq-gateway--non-empty-string-p account-id)
      (error "qq: Gateway account_id must be a non-empty opaque string"))
    (unless (or (null label)
                (and (qq-gateway--non-empty-string-p label)
                     (equal label (string-trim label))
                     (<= (length label) 128)))
      (error "qq: Gateway account label is invalid"))
    (unless (member phase qq-gateway--account-phases)
      (error "qq: Gateway account phase is invalid"))
    (unless (or (null uin) (qq-gateway--canonical-decimal-p uin))
      (error "qq: Gateway account UIN must be an exact decimal string or null"))
    (unless (or (null uid) (qq-gateway--non-empty-string-p uid))
      (error "qq: Gateway account UID must be an opaque string or null"))
    (unless (qq-gateway--canonical-decimal-p generation t)
      (error "qq: Gateway account generation must be a decimal string"))
    (qq-gateway--validate-challenge
     (alist-get 'challenge snapshot nil nil #'eq))
    (qq-gateway--validate-problem
     (alist-get 'problem snapshot nil nil #'eq))
    (copy-tree snapshot)))

(defun qq-gateway-account (account-id)
  "Return a copy of managed ACCOUNT-ID's snapshot, or nil."
  (copy-tree (and account-id (gethash account-id qq-gateway--accounts))))

(defun qq-gateway-accounts ()
  "Return managed account snapshots in authoritative Gateway order."
  (delq nil
        (mapcar (lambda (account-id)
                  (qq-gateway-account account-id))
                qq-gateway--account-order)))

(defun qq-gateway-current-account-id ()
  "Return the account selected by this Emacs client, or nil."
  qq-gateway--current-account-id)

(defun qq-gateway-current-account ()
  "Return a copy of this Emacs client's selected account snapshot."
  (qq-gateway-account qq-gateway--current-account-id))

(defun qq-gateway-current-account-owner ()
  "Return selected `(ACCOUNT-ID . GENERATION)' ownership, or nil."
  (when-let* ((snapshot (qq-gateway-current-account)))
    (cons (alist-get 'account_id snapshot)
          (alist-get 'generation snapshot))))

(defun qq-gateway--set-current-account (account-id)
  "Select ACCOUNT-ID locally and publish an exact selection change."
  (when account-id
    (unless (gethash account-id qq-gateway--accounts)
      (user-error "qq: Gateway account does not exist: %s" account-id)))
  (unless (equal account-id qq-gateway--current-account-id)
    (let ((old qq-gateway--current-account-id))
      (setq qq-gateway--current-account-id account-id)
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
            (completing-read (or prompt "Gateway account: ")
                             choices nil t nil nil default)
            choices)))))

;;;###autoload
(defun qq-gateway-account-select (account-id)
  "Select managed ACCOUNT-ID for this Emacs client's chat projection."
  (interactive (list (qq-gateway--read-account-id "Select Gateway account: ")))
  (qq-gateway--set-current-account account-id)
  (when (called-interactively-p 'interactive)
    (message "qq: selected Gateway account %s" account-id))
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
    (dolist (raw snapshots)
      (let* ((snapshot (qq-gateway--validate-account raw))
             (account-id (alist-get 'account_id snapshot)))
        (when (gethash account-id next)
          (error "qq: Duplicate Gateway account ID %s" account-id))
        (puthash account-id snapshot next)
        (push account-id order)))
    (setq qq-gateway--accounts next
          qq-gateway--account-order (nreverse order)
          qq-gateway--gateway-instance-id instance-id)
    (qq-gateway--maybe-select-single-account)
    (qq-gateway--run-hook 'qq-gateway-accounts-changed-hook reason nil)
    (qq-gateway-accounts)))

(defun qq-gateway--upsert-account (raw-snapshot reason)
  "Validate and merge RAW-SNAPSHOT for REASON."
  (let* ((snapshot (qq-gateway--validate-account raw-snapshot))
         (account-id (alist-get 'account_id snapshot))
         (existing (gethash account-id qq-gateway--accounts))
         (stale
          (and existing
               (qq-gateway--decimal-less-p
                (alist-get 'generation snapshot)
                (alist-get 'generation existing)))))
    (unless stale
      (unless existing
        (setq qq-gateway--account-order
              (append qq-gateway--account-order (list account-id))))
      (unless (equal existing snapshot)
        (puthash account-id snapshot qq-gateway--accounts)
        (qq-gateway--maybe-select-single-account)
        (qq-gateway--run-hook
         'qq-gateway-accounts-changed-hook reason account-id)))
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

(defun qq-gateway--validate-account-list-result (result)
  "Validate and return accounts carried by account.list RESULT."
  (unless (qq-gateway--exact-object-keys-p result '(accounts))
    (error "qq: Gateway account.list result has invalid fields"))
  (let ((accounts (alist-get 'accounts result nil nil #'eq)))
    (unless (listp accounts)
      (error "qq: Gateway account.list accounts must be an array"))
    accounts))

(defun qq-gateway--method-available-p (method)
  "Return non-nil when the ready Gateway advertises METHOD."
  (member method (qq-gateway-transport-capabilities)))

(defun qq-gateway--send (method params callback errback)
  "Send native METHOD with PARAMS after capability negotiation.

CALLBACK receives a successful result.  ERRBACK receives an error body and
human-readable reason."
  (cond
   ((not (qq-gateway-transport-ready-p))
    (qq-gateway--client-error
     errback "gateway_not_ready" "Gateway transport is not ready"))
   ((not (qq-gateway--method-available-p method))
    (qq-gateway--client-error
     errback "capability_unavailable"
     "Gateway does not advertise capability %s" method))
   (t
    (qq-gateway-transport-send method params callback errback))))

(defun qq-gateway-refresh-accounts (&optional callback errback reason)
  "Fetch the authoritative managed-account registry.

CALLBACK receives the copied account list.  ERRBACK follows the transport
error convention.  REASON defaults to `resync'."
  (qq-gateway--send
   "account.list" nil
   (lambda (result)
     (condition-case error-data
         (let ((accounts
                (qq-gateway--replace-accounts
                 (qq-gateway--validate-account-list-result result)
                 (or reason 'resync)
                 (qq-gateway-transport-gateway-instance-id))))
           (setq qq-gateway--resync-request-id nil)
           (qq-gateway--invoke callback accounts))
       (error
        (setq qq-gateway--resync-request-id nil)
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   (lambda (body failure)
     (setq qq-gateway--resync-request-id nil)
     (qq-gateway--invoke errback body failure))))

(defun qq-gateway--account-command
    (method account-id callback errback &optional remove-p)
  "Send account METHOD for ACCOUNT-ID and update the local registry.

CALLBACK receives the validated account snapshot and ERRBACK receives a
protocol error body plus reason.

When REMOVE-P is non-nil, remove the returned snapshot instead of merging it."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (qq-gateway--send
   method `((account_id . ,account-id))
   (lambda (result)
     (condition-case error-data
         (let ((snapshot (qq-gateway--validate-account result)))
           (unless (equal account-id (alist-get 'account_id snapshot))
             (error "qq: Gateway response account_id contradicts request"))
           (if remove-p
               (qq-gateway--remove-account account-id 'removed)
             (qq-gateway--upsert-account snapshot 'response))
           (qq-gateway--invoke callback snapshot))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

(defun qq-gateway--interactive-success (snapshot)
  "Report interactive success represented by account SNAPSHOT."
  (message "qq: account %s is %s (generation %s)"
           (alist-get 'account_id snapshot)
           (alist-get 'phase snapshot)
           (alist-get 'generation snapshot)))

(defun qq-gateway--interactive-error (_body reason)
  "Report interactive Gateway failure REASON."
  (message "qq: Gateway request failed: %s" reason))

;;;###autoload
(defun qq-gateway-account-create (label &optional callback errback)
  "Create a persistent Gateway account slot with optional LABEL.

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
  (qq-gateway--send
   "account.create" (if label `((label . ,label)) '((label)))
   (lambda (result)
     (condition-case error-data
         (let* ((snapshot (qq-gateway--upsert-account result 'response))
                (account-id (alist-get 'account_id snapshot)))
           ;; Creating a slot is an explicit local choice, unlike discovering
           ;; several pre-existing slots in gateway.ready.
           (qq-gateway--set-current-account account-id)
           (qq-gateway--invoke callback snapshot))
       (error
        (qq-gateway--client-error
         errback "invalid_gateway_result" "%s"
         (error-message-string error-data)))))
   errback))

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

(defun qq-gateway--validate-presence-receipt
    (result account-id generation presence)
  "Validate account presence RESULT against its request ownership.

ACCOUNT-ID and GENERATION identify the Account Runtime that accepted
PRESENCE.  The receipt acknowledges the command; it is not an authoritative
account snapshot."
  (unless (qq-gateway--exact-object-keys-p
           result '(account_id generation presence))
    (error "qq: Gateway account.set_presence result has invalid fields"))
  (let ((returned-account-id (alist-get 'account_id result))
        (returned-generation (alist-get 'generation result))
        (returned-presence
         (qq-protocol-validate-account-presence
          (alist-get 'presence result) "Gateway presence receipt")))
    (unless (equal returned-account-id account-id)
      (error "qq: Gateway presence receipt account_id contradicts request"))
    (unless (equal returned-generation generation)
      (error "qq: Gateway presence receipt generation contradicts request"))
    (unless (equal returned-presence presence)
      (error "qq: Gateway presence receipt contradicts requested presence"))
    `((account_id . ,returned-account-id)
      (generation . ,returned-generation)
      (presence . ,returned-presence))))

;;;###autoload
(defun qq-gateway-account-set-presence
    (account-id presence &optional callback errback)
  "Set PRESENCE for managed ACCOUNT-ID's current runtime generation.

CALLBACK receives a closed acknowledgement carrying account ownership and the
requested presence.  This command does not change the account lifecycle phase
or store presence in the local account snapshot."
  (unless (qq-gateway--non-empty-string-p account-id)
    (user-error "qq: Account ID must be a non-empty opaque string"))
  (setq presence
        (qq-protocol-validate-account-presence
         presence "account presence" 'user-error))
  (let* ((account
          (or (qq-gateway-account account-id)
              (user-error "qq: Gateway account does not exist: %s" account-id)))
         (generation (alist-get 'generation account))
         (owner (cons account-id generation)))
    (qq-gateway--send
     "account.set_presence"
     `((account_id . ,account-id) (presence . ,presence))
     (lambda (result)
       (condition-case error-data
           (let ((receipt
                  (qq-gateway--validate-presence-receipt
                   result account-id generation presence)))
             (unless (equal owner
                            (let ((current (qq-gateway-account account-id)))
                              (and current
                                   (cons account-id
                                         (alist-get 'generation current)))))
               (error "qq: Gateway account generation changed before presence acknowledgement"))
             (qq-gateway--invoke callback receipt))
         (error
          (qq-gateway--client-error
           errback "invalid_gateway_result" "%s"
           (error-message-string error-data)))))
     errback)))

;;;###autoload
(defun qq-gateway-account-start (account-id &optional callback errback)
  "Attach a new Account Runtime generation to managed ACCOUNT-ID.

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
        (qq-gateway--send
         method (append `((account_id . ,account-id))
                        (funcall params secret-copy))
         (lambda (result)
           (condition-case error-data
               (let ((snapshot (qq-gateway--validate-account result)))
                 (unless (equal account-id (alist-get 'account_id snapshot))
                   (error "qq: Gateway login response account_id contradicts request"))
                 (qq-gateway--upsert-account snapshot 'response)
                 (qq-gateway--invoke callback snapshot))
             (error
              (qq-gateway--client-error
               errback "invalid_gateway_result" "%s"
               (error-message-string error-data)))))
         errback)
      (clear-string secret-copy))))

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
  (unless (qq-gateway--canonical-decimal-p uin)
    (user-error "qq: UIN must be a canonical positive decimal string"))
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
(defun qq-gateway-account-login-new-device
    (account-id challenge-id token &optional callback errback)
  "Continue ACCOUNT-ID's new-device CHALLENGE-ID using TOKEN.

CALLBACK receives the account snapshot; ERRBACK receives a failure body and
reason."
  (interactive
   (let* ((account-id (qq-gateway--read-account-id "New-device account: "))
          (challenge (alist-get 'challenge (qq-gateway-account account-id))))
     (unless (equal (alist-get 'kind challenge) "new_device")
       (user-error "qq: Selected account has no new-device challenge"))
     (list account-id (alist-get 'challenge_id challenge)
           (read-passwd "New-device token: ")
           #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (unless (and (qq-gateway--non-empty-string-p challenge-id)
               (qq-gateway--non-empty-string-p token))
    (user-error "qq: Challenge ID and token must be non-empty strings"))
  (qq-gateway--login-command
   "account.login.new_device" account-id
   (lambda (secret)
     `((challenge_id . ,challenge-id) (token . ,secret)))
   token callback errback))

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
  "Stop ACCOUNT-ID's local runtime without logging out of QQ.

CALLBACK receives the snapshot; ERRBACK receives a failure body and reason."
  (interactive
   (let ((account-id (qq-gateway--read-account-id "Stop account runtime: ")))
     (unless (yes-or-no-p
              (format "Stop Gateway runtime %s without QQ logout? " account-id))
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
     (unless (yes-or-no-p (format "Log Gateway account %s out of QQ? " account-id))
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
              (format "Permanently remove Gateway account slot %s? " account-id))
       (user-error "qq: Account removal cancelled"))
     (list account-id #'qq-gateway--interactive-success
           #'qq-gateway--interactive-error)))
  (qq-gateway--account-command
   "account.remove" account-id callback errback t))

;;;###autoload
(defun qq-gateway-connect ()
  "Connect to the long-lived native Gateway without starting an account."
  (interactive)
  (qq-gateway-transport-start))

;;;###autoload
(defun qq-gateway-disconnect ()
  "Disconnect Emacs from Gateway without stopping or logging out accounts."
  (interactive)
  (qq-gateway-transport-stop))

(defun qq-gateway--handle-event (event data)
  "Project native Gateway EVENT with DATA into the account registry."
  (condition-case error-data
      (pcase event
        ("gateway.ready"
         (unless (qq-gateway--exact-object-keys-p
                  data '(gateway_instance_id accounts))
           (error "qq: Gateway.ready data has invalid fields"))
         (let ((instance-id (alist-get 'gateway_instance_id data))
               (accounts (alist-get 'accounts data nil nil #'eq)))
           (unless (and (qq-gateway--non-empty-string-p instance-id)
                        (listp accounts))
             (error "qq: Gateway.ready data is malformed"))
           (setq qq-gateway--resync-request-id nil)
           (qq-gateway--replace-accounts accounts 'ready instance-id)))
        ("account.changed"
         (qq-gateway--upsert-account data 'changed))
        ("account.removed"
         (unless (and (qq-gateway--exact-object-keys-p data '(account_id))
                      (qq-gateway--non-empty-string-p
                       (alist-get 'account_id data)))
           (error "qq: Account.removed data is malformed"))
         (qq-gateway--remove-account
          (alist-get 'account_id data) 'removed)))
    (error
     (qq-gateway-transport--protocol-violation
      "Malformed %s event: %s" event
      (error-message-string error-data)))))

(defun qq-gateway--handle-protocol-error (body)
  "Handle unsolicited Gateway protocol error BODY."
  (when (equal (alist-get 'code body) "event_stream_lagged")
    (qq-gateway--run-hook 'qq-gateway-desync-hook (copy-tree body))
    (unless qq-gateway--resync-request-id
      (setq qq-gateway--resync-request-id
            (qq-gateway-refresh-accounts
             nil
             (lambda (_error failure)
               (message "qq: Gateway account resync failed: %s" failure))
             'resync)))))

(add-hook 'qq-gateway-transport-event-hook #'qq-gateway--handle-event)
(add-hook 'qq-gateway-transport-protocol-error-hook
          #'qq-gateway--handle-protocol-error)

(provide 'qq-gateway)

;;; qq-gateway.el ends here
