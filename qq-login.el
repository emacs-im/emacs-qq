;;; qq-login.el --- Native QQ authorization interaction -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>
;; Keywords: comm

;;; Commentary:

;; Foreground authorization controller for one managed QQ account.  Gateway
;; owns every account runtime; this file only turns the selected account's
;; projected phase/challenge into a serialized Emacs interaction.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'browse-url)
(require 'qq-customize)
(require 'qq-gateway)
(require 'qq-gateway-transport)
(require 'qq-native)

(cl-defstruct (qq-login--session
               (:constructor qq-login--session-create))
  "One foreground authorization interaction."
  active-p
  account-id
  create-p
  label
  label-read-p
  login-accounts-loaded-p
  login-accounts
  quick-login-uin
  in-flight-p
  prompting-p
  retry-failed-p
  submitted-challenge-id
  status
  error
  qr-url
  qr-display
  qr-file
  timer)

(defvar qq-login--current nil
  "Current foreground authorization interaction, or nil.")

(defvar qq-login-change-hook nil
  "Hook run when the foreground login presentation changes.")

(defun qq-login-active-p ()
  "Return non-nil when an interactive login flow is active."
  (and (qq-login--session-p qq-login--current)
       (qq-login--session-active-p qq-login--current)))

(defun qq-login--current-p (session)
  "Return non-nil when SESSION still owns foreground authorization."
  (and (eq session qq-login--current)
       (qq-login--session-active-p session)))

(defun qq-login--cancel-timer (session)
  "Cancel SESSION's pending progression timer."
  (when-let* ((timer (qq-login--session-timer session)))
    (setf (qq-login--session-timer session) nil)
    (cancel-timer timer)))

(defun qq-login--delete-qr-file (session)
  "Delete SESSION's generated QR image, if any."
  (when-let* ((file (qq-login--session-qr-file session)))
    (setf (qq-login--session-qr-file session) nil)
    (when (file-exists-p file)
      (ignore-errors (delete-file file)))))

(defun qq-login--clear-qr (session)
  "Clear SESSION's QR presentation."
  (qq-login--delete-qr-file session)
  (setf (qq-login--session-qr-url session) nil
        (qq-login--session-qr-display session) nil))

(defun qq-login--changed ()
  "Publish a foreground login presentation change."
  (run-hooks 'qq-login-change-hook))

(defun qq-login--present (session status error-text)
  "Present STATUS and ERROR-TEXT for current SESSION."
  (when (qq-login--current-p session)
    (unless (and (equal status (qq-login--session-status session))
                 (equal error-text (qq-login--session-error session)))
      (setf (qq-login--session-status session) status
            (qq-login--session-error session) error-text)
      (qq-login--changed))))

(defun qq-login--finish (session &optional message-text)
  "Finish SESSION and optionally report MESSAGE-TEXT."
  (when (qq-login--current-p session)
    (qq-login--cancel-timer session)
    (qq-login--clear-qr session)
    (setf (qq-login--session-active-p session) nil
          (qq-login--session-in-flight-p session) nil
          (qq-login--session-prompting-p session) nil)
    (setq qq-login--current nil)
    (qq-login--changed)
    (when message-text
      (message "%s" message-text)))
  nil)

;;;###autoload
(defun qq-login-cancel ()
  "Cancel the foreground login interaction without stopping its QQ runtime."
  (interactive)
  (if (qq-login-active-p)
      (qq-login--finish
       qq-login--current
       "qq: login interaction cancelled; account runtime left unchanged")
    (when (called-interactively-p 'interactive)
      (message "qq: no login interaction is active"))))

(defun qq-login--timer-fire (session)
  "Progress SESSION outside the Gateway event callback."
  (when (qq-login--session-p session)
    (setf (qq-login--session-timer session) nil))
  (when (qq-login--current-p session)
    (qq-login--drive session)))

(defun qq-login--schedule (session)
  "Schedule one progression pass for current SESSION."
  (when (and (qq-login--current-p session)
             (not (timerp (qq-login--session-timer session))))
    (setf (qq-login--session-timer session)
          (run-at-time 0 nil #'qq-login--timer-fire session))))

(defun qq-login--request-error (session body reason)
  "Pause SESSION after request failure BODY described by REASON."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil)
    (let* ((code (alist-get 'code body))
           (error-text
            (format "Login failed%s: %s"
                    (if (qq-gateway--non-empty-string-p code)
                        (format " [%s]" code)
                      "")
                    (or reason "native request failed"))))
      (qq-login--present session "Login paused." error-text)
      (message "qq: %s" error-text))))

(defun qq-login--request-success (session _snapshot)
  "Continue SESSION after a lifecycle request returned a snapshot."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil)
    (qq-login--present session "Processing QQ login state…" nil)
    (qq-login--schedule session)))

(defun qq-login--create-success (session snapshot)
  "Bind SESSION to newly created account SNAPSHOT and continue."
  (when (qq-login--current-p session)
    (let ((account-id (alist-get 'account_id snapshot)))
      (setf (qq-login--session-account-id session)
            (copy-sequence account-id)
            (qq-login--session-create-p session) nil
            (qq-login--session-in-flight-p session) nil)
      (qq-gateway--set-current-account account-id)
      (qq-login--present session "Managed account created." nil)
      (qq-login--schedule session))))

(defun qq-login--request (session starter &optional success failure)
  "Run asynchronous lifecycle STARTER for SESSION.

SUCCESS defaults to `qq-login--request-success'.  FAILURE defaults to
`qq-login--request-error'."
  (setf (qq-login--session-in-flight-p session) t)
  (qq-login--changed)
  (condition-case error-data
      (funcall starter
               (apply-partially
                (or success #'qq-login--request-success) session)
               (apply-partially
                (or failure #'qq-login--request-error) session))
    (error
     (setf (qq-login--session-in-flight-p session) nil)
     (funcall
      (or failure #'qq-login--request-error)
      session nil
      (format "could not start native request: %s"
              (error-message-string error-data))))))

(defun qq-login--read-label ()
  "Read an optional label for a newly managed account."
  (let ((label (string-trim (read-string "Account label (optional): "))))
    (and (not (string-empty-p label)) label)))

(defun qq-login--quick-login-available-p ()
  "Return non-nil when the Gateway exposes the complete EasyLogin API."
  (and (qq-gateway--method-available-p "account.login.list")
       (qq-gateway--method-available-p "account.login.quick")))

(defun qq-login--active-runtime-p (account)
  "Return non-nil when managed ACCOUNT already has an active runtime."
  (member (alist-get 'phase account)
          '("online" "starting" "logging_in" "stopping")))

(defun qq-login--in-progress-account ()
  "Return the selected account when its lifecycle should be continued."
  (when-let* ((account (qq-gateway-current-account)))
    (and (qq-login--active-runtime-p account)
         account)))

(defun qq-login--quick-account-list-success (session accounts)
  "Install EasyLogin ACCOUNTS for current SESSION and schedule its chooser."
  (when (qq-login--current-p session)
    (setf (qq-login--session-in-flight-p session) nil
          (qq-login--session-login-accounts-loaded-p session) t
          (qq-login--session-login-accounts session)
          (copy-tree accounts))
    (qq-login--present session "Choose a QQ account." nil)
    (qq-login--schedule session)))

(defun qq-login--request-quick-accounts (session)
  "Request completion-safe EasyLogin identities for SESSION."
  (qq-login--present session "Loading quick-login accounts…" nil)
  (qq-login--request
   session
   (lambda (success failure)
     (qq-gateway-account-login-list success failure))
   #'qq-login--quick-account-list-success))

(defun qq-login--managed-account-title (account)
  "Return the compact user-facing identity for managed ACCOUNT."
  (let ((label (alist-get 'label account))
        (uin (alist-get 'uin account)))
    (format "%s%s"
            (or label uin "Unbound account")
            (if (and label uin) (format " (%s)" uin) ""))))

(defun qq-login--phase-label (phase)
  "Return a human-readable completion label for account PHASE."
  (capitalize (string-replace "_" " " phase)))

(defun qq-login--quick-account-label (quick-account &optional managed-account)
  "Return the completion label for QUICK-ACCOUNT.

When MANAGED-ACCOUNT already represents the same QQ identity, include its
user-facing label."
  (format "%s — Quick login"
          (if managed-account
              (qq-login--managed-account-title managed-account)
            (alist-get 'uin quick-account))))

(defun qq-login--managed-account-label (account)
  "Return a lifecycle-aware completion label for managed ACCOUNT."
  (format "%s — %s"
          (qq-login--managed-account-title account)
          (qq-login--phase-label (alist-get 'phase account))))

(defun qq-login--quick-account-for-managed (account quick-accounts)
  "Return the EasyLogin identity matching managed ACCOUNT."
  (when-let* ((uin (alist-get 'uin account)))
    (seq-find
     (lambda (quick-account)
       (equal uin (alist-get 'uin quick-account)))
     quick-accounts)))

(defun qq-login--account-choices (session)
  "Return unified managed-account and EasyLogin choices for SESSION."
  (let ((quick-accounts (qq-login--session-login-accounts session))
        managed-choices
        quick-choices
        represented-uins)
    (dolist (account (qq-gateway-accounts))
      (let ((quick-account
             (qq-login--quick-account-for-managed account quick-accounts)))
        (when quick-account
          (push (alist-get 'uin quick-account) represented-uins))
        (if (and quick-account
                 (not (qq-login--active-runtime-p account)))
            (push
             (cons (qq-login--quick-account-label quick-account account)
                   (cons 'quick quick-account))
             managed-choices)
          (push
           (cons (qq-login--managed-account-label account)
                 (cons 'managed account))
           managed-choices))))
    (dolist (quick-account quick-accounts)
      (unless (member (alist-get 'uin quick-account) represented-uins)
        (push
         (cons (qq-login--quick-account-label quick-account)
               (cons 'quick quick-account))
         quick-choices)))
    (append (nreverse managed-choices)
            (nreverse quick-choices)
            '(("New account" . (new))))))

(defun qq-login--matching-managed-account (quick-account)
  "Return a managed slot already bound to QUICK-ACCOUNT, or nil."
  (let* ((uin (alist-get 'uin quick-account))
         (uid (alist-get 'uid quick-account))
         (current (qq-gateway-current-account))
         (accounts (qq-gateway-accounts))
         (bound
          (cl-remove-if-not
           (lambda (account)
             (equal (alist-get 'uin account) uin))
           accounts)))
    (dolist (account bound)
      (when (and (alist-get 'uid account)
                 (not (equal (alist-get 'uid account) uid)))
        (error "qq: stored quick-login identity contradicts managed UIN %s"
               uin)))
    (or
     (and current
          (equal (alist-get 'uin current) uin)
          current)
     (car bound)
     ;; An explicitly selected unbound slot is safe to bind.  This matters
     ;; after `account.start' has already brought it to `login_required'.
     (and current
          (null (alist-get 'uin current))
          (null (alist-get 'uid current))
          current))))

(defun qq-login--read-account-choice (session)
  "Read and install one managed, EasyLogin, or new-account choice for SESSION."
  (let* ((choices (qq-login--account-choices session))
         choice)
    (setf (qq-login--session-prompting-p session) t)
    (unwind-protect
        (setq choice
              (cdr
               (assoc
                (completing-read "QQ login: " choices nil t)
                choices)))
      (when (qq-login--session-p session)
        (setf (qq-login--session-prompting-p session) nil)))
    (pcase choice
      (`(managed . ,account)
       (let ((account-id (alist-get 'account_id account)))
         (setf (qq-login--session-account-id session)
               (copy-sequence account-id)
               (qq-login--session-create-p session) nil
               (qq-login--session-quick-login-uin session) nil)
         (qq-gateway--set-current-account account-id)))
      (`(quick . ,account)
       (let ((managed (qq-login--matching-managed-account account)))
         (setf (qq-login--session-quick-login-uin session)
               (copy-sequence (alist-get 'uin account))
               (qq-login--session-account-id session)
               (and managed
                    (copy-sequence (alist-get 'account_id managed)))
               (qq-login--session-create-p session) (null managed)
               ;; A quick-login identity already provides the useful label.
               (qq-login--session-label session) nil
               (qq-login--session-label-read-p session) t)
         (when managed
           (qq-gateway--set-current-account
            (alist-get 'account_id managed)))))
      (`(new)
       (setf (qq-login--session-account-id session) nil
             (qq-login--session-create-p session) t
             (qq-login--session-label session) nil
             (qq-login--session-label-read-p session) nil
             (qq-login--session-quick-login-uin session) nil))
      (_ (error "qq: invalid login account choice")))
    t))

(defun qq-login--resolve-account-choice (session)
  "Resolve SESSION's account source before driving its lifecycle.

Return non-nil once SESSION names a managed slot or requests creation.  A nil
return means the asynchronous EasyLogin identity request is still pending."
  (let ((in-progress (qq-login--in-progress-account)))
    (cond
     ((or (qq-login--session-account-id session)
          (qq-login--session-create-p session))
      t)
     (in-progress
      (setf (qq-login--session-account-id session)
            (copy-sequence (alist-get 'account_id in-progress)))
      t)
     ((not (qq-login--quick-login-available-p))
      ;; Credential storage is optional.  Retain the account/password path
      ;; when the Gateway deliberately omits EasyLogin capabilities.
      t)
     ((not (qq-login--session-login-accounts-loaded-p session))
      (qq-login--request-quick-accounts session)
      nil)
     (t
      (qq-login--read-account-choice session)))))

(defun qq-login--ensure-account (session)
  "Resolve or create SESSION's account.

Return its current snapshot, or nil while account creation is in flight."
  (let ((account-id (qq-login--session-account-id session)))
    (cond
     (account-id
      (or (qq-gateway-account account-id)
          (user-error "qq: QQ account does not exist: %s" account-id)))
     ((or (qq-login--session-create-p session)
          (null (qq-gateway-accounts)))
      (unless (qq-login--session-in-flight-p session)
        (unless (qq-login--session-label-read-p session)
          (setf (qq-login--session-label session) (qq-login--read-label)
                (qq-login--session-label-read-p session) t))
        (qq-login--present session "Creating managed QQ account…" nil)
        (qq-login--request
         session
         (lambda (success failure)
           (qq-gateway-account-create
            (qq-login--session-label session) success failure))
         #'qq-login--create-success))
      nil)
     (t
      (let ((selected
             (or (qq-gateway-current-account-id)
                 (and (= (length (qq-gateway-accounts)) 1)
                      (alist-get 'account_id (car (qq-gateway-accounts))))
                 (qq-gateway--read-account-id "Login account: "))))
        (setf (qq-login--session-account-id session)
              (copy-sequence selected))
        (qq-gateway--set-current-account selected)
        (qq-gateway-account selected))))))

(defun qq-login--open-captcha-url (url)
  "Offer the CAPTCHA URL to the user."
  (when (qq-gateway--non-empty-string-p url)
    (message "qq: complete QQ captcha verification: %s" url)
    (when qq-login-open-verification-url
      (browse-url url))))

(defun qq-login--password (session account)
  "Read credentials and begin password login for SESSION ACCOUNT."
  (setf (qq-login--session-prompting-p session) t)
  (unwind-protect
      (let* ((account-id (alist-get 'account_id account))
             (uin (read-string "QQ UIN: " (alist-get 'uin account)))
             (password (read-passwd "QQ password: ")))
        (unwind-protect
            (progn
              (setf (qq-login--session-retry-failed-p session) nil)
              (qq-login--present session "Signing in to QQ…" nil)
              (qq-login--request
               session
               (lambda (success failure)
                 (qq-gateway-account-login-password
                  account-id uin password nil success failure))))
          (clear-string password)))
    (when (qq-login--session-p session)
      (setf (qq-login--session-prompting-p session) nil))))

(defun qq-login--quick-error (session body reason)
  "Pause SESSION after EasyLogin failure BODY described by REASON."
  (when (qq-login--session-p session)
    ;; Continuing the same session retries this stable managed slot through
    ;; password login instead of repeatedly using a rejected stored record.
    (setf (qq-login--session-quick-login-uin session) nil))
  (qq-login--request-error session body reason))

(defun qq-login--quick (session account)
  "Begin EasyLogin for SESSION ACCOUNT using its selected stored identity."
  (let ((uin (qq-login--session-quick-login-uin session)))
    (unless uin
      (error "qq: quick login has no selected UIN"))
    (setf (qq-login--session-retry-failed-p session) nil)
    (qq-login--present session (format "Quick logging in QQ %s…" uin) nil)
    (qq-login--request
     session
     (lambda (success failure)
       (qq-gateway-account-login-quick
        (alist-get 'account_id account) uin nil success failure))
     nil
     #'qq-login--quick-error)))

(defun qq-login--captcha (session account challenge)
  "Read CAPTCHA proof for SESSION ACCOUNT and CHALLENGE."
  (setf (qq-login--session-prompting-p session) t)
  (unwind-protect
      (let* ((account-id (alist-get 'account_id account))
             (challenge-id (alist-get 'challenge_id challenge))
             (_opened
              (qq-login--open-captcha-url (alist-get 'url challenge)))
             (ticket (read-passwd "Captcha ticket: "))
             (rand-str (read-string "Captcha randStr: "))
             (sid (read-string "Captcha sid: " (alist-get 'sid challenge))))
        (unwind-protect
            (progn
              (setf (qq-login--session-retry-failed-p session) nil
                    (qq-login--session-submitted-challenge-id session)
                    (copy-sequence challenge-id))
              (qq-login--present session "Submitting CAPTCHA proof…" nil)
              (qq-login--request
               session
               (lambda (success failure)
                 (qq-gateway-account-login-captcha
                  account-id challenge-id ticket rand-str sid success failure))))
          (clear-string ticket)))
    (when (qq-login--session-p session)
      (setf (qq-login--session-prompting-p session) nil))))

(defun qq-login--qrencode ()
  "Return the executable used for QR rendering, or signal a user error."
  (or (and (stringp qq-login-qrencode-program)
           (executable-find qq-login-qrencode-program))
      (user-error
       "qq: install qrencode or customize `qq-login-qrencode-program'")))

(defun qq-login--render-qr (url)
  "Return (DISPLAY . FILE) for the scannable QQ URL."
  (let ((program (qq-login--qrencode)))
    (if (and (display-graphic-p)
             (image-type-available-p 'png))
        (let ((file (make-temp-file "emacs-qq-login-qr-" nil ".png")))
          (unless (eq 0
                      (call-process
                       program nil nil nil "-m" "1" "-s" "8" "-t" "PNG"
                       "-o" file url))
            (delete-file file)
            (user-error "qq: qrencode could not render the login QR code"))
          (cons
           (propertize
            " "
            'display
            (create-image
             file 'png nil :ascent 'center
             :width qq-login-qr-image-size
             :height qq-login-qr-image-size))
           file))
      (with-temp-buffer
        (unless (eq 0
                    (call-process
                     program nil t nil "-m" "1" "-t" "UTF8" url))
          (user-error "qq: qrencode could not render the login QR code"))
        (cons (buffer-string) nil)))))

(defun qq-login--prepare-qr (session url)
  "Render URL and install its presentation into SESSION."
  (unless (equal url (qq-login--session-qr-url session))
    (pcase-let ((`(,display . ,file) (qq-login--render-qr url)))
      (qq-login--clear-qr session)
      (setf (qq-login--session-qr-url session) (copy-sequence url)
            (qq-login--session-qr-display session) display
            (qq-login--session-qr-file session) file)
      (qq-login--changed))))

(defun qq-login-view-model ()
  "Return the current foreground login presentation, or nil."
  (when (qq-login-active-p)
    (list
     :account-id (copy-sequence
                  (qq-login--session-account-id qq-login--current))
     :status (copy-sequence
              (or (qq-login--session-status qq-login--current)
                  "Preparing QQ login…"))
     :error (and (qq-login--session-error qq-login--current)
                 (copy-sequence
                  (qq-login--session-error qq-login--current)))
     :display (and (qq-login--session-qr-display qq-login--current)
                   (copy-sequence
                    (qq-login--session-qr-display qq-login--current))))))

(defun qq-login-insert-view (model)
  "Insert foreground login presentation MODEL into the current buffer."
  (insert
   (propertize
    (concat (or (plist-get model :status) "QQ login…") "\n")
    'face 'bold))
  (when-let* ((error-text (plist-get model :error)))
    (insert (propertize (concat error-text "\n") 'face 'error)))
  (when-let* ((display (plist-get model :display)))
    (insert display)
    (unless (bolp)
      (insert "\n"))
    (insert
     (propertize "Scan with mobile QQ to verify this new device.\n"
                 'face 'bold)
     "After scanning, confirm the login on your phone.  "
     "The native service will continue automatically.\n"))
  (when-let* ((account-id (plist-get model :account-id)))
    (insert (format "Managed account: %s\n" account-id))))

(defun qq-login--new-device (session account challenge)
  "Display Rust-owned new-device verification for SESSION ACCOUNT CHALLENGE."
  (ignore account)
  (let ((qr-url (alist-get 'qr_url challenge)))
    (unless (qq-gateway--non-empty-string-p qr-url)
      (error "qq: new-device challenge has no scannable qr_url"))
    (qq-login--prepare-qr session qr-url)
    (setf (qq-login--session-retry-failed-p session) nil)
    (qq-login--present session "Waiting for mobile QQ confirmation…" nil)))

(defun qq-login--unusual-device (session account challenge)
  "Read unusual-device proof for SESSION ACCOUNT and CHALLENGE."
  (setf (qq-login--session-prompting-p session) t)
  (unwind-protect
      (let* ((account-id (alist-get 'account_id account))
             (challenge-id (alist-get 'challenge_id challenge))
             (device-sig (read-passwd "Device verification signature (hex): ")))
        (unwind-protect
            (progn
              (setf (qq-login--session-retry-failed-p session) nil
                    (qq-login--session-submitted-challenge-id session)
                    (copy-sequence challenge-id))
              (qq-login--present
               session "Submitting device verification…" nil)
              (qq-login--request
               session
               (lambda (success failure)
                 (qq-gateway-account-login-unusual-device
                  account-id challenge-id device-sig success failure))))
          (clear-string device-sig)))
    (when (qq-login--session-p session)
      (setf (qq-login--session-prompting-p session) nil))))

(defun qq-login--challenge (session account challenge)
  "Continue SESSION ACCOUNT using projected CHALLENGE."
  (pcase (alist-get 'kind challenge)
    ("new_device" (qq-login--new-device session account challenge))
    ((or "captcha" "unusual_device")
     (if (equal (alist-get 'challenge_id challenge)
                (qq-login--session-submitted-challenge-id session))
         (qq-login--present session "Waiting for QQ login to continue…" nil)
       (pcase (alist-get 'kind challenge)
         ("captcha" (qq-login--captcha session account challenge))
         ("unusual_device"
          (qq-login--unusual-device session account challenge)))))
    (kind (error "qq: unsupported login challenge kind %S" kind))))

(defun qq-login--start-account (session account)
  "Start native runtime for SESSION ACCOUNT."
  (setf (qq-login--session-retry-failed-p session) nil)
  (qq-login--present session "Starting native QQ account…" nil)
  (qq-login--request
   session
   (lambda (success failure)
     (qq-gateway-account-start
      (alist-get 'account_id account) success failure))))

(defun qq-login--drive-ready (session)
  "Progress SESSION while the Gateway is ready."
  (when (qq-login--resolve-account-choice session)
    (when-let* ((account (qq-login--ensure-account session)))
      (let ((phase (alist-get 'phase account))
            (challenge (alist-get 'challenge account)))
        (when (and (qq-login--session-qr-display session)
                   (not (and (equal phase "logging_in")
                             (equal (alist-get 'kind challenge)
                                    "new_device"))))
          (qq-login--clear-qr session)
          (qq-login--changed))
        (pcase phase
          ((or "stopped" "logged_out" "failed")
           (if (qq-login--session-retry-failed-p session)
               (qq-login--start-account session account)
             (let* ((problem (alist-get 'problem account))
                    (code (alist-get 'code problem))
                    (message-text (alist-get 'message problem)))
               (qq-login--present
                session
                "QQ login is not running."
                (cond
                 ((and code message-text)
                  (format "[%s] %s" code message-text))
                 (message-text message-text)
                 (t "Run `M-x qq` again to retry."))))))
          ("login_required"
           (if (qq-login--session-quick-login-uin session)
               (qq-login--quick session account)
             (qq-login--password session account)))
          ("logging_in"
           (if challenge
               (qq-login--challenge session account challenge)
             (qq-login--present
              session
              (if (qq-login--session-qr-display session)
                  "Waiting for mobile QQ confirmation…"
                "QQ login is continuing in the native service…")
              nil)))
          ("online"
           (qq-login--finish
            session
            (format "qq: account %s is online"
                    (or (alist-get 'uin account)
                        (alist-get 'account_id account)))))
          ("starting"
           (qq-login--present session "Starting native QQ account…" nil))
          ("stopping"
           (qq-login--present session "Stopping native QQ account…" nil))
          (_ (error "qq: unsupported account phase %S" phase)))))))

(defun qq-login--drive (session)
  "Progress current foreground authorization SESSION by one state."
  (when (and (qq-login--current-p session)
             (not (qq-login--session-in-flight-p session))
             (not (qq-login--session-prompting-p session))
             (qq-gateway-transport-ready-p))
    (condition-case error-data
        (qq-login--drive-ready session)
      (quit
       (qq-login--finish
        session
        (format "qq: login interaction cancelled: %s"
                (error-message-string error-data))))
      (error
       (qq-login--request-error
        session nil (error-message-string error-data))))))

(defun qq-login--projection-changed (&rest _arguments)
  "Continue the current login after account projection changes."
  (when (qq-login-active-p)
    (qq-login--schedule qq-login--current)))

(defun qq-login--start (account-id create-p label label-read-p)
  "Start foreground login for ACCOUNT-ID or a new account.

CREATE-P requests a new managed slot with optional LABEL.  LABEL-READ-P means
the caller has already made the optional label choice, including choosing nil."
  (if (and (qq-login-active-p)
           (not create-p)
           (or (null account-id)
               (equal account-id
                      (qq-login--session-account-id qq-login--current))))
      (let ((session qq-login--current))
        (if (qq-login--session-in-flight-p session)
            (message "qq: %s"
                     (or (qq-login--session-status session)
                         "login request is still running"))
          (setf (qq-login--session-retry-failed-p session) t
                (qq-login--session-submitted-challenge-id session) nil)
          (qq-login--present session "Continuing QQ login…" nil)
          (qq-login--schedule session))
        session)
    (when (qq-login-active-p)
      (qq-login-cancel))
    (let ((online-account
           (and (not create-p)
                (qq-gateway-transport-ready-p)
                (if account-id
                    (qq-gateway-account account-id)
                  (qq-gateway-current-account)))))
      (if (and online-account
               (equal (alist-get 'phase online-account) "online"))
          (progn
            (when account-id
              (qq-gateway--set-current-account account-id))
            (message "qq: account %s is already online"
                     (or (alist-get 'uin online-account)
                         (alist-get 'account_id online-account)))
            nil)
        (let ((session
               (qq-login--session-create
                :active-p t
                :account-id (and account-id (copy-sequence account-id))
                :create-p create-p
                :label label
                :label-read-p label-read-p
                :login-accounts-loaded-p nil
                :login-accounts nil
                :quick-login-uin nil
                :retry-failed-p t
                :status "Preparing QQ login…")))
          (setq qq-login--current session)
          (qq-login--changed)
          (condition-case error-data
              (progn
                (unless (qq-native-running-p)
                  (qq-login--present
                   session "Connecting to native QQ service…" nil)
                  (qq-native-connect))
                (qq-login--schedule session)
                session)
            (error
             (qq-login--request-error
              session nil (error-message-string error-data))
             session)))))))

;;;###autoload
(defun qq-login (&optional account-id)
  "Log in or continue authorization for managed ACCOUNT-ID.

When ACCOUNT-ID is nil, continue the selected active runtime or offer managed
accounts, every Gateway EasyLogin identity, and `New account'.  An online
managed account is selected without logging in again.  An EasyLogin identity
reuses a matching managed slot or creates one as needed.  The command then
follows projected account phases until the account is online."
  (interactive)
  (qq-login--start account-id nil nil nil))

;;;###autoload
(defun qq-login-new-account (label)
  "Create a managed account with optional LABEL and run its login flow."
  (interactive (list (qq-login--read-label)))
  (qq-login--start nil t label t))

(add-hook 'qq-gateway-ready-hook #'qq-login--projection-changed t)
(add-hook 'qq-gateway-accounts-changed-hook #'qq-login--projection-changed t)

(provide 'qq-login)

;;; qq-login.el ends here
