;;; qq-gateway-attachment-test.el --- Tests for prepared attachments -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-attachment)

(defconst qq-gateway-attachment-test-id
  "att-11111111-2222-4333-8444-555555555555")

(defconst qq-gateway-attachment-test-record-id
  "att-11111111-2222-4333-8444-555555555556")

(cl-defun qq-gateway-attachment-test-snapshot
    (&key
     (attachment-id qq-gateway-attachment-test-id)
     (resource-id "res-image-a")
     (account-id "slot-a")
     (conversation '((kind . "group") (group_uin . "8209413637")))
     (use '((kind . "image") (summary . "[图片]") (sub_type . 0)))
     (phase "queued")
     (bytes-done "0")
     (bytes-total "3")
     fast-path
     (created-at 1784700000)
     (updated-at 1784700000)
     error)
  "Return one closed Prepared Attachment snapshot fixture."
  `((attachment_id . ,attachment-id)
    (resource_id . ,resource-id)
    (account_id . ,account-id)
    (conversation . ,(copy-tree conversation))
    (use . ,(copy-tree use))
    (phase . ,phase)
    (bytes_done . ,bytes-done)
    (bytes_total . ,bytes-total)
    (fast_path . ,fast-path)
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (error . ,error)))

(defun qq-gateway-attachment-test-ready (&optional attachment-id resource-id)
  "Return one fast-path ready fixture."
  (qq-gateway-attachment-test-snapshot
   :attachment-id (or attachment-id qq-gateway-attachment-test-id)
   :resource-id (or resource-id "res-image-a")
   :phase "ready" :fast-path t :updated-at 1784700001))

(defun qq-gateway-attachment-test-ready-record ()
  "Return one fast-path ready native-record fixture."
  (qq-gateway-attachment-test-snapshot
   :attachment-id qq-gateway-attachment-test-record-id
   :resource-id "res-record-a"
   :use '((kind . "record"))
   :phase "ready" :fast-path t :updated-at 1784700001))

(defun qq-gateway-attachment-test-account (&optional phase)
  "Return one selected account fixture in PHASE, normally online."
  `((account_id . "slot-a")
    (label . "Primary")
    (phase . ,(or phase "online"))
    (uin . "10002")
    (uid . "u_self")
    (challenge)
    (problem)))

(defmacro qq-gateway-attachment-test-with-state (&rest body)
  "Run BODY with isolated account, resource, and attachment projections."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway--refresh-owner nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-gateway-attachment--attachments (make-hash-table :test #'equal))
         (qq-gateway-attachment--order nil)
         (qq-gateway-attachment--gateway-instance-id nil)
         (qq-gateway-attachment--refresh-owner nil)
         (qq-gateway-attachment--resync-request-id nil)
         (qq-gateway-attachment-changed-hook nil)
         (qq-gateway-attachment-desync-hook nil)
         (qq-gateway-resource--resources (make-hash-table :test #'equal))
         (qq-gateway-resource--order nil)
         (qq-gateway-resource--gateway-instance-id nil)
         (qq-gateway-resource--refresh-owner nil)
         (qq-gateway-resource--resync-request-id nil)
         (qq-gateway-resource-changed-hook nil)
         (qq-gateway-resource-desync-hook nil))
     (qq-gateway--replace-accounts
      (list (qq-gateway-attachment-test-account)) 'test "gateway-test")
     (qq-gateway-account-select "slot-a")
     ,@body))

(ert-deftest qq-gateway-attachment-validator-is-closed-and-pathless ()
  (let ((snapshot (qq-gateway-attachment-test-ready)))
    (should (qq-gateway-attachment--validate-snapshot snapshot))
    (push '(path . "/tmp/private.png") snapshot)
    (should-error (qq-gateway-attachment--validate-snapshot snapshot))
    (setq snapshot (qq-gateway-attachment-test-ready))
    (push '(generation . "7") snapshot)
    (should-error (qq-gateway-attachment--validate-snapshot snapshot)))
  (let ((record (qq-gateway-attachment-test-ready-record)))
    (should (qq-gateway-attachment--validate-snapshot record))
    (setf (alist-get 'use record)
          '((kind . "record") (summary . "must-not-exist")))
    (should-error (qq-gateway-attachment--validate-snapshot record))))

(ert-deftest qq-gateway-attachment-private-conversation-is-uin-only ()
  (let* ((snapshot
          (qq-gateway-attachment-test-snapshot
           :conversation '((kind . "private") (peer_uin . "10001"))))
         (validated (qq-gateway-attachment--validate-snapshot snapshot)))
    (should
     (equal (alist-get 'conversation validated)
            '((kind . "private") (peer_uin . "10001")))))
  (dolist
      (conversation
       '(((kind . "private") (peer_uid . "u_native"))
         ((kind . "private") (peer_uin . "10001") (peer_uid . "u_native"))
         ((kind . "private") (peer_uin . "10001") (native_hint . "u_native"))
         ((kind . "private") (peer_uin . "010001"))))
    (should-error
     (qq-gateway-attachment--validate-snapshot
      (qq-gateway-attachment-test-snapshot
       :conversation conversation)))))

(ert-deftest qq-gateway-attachment-conversation-uins-are-bounded-uint64 ()
  (dolist
      (conversation
       '(((kind . "private") (peer_uin . "18446744073709551615"))
         ((kind . "group") (group_uin . "18446744073709551615"))))
    (should (qq-gateway-attachment--validate-conversation conversation)))
  (dolist (uin '("0" "010001" "18446744073709551616"))
    (dolist (kind '((private . peer_uin) (group . group_uin)))
      (should-error
       (qq-gateway-attachment--validate-conversation
        `((kind . ,(symbol-name (car kind))) (,(cdr kind) . ,uin)))))))

(ert-deftest qq-gateway-attachment-progress-is-an-exact-uint64 ()
  (should
   (qq-gateway-attachment--validate-snapshot
    (qq-gateway-attachment-test-snapshot
     :bytes-done "18446744073709551615"
     :bytes-total "18446744073709551615")))
  (should
   (qq-gateway-attachment--validate-snapshot
    (qq-gateway-attachment-test-snapshot
     :bytes-done "0" :bytes-total "0")))
  (dolist (value '("00" "18446744073709551616"))
    (should-error
     (qq-gateway-attachment--validate-snapshot
      (qq-gateway-attachment-test-snapshot :bytes-total value)))))

(ert-deftest qq-gateway-attachment-conversation-params-require-uint64-uin ()
  (should
   (equal
    (qq-gateway-attachment--conversation-params
     "group:18446744073709551615")
    '((kind . "group") (group_uin . "18446744073709551615"))))
  (dolist (session-key
           '("private:0" "private:010001" "group:18446744073709551616"))
    (should-error
     (qq-gateway-attachment--conversation-params session-key)
     :type 'user-error)))

(ert-deftest qq-gateway-attachment-validator-rejects-phase-contradictions ()
  (let ((ready (qq-gateway-attachment-test-ready))
        (uploaded
         (qq-gateway-attachment-test-snapshot
          :phase "ready" :bytes-done "3" :fast-path :false
          :updated-at 1784700001))
        (failed
         (qq-gateway-attachment-test-snapshot
          :phase "failed" :updated-at 1784700001
          :error '((code . "upload_failed") (message . "no route")))))
    (should (qq-gateway-attachment--validate-snapshot ready))
    (should (qq-gateway-attachment--validate-snapshot uploaded))
    (should (qq-gateway-attachment--validate-snapshot failed))
    (setf (alist-get 'bytes_done uploaded) "2")
    (should-error (qq-gateway-attachment--validate-snapshot uploaded))))

(ert-deftest qq-gateway-attachment-validator-normalizes-wire-null ()
  (let ((snapshot (qq-gateway-attachment-test-snapshot)))
    (setf (alist-get 'fast_path snapshot) qq-gateway-wire-null
          (alist-get 'error snapshot) qq-gateway-wire-null)
    (let ((validated (qq-gateway-attachment--validate-snapshot snapshot)))
      (should (null (alist-get 'fast_path validated nil nil #'eq)))
      (should (null (alist-get 'error validated nil nil #'eq)))
      (should-not
       (qq-gateway-wire-null-p
        (alist-get 'fast_path validated nil nil #'eq)))
      (should-not
       (qq-gateway-wire-null-p
        (alist-get 'error validated nil nil #'eq)))))
  (let ((snapshot (qq-gateway-attachment-test-snapshot)))
    (setf (alist-get 'fast_path snapshot) [])
    (should-error (qq-gateway-attachment--validate-snapshot snapshot)))
  (let ((snapshot (qq-gateway-attachment-test-snapshot)))
    (setf (alist-get 'error snapshot) [])
    (should-error (qq-gateway-attachment--validate-snapshot snapshot))))

(ert-deftest qq-gateway-attachment-list-result-accepts-wire-array-only ()
  (let ((snapshot (qq-gateway-attachment-test-snapshot)))
    (setf (alist-get 'fast_path snapshot) qq-gateway-wire-null
          (alist-get 'error snapshot) qq-gateway-wire-null)
    (let ((attachments
           (qq-gateway-attachment--validate-list-result
            `((attachments . ,(vector snapshot))))))
      (should (proper-list-p attachments))
      (should (= (length attachments) 1))
      ;; The outer array is normalized here; nested wire values stay distinct
      ;; until the snapshot validator accepts their nullable fields.
      (should
       (qq-gateway-wire-null-p
        (alist-get 'fast_path (car attachments) nil nil #'eq)))
      (let ((validated
             (qq-gateway-attachment--validate-snapshot (car attachments))))
        (should (null (alist-get 'fast_path validated nil nil #'eq)))
        (should (null (alist-get 'error validated nil nil #'eq))))))
  (should-error
   (qq-gateway-attachment--validate-list-result
    `((attachments . ,qq-gateway-wire-null)))))

(ert-deftest qq-gateway-attachment-registry-owns-nested-strings ()
  (qq-gateway-attachment-test-with-state
    (let* ((summary (copy-sequence "photo"))
           (snapshot
            (qq-gateway-attachment-test-snapshot
             :use `((kind . "image")
                    (summary . ,summary)
                    (sub_type . 0)))))
      (qq-gateway-attachment--upsert snapshot 'test)
      (aset summary 0 ?X)
      (should
       (equal
        (alist-get 'summary
                   (alist-get
                    'use
                    (qq-gateway-attachment qq-gateway-attachment-test-id)))
        "photo"))
      (let* ((public
              (qq-gateway-attachment qq-gateway-attachment-test-id))
             (public-summary
              (alist-get 'summary (alist-get 'use public))))
        (aset public-summary 0 ?Y)
        (should
         (equal
          (alist-get 'summary
                     (alist-get
                      'use
                      (qq-gateway-attachment qq-gateway-attachment-test-id)))
          "photo"))))))

(ert-deftest qq-gateway-attachment-events-never-regress-progress-or-terminal-state ()
  (qq-gateway-attachment-test-with-state
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot) 'queued)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot
      :phase "negotiating" :updated-at 1784700001)
     'negotiating)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot
      :phase "uploading" :bytes-done "2" :updated-at 1784700002)
     'progress)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot
      :phase "uploading" :bytes-done "1" :updated-at 1784700003)
     'late-progress)
    (should (equal
             (alist-get 'bytes_done
                        (qq-gateway-attachment qq-gateway-attachment-test-id))
             "2"))
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot
      :phase "ready" :fast-path t :updated-at 1784700004)
     'ready)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot) 'late-queued)
    (should (equal
             (alist-get 'phase
                        (qq-gateway-attachment qq-gateway-attachment-test-id))
             "ready"))))

(ert-deftest qq-gateway-attachment-identical-event-notifies-once ()
  (qq-gateway-attachment-test-with-state
    (let (changes)
      (add-hook 'qq-gateway-attachment-changed-hook
                (lambda (reason attachment-id)
                  (push (list reason attachment-id) changes)))
      (dotimes (_ 2)
        (qq-gateway-attachment--handle-event
         "attachment.changed"
         `((attachment . ,(qq-gateway-attachment-test-snapshot)))))
      (should
       (equal changes
              `((changed ,qq-gateway-attachment-test-id)))))))

(ert-deftest qq-gateway-attachment-ready-uses-typed-single-flight-resync ()
  (qq-gateway-attachment-test-with-state
    (let ((capabilities '("attachment.list")) calls)
      (cl-letf (((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () capabilities))
                ((symbol-function 'qq-gateway-attachment-refresh)
                 (lambda (_callback _errback reason &optional _owner)
                   (push reason calls)
                   "attachment-list-request")))
        (qq-gateway-attachment--handle-ready "gateway-a")
        (qq-gateway-attachment--handle-ready "gateway-a")
        (should (equal calls '(ready)))
        (should (equal qq-gateway-attachment--gateway-instance-id
                       "gateway-a"))
        (should (equal (car qq-gateway-attachment--resync-request-id)
                       'attachment-resync))
        (setq capabilities nil)
        (qq-gateway-attachment--handle-ready "gateway-b")
        (should (equal qq-gateway-attachment--gateway-instance-id
                       "gateway-b"))
        (should-not qq-gateway-attachment--resync-request-id)
        (should (equal calls '(ready)))))))

(ert-deftest qq-gateway-attachment-newest-refresh-owns-full-replacement ()
  (qq-gateway-attachment-test-with-state
    (let (requests old-callback old-error new-callback)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("attachment.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "attachment-request-%d"
                                   (length requests))))))
        (qq-gateway-attachment-refresh
         (lambda (_) (setq old-callback t))
         (lambda (body _failure) (setq old-error body)))
        (qq-gateway-attachment-refresh
         (lambda (_) (setq new-callback t)) #'ignore)
        (funcall
         (car (nth 1 requests))
         `((attachments .
            [,(qq-gateway-attachment-test-snapshot
               :attachment-id qq-gateway-attachment-test-record-id
               :resource-id "res-new")])))
        (funcall
         (car (nth 0 requests))
         `((attachments .
            [,(qq-gateway-attachment-test-snapshot
               :resource-id "res-old")])))
        (should new-callback)
        (should-not old-callback)
        (should (equal (alist-get 'code old-error) "superseded_request"))
        (should (qq-gateway-attachment
                 qq-gateway-attachment-test-record-id))
        (should-not (qq-gateway-attachment qq-gateway-attachment-test-id))
        (should-not qq-gateway-attachment--refresh-owner)))))

(ert-deftest qq-gateway-attachment-reset-cancels-pending-refresh ()
  (qq-gateway-attachment-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("attachment.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'attachment-refresh-token))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-gateway-attachment-refresh
         nil (lambda (body _reason) (push body failures)))
        (qq-gateway-attachment-reset)
        (should (equal canceled '(attachment-refresh-token)))
        (should (= (length failures) 1))
        (should-not qq-gateway-attachment--refresh-owner)
        (funcall late-success
                 `((attachments .
                    [,(qq-gateway-attachment-test-snapshot)])))
        (should (= (length failures) 1))
        (should-not (qq-gateway-attachments))))))

(ert-deftest qq-gateway-attachment-prepare-binds-stable-account-and-chat ()
  (qq-gateway-attachment-test-with-state
    (puthash "res-image-a"
             '((resource_id . "res-image-a") (phase . "ready"))
             qq-gateway-resource--resources)
    (let (method params delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("attachment.prepare")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (wire-method wire-params success _failure
                                      &optional _early)
                   (setq method wire-method params wire-params)
                   (funcall success
                            `((attachment
                               . ,(qq-gateway-attachment-test-snapshot))))
                   "prepare-request")))
        (should
         (equal
          (qq-gateway-attachment-prepare-image
           "group:8209413637" "res-image-a" "[图片]" 0
           (lambda (snapshot) (setq delivered snapshot)))
          "prepare-request"))
        (should (equal method "attachment.prepare"))
        (should (equal (alist-get 'account_id params) "slot-a"))
        (should (equal (alist-get 'resource_id params) "res-image-a"))
        (should (equal (alist-get 'conversation params)
                       '((kind . "group") (group_uin . "8209413637"))))
        (should-not (assq 'generation params))
        (should-not (assq 'generation delivered))))))

(ert-deftest qq-gateway-attachment-prepare-record-sends-only-closed-use ()
  (qq-gateway-attachment-test-with-state
    (puthash "res-record-a"
             '((resource_id . "res-record-a") (phase . "ready"))
             qq-gateway-resource--resources)
    (let (method params delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p) (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("attachment.prepare")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (wire-method wire-params success _failure
                                      &optional _early)
                   (setq method wire-method params wire-params)
                   (funcall
                    success
                    `((attachment
                       . ,(qq-gateway-attachment-test-snapshot
                           :attachment-id qq-gateway-attachment-test-record-id
                           :resource-id "res-record-a"
                           :use '((kind . "record"))))))
                   "prepare-record-request")))
        (should
         (equal
          (qq-gateway-attachment-prepare-record
           "group:8209413637" "res-record-a"
           (lambda (snapshot) (setq delivered snapshot)))
          "prepare-record-request"))
        (should (equal method "attachment.prepare"))
        (should (equal (alist-get 'use params) '((kind . "record"))))
        (should (equal (alist-get 'use delivered) '((kind . "record"))))))))

(ert-deftest qq-gateway-attachment-sendable-checks-account-and-conversation ()
  (qq-gateway-attachment-test-with-state
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-ready) 'ready)
    (should
     (equal
      (qq-gateway-attachment-assert-sendable
       qq-gateway-attachment-test-id "group:8209413637" "slot-a")
      qq-gateway-attachment-test-id))
    ;; Runtime identity pairs are not part of the client contract.
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-id "group:8209413637" '("slot-a" . "ignored"))
     :type 'user-error)
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-id "group:8209413637" "slot-b")
     :type 'user-error)
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-id "group:10001" "slot-a")
     :type 'user-error)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-ready-record) 'ready-record)
    (should
     (equal
      (qq-gateway-attachment-assert-sendable
       qq-gateway-attachment-test-record-id
       "group:8209413637" "slot-a" "record")
      qq-gateway-attachment-test-record-id))
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-record-id
      "group:8209413637" "slot-a")
     :type 'user-error)))

(ert-deftest qq-gateway-attachment-await-observer-is-explicitly-cancellable ()
  (qq-gateway-attachment-test-with-state
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-snapshot
      :phase "negotiating" :updated-at 1784700001)
     'negotiating)
    (let ((called nil)
          (cancel
           (qq-gateway-attachment--await
            qq-gateway-attachment-test-id
            (lambda (_) (setq called t))
            (lambda (&rest _) (setq called t)))))
      (should (= (length qq-gateway-attachment-changed-hook) 1))
      (funcall cancel)
      (should-not qq-gateway-attachment-changed-hook)
      (qq-gateway-attachment--upsert
       (qq-gateway-attachment-test-snapshot
        :phase "ready" :fast-path t :updated-at 1784700002)
       'ready)
      (should-not called))))

(ert-deftest qq-gateway-attachment-stage-and-prepare-settles-synchronously ()
  (qq-gateway-attachment-test-with-state
    (let ((path (make-temp-file "qq-image-" nil ".png" "abc"))
          delivered canceled
          (resource-observer-revocations 0)
          (attachment-observer-revocations 0))
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-gateway-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (puthash "res-image-a"
                           '((resource_id . "res-image-a") (phase . "ready"))
                           qq-gateway-resource--resources)
                  (funcall success
                           '((resource_id . "res-image-a") (phase . "staging")))
                  "stage-request"))
               ((symbol-function 'qq-gateway-attachment--await-resource)
                (lambda (resource-id callback _errback)
                  (funcall callback
                           `((resource_id . ,resource-id) (phase . "ready")))
                  (lambda ()
                    (cl-incf resource-observer-revocations)
                    nil)))
               ((symbol-function 'qq-gateway-attachment--await)
                (lambda (_attachment-id callback _errback)
                  (funcall callback (qq-gateway-attachment-test-ready))
                  (lambda ()
                    (cl-incf attachment-observer-revocations)
                    nil)))
               ((symbol-function 'qq-gateway-attachment-prepare-image)
                (lambda (_session _resource _summary _sub success _failure)
                  (let ((ready (qq-gateway-attachment-test-ready)))
                    (puthash qq-gateway-attachment-test-id ready
                             qq-gateway-attachment--attachments)
                    (setq qq-gateway-attachment--order
                          (list qq-gateway-attachment-test-id))
                    (funcall success
                             (qq-gateway-attachment-test-snapshot)))
                  "prepare-request"))
               ((symbol-function 'qq-gateway-transport-cancel)
                (lambda (request-id) (push request-id canceled) t)))
            (let ((operation
                   (qq-gateway-attachment-stage-and-prepare-image
                    "group:8209413637" path nil nil
                    (lambda (snapshot) (setq delivered snapshot)) #'ignore)))
              (should (qq-gateway-attachment-operation-p operation))
              (should-not
               (qq-gateway-attachment-operation-active-p operation))
              (should (equal (alist-get 'phase delivered) "ready"))
              (should (equal canceled '("stage-request" "prepare-request")))
              (should (= resource-observer-revocations 2))
              (should (= attachment-observer-revocations 1))))
        (delete-file path)))))

(ert-deftest qq-gateway-attachment-record-releases-source-after-derivation ()
  (qq-gateway-attachment-test-with-state
    (let ((path (make-temp-file "qq-record-" nil ".wav" "pcm"))
          derived-source prepared-resource delivered released)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-gateway-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (puthash "res-record-source"
                           '((resource_id . "res-record-source")
                             (phase . "ready"))
                           qq-gateway-resource--resources)
                  (funcall success
                           '((resource_id . "res-record-source")
                             (phase . "staging")))
                  "stage-request"))
               ((symbol-function 'qq-gateway-resource-derive-record)
                (lambda (source-id _name success _failure)
                  (setq derived-source source-id)
                  (puthash "res-record-a"
                           '((resource_id . "res-record-a") (phase . "ready"))
                           qq-gateway-resource--resources)
                  (funcall success
                           '((resource_id . "res-record-a")
                             (phase . "staging")))
                  "derive-request"))
               ((symbol-function 'qq-gateway-resource-release)
                (lambda (resource-id &rest _)
                  (push resource-id released)))
               ((symbol-function 'qq-gateway-attachment-prepare-record)
                (lambda (_session resource-id success _failure)
                  (setq prepared-resource resource-id)
                  (let ((ready (qq-gateway-attachment-test-ready-record)))
                    (puthash qq-gateway-attachment-test-record-id ready
                             qq-gateway-attachment--attachments)
                    (funcall success
                             (qq-gateway-attachment-test-snapshot
                              :attachment-id
                              qq-gateway-attachment-test-record-id
                              :resource-id "res-record-a"
                              :use '((kind . "record")))))
                  "prepare-request")))
            (let ((operation
                   (qq-gateway-attachment-stage-and-prepare-record
                    "group:8209413637" path
                    (lambda (snapshot) (setq delivered snapshot)) #'ignore)))
              (should-not
               (qq-gateway-attachment-operation-active-p operation))
              (should (equal derived-source "res-record-source"))
              (should (equal prepared-resource "res-record-a"))
              (should (equal released '("res-record-source")))
              (should (equal (alist-get 'phase delivered) "ready"))))
        (delete-file path)))))

(ert-deftest qq-gateway-attachment-record-cancel-during-derivation-releases-source ()
  (qq-gateway-attachment-test-with-state
    (let ((path (make-temp-file "qq-record-cancel-" nil ".wav" "pcm"))
          canceled released)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-gateway-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (puthash "res-record-source"
                           '((resource_id . "res-record-source")
                             (phase . "ready"))
                           qq-gateway-resource--resources)
                  (funcall success '((resource_id . "res-record-source")))
                  "stage-request"))
               ((symbol-function 'qq-gateway-resource-derive-record)
                (lambda (_source _name _success _failure) "derive-request"))
               ((symbol-function 'qq-gateway-transport-cancel)
                (lambda (request-id) (setq canceled request-id)))
               ((symbol-function 'qq-gateway-resource-release)
                (lambda (resource-id &rest _) (push resource-id released))))
            (let ((operation
                   (qq-gateway-attachment-stage-and-prepare-record
                    "private:10001" path nil #'ignore)))
              (should (qq-gateway-attachment-operation-active-p operation))
              (should (qq-gateway-attachment-cancel-operation operation))
              (should (equal canceled "derive-request"))
              (should (equal released '("res-record-source")))))
        (delete-file path)))))

(ert-deftest qq-gateway-attachment-preparation-survives-runtime-restart ()
  (qq-gateway-attachment-test-with-state
    (let ((path (make-temp-file "qq-image-owner-" nil ".png" "abc"))
          delivered failure released-resource released-attachment)
      (unwind-protect
          (cl-letf
              (((symbol-function 'qq-gateway-resource-stage-local)
                (lambda (_path _name _digest success _failure)
                  (funcall success '((resource_id . "res-image-a")))
                  "stage-request"))
               ((symbol-function 'qq-gateway-attachment--await-resource)
                (lambda (_resource-id callback _errback)
                  (funcall callback
                           '((resource_id . "res-image-a") (phase . "ready")))
                  #'ignore))
               ((symbol-function 'qq-gateway-attachment-prepare-image)
                (lambda (_session _resource _summary _sub success _failure)
                  (let ((queued (qq-gateway-attachment-test-snapshot)))
                    (qq-gateway-attachment--upsert queued 'prepare)
                    (funcall success queued))
                  "prepare-request"))
               ((symbol-function 'qq-gateway-resource-release)
                (lambda (resource-id &rest _)
                  (setq released-resource resource-id)))
               ((symbol-function 'qq-gateway-attachment-release)
                (lambda (attachment-id &rest _)
                  (setq released-attachment attachment-id))))
            (let ((operation
                   (qq-gateway-attachment-stage-and-prepare-image
                    "group:8209413637" path nil nil
                    (lambda (snapshot) (setq delivered snapshot))
                    (lambda (body reason) (setq failure (list body reason))))))
              (should (qq-gateway-attachment-operation-active-p operation))
              ;; A stop/start cycle replaces the same stable account slot.
              ;; Attachment ownership remains service-side and opaque.
              (qq-gateway--upsert-account
               (qq-gateway-attachment-test-account "stopped") 'changed)
              (qq-gateway--upsert-account
               (qq-gateway-attachment-test-account "starting") 'changed)
              (qq-gateway--upsert-account
               (qq-gateway-attachment-test-account "online") 'changed)
              (qq-gateway-attachment--upsert
               (qq-gateway-attachment-test-snapshot
                :phase "negotiating" :updated-at 1784700001)
               'negotiating)
              (qq-gateway-attachment--upsert
               (qq-gateway-attachment-test-snapshot
                :phase "ready" :fast-path t :updated-at 1784700002)
               'ready)
              (should delivered)
              (should-not
               (qq-gateway-attachment-operation-active-p operation))
              (should-not failure)
              (should-not released-resource)
              (should-not released-attachment))
        (delete-file path))))))

(ert-deftest qq-gateway-attachment-cancel-revokes-local-work-and-created-objects ()
  (let ((operation
         (qq-gateway-attachment-operation-create
          :active-p t :request-id "request-a"
          :resource-id "res-image-a"
          :attachment-id qq-gateway-attachment-test-id
          :resource-wait-cancel #'ignore
          :attachment-wait-cancel #'ignore))
        canceled released-resource released-attachment)
    (cl-letf (((symbol-function 'qq-gateway-transport-cancel)
               (lambda (request-id) (setq canceled request-id)))
              ((symbol-function 'qq-gateway-resource-release)
               (lambda (resource-id &rest _)
                 (setq released-resource resource-id)))
              ((symbol-function 'qq-gateway-attachment-release)
               (lambda (attachment-id &rest _)
                 (setq released-attachment attachment-id))))
      (should (qq-gateway-attachment-cancel-operation operation))
      (should-not (qq-gateway-attachment-cancel-operation operation))
      (should (equal canceled "request-a"))
      (should (equal released-resource "res-image-a"))
      (should (equal released-attachment qq-gateway-attachment-test-id)))))

(provide 'qq-gateway-attachment-test)

;;; qq-gateway-attachment-test.el ends here
