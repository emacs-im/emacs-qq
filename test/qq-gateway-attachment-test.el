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
     (generation "7")
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
    (generation . ,generation)
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

(defun qq-gateway-attachment-test-account ()
  "Return one selected online account fixture."
  '((account_id . "slot-a")
    (label . "Primary")
    (phase . "online")
    (uin . "10002")
    (uid . "u_self")
    (generation . "7")
    (challenge)
    (problem)))

(defmacro qq-gateway-attachment-test-with-state (&rest body)
  "Run BODY with isolated account, resource, and attachment projections."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway--accounts (make-hash-table :test #'equal))
         (qq-gateway--account-order nil)
         (qq-gateway--current-account-id nil)
         (qq-gateway-accounts-changed-hook nil)
         (qq-gateway-current-account-changed-hook nil)
         (qq-gateway-attachment--attachments (make-hash-table :test #'equal))
         (qq-gateway-attachment--order nil)
         (qq-gateway-attachment--gateway-instance-id nil)
         (qq-gateway-attachment--resync-request-id nil)
         (qq-gateway-attachment-changed-hook nil)
         (qq-gateway-attachment-desync-hook nil)
         (qq-gateway-resource--resources (make-hash-table :test #'equal))
         (qq-gateway-resource--order nil)
         (qq-gateway-resource--gateway-instance-id nil)
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
    (setf (alist-get 'generation snapshot) 7)
    (should-error (qq-gateway-attachment--validate-snapshot snapshot)))
  (let ((record (qq-gateway-attachment-test-ready-record)))
    (should (qq-gateway-attachment--validate-snapshot record))
    (setf (alist-get 'use record)
          '((kind . "record") (summary . "must-not-exist")))
    (should-error (qq-gateway-attachment--validate-snapshot record))))

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

(ert-deftest qq-gateway-attachment-prepare-binds-account-generation-and-chat ()
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
        (should (equal (alist-get 'generation delivered) "7"))))))

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

(ert-deftest qq-gateway-attachment-sendable-checks-owner-and-conversation ()
  (qq-gateway-attachment-test-with-state
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-ready) 'ready)
    (should
     (equal
      (qq-gateway-attachment-assert-sendable
       qq-gateway-attachment-test-id "group:8209413637" '("slot-a" . "7"))
      qq-gateway-attachment-test-id))
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-id "group:8209413637" '("slot-a" . "8"))
     :type 'user-error)
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-id "group:10001" '("slot-a" . "7"))
     :type 'user-error)
    (qq-gateway-attachment--upsert
     (qq-gateway-attachment-test-ready-record) 'ready-record)
    (should
     (equal
      (qq-gateway-attachment-assert-sendable
       qq-gateway-attachment-test-record-id
       "group:8209413637" '("slot-a" . "7") "record")
      qq-gateway-attachment-test-record-id))
    (should-error
     (qq-gateway-attachment-assert-sendable
      qq-gateway-attachment-test-record-id
      "group:8209413637" '("slot-a" . "7"))
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
          delivered)
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
               ((symbol-function 'qq-gateway-attachment-prepare-image)
                (lambda (_session _resource _summary _sub success _failure)
                  (let ((ready (qq-gateway-attachment-test-ready)))
                    (puthash qq-gateway-attachment-test-id ready
                             qq-gateway-attachment--attachments)
                    (setq qq-gateway-attachment--order
                          (list qq-gateway-attachment-test-id))
                    (funcall success
                             (qq-gateway-attachment-test-snapshot)))
                  "prepare-request")))
            (let ((operation
                   (qq-gateway-attachment-stage-and-prepare-image
                    "group:8209413637" path nil nil
                    (lambda (snapshot) (setq delivered snapshot)) #'ignore)))
              (should (qq-gateway-attachment-operation-p operation))
              (should-not
               (qq-gateway-attachment-operation-active-p operation))
              (should (equal (alist-get 'phase delivered) "ready"))))
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

(ert-deftest qq-gateway-attachment-ready-after-generation-drift-is-released ()
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
              (let ((account (qq-gateway-attachment-test-account)))
                (setf (alist-get 'generation account) "8")
                (qq-gateway--upsert-account account 'generation-changed))
              (qq-gateway-attachment--upsert
               (qq-gateway-attachment-test-snapshot
                :phase "negotiating" :updated-at 1784700001)
               'negotiating)
              (qq-gateway-attachment--upsert
               (qq-gateway-attachment-test-snapshot
                :phase "ready" :fast-path t :updated-at 1784700002)
               'ready)
              (should-not delivered)
              (should-not
               (qq-gateway-attachment-operation-active-p operation))
              (should
               (equal (cadr failure)
                      "QQ account generation changed during image preparation"))
              (should (equal released-resource "res-image-a"))
              (should
               (equal released-attachment qq-gateway-attachment-test-id))))
        (delete-file path)))))

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
