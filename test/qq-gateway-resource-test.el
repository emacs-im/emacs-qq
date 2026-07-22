;;; qq-gateway-resource-test.el --- Tests for Gateway resources -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-resource)

(defconst qq-gateway-resource-test-capabilities
  '("resource.list" "resource.stage_local" "resource.derive_record"
    "resource.status"
    "resource.release" "resource.import.list_sources"
    "resource.import.list_images" "resource.import.stage_image"))

(defconst qq-gateway-resource-test-source-id
  (concat "src-linuxqq-" (make-string 64 ?a)))

(defconst qq-gateway-resource-test-candidate-id
  (concat "imp-linuxqq-" (make-string 64 ?b)))

(defun qq-gateway-resource-test-import-source ()
  "Return one pathless native-cache source fixture."
  `((source_id . ,qq-gateway-resource-test-source-id)
    (layout . "linuxqq.nt_data.images.v1")
    (kinds . ("image"))))

(defun qq-gateway-resource-test-import-candidate ()
  "Return one pathless native image candidate fixture."
  `((candidate_id . ,qq-gateway-resource-test-candidate-id)
    (layout . "linuxqq.nt_data.images.v1")
    (family . "picture")
    (month . "2026-04")
    (suggested_name . "84306143ce7c58d1997a95b413e5c177.png")
    (size . "14")
    (expected_md5 . "84306143ce7c58d1997a95b413e5c177")))

(cl-defun qq-gateway-resource-test-snapshot
    (&key
     (resource-id "res-opaque-a")
     (phase "staging")
     (suggested-name "payload.bin")
     (size "3")
     media-type digests
     (created-at 1784700000)
     (updated-at 1784700000)
     (expires-at 1784786400)
     error)
  "Return one closed staged-resource snapshot fixture."
  `((resource_id . ,resource-id)
    (phase . ,phase)
    (suggested_name . ,suggested-name)
    (size . ,size)
    (media_type . ,media-type)
    (digests . ,digests)
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (expires_at . ,expires-at)
    (error . ,error)))

(defun qq-gateway-resource-test-ready (&optional resource-id updated-at)
  "Return one ready fixture for RESOURCE-ID at UPDATED-AT."
  (qq-gateway-resource-test-snapshot
   :resource-id (or resource-id "res-opaque-a")
   :phase "ready"
   :media-type "application/octet-stream"
   :digests
   '((sha256 . "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
     (sha1 . "a9993e364706816aba3e25717850c26c9cd0d89d")
     (md5 . "900150983cd24fb0d6963f7d28e17f72"))
   :updated-at (or updated-at 1784700001)))

(defmacro qq-gateway-resource-test-with-state (&rest body)
  "Run BODY with an isolated resource projection."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway-resource--resources (make-hash-table :test #'equal))
         (qq-gateway-resource--order nil)
         (qq-gateway-resource--gateway-instance-id nil)
         (qq-gateway-resource--resync-request-id nil)
         (qq-gateway-resource-changed-hook nil)
         (qq-gateway-resource-desync-hook nil))
     ,@body))

(ert-deftest qq-gateway-resource-validator-keeps-exact-size-and-hides-path ()
  (let* ((snapshot (qq-gateway-resource-test-ready))
         (validated (qq-gateway-resource--validate-snapshot snapshot)))
    (should (equal (alist-get 'size validated) "3"))
    (should-not (assq 'path validated))
    (setf (alist-get 'size snapshot) 3)
    (should-error (qq-gateway-resource--validate-snapshot snapshot))
    (setf (alist-get 'size snapshot) "3")
    (push '(path . "/tmp/payload.bin") snapshot)
    (should-error (qq-gateway-resource--validate-snapshot snapshot))))

(ert-deftest qq-gateway-resource-validator-rejects-contradictory-phases ()
  (let ((staging (qq-gateway-resource-test-snapshot))
        (ready (qq-gateway-resource-test-ready))
        (failed
         (qq-gateway-resource-test-snapshot
          :phase "failed"
          :error '((code . "copy_failed") (message . "disk full")))))
    (should (qq-gateway-resource--validate-snapshot staging))
    (should (qq-gateway-resource--validate-snapshot ready))
    (should (qq-gateway-resource--validate-snapshot failed))
    (setf (alist-get 'error ready)
          '((code . "impossible") (message . "contradiction")))
    (should-error (qq-gateway-resource--validate-snapshot ready))))

(ert-deftest qq-gateway-resource-stage-local-sends-only-explicit-source-input ()
  (qq-gateway-resource-test-with-state
    (let ((path (make-temp-file "qq-resource-" nil ".bin" "abc"))
          sent-method sent-params delivered)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                     (lambda () t))
                    ((symbol-function 'qq-gateway-transport-capabilities)
                     (lambda () qq-gateway-resource-test-capabilities))
                    ((symbol-function 'qq-gateway-transport-send)
                     (lambda (method params callback _errback &optional _early)
                       (setq sent-method method sent-params params)
                       (funcall callback
                                `((resource
                                   . ,(qq-gateway-resource-test-snapshot))))
                       "stage-request")))
            (should
             (equal
              (qq-gateway-resource-stage-local
               path "payload.bin" nil
               (lambda (snapshot) (setq delivered snapshot)))
              "stage-request"))
            (should (equal sent-method "resource.stage_local"))
            (should (equal (alist-get 'path sent-params)
                           (expand-file-name path)))
            (should
             (equal (alist-get 'expected sent-params)
                    '((size . "3") (sha256))))
            (should (equal (alist-get 'suggested_name sent-params)
                           "payload.bin"))
            (should (equal (alist-get 'phase delivered) "staging"))
            (should-not (assq 'path (qq-gateway-resource "res-opaque-a"))))
        (delete-file path)))))

(ert-deftest qq-gateway-resource-derive-record-keeps-source-and-result-distinct ()
  (qq-gateway-resource-test-with-state
    (puthash "res-source-wav"
             (qq-gateway-resource-test-ready "res-source-wav")
             qq-gateway-resource--resources)
    (let (sent-method sent-params delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-resource-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (setq sent-method method sent-params params)
                   (funcall
                    callback
                    `((resource
                       . ,(qq-gateway-resource-test-snapshot
                           :resource-id "res-derived-silk"
                           :suggested-name "voice.silk"
                           :size "27"))))
                   "derive-request")))
        (should
         (equal
          (qq-gateway-resource-derive-record
           "res-source-wav" "voice.silk"
           (lambda (snapshot) (setq delivered snapshot)))
          "derive-request"))
        (should (equal sent-method "resource.derive_record"))
        (should
         (equal sent-params
                '((source_resource_id . "res-source-wav")
                  (suggested_name . "voice.silk"))))
        (should (equal (alist-get 'resource_id delivered)
                       "res-derived-silk"))
        (should (qq-gateway-resource "res-source-wav"))
        (should (qq-gateway-resource "res-derived-silk"))))))

(ert-deftest qq-gateway-resource-derive-record-rejects-reused-source-id ()
  (qq-gateway-resource-test-with-state
    (let (failure)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-resource-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback _errback &optional _early)
                   (funcall
                    callback
                    `((resource
                       . ,(qq-gateway-resource-test-snapshot
                           :resource-id "res-source-wav"))))
                   "derive-request")))
        (qq-gateway-resource-derive-record
         "res-source-wav" nil nil
         (lambda (body reason) (setq failure (list body reason))))
        (should (equal (alist-get 'code (car failure))
                       "invalid_gateway_result"))
        (should (string-match-p "reused its source identity" (cadr failure)))
        (should-not (qq-gateway-resource "res-source-wav"))))))

(ert-deftest qq-gateway-resource-events-do-not-regress-terminal-state ()
  (qq-gateway-resource-test-with-state
    (let (changes)
      (add-hook 'qq-gateway-resource-changed-hook
                (lambda (reason resource-id)
                  (push (list reason resource-id) changes)))
      (qq-gateway-resource--handle-event
       "resource.changed"
       `((resource . ,(qq-gateway-resource-test-snapshot))))
      (qq-gateway-resource--handle-event
       "resource.changed"
       `((resource . ,(qq-gateway-resource-test-ready))))
      ;; A delayed staging response cannot overwrite the ready snapshot.
      (qq-gateway-resource--upsert
       (qq-gateway-resource-test-snapshot) 'stage-response)
      (should (equal (alist-get 'phase
                                (qq-gateway-resource "res-opaque-a"))
                     "ready"))
      (should (= (length changes) 2))
      (qq-gateway-resource--handle-event
       "resource.removed" '((resource_id . "res-opaque-a")))
      (should-not (qq-gateway-resource "res-opaque-a"))
      (should (equal (car changes) '(removed "res-opaque-a"))))))

(ert-deftest qq-gateway-resource-native-import-validators-are-closed-and-pathless ()
  (let ((source (qq-gateway-resource-test-import-source))
        (candidate (qq-gateway-resource-test-import-candidate)))
    (should (qq-gateway-resource--validate-import-source source))
    (should (qq-gateway-resource--validate-import-candidate candidate))
    (push '(path . "/private/nt_data") source)
    (should-error (qq-gateway-resource--validate-import-source source))
    (setf (alist-get 'size candidate) 14)
    (should-error (qq-gateway-resource--validate-import-candidate candidate))))

(ert-deftest qq-gateway-resource-native-import-lists-and-stages-with-opaque-identities ()
  (qq-gateway-resource-test-with-state
    (let (calls sources page staged)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-resource-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (push (list method params) calls)
                   (pcase method
                     ("resource.import.list_sources"
                      (funcall callback
                               `((sources
                                  . (,(qq-gateway-resource-test-import-source))))))
                     ("resource.import.list_images"
                      (funcall callback
                               `((source_id . ,qq-gateway-resource-test-source-id)
                                 (candidates
                                  . (,(qq-gateway-resource-test-import-candidate)))
                                 (next))))
                     ("resource.import.stage_image"
                      (funcall callback
                               `((source_id . ,qq-gateway-resource-test-source-id)
                                 (candidate_id
                                  . ,qq-gateway-resource-test-candidate-id)
                                 (resource
                                  . ,(qq-gateway-resource-test-snapshot))))))
                   method)))
        (qq-gateway-resource-import-sources
         (lambda (value) (setq sources value)))
        (qq-gateway-resource-import-images
         qq-gateway-resource-test-source-id nil 25
         (lambda (value) (setq page value)))
        (qq-gateway-resource-import-image
         qq-gateway-resource-test-source-id
         qq-gateway-resource-test-candidate-id
         (lambda (value) (setq staged value)))
        (should (equal sources (list (qq-gateway-resource-test-import-source))))
        (should
         (equal (alist-get 'candidates page)
                (list (qq-gateway-resource-test-import-candidate))))
        (should (equal (alist-get 'phase staged) "staging"))
        (should (qq-gateway-resource "res-opaque-a"))
        (should
         (member
          `("resource.import.list_images"
            ((source_id . ,qq-gateway-resource-test-source-id)
             (after)
             (limit . 25)))
          calls))
        (should
         (member
          `("resource.import.stage_image"
            ((source_id . ,qq-gateway-resource-test-source-id)
             (candidate_id . ,qq-gateway-resource-test-candidate-id)))
          calls))))))

(ert-deftest qq-gateway-resource-ready-and-lag-errors-request-one-resync ()
  (qq-gateway-resource-test-with-state
    (let (calls desync)
      (add-hook 'qq-gateway-resource-desync-hook
                (lambda (body) (setq desync body)))
      (cl-letf (((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-resource-test-capabilities))
                ((symbol-function 'qq-gateway-resource-refresh)
                 (lambda (_callback _errback reason)
                   (push reason calls)
                   "resource-list-request")))
        (qq-gateway-resource--handle-event
         "gateway.ready"
         '((gateway_instance_id . "gateway-a") (accounts)))
        (should (equal qq-gateway-resource--gateway-instance-id "gateway-a"))
        (should (equal qq-gateway-resource--resync-request-id
                       "resource-list-request"))
        (qq-gateway-resource--handle-protocol-error
         '((code . "resource_event_stream_lagged")
           (message . "missed 2")))
        (should (equal calls '(ready)))
        (should (equal (alist-get 'code desync)
                       "resource_event_stream_lagged"))))))

(ert-deftest qq-gateway-resource-list-replacement-is-atomic-on-duplicates ()
  (qq-gateway-resource-test-with-state
    (qq-gateway-resource--replace
     (list (qq-gateway-resource-test-snapshot)) 'initial)
    (should-error
     (qq-gateway-resource--replace
      (list (qq-gateway-resource-test-ready)
            (qq-gateway-resource-test-ready))
      'resync))
    (should (equal (alist-get 'phase
                              (qq-gateway-resource "res-opaque-a"))
                   "staging"))))

(provide 'qq-gateway-resource-test)

;;; qq-gateway-resource-test.el ends here
