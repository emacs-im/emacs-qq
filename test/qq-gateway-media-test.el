;;; qq-gateway-media-test.el --- Tests for native remote media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-gateway-media)

(defconst qq-gateway-media-test-id
  "media-aaaaaaaa-bbbb-4ccc-8ddd-eeeeeeeeeeee")

(defconst qq-gateway-media-test-capabilities
  '("media.list" "media.status" "media.materialize" "media.cancel"
    "media.release"
    "resource.derive_playable_record" "resource.open_local"
    "resource.close_local"))

(cl-defun qq-gateway-media-test-snapshot
    (&key
     (media-id qq-gateway-media-test-id)
     (account-id "slot-a")
     (message-id "7348923749823749823")
     (segment-index 0)
     (duration-seconds 17)
     (generation "7")
     (phase "available")
     (bytes-done "0")
     (created-at 1784700000)
     (updated-at 1784700000)
     (expected-size "128")
     (bytes-total "128")
     resource-id error)
  "Return one closed native remote-media fixture."
  `((media_id . ,media-id)
    (account_id . ,account-id)
    (message_id . ,message-id)
    (segment_index . ,segment-index)
    (kind . "record")
    (duration_seconds . ,duration-seconds)
    ,@(when expected-size `((expected_size . ,expected-size)))
    (observed_generation . ,generation)
    (phase . ,phase)
    (bytes_done . ,bytes-done)
    ,@(when bytes-total `((bytes_total . ,bytes-total)))
    ,@(when resource-id `((resource_id . ,resource-id)))
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    ,@(when error `((error . ,error)))))

(cl-defun qq-gateway-media-test-resource
    (&key
     (resource-id "res-native-silk")
     (phase "ready")
     (suggested-name "voice.silk")
     (size "128")
     (media-type "audio/x-tencent-silk")
     (created-at 1784700000)
     (updated-at 1784700001))
  "Return one closed staged-resource fixture."
  `((resource_id . ,resource-id)
    (phase . ,phase)
    (suggested_name . ,suggested-name)
    (size . ,size)
    (media_type . ,(and (equal phase "ready") media-type))
    (digests
     . ,(and (equal phase "ready")
             '((sha256 . "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
               (sha1 . "a9993e364706816aba3e25717850c26c9cd0d89d")
               (md5 . "900150983cd24fb0d6963f7d28e17f72"))))
    (created_at . ,created-at)
    (updated_at . ,updated-at)
    (expires_at . 1784786400)
    (error)))

(defmacro qq-gateway-media-test-with-state (&rest body)
  "Run BODY with isolated remote-media and resource projections."
  (declare (indent 0) (debug t))
  `(let ((qq-gateway-media--media (make-hash-table :test #'equal))
         (qq-gateway-media--order nil)
         (qq-gateway-media--gateway-instance-id nil)
         (qq-gateway-media--resync-request-id nil)
         (qq-gateway-media--playable-resources (make-hash-table :test #'equal))
         (qq-gateway-media-changed-hook nil)
         (qq-gateway-media-desync-hook nil)
         (qq-gateway-resource--resources (make-hash-table :test #'equal))
         (qq-gateway-resource--order nil)
         (qq-gateway-resource-changed-hook nil))
     ,@body))

(ert-deftest qq-gateway-media-validator-keeps-snowflake-text-and-hides-native-reference ()
  (let* ((snapshot (qq-gateway-media-test-snapshot))
         (validated (qq-gateway-media--validate-snapshot snapshot)))
    (should (equal (alist-get 'message_id validated)
                   "7348923749823749823"))
    (should (equal (alist-get 'bytes_done validated) "0"))
    (should-not (assq 'native_reference validated))
    (setf (alist-get 'message_id snapshot) 7348923749823749823)
    (should-error (qq-gateway-media--validate-snapshot snapshot))
    (setf (alist-get 'message_id snapshot) "7348923749823749823")
    (push '(native_reference . "secret-file-uuid") snapshot)
    (should-error (qq-gateway-media--validate-snapshot snapshot))))

(ert-deftest qq-gateway-media-validator-enforces-terminal-phase-shapes ()
  (should
   (qq-gateway-media--validate-snapshot
    (qq-gateway-media-test-snapshot
     :phase "materialized" :resource-id "res-native")))
  (should
   (qq-gateway-media--validate-snapshot
    (qq-gateway-media-test-snapshot
     :phase "failed" :bytes-total nil
     :error '((code . "download_failed") (message . "network")))))
  (should-error
   (qq-gateway-media--validate-snapshot
    (qq-gateway-media-test-snapshot
     :phase "materialized" :resource-id nil)))
  (should-error
   (qq-gateway-media--validate-snapshot
    (qq-gateway-media-test-snapshot
     :phase "available" :resource-id "res-impossible"))))

(ert-deftest qq-gateway-media-materialize-starts-background-state-machine ()
  (qq-gateway-media-test-with-state
    (let (delivered)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () qq-gateway-media-test-capabilities))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (method params callback _errback &optional _early)
                   (should (equal method "media.materialize"))
                   (should (equal params
                                  `((media_id . ,qq-gateway-media-test-id))))
                   (funcall
                    callback
                    `((media . ,(qq-gateway-media-test-snapshot
                                  :phase "materializing"
                                  :bytes-done "0"
                                  :updated-at 1784700001))))
                   "materialize-request")))
        (should
         (equal
          (qq-gateway-media-materialize
           qq-gateway-media-test-id
           (lambda (result) (setq delivered result)))
          "materialize-request"))
        (should (equal (alist-get 'phase delivered) "materializing"))
        (should (qq-gateway-media qq-gateway-media-test-id))
        (should-not (qq-gateway-resource "res-native-silk"))))))

(ert-deftest qq-gateway-media-await-materialized-follows-events-and-is-locally-cancellable ()
  (qq-gateway-media-test-with-state
    (let (delivered failed)
      (qq-gateway-media--upsert
       (qq-gateway-media-test-snapshot :phase "materializing") 'test)
      (let ((cancel
             (qq-gateway-media-await-materialized
              qq-gateway-media-test-id
              (lambda (media) (setq delivered media))
              (lambda (_body reason) (setq failed reason)))))
        (should-not delivered)
        (qq-gateway-media--handle-event
         "media.changed"
         `((media
            . ,(qq-gateway-media-test-snapshot
                :phase "materialized"
                :resource-id "res-native-silk"
                :bytes-done "128"
                :updated-at 1784700001))))
        (should (equal (alist-get 'resource_id delivered)
                       "res-native-silk"))
        (should-not failed)
        (funcall cancel)))))

(ert-deftest qq-gateway-media-cancel-operation-mutates-service-lifecycle ()
  (qq-gateway-media-test-with-state
    (let ((operation
           (qq-gateway-media-operation-create
            :active-p t :media-id qq-gateway-media-test-id :owner '("slot-a" . "7")))
          canceled)
      (cl-letf (((symbol-function 'qq-gateway--method-available-p)
                 (lambda (method) (equal method "media.cancel")))
                ((symbol-function 'qq-gateway-media-cancel)
                 (lambda (media-id &optional _callback _errback)
                   (setq canceled media-id)
                   "cancel-request")))
        (should (qq-gateway-media-cancel-operation operation))
        (should-not (qq-gateway-media-operation-active-p operation))
        (should (equal canceled qq-gateway-media-test-id))))))

(ert-deftest qq-gateway-media-playback-pipeline-reuses-derived-wav ()
  (qq-gateway-media-test-with-state
    (let* ((owner '("slot-a" . "7"))
           (path (make-temp-file "qq-playback-" nil ".wav" "RIFF"))
           (raw (qq-gateway-media-test-resource
                 :resource-id "res-native-silk"))
           (playable (qq-gateway-media-test-resource
                      :resource-id "res-playback-wav"
                      :suggested-name "voice.wav"
                      :size "1964"
                      :media-type "audio/wav"))
           (derive-count 0)
           (open-count 0)
           delivered)
      (setf (alist-get 'suggested_name raw) "voice.silk"
            (alist-get 'media_type raw) "audio/x-tencent-silk"
            (alist-get 'suggested_name playable) "voice.wav"
            (alist-get 'media_type playable) "audio/wav")
      (unwind-protect
          (cl-letf (((symbol-function 'qq-gateway-current-account-owner)
                     (lambda () owner))
                    ((symbol-function 'qq-gateway-media-materialize)
                     (lambda (_media-id callback _errback)
                       (funcall
                        callback
                        (qq-gateway-media-test-snapshot
                         :phase "materializing"
                         :updated-at 1784700001))
                       "materialize"))
                    ((symbol-function 'qq-gateway-media-await-materialized)
                     (lambda (_media-id callback _errback)
                       (funcall
                        callback
                        (qq-gateway-media-test-snapshot
                         :phase "materialized"
                         :resource-id "res-native-silk"
                         :bytes-done "128"
                         :updated-at 1784700002))
                       #'ignore))
                    ((symbol-function 'qq-gateway-resource-status)
                     (lambda (_resource-id callback _errback)
                       (funcall callback raw)
                       "resource-status"))
                    ((symbol-function 'qq-gateway-resource-await-ready)
                     (lambda (resource-id callback _errback)
                       (let ((resource
                              (if (equal resource-id "res-native-silk")
                                  raw playable)))
                         (puthash resource-id resource
                                  qq-gateway-resource--resources)
                         (funcall callback resource))
                       #'ignore))
                    ((symbol-function
                      'qq-gateway-resource-derive-playable-record)
                     (lambda (_source _name callback _errback)
                       (cl-incf derive-count)
                       (funcall callback
                                (qq-gateway-media-test-resource
                                 :resource-id "res-playback-wav"
                                 :phase "staging"
                                 :suggested-name "voice.wav"
                                 :size "1964"))
                       "derive"))
                    ((symbol-function 'qq-gateway-resource-open-local)
                     (lambda (resource-id callback _errback)
                       (cl-incf open-count)
                       (funcall
                        callback
                        `((access_id
                           . ,(format
                               "access-aaaaaaaa-bbbb-4ccc-8ddd-%012d"
                               open-count))
                          (resource_id . ,resource-id)
                          (path . ,path)
                          (expires_at . 1784703600)))
                       "open")))
            (let ((operation
                   (qq-gateway-media-prepare-record-playback
                    qq-gateway-media-test-id
                    (lambda (result) (push result delivered)))))
              (should-not (qq-gateway-media-operation-active-p operation)))
            (let ((operation
                   (qq-gateway-media-prepare-record-playback
                    qq-gateway-media-test-id
                    (lambda (result) (push result delivered)))))
              (should-not (qq-gateway-media-operation-active-p operation)))
            (should (= derive-count 1))
            (should (= open-count 2))
            (should (= (length delivered) 2))
            (should
             (cl-every
              (lambda (result)
                (and (equal (alist-get 'source_resource_id result)
                            "res-native-silk")
                     (equal (alist-get 'playback_resource_id result)
                            "res-playback-wav")
                     (equal (alist-get 'path (alist-get 'access result)) path)))
              delivered)))
        (delete-file path)))))

(ert-deftest qq-gateway-media-events-project-and-remove-opaque_handles ()
  (qq-gateway-media-test-with-state
    (let (changes)
      (add-hook 'qq-gateway-media-changed-hook
                (lambda (reason media-id)
                  (push (list reason media-id) changes)))
      (qq-gateway-media--handle-event
       "media.changed"
       `((media . ,(qq-gateway-media-test-snapshot))))
      (should (qq-gateway-media qq-gateway-media-test-id))
      (qq-gateway-media--handle-event
       "media.removed" `((media_id . ,qq-gateway-media-test-id)))
      (should-not (qq-gateway-media qq-gateway-media-test-id))
      (should (equal (car changes)
                     `(removed ,qq-gateway-media-test-id))))))

(provide 'qq-gateway-media-test)

;;; qq-gateway-media-test.el ends here
