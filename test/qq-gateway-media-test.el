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
         (qq-gateway-media--refresh-owner nil)
         (qq-gateway-media--resync-request-id nil)
         (qq-gateway-media--playable-resources (make-hash-table :test #'equal))
         (qq-gateway-media-changed-hook nil)
         (qq-gateway-media-desync-hook nil)
         (qq-gateway-resource--resources (make-hash-table :test #'equal))
         (qq-gateway-resource--order nil)
         (qq-gateway-resource--refresh-owner nil)
         (qq-gateway-resource-changed-hook nil))
     ,@body))

(ert-deftest qq-gateway-media-accessors-copy-projected-values ()
  (qq-gateway-media-test-with-state
    (qq-gateway-media--replace
     (list (qq-gateway-media-test-snapshot)) 'test)
    (let* ((public (qq-gateway-media qq-gateway-media-test-id))
           (public-account-id (alist-get 'account_id public)))
      (aset public-account-id 0 ?Y)
      (should (equal
               (alist-get 'account_id
                          (qq-gateway-media qq-gateway-media-test-id))
               "slot-a")))))

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
      (let ((watch
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
        (should-not (qq-gateway-watch-active-p watch))))))

(ert-deftest qq-gateway-media-cancel-operation-mutates-service-lifecycle ()
  (qq-gateway-media-test-with-state
      (let ((operation
           (qq-gateway-media-operation-create
            :active-p t :media-id qq-gateway-media-test-id
            :account-id "slot-a"))
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
    (let* ((account-id "slot-a")
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
           delivered materialize-callback media-ready-callback
           status-callback raw-ready-callback playable-ready-callback
           derive-callback open-callback)
      (setf (alist-get 'suggested_name raw) "voice.silk"
            (alist-get 'media_type raw) "audio/x-tencent-silk"
            (alist-get 'suggested_name playable) "voice.wav"
            (alist-get 'media_type playable) "audio/wav")
      (unwind-protect
          (cl-letf (((symbol-function 'qq-gateway-current-account-id)
                     (lambda () account-id))
                    ((symbol-function 'qq-gateway-account)
                     (lambda (candidate)
                       (and (equal candidate account-id)
                            `((account_id . ,account-id)
                              (phase . "online")))))
                    ((symbol-function 'qq-gateway-media-materialize)
                     (lambda (_media-id callback _errback)
                       (setq materialize-callback callback)
                       "materialize"))
                    ((symbol-function 'qq-gateway-media-await-materialized)
                     (lambda (_media-id callback _errback)
                       (let ((watch
                              (qq-gateway-watch-create
                               :active-p t :cancel-function #'ignore)))
                         (setq media-ready-callback
                               (lambda (media)
                                 (qq-gateway-watch-cancel watch)
                                 (funcall callback media)))
                         watch)))
                    ((symbol-function 'qq-gateway-resource-status)
                     (lambda (_resource-id callback _errback)
                       (setq status-callback callback)
                       "resource-status"))
                    ((symbol-function 'qq-gateway-resource-await-ready)
                     (lambda (resource-id callback _errback)
                       (let* ((watch
                               (qq-gateway-watch-create
                                :active-p t :cancel-function #'ignore))
                              (deliver
                               (lambda (resource)
                                 (qq-gateway-watch-cancel watch)
                                 (funcall callback resource))))
                         (if (equal resource-id "res-native-silk")
                             (setq raw-ready-callback deliver)
                           (setq playable-ready-callback deliver))
                         watch)))
                    ((symbol-function
                      'qq-gateway-resource-derive-playable-record)
                     (lambda (_source _name callback _errback)
                       (cl-incf derive-count)
                       (setq derive-callback callback)
                       "derive"))
                    ((symbol-function 'qq-gateway-resource-open-local)
                     (lambda (resource-id callback _errback)
                       (cl-incf open-count)
                       (setq open-callback
                             (lambda ()
                               (funcall
                                callback
                                `((access_id
                                   . ,(format
                                       "access-aaaaaaaa-bbbb-4ccc-8ddd-%012d"
                                       open-count))
                                  (resource_id . ,resource-id)
                                  (path . ,path)
                                  (expires_at . 1784703600)))))
                       "open")))
            (cl-labels
                ((drive (derive-p)
                   (setq derive-callback nil
                         playable-ready-callback nil)
                   (let ((operation
                          (qq-gateway-media-prepare-record-playback
                           qq-gateway-media-test-id
                           (lambda (result) (push result delivered)))))
                     (should
                      (qq-gateway-media-operation-active-p operation))
                     (funcall
                      materialize-callback
                      (qq-gateway-media-test-snapshot
                       :phase "materializing"
                       :updated-at 1784700001))
                     (funcall
                      media-ready-callback
                      (qq-gateway-media-test-snapshot
                       :phase "materialized"
                       :resource-id "res-native-silk"
                       :bytes-done "128"
                       :updated-at 1784700002))
                     (funcall status-callback raw)
                     (puthash "res-native-silk" raw
                              qq-gateway-resource--resources)
                     (funcall raw-ready-callback raw)
                     (if derive-p
                         (progn
                           (funcall
                            derive-callback
                            (qq-gateway-media-test-resource
                             :resource-id "res-playback-wav"
                             :phase "staging"
                             :suggested-name "voice.wav"
                             :size "1964"))
                           (puthash "res-playback-wav" playable
                                    qq-gateway-resource--resources)
                           (funcall playable-ready-callback playable))
                       (should-not derive-callback))
                     (funcall open-callback)
                     (should-not
                      (qq-gateway-media-operation-active-p operation)))))
              (drive t)
              (drive nil))
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
       "media.changed"
       `((media . ,(qq-gateway-media-test-snapshot))))
      (should (= (length changes) 1))
      (qq-gateway-media--handle-event
       "media.removed" `((media_id . ,qq-gateway-media-test-id)))
      (should-not (qq-gateway-media qq-gateway-media-test-id))
      (should (equal (car changes)
                     `(removed ,qq-gateway-media-test-id))))))

(ert-deftest qq-gateway-media-ready-uses-typed-single-flight-resync ()
  (qq-gateway-media-test-with-state
    (let ((capabilities '("media.list")) calls)
      (cl-letf (((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () capabilities))
                ((symbol-function 'qq-gateway-media-refresh)
                 (lambda (_callback _errback reason &optional _owner)
                   (push reason calls)
                   "media-list-request")))
        (qq-gateway-media--handle-ready "gateway-a")
        (qq-gateway-media--handle-ready "gateway-a")
        (should (equal calls '(ready)))
        (should (equal qq-gateway-media--gateway-instance-id "gateway-a"))
        (should (equal (car qq-gateway-media--resync-request-id)
                       'media-resync))
        (setq capabilities nil)
        (qq-gateway-media--handle-ready "gateway-b")
        (should (equal qq-gateway-media--gateway-instance-id "gateway-b"))
        (should-not qq-gateway-media--resync-request-id)
        (should (equal calls '(ready)))))))

(ert-deftest qq-gateway-media-newest-refresh-owns-full-replacement ()
  (qq-gateway-media-test-with-state
    (let ((old-id "media-11111111-2222-4333-8444-555555555555")
          (new-id "media-11111111-2222-4333-8444-555555555556")
          requests old-callback old-error new-callback)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("media.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params callback errback &optional _early)
                   (setq requests
                         (append requests (list (cons callback errback))))
                   (intern (format "media-request-%d" (length requests))))))
        (qq-gateway-media-refresh
         (lambda (_) (setq old-callback t))
         (lambda (body _failure) (setq old-error body)))
        (qq-gateway-media-refresh
         (lambda (_) (setq new-callback t)) #'ignore)
        (funcall
         (car (nth 1 requests))
         `((media . [,(qq-gateway-media-test-snapshot :media-id new-id)])))
        (funcall
         (car (nth 0 requests))
         `((media . [,(qq-gateway-media-test-snapshot :media-id old-id)])))
        (should new-callback)
        (should-not old-callback)
        (should (equal (alist-get 'code old-error) "superseded_request"))
        (should (qq-gateway-media new-id))
        (should-not (qq-gateway-media old-id))
        (should-not qq-gateway-media--refresh-owner)))))

(ert-deftest qq-gateway-media-reset-cancels-pending-refresh ()
  (qq-gateway-media-test-with-state
    (let (late-success canceled failures)
      (cl-letf (((symbol-function 'qq-gateway-transport-ready-p)
                 (lambda () t))
                ((symbol-function 'qq-gateway-transport-capabilities)
                 (lambda () '("media.list")))
                ((symbol-function 'qq-gateway-transport-send)
                 (lambda (_method _params success _failure &optional _early)
                   (setq late-success success)
                   'media-refresh-token))
                ((symbol-function 'qq-gateway-transport-cancel)
                 (lambda (token) (push token canceled) t)))
        (qq-gateway-media-refresh
         nil (lambda (body _reason) (push body failures)))
        (qq-gateway-media-reset)
        (should (equal canceled '(media-refresh-token)))
        (should (= (length failures) 1))
        (should-not qq-gateway-media--refresh-owner)
        (funcall late-success
                 `((media . [,(qq-gateway-media-test-snapshot)])))
        (should (= (length failures) 1))
        (should-not (qq-gateway-media-list))))))

(provide 'qq-gateway-media-test)

;;; qq-gateway-media-test.el ends here
