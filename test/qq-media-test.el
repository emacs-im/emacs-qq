;;; qq-media-test.el --- Tests for qq-media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-media)
(require 'qq-runtime)

(defmacro qq-media-test-with-reset (&rest body)
  "Run BODY with clean qq-media caches and rerender hooks disabled."
  `(let ((qq-media-cache-update-hook nil))
     (qq-media-clear-cache)
     (unwind-protect
         (progn ,@body)
       (qq-media-clear-cache))))

(ert-deftest qq-media-ensure-resource-image-uses-existing-disk-cache ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (key "avatar:10001")
          (cache-file (expand-file-name (format "%s.jpg" (md5 key)) qq-media-cache-directory))
          (fetch-called nil))
     (unwind-protect
         (progn
           (with-temp-file cache-file
             (insert "cached avatar bytes"))
           (qq-media--cache-resource key '((url . "https://example.com/avatar.jpg")))
           (should
            (equal
             (qq-media--ensure-resource-image
              key
              (lambda (_done _error)
                (setq fetch-called t))
              20
              (lambda (file spec)
                (list file spec)))
             (list cache-file 20)))
           (should-not fetch-called)
           (should (equal (alist-get 'file (qq-media--cached-resource key))
                          cache-file)))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-clear-cache-removes-disk-cache-directory ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
         (cache-file (expand-file-name "sample.jpg" qq-media-cache-directory)))
    (with-temp-file cache-file
      (insert "cached avatar bytes"))
    (should (file-exists-p cache-file))
    (qq-media-clear-cache)
    (should-not (file-exists-p cache-file))
    (should-not (file-directory-p qq-media-cache-directory))))

(ert-deftest qq-media-clear-cache-cancels-active-preview-downloads ()
  (let ((qq-media--fetching-cache (make-hash-table :test #'equal))
        canceled)
    (puthash "image:key" 'image-transfer qq-media--fetching-cache)
    (cl-letf (((symbol-function 'appkit-media-transfer-p)
               (lambda (object) (eq object 'image-transfer)))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (transfer) (push transfer canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore))
      (qq-media-clear-cache)
      (should (equal '(image-transfer) canceled))
      (should-not (gethash "image:key" qq-media--fetching-cache)))))

(ert-deftest qq-media-clear-cache-cancels-shared-video-previews ()
  (let ((qq-media--resource-cache (make-hash-table :test #'equal))
        (qq-media--image-cache (make-hash-table :test #'equal))
        (qq-media--preview-missing-cache (make-hash-table :test #'equal))
        (qq-media--fetching-cache (make-hash-table :test #'equal))
        (qq-media--download-state-table (make-hash-table :test #'equal))
        (qq-media-cache-directory (make-temp-file "qq-clear-video" t))
        cancelled)
    (unwind-protect
        (progn
          (puthash "video:key" t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cancel-video-preview)
                     (lambda (key) (push key cancelled))))
            (qq-media-clear-cache))
          (should (equal cancelled '("qq:video:key")))
          (should-not (gethash "video:key" qq-media--fetching-cache)))
      (when (file-directory-p qq-media-cache-directory)
        (delete-directory qq-media-cache-directory t)))))

(ert-deftest qq-media-clear-cache-cancels-segment-download-transfers ()
  (let ((qq-media--resource-cache (make-hash-table :test #'equal))
        (qq-media--image-cache (make-hash-table :test #'equal))
        (qq-media--preview-missing-cache (make-hash-table :test #'equal))
        (qq-media--fetching-cache (make-hash-table :test #'equal))
        (qq-media--download-state-table (make-hash-table :test #'equal))
        (qq-media-cache-directory (make-temp-file "qq-clear-download" t))
        canceled)
    (puthash "download:key" '(:status downloading :transfer download-handle)
             qq-media--download-state-table)
    (cl-letf (((symbol-function 'appkit-media-transfer-p)
               (lambda (object) (eq object 'download-handle)))
              ((symbol-function 'appkit-media-cancel-transfer)
               (lambda (handle) (push handle canceled)))
              ((symbol-function 'appkit-media-cancel-video-preview) #'ignore))
      (qq-media-clear-cache)
      (should (equal '(download-handle) canceled))
      (should (= 0 (hash-table-count qq-media--download-state-table))))))

(ert-deftest qq-media-clear-cache-revokes-custom-face-account-generation ()
  "Late favorite callbacks cannot repopulate a replacement account's cache."
  (let* ((qq-media-cache-directory (make-temp-file "qq-faces-reset" t))
         (qq-media--resource-cache (make-hash-table :test #'equal))
         (qq-media--image-cache (make-hash-table :test #'equal))
         (qq-media--preview-missing-cache (make-hash-table :test #'equal))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (qq-media--download-state-table (make-hash-table :test #'equal))
         (qq-media--account-generation 3)
         (qq-media--custom-faces nil)
         (qq-media--custom-faces-fetched-at nil)
         (qq-media--custom-face-waiters nil)
         (qq-media--custom-face-refresh-owner nil)
         (qq-media--custom-face-completion-pairs
          '(("OLD COMPLETION SECRET" . ((md5 . "old")))))
         old-success old-error new-success
         old-deliveries new-deliveries failures cancelled
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'qq-api-fetch-custom-face-info)
                   (lambda (callback &optional errback _count)
                     (cl-incf calls)
                     (if (= calls 1)
                         (setq old-success callback
                               old-error errback)
                       (setq new-success callback))
                     (if (= calls 1) 'old-face-request 'new-face-request)))
                  ((symbol-function 'qq-api-cancel-request)
                   (lambda (token)
                     (push token cancelled)
                     ;; Cancellation may synchronously emit an errback.  The
                     ;; old owner has already been revoked at this boundary.
                     (when old-error
                       (funcall old-error nil "cancelled")))))
          (qq-media-ensure-custom-faces
           (lambda (faces) (push faces old-deliveries))
           (lambda (_response reason) (push reason failures)))
          (should qq-media--custom-face-refresh-owner)
          (qq-media-clear-cache)
          (should (= qq-media--account-generation 4))
          (should (equal cancelled '(old-face-request)))
          (should-not failures)
          (should-not qq-media--custom-faces)
          (should-not qq-media--custom-faces-fetched-at)
          (should-not qq-media--custom-face-refresh-owner)
          (should-not qq-media--custom-face-waiters)
          (should-not qq-media--custom-face-completion-pairs)

          (qq-media-ensure-custom-faces
           (lambda (faces) (push faces new-deliveries)))
          (let ((replacement-owner qq-media--custom-face-refresh-owner))
            (funcall old-success
                     '(((md5 . "old-secret-md5")
                        (desc . "OLD FAVORITE SECRET"))))
            (should (eq replacement-owner
                        qq-media--custom-face-refresh-owner))
            (should-not qq-media--custom-faces)
            (should-not old-deliveries)
            (should-not
             (string-match-p
              "OLD \(?:COMPLETION\|FAVORITE\) SECRET"
              (prin1-to-string
               (list qq-media--custom-faces
                     qq-media--custom-face-waiters
                     qq-media--custom-face-completion-pairs)))))
          (funcall new-success
                   '(((md5 . "new-md5") (desc . "NEW FAVORITE"))))
          (should-not old-deliveries)
          (should (= 1 (length new-deliveries)))
          (should (equal "new-md5"
                         (alist-get 'md5
                                    (car (car new-deliveries)))))
          (should-not qq-media--custom-face-refresh-owner)
          (should-not qq-media--custom-face-waiters))
      (when (file-directory-p qq-media-cache-directory)
        (delete-directory qq-media-cache-directory t)))))

(ert-deftest qq-media-ensure-resource-image-starts-remote-download-for-url-only-resource ()
  (qq-media-test-with-reset
   (let (started-key started-resource started-spec)
     (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                (lambda (key resource spec _builder)
                  (setq started-key key)
                  (setq started-resource resource)
                  (setq started-spec spec))))
       (should-not
        (qq-media--ensure-resource-image
         "avatar:10001"
         (lambda (done _error)
           (funcall done '((url . "https://example.com/avatar.jpg"))))
         20
         (lambda (_file _spec)
           'image)))
       (should (equal started-key "avatar:10001"))
       (should (equal started-spec 20))
       (should (equal (alist-get 'url started-resource)
                      "https://example.com/avatar.jpg"))))))

(ert-deftest qq-media-resource-image-download-uses-shared-transfer-runtime ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-transfer" t))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (key "avatar:10001")
         (resource '((url . "https://example.com/avatar.jpg")
                     (name . "avatar.jpg")))
         captured-resource
         captured-base)
    (unwind-protect
        (progn
          (puthash key t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
                     (lambda (canonical cache-base _success _error
                                        &rest _arguments)
                       (setq captured-resource canonical
                             captured-base cache-base)
                       :image-transfer)))
            (qq-media--start-resource-image-download
             key resource 20 (lambda (_file _spec) :image)))
          (should (equal resource captured-resource))
          (should (equal (qq-media--remote-image-cache-file-base key)
                         captured-base))
          (should (eq :image-transfer
                      (gethash key qq-media--fetching-cache))))
      (delete-directory qq-media-cache-directory t))))

(ert-deftest qq-media-avatar-image-prefers-remote-refresh-over-stale-local-file ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-media-cache" t))
          (stale-file (make-temp-file "qq-stale-avatar" nil ".png"))
          (key "avatar:10001")
          started-key)
     (unwind-protect
         (progn
           (with-temp-file stale-file
             (insert "stale avatar bytes"))
           (qq-media--cache-resource
            key
            `((file . ,stale-file)
              (url . "https://example.com/avatar.jpg")))
           (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                      (lambda (download-key _resource _spec _builder)
                        (setq started-key download-key))))
             (should (equal (qq-media--ensure-resource-image
                             key
                             (lambda (_done _error)
                               (ert-fail "fetcher should not run for cached avatar resource"))
                             20
                             (lambda (file spec)
                               (list file spec)))
                            (list stale-file 20)))
             (should (equal started-key key))))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))
       (when (file-exists-p stale-file)
         (delete-file stale-file))))))

(ert-deftest qq-media-avatar-download-excludes-stale-local-transfer-source ()
  (let* ((qq-media-cache-directory (make-temp-file "qq-media-transfer" t))
         (qq-media--fetching-cache (make-hash-table :test #'equal))
         (stale-file (make-temp-file "qq-stale-avatar" nil ".png"))
         (key "avatar:10001")
         (resource
          `((file . ,stale-file)
            (url . "https://example.com/avatar.jpg")
            (name . "avatar.jpg")))
         captured-resource)
    (unwind-protect
        (progn
          (with-temp-file stale-file
            (insert "stale avatar bytes"))
          (puthash key t qq-media--fetching-cache)
          (cl-letf (((symbol-function 'appkit-media-cache-image-resource-async)
                     (lambda (canonical _cache-base _success _error
                                        &rest _arguments)
                       (setq captured-resource canonical)
                       :image-transfer)))
            (qq-media--start-resource-image-download
             key resource 20 (lambda (_file _spec) :image)))
          (should (equal "https://example.com/avatar.jpg"
                         (alist-get 'url captured-resource)))
          (should-not (alist-get 'file captured-resource))
          (should (equal stale-file (alist-get 'file resource)))
          (should (file-exists-p stale-file)))
      (when (file-directory-p qq-media-cache-directory)
        (delete-directory qq-media-cache-directory t))
      (when (file-exists-p stale-file)
        (delete-file stale-file)))))

(ert-deftest qq-media-non-avatar-image-keeps-local-file-preference ()
  (qq-media-test-with-reset
   (let ((local-file (make-temp-file "qq-local-image" nil ".png"))
         (started nil))
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "local image bytes"))
           (qq-media--cache-resource
            "face:100"
            `((file . ,local-file)
              (url . "https://example.com/face.png")))
           (cl-letf (((symbol-function 'qq-media--start-resource-image-download)
                      (lambda (&rest _args)
                        (setq started t))))
             (should (equal (qq-media--ensure-resource-image
                             "face:100"
                             (lambda (_done _error)
                               (ert-fail "fetcher should not run for cached local resource"))
                             18
                             (lambda (file spec)
                               (list file spec)))
                            (list local-file 18)))
             (should-not started)))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-ensure-resource-image-clears-fetching-on-error ()
  (qq-media-test-with-reset
   (should-not
    (qq-media--ensure-resource-image
     "avatar:10001"
     (lambda (_done error)
       (funcall error nil "boom"))
     20
     (lambda (_file _spec)
       'image)))
   (should-not (qq-media--resource-fetching-p "avatar:10001"))))

(ert-deftest qq-media-avatar-image-passes-error-callback-to-api ()
  (qq-media-test-with-reset
   (let (errback-called)
     (cl-letf (((symbol-function 'qq-api-get-avatar)
                (lambda (_user-id _done errback &optional _no-cache)
                  (setq errback-called (functionp errback))
                  (when errback
                    (funcall errback nil "boom")))))
       (should-not (qq-media-avatar-image "10001"))
       (should errback-called)
       (should-not (qq-media--resource-fetching-p "avatar:10001"))))))

(ert-deftest qq-media-message-avatar-keeps-guild-and-qq-identities-disjoint ()
  (let (guild-request user-request)
    (cl-letf (((symbol-function 'qq-media-guild-member-avatar-image)
               (lambda (guild-id native-id)
                 (setq guild-request (list guild-id native-id))
                 'guild-avatar))
              ((symbol-function 'qq-media-avatar-image)
               (lambda (user-id)
                 (setq user-request user-id)
                 'user-avatar)))
      (should (eq (qq-media-message-avatar-image
                   '((session-key
                      . "guild:9007199254740993:channel:9007199254741999")
                     (guild-id . "9007199254740993")
                     (sender-native-id . "144115219000000001")
                     (sender-id . "144115219000000001")))
                  'guild-avatar))
      (should (equal guild-request
                     '("9007199254740993"
                       "144115219000000001")))
      (should-not user-request)
      (should (eq (qq-media-message-avatar-image
                   '((sender-id . "10001")))
                  'user-avatar))
      (should (equal user-request "10001")))))

(ert-deftest qq-media-forum-avatar-uses-authoritative-feed-url ()
  (let (profile-request fetch-resource)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (_key fetch _height &optional _factory)
                 (funcall fetch
                          (lambda (resource) (setq fetch-resource resource))
                          #'ignore)
                 'forum-avatar))
              ((symbol-function 'qq-media-guild-member-avatar-image)
               (lambda (&rest args) (setq profile-request args))))
      (should
       (eq
        (qq-media-message-avatar-image
         '((session-key
            . "guild:9007199254740993:channel:9007199254741999")
           (sender-native-id . "144115219000000001")
           (sender-avatar-url . "https://example.invalid/forum-avatar.png")))
        'forum-avatar))
      (should (equal fetch-resource
                     '((url . "https://example.invalid/forum-avatar.png"))))
      (should-not profile-request))))

(ert-deftest qq-media-forward-avatar-uses-url-instead-of-repeated-user-id ()
  (let ((url "https://example.test/forward-node.png")
        fetch-key fetch-resource user-request)
    (cl-letf (((symbol-function 'qq-media--ensure-resource-image)
               (lambda (key fetch _height &optional _factory)
                 (setq fetch-key key)
                 (funcall fetch
                          (lambda (resource) (setq fetch-resource resource))
                          #'ignore)
                 'forward-avatar))
              ((symbol-function 'qq-media-avatar-image)
               (lambda (user-id) (setq user-request user-id))))
      (should
       (eq
        (qq-media-message-avatar-image
         `((sender-id . "1094950020")
           (sender-avatar-url . ,url)))
        'forward-avatar))
      (should (equal fetch-key (concat "message-avatar-url:" url)))
      (should (equal fetch-resource `((url . ,url))))
      (should-not user-request))))

(ert-deftest qq-media-forward-avatar-cache-keys-are-url-scoped ()
  (let ((first
         '((sender-id . "1094950020")
           (sender-avatar-url . "https://example.test/first.png")))
        (second
         '((sender-id . "1094950020")
           (sender-avatar-url . "https://example.test/second.png"))))
    (should-not
     (equal (qq-media-message-avatar-cache-key first)
            (qq-media-message-avatar-cache-key second)))))

(ert-deftest qq-media-message-avatar-cache-key-keeps-native-identities-disjoint ()
  (let ((guild-message
         '((session-key
            . "guild:9007199254740993:channel:9007199254741999")
           (sender-native-id . "144115219000000001")
           (sender-id . "144115219000000001"))))
    (should
     (equal
      (qq-media-message-avatar-cache-key guild-message)
      (concat "guild-member-avatar:9007199254740993:"
              "144115219000000001")))
    (should
     (equal (qq-media-message-avatar-cache-key '((sender-id . "10001")))
            "avatar:10001"))
    (should-not
     (qq-media-message-avatar-cache-key '((sender-id . "0"))))))

(ert-deftest qq-media-normalize-custom-face-list-keeps-multiple-faces ()
  "A list of face alists must not collapse into one bogus entry."
  (let* ((faces '(((url . "https://a")
                   (md5 . "AAAAAAAA")
                   (desc . "")
                   (file . "/tmp/a.jpg"))
                  ((url . "https://b")
                   (md5 . "BBBBBBBB")
                   (desc . "")
                   (file . "/tmp/b.jpg"))
                  ((url . "https://c")
                   (md5 . "CCCCCCCC")
                   (desc . "named")
                   (file . "/tmp/c.jpg"))))
         (normalized (qq-media--normalize-custom-face-list faces))
         (pairs (qq-media-custom-face-completion-candidates normalized)))
    (should (= 3 (length normalized)))
    (should (= 3 (length pairs)))
    (should (string-match-p "AAAAAAAA" (car (nth 0 pairs))))
    (should (string-match-p "BBBBBBBB" (car (nth 1 pairs))))
    (should (string-match-p "named" (car (nth 2 pairs))))
    ;; Labels carry the face for affixation / lookup.
    (should (equal "AAAAAAAA"
                   (alist-get 'md5 (get-text-property 0 'qq-custom-face
                                                      (car (nth 0 pairs))))))))

(ert-deftest qq-media-custom-face-completion-table-metadata ()
  "Favorite picker must keep NapCat order and declare affixation."
  (let* ((faces '(((md5 . "AAAAAAAA") (desc . "") (url . "https://a"))
                  ((md5 . "BBBBBBBB") (desc . "meme") (url . "https://b"))))
         (table (qq-media-custom-face-completion-table faces))
         (meta (funcall table "" nil 'metadata))
         (labels (all-completions "" table)))
    (should (eq (completion-metadata-get meta 'display-sort-function)
                #'identity))
    (should (eq (completion-metadata-get meta 'affixation-function)
                #'qq-media-custom-face-affixation-function))
    (should (= 2 (length labels)))
    (should (string-match-p "AAAAAAAA" (nth 0 labels)))
    (should (string-match-p "meme" (nth 1 labels)))
    ;; Plain-string choice still resolves (completing-read may strip props).
    (should (equal "AAAAAAAA"
                   (alist-get 'md5
                              (qq-media-custom-face-from-completion
                               (substring-no-properties (nth 0 labels))))))))

(ert-deftest qq-media-ensure-custom-faces-coalesces-unloaded-callers ()
  (let ((qq-media--custom-faces nil)
        (qq-media--custom-faces-fetched-at nil)
        (qq-media--custom-face-waiters nil)
        (qq-media--custom-face-refresh-owner nil)
        (requests 0)
        callbacks
        received)
    (cl-letf (((symbol-function 'qq-media-refresh-custom-faces)
               (lambda (callback &optional errback _count)
                 (cl-incf requests)
                 (setq callbacks (list callback errback))
                 'request)))
      (qq-media-ensure-custom-faces
       (lambda (faces) (push (cons 'first faces) received)))
      (qq-media-ensure-custom-faces
       (lambda (faces) (push (cons 'second faces) received)))
      (should (= 1 requests))
      (let ((faces '(((md5 . "one")))))
        (setq qq-media--custom-faces faces
              qq-media--custom-faces-fetched-at (float-time))
        (funcall (car callbacks) faces))
      (should (equal '(first second)
                     (sort (mapcar #'car received)
                           (lambda (left right)
                             (string-lessp (symbol-name left)
                                           (symbol-name right))))))
      (should-not qq-media--custom-face-refresh-owner)
      (should-not qq-media--custom-face-waiters))))

(ert-deftest qq-media-custom-face-waiters-stop-at-account-reset ()
  "A reset from one waiter prevents later old-account waiters from running."
  (let ((qq-media--account-generation 30)
        (qq-media--custom-faces nil)
        (qq-media--custom-faces-fetched-at nil)
        (qq-media--custom-face-waiters nil)
        (qq-media--custom-face-refresh-owner nil)
        success
        first-called
        second-called)
    (cl-letf (((symbol-function 'qq-media-refresh-custom-faces)
               (lambda (callback &optional _errback _count)
                 (setq success callback)
                 'favorite-request)))
      (qq-media-ensure-custom-faces
       (lambda (_faces)
         (setq first-called t)
         (qq-media--revoke-custom-face-work)))
      (qq-media-ensure-custom-faces
       (lambda (_faces) (setq second-called t)))
      (funcall success '(((md5 . "old-account-face"))))
      (should first-called)
      (should-not second-called)
      (should (= qq-media--account-generation 31))
      (should-not qq-media--custom-faces)
      (should-not qq-media--custom-face-waiters)
      (should-not qq-media--custom-face-refresh-owner))))

(ert-deftest qq-media-ensure-custom-faces-cleans-up-synchronous-success ()
  (let ((qq-media--custom-faces nil)
        (qq-media--custom-faces-fetched-at nil)
        (qq-media--custom-face-waiters nil)
        (qq-media--custom-face-refresh-owner nil)
        received)
    (cl-letf (((symbol-function 'qq-media-refresh-custom-faces)
               (lambda (callback &optional _errback _count)
                 (funcall callback '(((md5 . "one"))))
                 'request)))
      (qq-media-ensure-custom-faces (lambda (faces) (setq received faces)))
      (should (equal "one" (alist-get 'md5 (car received))))
      (should-not qq-media--custom-face-refresh-owner)
      (should-not qq-media--custom-face-waiters))))

(ert-deftest qq-media-custom-faces-loaded-p-distinguishes-empty-cache ()
  (let ((qq-media--custom-faces nil)
        (qq-media--custom-faces-fetched-at nil))
    (should-not (qq-media-custom-faces-loaded-p))
    (setq qq-media--custom-faces-fetched-at (float-time))
    (should (qq-media-custom-faces-loaded-p))
    (let ((called :missing))
      (qq-media-ensure-custom-faces
       (lambda (faces) (setq called faces)))
      (should (null called)))))

(ert-deftest qq-media-ensure-custom-faces-error-clears-waiters-and-retries ()
  (let ((qq-media--custom-faces nil)
        (qq-media--custom-faces-fetched-at nil)
        (qq-media--custom-face-waiters nil)
        (qq-media--custom-face-refresh-owner nil)
        (requests 0)
        errbacks
        failures)
    (cl-letf (((symbol-function 'qq-media-refresh-custom-faces)
               (lambda (_callback &optional errback _count)
                 (cl-incf requests)
                 (push errback errbacks)
                 'request)))
      (qq-media-ensure-custom-faces
       nil (lambda (_response reason) (push (cons 'first reason) failures)))
      (qq-media-ensure-custom-faces
       nil (lambda (_response reason) (push (cons 'second reason) failures)))
      (funcall (car errbacks) nil "offline")
      (should (= 1 requests))
      (should (equal '(first second)
                     (sort (mapcar #'car failures)
                           (lambda (left right)
                             (string-lessp (symbol-name left)
                                           (symbol-name right))))))
      (should-not qq-media--custom-face-refresh-owner)
      (should-not qq-media--custom-face-waiters)
      (qq-media-ensure-custom-faces nil #'ignore)
      (should (= 2 requests)))))

(ert-deftest qq-media-custom-face-affixation-uses-local-thumb ()
  "Favorite affix should show local thumb/file when present."
  (let* ((file (make-temp-file "qq-fav-affix" nil ".png"))
         (face `((md5 . "DEADBEEFDEADBEEF")
                 (desc . "")
                 (thumb_file . ,file)
                 (file . ,file)
                 (url . "https://example.com/x")))
         (qq-media-face-image-height 18))
    (unwind-protect
        (progn
          (with-temp-file file
            (set-buffer-multibyte nil)
            (insert (unibyte-string
                     #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
                     #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
                     #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                     #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                     #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41
                     #x54 #x08 #xd7 #x63 #xf8 #xcf #xc0 #x00
                     #x00 #x00 #x03 #x00 #x01 #x00 #x05 #xfe
                     #xd4 #xef #x00 #x00 #x00 #x00 #x49 #x45
                     #x4e #x44 #xae #x42 #x60 #x82)))
          (qq-media-custom-face-completion-table (list face))
          (let* ((label (car (car qq-media--custom-face-completion-pairs)))
                 (affixed (qq-media-custom-face-affixation-function
                           (list label
                                 "[fav] missing  (9)")))
                 (prefix0 (nth 1 (nth 0 affixed)))
                 (prefix1 (nth 1 (nth 1 affixed))))
            (should (eq (car (get-text-property 0 'display prefix0)) 'image))
            (should-not (get-text-property 0 'display prefix1))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest qq-media-normalize-custom-face-list-vector-and-single ()
  (should (= 2 (length (qq-media--normalize-custom-face-list
                        [((md5 . "A1A1A1A1") (url . "u1"))
                         ((md5 . "B2B2B2B2") (url . "u2"))]))))
  (let ((one (qq-media--normalize-custom-face-list
              '((md5 . "C3C3C3C3") (url . "u3")))))
    (should (= 1 (length one)))
    (should (equal "C3C3C3C3" (alist-get 'md5 (car one))))))

(ert-deftest qq-media-refresh-custom-faces-expands-when-full-page ()
  "When NapCat returns exactly COUNT faces, retry with a larger count."
  (let* ((qq-media-custom-face-count 2)
         (qq-media-custom-face-count-max 8)
         (qq-media--custom-faces nil)
         (calls nil)
         (final nil))
    (cl-letf (((symbol-function 'qq-api-fetch-custom-face-info)
               (lambda (callback &optional _errback count)
                 (push count calls)
                 (let* ((n (or count 2))
                        ;; First full pages, then a short one.
                        (take (if (>= n 8) 5 n))
                        (data
                         (cl-loop for i from 1 to take
                                  collect
                                  `((md5 . ,(format "MD5%06dXXXXXXXX" i))
                                    (url . ,(format "https://e/%d" i))
                                    (desc . "")))))
                   (funcall callback data)))))
      (qq-media-refresh-custom-faces
       (lambda (faces) (setq final faces)))
      (should (equal (nreverse calls) '(2 4 8)))
      (should (= 5 (length final)))
      (should (= 5 (length qq-media--custom-faces))))))

(ert-deftest qq-media-custom-face-to-segment-personal-sticker ()
  "Personal favorites become image segments with sub_type 1."
  (let* ((file (make-temp-file "qq-fav" nil ".jpg"))
         (face `((url . "https://example.com/x")
                 (file . ,file)
                 (thumb_file . ,file)
                 (desc . "meme")
                 (md5 . "ABCDEF12")
                 (emo_id . 3)
                 (is_mark_face . :false)
                 (e_id . "")
                 (ep_id . "0"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "x"))
          (let ((seg (qq-media-custom-face-to-segment face)))
            (should (equal "image" (alist-get 'type seg)))
            (should (equal 1 (alist-get 'sub_type (alist-get 'data seg))))
            (should (equal file (alist-get 'file (alist-get 'data seg))))
            (should (equal "meme" (alist-get 'summary (alist-get 'data seg))))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest qq-media-custom-face-to-segment-mark-face ()
  (let* ((face '((is_mark_face . t)
                 (e_id . "abc123")
                 (ep_id . "5")
                 (desc . "pack sticker")
                 (key . "k1")))
         (seg (qq-media-custom-face-to-segment face)))
    (should (equal "mface" (alist-get 'type seg)))
    (should (equal "abc123" (alist-get 'emoji_id (alist-get 'data seg))))
    (should (equal 5 (alist-get 'emoji_package_id (alist-get 'data seg))))))

(ert-deftest qq-media-face-uses-local-default-emoji-png ()
  "Base faces should render from LinuxQQ default-emojis without API."
  (qq-media-test-with-reset
   (let* ((dir (make-temp-file "qq-default-emojis" t))
          (png (expand-file-name "178.png" dir))
          (qq-media-default-emoji-directory dir)
          (qq-media--face-names-table (make-hash-table :test #'equal))
          (api-called nil))
     (unwind-protect
         (progn
           (puthash "178" "/斜眼笑" qq-media--face-names-table)
           ;; Minimal valid 1x1 PNG.
           (with-temp-file png
             (set-buffer-multibyte nil)
             (insert (unibyte-string
                      #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
                      #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
                      #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                      #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                      #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41
                      #x54 #x08 #xd7 #x63 #xf8 #xcf #xc0 #x00
                      #x00 #x00 #x03 #x00 #x01 #x00 #x05 #xfe
                      #xd4 #xef #x00 #x00 #x00 #x00 #x49 #x45
                      #x4e #x44 #xae #x42 #x60 #x82)))
           (cl-letf (((symbol-function 'qq-api-get-base-emoji)
                      (lambda (&rest _args)
                        (setq api-called t)
                        (ert-fail "get_base_emoji must not run when local PNG exists"))))
             (should (equal (qq-media--local-base-emoji-file "178") png))
             (should (equal (qq-media-face-text-fallback "178") "/斜眼笑"))
             (let ((image (qq-media-face-image "178")))
               (should image)
               (should (eq (car image) 'image)))
             (let ((display (qq-media-face-display-string "178")))
               (should (get-text-property 0 'display display)))
             (should-not api-called)))
       (when (file-directory-p dir)
         (delete-directory dir t))))))

(ert-deftest qq-media-resolve-fileish-prefers-existing-local-path ()
  "Outbound attach paths must not hit NapCat get_image."
  (qq-media-test-with-reset
   (let* ((local-file (make-temp-file "qq-attach" nil ".png"))
          (segment `((type . "image")
                     (data . ((file . ,local-file)
                              (name . "attach.png")
                              (url . "https://example.com/ignored.png")))))
          (api-called nil)
          result)
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "png-bytes"))
           (cl-letf (((symbol-function 'qq-api-call)
                      (lambda (&rest _args)
                        (setq api-called t)
                        (ert-fail "get_image must not run for local path"))))
             (qq-media--resolve-fileish-segment
              segment "get_image"
              (lambda (resource) (setq result resource))
              (lambda (&rest _args)
                (ert-fail "errback must not run for local path"))))
           (should-not api-called)
           (should (equal (alist-get 'file result) local-file))
           (should (equal (alist-get 'url result)
                          "https://example.com/ignored.png")))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-resolve-fileish-falls-back-to-url-after-get-image-fails ()
  (qq-media-test-with-reset
   (let* ((segment '((type . "image")
                     (data . ((file . "not-registered.jpg")
                              (url . "https://example.com/pic.jpg")))))
          (api-actions nil)
          result)
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (action _params success error)
                  (push action api-actions)
                  (funcall error nil "file not found"))))
       (qq-media--resolve-fileish-segment
        segment "get_image"
        (lambda (resource) (setq result resource))
        (lambda (&rest _args)
          (ert-fail "should fall back to url instead of errback"))))
     (should (equal api-actions '("get_image")))
     (should (equal (alist-get 'url result) "https://example.com/pic.jpg"))
     (should-not (alist-get 'file result)))))

(ert-deftest qq-media-resolve-fileish-skips-local-path-in-remote-keys ()
  (qq-media-test-with-reset
   (let* ((local-file (make-temp-file "qq-attach" nil ".png"))
          (segment `((type . "image")
                     (data . ((file_id . "remote-name.jpg")
                              (file . ,local-file)))))
          (api-files nil)
          result)
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "png-bytes"))
           ;; Local path wins entirely; remote key is not consulted.
           (cl-letf (((symbol-function 'qq-api-call)
                      (lambda (_action params _success _error)
                        (push (alist-get 'file params) api-files))))
             (qq-media--resolve-fileish-segment
              segment "get_image"
              (lambda (resource) (setq result resource))
              #'ignore))
           (should-not api-files)
           (should (equal (alist-get 'file result) local-file))
           (should (equal (qq-media--segment-remote-file-keys segment)
                          '("remote-name.jpg"))))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-segment-file-keys-ignore-empty-file-id ()
  "DataLine FILE images with an empty file_id must not share one cache key."
  (let* ((first '((type . "file")
                  (data . ((file_id . "")
                           (file . "qq-clip-first.png")
                           (path . "   ")))))
         (second '((type . "file")
                   (data . ((file_id . "")
                            (file . "qq-clip-second.png"))))))
    (should (equal (qq-media--segment-file-keys first)
                   '("qq-clip-first.png")))
    (should (equal (qq-media--segment-remote-file-keys first)
                   '("qq-clip-first.png")))
    (should (equal (qq-media-segment-preview-key first)
                   "preview:file-image:qq-clip-first.png"))
    (should (equal (qq-media-segment-preview-key second)
                   "preview:file-image:qq-clip-second.png"))
    (should-not (equal (qq-media-segment-preview-key first)
                       (qq-media-segment-preview-key second)))))

(ert-deftest qq-media-segment-preview-image-uses-local-file-without-api ()
  (qq-media-test-with-reset
   (let* ((local-file (make-temp-file "qq-preview" nil ".png"))
          (segment `((type . "image")
                     (data . ((file . ,local-file)))))
          (api-called nil)
          image)
     (unwind-protect
         (progn
           (with-temp-file local-file
             (insert "png-bytes"))
           (cl-letf (((symbol-function 'qq-api-call)
                      (lambda (&rest _args)
                        (setq api-called t)
                        nil))
                     ((symbol-function 'qq-media--preview-image-from-file)
                      (lambda (file _spec)
                        (list 'preview file))))
             (setq image (qq-media-segment-preview-image segment))
             (should (equal image (list 'preview local-file)))
             (should-not api-called)
             ;; Second call hits image cache.
             (should (equal (qq-media-segment-preview-image segment)
                            (list 'preview local-file)))))
       (when (file-exists-p local-file)
         (delete-file local-file))))))

(ert-deftest qq-media-face-completion-sorted-by-numeric-id ()
  "Base face picker must list faces in QQ id order, not hash/string order."
  (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
    (puthash "10" "/尴尬" qq-media--face-names-table)
    (puthash "2" "/色" qq-media--face-names-table)
    (puthash "0" "/惊讶" qq-media--face-names-table)
    (puthash "178" "/斜眼笑" qq-media--face-names-table)
    (let ((cands (qq-media-face-completion-candidates)))
      (should (equal cands
                     '("/惊讶  (0)"
                       "/色  (2)"
                       "/尴尬  (10)"
                       "/斜眼笑  (178)")))
      (should (equal (qq-media-face-id-from-completion (car cands)) "0"))
      (should (equal (qq-media-face-id-from-completion "/斜眼笑  (178)")
                     "178")))))

(ert-deftest qq-media-face-completion-table-metadata ()
  "Completion table must pin sort order and declare affixation."
  (let ((qq-media--face-names-table (make-hash-table :test #'equal)))
    (puthash "0" "/惊讶" qq-media--face-names-table)
    (puthash "1" "/撇嘴" qq-media--face-names-table)
    (let* ((table (qq-media-face-completion-table))
           (meta (funcall table "" nil 'metadata)))
      (should (eq (car meta) 'metadata))
      (should (eq (completion-metadata-get meta 'display-sort-function)
                  #'identity))
      (should (eq (completion-metadata-get meta 'cycle-sort-function)
                  #'identity))
      (should (eq (completion-metadata-get meta 'affixation-function)
                  #'qq-media-face-affixation-function))
      (should (equal (all-completions "" table)
                     '("/惊讶  (0)" "/撇嘴  (1)"))))))

(ert-deftest qq-media-face-affixation-uses-local-png ()
  "Picker affix should show the local default-emoji PNG when present."
  (let* ((dir (make-temp-file "qq-emoji-affix" t))
         (qq-media-default-emoji-directory dir)
         (qq-media-face-image-height 18)
         (png (expand-file-name "0.png" dir)))
    (unwind-protect
        (progn
          ;; Minimal valid 1x1 PNG.
          (with-temp-file png
            (set-buffer-multibyte nil)
            (insert (unibyte-string
                     #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a
                     #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
                     #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x01
                     #x08 #x02 #x00 #x00 #x00 #x90 #x77 #x53
                     #xde #x00 #x00 #x00 #x0c #x49 #x44 #x41
                     #x54 #x08 #xd7 #x63 #xf8 #xcf #xc0 #x00
                     #x00 #x00 #x03 #x00 #x01 #x00 #x05 #xfe
                     #xd4 #xef #x00 #x00 #x00 #x00 #x49 #x45
                     #x4e #x44 #xae #x42 #x60 #x82)))
          (let* ((affixed (qq-media-face-affixation-function
                           '("/惊讶  (0)" "/missing  (999)")))
                 (prefix0 (nth 1 (nth 0 affixed)))
                 (prefix1 (nth 1 (nth 1 affixed))))
            (should (get-text-property 0 'display prefix0))
            (should (eq (car (get-text-property 0 'display prefix0)) 'image))
            ;; Missing id keeps a plain spacer (no image property).
            (should-not (get-text-property 0 'display prefix1))))
      (when (file-directory-p dir)
        (delete-directory dir t)))))

(ert-deftest qq-media-open-resource-adapts-shared-backend-and-cache ()
  "QQ supplies only its cache policy to the shared media opener."
  (let ((qq-media-cache-directory "/tmp/qq-media-cache/")
        (qq-media--resource-cache (make-hash-table :test #'equal))
        (owner (list 'exact-owner))
        captured)
    (cl-letf (((symbol-function 'appkit-media-open-resource)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 (funcall
                  (plist-get (cdr arguments) :cache-update-function)
                  '((file . "/tmp/cat.png")
                    (url . "https://example.com/cat.png")))
                 'opened)))
      (should (eq 'opened
                  (qq-media-open-resource
                   '((url . "https://example.com/cat.png"))
                   'image
                   "image:test"
                   :owner owner)))
      (should (equal (nth 0 captured)
                     '((url . "https://example.com/cat.png"))))
      (should (eq (plist-get (cdr captured) :kind) 'image))
      (should (equal (plist-get (cdr captured) :cache-key) "image:test"))
      (should (equal (plist-get (cdr captured) :cache-directory)
                     qq-media-cache-directory))
      (should (equal (plist-get (cdr captured) :client-label) "qq"))
      (should (eq (plist-get (cdr captured) :owner) owner))
      (should (equal (alist-get 'file
                                (qq-media--cached-resource "image:test"))
                     "/tmp/cat.png")))))

(ert-deftest qq-media-open-video-file-segment-delegates-to-player ()
  "An mp4 delivered as a file segment still takes the video-player path."
  (let* ((segment '((type . "file")
                    (data . ((name . "movie.mp4")
                             (url . "https://example.com/movie.mp4")))))
         (owner (list 'exact-owner))
         played-source played-owner)
    (cl-letf (((symbol-function 'qq-media-segment-local-file)
               (lambda (_segment) nil))
              ((symbol-function 'qq-media-resolve-segment-resource)
               (lambda (_segment callback &optional _errback)
                 (funcall callback
                          '((url . "https://example.com/movie.mp4")))))
              ((symbol-function 'appkit-media-play-video-source)
               (lambda (source &optional _client-label &rest keys)
                 (setq played-source source
                       played-owner (plist-get keys :owner)))))
      (qq-media-segment-open segment :owner owner)
      (should (equal played-source "https://example.com/movie.mp4"))
      (should (eq played-owner owner)))))

(ert-deftest qq-media-video-segments-are-inline-preview-capable ()
  (should
   (qq-media-segment-preview-capable-p
    '((type . "video")
      (data . ((path . "/tmp/short.mp4")
               (file_size . "2048")
               (remote_status . "unavailable"))))))
  (should
   (qq-media-segment-preview-capable-p
    '((type . "file")
      (data . ((name . "short.mp4")
               (url . "https://example.com/short.mp4")))))))

(ert-deftest qq-media-video-preview-reuses-shared-animation-pipeline ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-preview" t))
          (source (make-temp-file "qq-short-video" nil ".mp4"))
          (segment `((type . "video")
                     (data . ((path . ,source)
                              (name . "short.mp4")
                              (file_size . "2048")
                              (duration_secs . 4.5)
                              (remote_status . "unavailable")))))
          (key (qq-media-segment-preview-key segment))
          (image '(image :type gif :appkit-media-inline-animation t))
          captured updated)
     (unwind-protect
         (cl-letf (((symbol-function 'appkit-media-start-video-preview)
                    (lambda (&rest arguments)
                      (setq captured arguments)
                      (funcall (plist-get arguments :callback)
                               image "/tmp/preview.gif")))
                     ((symbol-function 'qq-media--note-cache-updated)
                      (lambda (media-key) (setq updated media-key)))
                     ((symbol-function 'appkit-media-video-preview-display-image)
                      (lambda (candidate &optional _namespace) candidate))
                     ((symbol-function 'image-size)
                      (lambda (&rest _args) '(16 . 16))))
             (qq-media-segment-preview-image segment)
             (should (equal (plist-get captured :key) (concat "qq:" key)))
             (should (equal (plist-get captured :source) source))
             (should (equal (plist-get captured :source-size) "2048"))
             (should (= (plist-get captured :duration) 4.5))
             (should (eq image (gethash key qq-media--image-cache)))
             (should-not (gethash key qq-media--fetching-cache))
             (should (eq image (qq-media-segment-preview-image segment)))
             (should (equal updated key)))
       (when (file-exists-p source) (delete-file source))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-video-preview-uses-local-poster-without-video-source ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-poster" t))
          (poster (make-temp-file "qq-video-poster" nil ".png"))
          (segment `((type . "video")
                     (data . ((path . "/missing/video.mp4")
                              (thumb . ,poster)
                              (name . "video.mp4")
                              (file_size . "2048")
                              (remote_status . "unavailable")))))
          (image '(image :type jpeg))
          captured)
     (unwind-protect
         (cl-letf (((symbol-function 'appkit-media-start-video-preview)
                    (lambda (&rest arguments)
                      (setq captured arguments)
                      (funcall (plist-get arguments :callback)
                               image "/tmp/poster.jpg")))
                   ((symbol-function 'appkit-media-video-preview-display-image)
                    (lambda (candidate &optional _namespace) candidate)))
           (qq-media-segment-preview-image segment)
           (should-not (plist-get captured :source))
           (should (equal (plist-get captured :preview-source) poster))
           (should (equal (plist-get captured :source-size) "2048")))
       (when (file-exists-p poster) (delete-file poster))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-video-poster-never-becomes-playable-local-source ()
  (qq-media-test-with-reset
   (let* ((qq-media-cache-directory (make-temp-file "qq-video-cache" t))
          (poster (make-temp-file "qq-video-poster" nil ".jpg"))
          (segment '((type . "video")
                     (data . ((file . "video-handle")
                              (path . "/missing/video.mp4")
                              (remote_status . "unavailable")))))
          (preview-key (qq-media-segment-preview-key segment))
          (disk-poster
           (format "%s.jpg"
                   (qq-media--remote-image-cache-file-base preview-key))))
     (unwind-protect
         (progn
           (qq-media--cache-resource preview-key `((file . ,poster)))
           (should-not (qq-media-segment-local-file segment))
           (remhash preview-key qq-media--resource-cache)
           (with-temp-file disk-poster
             (insert "poster bytes"))
           (should-not (qq-media-segment-local-file segment))
           (let ((capabilities (qq-media-segment-capabilities segment)))
             (should-not (plist-get capabilities :local-file))
             (should-not (plist-get capabilities :open))))
       (when (file-exists-p poster) (delete-file poster))
       (when (file-directory-p qq-media-cache-directory)
         (delete-directory qq-media-cache-directory t))))))

(ert-deftest qq-media-resolvable-video-identities-use-the-complete-resolver ()
  (qq-media-test-with-reset
   (let* ((resolver-a
           '((kind . "message")
             (peer . ((chat_type . 2)
                      (peer_uid . "20001")
                      (guild_id . "")))
             (message_id . "9007199254745006083")
             (element_id . "9007199254745006082")))
          (resolver-b (copy-tree resolver-a))
          (first `((type . "video")
                   (data . ((file . "same-name.mp4")
                            (remote_status . "resolvable")
                            (resolver . ,resolver-a)))))
          (same-resolver `((type . "video")
                           (data . ((file . "renamed.mp4")
                                    (remote_status . "resolvable")
                                    (resolver . ,resolver-a)))))
          (reordered-resolver
           '((type . "video")
             (data . ((file . "renamed-again.mp4")
                      (remote_status . "resolvable")
                      (resolver
                       . ((element_id . "9007199254745006082")
                          (message_id . "9007199254745006083")
                          (peer . ((guild_id . "")
                                   (peer_uid . "20001")
                                   (chat_type . 2)))
                          (kind . "message")))))))
          different-resolver)
     (setf (alist-get 'element_id resolver-b) "9007199254745006000")
     (setq different-resolver
           `((type . "video")
             (data . ((file . "same-name.mp4")
                      (remote_status . "resolvable")
                      (resolver . ,resolver-b)))))
     ;; Presentation names do not split one native resource.
     (should (equal (qq-media--segment-resource-key first)
                    (qq-media--segment-resource-key same-resolver)))
     (should (equal (qq-media-segment-download-key first)
                    (qq-media-segment-download-key same-resolver)))
     (should (equal (qq-media-segment-preview-key first)
                    (qq-media-segment-preview-key same-resolver)))
     (should (equal (qq-media--segment-resource-key first)
                    (qq-media--segment-resource-key reordered-resolver)))
     ;; Conversely, the same filename cannot merge distinct message elements.
     (should-not (equal (qq-media--segment-resource-key first)
                        (qq-media--segment-resource-key different-resolver)))
     (should-not (equal (qq-media-segment-download-key first)
                        (qq-media-segment-download-key different-resolver)))
     (should-not (equal (qq-media-segment-preview-key first)
                        (qq-media-segment-preview-key different-resolver))))))

(ert-deftest qq-media-relative-data-file-remains-an-opaque-remote-handle ()
  (qq-media-test-with-reset
   (let* ((directory (make-temp-file "qq-relative-file" t))
          (default-directory directory)
          (name "opaque.mp4")
          (file (expand-file-name name directory))
          (segment `((type . "file")
                     (data . ((file . ,name)
                              (name . ,name))))))
     (unwind-protect
         (progn
           (with-temp-file file (insert "not the protocol resource"))
           (should-not (qq-media--segment-existing-path segment))
           (should-not (qq-media-segment-local-file segment))
           (should (equal (qq-media--segment-remote-file-keys segment)
                          (list name))))
       (delete-directory directory t)))))

(ert-deftest qq-media-main-video-cache-rejects-image-preview-artifacts ()
  (qq-media-test-with-reset
   (let* ((resolver
           '((kind . "snapshot")
             (peer . ((chat_type . 2)
                      (peer_uid . "20001")
                      (guild_id . "")))
             (file_uuid . "native-file-uuid")))
          (segment `((type . "video")
                     (data . ((file . "clip.mp4")
                              (remote_status . "resolvable")
                              (resolver . ,resolver)))))
          (resource-key (qq-media--segment-resource-key segment))
          (directory (make-temp-file "qq-video-main-cache" t))
          called resolved)
     (unwind-protect
         (progn
           (dolist (extension '("jpg" "gif"))
             (let ((artifact (expand-file-name
                              (format "preview.%s" extension) directory)))
               (with-temp-file artifact (insert "preview bytes"))
               (qq-media--cache-resource resource-key `((file . ,artifact)))
               (should-not (qq-media-segment-local-file segment))
               (should-not (plist-get (qq-media-segment-capabilities segment)
                                      :local-file))))
           (cl-letf (((symbol-function 'qq-api-resolve-video)
                      (lambda (called-resolver callback &optional _errback)
                        (setq called called-resolver)
                        (funcall callback
                                 '((state . "available")
                                   (url . "https://video.example/fresh"))))))
             (qq-media-resolve-segment-resource
              segment (lambda (resource) (setq resolved resource))))
           (should (equal called resolver))
           (should (equal resolved
                          '((url . "https://video.example/fresh")))))
       (delete-directory directory t)))))

(ert-deftest qq-media-real-download-invalidates-negative-video-preview ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-downloaded-video" nil ".mp4"))
          (segment '((type . "video")
                     (data . ((file . "clip.mp4")
                              (remote_status . "available")
                              (url . "https://video.example/clip")))))
          (preview-key (qq-media-segment-preview-key segment)))
     (unwind-protect
         (progn
           (puthash preview-key t qq-media--preview-missing-cache)
           (qq-media--put-segment-download-state
            segment `(:status downloaded :path ,file))
           (should-not (gethash preview-key qq-media--preview-missing-cache)))
       (delete-file file)))))

(ert-deftest qq-media-video-play-provides-an-asynchronous-error-callback ()
  (let ((segment '((type . "video") (data . nil)))
        supplied-error
        displayed)
    (cl-letf (((symbol-function 'qq-media-segment-playable-p)
               (lambda (_segment) t))
              ((symbol-function 'qq-media-segment-local-file)
               (lambda (_segment) nil))
              ((symbol-function 'qq-media-resolve-segment-resource)
               (lambda (_segment _success &optional error)
                 (setq supplied-error error)
                 (funcall error nil "manual resolution failed")))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq displayed (apply #'format format-string args)))))
      (qq-media-segment-play segment))
    (should (functionp supplied-error))
    (should (equal displayed
                   "qq: failed to play video: manual resolution failed"))))

(ert-deftest qq-media-video-play-keeps-owner-across-runtime-replacement ()
  "A late resolver callback cannot transfer its player to a same-id app."
  (let* ((segment '((type . "video") (data . nil)))
         (old-app (appkit-start-app 'qq :id 'default :shutdown #'ignore))
         (qq-runtime--app old-app)
         replacement resolver-success played-owner played-source)
    (unwind-protect
        (cl-letf (((symbol-function 'qq-media-segment-playable-p)
                   (lambda (_segment) t))
                  ((symbol-function 'qq-media-segment-local-file)
                   (lambda (_segment) nil))
                  ((symbol-function 'qq-media-resolve-segment-resource)
                   (lambda (_segment success &optional _error)
                     (setq resolver-success success)))
                  ((symbol-function 'appkit-media-play-video-source)
                   (lambda (source &optional _client-label &rest keys)
                     (setq played-source source
                           played-owner (plist-get keys :owner)))))
          (qq-media-segment-play segment :owner old-app)
          (should (functionp resolver-success))
          (appkit-stop-app old-app)
          (setq replacement
                (appkit-start-app 'qq :id 'default :shutdown #'ignore)
                qq-runtime--app replacement)
          (funcall resolver-success
                   '((url . "https://example.com/late.mp4")))
          (should (equal played-source "https://example.com/late.mp4"))
          (should (eq played-owner old-app))
          (should-not (eq played-owner replacement))
          (should-not (appkit-app-live-p old-app)))
      (when (appkit-app-live-p old-app)
        (appkit-stop-app old-app))
      (when (appkit-app-live-p replacement)
        (appkit-stop-app replacement)))))

(ert-deftest qq-media-video-process-follows-exact-account-generation ()
  "Account stop kills its real player without touching a replacement's one."
  (let ((shell (executable-find "sh"))
        (sleeper (executable-find "sleep")))
    (skip-unless (and shell sleeper))
    (let* ((source (make-temp-file "qq-media-player-" nil ".mp4"))
           ;; SOURCE is appended after these arguments.  The shell receives it
           ;; as $2 while executing the absolute sleep program from $1.
           (appkit-media-video-player-command
            (list shell "-c" "exec \"$1\" 30" "qq-media-player" sleeper))
           (segment '((type . "video") (data . nil)))
           (qq-runtime--app nil)
           old-app replacement-app old-process replacement-process)
      (unwind-protect
          (cl-letf (((symbol-function 'qq-media-segment-playable-p)
                     (lambda (_segment) t))
                    ((symbol-function 'qq-media-segment-local-file)
                     (lambda (_segment) source)))
            (setq old-app
                  (appkit-start-app 'qq :id 'default :shutdown #'ignore)
                  qq-runtime--app old-app
                  old-process
                  (qq-media-segment-play segment :owner old-app))
            (should (process-live-p old-process))
            (qq-runtime-stop)
            (should-not (process-live-p old-process))

            (setq replacement-app
                  (appkit-start-app 'qq :id 'default :shutdown #'ignore)
                  qq-runtime--app replacement-app
                  replacement-process
                  (qq-media-segment-play segment :owner replacement-app))
            (should (process-live-p replacement-process))
            ;; Re-stopping the exact old generation is inert for the same-id
            ;; replacement and its independently owned process.
            (appkit-stop-app old-app)
            (should (process-live-p replacement-process))
            (qq-runtime-stop)
            (should-not (process-live-p replacement-process)))
        (dolist (process (list old-process replacement-process))
          (when (processp process)
            (set-process-sentinel process nil)
            (when (process-live-p process)
              (delete-process process))))
        (when (appkit-app-live-p old-app)
          (appkit-stop-app old-app))
        (when (appkit-app-live-p replacement-app)
          (appkit-stop-app replacement-app))
        (when (file-exists-p source)
          (delete-file source))))))

;; Strict video remote-status model overrides for the pre-wire-model fixtures.

(ert-deftest qq-media-expired-video-does-not-resolve-a-remote-resource ()
  (qq-media-test-with-reset
   (let ((segment '((type . "video")
                    (data . ((file . "expired-video-token")
                             (remote_status . "expired")))))
         api-called resolved failure)
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (&rest _) (setq api-called t))))
       (let ((caps (qq-media-segment-capabilities segment)))
         (should (equal (plist-get caps :status) "Expired"))
         (should-not (plist-get caps :open))
         (should-not (plist-get caps :download))
         (should-not (plist-get caps :save))
         (should-not (plist-get caps :copy-url)))
       (qq-media-resolve-segment-resource
        segment
        (lambda (resource) (setq resolved resource))
        (lambda (_response reason) (setq failure reason)))
       (should-not api-called)
       (should-not resolved)
       (should (equal failure "video resource has expired"))))))

(ert-deftest qq-media-expired-video-keeps-an-existing-local-copy-usable ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-expired-video" nil ".mp4"))
          (segment `((type . "video")
                     (data . ((path . ,file)
                              (remote_status . "expired")))))
          resolved)
     (unwind-protect
         (let ((caps (qq-media-segment-capabilities segment)))
           (should (plist-get caps :open))
           (should (plist-get caps :save))
           (should-not (plist-get caps :download))
           (should-not (plist-get caps :copy-url))
           (should (equal (plist-get caps :status) "Expired"))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq resolved resource)))
           (should (equal (alist-get 'file resolved) file)))
       (when (file-exists-p file) (delete-file file))))))

(ert-deftest qq-media-nonstring-remote-status-is-invalid ()
  (let ((caps
         (qq-media-segment-capabilities
          '((type . "video")
            (data . ((file . "token") (remote_status . :false)))))))
    (should (eq (plist-get caps :remote-status) 'invalid))
    (should (equal (plist-get caps :status) "Invalid remote status"))
    (should-not (plist-get caps :open))))

(ert-deftest qq-media-video-terminal-remote-status-capabilities ()
  (dolist (case
           '(("unavailable" "Unavailable" "video resource is unavailable")
             ("unresolved" "Unresolved" "video resource is unresolved")))
    (pcase-let ((`(,wire ,status ,reason) case))
      (let* ((segment `((type . "video")
                        (data . ((file . "remote-token")
                                 (remote_status . ,wire)))))
             (caps (qq-media-segment-capabilities segment))
             api-called failure)
        (should (equal (plist-get caps :status) status))
        (dolist (key '(:open :download :save :copy-url :resolve-remote))
          (should-not (plist-get caps key)))
        (cl-letf (((symbol-function 'qq-api-call)
                   (lambda (&rest _) (setq api-called t))))
          (qq-media-resolve-segment-resource
           segment #'ignore
           (lambda (_response text) (setq failure text))))
        (should-not api-called)
        (should (equal failure reason))))))

(ert-deftest qq-media-resolvable-video-uses-only-exact-manual-capability ()
  (qq-media-test-with-reset
   (let* ((resolver
           '((kind . "message")
             (peer . ((chat_type . 2)
                      (peer_uid . "20001")
                      (guild_id . "")))
             (message_id . "9007199254745006083")
             (element_id . "9007199254745006082")))
          (segment
           `((type . "video")
             (data . ((file . "must-not-go-to-get-file")
                      (remote_status . "resolvable")
                      (resolver . ,resolver)))))
          calls resources generic-called)
     (let ((caps (qq-media-segment-capabilities segment)))
       (should (eq (plist-get caps :remote-status) 'resolvable))
       (dolist (key '(:open :download :save :resolve-remote))
         (should (plist-get caps key)))
       (should-not (plist-get caps :copy-url))
       (should-not (plist-get caps :status)))
     (cl-letf (((symbol-function 'qq-api-resolve-video)
                (lambda (called-resolver callback &optional _errback)
                  (push (copy-tree called-resolver) calls)
                  (funcall callback
                           '((state . "available")
                             (url . "https://video.example/manual")))))
               ((symbol-function 'qq-media--fetch-segment-resource)
                (lambda (&rest _)
                  (setq generic-called t))))
       ;; Every explicit operation requests a fresh signed URL rather than
       ;; persisting one and guessing its expiry.
       (dotimes (_ 2)
         (qq-media-resolve-segment-resource
          segment (lambda (resource) (push resource resources)))))
     (should-not generic-called)
     (should (equal calls (list resolver resolver)))
     (should
      (equal resources
             '(((url . "https://video.example/manual"))
               ((url . "https://video.example/manual"))))))))

(ert-deftest qq-media-resolvable-video-propagates-native-terminal-state ()
  (let* ((resolver
          '((kind . "snapshot")
            (peer . ((chat_type . 2)
                     (peer_uid . "20001")
                     (guild_id . "")))
            (file_uuid . "native-file-uuid")))
         (segment
          `((type . "video")
            (data . ((file . "video.mp4")
                     (remote_status . "resolvable")
                     (resolver . ,resolver)))))
         resolved failure)
    (cl-letf (((symbol-function 'qq-api-resolve-video)
               (lambda (_resolver callback &optional _errback)
                 (funcall callback '((state . "expired"))))))
      (qq-media-resolve-segment-resource
       segment
       (lambda (resource) (setq resolved resource))
       (lambda (_response reason) (setq failure reason))))
    (should-not resolved)
    (should (equal failure "video resource has expired"))))

(ert-deftest qq-media-resolvable-video-prefers-a-real-local-file ()
  (qq-media-test-with-reset
   (let* ((file (make-temp-file "qq-local-video" nil ".mp4"))
          (segment
           `((type . "video")
             (data . ((file . "video.mp4")
                      (path . ,file)
                      (remote_status . "resolvable")
                      (resolver
                       . ((kind . "snapshot")
                          (peer . ((chat_type . 2)
                                   (peer_uid . "20001")
                                   (guild_id . "")))
                          (file_uuid . "native-file-uuid")))))))
          api-called resolved)
     (unwind-protect
         (cl-letf (((symbol-function 'qq-api-resolve-video)
                    (lambda (&rest _args) (setq api-called t))))
           (qq-media-resolve-segment-resource
            segment (lambda (resource) (setq resolved resource)))
           (should-not api-called)
           (should (equal (alist-get 'file resolved) file)))
       (when (file-exists-p file) (delete-file file))))))

(ert-deftest qq-media-available-video-uses-only-wire-url ()
  (qq-media-test-with-reset
   (let* ((url "https://example.com/movie.mp4")
          (segment `((type . "video")
                     (data . ((file . "must-not-go-to-get-file")
                              (url . ,url)
                              (remote_status . "available")))))
          api-called resolved)
     (let ((caps (qq-media-segment-capabilities segment)))
       (dolist (key '(:open :download :save :copy-url :resolve-remote))
         (should (plist-get caps key)))
       (should (equal (plist-get caps :remote-url) url)))
     (cl-letf (((symbol-function 'qq-api-call)
                (lambda (&rest _) (setq api-called t))))
       (qq-media-resolve-segment-resource
        segment (lambda (resource) (setq resolved resource))))
     (should-not api-called)
     (should (equal resolved `((url . ,url)))))))

(ert-deftest qq-media-video-missing-status-or-available-without-url-is-invalid ()
  (dolist (segment
           '(((type . "video") (data . ((file . "token"))))
             ((type . "video")
              (data . ((file . "token") (remote_status . "bogus"))))
             ((type . "video")
              (data . ((file . "token") (remote_status . "available"))))))
    (let ((caps (qq-media-segment-capabilities segment)))
      (should-not (plist-get caps :resolve-remote))
      (should-not (plist-get caps :open))
      (if (equal (alist-get 'remote_status (alist-get 'data segment))
                 "available")
          (should (equal (plist-get caps :remote-error)
                         "available video resource has no URL"))
        (should (eq (plist-get caps :remote-status) 'invalid))))))

(ert-deftest qq-media-segment-kind-recognizes-media-file-urls ()
  (should
   (eq (qq-media-segment-kind
        '((type . "file")
          (data . ((file . "https://example.com/picture.gif?token=1")))))
       'image))
  (let ((video '((type . "file")
                 (data . ((name . "movie.MP4#fragment")
                          (url . "https://example.com/movie.mp4"))))))
    (should (eq (qq-media-segment-kind video) 'video))
    (should (qq-media-segment-playable-p video))))

(ert-deftest qq-media-download-setup-error-is-retryable ()
  (qq-media-test-with-reset
   (let* ((path (make-temp-name "/tmp/qq-media-download-error-"))
          (segment '((type . "file")
                     (data . ((file . "remote-token")
                              (name . "report.pdf")))))
          (capabilities
           `(:download t
             :download-state (:status not-downloaded :path ,path))))
     (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                (lambda (_segment) capabilities))
               ((symbol-function 'qq-media-resolve-segment-resource)
                (lambda (_segment success &optional _error)
                  (funcall success
                           '((url . "https://example.invalid/report.pdf")))))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _arguments)
                  (error "queue unavailable")))
               ((symbol-function 'message) #'ignore))
       (qq-media-segment-start-download segment)
       (let ((state (qq-media-segment-download-state segment)))
         (should (eq 'error (plist-get state :status)))
         (should (string-match-p "queue unavailable"
                                 (plist-get state :error))))))))

(ert-deftest qq-media-segment-download-retains-cancelable-transfer-handle ()
  (qq-media-test-with-reset
   (let* ((path (make-temp-name "/tmp/qq-media-download-handle-"))
          (segment '((type . "file")
                     (data . ((file . "remote-token")
                              (name . "report.pdf")))))
          (capabilities
           `(:download t
             :download-state (:status not-downloaded :path ,path))))
     (cl-letf (((symbol-function 'qq-media-segment-capabilities)
                (lambda (_segment) capabilities))
               ((symbol-function 'qq-media-resolve-segment-resource)
                (lambda (_segment success &optional _error)
                  (funcall success
                           '((url . "https://example.invalid/report.pdf")))))
               ((symbol-function
                 'appkit-media-copy-or-download-resource-async)
                (lambda (&rest _arguments) 'download-handle))
               ((symbol-function 'message) #'ignore))
       (qq-media-segment-start-download segment)
       (let ((state (qq-media-segment-download-state segment)))
         (should (eq 'downloading (plist-get state :status)))
         (should (eq 'download-handle (plist-get state :transfer)))
         (should (symbolp (plist-get state :token))))))))

(ert-deftest qq-media-cached-avatar-rendering-never-starts-fetches ()
  (let (api-called)
    (cl-letf (((symbol-function 'qq-media--cached-image) (lambda (_key) nil))
              ((symbol-function 'qq-api-get-avatar)
               (lambda (&rest _args) (setq api-called t)))
              ((symbol-function 'qq-api-get-group-avatar)
               (lambda (&rest _args) (setq api-called t))))
      (should (equal (qq-media-avatar-cached-display-string "10001") "@"))
      (should (equal (qq-media-group-avatar-cached-display-string "20001") "#"))
      (should-not api-called))))

(provide 'qq-media-test)

;;; qq-media-test.el ends here
