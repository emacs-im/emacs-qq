;;; qq-media-test.el --- Tests for qq-media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-media)

(defmacro qq-media-test-with-reset (&rest body)
  "Run BODY with clean qq-media caches and rerender hooks disabled."
  `(let ((qq-media-cache-update-hook nil)
         (qq-media-rerender-function nil))
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

(provide 'qq-media-test)

;;; qq-media-test.el ends here
