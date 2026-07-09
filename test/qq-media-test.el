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

(provide 'qq-media-test)

;;; qq-media-test.el ends here
