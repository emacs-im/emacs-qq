;;; qq-media-test.el --- Tests for qq-media -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'qq-media)

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
  (let ((qq-media--remote-image-plz-queue 'image-queue)
        cleared)
    (cl-letf (((symbol-function 'plz-clear)
               (lambda (queue) (push queue cleared))))
      (qq-media-clear-cache)
      (should (= (length cleared) 1))
      (should (memq 'image-queue cleared))
      (should-not qq-media--remote-image-plz-queue))))

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
        captured)
    (cl-letf (((symbol-function 'disco-media-open-resource)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 (funcall
                  (plist-get (nthcdr 3 arguments) :cache-update-function)
                  '((file . "/tmp/cat.png")
                    (url . "https://example.com/cat.png")))
                 'opened)))
      (should (eq 'opened
                  (qq-media-open-resource
                   '((url . "https://example.com/cat.png"))
                   'image
                   "image:test")))
      (should (equal (nth 0 captured)
                     '((url . "https://example.com/cat.png"))))
      (should (eq (nth 1 captured) 'image))
      (should (equal (nth 2 captured) "image:test"))
      (should (equal (plist-get (nthcdr 3 captured) :cache-directory)
                     qq-media-cache-directory))
      (should (equal (plist-get (nthcdr 3 captured) :client-label) "qq"))
      (should (equal (alist-get 'file
                                (qq-media--cached-resource "image:test"))
                     "/tmp/cat.png")))))

(ert-deftest qq-media-open-video-file-segment-delegates-to-player ()
  "An mp4 delivered as a file segment still takes the video-player path."
  (let* ((segment '((type . "file")
                    (data . ((name . "movie.mp4")
                             (url . "https://example.com/movie.mp4")))))
         played-source)
    (cl-letf (((symbol-function 'qq-media-segment-local-file)
               (lambda (_segment) nil))
              ((symbol-function 'qq-media-resolve-segment-resource)
               (lambda (_segment callback &optional _errback)
                 (funcall callback
                          '((url . "https://example.com/movie.mp4")))))
              ((symbol-function 'disco-media-play-video-source)
               (lambda (source)
                 (setq played-source source))))
      (qq-media-segment-open segment)
      (should (equal played-source "https://example.com/movie.mp4")))))

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

(ert-deftest qq-media-video-remote-status-four-state-capabilities ()
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

(provide 'qq-media-test)

;;; qq-media-test.el ends here
