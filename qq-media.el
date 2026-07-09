;;; qq-media.el --- QQ-specific media helpers for emacs-qq -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Resource helpers inspired by disco-media.el, specialized for NapCat/QQ
;; resource types such as avatars, base emojis, and OneBot file/image segments.

;;; Code:

(require 'browse-url)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-handlers)
(require 'disco-media)
(require 'qq-api)
(require 'qq-customize)

(defvar qq-media-cache-update-hook nil
  "Hook run after media resource/image cache updates.")

(defvar qq-media-rerender-function nil
  "Compatibility function called after media resource/image cache updates.")

(defvar qq-media--resource-cache (make-hash-table :test #'equal)
  "Simple in-memory resource cache keyed by logical resource identity.")

(defvar qq-media--image-cache (make-hash-table :test #'equal)
  "In-memory image object cache keyed by logical resource identity.")

(defvar qq-media--fetching-cache (make-hash-table :test #'equal)
  "Set of logical resource identities currently being fetched.")

(defvar qq-media--download-state-table (make-hash-table :test #'equal)
  "Download state plist table keyed by QQ media logical identity.")

(defconst qq-media--remote-image-cache-extensions
  '("webp" "png" "jpg" "jpeg" "gif" "img")
  "Preferred file extensions for cached remote QQ image resources.")

(defvar qq-media--remote-image-plz-queue nil
  "Shared plz queue used for remote QQ image downloads.")

(defvar qq-media--remote-image-plz-queue-limit nil
  "Last applied queue limit for `qq-media--remote-image-plz-queue'.")

(defun qq-media-clear-cache ()
  "Clear cached resource metadata and disk-backed remote image cache."
  (interactive)
  (clrhash qq-media--resource-cache)
  (clrhash qq-media--image-cache)
  (clrhash qq-media--fetching-cache)
  (clrhash qq-media--download-state-table)
  (setq qq-media--remote-image-plz-queue nil)
  (setq qq-media--remote-image-plz-queue-limit nil)
  (when (file-directory-p qq-media-cache-directory)
    (ignore-errors (delete-directory qq-media-cache-directory t)))
  (message "qq: media cache cleared"))

(defun qq-media-url-present-p (url)
  "Return non-nil when URL is a non-empty string."
  (and (stringp url)
       (not (string-empty-p url))))

(defun qq-media-file-present-p (file)
  "Return non-nil when FILE exists locally."
  (and (stringp file)
       (file-exists-p file)))

(defun qq-media-open-resource (resource)
  "Open RESOURCE preferring a local file, falling back to URL.

RESOURCE is an alist that may contain `file' and `url'."
  (let ((file (alist-get 'file resource))
        (url (alist-get 'url resource)))
    (cond
     ((qq-media-file-present-p file)
      (find-file file))
     ((qq-media-url-present-p url)
      (browse-url url t))
     (t
      (user-error "qq: resource has neither local file nor URL")))))

(defun qq-media--cached-resource (key)
  "Return cached resource for KEY when still usable."
  (let ((resource (copy-tree (gethash key qq-media--resource-cache))))
    (when resource
      (if (or (qq-media-file-present-p (alist-get 'file resource))
              (qq-media-url-present-p (alist-get 'url resource)))
          resource
        (remhash key qq-media--resource-cache)
        nil))))

(defun qq-media--cache-resource (key resource)
  "Store RESOURCE under KEY and return RESOURCE."
  (puthash key (copy-tree resource) qq-media--resource-cache)
  resource)

(defun qq-media--cached-image (key)
  "Return cached image object for KEY when valid."
  (let ((image (gethash key qq-media--image-cache)))
    (when image
      (condition-case _
          (progn
            (image-size image t)
            image)
        (error
         (remhash key qq-media--image-cache)
         nil)))))

(defun qq-media--cache-image (key image)
  "Store IMAGE object under KEY and return IMAGE."
  (when image
    (puthash key image qq-media--image-cache))
  image)

(defun qq-media--note-cache-updated (&optional media-key)
  "Notify UI that media cache content changed.

When MEDIA-KEY is non-nil, it identifies the logical cache entry that changed."
  (run-hook-with-args 'qq-media-cache-update-hook media-key)
  (when (functionp qq-media-rerender-function)
    (funcall qq-media-rerender-function)))

(defun qq-media--image-from-file (file height)
  "Create an Emacs image object from FILE at pixel HEIGHT, or nil."
  (when (qq-media-file-present-p file)
    (condition-case _
        (create-image file nil nil :height (max 1 height) :ascent 'center)
      (error nil))))

(defun qq-media--preview-image-from-file (file _spec)
  "Create a preview image object from FILE using disco-media helpers."
  (when (qq-media-file-present-p file)
    (disco-media-preview-image-from-file
     file
     qq-media-preview-image-max-width
     qq-media-preview-image-height)))

(defun qq-media--image-display-string (image fallback)
  "Return display string for IMAGE, or FALLBACK when IMAGE is nil."
  (if image
      (propertize (or fallback " ") 'display image 'rear-nonsticky '(display))
    fallback))

(defun qq-media--remote-image-cache-file-base (key)
  "Return disk cache file base for remote image KEY."
  (expand-file-name (md5 key) qq-media-cache-directory))

(defun qq-media--remote-image-cache-file (key extension)
  "Return disk cache file path for remote image KEY and EXTENSION."
  (format "%s.%s"
          (qq-media--remote-image-cache-file-base key)
          extension))

(defun qq-media--remote-image-cache-existing-file (key)
  "Return existing disk cache file for remote image KEY, or nil."
  (let (found)
    (dolist (ext qq-media--remote-image-cache-extensions found)
      (let ((candidate (qq-media--remote-image-cache-file key ext)))
        (when (and (null found) (file-exists-p candidate))
          (setq found candidate))))))

(defun qq-media--remote-image-delete-stale-cache-files (cache-base)
  "Delete stale remote image cache files rooted at CACHE-BASE."
  (dolist (ext qq-media--remote-image-cache-extensions)
    (let ((old-file (format "%s.%s" cache-base ext)))
      (when (file-exists-p old-file)
        (ignore-errors (delete-file old-file))))))

(defun qq-media--remote-image-ensure-queue ()
  "Return active plz queue for remote QQ image downloads."
  (let ((limit 8))
    (when (or (null qq-media--remote-image-plz-queue)
              (not (equal qq-media--remote-image-plz-queue-limit limit)))
      (setq qq-media--remote-image-plz-queue (make-plz-queue :limit limit))
      (setq qq-media--remote-image-plz-queue-limit limit))
    qq-media--remote-image-plz-queue))

(defun qq-media--prefer-remote-image-resource-p (key resource)
  "Return non-nil when RESOURCE at KEY should prefer remote image refresh.

Currently this is enabled for user avatars so stale NapCat local avatar
files do not permanently override fresher public avatar URLs."
  (and (stringp key)
       (string-prefix-p "avatar:" key)
       resource
       (qq-media-url-present-p (alist-get 'url resource))))

(defun qq-media--resource-image-file (key resource)
  "Return local image file for RESOURCE at KEY, consulting disk cache when needed."
  (let* ((cached-file (and key (qq-media--remote-image-cache-existing-file key)))
         (prefer-remote (qq-media--prefer-remote-image-resource-p key resource))
         (file (alist-get 'file resource)))
    (cond
     ((qq-media-file-present-p cached-file)
      (setf (alist-get 'file resource nil nil #'eq) cached-file)
      (qq-media--cache-resource key resource)
      cached-file)
     ((and prefer-remote (qq-media-url-present-p (alist-get 'url resource)))
      nil)
     ((qq-media-file-present-p file)
      file)
     (t nil))))

(defun qq-media--finish-resource-image-fetch (key &optional file image resource)
  "Finalize image fetch for KEY using FILE, IMAGE, and RESOURCE."
  (when (and resource file)
    (setf (alist-get 'file resource nil nil #'eq) file)
    (qq-media--cache-resource key resource))
  (when image
    (qq-media--cache-image key image)
    (qq-media--note-cache-updated key))
  (remhash key qq-media--fetching-cache))

(defun qq-media--start-resource-image-download (key resource spec builder)
  "Download remote image RESOURCE for KEY, then build image with BUILDER."
  (let* ((url (alist-get 'url resource))
         (cache-base (qq-media--remote-image-cache-file-base key))
         (cache-file (qq-media--resource-image-file key resource))
         (cached-image (and cache-file (funcall builder cache-file spec))))
    (cond
     (cached-image
      (qq-media--finish-resource-image-fetch key cache-file cached-image resource))
     ((not (qq-media-url-present-p url))
      (qq-media--finish-resource-image-fetch key nil nil resource))
     (t
      (when cache-file
        (ignore-errors (delete-file cache-file))
        (setf (alist-get 'file resource nil nil #'eq) nil)
        (qq-media--cache-resource key resource))
      (let ((queue (qq-media--remote-image-ensure-queue))
            (headers '(("Accept" . "image/png,image/webp,image/*;q=0.8,*/*;q=0.1"))))
        (condition-case err
            (progn
              (plz-queue
                queue
                'get
                url
                :headers headers
                :as 'binary
                :noquery t
                :then
                (lambda (data)
                  (let* ((raw-bytes
                          (disco-media-normalize-image-bytes
                           (if (multibyte-string-p data)
                               (encode-coding-string data 'binary)
                             data)))
                         (extension (disco-media-bytes->extension raw-bytes "img"))
                         (target-file (format "%s.%s" cache-base extension))
                         (image nil))
                    (qq-media--remote-image-delete-stale-cache-files cache-base)
                    (make-directory (file-name-directory target-file) t)
                    (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert raw-bytes)
                      (let ((coding-system-for-write 'binary))
                        (write-region (point-min) (point-max) target-file nil 'silent)))
                    (setq image (funcall builder target-file spec))
                    (qq-media--finish-resource-image-fetch key target-file image resource)))
                :else
                (lambda (_err)
                  (qq-media--finish-resource-image-fetch key nil nil resource)))
              (plz-run queue))
          (error
           (message "qq: failed to queue remote image fetch for %s: %s"
                    key
                    (error-message-string err))
           (qq-media--finish-resource-image-fetch key nil nil resource))))))))

(defun qq-media--ensure-resource-image (key fetcher spec &optional image-builder)
  "Return cached image for KEY, triggering FETCHER when needed.

FETCHER must accept SUCCESS and ERROR callbacks.  SUCCESS receives one
resource alist.  SPEC is forwarded to IMAGE-BUILDER, which defaults to
`qq-media--image-from-file'."
  (let ((builder (or image-builder #'qq-media--image-from-file)))
    (or (qq-media--cached-image key)
        (let* ((resource (qq-media--cached-resource key))
               (original-file (and resource (alist-get 'file resource)))
               (file (and resource (qq-media--resource-image-file key resource))))
          (cond
           ((qq-media-file-present-p file)
            (qq-media--cache-image key (funcall builder file spec)))
           ((gethash key qq-media--fetching-cache)
            nil)
           ((and resource
                 (qq-media--prefer-remote-image-resource-p key resource)
                 (qq-media-file-present-p original-file))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            (qq-media--cache-image key (funcall builder original-file spec)))
           ((and resource (qq-media-url-present-p (alist-get 'url resource)))
            (puthash key t qq-media--fetching-cache)
            (qq-media--start-resource-image-download key resource spec builder)
            nil)
           (t
            (puthash key t qq-media--fetching-cache)
            (funcall
             fetcher
             (lambda (fetched-resource)
               (let* ((resource* (qq-media--cache-resource key fetched-resource))
                      (original-file* (and resource* (alist-get 'file resource*)))
                      (file* (and resource* (qq-media--resource-image-file key resource*))))
                 (cond
                  ((qq-media-file-present-p file*)
                   (let ((image (funcall builder file* spec)))
                     (when image
                       (qq-media--cache-image key image)
                       (qq-media--note-cache-updated key))
                     (remhash key qq-media--fetching-cache)))
                  ((and (qq-media--prefer-remote-image-resource-p key resource*)
                        (qq-media-file-present-p original-file*))
                   (let ((image (funcall builder original-file* spec)))
                     (when image
                       (qq-media--cache-image key image)
                       (qq-media--note-cache-updated key)))
                   (qq-media--start-resource-image-download key resource* spec builder))
                  ((qq-media-url-present-p (alist-get 'url resource*))
                   (qq-media--start-resource-image-download key resource* spec builder))
                  (t
                   (remhash key qq-media--fetching-cache)))))
             (lambda (_response _reason)
               (remhash key qq-media--fetching-cache)))
            nil))))))

(defun qq-media--resource-fetching-p (key)
  "Return non-nil when KEY is currently being fetched."
  (gethash key qq-media--fetching-cache))

(defun qq-media--resolve-resource (key fetcher callback)
  "Resolve resource by KEY using FETCHER, then run CALLBACK.

FETCHER is called with a one-argument callback that receives the resource
alist returned by NapCat."
  (if-let* ((cached (qq-media--cached-resource key)))
      (funcall callback cached)
    (funcall fetcher
             (lambda (resource)
               (let ((cached-resource (qq-media--cache-resource key resource)))
                 (qq-media--note-cache-updated key)
                 (funcall callback cached-resource))))))

(defun qq-media--segment-file-keys (segment)
  "Return candidate file keys from SEGMENT, in preference order."
  (let* ((data (alist-get 'data segment))
         (keys (list (alist-get 'file_id data)
                     (alist-get 'file data)
                     (alist-get 'path data))))
    (delete-dups (delq nil keys))))

(defun qq-media--segment-file-key (segment)
  "Return best file key from SEGMENT."
  (car (qq-media--segment-file-keys segment)))

(defun qq-media--segment-existing-path (segment)
  "Return an existing local filesystem path from SEGMENT, or nil.

Outbound attach/pending segments carry absolute paths in `file' or `path'.
Those must never be sent to NapCat `get_image'/`get_file' (Telega-style
local-first rendering)."
  (let* ((data (alist-get 'data segment))
         (candidates (list (alist-get 'path data)
                           (alist-get 'file data))))
    (catch 'found
      (dolist (candidate candidates)
        (when (qq-media-file-present-p candidate)
          (throw 'found candidate)))
      nil)))

(defun qq-media--segment-remote-file-keys (segment)
  "Return SEGMENT file keys that are not existing local paths.

Only these keys are safe to pass to NapCat `get_image'/`get_file'."
  (let (remote)
    (dolist (key (qq-media--segment-file-keys segment))
      (unless (qq-media-file-present-p key)
        (push key remote)))
    (nreverse remote)))

(defun qq-media--resource-from-local+url (local url)
  "Build a resource alist from LOCAL path and optional URL."
  (append
   (when (qq-media-file-present-p local)
     `((file . ,local)))
   (when (qq-media-url-present-p url)
     `((url . ,url)))))

(defun qq-media--resolve-fileish-segment (segment action callback errback &optional final-error)
  "Resolve file-like SEGMENT with Telega-style priority.

Order:
1. Existing local path on the segment (outbound attach / pending)
2. NapCat ACTION (`get_image'/`get_file') with remote file keys only
3. Direct `url' from the segment
4. ERRBACK

NapCat/NT still owns remote materialization and QQ disk cache; the client
never treats a local absolute path as a NapCat file id."
  (let* ((url (qq-media--segment-url segment))
         (local (qq-media--segment-existing-path segment))
         (remote-keys (qq-media--segment-remote-file-keys segment))
         (error-fn (or errback #'qq-api--default-error))
         (fail-msg (or final-error
                       "media segment has neither local file, file id, nor URL")))
    (cond
     (local
      (funcall callback (qq-media--resource-from-local+url local url)))
     (remote-keys
      (qq-media--call-fileish-action
       action remote-keys
       callback
       (lambda (response reason)
         (if (qq-media-url-present-p url)
             (funcall callback `((url . ,url)))
           (funcall error-fn response (or reason fail-msg))))
       fail-msg))
     ((qq-media-url-present-p url)
      (funcall callback `((url . ,url))))
     (t
      (funcall error-fn nil fail-msg)))))

(defun qq-media--image-file-name-p (filename)
  "Return non-nil when FILENAME looks like an image file."
  (and (stringp filename)
       (string-match-p
        (rx "." (or "png" "jpg" "jpeg" "gif" "webp" "bmp" "heic" "heif") string-end)
        (downcase filename))))

(defun qq-media-imageish-file-segment-p (segment)
  "Return non-nil when SEGMENT is a file that should preview like an image."
  (and (equal (alist-get 'type segment) "file")
       (let* ((data (alist-get 'data segment))
              (name (or (alist-get 'name data)
                        (alist-get 'file data))))
         (qq-media--image-file-name-p name))))

(defun qq-media-segment-preview-capable-p (segment)
  "Return non-nil when SEGMENT supports inline preview rendering."
  (or (member (alist-get 'type segment) '("image" "mface"))
      (qq-media-imageish-file-segment-p segment)))

(defun qq-media--call-fileish-action (action file-keys callback errback &optional final-error)
  "Call ACTION with FILE-KEYS until one succeeds.

On success pass decoded data to CALLBACK.  When all candidates fail, call
ERRBACK with FINAL-ERROR or the last backend reason."
  (letrec ((try-next
            (lambda (keys last-reason last-response)
              (if (null keys)
                  (funcall errback last-response (or final-error last-reason "file not found"))
                (qq-api-call
                 action
                 `((file . ,(car keys)))
                 (lambda (response)
                   (funcall callback (qq-api--response-data response)))
                 (lambda (response reason)
                   (funcall try-next (cdr keys) reason response)))))))
    (funcall try-next file-keys nil nil)))

(defun qq-media--segment-url (segment)
  "Return best direct URL from SEGMENT, or nil."
  (alist-get 'url (alist-get 'data segment)))

(defun qq-media--segment-resource-key (segment)
  "Return logical resource cache key for SEGMENT, or nil."
  (let* ((type (alist-get 'type segment))
         (file-key (qq-media--segment-file-key segment))
         (url (qq-media--segment-url segment))
         (data (alist-get 'data segment))
         (emoji-id (alist-get 'id data)))
    (pcase type
      ("image" (or (and file-key (format "image:%s" file-key))
                   (and (qq-media-url-present-p url) (format "image-url:%s" url))))
      ((or "file" "video")
       (or (and file-key (format "%s:%s" type file-key))
           (and (qq-media-url-present-p url) (format "%s-url:%s" type url))))
      ("record" (and file-key (format "record:%s" file-key)))
      ("face" (and emoji-id (format "face:%s" emoji-id)))
      ("mface" (or (and file-key (format "mface:%s" file-key))
                   (and (qq-media-url-present-p url) (format "mface-url:%s" url))))
      (_ nil))))

(defun qq-media--fetch-segment-resource (segment callback &optional errback)
  "Fetch media resource for SEGMENT and pass it to CALLBACK.

Uses local path → NapCat get_* → URL (see `qq-media--resolve-fileish-segment')."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (emoji-id (alist-get 'id data))
         (error-fn (or errback #'qq-api--default-error)))
    (pcase type
      ("image"
       (qq-media--resolve-fileish-segment
        segment "get_image" callback error-fn
        "image segment has neither local file, file id, nor URL"))
      ((or "file" "video")
       (qq-media--resolve-fileish-segment
        segment
        (if (qq-media-imageish-file-segment-p segment)
            "get_image"
          "get_file")
        callback error-fn
        (format "%s segment has neither local file, file id, nor URL" type)))
      ("record"
       (let ((remote-keys (qq-media--segment-remote-file-keys segment))
             (local (qq-media--segment-existing-path segment)))
         (cond
          (local
           (funcall callback `((file . ,local))))
          (remote-keys
           (letrec ((try-next
                     (lambda (keys last-reason last-response)
                       (if (null keys)
                           (funcall error-fn last-response
                                    (or last-reason "record segment has no usable file id"))
                         (qq-api-call
                          "get_record"
                          `((file . ,(car keys))
                            (out_format . "mp3"))
                          (lambda (response)
                            (funcall callback (qq-api--response-data response)))
                          (lambda (response reason)
                            (funcall try-next (cdr keys) reason response)))))))
             (funcall try-next remote-keys nil nil)))
          (t
           (funcall error-fn nil "record segment has no file id")))))
      ("face"
       (cond
        ((not emoji-id)
         (funcall error-fn nil "face segment has no id"))
        ((qq-media--face-resource-from-local emoji-id)
         (funcall callback (qq-media--face-resource-from-local emoji-id)))
        (t
         (qq-api-get-base-emoji emoji-id callback error-fn))))
      ("mface"
       (qq-media--resolve-fileish-segment
        segment "get_image" callback error-fn
        "mface segment has neither local file, file id, nor URL"))
      (_
       (funcall error-fn nil (format "unsupported segment type for resource fetch: %s" type))))))

(defun qq-media-resolve-segment-resource (segment callback &optional errback)
  "Resolve media resource for SEGMENT and pass it to CALLBACK."
  (let ((cache-key (qq-media--segment-resource-key segment))
        (url (qq-media--segment-url segment))
        (local (qq-media--segment-existing-path segment)))
    (cond
     ((and cache-key (qq-media--cached-resource cache-key))
      (funcall callback (qq-media--cached-resource cache-key)))
     ;; Local-first even when no logical cache key is available.
     (local
      (let ((resource (qq-media--resource-from-local+url local url)))
        (when cache-key
          (qq-media--cache-resource cache-key resource)
          (qq-media--note-cache-updated cache-key))
        (funcall callback resource)))
     (cache-key
      (qq-media--fetch-segment-resource
       segment
       (lambda (resource)
         (let ((cached-resource (qq-media--cache-resource cache-key resource)))
           (qq-media--note-cache-updated cache-key)
           (funcall callback cached-resource)))
       errback))
     ((qq-media-url-present-p url)
      (funcall callback `((url . ,url))))
     (errback
      (funcall errback nil "resource has neither local file, cache key, nor URL"))
     (t
      (user-error "qq: segment has neither local file nor URL")))))

(defun qq-media-segment-open (segment)
  "Open OneBot message SEGMENT using QQ-aware resource resolution."
  (qq-media-resolve-segment-resource segment #'qq-media-open-resource))

(defun qq-media-segment-openable-p (segment)
  "Return non-nil when SEGMENT can be opened via `qq-media'."
  (member (alist-get 'type segment)
          '("image" "file" "record" "video" "face" "mface")))

(defun qq-media-segment-playable-p (segment)
  "Return non-nil when SEGMENT supports `qq-media-segment-play'."
  (equal (alist-get 'type segment) "video"))

(defun qq-media--sanitize-filename (filename)
  "Return filesystem-safe variant of FILENAME."
  (replace-regexp-in-string
   "[[:cntrl:]/\\\\]+"
   "_"
   (or filename "qq-media.bin")))

(defun qq-media--segment-url-filename (url)
  "Extract a best-effort filename from URL."
  (let* ((base (car (split-string (or url "") "[?#]")))
         (name (file-name-nondirectory base)))
    (and (stringp name)
         (not (string-empty-p name))
         name)))

(defun qq-media-segment-default-save-name (segment)
  "Return default filename for saving SEGMENT locally."
  (let* ((data (alist-get 'data segment))
         (type (or (alist-get 'type segment) "media"))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (seed (or (qq-media--segment-file-key segment)
                   (alist-get 'id data)
                   (alist-get 'emoji_id data)
                   (substring (md5 (prin1-to-string segment)) 0 8)))
         (name (or (alist-get 'name data)
                   (alist-get 'file data)
                   (and cached-resource
                        (let ((file (alist-get 'file cached-resource)))
                          (and (stringp file) (file-name-nondirectory file))))
                   (qq-media--segment-url-filename (qq-media--segment-url segment))
                   (format "%s-%s.bin" type seed))))
    (qq-media--sanitize-filename name)))

(defun qq-media-segment-download-key (segment)
  "Return stable download-state key for SEGMENT."
  (or (qq-media--segment-resource-key segment)
      (format "download:%s" (md5 (prin1-to-string segment)))))

(defun qq-media-segment-download-path (segment)
  "Return default local download path for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (safe-name (qq-media-segment-default-save-name segment)))
    (expand-file-name
     (format "%s-%s" (substring (md5 key) 0 10) safe-name)
     qq-media-download-directory)))

(defun qq-media-segment-download-state (segment)
  "Return normalized download state plist for SEGMENT."
  (let* ((key (qq-media-segment-download-key segment))
         (entry (copy-tree (or (gethash key qq-media--download-state-table) '())))
         (path (or (plist-get entry :path)
                   (qq-media-segment-download-path segment)))
         (status (plist-get entry :status)))
    (setq entry (plist-put entry :path path))
    (when (and (eq status 'downloaded)
               (not (qq-media-file-present-p path)))
      (setq entry (plist-put entry :status 'not-downloaded))
      (setq entry (plist-put entry :error nil))
      (setq status 'not-downloaded))
    (unless (plist-get entry :status)
      (setq entry (plist-put entry :status (if (qq-media-file-present-p path)
                                               'downloaded
                                             'not-downloaded))))
    (puthash key entry qq-media--download-state-table)
    entry))

(defun qq-media--put-segment-download-state (segment entry)
  "Store download-state ENTRY for SEGMENT and notify UI."
  (let ((key (qq-media-segment-download-key segment)))
    (puthash key entry qq-media--download-state-table)
    (qq-media--note-cache-updated key))
  entry)

(defun qq-media-segment-local-file (segment)
  "Return best local file path for SEGMENT, or nil."
  (let* ((download-state (qq-media-segment-download-state segment))
         (download-path (plist-get download-state :path))
         (cached-resource (qq-media--cached-resource (qq-media--segment-resource-key segment)))
         (cached-file (and cached-resource (alist-get 'file cached-resource))))
    (cond
     ((qq-media-file-present-p download-path) download-path)
     ((qq-media-file-present-p cached-file) cached-file)
     (t nil))))

(defun qq-media--copy-or-download-resource-to (resource target)
  "Copy RESOURCE into TARGET, downloading from URL when needed."
  (let ((file (alist-get 'file resource))
        (url (alist-get 'url resource)))
    (make-directory (or (file-name-directory target) qq-media-download-directory) t)
    (cond
     ((qq-media-file-present-p file)
      (if (and (fboundp 'file-equal-p)
               (file-exists-p target)
               (file-equal-p file target))
          target
        (copy-file file target t)
        target))
     ((qq-media-url-present-p url)
      (url-copy-file url target t)
      target)
     (t
      (error "resource has neither local file nor URL")))))

(defun qq-media-segment-start-download (segment &optional open-after)
  "Download SEGMENT into `qq-media-download-directory'."
  (let* ((entry (qq-media-segment-download-state segment))
         (path (plist-get entry :path))
         (status (plist-get entry :status)))
    (cond
     ((eq status 'downloading)
      (user-error "qq: media download already in progress"))
     ((and (eq status 'downloaded)
           (qq-media-file-present-p path))
      (when open-after
        (find-file path))
      path)
     (t
      (qq-media--put-segment-download-state
       segment
       (plist-put (plist-put (plist-put entry :status 'downloading) :error nil) :path path))
      (message "qq: downloading media -> %s" path)
      (qq-media-resolve-segment-resource
       segment
       (lambda (resource)
         (condition-case err
             (progn
               (qq-media--copy-or-download-resource-to resource path)
               (qq-media--put-segment-download-state
                segment
                (plist-put (plist-put (plist-put entry :status 'downloaded) :error nil) :path path))
               (message "qq: downloaded media -> %s" path)
               (when open-after
                 (find-file path)))
           (error
            (qq-media--put-segment-download-state
             segment
             (plist-put (plist-put (plist-put entry :status 'error)
                                   :error (error-message-string err))
                        :path path))
            (message "qq: media download failed: %s" (error-message-string err)))))
       (lambda (_response reason)
         (qq-media--put-segment-download-state
          segment
          (plist-put (plist-put (plist-put entry :status 'error) :error reason) :path path))
         (message "qq: media download failed: %s" reason)))
      nil))))

(defun qq-media-segment-open-local (segment)
  "Open the best local file available for SEGMENT."
  (if-let* ((file (qq-media-segment-local-file segment)))
      (find-file file)
    (user-error "qq: media segment has no local file yet")))

(defun qq-media-segment-save-as (segment &optional target-path)
  "Save SEGMENT to TARGET-PATH, prompting when nil."
  (interactive)
  (let* ((default-name (qq-media-segment-default-save-name segment))
         (target (or target-path
                     (read-file-name "Save media as: "
                                     nil
                                     default-name
                                     nil
                                     default-name))))
    (condition-case err
        (if-let* ((local-file (qq-media-segment-local-file segment)))
            (progn
              (make-directory (or (file-name-directory target) default-directory) t)
              (copy-file local-file target t)
              (message "qq: saved media -> %s" target))
          (qq-media-resolve-segment-resource
           segment
           (lambda (resource)
             (condition-case copy-err
                 (progn
                   (qq-media--copy-or-download-resource-to resource target)
                   (message "qq: saved media -> %s" target))
               (error
                (message "qq: failed to save media: %s"
                         (error-message-string copy-err)))))
           (lambda (_response reason)
             (message "qq: failed to save media: %s" reason))))
      (error
       (user-error "qq: failed to save media: %s" (error-message-string err))))))

(defun qq-media-segment-play (segment)
  "Play video SEGMENT, preferring local files when available."
  (unless (qq-media-segment-playable-p segment)
    (user-error "qq: segment is not playable"))
  (if-let* ((file (qq-media-segment-local-file segment)))
      (disco-media-play-video-file file)
    (qq-media-resolve-segment-resource
     segment
     (lambda (resource)
       (let ((resolved-file (alist-get 'file resource))
             (url (or (alist-get 'url resource)
                      (qq-media--segment-url segment))))
         (cond
          ((qq-media-file-present-p resolved-file)
           (disco-media-play-video-file resolved-file))
          ((qq-media-url-present-p url)
           (disco-media-play-video-url url))
          (t
           (user-error "qq: video segment has no playable source"))))))))

(defun qq-media-message-primary-segment (message)
  "Return the most useful openable segment from MESSAGE, or nil."
  (let ((segments (alist-get 'segments message))
        found)
    (while (and segments (not found))
      (let ((segment (car segments)))
        (when (qq-media-segment-openable-p segment)
          (setq found segment))
        (setq segments (cdr segments))))
    found))

(defun qq-media-message-has-openable-resource-p (message)
  "Return non-nil when MESSAGE has at least one openable resource segment."
  (not (null (qq-media-message-primary-segment message))))

(defun qq-media-open-message-resource (message)
  "Open the most relevant resource from MESSAGE."
  (if-let* ((segment (qq-media-message-primary-segment message)))
      (qq-media-segment-open segment)
    (user-error "qq: message has no openable media segment")))

(defun qq-media-open-user-avatar (user-id)
  "Open avatar for USER-ID."
  (unless user-id
    (user-error "qq: missing user id for avatar"))
  (qq-media--resolve-resource
   (format "avatar:%s" user-id)
   (lambda (done)
     (qq-api-get-avatar user-id done))
   #'qq-media-open-resource))

(defun qq-media-open-group-avatar (group-id)
  "Open group avatar for GROUP-ID."
  (unless group-id
    (user-error "qq: missing group id for avatar"))
  (qq-media--resolve-resource
   (format "group-avatar:%s" group-id)
   (lambda (done)
     (qq-api-get-group-avatar group-id done))
   #'qq-media-open-resource))

(defun qq-media-open-session-avatar (session)
  "Open session avatar for SESSION."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group (qq-media-open-group-avatar target-id))
      ('dataline (user-error "qq: dataline sessions have no QQ avatar"))
      (_ (qq-media-open-user-avatar target-id)))))

(defun qq-media-open-message-avatar (message)
  "Open sender avatar for MESSAGE."
  (let ((sender-id (or (alist-get 'sender-id message)
                       (alist-get 'user-id message))))
    (unless sender-id
      (user-error "qq: message has no sender id"))
    (when (equal (format "%s" sender-id) "0")
      (user-error "qq: virtual sender has no avatar"))
    (qq-media-open-user-avatar sender-id)))

(defun qq-media-avatar-image (user-id)
  "Return inline avatar image for USER-ID, triggering fetch when needed."
  (qq-media--ensure-resource-image
   (format "avatar:%s" user-id)
   (lambda (done error)
     (qq-api-get-avatar user-id done error))
   qq-media-avatar-image-height))

(defun qq-media-avatar-display-string (user-id)
  "Return inline display string for USER-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-avatar-image user-id)
   "@"))

(defun qq-media-group-avatar-image (group-id)
  "Return inline group avatar image for GROUP-ID, triggering fetch when needed."
  (qq-media--ensure-resource-image
   (format "group-avatar:%s" group-id)
   (lambda (done error)
     (qq-api-get-group-avatar group-id done error))
   qq-media-avatar-image-height))

(defun qq-media-group-avatar-display-string (group-id)
  "Return inline display string for GROUP-ID avatar.

When image data is not ready yet, return a textual fallback."
  (qq-media--image-display-string
   (qq-media-group-avatar-image group-id)
   "#"))

(defun qq-media-session-avatar-display-string (session)
  "Return inline avatar display string for SESSION."
  (let ((target-id (alist-get 'target-id session)))
    (pcase (alist-get 'type session)
      ('group (qq-media-group-avatar-display-string target-id))
      ('dataline "📱")
      (_ (qq-media-avatar-display-string target-id)))))

(defvar qq-media--face-names-table nil
  "Lazy hash table: face id string → QDes name (e.g. \"/斜眼笑\").")

(defun qq-media--face-names-file ()
  "Return resolved path to the face-names JSON, preferring a readable file."
  (let* ((configured qq-media-face-names-file)
         (lib (or (locate-library "qq-media.el")
                  (locate-library "qq-customize.el")))
         (beside (and lib
                      (expand-file-name
                       "qq-face-names.json"
                       (file-name-directory (file-truename lib))))))
    (cond
     ((and (stringp configured) (file-readable-p configured)) configured)
     ((and (stringp beside) (file-readable-p beside)) beside)
     (t configured))))

(defun qq-media--load-face-names-table ()
  "Load face-names JSON into `qq-media--face-names-table'."
  (or qq-media--face-names-table
      (let ((table (make-hash-table :test #'equal))
            (file (qq-media--face-names-file)))
        (when (and (stringp file) (file-readable-p file))
          (condition-case err
              (let* ((json-object-type 'alist)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (json-false nil)
                     (data (json-read-file file)))
                (dolist (pair data)
                  (when (and (consp pair)
                             (stringp (car pair))
                             (stringp (cdr pair)))
                    (puthash (car pair) (cdr pair) table))))
            (error
             (message "qq: failed to load face names from %s: %s"
                      file (error-message-string err)))))
        (setq qq-media--face-names-table table)
        table)))

(defun qq-media-face-name (emoji-id)
  "Return human-readable QQ face name for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (table (qq-media--load-face-names-table)))
    (or (gethash id table)
        ;; Also accept numeric keys that json may have stored differently.
        (and (string-match-p "\\`[0-9]+\\'" id)
             (gethash id table)))))

(defun qq-media--face-id-number (id)
  "Return numeric value of face ID string, or nil."
  (and (stringp id)
       (string-match-p "\\`[0-9]+\\'" id)
       (string-to-number id)))

(defun qq-media-face-completion-candidates ()
  "Return base face candidates sorted by numeric id.

Each candidate looks like \"/斜眼笑  (178)\" so users can search by name
or numeric id.  Use `qq-media-face-id-from-completion'.

Order is by face id (0, 1, 2, …) — the same order as QQ's default
emoji panel.  Pair with `qq-media-face-completion-table' so Vertico
does not re-sort by string length/history."
  (let ((table (qq-media--load-face-names-table))
        (candidates nil))
    (maphash
     (lambda (id name)
       (push (cons id (format "%s  (%s)"
                              (or name (format "[face:%s]" id))
                              id))
             candidates))
     table)
    (mapcar
     #'cdr
     (sort candidates
           (lambda (a b)
             (let ((ida (qq-media--face-id-number (car a)))
                   (idb (qq-media--face-id-number (car b))))
               (cond
                ((and ida idb) (< ida idb))
                (ida t)
                (idb nil)
                (t (string-lessp (car a) (car b))))))))))

(defun qq-media-face-id-from-completion (candidate)
  "Extract face id string from a `qq-media-face-completion-candidates' CANDIDATE."
  (let ((text (and (stringp candidate)
                   (substring-no-properties candidate))))
    (cond
     ((and text
           (string-match "(\\([0-9]+\\))\\'" text))
      (match-string 1 text))
     ((and text
           (string-match-p "\\`[0-9]+\\'" text))
      text)
     (t nil))))

(defun qq-media--face-completion-prefix (id)
  "Return minibuffer prefix string with the face image for ID, or spaces."
  (let* ((file (qq-media--local-base-emoji-file id))
         (image (and file
                     (qq-media--image-from-file
                      file
                      (max 1 qq-media-face-image-height)))))
    (if image
        (concat (propertize " " 'display image) " ")
      "  ")))

(defun qq-media-face-affixation-function (candidates)
  "Affixation function: show local face PNG before each CANDIDATE.

Uses LinuxQQ `default-emojis/<id>.png' only (sync).  Missing files get
a blank spacer so columns stay aligned."
  (mapcar
   (lambda (cand)
     (let ((id (qq-media-face-id-from-completion cand)))
       (list cand
             (if id (qq-media--face-completion-prefix id) "  ")
             "")))
   candidates))

(defun qq-media-face-completion-table ()
  "Completion table for base QQ faces with images and stable id order.

Metadata:
- `display-sort-function'/`cycle-sort-function' = identity (keep id order)
- `affixation-function' = face PNG prefix for Vertico/Icomplete"
  (let ((candidates (qq-media-face-completion-candidates)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata
            (category . qq-face)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . qq-media-face-affixation-function))
        (complete-with-action action candidates string pred)))))


;;; Favorite / custom faces (收藏表情)

(defvar qq-media--custom-faces nil
  "Cached list of favorite custom-face alists from NapCat.")

(defvar qq-media--custom-faces-fetched-at nil
  "Float time when `qq-media--custom-faces' was last fetched.")

(defun qq-media--json-truthy-p (value)
  "Return non-nil when JSON VALUE is a true-ish flag (not :false/:null)."
  (and value
       (not (eq value :false))
       (not (eq value :null))
       (not (equal value 0))
       (not (equal value "false"))
       (not (equal value "0"))))

(defun qq-media--face-alist-p (value)
  "Return non-nil when VALUE looks like one face resource alist."
  (and (consp value)
       (listp value)
       (consp (car value))
       ;; first element is (KEY . VAL), not another nested face list
       (symbolp (car (car value)))))

(defun qq-media--normalize-custom-face-list (data)
  "Normalize DATA from fetch_custom_face_info into a list of face alists.

NapCat returns a JSON array → vector or list of alists.  A single face
alist must not be confused with a list of faces: for a list of faces the
first element is itself an alist; for one face the first element is a
pair `(url . …)'."
  (cond
   ((null data) nil)
   ((vectorp data)
    (qq-media--normalize-custom-face-list (append data nil)))
   ((not (listp data)) nil)
   ;; List of face alists: car is an alist (caar is a pair).
   ((and (consp (car data))
         (qq-media--face-alist-p (car data)))
    data)
   ;; Single face alist: car is (symbol . value).
   ((qq-media--face-alist-p data)
    (list data))
   (t nil)))

(defun qq-media-custom-faces (&optional force)
  "Return cached favorite custom faces, or nil if not loaded.

With FORCE non-nil, ignore the cache (caller should still refresh via
`qq-media-refresh-custom-faces')."
  (unless force
    qq-media--custom-faces))

(defun qq-media-refresh-custom-faces (&optional callback errback count)
  "Fetch favorite custom faces from NapCat and cache them.

CALLBACK is called with the face list on success.

`fetch_custom_face_info' is capped by its `count' argument (see
`qq-media-custom-face-count').  When the response is full — length
equals the requested count — this function retries with a larger
count (doubling, capped by `qq-media-custom-face-count-max') so large
favorites libraries are not silently truncated at 96/page-size."
  (let ((req (max 1 (or count qq-media-custom-face-count))))
    (qq-api-fetch-custom-face-info
     (lambda (data)
       (let* ((faces (qq-media--normalize-custom-face-list data))
              (n (length faces))
              (max-count (max req qq-media-custom-face-count-max)))
         (if (and (>= n req)
                  (< req max-count))
             ;; Likely truncated: ask for more.
             (qq-media-refresh-custom-faces
              callback errback
              (min max-count (max (* req 2) (1+ req))))
           (setq qq-media--custom-faces faces
                 qq-media--custom-faces-fetched-at (float-time))
           (when (and (>= n req) (>= req max-count))
             (message
              "qq: favorite faces may be truncated (%d returned, count max %d)"
              n max-count))
           (when callback
             (funcall callback faces)))))
     errback
     req)))

(defun qq-media-custom-face-id (face)
  "Return a stable string id for favorite FACE alist."
  (or (let ((md5 (alist-get 'md5 face)))
        (and (stringp md5) (not (string-empty-p md5)) md5))
      (let ((res (alist-get 'res_id face)))
        (and (stringp res) (not (string-empty-p res)) res))
      (let ((emo (alist-get 'emo_id face)))
        (and emo (format "emo:%s" emo)))
      (format "fav:%s" (sxhash-equal face))))

(defun qq-media-custom-face-label (face &optional index)
  "Return a human completion label for favorite FACE.

INDEX, when non-nil, is included so completing-read candidates stay unique
even when several favorites share an empty `desc'."
  (let* ((desc (alist-get 'desc face))
         (md5 (alist-get 'md5 face))
         (emo (alist-get 'emo_id face))
         (mark (qq-media--json-truthy-p (alist-get 'is_mark_face face)))
         (short (cond
                 ((and (stringp desc) (not (string-empty-p (string-trim desc))))
                  (string-trim desc))
                 ((and (stringp md5) (>= (length md5) 8))
                  (concat (substring md5 0 8) "…"))
                 ((and emo (not (equal emo 0)) (not (equal emo "0")))
                  (format "#%s" emo))
                 (t "favorite")))
         (kind (if mark "mface" "fav"))
         (base (format "[%s] %s" kind short)))
    (if index
        (format "%s  (%d)" base (1+ index))
      base)))

(defun qq-media-custom-face-file (face)
  "Return best local image path for FACE, or nil."
  (seq-find
   #'qq-media-file-present-p
   (list (alist-get 'file face)
         (alist-get 'original_file face)
         (alist-get 'thumb_file face))))

(defun qq-media-custom-face-thumb (face)
  "Return best local thumb path for FACE, or nil."
  (seq-find
   #'qq-media-file-present-p
   (list (alist-get 'thumb_file face)
         (alist-get 'file face)
         (alist-get 'original_file face))))

(defun qq-media-custom-face-display-string (face)
  "Return composer/timeline display string for favorite FACE."
  (let* ((thumb (qq-media-custom-face-thumb face))
         (image (and thumb
                     (qq-media--image-from-file
                      thumb
                      (max qq-media-face-image-height 32))))
         (fallback (qq-media-custom-face-label face)))
    (qq-media--image-display-string image fallback)))

(defvar qq-media--custom-face-completion-pairs nil
  "Active `(LABEL . FACE)' pairs for the favorite-face completing-read.")

(defun qq-media-custom-face-completion-candidates (&optional faces)
  "Return completion candidates for favorite FACES (default: cache).

Each entry is `(LABEL . FACE)'.  Labels are unique (md5 + index).
Order follows the NapCat favorites list (not re-sorted by string)."
  (let ((faces (or faces qq-media--custom-faces))
        (candidates nil)
        (i 0))
    (dolist (face faces)
      (when (qq-media--face-alist-p face)
        (let* ((label (qq-media-custom-face-label face i))
               ;; Attach FACE so affixation / lookup work even if the
               ;; completion UI strips the pairs list context.
               (labeled (propertize label 'qq-custom-face face)))
          (push (cons labeled face) candidates)
          (setq i (1+ i)))))
    (nreverse candidates)))

(defun qq-media-custom-face-from-completion (candidate &optional pairs)
  "Return face alist matching completion CANDIDATE in PAIRS.

PAIRS defaults to the active completion pairs, then the cache."
  (or (and (stringp candidate)
           (get-text-property 0 'qq-custom-face candidate))
      (let* ((key (and (stringp candidate)
                       (substring-no-properties candidate)))
             (pairs (or pairs
                        qq-media--custom-face-completion-pairs
                        (qq-media-custom-face-completion-candidates))))
        (when key
          (or (cdr (assoc key pairs))
              ;; assoc with text-propertized cars: match by plain string.
              (cdr (seq-find (lambda (p)
                               (equal key (substring-no-properties (car p))))
                             pairs)))))))

(defun qq-media--custom-face-completion-prefix (face)
  "Return minibuffer prefix with thumb image for FACE, or spaces."
  (let* ((thumb (qq-media-custom-face-thumb face))
         (image (and thumb
                     (qq-media--image-from-file
                      thumb
                      (max qq-media-face-image-height 32)))))
    (if image
        (concat (propertize " " 'display image) " ")
      "  ")))

(defun qq-media-custom-face-affixation-function (candidates)
  "Affixation function: show favorite thumb before each CANDIDATE."
  (mapcar
   (lambda (cand)
     (let ((face (qq-media-custom-face-from-completion cand)))
       (list cand
             (if face
                 (qq-media--custom-face-completion-prefix face)
               "  ")
             "")))
   candidates))

(defun qq-media-custom-face-completion-table (&optional faces)
  "Completion table for favorite custom faces with images and stable order.

Keeps NapCat favorites order (identity display/cycle sort) and prefixes
each candidate with its local thumb when available."
  (let* ((pairs (qq-media-custom-face-completion-candidates faces))
         (labels (mapcar #'car pairs)))
    (setq qq-media--custom-face-completion-pairs pairs)
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata
            (category . qq-custom-face)
            (display-sort-function . identity)
            (cycle-sort-function . identity)
            (affixation-function . qq-media-custom-face-affixation-function))
        (complete-with-action action labels string pred)))))

(defun qq-media-custom-face-to-segment (face)
  "Convert favorite FACE alist into an outbound OneBot segment.

Personal favorites (most common) are sent as image with `sub_type' 1
(KCUSTOM sticker).  Market favorites (`is_mark_face') become mface when
e_id is present."
  (let* ((mark (qq-media--json-truthy-p (alist-get 'is_mark_face face)))
         (e-id (alist-get 'e_id face))
         (ep-id (alist-get 'ep_id face))
         (file (qq-media-custom-face-file face))
         (url (alist-get 'url face))
         (desc (alist-get 'desc face))
         (md5 (alist-get 'md5 face))
         (summary (if (and (stringp desc) (not (string-empty-p (string-trim desc))))
                      (string-trim desc)
                    "[收藏表情]")))
    (cond
     ((and mark
           (stringp e-id)
           (not (string-empty-p e-id)))
      `((type . "mface")
        (data . ((emoji_id . ,e-id)
                 (emoji_package_id . ,(condition-case _
                                          (string-to-number (format "%s" (or ep-id 0)))
                                        (error 0)))
                 (key . ,(or (alist-get 'key face) ""))
                 (summary . ,summary)))))
     ((or file (qq-media-url-present-p url))
      `((type . "image")
        (data . (,@(when file `((file . ,file)))
                 ,@(when (qq-media-url-present-p url) `((url . ,url)))
                 (name . ,(or md5 "custom-face"))
                 (summary . ,summary)
                 ;; PicSubType.KCUSTOM — QQ treats this as a sticker/emoji image.
                 (sub_type . 1)))))
     (t
      (user-error "qq: favorite face has neither local file nor URL")))))

(defun qq-media-face-text-fallback (emoji-id)
  "Return plain-text fallback for face EMOJI-ID (never a CQ blob)."
  (or (qq-media-face-name emoji-id)
      (format "[face:%s]" emoji-id)))

(defun qq-media--local-base-emoji-file (emoji-id)
  "Return path to LinuxQQ default face image for EMOJI-ID, or nil."
  (let* ((id (format "%s" emoji-id))
         (dir qq-media-default-emoji-directory))
    (when (and (stringp dir)
               (not (string-empty-p id))
               (file-directory-p dir))
      (seq-find
       #'file-exists-p
       (mapcar (lambda (ext)
                 (expand-file-name (concat id "." ext) dir))
               '("png" "gif" "webp" "jpg" "jpeg"))))))

(defun qq-media--face-resource-from-local (emoji-id)
  "Return resource alist for local face EMOJI-ID, or nil."
  (when-let* ((file (qq-media--local-base-emoji-file emoji-id)))
    `((file . ,file)
      (emoji_id . ,(format "%s" emoji-id))
      (description . ,(qq-media-face-name emoji-id)))))

(defun qq-media-face-image (emoji-id)
  "Return inline QQ base face image for EMOJI-ID.

Resolution order:
1. LinuxQQ `default-emojis/<id>.png' (sync, offline)
2. NapCat `get_base_emoji' (download/path lookup)"
  (let* ((id (format "%s" emoji-id))
         (key (format "face:%s" id))
         (local (qq-media--face-resource-from-local id)))
    ;; Seed resource cache so ensure-resource-image can build the image
    ;; synchronously without waiting on NapCat.
    (when local
      (qq-media--cache-resource key local))
    (qq-media--ensure-resource-image
     key
     (lambda (done error)
       (if-let* ((resource (qq-media--face-resource-from-local id)))
           (funcall done resource)
         (qq-api-get-base-emoji
          id
          (lambda (resource)
            ;; Merge description from local name table when API omits it.
            (let* ((resource (copy-tree (or resource '())))
                   (desc (or (alist-get 'description resource)
                             (qq-media-face-name id))))
              (when desc
                (setf (alist-get 'description resource) desc))
              (funcall done resource)))
          error)))
     qq-media-face-image-height)))

(defun qq-media-face-display-string (emoji-id)
  "Return inline display string for QQ face EMOJI-ID.

Prefer the face image (local default-emojis first).  When the image is
not ready yet, show the human face name (`/斜眼笑') rather than CQ."
  (qq-media--image-display-string
   (qq-media-face-image emoji-id)
   (qq-media-face-text-fallback emoji-id)))

(defun qq-media-segment-preview-key (segment)
  "Return preview cache key for SEGMENT, or nil when unsupported."
  (when (qq-media-segment-preview-capable-p segment)
    (let* ((type (alist-get 'type segment))
           (preview-type (if (qq-media-imageish-file-segment-p segment)
                             "file-image"
                           type))
           (file-key (qq-media--segment-file-key segment))
           (url (qq-media--segment-url segment)))
      (cond
       (file-key
        (format "preview:%s:%s" preview-type file-key))
       ((qq-media-url-present-p url)
        (format "preview:%s-url:%s" preview-type url))
       (t nil)))))

(defun qq-media-segment-cache-keys (segment)
  "Return logical media cache keys that can affect SEGMENT rendering."
  (let* ((type (alist-get 'type segment))
         (data (alist-get 'data segment))
         (face-id (and (equal type "face") (alist-get 'id data)))
         keys)
    (when face-id
      (push (format "face:%s" face-id) keys))
    (when-let* ((resource-key (qq-media--segment-resource-key segment)))
      (push resource-key keys))
    (when-let* ((download-key (qq-media-segment-download-key segment)))
      (push download-key keys))
    (when-let* ((preview-key (qq-media-segment-preview-key segment)))
      (push preview-key keys))
    (delete-dups (delq nil keys))))

(defun qq-media-segment-preview-image (segment)
  "Return inline preview image for SEGMENT, triggering fetch when needed.

Resolution order matches `qq-media--resolve-fileish-segment': local path,
then NapCat get_image for remote keys, then segment URL.  Preview failures
are soft (no NapCat error spam).

When SEGMENT already has an existing local path (outbound attach), seed the
resource cache so the image can be built synchronously without touching NapCat."
  (let ((key (qq-media-segment-preview-key segment))
        (local (qq-media--segment-existing-path segment)))
    (when key
      (when local
        (qq-media--cache-resource
         key
         (qq-media--resource-from-local+url
          local (qq-media--segment-url segment))))
      (qq-media--ensure-resource-image
       key
       (lambda (done error)
         (if (qq-media-segment-preview-capable-p segment)
             (qq-media--resolve-fileish-segment
              segment
              "get_image"
              done
              ;; Soft-fail: clear fetching without user-error / NapCat spam.
              (lambda (_response _reason)
                (funcall error nil "preview image not found"))
              "preview image not found")
           (funcall done nil)))
       nil
       #'qq-media--preview-image-from-file))))

(defun qq-media-segment-preview-fetching-p (segment)
  "Return non-nil when preview fetch for SEGMENT is currently active."
  (when-let* ((key (qq-media-segment-preview-key segment)))
    (qq-media--resource-fetching-p key)))

(provide 'qq-media)

;;; qq-media.el ends here
