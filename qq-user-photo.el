;;; qq-user-photo.el --- Native QQ photo-wall buffers -*- lexical-binding: t; -*-

;; Author: emacs-qq contributors

;;; Commentary:

;; Dedicated photo-wall view backed only by Linux QQ native profile data.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)
(require 'qq-api)
(require 'qq-media)
(require 'qq-view)

(declare-function qq-api-cancel-request "qq-api" (request-token))

(defvar-local qq-user-photo--user-id nil
  "QQ number whose photo wall is displayed.")

(defvar-local qq-user-photo--display-name nil
  "Display name used by the current photo-wall buffer.")

(defvar-local qq-user-photo--photos nil
  "Native photo-wall entries displayed by the current buffer.")

(defvar-local qq-user-photo--request nil
  "Active photo-wall request token.")

(defvar-local qq-user-photo--request-owner nil
  "Owner object for the active photo-wall request.")

(defvar-local qq-user-photo--loading nil
  "Non-nil while native photo-wall data is loading.")

(defvar-local qq-user-photo--error nil
  "Last photo-wall loading error, or nil.")

(defun qq-user-photo--buffer-name (user-id)
  "Return photo-wall buffer name for USER-ID."
  (format "*qq-photos:%s*" user-id))

(defun qq-user-photo--header-line ()
  "Return dynamic header line for a photo-wall buffer."
  (format " QQ Photos · %s (%s)%s"
          (or qq-user-photo--display-name qq-user-photo--user-id "unknown")
          (or qq-user-photo--user-id "unknown")
          (if qq-user-photo--loading " · loading" "")))

(defun qq-user-photo-url (photo)
  "Return best exact native URL from PHOTO."
  (or (alist-get 'original_url photo)
      (alist-get 'thumbnail_url photo)))

(defun qq-user-photo-preview-url (photo)
  "Return best preview URL from PHOTO."
  (or (alist-get 'thumbnail_url photo)
      (alist-get 'original_url photo)))

(defun qq-user-photo-cache-key (user-id photo)
  "Return stable media cache key for USER-ID and PHOTO."
  (format "photo-wall:%s:%s:%s"
          user-id
          (alist-get 'id photo)
          (md5 (or (qq-user-photo-preview-url photo) ""))))

(defun qq-user-photo-preview-display-string (user-id photo fallback)
  "Return inline preview for USER-ID PHOTO, or FALLBACK while loading."
  (qq-media-url-preview-display-string
   (qq-user-photo-cache-key user-id photo)
   (qq-user-photo-preview-url photo)
   fallback))

(defun qq-user-photo-open-entry (user-id photo)
  "Open USER-ID's native PHOTO entry."
  (qq-media-open-image-url
   (qq-user-photo-cache-key user-id photo)
   (qq-user-photo-url photo)))

(defun qq-user-photo--button-action (button)
  "Open native photo represented by BUTTON."
  (qq-user-photo-open-entry
   (button-get button 'qq-user-id)
   (button-get button 'qq-user-photo)))

(defun qq-user-photo-make-button (start end user-id photo)
  "Turn START to END into a photo button for USER-ID and PHOTO."
  (make-text-button
   start end
   'follow-link t
   'action #'qq-user-photo--button-action
   'help-echo "查看照片"
   'face 'default
   'mouse-face 'highlight
   'qq-user-id user-id
   'qq-user-photo photo
   'rear-nonsticky '(qq-user-id qq-user-photo mouse-face)))

(defun qq-user-photo--photo-at-point ()
  "Return native photo entry at point, or nil."
  (get-text-property (point) 'qq-user-photo))

(defun qq-user-photo-render ()
  "Render the current native photo-wall buffer."
  (interactive)
  (qq-view-render-preserving-position
   (lambda ()
     (let ((inhibit-read-only t))
       (erase-buffer)
       (setq-local header-line-format '(:eval (qq-user-photo--header-line)))
       (qq-view-insert-note-line "g refresh  RET view  q quit")
       (insert "\n")
       (cond
        (qq-user-photo--loading
         (qq-view-insert-note-line "Loading photo wall…"))
        (qq-user-photo--error
         (qq-view-insert-note-line qq-user-photo--error :face 'error))
        ((null qq-user-photo--photos)
         (qq-view-insert-note-line "No public photos."))
        (t
         (cl-loop for photo in qq-user-photo--photos
                  for index from 1
                  do
                  (let* ((start (point))
                         (preview-url (qq-user-photo-preview-url photo))
                         (key (qq-user-photo-cache-key
                               qq-user-photo--user-id photo)))
                    (insert
                     (qq-media-url-preview-display-string
                      key preview-url (format "Photo %d" index))
                     "\n\n")
                    (qq-user-photo-make-button
                     start (point) qq-user-photo--user-id photo)))))
       (goto-char (point-min))))
   :preserve-window-start t))

(defun qq-user-photo-open-at-point ()
  "Open the native photo-wall image at point."
  (interactive)
  (let ((photo (qq-user-photo--photo-at-point)))
    (unless photo
      (user-error "qq: no photo at point"))
    (qq-user-photo-open-entry qq-user-photo--user-id photo)))

(defun qq-user-photo--request-current-p (buffer user-id owner)
  "Return non-nil when OWNER still loads USER-ID in BUFFER."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-user-photo-mode)
              (equal qq-user-photo--user-id user-id)
              (eq qq-user-photo--request-owner owner)))))

(defun qq-user-photo-refresh ()
  "Refresh the current native photo wall."
  (interactive)
  (unless qq-user-photo--user-id
    (user-error "qq: this photo buffer has no user identity"))
  (when qq-user-photo--request
    (qq-api-cancel-request qq-user-photo--request))
  (let ((buffer (current-buffer))
        (user-id qq-user-photo--user-id)
        (owner (list 'photo-wall qq-user-photo--user-id)))
    (setq qq-user-photo--photos nil
          qq-user-photo--loading t
          qq-user-photo--error nil
          qq-user-photo--request nil
          qq-user-photo--request-owner owner)
    (qq-user-photo-render)
    (let ((request
           (qq-api-get-user-photo-wall
            user-id
            (lambda (photos)
              (when (qq-user-photo--request-current-p buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user-photo--photos photos
                        qq-user-photo--loading nil
                        qq-user-photo--request nil
                        qq-user-photo--request-owner nil)
                  (qq-user-photo-render))))
            (lambda (_response reason)
              (when (qq-user-photo--request-current-p buffer user-id owner)
                (with-current-buffer buffer
                  (setq qq-user-photo--loading nil
                        qq-user-photo--error
                        (format "Unable to load photo wall: %s"
                                (or reason "unknown error"))
                        qq-user-photo--request nil
                        qq-user-photo--request-owner nil)
                  (qq-user-photo-render)))))))
      (when (eq qq-user-photo--request-owner owner)
        (setq qq-user-photo--request request)))))

(defun qq-user-photo--cancel-request ()
  "Cancel the active photo-wall request."
  (when qq-user-photo--request
    (qq-api-cancel-request qq-user-photo--request))
  (setq qq-user-photo--request nil
        qq-user-photo--request-owner nil))

(defun qq-user-photo-button-backward ()
  "Move point to the previous photo button."
  (interactive)
  (forward-button -1))

(defvar qq-user-photo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'qq-user-photo-refresh)
    (define-key map (kbd "RET") #'qq-user-photo-open-at-point)
    (define-key map (kbd "TAB") #'forward-button)
    (define-key map (kbd "<backtab>") #'qq-user-photo-button-backward)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-user-photo-mode'.")

(define-derived-mode qq-user-photo-mode special-mode "QQ-Photos"
  "Major mode for one native QQ photo wall."
  (setq-local truncate-lines nil)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (add-hook 'kill-buffer-hook #'qq-user-photo--cancel-request nil t))

(defun qq-user-photo-open (user-id display-name)
  "Open native photo wall for USER-ID titled with DISPLAY-NAME."
  (unless (qq-api-user-id-p user-id)
    (user-error "qq: photo wall requires a decimal string user id"))
  (let ((buffer (get-buffer-create (qq-user-photo--buffer-name user-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-user-photo-mode)
        (qq-user-photo-mode))
      (setq qq-user-photo--user-id user-id
            qq-user-photo--display-name display-name)
      (qq-user-photo-render)
      (unless qq-user-photo--loading
        (qq-user-photo-refresh)))
    (pop-to-buffer buffer)
    buffer))

(defun qq-user-photo--handle-media-cache-update (media-key)
  "Rerender photo buffers affected by MEDIA-KEY."
  (when (and (stringp media-key)
             (string-prefix-p "photo-wall:" media-key))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (derived-mode-p 'qq-user-photo-mode)
                   (string-prefix-p
                    (format "photo-wall:%s:" qq-user-photo--user-id)
                    media-key))
          (qq-user-photo-render))))))

(add-hook 'qq-media-cache-update-hook
          #'qq-user-photo--handle-media-cache-update)

(provide 'qq-user-photo)

;;; qq-user-photo.el ends here
