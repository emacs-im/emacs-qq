;;; qq-guild-channel.el --- Non-timeline QQ Guild channel views -*- lexical-binding: t; -*-

;;; Commentary:

;; Exact, read-only presentation for native QQ Guild channel kinds whose
;; interaction protocol has not yet been implemented.  They must never be
;; presented as ordinary message timelines.

;;; Code:

(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-transaction)
(require 'appkit-view)
(require 'qq-guild-channel-type)
(require 'qq-runtime)
(require 'qq-state)

(defvar-local qq-guild-channel--guild-id nil)
(defvar-local qq-guild-channel--channel-id nil)
(defvar-local qq-guild-channel--channel nil)

(defun qq-guild-channel--view-id (guild-id channel-id)
  "Return Appkit view identity for GUILD-ID and CHANNEL-ID."
  (list 'guild-channel-inspect guild-id channel-id))

(defun qq-guild-channel--buffer-name (guild-id channel-id)
  "Return inspect buffer name for GUILD-ID and CHANNEL-ID."
  (let ((channel (qq-state-guild-channel guild-id channel-id)))
    (format "*qq-channel:%s*" (or (alist-get 'name channel) channel-id))))

(defun qq-guild-channel--refresh-model ()
  "Refresh current inspect model from authoritative directory state."
  (setq qq-guild-channel--channel
        (or (qq-state-guild-channel
             qq-guild-channel--guild-id qq-guild-channel--channel-id)
            (user-error "qq: channel is no longer in the directory"))))

(defun qq-guild-channel--insert-field (label value)
  "Insert one read-only metadata row with LABEL and VALUE."
  (appkit-view-insert-label-line
   (concat (propertize (format "%-10s" label) 'face 'shadow)
           (format "%s" value))))

(defun qq-guild-channel--render ()
  "Render the current exact non-timeline channel model."
  (let* ((channel qq-guild-channel--channel)
         (kind (alist-get 'kind channel))
         (label (qq-guild-channel-type-label kind)))
    (appkit-view-insert-heading-line
     (format "%s  %s" (qq-guild-channel-type-icon kind)
             (alist-get 'name channel))
     :face 'font-lock-function-name-face)
    (insert "\n")
    (qq-guild-channel--insert-field "QQ频道" (alist-get 'guild_name channel))
    (qq-guild-channel--insert-field "类型" label)
    (qq-guild-channel--insert-field "频道 ID" (alist-get 'channel_id channel))
    (insert "\n")
    (appkit-view-insert-note-line
     (pcase kind
       ("live" "直播频道需要独立的观看状态与直播交互协议。")
       ("application" "应用频道需要读取原生频道应用入口。")
       ("schedule" "日程频道需要读取原生日程列表与参与状态。")
       (_ (error "qq: channel kind %S does not belong in inspect mode" kind)))
     :face 'shadow)))

(defun qq-guild-channel--sync-invalidations (view invalidations)
  "Synchronize inspect VIEW from Appkit INVALIDATIONS."
  (when (and (appkit-view-live-p view)
             (or (appkit-invalidations-structure-p invalidations)
                 (memq 'channel (appkit-invalidations-parts invalidations))))
    (qq-guild-channel--refresh-model)
    (appkit-with-content-update view
      (let ((inhibit-read-only t))
        (erase-buffer)
        (qq-guild-channel--render)))))

(defun qq-guild-channel-refresh ()
  "Refresh the current inspect view from authoritative directory state."
  (interactive)
  (let ((view (or (appkit-current-view)
                  (user-error "qq: this channel view is detached"))))
    (appkit-request-sync view :structure t :part 'channel)
    (appkit-sync-invalidations view)))

(defun qq-guild-channel--setup-view (view)
  "Register exact directory-state ownership for inspect VIEW."
  (let ((handler
         (lambda (event)
           (when (and (appkit-view-live-p view)
                      (memq (plist-get event :type)
                            '(reset guild-directory-refreshed)))
             (appkit-with-live-view view
               (appkit-request-sync view :structure t :part 'channel))))))
    (add-hook 'qq-state-change-hook handler)
    (appkit-register-handle
     view 'hook (list 'qq-state-change-hook handler nil (current-buffer)))))

(defvar qq-guild-channel-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'qq-guild-channel-refresh)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode qq-guild-channel-mode special-mode "QQ-Channel"
  "Major mode for a non-timeline QQ Guild channel."
  (setq buffer-read-only t
        truncate-lines nil)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t))

(defun qq-guild-channel-open (guild-id channel-id)
  "Open non-timeline GUILD-ID and CHANNEL-ID in an exact inspect view."
  (let* ((channel (or (qq-state-guild-channel guild-id channel-id)
                      (user-error "qq: channel is no longer in the directory")))
         (kind (alist-get 'kind channel)))
    (unless (eq (qq-guild-channel-open-mode kind) 'inspect)
      (user-error "qq: %s channel does not use inspect mode" kind))
    (let* ((app (qq-runtime-app))
           (view
            (appkit-open-view
             :app app
             :id (qq-guild-channel--view-id guild-id channel-id)
             :mode 'qq-guild-channel-mode
             :buffer-name (qq-guild-channel--buffer-name guild-id channel-id)
             :state (cons guild-id channel-id)
             :sync-function #'qq-guild-channel--sync-invalidations
             :parts '(channel)
             :setup #'qq-guild-channel--setup-view
             :select t))
           (buffer (appkit-view-buffer view)))
      (with-current-buffer buffer
        (setq qq-guild-channel--guild-id guild-id
              qq-guild-channel--channel-id channel-id
              qq-guild-channel--channel (copy-tree channel))
        (appkit-request-sync view :structure t :part 'channel)
        (appkit-sync-invalidations view))
      buffer)))

(provide 'qq-guild-channel)

;;; qq-guild-channel.el ends here
