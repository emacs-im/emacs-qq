;;; qq-search.el --- Authoritative QQ message search results -*- lexical-binding: t; -*-

;; Author: 0WD0 <wd.1105848296@gmail.com>

;;; Commentary:

;; A session-scoped, server-backed message search buffer.  Search cursors are
;; opaque NapCat capabilities: this module stores and returns them unchanged.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'qq-api)
(require 'qq-runtime)
(require 'qq-state)

(declare-function qq-chat-open-message
                  "qq-chat" (session-key message-id &optional query))

(defgroup qq-search nil
  "Message search for emacs-qq."
  :group 'qq)

(defcustom qq-search-page-size 50
  "Number of results requested for the first search page."
  :type 'integer
  :group 'qq-search)

(defvar qq-search-history nil
  "Minibuffer history for QQ message search queries.")

(defconst qq-search-buffer-name "*qq-search*"
  "Name of the session message-search buffer.")

(cl-defstruct (qq-search--entry
               (:constructor qq-search--entry-create))
  key
  type
  object)

(defvar-local qq-search--ewoc nil
  "Persistent keyed EWOC used by the current search view.")

(defvar-local qq-search--node-table nil
  "Stable search entry key to EWOC node table.")

(defvar-local qq-search--session-key nil)
(defvar-local qq-search--query nil)
(defvar-local qq-search--results nil)
(defvar-local qq-search--results-tail nil)
(defvar-local qq-search--seen nil)
(defvar-local qq-search--consumed-cursors nil)
(defvar-local qq-search--next-cursor nil)
(defvar-local qq-search--status 'eof)
(defvar-local qq-search--error nil)
(defvar-local qq-search--request nil)
(defvar-local qq-search--request-owner nil)
(defvar-local qq-search--pending-next-key nil
  "Result key after which navigation should resume once a page arrives.")
(defvar-local qq-search--focus-first-result-p nil
  "Non-nil when the next sync should select the first result.")

(defun qq-search--view-id (session-key)
  "Return Appkit view identity for SESSION-KEY search results."
  (list 'search session-key))

(defun qq-search--cancel-request ()
  "Cancel the current buffer's owned transport request, if any."
  (when qq-search--request
    (qq-api-cancel-request qq-search--request))
  (setq qq-search--request nil
        qq-search--request-owner nil))

(defun qq-search--request-current-p (view buffer session-key owner)
  "Return non-nil when VIEW and OWNER still own BUFFER and SESSION-KEY."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'qq-search-mode)
              (eq view (appkit-current-view))
              (equal qq-search--session-key session-key)
              (eq qq-search--request-owner owner)))))

(defun qq-search--result-key (result)
  "Return stable deduplication key for search RESULT."
  (cons (qq-api-session-key-from-locator (alist-get 'chat result))
        (alist-get 'message_id result)))

(defun qq-search--append-results (results)
  "Append unseen RESULTS for the current session and return their count."
  (let ((added 0))
    (dolist (result results)
      (let* ((key (qq-search--result-key result))
             (result-session (car key)))
        (unless (equal result-session qq-search--session-key)
          (error "qq: message search returned result for %s while searching %s"
                 result-session qq-search--session-key))
        (unless (gethash key qq-search--seen)
          (puthash key t qq-search--seen)
          (let ((cell (list result)))
            (if qq-search--results-tail
                (setcdr qq-search--results-tail cell)
              (setq qq-search--results cell))
            (setq qq-search--results-tail cell))
          (cl-incf added))))
    added))

(defun qq-search--highlight-preview (preview query)
  "Return PREVIEW with literal QUERY tokens highlighted.

Only the preview copy is scanned; dates, sender names, headings, and status
text in the results buffer can never receive search highlighting."
  (let* ((copy (copy-sequence preview))
         (tokens (split-string query "[[:space:]]+" t))
         (regexp (and tokens (regexp-opt tokens)))
         (case-fold-search t)
         (start 0))
    (when regexp
      (while (string-match regexp copy start)
        (add-face-text-property (match-beginning 0) (match-end 0)
                                'isearch t copy)
        (setq start (max (match-end 0) (1+ (match-beginning 0))))))
    copy))

(defun qq-search--session-title ()
  "Return display title for the current search session."
  (let ((session (qq-state-session qq-search--session-key)))
    (or (alist-get 'title session)
        (alist-get 'name session)
        qq-search--session-key)))

(defun qq-search--insert-result (result)
  "Insert one whole-line actionable RESULT."
  (let* ((sender (alist-get 'sender result))
         (name (or (alist-get 'name sender)
                   (alist-get 'user_id sender)
                   "QQ"))
         (sent-at (alist-get 'sent_at result))
         (preview (replace-regexp-in-string
                   "[\n\r]+" " " (alist-get 'preview result)))
         (line (concat
                (propertize
                 (format-time-string "%m-%d %H:%M"
                                     (seconds-to-time sent-at))
                 'face 'shadow)
                "  "
                (propertize name 'face 'bold)
                "  "
                (qq-search--highlight-preview preview qq-search--query)))
         (start (point)))
    (insert line)
    (add-text-properties
     start (point)
     (list 'qq-search-result result
           'qq-search-session-key qq-search--session-key
           'qq-search-result-key (qq-search--result-key result)))
    (appkit-ui-make-action-row
     start (point) result #'qq-search--open-result
     :help-echo "mouse-1 or RET: open this message")
    (insert "\n")))

(defun qq-search--insert-status ()
  "Insert the explicit current paging status."
  (pcase qq-search--status
    ('loading (insert (propertize "\nLoading…\n" 'face 'shadow)))
    ('error
     (insert (propertize
              (format "\nError: %s\n" (or qq-search--error "unknown error"))
              'face 'error)))
    ('eof
     (insert (propertize
              (if qq-search--results
                  "\nEnd of results.\n"
                "\nNo results.\n")
              'face 'shadow)))
    (_
     (insert (propertize "\nm: load more\n" 'face 'shadow)))))

(defun qq-search--project-entries ()
  "Project current search state into stable keyed presentation entries."
  (append
   (list
    (qq-search--entry-create
     :key 'header
     :type 'header
     :object (list (qq-search--session-title)
                   qq-search--query
                   (length qq-search--results))))
   (mapcar
    (lambda (result)
      (qq-search--entry-create
       :key (cons 'result (qq-search--result-key result))
       :type 'result
       :object result))
    qq-search--results)
   (list
    (qq-search--entry-create
     :key 'status
     :type 'status
     :object (list qq-search--status qq-search--error
                   (and qq-search--results t)
                   qq-search--next-cursor)))))

(defun qq-search--ewoc-printer (entry)
  "Insert one projected search ENTRY."
  (pcase (qq-search--entry-type entry)
    ('header
     (insert (propertize
              (format "Search in %s\n" (qq-search--session-title))
              'face 'bold))
     (insert (format "Query: %s\nLoaded: %d\n\n"
                     qq-search--query (length qq-search--results))))
    ('result (qq-search--insert-result (qq-search--entry-object entry)))
    ('status (qq-search--insert-status))
    (type (error "qq: unknown search entry type %S" type))))

(defun qq-search--live-current-view ()
  "Return this buffer's live session-search view, or nil."
  (let ((view (appkit-current-view)))
    (and (derived-mode-p 'qq-search-mode)
         (appkit-view-live-p view)
         (equal (appkit-view-id view)
                (qq-search--view-id qq-search--session-key))
         view)))

(defun qq-search--cancel-buffer-work (buffer)
  "Cancel search work still owned by BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'qq-search-mode)
        (qq-search--cancel-request)))))

(defun qq-search--setup-view (view)
  "Register lifecycle cleanup for newly attached search VIEW."
  (appkit-register-handle
   view 'function
   (apply-partially #'qq-search--cancel-buffer-work
                    (appkit-view-buffer view))))

(defun qq-search--ensure-view ()
  "Return the live Appkit view owning the current search buffer."
  (unless qq-search--session-key
    (error "QQ: cannot attach a search view without a session identity"))
  (let* ((app (qq-runtime-app))
         (view-id (qq-search--view-id qq-search--session-key))
         (current (appkit-current-view)))
    (cond
     ((and (appkit-view-live-p current)
           (eq app (appkit-view-app current))
           (equal view-id (appkit-view-id current)))
      (setf (appkit-view-sync-function current)
            #'qq-search--sync-invalidations
            (appkit-view-parts current) '(results))
      current)
     ((appkit-view-live-p current)
      (error "QQ: search buffer belongs to another Appkit view"))
     (t
      (let ((view
             (appkit-attach-view
              :app app
              :id view-id
              :mode 'qq-search-mode
              :sync-function #'qq-search--sync-invalidations
              :parts '(results))))
        (qq-search--setup-view view)
        view)))))

(defun qq-search--request-sync (&optional view)
  "Request a coalesced results sync for live VIEW."
  (when-let* ((view (or view (qq-search--live-current-view))))
    (appkit-request-sync view :structure t :part 'results)))

(defun qq-search--pending-invalidations-p (view)
  "Return non-nil when live VIEW has pending invalidations."
  (and (appkit-view-live-p view)
       (appkit-invalidations-any-p (appkit-view-invalidations view))))

(defun qq-search--sync-now (view)
  "Consume immediately available invalidations for VIEW.

The bounded loop also handles synchronous transports that return several
empty continuation pages while next-result navigation is pending."
  (let ((remaining 64))
    (while (and (> remaining 0)
                (qq-search--pending-invalidations-p view))
      (setq remaining (1- remaining))
      (appkit-sync-invalidations view))
    (when (and (= remaining 0)
               (qq-search--pending-invalidations-p view))
      (error "qq: search synchronization did not quiesce"))))

(defun qq-search--sync-invalidations (view invalidations)
  "Consume coalesced search INVALIDATIONS for VIEW."
  (when (and (appkit-view-live-p view)
             (or (appkit-invalidations-structure-p invalidations)
                 (appkit-invalidations-parts invalidations)
                 (appkit-invalidations-entry-keys invalidations)
                 (appkit-invalidations-position-p invalidations)))
    (let ((snapshot
           (with-current-buffer (appkit-view-buffer view)
             (appkit-position-capture
              :anchor-property 'qq-search-result-key
              :preserve-window-start t))))
      (appkit-with-content-update view
				  (unless qq-search--ewoc
				    (erase-buffer)
				    (setq qq-search--ewoc
					  (ewoc-create #'qq-search--ewoc-printer nil nil t)))
				  (setq qq-search--node-table
					(appkit-ewoc-reconcile
					 qq-search--ewoc
					 (qq-search--project-entries)
					 #'qq-search--entry-key
					 :force-keys (appkit-invalidations-entry-keys invalidations)))
				  (when snapshot
				    (appkit-position-restore snapshot))
				  (when (and qq-search--focus-first-result-p qq-search--results)
				    (setq qq-search--focus-first-result-p nil)
				    (goto-char (point-min))
				    (qq-search--next-local-result))
				  (qq-search--resume-pending-next)))))

(defun qq-search--goto-result-key (key)
  "Move point to the result identified by KEY and return non-nil."
  (let ((found nil))
    (goto-char (point-min))
    (while (and (not found) (< (point) (point-max)))
      (when-let* ((result (get-text-property (point) 'qq-search-result)))
        (when (equal key (qq-search--result-key result))
          (setq found t)))
      (unless found (forward-line 1)))
    found))

(defun qq-search--next-local-result ()
  "Move point to the next already loaded result and return non-nil."
  (let ((origin (point)) found)
    (when (qq-search--result-at-point) (forward-line 1))
    (while (and (< (point) (point-max)) (not found))
      (if (qq-search--result-at-point)
          (setq found t)
        (forward-line 1)))
    (unless found (goto-char origin))
    found))

(defun qq-search--resume-pending-next ()
  "Resume a pending next-result move, consuming empty pages if necessary."
  (when qq-search--pending-next-key
    (let ((anchor qq-search--pending-next-key))
      (unless (qq-search--goto-result-key anchor)
        (error "qq: search navigation lost its result anchor"))
      (cond
       ((qq-search--next-local-result)
        (setq qq-search--pending-next-key nil))
       ((or qq-search--request qq-search--request-owner
            (eq qq-search--status 'loading))
        nil)
       (qq-search--next-cursor
        (qq-search--start-request t))
       (t
        (setq qq-search--pending-next-key nil)
        (message "qq: reached end of search results"))))))

(defun qq-search--page-succeeded (view buffer session-key owner page)
  "Apply PAGE when VIEW and OWNER still own BUFFER and SESSION-KEY."
  (when (qq-search--request-current-p view buffer session-key owner)
    (with-current-buffer buffer
      (setf (plist-get owner :pending) nil)
      (setq qq-search--request nil
            qq-search--request-owner nil)
      (condition-case error-data
          (progn
            (qq-search--append-results (alist-get 'results page))
            (setq qq-search--next-cursor (alist-get 'next_cursor page)
                  qq-search--status (if qq-search--next-cursor 'ready 'eof)
                  qq-search--error nil)
            (qq-search--request-sync view))
        (error
         (setq qq-search--status 'error
               qq-search--error (error-message-string error-data)
               qq-search--pending-next-key nil)
         (qq-search--request-sync view))))))

(defun qq-search--page-failed (view buffer session-key owner _response reason)
  "Record search failure REASON when VIEW and OWNER still own BUFFER."
  (when (qq-search--request-current-p view buffer session-key owner)
    (with-current-buffer buffer
      (setf (plist-get owner :pending) nil)
      (setq qq-search--request nil
            qq-search--request-owner nil
            qq-search--status 'error
            qq-search--error reason
            qq-search--pending-next-key nil)
      (qq-search--request-sync view))))

(defun qq-search--start-request (next-p)
  "Start an owned search request; continue a cursor when NEXT-P."
  (let ((view (qq-search--ensure-view)))
    (when qq-search--request
      (qq-api-cancel-request qq-search--request))
    (let* ((buffer (current-buffer))
           (session-key qq-search--session-key)
           (owner (list :session-key session-key :pending t))
           (cursor qq-search--next-cursor)
           request)
      (setq qq-search--request-owner owner
            qq-search--request nil
            qq-search--status 'loading
            qq-search--error nil)
      (qq-search--request-sync view)
      (condition-case error-data
          (progn
            (when next-p
              (unless cursor
		(error "qq: message search has no continuation cursor"))
              (when (gethash cursor qq-search--consumed-cursors)
		(error "qq: message search repeated an already consumed cursor"))
              (puthash cursor t qq-search--consumed-cursors)
              ;; NapCat cursors own one native searchMore operation.  Consume
              ;; before dispatch and never restore after failure or signal.
              (setq qq-search--next-cursor nil))
            (setq request
                  (if next-p
                      (qq-api-search-messages-next
                       session-key cursor 'summary
                       (lambda (page)
			 (qq-search--page-succeeded
                          view buffer session-key owner page))
                       (lambda (response reason)
			 (qq-search--page-failed
                          view buffer session-key owner response reason)))
                    (qq-api-search-messages-start
                     session-key qq-search--query
                     (lambda (page)
                       (qq-search--page-succeeded
			view buffer session-key owner page))
                     (lambda (response reason)
                       (qq-search--page-failed
			view buffer session-key owner response reason))
                     qq-search-page-size)))
            ;; A mocked or local transport may complete synchronously.  Store
            ;; the token only while this exact owner remains pending.
            (when (and (qq-search--request-current-p
			view buffer session-key owner)
                       (plist-get owner :pending))
              (setq qq-search--request request))
            request)
	(error
	 (when (qq-search--request-current-p view buffer session-key owner)
           (setf (plist-get owner :pending) nil)
           (setq qq-search--request nil
		 qq-search--request-owner nil
		 qq-search--next-cursor nil
		 qq-search--status 'error
		 qq-search--pending-next-key nil
		 qq-search--error
		 (format "dispatch failed: %s; press g to restart"
			 (error-message-string error-data)))
           (qq-search--request-sync view))
	 nil)))))

(defun qq-search-search (query)
  "Replace this buffer with a server-backed search for QUERY."
  (interactive
   (list (read-string "Search messages: " qq-search--query
                      'qq-search-history)))
  (unless (and (stringp query) (not (string-empty-p query)))
    (user-error "qq: empty search query"))
  (setq query (string-trim query))
  (when (string-empty-p query)
    (user-error "qq: empty search query"))
  (when (> (length query) 512)
    (user-error "qq: search query must be at most 512 characters"))
  (qq-search--cancel-request)
  (let ((view (qq-search--ensure-view)))
    (setq qq-search--query query
          qq-search--results nil
          qq-search--results-tail nil
          qq-search--seen (make-hash-table :test #'equal)
          qq-search--consumed-cursors (make-hash-table :test #'equal)
          qq-search--next-cursor nil
          qq-search--status 'loading
          qq-search--error nil
          qq-search--pending-next-key nil
          qq-search--focus-first-result-p t)
    (let ((request (qq-search--start-request nil)))
      (qq-search--sync-now view)
      request)))

(defun qq-search-load-more ()
  "Load the next server-owned result page."
  (interactive)
  (cond
   (qq-search--request
    (message "qq: message search is already loading"))
   ((eq qq-search--status 'error)
    (user-error "qq: search failed; press g to restart from the first page"))
   ((not qq-search--next-cursor)
    (setq qq-search--status 'eof)
    (let ((view (qq-search--ensure-view)))
      (qq-search--request-sync view)
      (qq-search--sync-now view))
    (message "qq: reached end of search results"))
   (t
    (let ((view (qq-search--ensure-view)))
      (prog1 (qq-search--start-request t)
        (qq-search--sync-now view))))))

(defun qq-search-refresh ()
  "Restart the current search from the authoritative first page."
  (interactive)
  (qq-search-search qq-search--query))

(defun qq-search--result-at-point ()
  "Return the search result on the current line, or nil."
  (or (get-text-property (point) 'qq-search-result)
      (get-text-property (line-beginning-position) 'qq-search-result)))

(defun qq-search-next-result ()
  "Move to the next result, loading continuation pages when necessary."
  (interactive)
  (or (qq-search--next-local-result)
      (cond
       (qq-search--request
        (message "qq: message search is already loading")
        nil)
       (qq-search--next-cursor
        (let ((anchor (or (qq-search--result-at-point)
                          (car qq-search--results-tail))))
          (unless anchor
            (user-error "qq: cannot continue search navigation without an anchor"))
          (setq qq-search--pending-next-key (qq-search--result-key anchor))
          (let ((view (qq-search--ensure-view)))
            (qq-search--start-request t)
            (qq-search--sync-now view))
          t))
       (t
        (message "qq: no next search result")
        nil))))

(defun qq-search-previous-result ()
  "Move point to the previous result row without wrapping."
  (interactive)
  (let ((origin (point)) found done)
    (beginning-of-line)
    (while (and (not found) (not done))
      (if (= (point) (point-min))
          (setq done t)
        (forward-line -1)
        (when (qq-search--result-at-point)
          (setq found t))))
    (unless found
      (goto-char origin)
      (message "qq: no previous search result"))
    found))

(defun qq-search-open-result ()
  "Open the exact actionable search result at point."
  (interactive)
  (qq-search--open-result
   (or (get-text-property (point) 'qq-search-result)
       (user-error "qq: no search result at point"))))

(defun qq-search--open-result (result)
  "Open exact message-search RESULT from the current search buffer."
  (let ((session-key
         (qq-api-session-key-from-locator (alist-get 'chat result))))
    (qq-chat-open-message session-key (alist-get 'message_id result)
                          qq-search--query)))

(defvar qq-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'qq-search-next-result)
    (define-key map (kbd "p") #'qq-search-previous-result)
    (define-key map (kbd "RET") #'qq-search-open-result)
    (define-key map (kbd "m") #'qq-search-load-more)
    (define-key map (kbd "g") #'qq-search-refresh)
    (define-key map (kbd "s") #'qq-search-search)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `qq-search-mode'.")

(define-derived-mode qq-search-mode special-mode "QQ-Search"
  "Major mode for paginated QQ message-search results."
  (setq-local truncate-lines t)
  (setq-local qq-search--ewoc nil)
  (setq-local qq-search--node-table nil)
  (add-hook 'kill-buffer-hook #'qq-search--cancel-request nil t)
  (add-hook 'change-major-mode-hook #'qq-search--cancel-request nil t))

(defun qq-search-open (session-key &optional query)
  "Open `*qq-search*' for SESSION-KEY and search for QUERY."
  (let* ((identity (qq-state-session-key-identity session-key))
         (type (alist-get 'type identity)))
    (unless (memq type '(group private))
      (user-error "qq: message search is unsupported for %s sessions" type)))
  (setq query
        (or query
            (read-string "Search messages: " nil 'qq-search-history)))
  (unless (and (stringp query) (not (string-empty-p query)))
    (user-error "qq: empty search query"))
  (let* ((app (qq-runtime-app))
         (view-id (qq-search--view-id session-key))
         (buffer (get-buffer-create qq-search-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'qq-search-mode)
        (qq-search-mode))
      (when-let* ((current (appkit-current-view)))
        (when (and (appkit-view-live-p current)
                   (not (equal view-id (appkit-view-id current))))
          (appkit-kill-view current)))
      (setq qq-search--session-key session-key))
    (let ((view
           (appkit-open-view
            :app app
            :id view-id
            :mode 'qq-search-mode
            :buffer-name qq-search-buffer-name
            :sync-function #'qq-search--sync-invalidations
            :parts '(results)
            :setup #'qq-search--setup-view)))
      (setq buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (qq-search-search query))
    (pop-to-buffer buffer)
    buffer))

(provide 'qq-search)

;;; qq-search.el ends here
