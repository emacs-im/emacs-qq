;;; qq-guild-forum-test.el --- Tests for native QQ forum views -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-chatbuf)
(require 'qq-guild-forum)

(defconst qq-guild-forum-test--guild-id "9007199254740993")
(defconst qq-guild-forum-test--channel-id "9007199254741999")

(defun qq-guild-forum-test--directory ()
  "Return a strict synthetic directory containing one forum channel."
  `((guilds . (((guild_id . ,qq-guild-forum-test--guild-id)
                (name . "Synthetic guild")
                (avatar_seq . "1")
                (pinned_at))))
    (categories . (((guild_id . ,qq-guild-forum-test--guild-id)
                    (category_id . "0")
                    (name . "")
                    (uncategorized . t)
                    (channel_ids . (,qq-guild-forum-test--channel-id)))))
    (channels . (((guild_id . ,qq-guild-forum-test--guild-id)
                  (channel_id . ,qq-guild-forum-test--channel-id)
                  (guild_name . "Synthetic guild")
                  (name . "Synthetic forum")
                  (kind . "forum")
                  (avatar_seq . "2")
                  (pinned_at)
                  (latest_sequence . "0"))))))

(defun qq-guild-forum-test--post (post-id created-at title)
  "Return a strict synthetic forum post with POST-ID, CREATED-AT, and TITLE."
  (copy-tree
   `((chat . ((kind . "guild-channel")
              (guild_id . ,qq-guild-forum-test--guild-id)
              (channel_id . ,qq-guild-forum-test--channel-id)))
     (post_id . ,post-id)
     (created_at . ,created-at)
     (updated_at . nil)
     (channel_name . "Synthetic forum")
     (sender . ((native_id . "144115219000000001")
                (display_name . "Synthetic member")
                (avatar_url . "https://example.invalid/avatar.png")))
     (state . "live")
     (title . ,title)
     (comment_count . 3)
     (segments . (((kind . "text")
                   (payload . ((text . "Synthetic body")))))))))

(ert-deftest qq-guild-forum-removes-only-an-exact-native-title-prefix ()
  (let* ((segments
          '(((type . "text") (data . ((text . "Synthetic "))))
            ((type . "text") (data . ((text . "title and body"))))
            ((type . "image") (data . ((file . "synthetic.png"))))))
         (trimmed
          (qq-guild-forum--segments-without-title-prefix
           segments "Synthetic title")))
    (should
     (equal trimmed
            '(((type . "text") (data . ((text . " and body"))))
              ((type . "image") (data . ((file . "synthetic.png")))))))
    (should
     (equal (qq-guild-forum--segments-without-title-prefix
             segments "Different title")
            segments))))

(ert-deftest qq-guild-forum-is-a-feed-directory-with-opaque-pagination ()
  (let* ((qq-state-change-hook nil)
         (qq-media-cache-update-hook nil)
         (app (appkit-start-app 'qq :id (make-symbol "qq-forum-test")))
         (qq-runtime--app app)
         (session-key
          (qq-state-guild-channel-session-key
           qq-guild-forum-test--guild-id qq-guild-forum-test--channel-id))
         (newest
          (qq-guild-forum-test--post
           "B_synthetic_newest" 1784000100 "Newest post"))
         (older
          (qq-guild-forum-test--post
           "B_synthetic_older" 1784000000 "Older post"))
         cursors
         buffer)
    (qq-state-reset)
    (unwind-protect
        (progn
          (setf (alist-get 'text
                           (alist-get 'payload
                                      (car (alist-get 'segments newest))))
                "Newest post")
          (qq-state-apply-guild-directory
           (qq-guild-forum-test--directory))
          (cl-letf (((symbol-function 'qq-runtime-app) (lambda () app))
                    ((symbol-function 'qq-media-message-avatar-image)
                     (lambda (_message) nil))
                    ((symbol-function 'qq-api-fetch-guild-forum-page)
                     (lambda (candidate-session cursor callback
                              &optional _errback)
                       (should (equal candidate-session session-key))
                       (push cursor cursors)
                       (if (string-empty-p cursor)
                           (progn
                             (qq-state-replace-guild-forum-posts
                              session-key (list newest))
                             (funcall
                              callback
                              `((posts . (,newest))
                                (next_cursor . "opaque-page-2")
                                (finished . :false))))
                         (should (equal cursor "opaque-page-2"))
                         (qq-state-merge-guild-forum-post older)
                         (funcall
                          callback
                          `((posts . (,older))
                            (next_cursor . nil)
                            (finished . t))))
                       'synthetic-request)))
            (setq buffer
                  (qq-guild-forum-open
                   qq-guild-forum-test--guild-id
                   qq-guild-forum-test--channel-id))
            (with-current-buffer buffer
              (appkit-sync-invalidations (appkit-current-view))
              (should (derived-mode-p 'qq-guild-forum-mode))
              (should-not (appkit-chatbuf-input-start-position))
              (should (string-match-p "Newest post" (buffer-string)))
              (save-excursion
                (goto-char (point-min))
                (should (= (how-many "Newest post") 1)))
              (should-not (string-match-p "Older post" (buffer-string)))
              (should (equal qq-guild-forum--next-cursor "opaque-page-2"))
              (qq-guild-forum-load-older)
              (appkit-sync-invalidations (appkit-current-view))
              (should (string-match-p "Newest post" (buffer-string)))
              (should (string-match-p "Older post" (buffer-string)))
              (should qq-guild-forum--finished-p)
              (should-not qq-guild-forum--next-cursor)))
          (should (equal (nreverse cursors) '("" "opaque-page-2"))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (when (appkit-app-live-p app)
        (appkit-stop-app app))
      (qq-state-reset))))

(provide 'qq-guild-forum-test)

;;; qq-guild-forum-test.el ends here
