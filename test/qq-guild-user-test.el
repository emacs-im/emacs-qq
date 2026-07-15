;;; qq-guild-user-test.el --- Tests for native QQ channel member pages -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'qq-guild-user)

(defconst qq-guild-user-test--profile
  '((guild_id . "9007199254740993")
    (native_id . "144115219000000001")
    (display_name . "Synthetic member")
    (nickname . "Synthetic nickname")
    (member_name . "Synthetic member")
    (avatar_url . "https://example.invalid/synthetic-avatar.jpg")))

(ert-deftest qq-guild-user-refresh-renders-only-native-channel-identity ()
  (let (requested)
    (with-temp-buffer
      (qq-guild-user-mode)
      (setq qq-guild-user--guild-id "9007199254740993"
            qq-guild-user--native-id "144115219000000001")
      (cl-letf (((symbol-function 'qq-api-get-guild-member-profile)
                 (lambda (guild-id native-id callback &optional _errback)
                   (setq requested (list guild-id native-id))
                   (funcall callback (copy-tree qq-guild-user-test--profile))
                   'synthetic-request))
                ((symbol-function 'qq-state-guild)
                 (lambda (_guild-id)
                   '((name . "Synthetic Guild"))))
                ((symbol-function 'qq-media-url-preview-display-string)
                 (lambda (&rest _args) "[avatar]")))
        (qq-guild-user-refresh))
      (should (equal requested
                     '("9007199254740993" "144115219000000001")))
      (should (equal qq-guild-user--profile qq-guild-user-test--profile))
      (should (string-match-p "Synthetic Guild" (buffer-string)))
      (should (string-match-p "Synthetic member" (buffer-string)))
      (should (string-match-p "频道 ID 是 GPro tinyId，不是 QQ 号"
                              (buffer-string)))
      (should-not (string-match-p "私聊\|加好友\|QQ:" (buffer-string))))))

(ert-deftest qq-guild-user-open-avatar-uses-profile-url-and-shared-cache-key ()
  (let (opened)
    (with-temp-buffer
      (qq-guild-user-mode)
      (setq qq-guild-user--guild-id "9007199254740993"
            qq-guild-user--native-id "144115219000000001"
            qq-guild-user--profile (copy-tree qq-guild-user-test--profile))
      (cl-letf (((symbol-function 'qq-media-open-image-url)
                 (lambda (key url) (setq opened (list key url)))))
        (qq-guild-user-open-avatar))
      (should
       (equal opened
              '("guild-member-avatar:9007199254740993:144115219000000001"
                "https://example.invalid/synthetic-avatar.jpg"))))))

(provide 'qq-guild-user-test)

;;; qq-guild-user-test.el ends here
