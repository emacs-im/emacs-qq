;;; qq-guild-channel-test.el --- Tests for QQ channel inspectors -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'appkit-chatbuf)
(require 'qq-guild-channel)

(ert-deftest qq-guild-channel-inspector-does-not-invent-a-chat-timeline ()
  (let* ((qq-state-change-hook nil)
         (qq-runtime--context-account-id "slot-a")
         (guild-id "9007199254740993")
         (channel-id "9007199254741999")
         buffer)
    (qq-state-select-account "slot-a")
    (qq-state-reset)
    (unwind-protect
        (progn
          (qq-state-apply-guild-directory
           `((guilds . (((guild_id . ,guild-id)
                         (name . "Synthetic guild")
                         (avatar_seq . "1")
                         (pinned_at))))
             (categories . (((guild_id . ,guild-id)
                             (category_id . "0")
                             (name . "")
                             (uncategorized . t)
                             (channel_ids . (,channel-id)))))
             (channels . (((guild_id . ,guild-id)
                           (channel_id . ,channel-id)
                           (guild_name . "Synthetic guild")
                           (name . "Synthetic live")
                           (kind . "live")
                           (avatar_seq . "2")
                           (pinned_at)
                           (latest_sequence . "0"))))))
          (setq buffer (qq-guild-channel-open guild-id channel-id))
          (with-current-buffer buffer
            (should (derived-mode-p 'qq-guild-channel-mode))
            (should-not (appkit-chatbuf-input-start-position))
            (should (string-match-p "◉  Synthetic live" (buffer-string)))
            (should (string-match-p "直播频道需要独立" (buffer-string)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (qq-runtime-stop-account "slot-a" t)
      (qq-state-reset))))

(provide 'qq-guild-channel-test)

;;; qq-guild-channel-test.el ends here
