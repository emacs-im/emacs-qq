# emacs-qq

A NapCat-backed QQ client for Emacs.

Current scope:

- connect to NapCat over OneBot websocket
- keep recent sessions, friend list, and group list in memory
- use appkit's shared UI and chat runtime across root and chat buffers
- keep chat keybindings closer to telega.el conventions
- handle live message events, replies, QQ media resources, and basic recall notices
- complete native group members and QQ faces directly in the composer

Quick start:

```elisp
(require 'qq)

(setq qq-onebot-websocket-url "ws://127.0.0.1:3001/")
(setq qq-onebot-token "YOUR_ONEBOT_TOKEN")
```

Then run `M-x qq`.

In a group composer, type `@` followed by a group card, nickname, QID, or QQ
number and press `TAB`.  The selected row is stored as a real QQ `at` segment,
so it notifies the member; plain typed text is never treated as a mention.
Type `/` plus a QQ face name and press `TAB` for inline base-face completion;
type `/fav` (optionally followed by a description) for the visual favorite-face
list.  Favorite selection inserts a real image/mface object while preserving
its thumbnail in the composer.  `:unicode_name:` completes standard Unicode
emoji through Appkit.  `C-c C-e` opens the image-annotated base-face picker,
while `C-u C-c C-e` opens favorite faces directly.

Desktop notifications follow telega's opt-in global minor-mode model:

```elisp
(qq-notifications-mode 1)
```

Notifications are delayed briefly and suppressed when the target chat is
selected on a focused frame.  Muted chats stay quiet except for native QQ
`@我`; `@全体成员` mute breakthrough is controlled by
`qq-notifications-at-all-breaks-mute`.  `M-x qq-notifications-history` opens
the local notification history.

Development with Eask:

- `eask install-deps`
- `eask recompile`
- `eask run script test`
- `eask run script test-local` — force-refresh the sibling `appkit.el`, then
  run the tests
- `eask emacs -Q -L . -l qq.el`

For live two-repository development, Eask can link the appkit source package:

```sh
eask link add appkit ../appkit.el
```

Use `eask link list` to inspect active Eask links and
`eask link delete appkit-0.1.0` to return to snapshot installs.

Architecture notes:

- QQ depends only on appkit's view, presentation, media, chat-buffer,
  completion, and chat-timeline infrastructure; it has no runtime dependency
  on disco.el
- `qq-media.el` provides QQ-specific resource resolution for avatars, images, files, and base emojis
- `qq-chat.el` adapts OneBot segments to appkit's compact media-card and action-context APIs
- `qq-root.el` uses appkit's keyed one-line root layout
- `qq-chat.el` projects QQ messages into the shared keyed chat timeline
- chat keybindings currently center on telega-like habits (`RET`, `M-p`, `M-n`, `C-c C-c`, `C-c C-k`, `r`, `C-c C-r`, `C-c C-d`, `o`, `a`)

Recommended NapCat settings for development:

- websocket enabled on `ws://127.0.0.1:3001/`
- `messagePostFormat = array`
- `reportSelfMessage = true`
