# emacs-qq

A NapCat-backed QQ client for Emacs.

Current scope:

- connect to NapCat over OneBot websocket
- keep recent sessions, friend list, and group list in memory
- use a shared disco-inspired UI layer across root and chat buffers
- keep chat keybindings closer to telega.el conventions
- handle live message events, replies, QQ media resources, and basic recall notices

Quick start:

```elisp
(add-to-list 'load-path "/path/to/emacs-qq")
(require 'qq)

(setq qq-onebot-websocket-url "ws://127.0.0.1:3001/")
(setq qq-onebot-token "YOUR_ONEBOT_TOKEN")
```

Then run `M-x qq`.

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
- `eask run script test-local` — force-refresh the sibling `disco.el`
  snapshot, then run the tests
- `eask exec emacs -Q -L . -l qq.el`

`(depends-on "disco" :file "../disco.el")` is a package snapshot: a normal
`eask install-deps` skips it after the first install.  After changing disco,
use `eask run script refresh-disco` or `test-local`; both reinstall only disco
and regenerate its bytecode, avoiding stale source and `.elc` caches.

For live two-repository development, Eask can instead link the source package:

```sh
eask link add disco ../disco.el
```

`eask recompile` only refreshes Eask's own package environment.  A running
Doom/straight Emacs may use a separate build directory whose `.el` files are
linked to the repository but whose `.elc` files are independent.  After
changing disco, evaluate the following in that running Emacs to rebuild and
reload the bytecode it actually uses:

```elisp
(let* ((loaded (or (symbol-file 'disco-view--chars-xwidth 'defun)
                   (locate-library "disco-view")))
       (dir (file-name-directory loaded)))
  (byte-compile-file (expand-file-name "disco-view.el" dir))
  (load (expand-file-name "disco-view.elc" dir) nil nil t)
  (byte-compile-file (expand-file-name "disco-root-view.el" dir)))
```

Use `eask link list` to inspect active Eask links and
`eask link delete disco-0.1.0` to return to snapshot installs.

Architecture notes:

- QQ modules depend directly on disco's `disco-ui`, `disco-view`,
  `appkit-chatbuf`, and `appkit-chat-timeline` infrastructure
- `qq-media.el` provides QQ-specific resource resolution for avatars, images, files, and base emojis
- `qq-chat.el` adapts OneBot segments to the shared `disco-ins` compact media-card and `disco-media` action-context protocol; card layout and transient action routing are not QQ-specific
- `qq-root.el` intentionally follows the disco.el root-buffer direction
- `qq-chat.el` projects QQ messages into the shared keyed chat timeline
- chat keybindings currently center on telega-like habits (`RET`, `M-p`, `M-n`, `C-c C-c`, `C-c C-k`, `r`, `C-c C-r`, `C-c C-d`, `o`, `a`)

Recommended NapCat settings for development:

- websocket enabled on `ws://127.0.0.1:3001/`
- `messagePostFormat = array`
- `reportSelfMessage = true`
