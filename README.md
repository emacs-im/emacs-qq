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

The linked repository's `.elc` files are used by Emacs, so run
`eask recompile` from `../disco.el` after changing disco.  Use
`eask link list` to inspect active links and `eask link delete disco-0.1.0` to
return to snapshot installs.

Architecture notes:

- `qq-ui.el` and `qq-view.el` provide shared rendering helpers copied/adapted from disco.el concepts
- `qq-media.el` provides QQ-specific resource resolution for avatars, images, files, and base emojis
- `qq-root.el` intentionally follows the disco.el root-buffer direction
- `qq-chat.el` is the primary chat implementation; `qq-room.el` stays as a compatibility shim
- chat keybindings currently center on telega-like habits (`RET`, `M-p`, `M-n`, `C-c C-c`, `C-c C-k`, `r`, `C-c C-r`, `C-c C-d`, `o`, `a`)

Recommended NapCat settings for development:

- websocket enabled on `ws://127.0.0.1:3001/`
- `messagePostFormat = array`
- `reportSelfMessage = true`
