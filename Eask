;; -*- mode: eask; lexical-binding: t -*-

(package "qq"
         "0.1.0"
         "NapCat-backed QQ client for Emacs")

(keywords "comm")

(package-file "qq.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "websocket" "1.16")
(depends-on "transient" "0.3")
(depends-on "plz" "0.8")
(depends-on "appkit" :file "../appkit.el")
(depends-on "disco" :file "../disco.el")

;; `depends-on :file' installs a snapshot and skips it once installed.  Disco
;; is co-developed next to emacs-qq, so provide an explicit refresh path that
;; forces package.el to replace the cached snapshot and byte-compile it again.
(script "refresh-disco" "eask -f install-file ../disco.el")
(script "refresh-appkit" "eask -f install-file ../appkit.el")
(script "test" "eask test ert test/*.el")
(script "test-local" "eask -f install-file ../appkit.el && eask -f install-file ../disco.el && eask test ert test/*.el")
