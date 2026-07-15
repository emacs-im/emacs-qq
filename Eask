;; -*- mode: eask; lexical-binding: t -*-

(package "qq"
         "0.1.0"
         "NapCat-backed QQ chat client")

(website-url "https://github.com/0WD0/emacs-qq")

(keywords "comm")

(package-file "qq.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "websocket" "1.16")
(depends-on "transient" "0.7")
(depends-on "appkit" :file "../appkit.el")

(script "refresh-appkit" "eask -f install-file ../appkit.el")
(script "test" "eask test ert test/*.el")
(script "test-concise" "./scripts/test-concise.sh")
(script "test-local"
        "eask -f install-file ../appkit.el && eask recompile && eask test ert test/*.el")
