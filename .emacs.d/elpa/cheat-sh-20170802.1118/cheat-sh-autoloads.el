;;; cheat-sh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cheat-sh" "../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh.el"
;;;;;;  "d46bff717161de02847cd8ea749c582e")
;;; Generated autoloads from ../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh.el

(autoload 'cheat-sh "cheat-sh" "\
Look up THING on cheat.sh and display the result.

\(fn THING)" t nil)

(autoload 'cheat-sh-region "cheat-sh" "\
Look up the text between START and END on cheat.sh.

\(fn START END)" t nil)

(autoload 'cheat-sh-maybe-region "cheat-sh" "\
If region is active lookup content of region, otherwise prompt.

\(fn)" t nil)

(autoload 'cheat-sh-help "cheat-sh" "\
Get help on using cheat.sh.

\(fn)" t nil)

(autoload 'cheat-sh-list "cheat-sh" "\
Get a list of topics available on cheat.sh.

Either gets a topic list for subject THING, or simply gets a list
of all available topics on cheat.sh if THING is supplied as an
empty string.

\(fn THING)" t nil)

(autoload 'cheat-sh-search "cheat-sh" "\
Search for THING on cheat.sh and display the result.

\(fn THING)" t nil)

(autoload 'cheat-sh-search-topic "cheat-sh" "\
Search TOPIC for THING on cheat.sh and display the result.

\(fn TOPIC THING)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "cheat-sh" "../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cheat-sh" '("cheat-sh-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/cheat-sh-20170802.1118/cheat-sh.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cheat-sh-autoloads.el ends here
