;;; selectric-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "selectric-mode" "selectric-mode.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from selectric-mode.el

(defvar selectric-mode nil "\
Non-nil if Selectric mode is enabled.
See the `selectric-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `selectric-mode'.")

(custom-autoload 'selectric-mode "selectric-mode" nil)

(autoload 'selectric-mode "selectric-mode" "\
Toggle Selectric mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Selectric mode is enabled, your Emacs will sound like an IBM
Selectric typewriter.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "selectric-mode" '("selectric-")))

;;;***

;;;### (autoloads nil nil ("selectric-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; selectric-mode-autoloads.el ends here
