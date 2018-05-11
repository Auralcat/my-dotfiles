;;; amx-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "amx" "amx.el" (23286 8142 962004 964000))
;;; Generated autoloads from amx.el

(defvar amx-mode nil "\
Non-nil if Amx mode is enabled.
See the command `amx-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `amx-mode'.")

(custom-autoload 'amx-mode "amx" nil)

(autoload 'amx-mode "amx" "\
Use ido completion for M-x

\(fn &optional ARG)" t nil)

(autoload 'amx "amx" "\
Read a command name and execute the command.

This is the main entry point for the Amx package, an alternative
to the normal \\[execute-extended-command] built into Emacs that
provides several extra features.

\(fn)" t nil)

(autoload 'amx-major-mode-commands "amx" "\
Like `amx', but limited to commands that are relevant to the active major mode.

\(fn)" t nil)

(autoload 'amx-initialize "amx" "\
Ensure that amx is properly initialized.

This function is normally idempotent, only having an effect the
first time it is called, so it is safe to call it at the
beginning of any function that expects amx to be initialized.
However, optional arg REINIT forces the initialization needs to
be re-run. Interactively, reinitialize when a prefix arg is
provided.

\(fn &optional REINIT)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; amx-autoloads.el ends here
