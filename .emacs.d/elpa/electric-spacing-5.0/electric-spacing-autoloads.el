;;; electric-spacing-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "electric-spacing" "electric-spacing.el" (23202
;;;;;;  32883 215426 192000))
;;; Generated autoloads from electric-spacing.el

(autoload 'electric-spacing-mode "electric-spacing" "\
Toggle automatic surrounding space insertion (Electric Spacing mode).
With a prefix argument ARG, enable Electric Spacing mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

This is a local minor mode.  When enabled, typing an operator automatically
inserts surrounding spaces.  e.g., `=' becomes ` = ',`+=' becomes ` += '.  This
is very handy for many programming languages.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; electric-spacing-autoloads.el ends here
