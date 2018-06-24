;;; highlight-sexp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "highlight-sexp" "highlight-sexp.el" (23343
;;;;;;  65186 13430 8000))
;;; Generated autoloads from highlight-sexp.el

(autoload 'highlight-sexp-mode "highlight-sexp" "\
Minor mode to highlight the current zone according to its
    context, i.e. sexp, comment, string.

\(fn &optional ARG)" t nil)

(defvar global-highlight-sexp-mode nil "\
Non-nil if Global-Highlight-Sexp mode is enabled.
See the command `global-highlight-sexp-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-sexp-mode'.")

(custom-autoload 'global-highlight-sexp-mode "highlight-sexp" nil)

(autoload 'global-highlight-sexp-mode "highlight-sexp" "\
Toggle Highlight-Sexp mode in all buffers.
With prefix ARG, enable Global-Highlight-Sexp mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Sexp mode is enabled in all buffers where
`(lambda nil (highlight-sexp-mode t))' would do it.
See `highlight-sexp-mode' for more information on Highlight-Sexp mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; highlight-sexp-autoloads.el ends here
