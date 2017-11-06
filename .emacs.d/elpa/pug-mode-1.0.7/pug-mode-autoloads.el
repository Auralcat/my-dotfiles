;;; pug-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "pug-mode" "../../../../.emacs.d/elpa/pug-mode-1.0.7/pug-mode.el"
;;;;;;  "24ad7a6f01003c2818a077014e0d7c2a")
;;; Generated autoloads from ../../../../.emacs.d/elpa/pug-mode-1.0.7/pug-mode.el

(autoload 'pug-mode "pug-mode" "\
Major mode for editing Pug files.

\\{pug-mode-map}

\(fn)" t nil)

(autoload 'pug-compile "pug-mode" "\
Compile the current pug file into html, using pug-cli.

If the universal argument is supplied, render pretty HTML (non-compressed).

\(fn &optional ARG)" t nil)

(add-to-list 'auto-mode-alist '("\\.\\(jade\\|pug\\)\\'" . pug-mode))

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/pug-mode-1.0.7/pug-mode-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/pug-mode-1.0.7/pug-mode.el") (23040
;;;;;;  56379 554388 153000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; pug-mode-autoloads.el ends here
