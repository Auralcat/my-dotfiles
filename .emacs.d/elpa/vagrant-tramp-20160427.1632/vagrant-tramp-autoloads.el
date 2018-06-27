;;; vagrant-tramp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "vagrant-tramp" "vagrant-tramp.el" (0 0 0 0))
;;; Generated autoloads from vagrant-tramp.el

(autoload 'vagrant-tramp--completions "vagrant-tramp" "\
List for vagrant tramp completion.  FILE argument is ignored.

\(fn &optional FILE)" nil nil)

(autoload 'vagrant-tramp-term "vagrant-tramp" "\
SSH into BOX-NAME using an `ansi-term'.

\(fn BOX-NAME)" t nil)

(autoload 'vagrant-tramp-add-method "vagrant-tramp" "\
Add `vagrant-tramp-method' to `tramp-methods'.

\(fn)" nil nil)

(define-obsolete-function-alias 'vagrant-tramp-enable 'vagrant-tramp-add-method)

(eval-after-load 'tramp '(progn (vagrant-tramp-add-method) (tramp-set-completion-function vagrant-tramp-method vagrant-tramp-completion-function-alist)))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "vagrant-tramp" '("vagrant-tramp-")))

;;;***

;;;### (autoloads nil nil ("vagrant-tramp-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vagrant-tramp-autoloads.el ends here
