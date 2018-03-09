;;; js2-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras"
;;;;;;  "js2-imenu-extras.el" "a7580f86032a9ca1d6f6e543cf770ba7")
;;; Generated autoloads from js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras" "\


\(fn)" nil nil)

(autoload 'js2-imenu-extras-mode "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-mode"
;;;;;;  "js2-mode.el" "dc591d6261abb2195d8d814a49856834")
;;; Generated autoloads from js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-mode" "\
Toggle highlight of unused variables.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

(autoload 'js2-jsx-mode "../../../my-dotfiles/.emacs.d/elpa/js2-mode-20180301/js2-mode" "\
Major mode for editing JSX code.

To customize the indentation for this mode, set the SGML offset
variables (`sgml-basic-offset' et al) locally, like so:

  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset js2-basic-offset))
  (add-hook \\='js2-jsx-mode-hook #\\='set-jsx-indentation)

\(fn)" t nil)

;;;***

;;;### (autoloads nil "js2-imenu-extras" "../../../../.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras.el"
;;;;;;  "a7580f86032a9ca1d6f6e543cf770ba7")
;;; Generated autoloads from ../../../../.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras.el

(autoload 'js2-imenu-extras-setup "js2-imenu-extras" "\


\(fn)" nil nil)

(autoload 'js2-imenu-extras-mode "js2-imenu-extras" "\
Toggle Imenu support for frameworks and structural patterns.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "js2-mode" "../../../../.emacs.d/elpa/js2-mode-20180301/js2-mode.el"
;;;;;;  "dc591d6261abb2195d8d814a49856834")
;;; Generated autoloads from ../../../../.emacs.d/elpa/js2-mode-20180301/js2-mode.el

(autoload 'js2-highlight-unused-variables-mode "js2-mode" "\
Toggle highlight of unused variables.

\(fn &optional ARG)" t nil)

(autoload 'js2-minor-mode "js2-mode" "\
Minor mode for running js2 as a background linter.
This allows you to use a different major mode for JavaScript editing,
such as `js-mode', while retaining the asynchronous error/warning
highlighting features of `js2-mode'.

\(fn &optional ARG)" t nil)

(autoload 'js2-mode "js2-mode" "\
Major mode for editing JavaScript code.

\(fn)" t nil)

(autoload 'js2-jsx-mode "js2-mode" "\
Major mode for editing JSX code.

To customize the indentation for this mode, set the SGML offset
variables (`sgml-basic-offset' et al) locally, like so:

  (defun set-jsx-indentation ()
    (setq-local sgml-basic-offset js2-basic-offset))
  (add-hook \\='js2-jsx-mode-hook #\\='set-jsx-indentation)

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/js2-mode-20180301/js2-imenu-extras.el"
;;;;;;  "../../../../.emacs.d/elpa/js2-mode-20180301/js2-mode-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/js2-mode-20180301/js2-mode-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/js2-mode-20180301/js2-mode.el"
;;;;;;  "../../../../.emacs.d/elpa/js2-mode-20180301/js2-old-indent.el"
;;;;;;  "js2-imenu-extras.el" "js2-mode.el") (23194 45877 826258
;;;;;;  192000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; js2-mode-autoloads.el ends here
