;;; eruby-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "eruby-mode" "eruby-mode.el" (23194 55612 853758
;;;;;;  229000))
;;; Generated autoloads from eruby-mode.el

(autoload 'eruby-mode "eruby-mode" "\
Minor mode for eRuby templates

\(fn &optional ARG)" t nil)

(defconst eruby-mode-file-regexp "\\.erb\\'")

(add-to-list 'auto-mode-alist `(,eruby-mode-file-regexp ignore t))

(autoload 'eruby-mode-auto-mode "eruby-mode" "\
Turn on eRuby mode for appropriate file extensions.

\(fn)" nil nil)

(add-hook 'find-file-hook #'eruby-mode-auto-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eruby-mode-autoloads.el ends here
