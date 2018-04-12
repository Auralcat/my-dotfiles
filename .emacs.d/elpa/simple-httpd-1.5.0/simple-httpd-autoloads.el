;;; simple-httpd-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "../../../my-dotfiles/.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd"
;;;;;;  "simple-httpd.el" "80e68570dd79872d63a5686babd39409")
;;; Generated autoloads from simple-httpd.el

(autoload 'httpd-start "../../../my-dotfiles/.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "../../../my-dotfiles/.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

(autoload 'httpd-serve-directory "../../../my-dotfiles/.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil "simple-httpd" "../../../../.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd.el"
;;;;;;  "80e68570dd79872d63a5686babd39409")
;;; Generated autoloads from ../../../../.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd.el

(autoload 'httpd-start "simple-httpd" "\
Start the web server process. If the server is already
running, this will restart the server. There is only one server
instance per Emacs instance.

\(fn)" t nil)

(autoload 'httpd-stop "simple-httpd" "\
Stop the web server if it is currently running, otherwise do nothing.

\(fn)" t nil)

(autoload 'httpd-serve-directory "simple-httpd" "\
Start the web server with given `directory' as `httpd-root'.

\(fn DIRECTORY)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd-pkg.el"
;;;;;;  "../../../../.emacs.d/elpa/simple-httpd-1.5.0/simple-httpd.el"
;;;;;;  "simple-httpd.el") (23246 17196 68516 193000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; simple-httpd-autoloads.el ends here
