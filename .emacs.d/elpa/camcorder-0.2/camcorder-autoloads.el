;;; camcorder-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "camcorder" "camcorder.el" (23334 26938 634944
;;;;;;  207000))
;;; Generated autoloads from camcorder.el

(let ((loads (get 'camcorder 'custom-loads))) (if (member '"camcorder" loads) nil (put 'camcorder 'custom-loads (cons '"camcorder" loads))))

(defconst camcorder-version "0.1" "\
Version of the camcorder package.")

(autoload 'camcorder-version "camcorder" "\
Version of the camcorder package.

\(fn)" t nil)

(autoload 'camcorder-record "camcorder" "\
Open a new Emacs frame and start recording.
You can customize the size and properties of this frame with
`camcorder-frame-parameters'.

\(fn)" t nil)

(autoload 'camcorder-start "camcorder" nil nil nil)

(defvar camcorder-mode nil "\
Non-nil if Camcorder mode is enabled.
See the command `camcorder-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `camcorder-mode'.")

(custom-autoload 'camcorder-mode "camcorder" nil)

(autoload 'camcorder-mode "camcorder" "\
Toggle Camcorder mode on or off.
With a prefix argument ARG, enable Camcorder mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is `toggle'.
\\{camcorder-mode-map}

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; camcorder-autoloads.el ends here
