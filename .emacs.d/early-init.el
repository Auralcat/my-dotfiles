;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Disable package.el in favor of use-package
(setq package-enable-at-startup nil)

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Reduce file handler operations during startup
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Disable UI elements early for faster startup
(setq tool-bar-mode nil
      menu-bar-mode nil
      scroll-bar-mode nil
      tooltip-mode nil
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-buffer-menu t)

;; Frame optimizations
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Improve subprocess performance
(setq read-process-output-max (* 1024 1024)) ; 1MB

;; Native compilation optimizations for Emacs 28+
(when (fboundp 'native-compile-async)
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-jit-compilation t))

;; Reset GC and file handlers after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024) ; 16MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;;; early-init.el ends here