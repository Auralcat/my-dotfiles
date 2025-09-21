;; -*- lexical-binding: t -*-

;; Startup time measurement
(defvar startup-time-start (current-time))

;; Precompute activation actions to speed up startup.
;; This requires the use of 'package-quickstart-refresh' every time the
;; activations need to be changed, such as when 'package-load-list' is modified.
(setq package-quickstart t)
(require 'package)

;; Package repositories (updated order for better performance)
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Ensure packages are initialized for use-package
(unless package--initialized
  (package-initialize))

;; Bootstrap use-package more efficiently
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package with optimizations
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(require 'bind-key)

;; Modern use-package configuration
(setopt use-package-always-ensure t
        use-package-always-defer t  ;; Defer everything by default
        use-package-expand-minimally t
        use-package-enable-imenu-support t
        use-package-compute-statistics t
        use-package-hook-name-suffix nil)


;; Load configuration more efficiently
(defun load-config-file ()
  "Load the configuration from myinit.org efficiently."
  (let ((config-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.org"))
        (compiled-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.el")))
    ;; Only recompile if .org is newer than .el or .el doesn't exist
    (when (or (not (file-exists-p compiled-file))
              (file-newer-than-file-p config-file compiled-file))
      (org-babel-tangle-file config-file compiled-file)
      (byte-compile-file compiled-file))
    ;; Load the compiled file
    (load compiled-file nil 'nomessage)))

;; Use org-babel only when necessary
(require 'org)
(require 'ob-tangle)
(load-config-file)

;; Startup time reporting
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s seconds with %d garbage collections."
                     (format "%.2f"
                             (float-time
                              (time-subtract (current-time)
                                             startup-time-start)))
                     gcs-done)))

;; Essential custom variables (cleaned up from bloated version)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
    '("ea4dd126d72d30805c083421a50544e235176d9698c8c541b824b60912275ba1"
       "ef725db3dc8a3c1afb16c52e96fb6aa31f5b20e5d60df0e8bc66ac5f98f0f69b"
       "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195"
       "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3"
       "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773"
       "f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230"
       default))
 '(keyboard-coding-system 'utf-8-unix)
 '(package-selected-packages
    '(gcmh json-mode rspec-mode terraform-mode zoom-window yatemplate nord-theme
       doom-themes goto-last-change org-ql org-web-tools ef-themes zenburn-theme
       howm ob-elixir ob-async solarized-theme gruvbox-theme auto-package-update
       sqlformat ox-gfm circadian yasnippet-classic-snippets yasnippet-snippets
       restart-emacs golden-ratio olivetti smartparens magit-delta git-link
       exec-path-from-shell notmuch ace-window expand-region engine-mode
       mode-icons diminish keyfreq company emmet-mode flycheck-inline php-mode
       csv-mode js2-mode nov web-mode markdown-mode ob-mermaid mermaid-mode
       yaml-mode robe rainbow-delimiters projectile-rails mix indent-tools
       hl-todo highlight-numbers highlight-indentation git-gutter-fringe
       elixir-mode editorconfig diff-hl bundler))
 '(projectile-mode t nil (projectile))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
