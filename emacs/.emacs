;; Emacs configuration file
;; Author: Auralcat
;; Started in May 2017.

;;-----PACKAGES-----------------------------------------------------------------
;; Fetching MELPA, a package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    (package-initialize)

;; Enables Evil mode
(require 'evil)
  (evil-mode 1)

;; YASnippet - Yet Another Snippet package.
(add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Autopair - Automatically pair braces and quotes like in TextMate
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;;-----KEYBINDINGS--------------------------------------------------------------
;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1 works too.
(keyboard-translate ?\C-h ?\C-?)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;;-----VARIABLES----------------------------------------------------------------
;; Set Text mode as default mode for new buffers:
(setq-default major-mode 'text-mode)

;; Set default fill to 79
(set-fill-column 79)

;; Set line number mode and column number mode for code files
(line-number-mode 1)
(column-number-mode 1)

;; Enable Autocomplete mode
(auto-complete-mode 1)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)

;; Python indentation
(setq python-indent 4)
(put 'upcase-region 'disabled nil)

;;-----CUSTOMIZATION-THROUGH-M-x-CUSTOMIZE--------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(dynamic-completion-mode t)
 '(global-linum-mode t)
 '(image-animate-loop t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
