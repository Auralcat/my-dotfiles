;; Emacs configuration file
;; Author: Auralcat
;; Started in May 2017.

;;-----ENV-CUSTOMIZATIONS-------------------------------------------------------

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/.emacs_backups")))

;; Backup files by copying them
(setq backup-by-copying t)

;;-----PACKAGES-----------------------------------------------------------------
;; Package sources
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
        (package-install package)))

;; Autopair - Automatically pair braces and quotes like in TextMate
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Enable improved Javascript mode:

(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))   (autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

;;-----KEYBINDINGS--------------------------------------------------------------
;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1 works too.
(keyboard-translate ?\C-h ?\C-?)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; Kill all the buffers matching the provided regex
(global-set-key [24 75] (quote kill-matching-buffers))

;; Open Emacs config file
;; (find-file "~/.emacs" t)

;; Set C-M-j to join lines
(global-set-key [27 10] (quote join-line))

;; Better buffer movement. C-x n goes to the next buffer and C-x p goes back
(global-set-key [27 110] (quote switch-to-next-buffer))
(global-set-key [27 112] (quote switch-to-prev-buffer))

;;-----VARIABLES----------------------------------------------------------------
;; Flycheck adjustments

;; Set Text mode as default mode for new buffers:
(setq-default major-mode 'text-mode)

;; Set Type Break mode on for all buffers (helps with ergonomics)
(type-break-mode 1)

;; Set default fill to 79
(set-fill-column 79)

;; Set line number mode and column number mode for code files
(line-number-mode 1)
(column-number-mode 1)

;; Set autocomplete mode as default
(auto-complete-mode t)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)

;; Python indentation
(setq python-indent 4)
(put 'upcase-region 'disabled nil)

;; Python: use python3 as default shell interpreter
(setq python-shell-interpreter "python3")

;;-----CUSTOMIZATION-THROUGH-M-x-CUSTOMIZE--------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(browse-url-firefox-program "cyberfox")
 '(dynamic-completion-mode t)
 '(global-linum-mode t)
 '(image-animate-loop t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
