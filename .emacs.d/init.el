;; Emacs configuration file
;; Author: Auralcat
;; Started in May 2017.

;;-----ENV-CUSTOMIZATIONS-------------------------------------------------------

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/file-bouncer/emacs-backups")))

;; Backup files by copying them
(setq backup-by-copying t)

;; I'm too lazy to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

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

;; Multi-term - Run multiple terminals and interface with Emacs commands
(require 'multi-term)

;; Pug-mode - Work with .pug files
(require 'pug-mode)

;; Setup the program multi-term will need
(setq multi-term-program "/bin/bash")

;; js-comint - Run JS in the shell, like Nodejs.
(require 'js-comint)

;; Enhanced Ruby Mode
(add-to-list 'auto-mode-alist
                          '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
                            . enh-ruby-mode))

;; Ruby tools: Goodies for ruby-mode
(require 'ruby-tools)

;; Start ruby-tools with Enhanced Ruby mode
;; (add-hook 'enh-ruby-mode ruby-tools t)

;;-----KEYBINDINGS--------------------------------------------------------------
;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1 works too.
(define-key key-translation-map [?\C-h] [?\C-?])

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; Kill all the buffers matching the provided regex
(global-set-key [24 75] (quote kill-matching-buffers))

;; Open Emacs config file
;; (find-file "~/.emacs" t)

;; Move to beginning of line or indentation
(defun back-to-indentation-or-beginning () (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") (quote back-to-indentation-or-beginning))

;; Hippie-Expand: change key to M-SPC
(global-set-key "\M- " 'hippie-expand)

;; Org-mode: bind org-capture to C-c c
(global-set-key (kbd "\C-c c") (quote org-capture))

;; Org-mode: bind org-pomodoro to C-c p
(global-set-key (kbd "\C-c p") (quote org-pomodoro))

;; Javascript: Pass buffer to Node.js
(add-hook 'js2-mode-hook ;; guessing
  '(lambda ()
     (local-set-key "\C-c \C-r" (quote shell-command-on-region 1 4692 "nodejs" nil nil nil t))))

;; SGML mode (AKA HTML mode) - Open buffer in browser
(add-hook 'sgml-mode-hook
  '(lambda ()
     (local-set-key "\C-c\C-o" (quote browse-url-of-buffer))))

;;-----VARIABLES----------------------------------------------------------------

;; Set Org mode as default mode for new buffers:
(setq-default major-mode 'org-mode)

;; Enable auto-fill mode by default
(auto-fill-mode 1)

;; Set default fill to 79
(set-fill-column 79)

;; Set line number mode and column number mode for code files
(line-number-mode 1)
(column-number-mode 1)

;; Set desktop save mode - opens last saved session when you start Emacs.
(desktop-save-mode 1)

;; Set autocomplete mode as default
(auto-complete-mode t)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Making Emacs auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)

;; Python indentation
(setq python-indent 4)

;; Pug-mode: use 2 spaces for indentation (DEFAULT: 4)
(setq pug-tab-width 2)

;; Pug-mode: call pug-compile whenever a .pug file is saved.
;; Compiles ugly HTML by default, use `pug -P <file_name>`
;; if you want pretty instead
(add-hook 'after-save-hook #'pug-compile)

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
 '(image-animate-loop t)
 '(org-agenda-files nil)
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
 '(remember-data-file "~/file-bouncer/everything-bucket"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
