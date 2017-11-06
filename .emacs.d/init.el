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

;; Clean whitespace before saving a file
(add-hook 'before-save-hook (quote whitespace-cleanup))

;; Enable ido-mode (fewer keystrokes to switch buffers!)
(ido-mode 1)

;; Replace the built-in buffer menu with ibuffer
(global-set-key [24 2] (quote ibuffer))

;;-----HOOKS--------------------------------------------------------------------

;; General programming mode
(defun set-programming-tweaks ()
    (linum-mode 1)
    (column-number-mode 1))

;; Prog-mode is from where all the programming modes are derived from.
;; This means that if you call prog-mode-hook, the settings will be
;; applied to ALL programming modes in Emacs.
(add-hook 'prog-mode-hook (quote set-programming-tweaks))

;; Ruby
;; Activate ruby-tools
(add-hook 'enh-ruby-mode-hook (quote ruby-tools-mode))
;;-----IRC----------------------------------------------------------------------

;; Qui Nov  2 19:57:06 BRST 2017 - Tried using IRC inside Emacs, didn't please
;; me, too many buffers to work with... for now.

;;-----CUSTOM-FUNCTIONS---------------------------------------------------------

;; Recreate scratch buffer
(defun create-scratch-buffer (mode-to-be-used)
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-mode)
  (insert initial-scratch-message))

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
(require-package 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Multi-term - Run multiple terminals and interface with Emacs commands
(require-package 'multi-term)

;; Pug-mode - Work with .pug files
(require-package 'pug-mode)

;; Setup the program multi-term will need
(setq multi-term-program "/bin/bash")

;; Enhanced Ruby Mode
(require-package 'enh-ruby-mode)
(add-to-list 'auto-mode-alist
                          '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
                            . enh-ruby-mode))

;; Ruby tools: Goodies for Ruby programming modes
(require-package 'ruby-tools)

;; Magit - Work with Git inside Emacs
(require-package 'magit)

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

;; Set desktop save mode - opens last saved session when you start Emacs.
(desktop-save-mode 1)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Making Emacs auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)
;; Don't do that for terminal mode!
(add-hook 'multi-term-mode-hook (setq-default show-trailing-whitespace nil))

;; Python indentation
(setq python-indent 4)

;; Pug-mode: use 2 spaces for indentation (DEFAULT: 4)
(setq pug-tab-width 2)

;; Pug-mode: call pug-compile whenever a .pug file is saved.
;; Compiles ugly HTML by default, use `pug -P <file_name>`
;; if you want pretty instead

(defun pug-compile-saved-file()
  (when (and (stringp buffer-file-name)
             (string-match "\\.pug\\'" buffer-file-name))
     (pug-compile)))
(add-hook 'after-save-hook 'pug-compile-saved-file)

;; Python: use python3 as default shell interpreter
(setq python-shell-interpreter "python3")

;;-TWITTERING-MODE--------------------------------------------------------------
;; Adjust update interval in seconds. It's timeR, not time!
(setq twittering-timer-interval 3600)

;; Display icons (if applicable)
(setq twittering-icon-mode t)

;; Use a master password so you don't have to ask for authentication every time
(setq twittering-use-master-password t)

;;-----CUSTOMIZATION-THROUGH-M-x-CUSTOMIZE--------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(browse-url-firefox-program "cyberfox")
 '(custom-safe-themes
   (quote
    ("f5729cac6fcade83c92e50c823eca0e24add9e6c30d1bcafa5076e07909a1ed2" "4134cc495b3e774194861d932a0c094dc729328664b00d82e02c4d31fead14e0" "3856e441793a643f6b2e733b70f3fc2d8e74cf810368eff22942f738d89c4854" "9b803ba5ea6d16e838c3229c7679e8325cbe43083cd4155202925b3f11c3c912" "c9dd6b38801d2db93472aaa63aa15e7c0de4f3f2acecc72b04ca6b3efa25f600" "764b72b168f2a5935f51977c49781c9864312fd9f45e3cc317997d12aa8db3bf" "40af07c028aab91628cac1a41c62f8e4b4e893bef605765196e6b3d8a02ab2e0" "25b0083e7ffd77261165fc391b8a7f94049537a58536da2f0e6fd83ecff30ae8" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" default)))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(dynamic-completion-mode t)
 '(enh-ruby-use-encoding-map nil)
 '(erc-away-nickname nil)
 '(erc-modules
   (quote
    (completion dcc fill list netsplit networks notifications readonly ring smiley track hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list)))
 '(erc-nick "Auralcat")
 '(erc-nicklist-use-icons nil)
 '(erc-nicklist-voiced-position (quote top))
 '(erc-script-path (quote ("~/my-dotfiles/.emacs.d/.erc/")))
 '(erc-try-new-nick-p t)
 '(erc-user-full-name "Realnamezz")
 '(global-linum-mode nil)
 '(image-animate-loop t)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote org-mode))
 '(initial-scratch-message
   "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(keyboard-coding-system (quote utf-8-unix))
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
