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

;; (define-global-minor-mode my-linum-column-number-mode linum-mode
;;   (lambda ()
;;     ;; Add the modes you don't want linum to trigger in
;;     (when (not (memq major-mode
;;                      (list 'erc-mode 'term-mode 'minibuffer-mode)))
;;       (linum-mode)
;;       (column-number-mode))))

(my-linum-column-number-mode 1)
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
(autoload 'js2-mode "js2" nil t)

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

;; Js2-mode: set syntax highlight level
(setq js2-highlight-level 3)

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
 '(display-time-24hr-format t)
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
 '(my-linum-column-number-mode t)
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
