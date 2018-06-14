
;; A small performance improvement
(setq redisplay-dont-pause t)

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/file-bouncer/emacs-backups")))

;; Manual packages load path
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/ob-elixir/")
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/emacs-solargraph/")

;; Backup files by copying them
(setq backup-by-copying t)

;; Save my desktop
(desktop-save-mode 1)

;; I'm too lazy to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Clean whitespace before saving a file
(add-hook 'before-save-hook (quote whitespace-cleanup))

;; Enable ido-mode (fewer keystrokes to switch buffers!)
(ido-mode 1)

;; ido-mode in the minibuffer
(icomplete-mode 1)

;; Enable windmove (switch windows with Shift+Arrow keys)
(when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings))

;; Replace the built-in buffer menu with ibuffer
(global-set-key [24 2] (quote ibuffer))

;; Prevent the scratch buffer from being killed
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; Set default font for Emacs
(set-frame-font "Ubuntu Mono 12")

;; Enable auto-revert-mode
(global-auto-revert-mode t)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; Org-babel - load ob-elixir
(load "ob-elixir")

;; Org-babel - load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (ruby . t)
   (elixir . t)
   (plantuml . t)
   (dot . t)
   ))

;; Use Bash as default shell interpreter
(setq org-babel-sh-command "/bin/bash")

;; Activate Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Enable global Abbrev mode
(setq-default abbrev-mode t)

;; Prog-mode is from where all the programming modes are derived from.
;; This means that if you call prog-mode-hook, the settings will be
;; applied to ALL programming modes in Emacs.
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'text-mode-hook 'column-number-mode)

;; Ruby
;; Activate ruby-tools
(add-hook 'enh-ruby-mode-hook (quote ruby-tools-mode))

;; Create filling for org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Use Weechat from Emacs
(require-package 'weechat)
;; Bind M-p to switch to previous buffer

;; Recreate scratch buffer
(defun create-scratch-buffer ()
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-mode)
  (insert initial-scratch-message)
  ;; Prevent the scratch buffer from being killed
  (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill)))

;; Unfill region, AKA leave single huge line
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
  logical line.  This is useful, e.g., for use with
  `visual-line-mode'."
  (interactive "*r")
    (let ((fill-column (point-max)))
      (fill-region beg end)))

;; Call the live page reload script from within Emacs and bind it to a key
;; (defun css-live-reload-current-webpage ()
;;     (when (and (stringp buffer-file-name)
;;       (string-match "\\.scss\\'" buffer-file-name))
;;     (shell-command "~/my-dotfiles/bash/live-reload-firefox.sh")))

;; The original idea is to call this function when saving a stylesheet
;; (add-hook 'after-save-hook 'css-live-reload-current-webpage)

;; This is how you define aliases for Elisp functions
(defalias 'plp 'package-list-packages)

(use-package sass-mode
   ;; Set Sass mode for SASS files and Css mode for SCSS files.
   :config
   (add-to-list 'auto-mode-alist
  '("\\.sass\\'" . sass-mode)))

(use-package scss-mode

   :config
   (add-to-list 'auto-mode-alist
  '("\\.scss\\'" . scss-mode)))

(use-package js2-mode)

;; Set js2-mode as default mode for JS files
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Set syntax highlight level
(setq js2-highlight-level 3)

(use-package php-mode)
;; Flymake support for PHP files
(use-package flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

(use-package enh-ruby-mode)

;; Set it as default mode for Ruby files
(add-to-list 'auto-mode-alist
    '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
    . enh-ruby-mode))

(require-package 'web-mode)

;; File associations
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Engine associations
(setq web-mode-engines-alist
    '(("php"    . "\\.phtml\\'")
    ("blade"  . "\\.blade\\."))
)

(use-package yaml-mode)

(use-package csv-mode)

(use-package projectile)
;; Enable it globally.
(add-hook 'after-init-hook #'projectile-global-mode)

(use-package autopair
   :init (autopair-global-mode))

(use-package emmet-mode)

(use-package highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(use-package flycheck
   :config
   ;; turn on flychecking globally
   (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package ruby-tools)

;; Ensure it's loaded
(use-package engine-mode)
;; Activate it
(engine-mode t)

;; Define search engines to use
(defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
(defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")
(defengine youtube
    "https://www.youtube.com/results?search_query=%s"
    :keybinding "y")
(defengine stackoverflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")



(use-package evil-leader)
(global-evil-leader-mode)
;; Evil mode needs to be loaded after evil-leader
(use-package evil)
(evil-mode 1)

;; Load configs
(load "~/my-dotfiles/.emacs.d/evilrc")

(use-package evil-surround)
(global-evil-surround-mode)

(require-package 'helm)
(require 'helm-config)
(helm-mode 1)

;; Bind the keys I want:
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "»") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Complete with tab in Helm buffer, remap action menu to C-tab
(define-key helm-map (kbd "<tab>") 'hippie-expand)
(define-key helm-map (kbd "C-<tab>") 'helm--action-prompt)

;; Enable fuzzy matching
(setq helm-M-x-fuzzy-match t)

(require-package 'company)

;; Add Tern to Company
(require-package 'company-tern)
(require-package 'tern)

;; Call that inside js2-mode and add tern to company backends
(defun tern-mode-tweaks ()
    (add-to-list 'company-backends 'company-tern)
    (tern-mode 1))
(add-hook 'js2-mode-hook 'tern-mode-tweaks)
;; Autocompletion for Bootstrap/FontAwesome classes
(require-package 'ac-html-bootstrap)

;; Web-mode completions
(require-package 'company-web)

;; Add web-mode completions when started
(require 'company-web-html)

;; Company statistics package
(use-package company-statistics)
(company-statistics-mode)

(require-package 'keyfreq)

;; Ignore arrow commands and self-insert-commands
(setq keyfreq-excluded-commands
    '(self-insert-command
    org-self-insert-command
    abort-recursive-edit
    forward-char
    backward-char
    previous-line
    next-line))

;; Activate it
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(use-package diminish
   ;; These are loaded at startup
   :diminish helm-mode
   :diminish company-mode
   :diminish undo-tree-mode
   :diminish auto-revert-mode
   :diminish auto-fill-function
   :diminish abbrev-mode
   :diminish autopair-mode)
;; These are loaded at other moments
(eval-after-load "editorconfig" '(diminish 'editorconfig-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))

(use-package editorconfig
   :init
   ;; Activate it.
   (editorconfig-mode 1))

(use-package nyan-mode)
(nyan-mode 1)

(use-package mode-icons
   :init
   (mode-icons-mode))

(use-package emojify)

(use-package rainbow-delimiters)
;; Add this to prog-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Smart-mode-line depends on powerline
(require-package 'powerline)
(require 'powerline)
(require-package 'smart-mode-line)

(use-package web-beautify)

(use-package magit)

;; Eshell extras
(use-package eshell-prompt-extras)

;; More configs
(with-eval-after-load "esh-opt"
(autoload 'epe-theme-lambda "eshell-prompt-extras")
(setq eshell-highlight-prompt t
    eshell-prompt-function 'epe-theme-lambda))

(use-package yasnippet-snippets)

(use-package theme-changer
   :config
   (progn ;; Set the location
   (setq calendar-location-name "Curitiba, PR")
   (setq calendar-latitude -25.41)
   (setq calendar-longitude -49.25)

   ;; Specify the day and night themes:
   (change-theme 'whiteboard 'fairyfloss)))

(use-package ace-jump-mode)

(use-package moe-theme)

;; Show highlighted buffer-id as decoration. (Default: nil)
(setq moe-theme-highlight-buffer-id t)

;; Activate SML
(sml/setup)
;; Choose a color for the mode line (Default: blue)
(powerline-moe-theme)
(moe-theme-set-color 'purple)

;; Set font in graphical mode
(when (display-graphic-p)
    ;; Use Fantasque Sans Mono when available
    (if (member "Fantasque Sans Mono" (font-family-list))
    (set-frame-font "Fantasque Sans Mono 12")
    '(set-frame-font "Ubuntu Mono 12" nil t))
    ;; Remove menu and scroll bars in graphical mode
    (menu-bar-mode 0)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    ;; Enable emoji images
    (global-emojify-mode)
    ;; Maximize frame on startup
    (toggle-frame-maximized))

;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1
;; works too.
(define-key key-translation-map [?\C-h] [?\C-?])

;; Unfill region
(define-key global-map "\C-\M-q" 'unfill-region)

;; Switch to last buffer - I do it all the time
(global-set-key [27 112] (quote mode-line-other-buffer))

;; Mapping AltGr-d to delete-other-windows,
;; Another symbol I don't use often.
(global-set-key [240] (quote delete-other-windows))

;; Access buffers with Alt-Gr b
(global-set-key [8221] (quote ido-switch-buffer))

;; Map the Home and End keys to go to the beginning and end of the buffer
(global-set-key [home] (quote beginning-of-buffer))
(global-set-key [end] (quote end-of-buffer))

;; Move to beginning of line or indentation
(defun back-to-indentation-or-beginning () (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") (quote back-to-indentation-or-beginning))

;; Hippie-Expand: change key to M-SPC; Replace dabbrev-expand
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\M-/" 'hippie-expand)

;; Eshell - bind M-p to go back to previous buffer
(defun eshell-tweaks ()
    "Keybindings for the Emacs shell"
    (local-set-key (kbd "M-p") 'switch-to-prev-buffer)
    "Start in Emacs mode"
    (evil-set-initial-state 'eshell-mode 'emacs))
(add-hook 'eshell-mode-hook 'eshell-tweaks)

;; Set C-x j to go to current clocked task in org-mode
(global-set-key (kbd "C-x j") 'org-clock-goto)

(defun web-mode-keybindings ()
    "Define mode-specific keybindings like this."
    (local-set-key (kbd "C-c C-v") 'browse-url-of-buffer)
    (local-set-key (kbd "C-c /") 'sgml-close-tag))

;; Add company backends when loading web-mode.
(defun web-mode-company-load-backends ()
    (company-web-bootstrap+)
    (company-web-fa+))

(add-hook 'web-mode-hook 'web-mode-keybindings)
(add-hook 'web-mode-hook 'web-mode-company-load-backends)

;; We don't need Flycheck in org-mode buffers. Usually.
(add-hook 'org-mode-hook '(lambda() (flycheck-mode 0)))

;; Bind org-capture to C-c c
(global-set-key (kbd "\C-c c") (quote org-capture))

;; Bind org-pomodoro to C-x p
(global-set-key (kbd "\C-x p") (quote org-pomodoro))

;; Open the agenda with C-c a
(global-set-key [3 97] (quote org-agenda))

;; Open subheading with C-c RET and invert with M-RET
(local-set-key [27 13] (quote org-ctrl-c-ret))
(local-set-key [3 13] (quote org-insert-subheading))

;; Org-agenda: point the files you want it to read
;; (setq org-agenda-files (list "~/file-bouncer/org-files/contact-based-system/"))

;; Always respect the content of a heading when creating todos!
(local-set-key [M-S-return] (quote org-insert-todo-heading-respect-content))

;; Map C-S-enter to org-insert-todo-subheading
(local-set-key [C-S-return] (quote org-insert-todo-subheading))

(use-package org-bullets
   :init
   (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

;; Set Org mode as default mode for new buffers:
(setq-default major-mode 'org-mode)

;; Enable auto-fill mode by default
(auto-fill-mode 1)

;; Set default fill to 79
(set-fill-column 79)

;; Set line number mode and column number mode for code files
(line-number-mode 1)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Making Emacs auto-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)
;; Don't do that for terminal mode!
(add-hook 'multi-term-mode-hook (setq-default show-trailing-whitespace nil))

(defun css-mode-tweaks()
  (emmet-mode 1)
  (rainbow-mode 1))

;; Emmet-mode: activate for html-mode, sgml-mode,
;; css-mode, web-mode and sass-mode
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'sass-mode-hook 'css-mode-tweaks)
(add-hook 'web-mode-hook 'emmet-mode)

;; By the way, it's nice to add rainbow-mode for CSS
(add-hook 'css-mode-hook 'css-mode-tweaks)

;; Python: use python3 as default shell interpreter
(setq python-shell-interpreter "python3")

;; To save a macro, record it with C-x ( (start) and C-x ) (stop),
;; give it a name with C-x C-k n (C-k is for maKro) and
;; insert it in this file with insert-kbd-macro.
;; Then you execute it mapping it to a key! 😊

;; Example macro: Mark todos as done
(fset 'org-mark-as-done
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("d" 0 "%d")) arg)))

(use-package twittering-mode
    :config
    (
  ;; Adjust update interval in seconds. It's timeR, not time!
  (setq twittering-timer-interval 3600)

  ;; Display icons (if applicable)
  (setq twittering-icon-mode t)

  ;; Use a master password so you don't have to ask for authentication every time
  (setq twittering-use-master-password t))
    :bind-keymap
    (
  ("C-c r" . twittering-reply-to-user)
  ("C-c f" . twittering-favorite)
  ("C-c n" . twittering-native-retweet)))
