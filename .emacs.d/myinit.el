;; Set fallback font
(set-face-attribute 'default nil :font "Ubuntu Mono" :height 120)

;; Sentences end with single spaces for me.
(setq sentence-end-double-space nil)
;; Change window title
(setq frame-title-format '("Yes, this is Emacs!"))

;; A small performance improvement
(setq redisplay-dont-pause t)

;; I don't like lockfiles
(setq create-lockfiles nil)

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/file-bouncer/emacs-backups")))

;; Manual packages load path
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/emacs-solargraph/")

;; Backup files by copying them
(setq backup-by-copying t)

;; I'm too lazy to type "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Clean whitespace before saving a file
(add-hook 'before-save-hook (quote whitespace-cleanup))

;; Allow only one theme at a time
(setq custom-theme-allow-multiple-selections nil)

;; Enable ido-mode (fewer keystrokes to switch buffers!)
(ido-mode 1)

;; Easier mark cycling, both local and global
(setq set-mark-command-repeat-pop t)

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

;; Enable auto-revert-mode
(global-auto-revert-mode t)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; ;; Use alternate icon fonts when available
;; (add-to-list 'load-path "~/.local/share/icons-in-terminal/")

;; Use Bash as default shell interpreter
(setq org-babel-sh-command "/bin/bash")

;; Activate Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Enable global Abbrev mode
(setq-default abbrev-mode t)

;; Save last edited place in files
(require 'saveplace)
(setq-default save-place t)
;; (save-place-mode 1)

;; Use dired asynchronously
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Load from external file.
(load "~/.emacs.secrets")

;; Prog-mode is from where all the programming modes are derived from.
;; This means that if you call prog-mode-hook, the settings will be
;; applied to ALL programming modes in Emacs.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'column-number-mode)

;; Ruby
;; Activate ruby-tools
(add-hook 'enh-ruby-mode-hook (quote ruby-tools-mode))

;; Create filling for org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Use Weechat from Emacs
(use-package weechat
 :bind (:map weechat-mode-map
       ("M-p" . previous-buffer)
       ("<up>" . weechat-previous-input)
       ("<down>" . weechat-next-input)
       ("ð" . delete-other-windows)
       ("”" . switch-to-buffer)))

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

(use-package elixir-mode)

(use-package flycheck-credo
 :config
 ((eval-after-load 'flycheck
   '(flycheck-credo-setup))
   (add-hook 'elixir-mode-hook 'flycheck-mode)))

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

(defun js2-mode-tweaks ()
;; Use company-yas as main backend
  (set (make-local-variable 'company-backends) '(company-yasnippet company-etags company-capf))
  (company-mode t))

(add-hook 'js2-mode-hook 'js2-mode-tweaks)

 ;; Replacing Tern with ac-js2 and js2-refactor
 (use-package ac-js2)
 (use-package js2-refactor
 :diminish js2-refactor-mode)

 ;; Add to js2-mode
 (add-hook 'js2-mode-hook #'js2-refactor-mode)
 (js2r-add-keybindings-with-prefix "C-c r")

;; Set syntax highlight level
(setq js2-highlight-level 3)

(use-package php-mode)
(add-hook 'php-mode-hook (lambda() (add-to-list 'company-backends 'company-php)))

(use-package enh-ruby-mode)

;; Set it as default mode for Ruby files
(add-to-list 'auto-mode-alist
'("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
. enh-ruby-mode))

(use-package web-mode :ensure t
:bind (:map web-mode-map
      ("C-<up>"    . web-mode-element-previous)
      ("C-<down>"  . web-mode-element-next)
      ("C-<left>"  . web-mode-element-beginning)
      ("C-<right>" . web-mode-tag-match)
      ("C-S-<up>"  . web-mode-element-parent)
      ("M-<up>"    . web-mode-element-content-select)
      ("C-k"       . web-mode-element-kill)
      ("M-RET"     . complete)))

;; File associations
(add-to-list 'auto-mode-alist '("\\.phtml\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'"    . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'"  . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'"   . web-mode))

;; Engine associations
(setq web-mode-engines-alist
'(("php"    . "\\.phtml\\'")
("blade"  . "\\.blade\\.")))

;; Highlight tag when editing
(setq web-mode-enable-current-element-highlight t)

(use-package yaml-mode :ensure t)

(use-package csv-mode)

(use-package alchemist :ensure t)
;; Activate it in Elixir mode
(add-hook 'elixir-mode-hook 'alchemist-mode)

(use-package projectile
 :init
 (setq projectile-keymap-prefix (kbd "C-c p")))
 ;; Enable it
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
;; Disable rubylint on default for Ruby modes.
;; If you need it, you can enable it locally using C-u C-c ! v.
(defun custom-disabled-ruby-checkers ()
 (add-to-list 'flycheck-disabled-checkers 'ruby-rubylint))
 (add-hook 'enh-ruby-mode-hook 'custom-disabled-ruby-checkers)

(use-package ruby-tools)



(use-package evil-leader)
(global-evil-leader-mode)
;; Evil mode needs to be loaded after evil-leader
(use-package evil)
(evil-mode 1)

;; Load configs
(load "~/my-dotfiles/.emacs.d/evilrc")

(use-package evil-surround)
(global-evil-surround-mode)

(use-package helm)
(require 'helm-config)
(helm-mode 1)

;; Bind the keys I want:
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "»") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Enable fuzzy matching
(setq helm-M-x-fuzzy-match t)

(use-package company)

;; Web-mode needs HTML and CSS completions.
;; JS is not satisfactory at this point IMO

(defun web-mode-tweaks ()
(require 'company-web-html)
(set (make-local-variable 'company-backends) '(company-web-html company-css))
(emmet-mode 1)
(company-mode t))

;; Completion for Ruby mode
(defun ruby-mode-tweaks ()
(require 'company-robe)
(set (make-local-variable 'company-backends) '(company-robe company-yasnippet)))
;; Add tweaks
(add-hook 'enh-ruby-mode-hook 'ruby-mode-tweaks)

;; Autocompletion for Bootstrap/FontAwesome classes
(use-package ac-html-bootstrap)

;; Web-mode completions
(use-package company-web)

;; Company statistics package
(use-package company-statistics)
(company-statistics-mode)

;; Company with prescient.el offers better sorting of completion candidates.
;; I don't know if it clashes with company-statistics.
(use-package company-prescient)

;; Activate it
(company-prescient-mode)

(use-package company-box
:diminish company-box-mode
:if window-system
:hook (company-mode . company-box-mode))

;; Add alternate icon font
(add-to-list 'load-path "~/.local/share/icons-in-terminal/")

;; Temporary fix
(add-to-list 'load-path "~/.emacs.d/manual-packages/font-lock+/")
(require 'font-lock+)
(require 'icons-in-terminal)

(setq company-box-icons-unknown 'fa_question_circle)

(setq company-box-icons-elisp
'((fa_tag :face font-lock-function-name-face) ;; Function
(fa_cog :face font-lock-variable-name-face) ;; Variable
(fa_cube :face font-lock-constant-face) ;; Feature
(md_color_lens :face font-lock-doc-face))) ;; Face

(setq company-box-icons-yasnippet 'fa_bookmark)

(use-package keyfreq)

;; Ignore arrow commands and self-insert-commands
(setq keyfreq-excluded-commands
'(self-insert-command
org-self-insert-command
weechat-self-insert-command
abort-recursive-edit
company-ignore
forward-char
backward-char
previous-line
next-line))

;; Activate it
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(use-package diminish :ensure t
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

(use-package evil-numbers
:bind ("C-c <up>" . evil-numbers/inc-at-pt)
      ("C-c <down>" . evil-numbers/dec-at-pt))

(use-package diff-hl
 :ensure
 :config
 ;; ((defun hl-diff-tweaks()
 ;;   (diff-hl-mode t)
 ;;   (diff-hl-flydiff-mode t))
 ;;   (add-hook 'prog-mode-hook 'hl-diff-tweaks))
 )

(use-package yatemplate
:ensure t
:init
(setq yatemplate-dir "~/.emacs.d/templates")
(yatemplate-fill-alist)
(auto-insert-mode 1))

(use-package evil-matchit
:ensure t
:init
(global-evil-matchit-mode 1))

(use-package evil-snipe
 :init
 ;; I just want override-mode, I use S for substituting an entire line
 ;; (evil-snipe-mode +1)
 (evil-snipe-override-mode +1)
 ;; Make search case insensitive
 (setq evil-snipe-smart-case t)
 ;; Currently this has a conflict with Magit
 (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package xterm-color
:ensure t
:requires (eshell)
:config
 ;; Set eshell $TERM envvar to xterm-256color
 (setenv "TERM" "xterm-256color"))

(use-package cheat-sh :ensure t)

(use-package writeroom-mode :ensure t)
;; Activate it manually, it doesn't play well with Moe modeline globally

(use-package restart-emacs)

(use-package restclient)

(use-package helm-projectile)
;; Activate it.
(helm-projectile-on)

(use-package rainbow-delimiters)
;; Add this to prog-mode
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Smart-mode-line depends on powerline
(use-package powerline :ensure t)

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

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . moe-light)
                           (:sunset  . jazz)))

  (circadian-setup))

(use-package ace-jump-mode)

(use-package robe)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;; Integrate with Company
(defun ruby-completion-tweaks ()
  ;; Robe-mode must be active for this to work.
(set (make-local-variable 'company-backends) '(company-robe company-yasnippet company-etags company-capf))
(company-mode t))
(add-hook 'enh-ruby-mode-hook 'ruby-completion-tweaks)

(use-package rvm)

(use-package evil-anzu)
(global-anzu-mode)

(use-package vagrant-tramp)

(use-package jazz-theme :ensure t
:defer t)

(use-package moe-theme
 :ensure t
 :config
 ;; I just want to touch the theme, don't use it
 (moe-dark)
 (disable-theme 'moe-dark)
 (powerline-moe-theme)
 ;; Choose a color for the mode line (Default: blue)
 (moe-theme-set-color 'yellow))

(use-package abyss-theme :ensure :defer t)

(use-package github-modern-theme :ensure :defer t)

(use-package intellij-theme :ensure :defer t)

(use-package doom-themes :ensure :defer t)

;; Set font in graphical mode
(when (display-graphic-p)
    ;; Use Fantasque Sans Mono when available
    (if (member "Fantasque Sans Mono" (font-family-list))
    (set-face-attribute (quote default) nil :font "Fantasque Sans Mono" :height 120)
    '(set-face-attribute (quote default) nil :font "Ubuntu Mono" :height 120))

    ;; Remove menu and scroll bars in graphical mode
    (menu-bar-mode 0)
    (tool-bar-mode 0)
    (scroll-bar-mode 0)
    ;; Enable emoji images
    (global-emojify-mode)
    ;; Enable them in the mode line as well.
    (global-emojify-mode-line-mode)
    ;; Maximize frame on startup
    (toggle-frame-maximized))

;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1
;; works too.
;; (define-key key-translation-map [?\C-h] [?\C-?])

;; Use the menu key for helm-m-x
(global-set-key [menu] (quote helm-M-x))

;; Unfill region
(define-key global-map "\C-\M-q" 'unfill-region)

;; Switch to last buffer - I do it all the time
(global-set-key [27 112] (quote mode-line-other-buffer))

;; Save buffer with F5
(global-set-key [f5] (quote save-buffer))

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

;; Elisp mode: use SPC SPC to eval buffer.
(add-hook 'emacs-lisp-mode-hook (lambda() (evil-leader/set-key "SPC" 'eval-buffer)))

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

;; Keep agenda file list in a single file so I can publish my config.
;; DO NOT use C-c [ or C-c ] to add/remove files to the agenda otherwise
;; Emacs will write the var to init.el
(setq org-agenda-files "~/file-bouncer/org-agenda-file-list.org")

;; Bind org-capture to C-c c
(global-set-key (kbd "\C-c c") (quote org-capture))

;; Open subheading with C-c RET and invert with M-RET
(local-set-key [27 13] (quote org-ctrl-c-ret))
(local-set-key [3 13] (quote org-insert-subheading))

;; Org-agenda: point the files you want it to read
;; (setq org-agenda-files (list "~/file-bouncer/org-files/contact-based-system/"))

;; Change default diary location
(setq diary-file "~/emacs-diary")

;; Use diary entries in org-agenda
(setq org-agenda-include-diary t)

;; Use C-RET to complete words in Org-mode
(local-set-key [C-return] (quote complete))

;; Always respect the content of a heading when creating todos!
(local-set-key [M-S-return] (quote org-insert-todo-heading-respect-content))

;; Map C-S-enter to org-insert-todo-subheading
(local-set-key [C-S-return] (quote org-insert-todo-subheading))

(use-package org-bullets
   :init
   (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(use-package ob-elixir)

(use-package ob-restclient)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
 ;; (sh . t)
(python . t)
(ruby . t)
(elixir . t)
(plantuml . t)
(dot . t)
 ))

(use-package org-pomodoro
  :bind ("C-x p" . org-pomodoro))

;; Display notification when a pomodoro is completed
(defun pomodoro-started-notification (title body)
  (notifications-notify :title title
                        :body body
                        :app-icon "~/my-dotfiles/.emacs.d/org-pomodoro/tomato.png"))

(defun pomodoro-finished-notification (title body)
  (notifications-notify :title title
                        :body body
                        :app-icon "~/my-dotfiles/.emacs.d/org-pomodoro/tomato.png"))

(add-hook 'org-pomodoro-finished-hook (lambda() (pomodoro-finished-notification "Pomodoro finished" "Time to take a break!")))
(add-hook 'org-pomodoro-started-hook (lambda() (pomodoro-started-notification "Pomodoro started!" "Testing stuff out.")))

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
(fset 'my-org-mark-as-done
  (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("d]]" 0 "%d")) arg)))

 ;; Quicker replies in Twittering-mode.
 (fset 'my-twittering-mode-reply-to-user
  (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 return 3 18 67] 0 "%d")) arg)))

(use-package twittering-mode
:bind (:map twittering-mode-map
      ("C-c r" . my-twittering-mode-reply-to-user)
      ("C-c f" . twittering-favorite)
      ("C-c n" . twittering-native-retweet)))

;; WIP, needs A LOT of remapping
;; Use evil-mode to navigate twittering's frame
;; (evil-set-initial-state 'twittering-mode 'emacs)

;; Adjust update interval in seconds. It's timeR, not time!
(setq twittering-timer-interval 3600)

;; Display icons (if applicable)
(setq twittering-icon-mode t)

;; Use a master password so you don't have to ask for authentication every time
(setq twittering-use-master-password t)
