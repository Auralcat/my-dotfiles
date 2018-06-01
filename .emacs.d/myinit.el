
;; A small performance improvement
(setq redisplay-dont-pause t)

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/file-bouncer/emacs-backups")))

;; Manual packages load path
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/ob-elixir/")
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/emacs-solargraph/")

;; Backup files by copying them
(setq backup-by-copying t)

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

;; Load the Fairyfloss theme on startup
(load-theme 'fairyfloss)

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

;; Enable projectile-mode
;; (projectile-mode 1)

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

;; I usually keep the terminal window with some transparency to copy stuff from
;; a browser or whatever... this allows me to toggle the transparency from
;; Emacs and saves the theme I had before
;; (defun toggle-terminal-transparency ())

;; Call the live page reload script from within Emacs and bind it to a key
;; (defun css-live-reload-current-webpage ()
;;     (when (and (stringp buffer-file-name)
;;       (string-match "\\.scss\\'" buffer-file-name))
;;     (shell-command "~/my-dotfiles/bash/live-reload-firefox.sh")))

;; The original idea is to call this function when saving a stylesheet
;; (add-hook 'after-save-hook 'css-live-reload-current-webpage)

;; This is how you define aliases for Elisp functions
(defalias 'plp 'package-list-packages)

;; Autopair - Automatically pair braces and quotes like in TextMate
(require-package 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; Multi-term - Run multiple terminals and interface with Emacs commands
(require-package 'multi-term)

;; ...of this, and...
(require-package 'emmet-mode)

;; ...this!
(require-package 'sass-mode)

;; Set Sass mode for SASS files and Css mode for SCSS files.
(add-to-list 'auto-mode-alist
      '("\\.sass\\'" . sass-mode))

(add-to-list 'auto-mode-alist
      '("\\.scss\\'" . css-mode))

;; js2-mode - A better default Javascript mode
(require-package 'js2-mode)

;; Set js2-mode as default mode for JS files
(add-to-list 'auto-mode-alist
      '("\\(?:\\.js\\|jsx\\|)file\\)\\'"
    . js2-mode))

;; Web-beautify - Format HTML/CSS and JS code with js-beautify
(require-package 'web-beautify)

;; Set syntax highlight level
(setq js2-highlight-level 3)

;; Flycheck - syntax checker, replaces flymake
(require-package 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Php-mode - PHP support for Emacs
(require-package 'php-mode)

;; Setup the program multi-term will need
(setq multi-term-program "/bin/bash")

;; Enhanced Ruby Mode
(require-package 'enh-ruby-mode)

;; Set it as default mode for Ruby files
(add-to-list 'auto-mode-alist
      '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'"
    . enh-ruby-mode))

;; Inf-ruby mode: Call IRB with C-c C-s in buffers with Ruby modes
(require-package 'inf-ruby)

;; Ruby tools: Goodies for Ruby programming modes
(require-package 'ruby-tools)

;; Magit - Work with Git inside Emacs
(require-package 'magit)

;; Twittering-mode: Use Twitter from within Emacs!
(require-package 'twittering-mode)

;; Org-pomodoro: a Pomodoro timer inside Emacs
(require-package 'org-pomodoro)

;; Yes, I'm committing this heresy
(require-package 'evil)
(evil-mode 1)

;; Load configs
(load "~/my-dotfiles/.emacs.d/evilrc")

;; Emacs Powerline setup: the modeline is an integral part of this program, so why
;; not prettify it? :D

;; Smart-mode-line depends on powerline
(require-package 'powerline)
(require 'powerline)
(require-package 'smart-mode-line)

;; Telephone line setup
(setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
    (accent . (telephone-line-vc-segment
       telephone-line-erc-modified-channels-segment
       telephone-line-process-segment))
    (nil    . (telephone-line-minor-mode-segment
       telephone-line-buffer-segment))))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
    (accent . (telephone-line-major-mode-segment))
    (evil   . (telephone-line-airline-position-segment))))

;; Activate telephone-line
(telephone-line-mode t)

;; Activate smart-mode-line
(setq sml/theme 'powerline)
(sml/setup)

;; EditorConfig - Helps developers define and maintain consistent
;; coding styles between different editors and IDEs
(require-package 'editorconfig)

;; Activate it
(editorconfig-mode 1)

;; YAML mode: work with YAML files
(require-package 'yaml-mode)

;; Web Mode - Use multiple web-related modes for development
(require-package 'web-mode)

;; Web Mode - File associations
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Web Mode - Start impatient mode and httpd server
(defun web-start-impatient-mode ()
    (impatient-mode 1)
    (start-httpd 1))

;; Engine associations
(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
    ("blade"  . "\\.blade\\."))
)

;; Flymake support for PHP files
(require-package 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

;; Company - COMPlete ANYthing inside Emacs
;; I switched to it because it works in GUI Emacs and auto-complete doesn't.
(require-package 'company)

;; Add Tern to Company
(require-package 'company-tern)
(require-package 'tern)

;; Call that inside js2-mode and add tern to company backends
(defun tern-mode-tweaks ()
    (add-to-list 'company-backends 'company-tern)
    (tern-mode 1))
(add-hook 'js2-mode-hook 'tern-mode-tweaks)

;; Eshell extras
(require-package 'eshell-prompt-extras)

;; More configs
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt t
    eshell-prompt-function 'epe-theme-lambda))

;; Make sessions persistent.
;; What's saved:
;; - Histories of user input
;; - Contents of registers
;; - List of recently copied/cut text blocks to paste, global markers to jump
;; to, and other so-called rings.
;; - List of recently changed files with their places and some buffer-local
;; variables.
;; (require-package 'session)

;; Initialize session when loading Emacs.
;; (add-hook 'after-init-hook 'session-initialize)

;; Maybe I can use this together with desktop-save-mode?
(desktop-save-mode 1)

;; Impatient mode - Live edit HTML buffers!
(require-package 'impatient-mode)

;; Yasnippets - it comes with company-mode, but what you also need is some
;; snippets to start with
(require-package 'yasnippet-snippets)

;; Mode-icons - Indicate modes in the mode line using icons
(require-package 'mode-icons)
;; Activate on startup
(mode-icons-mode)

;; Diminish - free some space in the mode line removing superfluous mode
;; indications
(require-package 'diminish)

;; Diminish them!
(diminish 'company-mode)
(diminish 'editorconfig-mode)
(diminish 'autopair-mode)

;; Emojify - add emoji support for Emacs
(require-package 'emojify)

;; Moe-theme - Light and dark theme
(require-package 'moe-theme)
(require 'moe-theme)

;; Keyfreq: shows most used commands in editing session.
;; To see the data, run (keyfreq-show) with M-:
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

;; Theme changer
(require-package 'theme-changer)

;; Set the location
(setq calendar-location-name "Curitiba, PR")
(setq calendar-latitude -25.41)
(setq calendar-longitude -49.25)

;; Specify the day and night themes:
(require 'theme-changer)
(change-theme 'whiteboard 'fairyfloss)

;; Org-bullets: change org-mode's *s to UTF-8 chars
(require-package 'org-bullets)

;; Activate it
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

;; Nyan mode - have a Nyan Cat in your mode-line!
(require-package 'nyan-mode)
;; Activate it
(nyan-mode 1)

;; Autocompletion for Bootstrap/FontAwesome classes
(require-package 'ac-html-bootstrap)

;; CSV mode - edit CSV files
(require-package 'csv-mode)

;; Ruby Solargraph - completion for Ruby modes
;; (require 'solargraph)

;; Solargraph dependency
(require-package 'request)

;; Helm - Emacs incremental completion and selection narrowing framework
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
;; Company-mode web-mode completions
(require-package 'company-web)

;; Add web-mode completions when started
(require 'company-web-html)

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

;; Kill all the buffers matching the provided regex
(global-set-key [24 75] (quote kill-matching-buffers))

;; Switch to last buffer - I do it all the time
(global-set-key [27 112] (quote mode-line-other-buffer))

;; Mapping AltGr-d to delete-other-windows,
;; Another symbol I don't use often.
(global-set-key [240] (quote delete-other-windows))

;; Map magit-status to C-x g
(global-set-key [24 103] (quote magit-status))

;; Access buffers with Alt-Gr b
(global-set-key [8221] (quote ido-switch-buffer))

;; Map the Home and End keys to go to the beginning and end of the buffer
(global-set-key [home] (quote beginning-of-buffer))
(global-set-key [end] (quote end-of-buffer))

;; Open Emacs config file
;; (find-file "~/.emacs" t)

;; Move to beginning of line or indentation
(defun back-to-indentation-or-beginning () (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") (quote back-to-indentation-or-beginning))

;; Hippie-Expand: change key to M-SPC; Replace dabbrev-expand
(global-set-key "\M- " 'hippie-expand)
(global-set-key "\M-/" 'hippie-expand)

;; Cmus configurations: use the media keys with it in GUI Emacs
;; Play/pause button

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

;; Enh-ruby-mode: Run buffer in inf-ruby process
(add-hook 'enh-ruby-mode-hook
  '(lambda ()
     (local-set-key [3 3] (quote ruby-send-buffer))))

;; Python-mode: Send buffer to python shell
(local-set-key [3 2] (quote python-shell-send-buffer))

;; Elisp-mode: Eval-buffer with C-c C-c
(add-hook 'emacs-lisp-mode-hook
  '(lambda ()
     (local-set-key "\C-c \C-c" (quote eval-buffer))))

;; SGML mode (AKA HTML mode) - Open buffer in browser
(add-hook 'sgml-mode-hook
  '(lambda ()
     (local-set-key "\C-c \C-o" (quote browse-url-of-buffer))))

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

;; Python indentation
(setq python-indent 4)

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

;; Adjust update interval in seconds. It's timeR, not time!
(setq twittering-timer-interval 3600)

;; Display icons (if applicable)
(setq twittering-icon-mode t)

;; Use a master password so you don't have to ask for authentication every time
(setq twittering-use-master-password t)

(defun twittering-mode-tweaks()
  ;; Set C-c r in twittering-mode to twittering-reply-to-user
  (local-set-key [3 114] (quote twittering-reply-to-user))
  ;; C-c f: favorite tweet
  (local-set-key [3 102] (quote twittering-favorite))
  ;; C-c n: native retweet
  (local-set-key [3 110] (quote twittering-native-retweet)))

(add-hook 'twittering-mode-hook 'twittering-mode-tweaks)
