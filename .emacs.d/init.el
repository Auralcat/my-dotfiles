;; Emacs configuration file
;; Author: Auralcat
;; Started in May 2017.

;;-----ENV-CUSTOMIZATIONS-------------------------------------------------------

;; Store all backups in a specific folder:
(setq backup-directory-alist `(("." . "~/file-bouncer/emacs-backups")))

;; Manual packages load path
(add-to-list 'load-path "~/my-dotfiles/.emacs.d/manual-packages/ob-elixir/")

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

;; Enable Abbrev mode
(abbrev-mode 1)

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

;; Create filling for org-mode
(add-hook 'org-mode-hook 'auto-fill-mode)
;;-----IRC----------------------------------------------------------------------

;; Qui Nov  2 19:57:06 BRST 2017 - Tried using IRC inside Emacs, didn't please
;; me, too many buffers to work with... for now.

;;-----CUSTOM-FUNCTIONS---------------------------------------------------------

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
(defun css-live-reload-current-webpage ()
    (when (and (stringp buffer-file-name)
      (string-match "\\.scss\\'" buffer-file-name))
    (shell-command "~/my-dotfiles/bash/live-reload-firefox.sh")))

;; The original idea is to call this function when saving a stylesheet
(add-hook 'after-save-hook 'css-live-reload-current-webpage)
;;-----FUNCTION-ALIASES---------------------------------------------------------

;; This is how you define aliases for Elisp functions
(defalias 'plp 'package-list-packages)

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

;; ...of this, and...
(require-package 'emmet-mode)

;; ...this!
(require-package 'sass-mode)

;; Set Sass mode for SCSS files
(add-to-list 'auto-mode-alist
      '("\\.scss\\'" . sass-mode))

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

;;-----GRAPHICAL----------------------------------------------------------------

;; Set font in graphical mode
(when (display-graphic-p)
    ;; Use Fantasque Sans Mono when available
    (if (member "Fantasque Sans Mono" (font-family-list))
        (set-frame-font "Fantasque Sans Mono 12")
        '(set-frame-font "Ubuntu Mono 12" nil t))
    ;; Remove menu and scroll bars in graphical mode
    (menu-bar-mode 0)
    (scroll-bar-mode 0)
    ;; Enable emoji images
    (global-emojify-mode)
    ;; Maximize frame on startup
    (toggle-frame-maximized))

;; Company-mode web-mode completions
(require-package 'company-web)

;; Add web-mode completions when started
(require 'company-web-html)

;;-----IRC----------------------------------------------------------------------
;; Use Weechat from Emacs
(require-package 'weechat)

;; Bind M-p to switch to previous buffer

;; -----KEYBINDINGS--------------------------------------------------------------
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

;; M-x gets the fingers too closed up for my taste.
;; Now I'm testing some combinations, AltGr-v seems cool
;; Nah, I mapped AltGr-x in the end.
(global-set-key [187] (quote execute-extended-command))

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

;; Nyan mode - have a Nyan Cat in your mode-line!
(require-package 'nyan-mode)
;; Activate it
(nyan-mode 1)

;; Theme-changer - switch between themes for day and night
(require-package 'theme-changer)

;; Set the location name, latitude and longitude
(setq calendar-location-name "Curitiba, PR")
(setq calendar-latitude -25.41)
(setq calendar-longitude -49.25)

;; Set themes to change at sunrise/sunset
(change-theme 'fairyfloss 'deeper-blue)

;; Autocompletion for Bootstrap/FontAwesome classes
(require-package 'ac-html-bootstrap)

;;-WEB-MODE---------------------------------------------------------------------
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

;;-ORG-MODE---------------------------------------------------------------------

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
;;------------------------------------------------------------------------------
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

;;-----VARIABLES----------------------------------------------------------------

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

;;-MACROS-----------------------------------------------------------------------

;; To save a macro, record it with C-x ( (start) and C-x ) (stop),
;; give it a name with C-x C-k n (C-k is for maKro) and
;; insert it in this file with insert-kbd-macro.
;; Then you execute it calling the function or mapping it to a key! 😊


;; Example macro
(fset 'kilroy-was-here
      (lambda (&optional arg)
    "Keyboard macro."
    (interactive "p")
    (kmacro-exec-ring-item
     (quote
      (" -- foobar was hereKilroy wsaas here" 0 "%d")) arg)))


;;-TWITTERING-MODE--------------------------------------------------------------
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
;;-----CUSTOMIZATION-THROUGH-M-x-CUSTOMIZE--------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(ansi-color-faces-vector
         [default default default italic underline success warning error])
    '(ansi-color-names-vector
         ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-image-file-mode t)
 '(browse-url-browser-display nil)
 '(browse-url-firefox-program "firefox")
 '(company-auto-complete (quote (quote company-explicit-action-p)))
    '(company-backends
         (quote
             (company-web-html company-nxml company-css company-eclim company-semantic company-clang company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.3)
 '(company-minimum-prefix-length 3)
    '(custom-safe-themes
         (quote
             ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "f8b5f1c5ab70da141ae308b9ce23ebc2b713dd3a22a6ebb08bfb55f5c7a11287" "5908457b14343ddca0ff1efa27247fb8eec94bc1eaab60fe58c1d033a3188315" "d857acdacdb74d5a3eb35c1d009d0c598f9954d51da523859db6366479cc31cd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36d92f830c21797ce34896a4cf074ce25dbe0dabe77603876d1b42316530c99d" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "59e5301cce088c4a493f1745b0d409eff6ba955f06150f77ffaf39d955bb8442" "da3f9bcccf44c8b3bb64807a4eb55a5b2089488ad28201a968333dee29b10d89" "39d212cddd810c2a5b450b32a0acad8ae85b2d534301b2cf86cbc318874b1a5d" "643ce4d69567eedf342aa4913e004dc37e8d4567b226e04bbbd7f08cdd6fb8a3" "c3009cace2d39b02a3c660a68b05ca2796b9a0f623802c1558addf092b0bc91a" "3f3ff89135934379b84e067b8db7386efc63bef5695be0b1765ed95801a9ac2e" "7326e5068f99b8022e2876503284ef8b35c4a6e2f9f5e2cc7e3a147b2af9bc86" "33b2941efadace8f164466cb183194a29fc2562e539bb932235cf015df85b65c" "002be25c63dc642988bcabf86aa1cc3cff72d168017f7b668974f70b76957324" "3a427b2b50f57b2e97b499b5971c2b79a0862f690ddb372292f72b3aa71e8ffb" "0fc7298072540a92cc11274c4e1712d351aabe3b20f1294da8f3a8ab6e5e2e43" "d3286e2f0cee02a049e98e3e2d9c31f944b0d1b34bf1b9d4bcbf401baca174e0" "32fba6a2b0f2e8388a75ed41e70e7d190966ce5c6fe4ef83abd6935ba6d82edb" "24fac42b4ad7f2eabbd134fd1b3aebab2964ad2e5eeb97a36c30bd343c3e3be3" "91d9e12212df3e3dcf313eee7336910166ebc3735268977f921433fcd15dc0e1" "e278b6a543c79c5758f95cc9d712af0724c565196bdd7393b1b0e738b7e4439a" "be4b75ad1770303225aaae89e446c60af40c5848e916ddf98e8089b1713e623e" "9919034690c9e2409c2dccf722e0b2f574d4d356d45681f807dbf1f70bd34825" "345a53dbeaeab1777e5c25b88481aabd37e1573dc015f6c4bff4d785a18daa7c" "62be4d850eb548bddd7cbcb650c088db83e35c058f03c3124153434a39cb2599" "e4384ced8f89c5133cff92bc314becbc219d7c3500c197c8bef7debc251d4676" "6af1c89536ac7be91d8a709923941299e0d8822fa5b32855b39eef36eb8bb7f1" "f32de7d2d8aa2f209397ab036a58c041e92ae40391c6b080e3e1f79ac5374661" "62c625342d7b9228294b52397ffee4afe5992fdf2cc17e7d328cfe054d6734b0" "e4f6561385e45763498650d034c579c2b545168d55c0d9057cc25dd1b8a2c931" "b6cbc6e9a3c0923ed2a8c08709c75ac5ac1665f4d5e44fbc6e14759ff1010ccb" "518b2cf78e56b057a3d4b44ada6c4b4e298fc9866b7c62391da6aa1d8a357292" "cd0d3e633ecc59e31d99d54af5e063def1ff2a6cb08ae71a3ef68ead857b8f74" "4f7a026ab163805b11d48059c9c8237a62beb54c6b2181f1de3656efe8ee616d" "759b9535d645287a170284034f1e399dc0652c12337e72b3d569ff6848667667" "71d9d87d6141d5aaa6a014edf31e142bbfb4866ae990c57645b193903f65d990" "4c8ef842834add984607af2a650fed685d0ef60d7b38c8ea802001236404b80a" "c0e10052057239274f44a31b02c9869815436776e1f34920005555265eb92001" "90b9b16036e9ff0a357c029efd2d9c8b4efc52cd5c3d32c041016992205696bc" "c71114e53758c572d2a854a66dd783c1cbb5036adc5f021f9fc21033e2eec112" "7baed9f018d6518afa7c55e5e6acd83b69db040ac1e0f7bf8cce0b42c5f29f23" "1a379cd3373fbd40a31285ea126d5433ae7c9827c8e881ce1f62c3ca6f7fae5c" "9671fe89034bdf5ea472289ff5b375d8b99c23f92c4dcfecbca613a3ed2c7844" "0b002097186667b1416e4499172007ca9ae8f9b4d80bcfba5b1722e6d9ef9b95" "f5729cac6fcade83c92e50c823eca0e24add9e6c30d1bcafa5076e07909a1ed2" "4134cc495b3e774194861d932a0c094dc729328664b00d82e02c4d31fead14e0" "3856e441793a643f6b2e733b70f3fc2d8e74cf810368eff22942f738d89c4854" "9b803ba5ea6d16e838c3229c7679e8325cbe43083cd4155202925b3f11c3c912" "c9dd6b38801d2db93472aaa63aa15e7c0de4f3f2acecc72b04ca6b3efa25f600" "764b72b168f2a5935f51977c49781c9864312fd9f45e3cc317997d12aa8db3bf" "40af07c028aab91628cac1a41c62f8e4b4e893bef605765196e6b3d8a02ab2e0" "25b0083e7ffd77261165fc391b8a7f94049537a58536da2f0e6fd83ecff30ae8" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" default)))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-mail-face (quote change-log-email))
 '(display-time-mail-string "✉")
 '(display-time-mode t)
    '(display-time-world-list
         (quote
             (("Europe/Stockholm" "TalonRO Server Time")
                 ("Europe/Stockholm" "Stockholm")
                 ("America/Sao_Paulo" "São Paulo (local time)")
                 ("America/New_York" "Boston")
                 ("Europe/London" "London"))))
 '(dynamic-completion-mode t)
 '(emmet-indent-after-insert t)
 '(emmet-indentation 4)
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
 '(evil-shift-width 2)
 '(font-use-system-font t)
 '(global-auto-revert-mode t)
 '(global-emojify-mode t)
 '(global-linum-mode nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote org-mode))
    '(initial-scratch-message
         "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(keyboard-coding-system (quote utf-8-unix))
    '(mode-icons
         (quote
             (("\\`CSS\\'" "css" xpm)
                 ("\\`Coffee\\'" "coffee" xpm-bw)
                 ("\\`Compilation\\'" "compile" xpm)
                 ("\\`Emacs-Lisp\\'" "emacs" xpm)
                 ("\\`Lisp Interaction\\'" "emacs" xpm)
                 ("\\`HTML\\'" "html" xpm)
                 ("\\`Haml\\'" "haml" xpm)
                 ("\\`Image\\[imagemagick\\]\\'" "svg" xpm)
                 ("\\`Inf-Ruby\\'" "infruby" xpm)
                 ("\\`Java[Ss]cript\\'" "js" xpm)
                 ("\\`Lisp\\'" "cl" xpm)
                 ("\\`nXML\\'" "xml" xpm)
                 ("\\`Org\\'" "org" xpm)
                 ("\\`PHP\\(\\|/.*\\)\\'" "php" xpm)
                 ("\\`Projectile Rails Server\\'" "rails" xpm)
                 ("\\`Python\\'" "python" xpm)
                 ("\\`(Enh)?Ruby\\'" "ruby" xpm)
                 ("\\`ESS\\[S\\]\\'" "R" xpm)
                 ("\\`ESS\\[SAS\\]\\'" "sas" xpm)
                 ("\\`ESS\\[BUGS\\]\\'" 61832 FontAwesome)
                 ("\\`iESS\\'" "R" xpm)
                 ("\\`SCSS\\'" "sass" xpm)
                 ("\\`Sass\\'" "sass" xpm)
                 ("\\`Scheme" "scheme" xpm-bw)
                 ("\\`Shell-script" "bash" xpm-bw)
                 ("\\`Slim" "slim" xpm-bw)
                 ("\\`Snippet" "yas" xpm)
                 ("\\`Term\\'" "term" xpm)
                 ("\\`Web\\'" "html" xpm)
                 ("\\`XML\\'" "xml" xpm)
                 ("\\`YAML\\'" "yaml" xpm)
                 ("\\` ?YASnippet\\'" "yas" xpm)
                 ("\\` ?yas\\'" "yas" xpm)
                 ("\\` ?hs\\'" "hs" xpm)
                 ("\\`Markdown\\'" 61641 github-octicons)
                 ("\\`Scala\\'" 61787 font-mfizz)
                 ("\\`Magit\\'" 61906 FontAwesome)
                 ("\\` Pulls\\'" 61586 FontAwesome)
                 ("\\`Zip-Archive\\'" 61894 FontAwesome)
                 ("\\` ARev\\'" 61473 FontAwesome)
                 ("\\`Calc\\(ulator\\)?\\'" 61932 FontAwesome)
                 ("\\`Debug.*\\'" 61832 FontAwesome)
                 ("\\`Debug.*\\'" 61832 FontAwesome)
                 ("\\`Calendar\\'" 61555 FontAwesome)
                 ("\\`Help\\'" 61529 FontAwesome)
                 ("\\`WoMan\\'" 61530 FontAwesome)
                 ("\\`C\\(/.*\\|\\)\\'" 61703 font-mfizz)
                 ("\\`Custom\\'" 61459 FontAwesome)
                 ("\\`Go\\'" "go" xpm)
                 ("\\` ?Rbow\\'" "rainbow" xpm)
                 ("\\` ?ICY\\'" "icy" xpm)
                 ("\\` ?Golden\\'" "golden" xpm-bw)
                 ("\\`BibTeX\\'\\'" "bibtex" xpm-bw)
                 ("\\`C[+][+]\\(/.*\\|\\)\\'" 61708 font-mfizz)
                 ("\\`C[#]\\(/.*\\|\\)\\'" 61709 font-mfizz)
                 ("\\`Elixir\\'" 61717 font-mfizz)
                 ("\\`Erlang\\'" 61718 font-mfizz)
                 ("\\`Haskell\\'" 61734 font-mfizz)
                 ("\\`Clojure\\'" 61706 font-mfizz)
                 ("\\`Java\\(/.*\\|\\)\\'" 61739 font-mfizz)
                 ("\\`C?Perl\\'" 61768 font-mfizz)
                 ("\\`Octave\\'" "octave" xpm)
                 ("\\`AHK\\'" "autohotkey" xpm)
                 ("\\`Info\\'" 61530 FontAwesome)
                 ("\\` ?Narrow\\'" 61542 FontAwesome)
                 ("\\`Dockerfile\\'" "docker" xpm)
                 (read-only 61475 FontAwesome)
                 (writable 61596 FontAwesome)
                 (save 61639 FontAwesome)
                 (saved "" nil)
                 (modified-outside 61553 FontAwesome)
                 (steal 61979 FontAwesome)
                 (apple 60095 IcoMoon-Free)
                 (apple 61817 FontAwesome)
                 (win 61818 FontAwesome)
                 (unix 60093 IcoMoon-Free)
                 (unix 61798 font-mfizz)
                 (unix 61820 FontAwesome)
                 (undecided 61736 FontAwesome)
                 ("Text\\'" 61686 FontAwesome)
                 ("\\` ?company\\'" 61869 FontAwesome)
                 ("\\` ?AC\\'" 61838 FontAwesome)
                 ("\\` ?Fly\\'" 59922 IcoMoon-Free)
                 ("\\` ?Ergo" 61724 FontAwesome)
                 ("\\` ?drag\\'" 61511 FontAwesome)
                 ("\\` ?Helm\\'" "helm" xpm-bw)
                 ("\\`Messages\\'" 62075 FontAwesome)
                 ("\\`Conf" 61918 FontAwesome)
                 ("\\`Fundamental\\'" 61462 FontAwesome)
                 ("\\`Javascript-IDE\\'" "js" xpm)
                 ("\\` Undo-Tree\\'" ":palm_tree:" emoji)
                 ("\\`LaTeX\\'" "tex" ext)
                 ("\\`Image\\[xpm\\]\\'" "xpm" ext)
                 ("\\`Image\\[png\\]\\'" "png" ext)
                 ("\\` ?AI\\'" 61500 FontAwesome)
                 ("\\`twittering-mode\\'" "twitter" xpm)
                 ("\\` Emmet\\'" "emmet" xpm)
                 ("\\` ?\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil))))
 '(mode-icons-change-mode-name t)
 '(mode-icons-generate-emoji-xpms t)
 '(mode-icons-generate-font-xpms t)
 '(mode-icons-mode t)
    '(mode-line-format
         (quote
             ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-file-name "   " mode-line-position evil-mode-line-tag
                 (vc-mode vc-mode)
                 "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.2)
 '(nyan-bar-length 16)
 '(nyan-mode t)
    '(org-agenda-files
         (quote
             ("~/file-bouncer/org-files/stand-up-meetings/Maio-2018.org" "/home/lucas/file-bouncer/org-files/contact-based-system/eu.org" "~/file-bouncer/org-files/blog.org" "/home/lucas/file-bouncer/org-files/contact-based-system/rosiane.org" "/home/lucas/file-bouncer/org-files/contact-based-system/elaine.org" "/home/lucas/file-bouncer/org-files/contact-based-system/aline-pegas.org" "~/file-bouncer/org-files/contact-based-system/ariane.org" "~/file-bouncer/org-files/site-congresso.org")))
 '(org-agenda-scheduled-leaders (quote ("Scheduled: " "Sched. previously %2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
    '(org-capture-templates
         (quote
             (("t" "Tarefa extra do stand-up (pra não colocar direto na lista do dia, organizar depois)" entry
                  (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas extras do stand-up (organizar depois)")
                  "** TODO %?" :prepend t)
                 ("k" "Testing regex target capturing" entry
                     (file+regexp "~/file-bouncer/org-files/stand-up-meetings/Abril-2018.org" "\\* CURRENT")
                     "*** TODO %?")
                 ("s" "Task to Self" entry
                     (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas capturadas")
                     "** TODO %?" :prepend t))))
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
    '(org-effort-durations
         (quote
             (("h" . 60)
                 ("d" . 480)
                 ("w" . 2400)
                 ("m" . 9600)
                 ("y" . 96000)
                 ("pomodoros" . 30))))
 '(org-plantuml-jar-path "~/file-bouncer/plantuml.jar")
 '(org-pomodoro-audio-player "/usr/bin/mpv")
    '(org-pomodoro-finished-sound
         "/home/lucas/.emacs.d/elpa/org-pomodoro-2.1.0/resources/Blip.ogg")
 '(org-pomodoro-format "Focus!~%s")
 '(org-pomodoro-start-sound-p nil)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-src-fontify-natively t)
    '(org-stuck-projects
         (quote
             ("+LEVEL=2/-DONE"
                 ("TODO" "NEXT" "NEXTACTION" "CANCELLED")
                 nil "")))
 '(org-todo-keyword-faces (quote (("CURRENT" . "#ffcc11") ("NEXT" . "#6666ff"))))
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(reb-re-syntax (quote string))
 '(remember-data-file "~/file-bouncer/everything-bucket")
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-file-name "/bin/bash")
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25)
 '(tags-tag-face (quote default))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(weechat-auto-monitor-buffers t)
    '(weechat-modules
         (quote
             (weechat-button weechat-complete weechat-notifications weechat-image)))
 '(weechat-notification-mode t)
 '(weechat-notifications-icon "~/Imagens/gata.jpg")
 '(weechat-notifications-sound nil)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 4096)) (:foreground "#5f5f5f" :background "#fdfde7")) (((class color) (min-colors 256)) (:foreground "#5f5f5f" :background "#fdfde7")) (((class color) (min-colors 89)) (:foreground "#5f5f5f" :background "#fdfde7"))))
 '(company-scrollbar-bg ((((class color) (min-colors 89)) (:background "#5f5f5f"))))
 '(company-scrollbar-fg ((((class color) (min-colors 89)) (:background "#9e9e9e"))))
 '(company-template-field ((((class color) (min-colors 89)) (:background "#ffffaf" :foreground "#626262"))))
 '(company-tooltip ((((class color) (min-colors 89)) (:background "#3a3a3a" :foreground "#5fafd7"))))
 '(company-tooltip-common ((((class color) (min-colors 89)) (:background "#5f5f5f" :foreground "#5fafd7"))))
 '(company-tooltip-selection ((((class color) (min-colors 89)) (:background "#626262" :foreground "#afd7ff"))))
 '(org-scheduled-previously ((t (:foreground "#778855"))))
 '(org-upcoming-deadline ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(region ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#5fafd7"))))
 '(sml/filename ((t (:inherit sml/global :background "Grey22" :foreground "Black"))))
 '(sml/folder ((t (:inherit sml/global :background "Grey22" :foreground "Black" :weight normal))))
 '(sml/modes ((t (:inherit sml/global :background "grey40" :foreground "Black" :height 1.0))))
 '(sml/time ((t (:inherit sml/global :background "black" :foreground "green" :height 1.05 :foundry "ALTS" :family "Digital"))))
 '(time-mail-face ((t (:family "IBM 3270"))) t))
(put 'upcase-region 'disabled nil)
