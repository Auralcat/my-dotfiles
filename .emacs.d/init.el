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

;; ido-mode in the minibuffer
(icomplete-mode 1)

;; Replace the built-in buffer menu with ibuffer
(global-set-key [24 2] (quote ibuffer))

;; Prevent the scratch buffer from being killed
(with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

;; I always have my init.el open, gonna protect it as well
;;(with-current-buffer "init.el"
;;  (emacs-lock-mode 'kill)))

;; Load the Fairyfloss theme on startup
(load-theme 'fairyfloss)

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
(defun create-scratch-buffer ()
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-mode)
  (insert initial-scratch-message)
  ;; Prevent the scratch buffer from being killed
  (with-current-buffer "*scratch*"
      (emacs-lock-mode 'kill)))

;; I usually keep the terminal window with some transparency to copy stuff from
;; a bxrowser or whatever... this allows me to toggle the transparency from
;; Emacs and saves the theme I had before
;; (defun toggle-terminal-transparency ())

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

;; Pug-mode - Work with .pug files
(require-package 'pug-mode)

;; Php-mode - PHP support for Emacs
(require-package 'php-mode)

;; Setup the program multi-term will need
(setq multi-term-program "/bin/bash")

;; Enhanced Ruby Mode
(require-package 'enh-ruby-mode)
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

;;-----KEYBINDINGS--------------------------------------------------------------
;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1 works too.
(define-key key-translation-map [?\C-h] [?\C-?])

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
  (menu-bar-mode -1))

;; Kill all the buffers matching the provided regex
(global-set-key [24 75] (quote kill-matching-buffers))

;; Switch to last buffer - I do it all the time
(global-set-key [27 112] (quote mode-line-other-buffer))

;; M-x gets the fingers too closed up for my taste.
;; Now I'm testing some combinations, AltGr-v seems cool
;; Nah, I mapped AltGr-x in the end.
(global-set-key [187] (quote execute-extended-command))

;; Mapping other-window to Ctrl-o, I don't need open-line
(global-set-key [15] (quote other-window))

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

;; Hippie-Expand: change key to M-SPC
(global-set-key "\M- " 'hippie-expand)

;;-ORG-MODE---------------------------------------------------------------------
;; Bind org-capture to C-c c
(global-set-key (kbd "\C-c c") (quote org-capture))

;; Bind org-pomodoro to C-c p
(global-set-key (kbd "\C-c p") (quote org-pomodoro))

;; Open the agenda with C-c a
(global-set-key [3 97] (quote org-agenda))

;; Open subheading with C-c RET and invert with M-RET
(local-set-key [27 13] (quote org-ctrl-c-ret))
(local-set-key [3 13] (quote org-insert-subheading))

;; Org-agenda: point the files you want it to read
(setq org-agenda-files (expand-file-name "~/file-bouncer/org-files/contact-based-system/"))

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

;; Set C-c r in twittering-mode to twittering-reply-to-user
(add-hook 'twittering-mode-hook
  (local-set-key [3 114] (quote twittering-reply-to-user)))

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
 '(custom-safe-themes
   (quote
    ("39d212cddd810c2a5b450b32a0acad8ae85b2d534301b2cf86cbc318874b1a5d" "643ce4d69567eedf342aa4913e004dc37e8d4567b226e04bbbd7f08cdd6fb8a3" "c3009cace2d39b02a3c660a68b05ca2796b9a0f623802c1558addf092b0bc91a" "3f3ff89135934379b84e067b8db7386efc63bef5695be0b1765ed95801a9ac2e" "7326e5068f99b8022e2876503284ef8b35c4a6e2f9f5e2cc7e3a147b2af9bc86" "33b2941efadace8f164466cb183194a29fc2562e539bb932235cf015df85b65c" "002be25c63dc642988bcabf86aa1cc3cff72d168017f7b668974f70b76957324" "3a427b2b50f57b2e97b499b5971c2b79a0862f690ddb372292f72b3aa71e8ffb" "0fc7298072540a92cc11274c4e1712d351aabe3b20f1294da8f3a8ab6e5e2e43" "d3286e2f0cee02a049e98e3e2d9c31f944b0d1b34bf1b9d4bcbf401baca174e0" "32fba6a2b0f2e8388a75ed41e70e7d190966ce5c6fe4ef83abd6935ba6d82edb" "24fac42b4ad7f2eabbd134fd1b3aebab2964ad2e5eeb97a36c30bd343c3e3be3" "91d9e12212df3e3dcf313eee7336910166ebc3735268977f921433fcd15dc0e1" "e278b6a543c79c5758f95cc9d712af0724c565196bdd7393b1b0e738b7e4439a" "be4b75ad1770303225aaae89e446c60af40c5848e916ddf98e8089b1713e623e" "9919034690c9e2409c2dccf722e0b2f574d4d356d45681f807dbf1f70bd34825" "345a53dbeaeab1777e5c25b88481aabd37e1573dc015f6c4bff4d785a18daa7c" "62be4d850eb548bddd7cbcb650c088db83e35c058f03c3124153434a39cb2599" "e4384ced8f89c5133cff92bc314becbc219d7c3500c197c8bef7debc251d4676" "6af1c89536ac7be91d8a709923941299e0d8822fa5b32855b39eef36eb8bb7f1" "f32de7d2d8aa2f209397ab036a58c041e92ae40391c6b080e3e1f79ac5374661" "62c625342d7b9228294b52397ffee4afe5992fdf2cc17e7d328cfe054d6734b0" "e4f6561385e45763498650d034c579c2b545168d55c0d9057cc25dd1b8a2c931" "b6cbc6e9a3c0923ed2a8c08709c75ac5ac1665f4d5e44fbc6e14759ff1010ccb" "518b2cf78e56b057a3d4b44ada6c4b4e298fc9866b7c62391da6aa1d8a357292" "cd0d3e633ecc59e31d99d54af5e063def1ff2a6cb08ae71a3ef68ead857b8f74" "4f7a026ab163805b11d48059c9c8237a62beb54c6b2181f1de3656efe8ee616d" "759b9535d645287a170284034f1e399dc0652c12337e72b3d569ff6848667667" "71d9d87d6141d5aaa6a014edf31e142bbfb4866ae990c57645b193903f65d990" "4c8ef842834add984607af2a650fed685d0ef60d7b38c8ea802001236404b80a" "c0e10052057239274f44a31b02c9869815436776e1f34920005555265eb92001" "90b9b16036e9ff0a357c029efd2d9c8b4efc52cd5c3d32c041016992205696bc" "c71114e53758c572d2a854a66dd783c1cbb5036adc5f021f9fc21033e2eec112" "7baed9f018d6518afa7c55e5e6acd83b69db040ac1e0f7bf8cce0b42c5f29f23" "1a379cd3373fbd40a31285ea126d5433ae7c9827c8e881ce1f62c3ca6f7fae5c" "9671fe89034bdf5ea472289ff5b375d8b99c23f92c4dcfecbca613a3ed2c7844" "0b002097186667b1416e4499172007ca9ae8f9b4d80bcfba5b1722e6d9ef9b95" "f5729cac6fcade83c92e50c823eca0e24add9e6c30d1bcafa5076e07909a1ed2" "4134cc495b3e774194861d932a0c094dc729328664b00d82e02c4d31fead14e0" "3856e441793a643f6b2e733b70f3fc2d8e74cf810368eff22942f738d89c4854" "9b803ba5ea6d16e838c3229c7679e8325cbe43083cd4155202925b3f11c3c912" "c9dd6b38801d2db93472aaa63aa15e7c0de4f3f2acecc72b04ca6b3efa25f600" "764b72b168f2a5935f51977c49781c9864312fd9f45e3cc317997d12aa8db3bf" "40af07c028aab91628cac1a41c62f8e4b4e893bef605765196e6b3d8a02ab2e0" "25b0083e7ffd77261165fc391b8a7f94049537a58536da2f0e6fd83ecff30ae8" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" default)))
 '(display-time-mode t)
 '(display-time-world-list
   (quote
    (("Europe/Stockholm" "Stockholm")
     ("America/Sao_Paulo" "São Paulo (local time)")
     ("America/New_York" "Boston")
     ("Europe/London" "London"))))
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
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-file-name "   " mode-line-position evil-mode-line-tag
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(org-agenda-files
   (quote
    ("~/file-bouncer/org-files/contact-based-system/techladies.org" "~/file-bouncer/org-files/contact-based-system/workshop-vim.org" "~/file-bouncer/org-files/contact-based-system/pitt.org" "~/file-bouncer/org-files/contact-based-system/kira.org" "~/file-bouncer/org-files/contact-based-system/helioloureiro.org" "/home/lucas/file-bouncer/org-files/contact-based-system/tatiane-alves.org" "~/file-bouncer/org-files/contact-based-system/elaine.org" "~/file-bouncer/org-files/contact-based-system/eu.org" "/home/lucas/file-bouncer/org-files/contact-based-system/rosiane.org" "/home/lucas/file-bouncer/org-files/contact-based-system/vanessa.org")))
 '(org-capture-templates
   (quote
    (("s" "Task to self" entry
      (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas")
      "** TODO %i %?" :prepend t)
     ("t" "Task" entry
      (file "~/file-bouncer/org-files/capture-tasks.org")
      "* TODO %? %i")
     ("n" "File your random thoughts here" entry
      (file "~/file-bouncer/org-files/capture-notes.org")
      "* %U -> %?"))))
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
 '(org-pomodoro-audio-player "/usr/bin/mpv")
 '(org-pomodoro-finished-sound
   "/home/lucas/.emacs.d/elpa/org-pomodoro-2.1.0/resources/Blip.ogg")
 '(org-pomodoro-format "Focus!~%s")
 '(org-pomodoro-start-sound-p nil)
 '(org-todo-keywords (quote ((sequence "TODO" "DONE"))))
 '(remember-data-file "~/file-bouncer/everything-bucket")
 '(shell-file-name "/bin/bash")
 '(tags-tag-face (quote default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-upcoming-deadline ((t (:foreground "color-91")))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
