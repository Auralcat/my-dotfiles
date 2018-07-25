(require 'package)
(setq package-enable-at-startup nil)

;; Use this for debugging
;; (setq debug-on-error t)

;; Package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("marmalade" . "https://marmalade-repo.org/packages/")
     ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")
     ;; Use it when you can't find what you want in other repos
     ("melpa" . "https://melpa.org/packages/")
                            ))
(package-initialize)

;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; This replaces the old require-package.
;; If package isn't installed, fetch it.
(setq use-package-always-ensure t)

;; Use-package
(require-package 'use-package)
(eval-when-compile
    (require 'use-package))

;; Load the Org file containing the customizations!
(org-babel-load-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.org"))

;; Created through M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
    '(ansi-color-names-vector
         ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
    '(ansi-term-color-vector
         [unspecified "#1F1611" "#660000" "#144212" "#EFC232" "#5798AE" "#BE73FD" "#93C1BC" "#E6E1DC"] t)
 '(auto-image-file-mode t)
 '(battery-mode-line-format " 🔋 %b%p%% ")
 '(browse-url-browser-display nil)
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(browse-url-firefox-program "firefox")
    '(camcorder-frame-parameters
         (quote
             ((name . "camcorder.el Recording - F12 to Stop - F11 to Pause/Resume")
                 (height . 240)
                 (width . 320)
                 (top . 80))))
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete t)
 '(company-auto-complete-chars (quote (41)))
    '(company-backends
         (quote
             (company-web-html company-nxml company-css company-eclim company-semantic company-clang company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-prescient-mode t)
 '(company-transformers (quote (company-sort-by-backend-importance)))
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (fairyfloss)))
    '(custom-safe-themes
         (quote
             ("26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "551596f9165514c617c99ad6ce13196d6e7caa7035cea92a0e143dbe7b28be0e" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "ab0950f92dc5e6b667276888cb0cdbc35fd1c16f667170a62c15bd3ed5ae5c5a" "d3a7eea7ebc9a82b42c47e49517f7a1454116487f6907cf2f5c2df4b09b50fc1" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "234455c4073e7007f7f0a0a7e74dd03f8495b09540f467993c5cb847cfb600e1" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02" "6ffef0161169e444b514a0f7f0cb7eac09d11c396cdc99bf85360a361c427886" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd2346baba899fa7eee2bba4936cfcdf30ca55cdc2df0a1a4c9808320c4d4b22" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "ea63cede1b158a5d1bda96d46b3d84ab72600282cf8b4c2f77383983c225fe2c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "aad2caf3759b343577768009f89d1a4cbfcea2c4f49331be753c397532250dae" default)))
 '(display-battery-mode t)
 '(display-default-load-average nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-format " 📅 %a, %d %b 🕙 %R ")
 '(display-time-mail-face (quote change-log-email))
 '(display-time-mail-string "✉")
 '(display-time-mode nil)
    '(display-time-world-list
         (quote
             (("Europe/Stockholm" "TalonRO Server Time")
                 ("Europe/Stockholm" "Stockholm")
                 ("America/Sao_Paulo" "São Paulo (local time)")
                 ("America/New_York" "Boston")
                 ("Europe/London" "London")
                 ("Asia/Tokyo" "Tokyo"))))
 '(dynamic-completion-mode t)
 '(emmet-indent-after-insert t)
 '(emmet-indentation 2)
 '(emmet-preview-default t)
 '(enh-ruby-use-encoding-map nil)
    '(ensime-sem-high-faces
         (quote
             ((var :foreground "#000000" :underline
                  (:style wave :color "yellow"))
                 (val :foreground "#000000")
                 (varField :foreground "#600e7a" :slant italic)
                 (valField :foreground "#600e7a" :slant italic)
                 (functionCall :foreground "#000000" :slant italic)
                 (implicitConversion :underline
                     (:color "#c0c0c0"))
                 (implicitParams :underline
                     (:color "#c0c0c0"))
                 (operator :foreground "#000080")
                 (param :foreground "#000000")
                 (class :foreground "#20999d")
                 (trait :foreground "#20999d" :slant italic)
                 (object :foreground "#5974ab" :slant italic)
                 (package :foreground "#000000")
                 (deprecated :strike-through "#000000"))))
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
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#073642")
 '(font-use-system-font nil)
 '(global-anzu-mode t)
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-emojify-mode t)
 '(global-linum-mode nil)
 '(global-writeroom-mode nil nil (writeroom-mode))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
    '(highlight-symbol-colors
         (--map
             (solarized-color-blend it "#002b36" 0.25)
             (quote
                 ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
    '(highlight-tail-colors
         (quote
             (("#073642" . 0)
                 ("#546E00" . 20)
                 ("#00736F" . 30)
                 ("#00629D" . 50)
                 ("#7B6000" . 60)
                 ("#8B2C02" . 70)
                 ("#93115C" . 85)
                 ("#073642" . 100))))
    '(hl-bg-colors
         (quote
             ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
    '(hl-fg-colors
         (quote
             ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ido-enable-flex-matching nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote org-mode))
    '(initial-scratch-message
         "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(keyboard-coding-system (quote utf-8-unix))
 '(line-number-mode nil)
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
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
                 ("\\`Ruby\\'" "ruby" xpm)
                 ("\\`EnhRuby\\'" "ruby" xpm)
                 ("\\`ESS\\[S\\]\\'" "R" xpm)
                 ("\\`ESS\\[SAS\\]\\'" "sas" xpm)
                 ("\\`ESS\\[BUGS\\]\\'" 61832 FontAwesome)
                 ("\\`iESS\\'" "R" xpm)
                 ("\\`Rust\\'" "rust" xpm)
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
                 ("\\`Vue\\'" "vue" xpm)
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
    '(nrepl-message-colors
         (quote
             ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.2)
 '(nyan-bar-length 16)
 '(nyan-mode t)
    '(org-agenda-files
         (quote
             ("~/file-bouncer/org-files/site-proprio.org" "~/file-bouncer/org-files/stand-up-meetings/Julho-2018.org" "~/file-bouncer/org-files/contact-based-system/eu.org" "~/file-bouncer/org-files/contact-based-system/aline-pegas.org" "~/file-bouncer/org-files/contact-based-system/kira.org" "~/file-bouncer/org-files/contact-based-system/elaine.org")))
 '(org-agenda-scheduled-leaders (quote ("Scheduled: " "Sched. previously %2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday 0)
 '(org-bullets-bullet-list (quote ("✿" "❀" "◉" "○" "✸")))
    '(org-capture-templates
         (quote
             (("n" "Note for the current day entry" plain
                  (file+headline "~/file-bouncer/org-files/stand-up-meetings/Maio-2018.org" "Anotações")
                  "  - %U %a%?" :prepend t)
                 ("t" "Tarefa extra do stand-up (pra não colocar direto na lista do dia, organizar depois)" entry
                     (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas extras do stand-up (organizar depois)")
                     "** TODO %?" :prepend t)
                 ("k" "Testing regex target capturing" entry
                     (file+regexp "~/file-bouncer/org-files/stand-up-meetings/Abril-2018.org" "\\* CURRENT")
                     "*** TODO %?")
                 ("s" "Task to Self" entry
                     (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas capturadas")
                     "** TODO %?" :prepend t))))
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
 '(org-plantuml-jar-path "~/file-bouncer/plantuml.jar")
 '(org-pomodoro-audio-player "/usr/bin/mpv")
    '(org-pomodoro-finished-sound
         "/home/lucas/.emacs.d/pomodoro-sounds/Librem-5-power-on-by-Antonio-Paternina-Alvarez.ogg")
 '(org-pomodoro-finished-sound-p t)
 '(org-pomodoro-format "Focus!~%s")
    '(org-pomodoro-long-break-sound
         "/home/lucas/.emacs.d/pomodoro-sounds/Librem-5-phone-call-3-by-Nohumanconcept.ogg")
 '(org-pomodoro-long-break-sound-p t)
    '(org-pomodoro-short-break-sound
         "/home/lucas/.emacs.d/pomodoro-sounds/Librem-5-email-notification-2-by-Pablo-Somonte.ogg")
 '(org-pomodoro-short-break-sound-p t)
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
    '(package-selected-packages
         (quote
             (circadian diminish alchemist sublimity writeroom-mode electric-operator jazz-theme birds-of-paradise-plus-theme highlight-indentation smartparens company-tern restart-emacs ob-restclient doom-themes intellij-theme github-modern-theme restclient helm-projectile ibuffer-projectile yasnippet-classic-snippets yasnippet-snippets vagrant-tramp ob-elixir company-prescient weechat web-mode web-beautify vagrant use-package twittering-mode solarized-theme scss-mode sass-mode rvm rust-mode ruby-tools robe rich-minority request rainbow-mode rainbow-delimiters projectile powerline org-pomodoro org-bullets nyan-mode moe-theme mode-icons memoize makey magit keyfreq js2-mode htmlize highlight-sexp highlight-numbers helm-tramp flymake-phpcs flymake-php flymake-elixir flycheck-mix evil-surround evil-leader evil-anzu eshell-prompt-extras eruby-mode enh-ruby-mode engine-mode emojify emmet-mode electric-spacing editorconfig csv-mode company-web company-statistics company-php cl-generic camcorder autopair ace-jump-mode ac-html-bootstrap abyss-theme)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(persp-show-modestring (quote (quote header)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(powerline-buffer-size-suffix t)
 '(powerline-default-separator (quote arrow-fade))
 '(powerline-display-buffer-size nil)
 '(powerline-gui-use-vcs-glyph t)
 '(projectile-mode t nil (projectile))
    '(projectile-mode-line
         (quote
             (:eval
                 (if
                     (file-remote-p default-directory)
                     " Projectile"
                     (format " [%s] "
                         (projectile-project-name))))))
 '(reb-re-syntax (quote string))
 '(remember-data-file "~/file-bouncer/everything-bucket")
 '(save-place-mode t nil (saveplace))
 '(scss-compile-at-save nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
    '(sml/mode-width
         (if
             (eq
                 (powerline-current-separator)
                 (quote arrow))
             (quote right)
             (quote full)))
    '(sml/pos-id-separator
         (quote
             (""
                 (:propertize " " face powerline-active1)
                 (:eval
                     (propertize " "
                         (quote display)
                         (funcall
                             (intern
                                 (format "powerline-%s-%s"
                                     (powerline-current-separator)
                                     (car powerline-default-separator-dir)))
                             (quote powerline-active1)
                             (quote powerline-active2))))
                 (:propertize " " face powerline-active2))))
    '(sml/pos-minor-modes-separator
         (quote
             (""
                 (:propertize " " face powerline-active1)
                 (:eval
                     (propertize " "
                         (quote display)
                         (funcall
                             (intern
                                 (format "powerline-%s-%s"
                                     (powerline-current-separator)
                                     (cdr powerline-default-separator-dir)))
                             (quote powerline-active1)
                             (quote sml/global))))
                 (:propertize " " face sml/global))))
    '(sml/pre-id-separator
         (quote
             (""
                 (:propertize " " face sml/global)
                 (:eval
                     (propertize " "
                         (quote display)
                         (funcall
                             (intern
                                 (format "powerline-%s-%s"
                                     (powerline-current-separator)
                                     (car powerline-default-separator-dir)))
                             (quote sml/global)
                             (quote powerline-active1))))
                 (:propertize " " face powerline-active1))))
    '(sml/pre-minor-modes-separator
         (quote
             (""
                 (:propertize " " face powerline-active2)
                 (:eval
                     (propertize " "
                         (quote display)
                         (funcall
                             (intern
                                 (format "powerline-%s-%s"
                                     (powerline-current-separator)
                                     (cdr powerline-default-separator-dir)))
                             (quote powerline-active2)
                             (quote powerline-active1))))
                 (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25)
 '(sublimity-mode nil)
 '(tags-tag-face (quote default))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(type-break-mode nil)
 '(vc-annotate-background nil)
    '(vc-annotate-color-map
         (quote
             ((20 . "#dc322f")
                 (40 . "#c85d17")
                 (60 . "#be730b")
                 (80 . "#b58900")
                 (100 . "#a58e00")
                 (120 . "#9d9100")
                 (140 . "#959300")
                 (160 . "#8d9600")
                 (180 . "#859900")
                 (200 . "#669b32")
                 (220 . "#579d4c")
                 (240 . "#489e65")
                 (260 . "#399f7e")
                 (280 . "#2aa198")
                 (300 . "#2898af")
                 (320 . "#2793ba")
                 (340 . "#268fc6")
                 (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding nil)
 '(weechat-auto-monitor-buffers t)
    '(weechat-color-list
         (quote
             (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
    '(weechat-modules
         (quote
             (weechat-button weechat-complete weechat-notifications weechat-image)))
 '(weechat-notification-mode t)
 '(weechat-notifications-icon "~/Imagens/gata.jpg")
 '(weechat-notifications-sound nil)
 '(writeroom-width 1.0)
 '(yas-global-mode t)
    '(yas-snippet-dirs
         (quote
             ("/home/lucas/.emacs.d/snippets" yasnippet-snippets-dir yasnippet-classic-snippets-dir))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Fantasque Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 120 :width normal))))
 '(company-scrollbar-bg ((((class color) (min-colors 89)) (:background "#5f5f5f"))))
 '(company-scrollbar-fg ((((class color) (min-colors 89)) (:background "#9e9e9e"))))
 '(company-template-field ((((class color) (min-colors 89)) (:background "#ffffaf" :foreground "#626262"))))
 '(company-tooltip ((((class color) (min-colors 89)) (:background "#3a3a3a" :foreground "#5fafd7"))))
 '(company-tooltip-common ((((class color) (min-colors 89)) (:background "#5f5f5f" :foreground "#5fafd7"))))
 '(company-tooltip-selection ((((class color) (min-colors 89)) (:background "#626262" :foreground "#afd7ff"))))
 '(enh-ruby-number-face ((t (:foreground "medium purple"))) t)
 '(org-scheduled-previously ((t (:foreground "#778855"))))
 '(org-upcoming-deadline ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :height 1.0))))
 '(region ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#5fafd7"))))
 '(sml/filename ((t (:inherit sml/global :foreground "Black"))))
 '(sml/folder ((t (:inherit sml/global :foreground "Black" :weight normal))))
 '(sml/modes ((t (:inherit sml/global :background "grey40" :foreground "Black" :height 1.0))))
 '(sml/time ((t (:inherit sml/global :foreground "green" :height 1.05 :foundry "ALTS" :family "Digital"))))
 '(time-mail-face ((t (:family "IBM 3270"))) t))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
