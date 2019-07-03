(require 'package)
(setq package-enable-at-startup nil)

;; Package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
          ("melpa-stable" . "https://melpa-stable.milkbox.net/packages/")
          ;; Use it when you can't find what you want in other repos
          ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(require 'use-package)

;; This replaces the old require-package.
;; If package isn't installed, fetch it.
(setq use-package-always-ensure t)

;; Load the Org file containing the customizations!
(org-babel-load-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.org"))

;; Created through M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
     [default default default italic underline success warning error])
  '(ansi-color-names-vector
     ["#151515" "#953331" "#546a29" "#909737" "#385e6b" "#7f355e" "#34676f" "#c6a57b"])
  '(ansi-term-color-vector
     [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(auto-image-file-mode t)
 '(battery-mode-line-format " 🔋 %b%p%% ")
 '(beacon-color "#F8BBD0")
 '(browse-url-browser-display nil)
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(browse-url-firefox-program "firefox")
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-auto-complete-chars (quote (41)))
  '(company-backends
     (quote
       (company-yasnippet
         (company-tern company-web-html company-css)
         company-semantic company-capf company-files
         (company-dabbrev-code company-gtags company-etags company-keywords)
         company-oddmuse company-dabbrev)))
 '(company-box-doc-enable nil)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 3)
 '(company-prescient-mode t)
 '(company-tooltip-minimum 6)
 '(company-transformers nil)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
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
 '(docker-command "/usr/local/bin/docker")
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
 '(epg-gpg-program "/usr/local/bin/gpg")
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
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)) t)
 '(evil-insert-state-cursor (quote ("#D50000" bar)) t)
 '(evil-normal-state-cursor (quote ("#F57F17" box)) t)
 '(evil-shift-width 2)
 '(evil-visual-state-cursor (quote ("#66BB6A" box)) t)
 '(fci-rule-character-color "#452E2E")
 '(fci-rule-color "#151515")
 '(font-use-system-font nil)
 '(global-anzu-mode t)
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-emojify-mode t)
 '(global-linum-mode nil)
 '(global-wakatime-mode t)
 '(global-writeroom-mode nil nil (writeroom-mode))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
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
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(ido-enable-flex-matching nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-major-mode (quote org-mode))
  '(initial-scratch-message
     "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(jdee-db-active-breakpoint-face-colors (cons "#10151C" "#5EC4FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#10151C" "#8BD49C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#10151C" "#384551"))
 '(js2-strict-missing-semi-warning nil)
 '(keyboard-coding-system (quote utf-8-unix))
 '(line-number-mode nil)
 '(linum-format "%3i")
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
 '(mode-icons-generate-emoji-xpms t)
 '(mode-icons-generate-font-xpms t)
 '(mode-icons-mode t)
 '(mode-ons-change-mode-name t)
  '(nrepl-message-colors
     (quote
       ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.2)
 '(nyan-bar-length 16)
 '(nyan-mode t)
 '(objed-cursor-color "#ff5555")
 '(org-agenda-scheduled-leaders (quote ("Scheduled: " "Sched. previously %2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday 0)
 '(org-bullets-bullet-list (quote ("✿" "❀" "◉" "○" "✸")))
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
 '(org-plantuml-jar-path "~/file-bouncer/plantuml.jar")
 '(org-pomodoro-audio-player "/usr/bin/mpv")
  '(org-pomodoro-finished-sound
     "~/.emacs.d/pomodoro-sounds/Librem-5-power-on-by-Antonio-Paternina-Alvarez.ogg")
 '(org-pomodoro-finished-sound-p t)
 '(org-pomodoro-format "Focus!~%s")
  '(org-pomodoro-long-break-sound
     "~/.emacs.d/pomodoro-sounds/Librem-5-phone-call-3-by-Nohumanconcept.ogg")
 '(org-pomodoro-long-break-sound-p t)
  '(org-pomodoro-short-break-sound
     "~/.emacs.d/pomodoro-sounds/Librem-5-email-notification-2-by-Pablo-Somonte.ogg")
 '(org-pomodoro-short-break-sound-p t)
  '(org-pomodoro-start-sound
     "~/.emacs.d/elpa/org-pomodoro-20171108.1314/resources/bell.wav")
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
       (guess-language flycheck-inline font-lock+ elm-mode slack telephone-line organic-green-theme flatui-theme twilight-bright-theme hemisu-theme ruby-extra-highlight ace-jump org-beautify-theme ruby-electric selectric-mode org-alert bundler projectile-rails cl-lib multi-term term+ wakatime-mode carbon-now-sh evil-snipe lua-mode yasnippet-snippets react-snippets nvm flycheck-credo zeno-theme rjsx-mode evil-numbers zeal-at-point js2-refactor ac-js2 edit-indirect yatemplate cheat-sh yaml-mode company-box evil-matchit evil-string-inflection circadian diminish alchemist sublimity writeroom-mode electric-operator jazz-theme highlight-indentation company-tern restart-emacs ob-restclient doom-themes restclient helm-projectile ibuffer-projectile yasnippet-classic-snippets vagrant-tramp ob-elixir company-prescient weechat web-mode web-beautify vagrant use-package twittering-mode scss-mode sass-mode rvm rust-mode ruby-tools robe rich-minority request rainbow-mode rainbow-delimiters projectile org-pomodoro org-bullets nyan-mode mode-icons memoize makey magit keyfreq js2-mode htmlize highlight-sexp highlight-numbers helm-tramp flymake-phpcs flymake-php flymake-elixir flycheck-mix evil-surround evil-leader evil-anzu eshell-prompt-extras eruby-mode enh-ruby-mode emojify emmet-mode electric-spacing editorconfig csv-mode company-web company-statistics company-php cl-generic autopair ace-jump-mode ac-html-bootstrap abyss-theme)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(persp-show-modestring (quote (quote header)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
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
 '(selectric-mode nil)
 '(send-mail-function (quote smtpmail-send-it))
 '(shell-file-name "/bin/bash")
 '(show-paren-mode t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(smtpmail-smtp-server "smtp.yandex.com")
 '(smtpmail-smtp-service 25)
 '(sublimity-mode nil)
 '(tabbar-background-color "#ffffffffffff")
 '(tags-tag-face (quote default))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(type-break-mode nil)
 '(vc-annotate-background "#1D252C")
 '(vc-annotate-background-mode nil)
  '(vc-annotate-color-map
     (list
       (cons 20 "#8BD49C")
       (cons 40 "#abcd93")
       (cons 60 "#cbc68b")
       (cons 80 "#EBBF83")
       (cons 100 "#e5ae6f")
       (cons 120 "#df9e5b")
       (cons 140 "#D98E48")
       (cons 160 "#dc885f")
       (cons 180 "#df8376")
       (cons 200 "#E27E8D")
       (cons 220 "#df7080")
       (cons 240 "#dc6274")
       (cons 260 "#D95468")
       (cons 280 "#b05062")
       (cons 300 "#884c5c")
       (cons 320 "#604856")
       (cons 340 "#56697A")
       (cons 360 "#56697A")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding nil)
 '(writeroom-width 1.0)
 '(yas-global-mode t)
  '(yas-snippet-dirs
     (quote
       ("~/.emacs.d/snippets" yasnippet-snippets-dir yasnippet-classic-snippets-dir))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
