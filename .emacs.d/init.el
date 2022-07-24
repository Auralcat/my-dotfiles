(require 'package)
(setq package-enable-at-startup nil)

;; Package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)

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
 '(auto-compression-mode nil)
 '(battery-mode-line-format " 🔋 %b%p%% ")
 '(beacon-color "#F8BBD0")
 '(browse-url-browser-display nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(browse-url-firefox-program "firefox")
 '(column-number-mode t)
 '(custom-safe-themes
   '("467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" default))
 '(display-battery-mode t)
 '(display-default-load-average nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-format " 📅 %a, %d %b 🕙 %R ")
 '(display-time-mail-face 'change-log-email)
 '(display-time-mail-string "✉")
 '(display-time-mode nil)
 '(enh-ruby-use-encoding-map nil)
 '(ensime-sem-high-faces
   '((var :foreground "#000000" :underline
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
     (deprecated :strike-through "#000000")))
 '(epg-gpg-program (string-trim (shell-command-to-string "command -v gpg")))
 '(erc-away-nickname nil)
 '(erc-modules
   '(completion dcc fill list netsplit networks notifications readonly ring smiley track hl-nicks netsplit fill button match track readonly networks ring autojoin noncommands irccontrols move-to-prompt stamp menu list))
 '(erc-nick "Auralcat")
 '(erc-nicklist-use-icons nil)
 '(erc-nicklist-voiced-position 'top)
 '(erc-script-path '("~/my-dotfiles/.emacs.d/.erc/"))
 '(erc-try-new-nick-p t)
 '(erc-user-full-name "Realnamezz")
 '(evil-search-module 'evil-search)
 '(evil-shift-width 2)
 '(fci-rule-character-color "#452E2E")
 '(font-use-system-font t)
 '(global-discover-mode t)
 '(global-eldoc-mode nil)
 '(global-linum-mode nil)
 '(highlight-indent-guides-auto-enabled nil)
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(ido-enable-flex-matching nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message
   "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(js2-strict-missing-semi-warning nil)
 '(keyboard-coding-system 'utf-8-unix)
 '(line-number-mode nil)
 '(linum-format "%3i")
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(mode-icons
   '(("\\`CSS\\'" "css" xpm)
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
     ("\\` ?\\(?:ElDoc\\|Anzu\\|SP\\|Guide\\|PgLn\\|Undo-Tree\\|Ergo.*\\|,\\|Isearch\\|Ind\\)\\'" nil nil)))
 '(mode-icons-generate-emoji-xpms t)
 '(mode-icons-generate-font-xpms t)
 '(mode-icons-mode t)
 '(mode-ons-change-mode-name t)
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.2)
 '(nyan-bar-length 16)
 '(nyan-mode t)
 '(org-agenda-scheduled-leaders '("Scheduled: " "Sched. previously %2dx: "))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday 0)
 '(org-bullets-bullet-list '("✿" "❀" "◉" "○" "✸"))
 '(org-default-notes-file "~/file-bouncer/everything-bucket")
 '(org-plantuml-jar-path "~/file-bouncer/plantuml.jar")
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)))
 '(org-src-fontify-natively t)
 '(org-stuck-projects
   '("+LEVEL=2/-DONE"
     ("TODO" "NEXT" "NEXTACTION" "CANCELLED")
     nil ""))
 '(org-todo-keyword-faces '(("CURRENT" . "#ffcc11") ("NEXT" . "#6666ff")))
 '(org-todo-keywords '((sequence "TODO" "DONE")))
 '(package-selected-packages
   '(feature-mode gherkin-mode code-review the-matrix-theme prodigy zoom-window yasnippet-snippets yasnippet-classic-snippets yaml-mode yafolding xhair writeroom-mode web-mode vterm uuidgen use-package undo-fu typescript-mode twilight-bright-theme tree-sitter-langs tree-mode sqlformat solo-jazz-theme solarized-theme solaire-mode smartparens shelldon scss-mode sass-mode ruby-electric restart-emacs request rainbow-mode rainbow-delimiters projectile-rails prettier-js php-mode paper-theme ox-textile ox-slack ox-jira organic-green-theme org-bullets olivetti oldlace-theme ob-mermaid ob-elixir nyan-mode nov notmuch nord-theme noflet modus-themes mode-icons mix mermaid-mode magit-delta magit-commit-mark kubernetes keyfreq json-reformat json-par json-navigator js2-mode js-comint jazz-theme inf-elixir inf-clojure highlight-numbers hemisu-theme guess-language gruvbox-theme gpastel golden-ratio github-modern-theme git-link git-gutter-fringe gh-notify format-all flycheck-yamllint flycheck-inline flatui-theme expand-region exec-path-from-shell evil-textobj-tree-sitter evil-surround evil-string-inflection evil-org evil-numbers evil-matchit evil-leader evil-anzu erlang enh-ruby-mode engine-mode emmet-mode elpher elixir-mode editorconfig doom-themes dockerfile-mode docker diminish diff-hl deferred csv-mode company-web company-statistics company-prescient circadian bundler blamer base16-theme auto-package-update apib-mode ancient-one-dark-theme all-the-icons-completion ag ace-window ace-jump-mode ac-html-bootstrap abyss-theme))
 '(persp-show-modestring ''header)
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line
   '(:eval
     (if
         (file-remote-p default-directory)
         " Projectile"
       (format " [%s] "
               (projectile-project-name)))))
 '(reb-re-syntax 'string)
 '(remember-data-file "~/file-bouncer/everything-bucket")
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#3e999f" "#c9b4cf" "#8abeb7" "#4d4d4c"])
 '(safe-local-variable-values
   '((inf-elixir-base-command . "docker-compose -p wireless-billing_coverage run --rm web iex -S mix")
     (mix-path-to-bin . "/home/miriam/.asdf/shims/mix")
     (sample-var "Everlong")
     (auralcat/project-deployment-job-name . "Deploy_Service_To_Production_Kubernetes")
     (projectile-tags-command . "ctags -eR .")
     (inf-elixir-base-command . "make local-iex")))
 '(save-place-mode t nil (saveplace))
 '(scss-compile-at-save nil)
 '(selectric-mode nil)
 '(send-mail-function 'smtpmail-send-it)
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
 '(tags-tag-face 'default)
 '(telega-mode-line-mode t)
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
 '(type-break-mode nil)
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(web-mode-enable-auto-closing t)
 '(web-mode-enable-auto-expanding nil)
 '(writeroom-width 1.0)
 '(yas-global-mode t)
 '(yas-snippet-dirs
   '("~/.emacs.d/snippets" yasnippet-snippets-dir yasnippet-classic-snippets-dir)))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
