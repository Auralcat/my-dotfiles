(require 'package)
(setq package-enable-at-startup nil)

;; Use this for debugging
;; (setq debug-on-error t)

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
    '(custom-safe-themes
         (quote
             ("8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "0eccc893d77f889322d6299bec0f2263bffb6d3ecc79ccef76f1a2988859419e" "02199888a97767d7779269a39ba2e641d77661b31b3b8dd494b1a7250d1c8dc1" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" "73a13a70fd111a6cd47f3d4be2260b1e4b717dbf635a9caee6442c949fad41cd" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "392395ee6e6844aec5a76ca4f5c820b97119ddc5290f4e0f58b38c9748181e8d" "87a431903d22fa1cbb2becd88572e7d985e28c2253935448d0d754c13e85a980" "8dce5b23232d0a490f16d62112d3abff6babeef86ae3853241a85856f9b0a6e7" "450f3382907de50be905ae8a242ecede05ea9b858a8ed3cc8d1fbdf2d57090af" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "40343730a29b0c3fd97bd86cd5b05ed4124f1f3aeabe623cf1c1c9e9fb8ef2e5" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "4639288d273cbd3dc880992e6032f9c817f17c4a91f00f3872009a099f5b3f84" "14680231a7917bf5109d535ae11e40365debf31a544d86e60a6764e1309bd16c" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "60948671aeed6450faa8f28978a09586f142af6eb0b59e70848fcc32b9d36fd1" "fbacfa15b88adfd13d09903739db2db29a4b6813d5e61472fe24af2bf4e9faae" "03805593893aa4f82003efe5913d1bb6140805ad7c63d75ff459d6a51bb27d3d" "ecba61c2239fbef776a72b65295b88e5534e458dfe3e6d7d9f9cb353448a569e" "3e83abe75cebf5621e34ce1cbe6e12e4d80766bed0755033febed5794d0c69bf" "a3b9c613ca9beaae6539fd76ce09c78baed7700a7f513dc33a1069592f8bbe07" "ae3a3bed17b28585ce84266893fa3a4ef0d7d721451c887df5ef3e24a9efef8c" "08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf" "0d5f2910bee90a86523eddc3fba78efe33a45a51d0e3db76aba6f287ef1ffa7c" "0e89bf7a9d5d2a327b291d5e646f58362b2386f948a594ca993e7b0016b8425a" "28fa7536c8f563c6d6296989937a8e87a2dc6477fd7b366e0336a8997a521094" "c9b89349d269af4ac5d832759df2f142ae50b0bbbabcce9c0dd53f79008443c9" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "0daf22a3438a9c0998c777a771f23435c12a1d8844969a28f75820dd71ff64e1" "edea0b35681cb05d1cffe47f7eae912aa8a930fa330f8c4aeb032118a5d0aabf" "84488351928a5ea92546c89ff71cc79d3bb880ffac6172229b2302549c9dc4e7" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "b54826e5d9978d59f9e0a169bbd4739dd927eead3ef65f56786621b53c031a7c" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "4b19d61c560a93ef90767abe513c11f236caec2864617d718aa366618133704c" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "f146e316858ae534bb5c6db452a527f4bc735b0af46fd659f8b13ff274aa8375" "a63355b90843b228925ce8b96f88c587087c3ee4f428838716505fd01cf741c8" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "eb7be1648009af366d83f855191057bdc09348a2d9353db31da03b1cdec50cc5" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "13d20048c12826c7ea636fbe513d6f24c0d43709a761052adbca052708798ce3" "551596f9165514c617c99ad6ce13196d6e7caa7035cea92a0e143dbe7b28be0e" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "ab0950f92dc5e6b667276888cb0cdbc35fd1c16f667170a62c15bd3ed5ae5c5a" "d3a7eea7ebc9a82b42c47e49517f7a1454116487f6907cf2f5c2df4b09b50fc1" "44eec3c3e6e673c0d41b523a67b64c43b6e38f8879a7969f306604dcf908832c" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "234455c4073e7007f7f0a0a7e74dd03f8495b09540f467993c5cb847cfb600e1" "a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "3a3de615f80a0e8706208f0a71bbcc7cc3816988f971b6d237223b6731f91605" "e3fc83cdb5f9db0d0df205f5da89af76feda8c56d79a653a5d092c82c7447e02" "6ffef0161169e444b514a0f7f0cb7eac09d11c396cdc99bf85360a361c427886" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "dd2346baba899fa7eee2bba4936cfcdf30ca55cdc2df0a1a4c9808320c4d4b22" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "ea63cede1b158a5d1bda96d46b3d84ab72600282cf8b4c2f77383983c225fe2c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "aad2caf3759b343577768009f89d1a4cbfcea2c4f49331be753c397532250dae" default)))
 '(display-battery-mode t)
 '(display-default-load-average nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-format " 📅 %a, %d %b 🕙 %R ")
 '(display-time-mail-face (quote change-log-email))
 '(display-time-mail-string "✉")
 '(display-time-mode t)
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
 '(org-agenda-scheduled-leaders (quote ("Scheduled: " "Sched. previously %2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday 0)
 '(org-bullets-bullet-list (quote ("✿" "❀" "◉" "○" "✸")))
    '(org-capture-templates
         (quote
             (("a" "Add birthday to diary" entry
                  (file+headline "~/file-bouncer/emacs-diary" "Aniversários")
                  "Is this working?" :immediate-finish t)
                 ("b" "Budget logging templates")
                 ("bi" "Add income item in budget" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Caixa geral")
                     "| %t | %^{Descrição} | %^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("bm" "Meal card expense" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Vale-refeição")
                     "| %t | %^{Descrição} | -%^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("bk" "Market card expense" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Vale-alimentação")
                     "| %t | %^{Descrição} | -%^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("be" "Education/Event budget expense" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Orçamento educação/eventos")
                     "| %t | %^{Descrição} | -%^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("bo" "Add expense item in budget" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Caixa geral")
                     "| %t | %^{Descrição} | -%^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("br" "Add reward fund expense item in budget" table-line
                     (file+headline "~/file-bouncer/org-files/finances.org" "Fundo recompensa")
                     "| %t | %^{Descrição} | -%^{Valor} |" :immediate-finish t :table-line-pos "II-1")
                 ("m" "Media list capture templates")
                 ("mb" "New bookmark" entry
                     (file+headline "~/file-bouncer/media-list.org" "Links")
                     "** %?" :prepend t)
                 ("mp" "Add podcast to list" entry
                     (file+headline "~/file-bouncer/media-list.org" "Podcasts")
                     "** %^{Nome do podcast}" :immediate-finish t)
                 ("mm" "Add movie to list" entry
                     (file+headline "~/file-bouncer/media-list.org" "Filmes")
                     "** %^{Nome do filme}" :immediate-finish t)
                 ("ma" "Add music album to list" entry
                     (file+headline "~/file-bouncer/media-list.org" "Álbuns musicais")
                     "** %^{Nome do álbum}" :immediate-finish t)
                 ("mb" "Add book to list" entry
                     (file+headline "~/file-bouncer/media-list.org" "Livros")
                     "** %^{Nome do livro}" :immediate-finish t)
                 ("o" "Add item to outbox" entry
                     (file "~/file-bouncer/outbox.org")
                     "* %?" :prepend t)
                 ("e" "Add new Emacs task" entry
                     (file+headline "~/file-bouncer/org-files/projetos/configuracoes-emacs.org" "Tarefas")
                     "** BACKLOG %^{Tarefa}" :prepend t )
                 ("t" "Tarefa extra do stand-up (pra não colocar direto na lista do dia, organizar depois)" entry
                     (file+headline "~/file-bouncer/org-files/contact-based-system/eu.org" "Tarefas extras do stand-up (organizar depois)")
                     "** BACKLOG %?" :prepend t))))
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
             (telephone-line organic-green-theme flatui-theme twilight-bright-theme hemisu-theme ruby-extra-highlight ace-jump org-beautify-theme ruby-electric selectric-mode org-alert bundler projectile-rails cl-lib multi-term term+ wakatime-mode carbon-now-sh evil-snipe lua-mode yasnippet-snippets react-snippets nvm flycheck-credo zeno-theme rjsx-mode evil-numbers zeal-at-point js2-refactor ac-js2 edit-indirect yatemplate cheat-sh yaml-mode company-box evil-matchit evil-string-inflection circadian diminish alchemist sublimity writeroom-mode electric-operator jazz-theme highlight-indentation company-tern restart-emacs ob-restclient doom-themes restclient helm-projectile ibuffer-projectile yasnippet-classic-snippets vagrant-tramp ob-elixir company-prescient weechat web-mode web-beautify vagrant use-package twittering-mode scss-mode sass-mode rvm rust-mode ruby-tools robe rich-minority request rainbow-mode rainbow-delimiters projectile org-pomodoro org-bullets nyan-mode mode-icons memoize makey magit keyfreq js2-mode htmlize highlight-sexp highlight-numbers helm-tramp flymake-phpcs flymake-php flymake-elixir flycheck-mix evil-surround evil-leader evil-anzu eshell-prompt-extras eruby-mode enh-ruby-mode emojify emmet-mode electric-spacing editorconfig csv-mode company-web company-statistics company-php cl-generic autopair ace-jump-mode ac-html-bootstrap abyss-theme)))
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
             ("~/.emacs.d/snippets" yasnippet-snippets-dir yasnippet-classic-snippets-dir))))
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
