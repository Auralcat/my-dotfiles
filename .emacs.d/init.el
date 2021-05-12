(require 'package)
(setq package-enable-at-startup nil)

;; Package repositories
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "https://orgmode.org/elpa/")
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
 '(auto-image-file-mode t)
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(battery-mode-line-format " 🔋 %b%p%% ")
 '(beacon-color "#F8BBD0")
 '(browse-url-browser-display nil)
 '(browse-url-browser-function 'browse-url-default-browser)
 '(browse-url-firefox-program "firefox")
 '(column-number-mode t)
 '(company-abort-manual-when-too-short nil)
 '(company-auto-commit ''company-explicit-action-p)
 '(company-auto-commit-chars '(41))
 '(company-box-doc-enable nil)
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 3)
 '(company-prescient-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-tooltip-minimum 6)
 '(company-transformers nil)
 '(custom-safe-themes
   '("0f7fa4835d02a927d7d738a0d2d464c38be079913f9d4aba9c97f054e67b8db9" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "99189f89d7a6566780a92addd8d01b4dac2ebe2c95e82da32bd4729fbf2897db" "6df412e59dbfe7f72f24319b9ee4513e40bb0e44384fc93a2c77399e641348f6" "7e22a8dcf2adcd8b330eab2ed6023fa20ba3b17704d4b186fa9c53f1fab3d4d2" "54ead94fc355041d7bf4570bb606bd4f9682b475fdf685c207695bfee93abd50" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "5f4dfda04fbf7fd55228266c8aab73953d3087cea7fd06dd7f8ff1e4a497c739" "e006d90eaf64a2d78154ecb277ccc82327e1b975d7d0e2f933acca3131cd0177" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "7b3ce93a17ce4fc6389bba8ecb9fee9a1e4e01027a5f3532cc47d160fe303d5a" "c8e076f0e2df414c02fdb46b09b735628e73c73f72f9d78392edf99de7d86977" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "19d399257f7bf0deb86a48f618754575ad30fcd730ba73aa9ec91b704d09a5a5" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "68156f0669d7763a858882414ccd7e2df7b92c9c75618ee7f242e14613adfbaa" "70fa68d2c9a02d46282b42c3193411b13490d5d130326c63bfba1f807e0c53a3" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "8e959d5a6771b4d1e2177263e1c1e62c62c0f848b265e9db46f18754ea1c1998" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "8310462f5008fe10661e27ceab164e05c509343641cc262134fc623422b5999d" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "0f302165235625ca5a827ac2f963c102a635f27879637d9021c04d845a32c568" "0ec7094cc0a201c1d6f7c37f2414595d6684403b89b6fd74dcc714b5d41cd338" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "64affc3597b4271ba6b0b428777d616cfb20d8f7f147dbd00f1de220b2b59bbf" "4ca84f9861113f889d505b66c4a9c30b81400f450b54f4c857d3eb44c5bdc601" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "5516001c13a43f1f5be2d7df8035445f8d7c73179f637e40c1503afb184d98f2" "4e6ec38d7940398caef89b1f653a7f88d909f58a2837d6504edc573b063919df" "1a094b79734450a146b0c43afb6c669045d7a8a5c28bc0210aba28d36f85d86f" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "d9c957b0e8d2d7f1bbb781fc729e06598017ade2d0c18611e5abbdde0f65d981" "6213a6047cc19f580c37ef3f6d47fd5a55ebdf9b5590475d8f7a6aecd79a1cc0" "7de92d9e450585f9f435f2d9b265f34218cb235541c3d0d42c154bbbfe44d4dd" "be0efbaebc85494f3c1c06e320fd13a24abf485d5f221a90fe811cea9a39ed85" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "081f8df9ac778062cb547abc53c8a0a3ad36417247c71afbcd437b82ff25a980" "6a9e48e3ff2170edd584d2e9655d3b5c44a8d5a36ac5e52e9db98671a606e345" "9fcc37e34ac8603a7ebef47aac016bf9158145d3b8035204c650038bee310450" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "711efe8b1233f2cf52f338fd7f15ce11c836d0b6240a18fffffc2cbd5bfe61b0" "f2c35f8562f6a1e5b3f4c543d5ff8f24100fae1da29aeb1864bbc17758f52b70" "75615f00bca2d070186d217af34b1337badbc55e6a6d6c3f6929e4c3405c8079" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "9c859eda5163e646e164e40467408e9a00f4f4209b1876b5af401bb716bb4f1b" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "7be5c904ab89af341a06fa146844d4f96ffb58c39290c45a5ac2c0ddb5a346cc" "74ba9ed7161a26bfe04580279b8cad163c00b802f54c574bfa5d924b99daa4b9" "e246ff6951678c835ab95aedb99a48fdd665656eadf8b8bc0a4f60e17fbc6b7e" "9d8ad1c413fccc14d992f6bed0ead11f1798a05ee8913daaa24a24604c212b61" "bca1ef96784645bfaf69ea2430dd16801310428eb5fca53c7f8067336be38871" "b7e66d62669194652257afbec5baa9909b261aa219022aa4e5b0319c9955bc01" "830c887bd2cefd77326ddf24389f3806c2a561a550e309691933f772f1bc5825" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "00664002472a541e3df8a699c2ea4a5474ea30518b6f9711fdf5fe3fe8d6d34f" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "9e39a8334e0e476157bfdb8e42e1cea43fad02c9ec7c0dbd5498cf02b9adeaf1" "ab32f86a67c520ca774b9697dbc47ca468d4ac001f32372208566abf2ceb114a" "8feca8afd3492985094597385f6a36d1f62298d289827aaa0d8a62fe6889b33c" "1d78d6d05d98ad5b95205670fe6022d15dabf8d131fe087752cc55df03d88595" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "34c1b320a9d35318ca01660d533eee299d538f5a0c505c076511493b0a4f093d" "5a3e0900b08e23428547faf56302e4478c122285ed83b8ce239c01e1037fe0d2" "3d9df5511048d0815b1ccc2204cc739117c1a458be92fb26c03451149a1b1c11" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "3e67ab3ba08bccc7f0c318a5d50fdd6ff6fe53135ef50a286679e850ee16f047" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "b34559587bfbb22c8cfba36a81c55eeff9bf60dfa7906a2f4e94a770358f715d" "8f3f0f1ae5a9907bf8a978a0b20b763f010dd77c28292bbca27871d3e956b3c1" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "3d73b17d60f39ab42de06bfbfbbba0df276a30e5ade90d37df20e6f40367f247" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "bb7303ab60750380957d8205794d031ab222390673ff6dd6369d0277b966c1d4" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "5903c5f26edd1ef3f9555a2864982b24f2980f096aacb9da0b4b5ccd47962233" "41c478598f93d62f46ec0ef9fbf351a02012e8651e2a0786e0f85e6ac598f599" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "b359d6cdc0684714046222cf2c21df785d01846a34f3676f2253a70c2bac31d6" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" default))
 '(display-battery-mode t)
 '(display-default-load-average nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date nil)
 '(display-time-default-load-average nil)
 '(display-time-format " 📅 %a, %d %b 🕙 %R ")
 '(display-time-mail-face 'change-log-email)
 '(display-time-mail-string "✉")
 '(display-time-mode nil)
 '(dynamic-completion-mode t)
 '(emmet-indent-after-insert t)
 '(emmet-indentation 2)
 '(emmet-preview-default t)
 '(engine-mode nil)
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
 '(evil-shift-width 2)
 '(fci-rule-character-color "#452E2E")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(font-use-system-font t)
 '(git-commit-summary-max-length 50)
 '(global-anzu-mode t)
 '(global-auto-revert-mode t)
 '(global-discover-mode t)
 '(global-eldoc-mode nil)
 '(global-emojify-mode t)
 '(global-linum-mode nil)
 '(global-wakatime-mode t)
 '(global-writeroom-mode nil nil (writeroom-mode))
 '(highlight-indent-guides-auto-enabled nil)
 '(hl-paren-background-colors '("#2492db" "#95a5a6" nil))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(ido-enable-flex-matching nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(initial-scratch-message
   "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(jdee-db-active-breakpoint-face-colors (cons "#f2f2f2" "#4271ae"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f2f2f2" "#718c00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f2f2f2" "#a5a4a5"))
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
 '(objed-cursor-color "#c82829")
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
   '(ox-jira undo-fu base16-theme esup solo-jazz-theme haskell-mode ob-mermaid mermaid-mode zenburn-theme gherkin-mode railscasts-theme symbol-overlay engine-mode inf-clojure clojure-mode notmuch atom-one-dark-theme erlang modus-vivendi-theme github-modern-theme github-theme paper-theme po-mode ts-comint all-the-icons ox-gfm nov mix tide typescript-mode go-mode dockerfile-mode exec-path-from-shell forge nord-theme cobol-mode undersea-theme modus-operandi-theme erc-hl-nicks olivetti gotham-theme night-owl-theme gruvbox-theme apib-mode rbtagger guess-language flycheck-inline font-lock+ elm-mode slack telephone-line organic-green-theme flatui-theme twilight-bright-theme hemisu-theme ruby-extra-highlight ace-jump org-beautify-theme ruby-electric selectric-mode org-alert bundler projectile-rails cl-lib lua-mode yasnippet-snippets react-snippets nvm flycheck-credo zeno-theme rjsx-mode evil-numbers zeal-at-point js2-refactor ac-js2 edit-indirect yatemplate cheat-sh yaml-mode company-box evil-matchit evil-string-inflection circadian diminish alchemist sublimity writeroom-mode electric-operator jazz-theme highlight-indentation restart-emacs doom-themes ibuffer-projectile yasnippet-classic-snippets vagrant-tramp ob-elixir company-prescient web-mode web-beautify vagrant use-package twittering-mode scss-mode sass-mode rust-mode ruby-tools robe rich-minority request rainbow-mode rainbow-delimiters projectile org-bullets nyan-mode mode-icons memoize makey magit keyfreq js2-mode htmlize highlight-sexp highlight-numbers flymake-phpcs flymake-php flymake-elixir flycheck-mix evil-surround evil-leader evil-anzu eshell-prompt-extras eruby-mode enh-ruby-mode emojify emmet-mode electric-spacing editorconfig csv-mode company-web company-statistics company-php cl-generic autopair ace-jump-mode ac-html-bootstrap abyss-theme))
 '(pdf-view-midnight-colors '("#282828" . "#f2e5bc"))
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
 '(tramp-default-method "ssh" nil (tramp))
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
