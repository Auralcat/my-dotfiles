;; This replaces the old require-package.
;; If package isn't installed, fetch it.
(setq use-package-always-ensure t)

;; If package isn't installed, fetch it
(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; Use-package
(require-package 'use-package)
(eval-when-compile
    (require 'use-package))

(require 'package)
(setq package-enable-at-startup nil)

;; Package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "http://orgmode.org/elpa/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

;; Load the Org file containing the customizations!
(org-babel-load-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.org"))

;; Created through M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(browse-url-browser-display nil)
 '(browse-url-firefox-program "firefox")
 '(company-abort-manual-when-too-short nil)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-auto-complete-chars (quote (32 41 46)))
    '(company-backends
         (quote
             (company-web-html company-nxml company-css company-eclim company-semantic company-clang company-cmake company-capf company-files
                 (company-dabbrev-code company-gtags company-etags company-keywords)
                 company-oddmuse company-dabbrev)))
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.4)
 '(company-minimum-prefix-length 3)
 '(company-transformers (quote (company-sort-by-backend-importance)))
    '(custom-safe-themes
         (quote
             ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "5d8c90053cf9c111aa50d26596a746fc0dacd18f6bc75fba8251fdd5f43a6277" "dcf7154867ba67b250fe2c5cdc15a7d170acd9cbe6707cc36d9dd1462282224d" "4e7e5808a568cbbc6154298ac4153c2ee15b3aed5dc6e7267e3f18b811c4616a" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced" "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "f8b5f1c5ab70da141ae308b9ce23ebc2b713dd3a22a6ebb08bfb55f5c7a11287" "5908457b14343ddca0ff1efa27247fb8eec94bc1eaab60fe58c1d033a3188315" "d857acdacdb74d5a3eb35c1d009d0c598f9954d51da523859db6366479cc31cd" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36d92f830c21797ce34896a4cf074ce25dbe0dabe77603876d1b42316530c99d" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "59e5301cce088c4a493f1745b0d409eff6ba955f06150f77ffaf39d955bb8442" "da3f9bcccf44c8b3bb64807a4eb55a5b2089488ad28201a968333dee29b10d89" "39d212cddd810c2a5b450b32a0acad8ae85b2d534301b2cf86cbc318874b1a5d" "643ce4d69567eedf342aa4913e004dc37e8d4567b226e04bbbd7f08cdd6fb8a3" "c3009cace2d39b02a3c660a68b05ca2796b9a0f623802c1558addf092b0bc91a" "3f3ff89135934379b84e067b8db7386efc63bef5695be0b1765ed95801a9ac2e" "7326e5068f99b8022e2876503284ef8b35c4a6e2f9f5e2cc7e3a147b2af9bc86" "33b2941efadace8f164466cb183194a29fc2562e539bb932235cf015df85b65c" "002be25c63dc642988bcabf86aa1cc3cff72d168017f7b668974f70b76957324" "3a427b2b50f57b2e97b499b5971c2b79a0862f690ddb372292f72b3aa71e8ffb" "0fc7298072540a92cc11274c4e1712d351aabe3b20f1294da8f3a8ab6e5e2e43" "d3286e2f0cee02a049e98e3e2d9c31f944b0d1b34bf1b9d4bcbf401baca174e0" "32fba6a2b0f2e8388a75ed41e70e7d190966ce5c6fe4ef83abd6935ba6d82edb" "24fac42b4ad7f2eabbd134fd1b3aebab2964ad2e5eeb97a36c30bd343c3e3be3" "91d9e12212df3e3dcf313eee7336910166ebc3735268977f921433fcd15dc0e1" "e278b6a543c79c5758f95cc9d712af0724c565196bdd7393b1b0e738b7e4439a" "be4b75ad1770303225aaae89e446c60af40c5848e916ddf98e8089b1713e623e" "9919034690c9e2409c2dccf722e0b2f574d4d356d45681f807dbf1f70bd34825" "345a53dbeaeab1777e5c25b88481aabd37e1573dc015f6c4bff4d785a18daa7c" "62be4d850eb548bddd7cbcb650c088db83e35c058f03c3124153434a39cb2599" "e4384ced8f89c5133cff92bc314becbc219d7c3500c197c8bef7debc251d4676" "6af1c89536ac7be91d8a709923941299e0d8822fa5b32855b39eef36eb8bb7f1" "f32de7d2d8aa2f209397ab036a58c041e92ae40391c6b080e3e1f79ac5374661" "62c625342d7b9228294b52397ffee4afe5992fdf2cc17e7d328cfe054d6734b0" "e4f6561385e45763498650d034c579c2b545168d55c0d9057cc25dd1b8a2c931" "b6cbc6e9a3c0923ed2a8c08709c75ac5ac1665f4d5e44fbc6e14759ff1010ccb" "518b2cf78e56b057a3d4b44ada6c4b4e298fc9866b7c62391da6aa1d8a357292" "cd0d3e633ecc59e31d99d54af5e063def1ff2a6cb08ae71a3ef68ead857b8f74" "4f7a026ab163805b11d48059c9c8237a62beb54c6b2181f1de3656efe8ee616d" "759b9535d645287a170284034f1e399dc0652c12337e72b3d569ff6848667667" "71d9d87d6141d5aaa6a014edf31e142bbfb4866ae990c57645b193903f65d990" "4c8ef842834add984607af2a650fed685d0ef60d7b38c8ea802001236404b80a" "c0e10052057239274f44a31b02c9869815436776e1f34920005555265eb92001" "90b9b16036e9ff0a357c029efd2d9c8b4efc52cd5c3d32c041016992205696bc" "c71114e53758c572d2a854a66dd783c1cbb5036adc5f021f9fc21033e2eec112" "7baed9f018d6518afa7c55e5e6acd83b69db040ac1e0f7bf8cce0b42c5f29f23" "1a379cd3373fbd40a31285ea126d5433ae7c9827c8e881ce1f62c3ca6f7fae5c" "9671fe89034bdf5ea472289ff5b375d8b99c23f92c4dcfecbca613a3ed2c7844" "0b002097186667b1416e4499172007ca9ae8f9b4d80bcfba5b1722e6d9ef9b95" "f5729cac6fcade83c92e50c823eca0e24add9e6c30d1bcafa5076e07909a1ed2" "4134cc495b3e774194861d932a0c094dc729328664b00d82e02c4d31fead14e0" "3856e441793a643f6b2e733b70f3fc2d8e74cf810368eff22942f738d89c4854" "9b803ba5ea6d16e838c3229c7679e8325cbe43083cd4155202925b3f11c3c912" "c9dd6b38801d2db93472aaa63aa15e7c0de4f3f2acecc72b04ca6b3efa25f600" "764b72b168f2a5935f51977c49781c9864312fd9f45e3cc317997d12aa8db3bf" "40af07c028aab91628cac1a41c62f8e4b4e893bef605765196e6b3d8a02ab2e0" "25b0083e7ffd77261165fc391b8a7f94049537a58536da2f0e6fd83ecff30ae8" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" default)))
 '(display-battery-mode t)
 '(display-default-load-average nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
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
 '(font-use-system-font nil)
 '(global-auto-revert-mode t)
 '(global-emojify-mode t)
 '(global-linum-mode nil)
 '(ido-enable-flex-matching nil)
 '(image-animate-loop t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice nil)
 '(initial-major-mode (quote org-mode))
    '(initial-scratch-message
         "# This buffer is for notes you don't want to save, and for program sketching.
# You can write the contents of this buffer to a file with C-x C-w.
# Current default mode for this buffer is: Org-mode.")
 '(keyboard-coding-system (quote utf-8-unix))
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
 '(nyan-mode nil)
    '(org-agenda-files
         (quote
             ("~/file-bouncer/org-files/stand-up-meetings/Junho-2018.org" "~/file-bouncer/org-files/projetos-sites/projeto-dermatologia.org" "/home/lucas/file-bouncer/org-files/contact-based-system/eu.org" "~/file-bouncer/org-files/blog.org" "/home/lucas/file-bouncer/org-files/contact-based-system/rosiane.org" "/home/lucas/file-bouncer/org-files/contact-based-system/elaine.org" "/home/lucas/file-bouncer/org-files/contact-based-system/aline-pegas.org" "~/file-bouncer/org-files/contact-based-system/ariane.org")))
 '(org-agenda-scheduled-leaders (quote ("Scheduled: " "Sched. previously %2dx: ")))
 '(org-agenda-skip-scheduled-if-done t)
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
 '(tags-tag-face (quote default))
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(tramp-default-method "ssh")
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
 '(company-scrollbar-bg ((((class color) (min-colors 89)) (:background "#5f5f5f"))))
 '(company-scrollbar-fg ((((class color) (min-colors 89)) (:background "#9e9e9e"))))
 '(company-template-field ((((class color) (min-colors 89)) (:background "#ffffaf" :foreground "#626262"))))
 '(company-tooltip ((((class color) (min-colors 89)) (:background "#3a3a3a" :foreground "#5fafd7"))))
 '(company-tooltip-common ((((class color) (min-colors 89)) (:background "#5f5f5f" :foreground "#5fafd7"))))
 '(company-tooltip-selection ((((class color) (min-colors 89)) (:background "#626262" :foreground "#afd7ff"))))
 '(org-scheduled-previously ((t (:foreground "#778855"))))
 '(org-upcoming-deadline ((((class color) (min-colors 89)) (:foreground "#a40000"))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :height 1.0))))
 '(region ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#5fafd7"))))
 '(sml/filename ((t (:inherit sml/global :background "Grey22" :foreground "Black"))))
 '(sml/folder ((t (:inherit sml/global :background "Grey22" :foreground "Black" :weight normal))))
 '(sml/modes ((t (:inherit sml/global :background "grey40" :foreground "Black" :height 1.0))))
 '(sml/time ((t (:inherit sml/global :background "black" :foreground "green" :height 1.05 :foundry "ALTS" :family "Digital"))))
 '(time-mail-face ((t (:family "IBM 3270"))) t))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
