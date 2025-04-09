(require 'package)
(setopt package-enable-at-startup nil)

;; Package repositories
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("elpa-nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (require 'use-package nil 'noerror)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrap use-package
(eval-when-compile
  (require 'use-package))

;; This replaces the old require-package.
;; If package isn't installed, fetch it.
(setopt use-package-always-ensure t)
(setopt use-package-hook-name-suffix nil)

;; Load the Org file containing the customizations!
(org-babel-load-file (expand-file-name "~/my-dotfiles/.emacs.d/myinit.org"))

;; TODO: Move all of these to your myinit.org file!
;; Created through M-x customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3d9df5511048d0815b1ccc2204cc739117c1a458be92fb26c03451149a1b1c11" "9d29a302302cce971d988eb51bd17c1d2be6cd68305710446f658958c0640f68" "45e409674661674c12070af5f8ef71741599eeb9fccd84557f1b822509f3b100" "e1990eeea39781f009b7f4634ca52a770d05bb7ce423a8fbbcd8a4f327efb626" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "82f0f47ac811eeb45fbcfc5fee48989e4d0bca11f74653b838c29fab9a20aee7" "6c01b5d4faa0f143055e63c9fba8e23e9160f181e54b14b46d56410811edbc9e" "8e08bb8da358e2cf92e10e4bac47b025ccbcf4c70788cdbd67dc4ed11f786194" "4c7228157ba3a48c288ad8ef83c490b94cb29ef01236205e360c2c4db200bb18" "2ef84b2c7ad4810912a095993ca8bdf386e1fd7f97842b57aac62dddb2bba211" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" "5a409d7844bfbc12bf6e6cf7a6a6cb9f79945a82dee315ed1778632b5686b6ec" "454e92bc5f22f690afce91cb6f92a3ccb638c57a89e84c55759fb16dfb2444e4" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "19d399257f7bf0deb86a48f618754575ad30fcd730ba73aa9ec91b704d09a5a5" "7d1c7ea4f3e73402f012b7011fc4be389597922fa67ad4ec417816971bca6f9d" "91e7cf11d25611122107adfed70b0751d3e57a10b099b185a6ae213f2c9405d3" "3ca84532551daa1b492545bbfa47fd1b726ca951d8be29c60a3214ced30b86f5" "64c4ff0a617e6bf33443821525f7feb3ef925a939c4575e77f3811c5b32e72c0" "9e1cf0f16477d0da814691c1b9add22d7cb34e0bb3334db7822424a449d20078" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "ba323a013c25b355eb9a0550541573d535831c557674c8d59b9ac6aa720c21d3" "f74e8d46790f3e07fbb4a2c5dafe2ade0d8f5abc9c203cd1c29c7d5110a85230" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "ba9c91bc43996f2fa710e4b5145d9de231150103e142acdcf24adcaaf0db7a17" "3cdd0a96236a9db4e903c01cb45c0c111eb1492313a65790adb894f9f1a33b2d" "5014b68d3880d21a5539af6ef40c1e257e9045d224efb3b65bf0ae7ff2a5e17a" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "13bf32d92677469e577baa02d654d462f074c115597652c1a3fce6872502bbea" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "41c478598f93d62f46ec0ef9fbf351a02012e8651e2a0786e0f85e6ac598f599" "b0dc32efddfd36f0a12d022ac3c79a3d6d9614558bc8a991e5a5a29be70dafe9" "ee77d69f78a1a17dcd141a58367fb5534dfdb07e94924a10a03c54190bb6a0ef" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" default))
 '(keyboard-coding-system 'utf-8-unix)
 '(package-selected-packages
   '(json-mode rspec-mode 0x0 terraform-mode zoom-window yatemplate nord-theme doom-themes goto-last-change org-ql org-web-tools ef-themes zenburn-theme howm ob-elixir ob-async solarized-theme gruvbox-theme twilight-bright-theme abyss-theme auto-package-update sqlformat ox-gfm circadian yasnippet-classic-snippets yasnippet-snippets restart-emacs golden-ratio olivetti smartparens magit-delta git-link exec-path-from-shell notmuch ace-window expand-region engine-mode mode-icons diminish keyfreq company emmet-mode flycheck-inline php-mode csv-mode js2-mode nov web-mode markdown-mode ob-mermaid mermaid-mode noflet yaml-mode robe rainbow-delimiters projectile-rails mix indent-tools hl-todo highlight-numbers highlight-indentation git-gutter-fringe elixir-mode editorconfig diff-hl bundler))
 '(projectile-mode t nil (projectile))
 '(projectile-mode-line
   '(:eval
     (if
         (file-remote-p default-directory)
         " Projectile"
       (format " [%s] "
               (projectile-project-name)))))
 '(save-place-mode t nil (saveplace))
 '(show-paren-mode t)
 '(tabbar-background-color "#ffffffffffff"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
