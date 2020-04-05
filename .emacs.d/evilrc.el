;; Porting my .vimrc to Emacs

;; Colorize the cursor depending on current mode.
;; It's one of those small things that you can't live without once you get it.

;; ADDENDUM: You can get the color of a face like this:
;; (color-values (face-foreground 'face-name))
;; Setting the color through faces blends well with each theme, but if you hover over the
;; face it gets the color from, it defeats the whole purpose of this.
;; Evil-mode cursor colors

;; I have bound C-a to back-to-indentation-or-beginning in another config,
;; Now I need C-e, $ is too far for me 😂
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

;; Some buffers in Emacs are read-only and can be usually killed pressing
;; lowercase q. Much more convenient if you ask me.
;; Use Q in normal mode to kill current buffer.
(define-key evil-normal-state-map (kbd "Q") 'kill-current-buffer)

;; Evil-snipe: use s and S for sniping, transfer the command from S to K
(define-key evil-normal-state-map (kbd "s") 'evil-snipe-s)
(define-key evil-normal-state-map (kbd "S") 'evil-snipe-S)
(define-key evil-normal-state-map (kbd "K") 'evil-change-whole-line)

;; INSERT MODE
;; Use C-g to exit from Normal mode as well.
(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)

;; Add C-a and C-e to insert mode
(define-key evil-insert-state-map (kbd "C-a") 'back-to-indentation-or-beginning)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

;; Use C-t to transpose chars, I can use tab in graphical mode,
;; and I don't use the pop-mark command from normal mode.
(define-key evil-normal-state-map (kbd "C-t") 'transpose-chars)
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)

;; Use C-w to kill region. I don't use C-w to delete backward words anymore.
(define-key evil-insert-state-map (kbd "C-w") 'completion-kill-region)

;; Use C-y to paste text, just like Emacs mode
;; The previous command was copy-line-from-above, not that useful IMO
(define-key evil-insert-state-map (kbd "C-y") 'yank)

(defun evil-control-at ()
  "Paste last insertion and exit insert mode."
  (interactive)
  (evil-paste-last-insertion)
  (evil-normal-state)
  )
;; Use C-@ in insert mode to paste the last insertion and quit insert mode.
;; This is present in Vim, but not in Evil for some reason.
(define-key evil-insert-state-map (kbd "C-@") 'evil-control-at)

;; Mimic the behavior of C-o from insert-mode in emacs-mode
(define-key evil-emacs-state-map (kbd "C-o") 'evil-execute-in-normal-state)

;; COMPLETIONS
;; C-x C-f completes a file path in insert mode.
(define-key evil-insert-state-map (kbd "C-x C-f") 'company-files)

;; C-x C-l completes a line.
(define-key evil-insert-state-map (kbd "C-x C-l") 'evil-complete-previous-line)

;; C-x C-o: syntax completion, depending on mode.
(add-hook 'enh-ruby-mode-hook #'(lambda () (define-key evil-insert-state-map (kbd "C-x C-o") 'company-robe)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (define-key evil-insert-state-map (kbd "C-x C-o") 'company-elisp)))

;; C-x C-p: Yasnippet template completion.
(define-key evil-insert-state-map (kbd "C-x C-p") 'company-yasnippet)

;; C-x C-]: Tag completion.
(define-key evil-insert-state-map (kbd "C-x C-]") 'company-etags)

;; Delete previous word with C-w in insert mode. Just like Vim.
(define-key evil-insert-state-map (kbd "C-w") 'backward-kill-word)

;; Evil Leader configs
;; Use SPC as leader key (yah, this is the idea behind Spacemacs)
(evil-leader/set-leader "SPC")

;; Global bindings go here
(evil-leader/set-key
  ;; Eshell
  "$" 'eshell
  ;; Org-agenda
  "a" 'org-agenda
  ;; Frequently visited bookmarks
  "bk" '(lambda() (interactive) (bookmark-jump "org-capture-last-stored"))
  "br" '(lambda() (interactive) (bookmark-jump "org-refile-last-stored"))
  ;; Bookmark shortcuts
  "bb" 'helm-bookmarks
  "b4b" 'bookmark-jump-other-window
  "bl" 'list-bookmarks
  "bs" 'bookmark-set
  ;; Org-clock functions. It makes sense to group them here.
  ;; For example, you can choose which clock funct
  "ci" 'org-clock-in
  "cl" 'org-clock-in-last
  "co" 'org-clock-out
  "cj" 'org-clock-goto
  "cp" 'org-pomodoro
  ;; Dired stuff
  "dj" 'dired-jump
  "d4j" 'dired-jump-other-window
  ;; Open frequently visited files
  "fb" 'my-find-budget-file
  "fe" 'my-find-evilrc-file
  "ff" 'my-find-finances-log-file
  "fm" 'jump-to-messages-buffer
  "fi" 'my-find-init-el-file
  "fo" 'my-find-init-org-file
  "fs" 'jump-to-scratch-buffer
  "fw" 'my-find-work-org-file
  ;; Magit stuff
  "gs" 'magit-status
  "gb" 'magit-blame
  "gc" 'magit-branch-or-checkout
  "gf" 'magit-fetch-all-prune
  "gm" 'magit-commit
  "g!" 'magit-git-command
  "g$" 'magit-process-buffer
  "gz" 'magit-stash
  "gl" 'magit-log
  "gp" 'magit-push
  "gx" 'magit-reset
  ;; Helm prefix
  "h" 'helm-command-prefix
  ;; Quick ace-jump in evil-mode.
  "j" 'ace-jump-mode
  ;; Org-capture. Keeping consistency with org-agenda's binding.
  "k" 'org-capture
  ;; Work-related stuff
  "mb" 'auralcat-run-bash-in-docker-container
  "mp" 'my-open-ci-current-build
  "mc" 'my-find-daily-todos-org-file
  "mr" 'auralcat-run-rails-console-in-docker-container
  "ms" 'my-goto-staging-server-url
  "ml" 'my-goto-local-server-url
  "mg" 'my-goto-work-github-repo-url
  ;; Get the relative file name of current buffer. I pass that to rspec.
  "mf" 'auralcat-kill-relative-file-name
  "mt" 'auralcat-run-local-rails-test-inside-docker-container
  ;; Flip window with ace-window.
  "o" 'aw-flip-window
  ;; Projectile prefix
  "p" 'projectile-command-map
  ;; Projectile-Rails prefix
  "r" 'projectile-rails-command-map
  ;; Slack bindings
  "smi" 'slack-im-select
  "smg" 'slack-group-select
  "smc" 'slack-channel-select
  "sus" 'slack-user-set-status
  ;; TODO: map this to org-mode locally.
  "t" 'my-org-remote-todo
  ;; Org mark ring. I use it a lot.
  ","   'org-mark-ring-push
  "."   'org-mark-ring-goto
  )
