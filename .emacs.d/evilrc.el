;; Customize undo command as of Emacs 28.
;; 'undo-redo is the native feature in this new version.
(evil-set-undo-system 'undo-redo)

;; I like to differentiate the Emacs state cursor with a box
(setq evil-emacs-state-cursor '("magenta" box))

;; I have bound C-a to back-to-indentation-or-beginning in another config,
;; Now I need C-e, $ is too far for me
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

;; Some buffers in Emacs are read-only and can be usually killed pressing
;; lowercase q. Much more convenient if you ask me.
;; Use Q in normal mode to kill current buffer.
(define-key evil-normal-state-map (kbd "Q") 'auralcat/quit-and-kill-buffer)

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

;; Use C-y to paste text, just like Emacs mode
;; The previous command was copy-line-from-above, not that useful IMO
(define-key evil-insert-state-map (kbd "C-y") 'yank)

(defun evil-control-at ()
  "Paste last insertion and exit insert mode."
  (interactive)
  (evil-paste-last-insertion)
  (evil-normal-state))

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
  ;; Org-agenda
  "a" 'org-agenda
  ;; Bookmark shortcuts
  "bj" 'bookmark-jump
  "b4" 'bookmark-jump-other-window
  "bl" 'list-bookmarks
  "bs" 'bookmark-set
  ;; Dired stuff
  "dj" 'dired-jump
  "d4j" 'dired-jump-other-window
  ;; Open frequently visited files
  "fe" 'my-find-evilrc-file
  "fm" 'jump-to-messages-buffer
  "fi" 'my-find-init-el-file
  "fo" 'my-find-init-org-file
  "fs" 'jump-to-scratch-buffer
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
  "i" 'imenu
  ;; Store links in Org.
  "j" 'jump-to-register
  "l" 'org-store-link
  ;; Projectile prefix
  "p" 'projectile-command-map
  "s" 'save-buffer
  ;; These are custom functions to copy text into GFM code blocks
  "yd" 'auralcat/prog-copy-region-in-diff-gfm-code-block
  "yp" 'auralcat/prog-copy-region-in-plain-md-code-block
  "ys" 'auralcat/prog-copy-region-in-suggestion-gfm-code-block
  "yy" 'auralcat/prog-copy-region-in-named-gfm-code-block
  "/" 'git-link)

;; Set Emacs state as initial state for a handful of modes.
;; This includes Dired, Xref, shell-related modes and list modes.
(evil-set-initial-state 'tabulated-list-mode 'emacs)
(evil-set-initial-state 'comint-mode 'emacs)
(evil-set-initial-state 'compilation-mode 'emacs)
(evil-set-initial-state 'special-mode 'emacs)
