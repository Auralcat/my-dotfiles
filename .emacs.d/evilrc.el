;; Porting my .vimrc to Emacs

;; Colorize the cursor depending on current mode.
;; It's one of those small things that you can't live without once you get it.

;; ADDENDUM: You can get the color of a face like this:
;; (color-values (face-foreground 'face-name))
;; Setting the color through faces blends well with each theme, but if you hover over the
;; face it gets the color from, it defeats the whole purpose of this.
(setq evil-emacs-state-cursor '("purple" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("gold" hbar))
;; This is when you use commands such as c,d,y in normal mode, for example.
(setq evil-operator-state-cursor '("red" hollow))

;; I have bound C-a to back-to-indentation-or-beginning in another config,
;; Now I need C-e, $ is too far for me 😂
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

;; Some buffers in Emacs are read-only and can be usually killed pressing
;; lowercase q. Much more convenient if you ask me.
;; Use Q in normal mode to kill current buffer.
(define-key evil-normal-state-map (kbd "Q") 'kill-current-buffer)

;; Use gb as goto-bookmark
(define-key evil-normal-state-map (kbd "gb") 'bookmark-jump)

;; Use gl as list-bookmarks
(define-key evil-normal-state-map (kbd "gl") 'list-bookmarks)

;; Use gs to set bookmarks
(define-key evil-normal-state-map (kbd "gs") 'bookmark-set)

;; INSERT MODE
;; Use C-g to exit from Normal mode as well.
(define-key evil-insert-state-map (kbd "C-g") 'evil-force-normal-state)

;; Add C-a and C-e to insert mode
(define-key evil-insert-state-map (kbd "C-a") 'back-to-indentation-or-beginning)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

;; Use C-t to transpose chars, I can use tab in graphical mode
(define-key evil-insert-state-map (kbd "C-t") 'transpose-chars)

;; Use C-w to kill region. I don't use C-w to delete backward words anymore.
(define-key evil-insert-state-map (kbd "C-w") 'completion-kill-region)

;; Use C-y to paste text, just like Emacs mode
;; The previous command was copy-line-from-above, not that useful IMO
(define-key evil-insert-state-map (kbd "C-y") 'yank)

;; Use C-. to paste last insertion.
;; This command is originally bound to C-a, but I overrode it.
(define-key evil-insert-state-map (kbd "C-.") 'evil-paste-last-insertion)

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

;; Evil Leader configs
;; Use SPC as leader key (yah, this is the idea behind Spacemacs)
(evil-leader/set-leader "SPC")

;; Global bindings go here
(evil-leader/set-key
    ;; Org-agenda
    "a" 'org-agenda
    ;; Org-capture
    "c" 'org-capture
    ;; Get the relative file name of current buffer. I pass that to rspec.
    "f" 'auralcat-kill-relative-file-name
    ;; Call Magit with Leader g
    "g" 'magit-status
    ;; Clock in
    "i" 'org-clock-in
    ;; Helm prefix
    "h" 'helm-command-prefix
    ;; Go to current or last task
    "j" 'org-clock-goto
    ;; Show key frequencies
    "k" 'keyfreq-show
    ;; Clock out
    "o" 'org-clock-out
    ;; Projectile prefix
    "p" 'projectile-command-map
    ;; Projectile-Rails prefix
    "r" 'projectile-rails-command-map
    ;; TODO: map this to org-mode locally.
    "t" 'org-todo)
