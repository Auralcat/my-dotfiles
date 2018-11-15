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
;; Add C-a and C-e to insert mode
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
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

;; Evil Leader configs
;; Use SPC as leader key (yah, this is the idea behind Spacemacs)
(evil-leader/set-leader "SPC")

;; Global bindings go here
(evil-leader/set-key
    ;; Org-agenda
    "a" 'org-agenda
    ;; Too lazy to type C-c C-t d
    "d" 'my-org-mark-as-done
    ;; Call Magit with Leader g
    "g" 'magit-status
    ;; Clock in
    "i" 'org-clock-in
    ;; Helm prefix
    "h" 'helm-command-prefix
    ;; Use Ace-Jump with Leader j
    "j" 'evil-ace-jump-char-mode
    ;; Show key frequencies
    "k" 'keyfreq-show
    ;; Clock out
    "o" 'org-clock-out
    ;; Projectile prefix
    "p" 'projectile-command-map
    ;; Projectile-Rails prefix
    "r" 'projectile-rails-command-map)
