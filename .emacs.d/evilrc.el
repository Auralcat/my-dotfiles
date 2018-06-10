;; Porting my .vimrc to Emacs

;; Quicker window navigation
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; I have bound C-a to back-to-indentation-or-beginning in another config,
;; Now I need C-e, $ is too far for me 😂
(define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

;; Add C-a and C-e to insert mode
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)

;; Use Q in normal mode to kill buffers (since there's no EX mode)
(define-key evil-normal-state-map (kbd "Q") 'kill-buffer)

;; Some buffers in Emacs are read-only and can be usually killed pressing
;; lowercase q. Much more convenient if you ask me.

;; Use gb as goto-bookmark
(define-key evil-normal-state-map (kbd "gb") 'bookmark-jump)

;; Use gl as list-bookmarks
(define-key evil-normal-state-map (kbd "gl") 'list-bookmarks)

;; Use gs to set bookmarks
(define-key evil-normal-state-map (kbd "gs") 'bookmark-set)

;; Evil Leader configs
;; Use SPC as leader key (yah, this is the idea behind Spacemacs)
(evil-leader/set-leader "SPC")

;; Bindings go here
(evil-leader/set-key
    ;; Use Ace-Jump with Leader j
    "j" 'evil-ace-jump-char-mode
    ;; Call Magit with Leader g
    "g" 'magit-status)
