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

;; (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit
