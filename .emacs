;; <-- This is how you comment things on your Emacs config file

;; Remapping the help hotkey so it doesn't clash with Unix backspace. Whenever you want to call help you can use M-x help as well. F1 works too.
(keyboard-translate ?\C-h ?\C-?)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
    (menu-bar-mode -1))
