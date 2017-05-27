;; Emacs configuration file
;; Author: Auralcat
;; Started in May 2017.

;;-----KEYBINDINGS--------------------------------------------------------------
;; Remapping the help hotkey so it doesn't clash with Unix backspace.
;; Whenever you want to call help you can use M-x help as well. F1 works too.
(keyboard-translate ?\C-h ?\C-?)

;; Remove the menu bar in terminal mode
(when (not (display-graphic-p))
    (menu-bar-mode -1))

;;-----VARIABLES----------------------------------------------------------------
;; Set Text mode as default mode for new buffers:
(setq-default major-mode 'text-mode')

;; Set default fill to 79
(set-fill-column 79)

;; Set line number mode and column number mode for code files
(line-number-mode 1)
(column-number-mode 1)

;; Change tab width and change tabs to spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Shows trailing whitespace, if any:
(setq-default show-trailing-whitespace t)

;; Python indentation
(setq python-indent 4)
