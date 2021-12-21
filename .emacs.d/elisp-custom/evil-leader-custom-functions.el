;;; evil-leader-custom-functions.el -- Custom functions to be called with evil-leader
;;
;;; Commentary:
;; Houses the functions I associate to evil-leader keybindings.
;; This is so I know what's going on when I ask for help from Emacs.
;;
;;; Code:
(defun my-find-evilrc-file ()
    "Jump to the file with Evil-mode run commands."
    (interactive)
    (find-file "~/.emacs.d/evilrc.el"))

(defun my-find-init-el-file ()
    "Jump to init.el file."
    (interactive)
    (find-file "~/.emacs.d/init.el"))

(defun my-find-init-org-file ()
    "Jump to myinit.org file."
    (interactive)
    (find-file "~/.emacs.d/myinit.org"))

(defun jump-to-messages-buffer ()
    "Jump to Emacs message buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))

(defun jump-to-scratch-buffer ()
    "Jump to Emacs scratch buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))

(defun my-find-work-org-file ()
    "Open the kanban Org file for work."
    (interactive)
    (find-file auralcat-org-work-file-path))

;; This function is to kill old windows on buffer kill.
(defun auralcat/quit-and-kill-buffer (args)
  "When there is only one window in the frame, use kill-current-buffer.
   When there are multiple windows in the frame, use kill-buffer-and-window."
  (interactive "P")
  (if (one-window-p)
      (kill-current-buffer)
      (kill-buffer-and-window)))

(provide 'evil-leader-custom-functions)
;;; evil-leader-custom-functions.el ends here
