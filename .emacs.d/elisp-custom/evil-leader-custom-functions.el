;;; evil-leader-custom-functions.el -- Custom functions to be called with evil-leader
;;
;;; Commentary:
;; Houses the functions I associate to evil-leader keybindings.
;; This is so I know what's going on when I ask for help from Emacs.
;;
;;; Code:
(require 's)

(defun my-find-budget-file ()
    "Jump to the budget file."
    (interactive)
    (find-file my-budget-file-path))

(defun my-find-finances-log-file ()
    "Jump to the finances log file."
    (interactive)
    (find-file my-finances-log-file-path))

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

(defun my-find-daily-todos-org-file ()
    "Jump to my daily TODO's Org file."
    (interactive)
    (find-file auralcat-org-daily-todos-file-path))

(defun jump-to-messages-buffer ()
    "Jump to Emacs message buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))

(defun jump-to-scratch-buffer ()
    "Jump to Emacs scratch buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))

;; Work related shortcuts
(defun my-open-ci-current-build ()
    "Open the page for the build in the current branch in the continuous integration service."
    (interactive)
    ;; I'm not refactoring this right now. Later!
    (browse-url (my-get-ci-build-url))
    )

(defun my-get-ci-build-url ()
    "Return the generated CI build url."
    (concat auralcat-work-project-ci-url
        (s-join "/" (list auralcat-work-organization-name
                      (projectile-project-name)
                        "branches"
                        (s-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))))
    )

(defun my-find-work-org-file ()
    "Open the kanban Org file for work."
    (interactive)
    (find-file auralcat-org-work-file-path)
    )

(defun my-goto-staging-server-url ()
    "Open the URL for your staging server at work."
    (interactive)
    (browse-url auralcat-work-staging-server-url)
    )

(defun my-goto-local-server-url ()
    "Open the URL for the local development server."
    (interactive)
    (browse-url "http://localhost:3000")
    )

(defun my-goto-work-github-repo-url ()
    "Open the project's GitHub repo page in the browser."
    (interactive)
    (browse-url (concat "https://github.com/magnetis/" (projectile-project-name)))
    )

(provide 'evil-leader-custom-functions)
;;; evil-leader-custom-functions.el ends here
