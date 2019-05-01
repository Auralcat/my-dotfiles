(require 'projectile)

;; Reloading purposes
(remove-hook 'projectile-after-switch-project-hook #'projectile-watch-for-docker-sync-presence)

;; Util function to get file path from project
(defun projectile-relative-file-path (filename)
    "Return the relative path for a given FILENAME in the current project."
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
        (expand-file-name filename))
    )

(defun docker-sync-running-p ()
    "Check if docker-sync is running for the current project."
    (and (projectile-project-p)
        (file-exists-p (projectile-relative-file-path ".docker-sync/daemon.pid"))
        (file-exists-p (projectile-relative-file-path  "docker-sync.yml")))
    )

(defun projectile-watch-for-docker-sync-presence ()
    "Check if the current project has docker-sync in it."
    (if (string-equal (projectile-default-project-name (projectile-project-root)) auralcat-work-project-name)
        (projectile-start-docker-sync-restart-script)
        'projectile-kill-docker-sync
        )
    )

(defun projectile-start-docker-sync-restart-script ()
    "Start the docker-sync restart script."
    (message "Loading Docker-sync restart script...")
    (when (not (docker-sync-running-p))
        (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
            (start-process "restart-docker-sync" "docker-sync restart log" (projectile-relative-file-path "restart-docker-sync.sh")))
        ))

(defun projectile-kill-docker-sync ()
    "Kills the docker-sync process when closing the project."
    (message "Cleaning docker-sync script from project...")
    (if (process-live-p "restart-docker-sync")
        (stop-process "restart-docker-sync"))
    (if (projectile-project-p)
        (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
            (shell-command "docker-sync stop"))
        (shell-command "docker-sync-stop"))
    )

(add-hook 'projectile-after-switch-project-hook #'projectile-watch-for-docker-sync-presence)

(provide 'docker-sync-utils)
