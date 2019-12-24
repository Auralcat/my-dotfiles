;;; rails-docker-utils --- Utilities for working with Rails apps inside Docker containers.
;;
;;; Commentary:
;; This file stores all the utility functions for working with Ruby on Rails
;; projects which use Docker containers.

;;; Code:

(require 'magit)
(require 'projectile)
(require 's)

;; This can be refactored. But it does the job for now.
(defun auralcat-run-rails-console-in-docker-container ()
    "Open a Rails console inside the Docker container. With C-u prefix, open console in sandbox mode."
    (interactive)
    (if (eq current-prefix-arg '(4))
      (run-ruby "docker-compose run web rails console --sandbox" "Docker Rails Console")
      (run-ruby "docker-compose run web rails console" "Docker Rails Console")
      ))

;; Open a terminal buffer inside the Docker container of the project.
(defun auralcat-run-bash-in-docker-container (args)
    "Open a single buffer with a terminal inside a Docker container."
    (interactive "P")
    (docker-compose-run-action-with-command "run" (quote ("--rm")) "web" "bash"))

;; Run test inside the Docker container in the Rails project using a shell
;; script. I use the rerun gem to keep that process alive.
(defun auralcat-run-local-rails-test-inside-docker-container (args)
    "Run (and rerun if there's a change in the project) the related Rspec test inside a disposable Docker container for the buffer you're working in at the moment."
    (interactive "P")
    (message "Running command: %s" (auralcat-create-docker-compose-test-shell-command))
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
        (async-shell-command (auralcat-create-docker-compose-test-shell-command) "Rails Rspec - Docker Container"))
    )

;; Utility functions
(defun auralcat-create-docker-compose-test-shell-command ()
    "Return the shell command for running the test related to the current file you're working in."
    (let ((shell-command-template (concat "rerun -x -b --no-notify -- docker-compose run web bash "
                                      (auralcat-get-test-shell-script-file-path) " %s" " %s")))
        (if (string-equal (auralcat-get-rails-engine-path) (auralcat-get-test-file-path))
            (format shell-command-template (auralcat-get-test-file-path) "")
            (format shell-command-template (auralcat-get-test-file-path) (auralcat-get-rails-engine-path))
            )
        ))

(defun auralcat-get-rails-engine-path ()
    "Return the engine path of the Rails project you're in."
    (car (s-slice-at "/spec" (magit-file-relative-name buffer-file-name)))
    )

(defun auralcat-get-test-file-path ()
    "Return the test/spec file path."
    (if (string-equal (auralcat-get-rails-engine-path) (magit-file-relative-name buffer-file-name))
        (magit-file-relative-name buffer-file-name)
        ;; This is a small hack to remove the first blank element in the split.
        (s-join "/" (cdr (s-split "/" (cadr (auralcat-engine-and-test-path-list))))))
    )

(defun auralcat-get-test-shell-script-file-path ()
    "Return the path to the shell script file you pass to docker-compose to run all the test."
    (s-join "/" (list "" (projectile-project-name) "local-test.sh"))
    )

(defun auralcat-engine-and-test-path-list ()
    "Return a list containing the Rails engine you're in and the test file from the current buffer."
    (s-slice-at "/spec" (magit-file-relative-name buffer-file-name))
    )

(provide 'rails-docker-utils)
;;; rails-docker-utils.el ends here
