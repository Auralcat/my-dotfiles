;; We need a way to differentiate when you press C-d to logout of the shell and pressing C-d
(if (process-live-p (get-buffer-process (current-buffer)))
    ;; Kill the process, buffer and window.
    ;; If process is not alive, just run usual kill-buffer-and-window wrapper you wrote.
    )
