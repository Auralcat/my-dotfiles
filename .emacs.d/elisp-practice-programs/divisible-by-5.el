(defun divisible-by-5 (x)
    ;; Checks if a number is divisible by 5
    (setq remainder (% x 5))
    (if (= remainder 0) t nil))

(setq foo 123)
(if (divisible-by-5 foo)
    (message "%s is divisible by 5." foo)
    (message "%s is NOT divisible by 5." foo))

(switch-to-buffer "*Messages*")
