(defun get-divisors (num)
    "Returns the set of divisors of a given number"
    (let ((count 2) (arr (list)))
    (while (< count (sqrt num))
        (when (eq (% num count) 0)
            (message "%s can be divided by %s" num count)
            (setq arr (cons (list (/ num count) count) arr)))
        (setq count (1+ count)))
    (message "vector: %s" arr)))

(get-divisors 5040)
