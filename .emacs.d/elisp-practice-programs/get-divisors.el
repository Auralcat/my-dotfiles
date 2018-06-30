;;; get-divisors.el --- Get the list of divisors of a number

;;; Commentary:
;;

;;; Code:

(defun get-divisors (num)
    "Return the set of divisors of a given number.
Argument NUM: A number."
    (let ((count 2) (arr (list)))
    (while (< count (sqrt num))
        (when (eq (% num count) 0)
            (message "%s can be divided by %s" num count)
            (setq arr (append (list (/ num count) count) arr)))
        (setq count (1+ count)))
    (message "vector: %s" arr)))


(provide 'get-divisors)

;;; get-divisors.el ends here
