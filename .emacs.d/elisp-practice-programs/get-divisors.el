;;; get-divisors.el --- Get the list of divisors of a number

;;; Commentary:
;; You can lint Elisp programs with M-x checkdoc.

;;; Code:

(defun get-divisors (num)
    "Return the set of divisors of a given number.
Argument NUM: A number."
    (let ((count 2) (arr))
    (while (< count (sqrt num))
        (when (eq (% num count) 0)
            (message "%s can be divided by %s" num count)
            (setq arr (append (list (/ num count) count) arr)))
        (setq count (1+ count)))
    (message "Before sorting: %s" arr)
    ;; The function -sort has NO SIDE EFFECTS, meaning that it doesn't
        ;; alter the original variable. A pure function.
        ;; The (sort) function alters the original var and vanishes with some stuff,
        ;; that's why I didn't use it.
    (setq arr (-sort '< arr))
    (message "List: %s" arr)))

(get-divisors 60)
(provide 'get-divisors)

;;; get-divisors.el ends here
