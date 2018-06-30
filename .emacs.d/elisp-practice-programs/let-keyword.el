;; First example
(let ((zebra "stripes")
      (tiger "fierce"))
    (message "One kind of animal has %s and another is %s." zebra tiger))

(let ((i 1))
    (while (< i 20)
        (message "I'm counting to %s" i)
        (setq i (1+ i))))
