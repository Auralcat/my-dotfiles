(defun diary:custom-get-doomsday-weekday-number (year)
    "Returns a number representing the weekday of the given year's doomsday"
    (let ((year-digits (% year 1000))
             (modded-year (% (% year 1000) 6)))
        (+ (/ year-digits 12)
            modded-year
            (/ modded-year 4)
            ;; This is the century's anchor value, since I'm interested only in 2000,
            ;; we can leave this value here.
            2)))
