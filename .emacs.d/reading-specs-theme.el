(deftheme reading-specs
  "Theme intended to be put over color themes.
   Adds custom fonts.
   Created 2019-06-11.")

(custom-theme-set-faces
 'reading-specs
 '(variable-pitch ((t (:family "Ubuntu"))))
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(org-date ((t (:inherit fixed-pitch))))
 '(org-table ((t (:inherit fixed-pitch)))))

(provide-theme 'reading-specs)
