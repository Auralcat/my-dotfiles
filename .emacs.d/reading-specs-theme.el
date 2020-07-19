(deftheme reading-specs
  "Theme intended to be put over color themes.
   Adds custom fonts.
   Created 2019-06-11.")

(custom-theme-set-faces
  'reading-specs
  '(variable-pitch ((t (:weight normal :height 100 :family "Open Sans"))))
  '(default ((t (:family "Courier 10 Pitch" :height 100))))
  '(org-date ((t (:inherit fixed-pitch))))
  '(org-table ((t (:inherit fixed-pitch))))
  '(org-src ((t (:inherit fixed-pitch))))

  '(outline-1  ((t  (:font-weight 600 :height  180 :family "B612"))))
  '(outline-2  ((t  (:height  160 :inherit outline-1))))
  '(outline-3  ((t  (:height  140 :inherit outline-1))))
  '(outline-4  ((t  (:height  120 :inherit outline-1))))

  '(org-level-1  ((t  (:inherit outline-1))))
  '(org-level-2  ((t  (:inherit outline-2))))
  '(org-level-3  ((t  (:inherit outline-3))))
  '(org-level-4  ((t  (:inherit outline-4)))))

(provide-theme 'reading-specs)
