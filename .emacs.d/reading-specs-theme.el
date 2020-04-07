(deftheme reading-specs
  "Theme intended to be put over color themes.
   Adds custom fonts.
   Created 2019-06-11.")

(custom-theme-set-faces
  'reading-specs
  '(variable-pitch ((t (:weight normal :height 160 :family "Helvetica Neue"))))
  ;; '(default ((t (:family "Inconsolata" :height 140))))
  '(org-date ((t (:inherit fixed-pitch))))
  '(org-table ((t (:inherit fixed-pitch))))
  '(org-src ((t (:inherit fixed-pitch))))

  '(outline-1  ((t  (:font-weight 600 :height  220))))
  '(outline-2  ((t  (:height  200 :inherit outline-1))))
  '(outline-3  ((t  (:height  180 :inherit outline-1))))
  '(outline-4  ((t  (:height  160 :inherit outline-1))))

  '(org-level-1  ((t  (:inherit outline-1))))
  '(org-level-2  ((t  (:inherit outline-2))))
  '(org-level-3  ((t  (:inherit outline-3))))
  '(org-level-4  ((t  (:inherit outline-4)))))

(provide-theme 'reading-specs)
