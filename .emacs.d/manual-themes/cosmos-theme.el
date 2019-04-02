(deftheme cosmos
    "Inspired by the Astro design system. - Created 2019-01-27.")

(let (
         (earth-100   "#b8fcac")
         (earth-200   "#9cf184")
         (earth-300   "#88e967")
         (earth-400   "#74e24a")
         (earth-500   "#56b53f")
         (earth-600   "#469d39")
         (earth-700   "#388833")
         (earth-800   "#276f2d")
         (earth-900   "#145327")
         (earth-1000  "#084222")
         (mars-100    "#ffcace")
         (mars-200    "#f6b1b5")
         (mars-300    "#ea9297")
         (mars-400    "#dd676b")
         (mars-500    "#d45459")
         (mars-600    "#ac3c40")
         (mars-700    "#9a3135")
         (mars-800    "#7b1e22")
         (mars-900    "#651115")
         (mars-1000   "#500508")
         (moon-100    "#f6f7f8")
         (moon-200    "#e9eef2")
         (moon-300    "#afbec9")
         (moon-400    "#8296a4")
         (moon-500    "#597183")
         (moon-600    "#495c69")
         (moon-700    "#3f4c55")
         (moon-800    "#384248")
         (moon-900    "#32383c")
         (moon-1000   "#1c2124")
         (space-100   "#fff")
         (space-200   "#f5f5f5")
         (space-300   "#e5e5e5")
         (space-400   "#c5c5c5")
         (space-500   "#a5a5a5")
         (space-600   "#858585")
         (space-700   "#656565")
         (space-800   "#454545")
         (space-900   "#353535")
         (space-1000  "#202020")
         (sun-100     "#fffec1")
         (sun-200     "#fbee88")
         (sun-300     "#f7e05b")
         (sun-400     "#f4d330")
         (sun-500     "#f1c500")
         (sun-600     "#cea800")
         (sun-700     "#b19100")
         (sun-800     "#8f7600")
         (sun-900     "#685500")
         (sun-1000    "#4b3e00")
         (uranus-100  "#a6f7ff")
         (uranus-200  "#86e3f9")
         (uranus-300  "#67cff3")
         (uranus-400  "#3db5eb")
         (uranus-500  "#159ce4")
         (uranus-600  "#147ec1")
         (uranus-700  "#1663a0")
         (uranus-800  "#16518b")
         (uranus-900  "#183a6e")
         (uranus-1000 "#182758")
         (venus-100   "#ffc3f9")
         (venus-200   "#f0a0e9")
         (venus-300   "#e78ade")
         (venus-400   "#e078d6")
         (venus-500   "#c763ba")
         (venus-600   "#ab4b9a")
         (venus-700   "#993c85")
         (venus-800   "#842a6d")
         (venus-900   "#6b1550")
         )

    (custom-theme-set-faces
        `cosmos

        ;; Default face.
        `(default ((t (:inherit nil :stipple nil :background ,moon-200 :foreground ,moon-900 :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Roboto Mono"))))
        `(cursor ((t (:background ,uranus-600))))
        `(fixed-pitch ((t (:family "Monospace"))))
        `(variable-pitch ((((type w32)) (:foundry "outline" :family "Arial")) (t (:family "Sans Serif"))))
        `(escape-glyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
        `(homoglyph ((((background dark)) (:foreground "cyan")) (((type pc)) (:foreground "magenta")) (t (:foreground "brown"))))
        `(minibuffer-prompt ((t (:foreground ,uranus-600))))
        `(highlight ((t (:background ,earth-300 :foreground ,uranus-800))))
        `(region ((t (:background ,sun-200))))
        `(shadow ((t (:foreground ,space-500))))
        `(secondary-selection ((t (:background ,earth-100))))
        `(trailing-whitespace ((t (:background ,mars-600))))

        ;; Font-lock mode
        `(font-lock-builtin-face               ((t (:foreground  ,venus-500 :slant italic))))
        `(font-lock-comment-delimiter-face     ((t (:inherit     (font-lock-comment-face)))))
        `(font-lock-comment-face               ((t (:foreground  ,space-500   :slant italic))))
        `(font-lock-constant-face              ((t (:foreground  ,earth-500))))
        `(font-lock-doc-face                   ((t (:foreground  ,earth-600 :inherit font-lock-comment-face))))
        `(font-lock-function-name-face         ((t (:foreground  ,uranus-500))))
        `(font-lock-keyword-face               ((t (:foreground  ,mars-600))))
        `(font-lock-negation-char-face         ((t nil)))
        `(font-lock-preprocessor-face          ((t (:inherit     (font-lock-builtin-face)))))
        `(font-lock-regexp-grouping-backslash  ((t (:foreground  ,sun-600   :inherit bold))))
        `(font-lock-regexp-grouping-construct  ((t (:foreground  ,sun-600   :inherit bold))))
        `(font-lock-string-face                ((t (:foreground  ,sun-800))))
        `(font-lock-type-face                  ((t (:foreground  ,venus-500 :slant italic))))
        `(font-lock-variable-name-face         ((t (:foreground  ,mars-500  :slant italic))))
        `(font-lock-warning-face               ((t (:foreground  ,mars-600))))

        ;; Searching
        `(isearch                              ((t (:foreground  ,uranus-600 :background ,venus-300))))
        `(isearch-fail                         ((t (:foreground  ,space-600 :background ,mars-300))))

        ;; Linum-mode
        `(line-number ((t (:inherit shadow))))
        `(line-number-current-line ((t (:foreground ,moon-600 :background "#b8fcac":inherit line-number))))

        ;; Rainbow delimiters
        `(rainbow-delimiters-base-face        ((t  (:foreground  ,venus-700))))
        `(rainbow-delimiters-depth-1-face     ((t  (:foreground  ,mars-600))))
        `(rainbow-delimiters-depth-2-face     ((t  (:foreground  ,sun-600))))
        `(rainbow-delimiters-depth-3-face     ((t  (:foreground  ,earth-600))))
        `(rainbow-delimiters-depth-4-face     ((t  (:foreground  ,uranus-600))))
        `(rainbow-delimiters-depth-5-face     ((t  (:foreground  ,uranus-800))))
        `(rainbow-delimiters-depth-6-face     ((t  (:foreground  ,venus-600))))
        `(rainbow-delimiters-mismatched-face  ((t  (:foreground  ,venus-800))))
        `(rainbow-delimiters-unmatched-face   ((t  (:foreground  ,venus-800))))

        ;; Highlight-numbers-mode
        `(highlight-numbers-number ((t (:foreground ,earth-500))))

        ;; Org-mode faces
        `(org-level-1  ((t  (:foreground ,uranus-500 :family  "Poppins" :height  220))))
        `(org-level-2  ((t  (:foreground ,venus-500  :inherit org-level-1   :height  200))))
        `(org-level-3  ((t  (:foreground ,earth-600  :inherit org-level-1   :height  180))))
        `(org-level-4  ((t  (:foreground ,mars-500   :inherit org-level-1   :height  160))))
        `(org-document-info-keyword    ((t  (:foreground ,moon-400))))
        `(org-block    ((t  (:foreground ,moon-700))))
        `(org-code     ((t  (:foreground ,sun-600))))
        `(org-date     ((t  (:underline  t           :foreground  ,venus-700))))
        `(org-table    ((t  (:foreground ,uranus-600))))
        `(org-tag    ((t  (:foreground ,uranus-600))))

        ;; Todo faces
        `(org-todo ((t (:box t :bold t :background ,mars-100 :foreground ,mars-500 :family "Poppins" :height 160))))
        `(org-done ((t (:background ,earth-100 :foreground ,earth-500 :inherit org-todo))))

        ;; Agenda faces
        `(org-agenda-calendar-event  ((t  (:bold t :foreground ,uranus-500))))
        `(org-agenda-calendar-sexp   ((t  (:bold t :foreground ,sun-700))))
        `(org-agenda-structure       ((t  (:foreground ,uranus-600))))
        `(org-agenda-date            ((t  (:bold t :foreground ,uranus-600 :family "Poppins" :height 180))))
        `(org-agenda-date-today      ((t  (:foreground  ,venus-400      :inherit     org-agenda-date))))
        `(org-agenda-date-weekend    ((t  (:foreground  ,earth-500      :inherit     org-agenda-date))))
        `(org-scheduled-previously   ((t  (:foreground  ,mars-800))))
        `(org-upcoming-deadline      ((t  (:bold        t               :foreground  ,mars-500))))
        `(org-scheduled-today        ((t  (:foreground  ,earth-700))))
        `(org-scheduled              ((t  (:foreground  ,venus-700))))

        ;; Enh-ruby-mode
        `(enh-ruby-op-face                ((t  (:foreground  ,uranus-600))))
        `(enh-ruby-regexp-face            ((t  (:foreground  ,sun-700))))
        `(enh-ruby-regexp-delimiter-face  ((t  (:foreground  ,sun-700))))
        `(enh-ruby-string-delimiter-face  ((t  (:foreground  ,sun-900))))

        ;; Js2-mode
        `(js2-function-call               ((t  (:foreground  ,uranus-700))))
        `(js2-function-param              ((t  (:foreground  ,earth-700))))

        ;; Web-mode
        `(web-mode-html-tag-face         ((t  (:foreground  ,venus-500))))
        `(web-mode-html-attr-name-face   ((t  (:foreground  ,uranus-600))))
        `(web-mode-html-attr-value-face  ((t  (:foreground  ,earth-700))))
        `(web-mode-block-delimiter-face  ((t  (:foreground  ,venus-700))))
        `(web-mode-current-element-highlight-face  ((t  (:foreground  ,venus-800 :background ,earth-100))))

        ;; Company-mode
        `(company-echo-common    ((t  (:foreground  ,mars-900))))
        `(company-box-selection  ((t  (:background  ,sun-300))))

        ;; Slack client faces
        `(slack-preview-face    ((t  (:background ,sun-100 :inherit org-code))))
        `(slack-message-output-text    ((t  (:family "Lato" :height 140 :inherit default))))
        `(slack-message-output-header    ((t  (:family "Poppins" :height 160 :bold t :underline t :foreground ,mars-500))))

        ;; Helm faces
        `(helm-locate-finish ((t  (:foreground  ,earth-600))))
        `(helm-grep-finish ((t  (:inherit helm-locate-finish))))

        ;; Eshell/terminal faces
        `(eshell-ls-directory ((t  (:foreground  ,venus-500 :bold t))))

        ;; Telephone-line Evil faces
        ;; `(telephone-line-evil-emacs ((t (:foreground ,venus-600 :inherit telephone-line-evil))))

        ;; Extra stuff
        `(button ((t (:inherit (link)))))
        `(link ((((class color) (min-colors 88) (background light)) (:underline (:color foreground-color :style line) :foreground ,uranus-700)) (((class color) (background light)) (:underline (:color foreground-color :style line) :foreground "blue")) (((class color) (min-colors 88) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan1")) (((class color) (background dark)) (:underline (:color foreground-color :style line) :foreground "cyan")) (t (:inherit (underline)))))
        `(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground ,uranus-800)) (((class color) (background dark)) (:foreground "violet"))))
        `(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
        `(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline (:color foreground-color :style line) :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "grey20" :background "grey90")) (((class color grayscale) (background dark)) (:box nil :foreground "grey90" :background "grey20")) (((class mono) (background light)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "black" :background "white")) (((class mono) (background dark)) (:underline (:color foreground-color :style line) :box nil :inverse-video nil :foreground "white" :background "black"))))
        `(tooltip ((((class color)) (:inherit (variable-pitch) :foreground "black" :background "lightyellow")) (t (:inherit (variable-pitch)))))
        `(mode-line ((t (:background ,space-300 :box nil))))
        `(mode-line-buffer-id ((t (:weight bold))))
        `(mode-line-emphasis ((t (:foreground ,uranus-400))))
        `(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "grey40" :style released-button))) (t (:inherit (highlight)))))
        `(mode-line-inactive ((t (:background ,space-300 :foreground ,space-600 :box nil))))
        `(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
        `(isearch-fail ((t (:background ,mars-400 :foreground "#FFFBF0" :weight bold))))
        `(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
        `(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
        `(next-error ((t (:inherit (region)))))
        `(query-replace ((t (:inherit (isearch)))))))

(provide-theme `cosmos)
