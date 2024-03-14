;;; ramblehead's default theme customisations

(deftheme rh-default-customisations)

(custom-set-faces
 '(iedit-occurrence
   ((((background light)) (:background "deep sky blue"))))
 '(iedit-read-only-occurrence
   ((((background light)) (:background "pale turquoise")))))

(let ((class '((class color) (background light))))
  (custom-theme-set-faces
   'rh-default-customisations
   `(region ((,class . (:box
                        (:line-width
                         (-1 . -1)
                         :color "#ea5e30"
                         :style nil)
                        :background "#ea5e30"))))
   `(iedit-occurrence
     ((,class . (:background "deep sky blue"))))
   `(iedit-read-only-occurrence
     ((,class . (:background "pale turquoise")))))
  (custom-theme-set-variables
   'rh-default-customisations
   ;; Customize theme variables if needed
   ))

(provide-theme 'rh-default-customisations)
