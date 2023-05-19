;;; ramblehead's rh-sanityinc-tomorrow-blue theme

;; (require 'color-theme-sanityinc-tomorrow)

;; (color-theme-sanityinc-tomorrow-blue)
;; (disable-theme 'sanityinc-tomorrow-blue)

(deftheme rh-sanityinc-tomorrow-blue)

(let ((colors (cdr (assoc 'blue color-theme-sanityinc-tomorrow-colors))))
  (custom-theme-set-faces
   'rh-sanityinc-tomorrow-blue
   ;; Customize the desired faces here
   `(region ((t . (:box
                   (:line-width
                    (-2 . -2)
                    :color ,(cdr (assoc 'selection colors))
                    :style nil)
                   :background ,(cdr (assoc 'selection colors))))))
   '(iedit-occurrence ((((class color) (min-colors 88) (background dark)) . (:background "dark blue"))
		       (((class color) (min-colors 88) (background light)) . (:background "deep sky blue"))))
   '(iedit-read-only-occurrence ((t . (:background "dark slate blue")))))
  (custom-theme-set-variables
   'rh-sanityinc-tomorrow-blue
   ;; Customize theme variables if needed
   ))

(provide-theme 'rh-sanityinc-tomorrow-blue)
