;;; ramblehead's custom-tomorrow-blue theme

(deftheme custom-tomorrow-blue)

(let ((colors (cdr (assoc 'blue color-theme-sanityinc-tomorrow-colors))))
  (custom-theme-set-faces
   'custom-tomorrow-blue
   ;; Customize the desired faces here
   '(iedit-occurrence ((t (:background "dark blue"))))
   '(iedit-read-only-occurrence ((t (:background "dark slate blue")))))
  (custom-theme-set-variables
   'custom-tomorrow-blue
   ;; Customize theme variables if needed
   ))

(provide-theme 'custom-tomorrow-blue)
