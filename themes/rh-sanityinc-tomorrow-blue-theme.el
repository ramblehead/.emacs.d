;;; ramblehead's rh-sanityinc-tomorrow-blue theme

(require 'color-theme-sanityinc-tomorrow)

(deftheme rh-sanityinc-tomorrow-blue)

(let ((colors (cdr (assoc 'blue color-theme-sanityinc-tomorrow-colors)))
      (class '((class color) (background dark))))
  (custom-theme-set-faces
   'rh-sanityinc-tomorrow-blue
   ;; Customize the desired faces here
   `(region ((,class . (:box
                        (:line-width
                         (-2 . -2)
                         :color ,(cdr (assoc 'selection colors))
                         :style nil)
                        :background ,(cdr (assoc 'selection colors))))))
   `(iedit-occurrence
     ((,class . (:background "dark blue"))))
   `(iedit-read-only-occurrence
     ((,class . (:background "dark slate blue"))))

   `(highlight-indent-guides-character-face
     ((,class . (:foreground ,(cdr (assoc 'selection colors))))))
   `(highlight-indent-guides-top-character-face
     ((,class . (:foreground ,(cdr (assoc 'comment colors))))))

   `(highlight-indent-guides-odd-face
     ((,class . (:background ,(cdr (assoc 'alt-background colors))))))
   `(highlight-indent-guides-even-face
     ((,class . (:background ,(cdr (assoc 'current-line colors))))))
   `(highlight-indent-guides-top-odd-face
     ((,class . (:background ,(cdr (assoc 'selection colors))))))
   `(highlight-indent-guides-top-even-face
     ((,class . (:background ,(cdr (assoc 'selection colors)))))))
  (custom-theme-set-variables
   'rh-sanityinc-tomorrow-blue
   ;; Customize theme variables if needed
   ))

(provide-theme 'rh-sanityinc-tomorrow-blue)
