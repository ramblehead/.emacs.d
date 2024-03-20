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
                         ;; :color ,(cdr (assoc 'aqua colors))
                         :color "#3b2000" ;; "#001938" "#512d00"
                         :style nil)
                        :background "#3b2000"))))
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
     ((,class . (:background ,(cdr (assoc 'selection colors))))))

   `(consult-preview-insertion
     ((,class . (:background ,(cdr (assoc 'selection colors))))))

   ;; (term (:foreground unspecified :background unspecified :inherit default))
   ;; '(term-color-black ((t . (:foreground "black" :background "black"))))
   ;; '(term-color-red ((t . (:foreground "red3" :background "red3"))))
   ;; '(term-color-green ((t . (:foreground "green3" :background "green3"))))
   ;; '(term-color-yellow ((t . (:foreground "yellow3" :background "yellow3"))))
   ;; '(term-color-blue ((t . (:foreground "blue2" :background "blue2"))))
   ;; '(term-color-magenta ((t . (:foreground "magenta3" :background "magenta3"))))
   ;; '(term-color-cyan ((t . (:foreground "cyan3" :background "cyan3"))))
   ;; '(term-color-white ((t . (:foreground "grey90" :background "gray90"))))

   '(vterm-color-black
     ((t . (:foreground "black" :background "black"))))
   '(vterm-color-red
     ((t . (:foreground "red3" :background "red3"))))
   '(vterm-color-green
     ((t . (:foreground "green3" :background "green3"))))
   '(vterm-color-yellow
     ((t . (:foreground "yellow3" :background "yellow3"))))
   `(vterm-color-blue
     ((t . (:foreground "blue2" :background "blue2"))))
   '(vterm-color-magenta
     ((t . (:foreground "magenta3" :background "magenta3"))))
   '(vterm-color-cyan
     ((t . (:foreground "cyan3" :background "cyan3"))))
   '(vterm-color-white
     ((t . (:foreground "grey90" :background "gray90"))))

   '(vterm-color-bright-black
     ((t . (:foreground "gray30" :background "gray30"))))
   '(vterm-color-bright-red
     ((t . (:foreground "red2" :background "red2"))))
   '(vterm-color-bright-green
     ((t . (:foreground "green2" :background "green2"))))
   '(vterm-color-bright-yellow
     ((t . (:foreground "yellow2" :background "yellow2"))))
   `(vterm-color-bright-blue
     ((t . (:foreground "blue1" :background "blue1"))))
   '(vterm-color-bright-magenta
     ((t . (:foreground "magenta2" :background "magenta2"))))
   '(vterm-color-bright-cyan
     ((t . (:foreground "cyan2" :background "cyan2"))))
   '(vterm-color-bright-white
     ((t . (:foreground "white" :background "white"))))

   '(vterm-color-underline
     ((t . (:underline t))))
   '(vterm-color-inverse-video
     ((t . (:background "#000d1e" :inverse-video t))))

   ;; '(vterm-color-default (:foreground unspecified :background unspecified :inherit default))
   ;; '(vterm-color-inverse-video (:background ,background :inverse-video t))
   )

  (custom-theme-set-variables
   'rh-sanityinc-tomorrow-blue
   ;; Customize theme variables if needed
   ))

(provide-theme 'rh-sanityinc-tomorrow-blue)
