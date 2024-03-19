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
     ((t . (:background "#767875" :foreground "#2e3436"))))
   '(vterm-color-red
     ((t . (:background "light coral" :foreground "IndianRed1"))))
   '(vterm-color-green
     ((t . (:background "#94c368" :foreground "medium sea green"))))
   '(vterm-color-yellow
     ((t . (:background "#fce94f" :foreground "#d3ac00"))))
   `(vterm-color-blue
     ;; ((t . (:background ,(cdr (assoc 'blue colors)) :foreground ,(cdr (assoc 'background colors))))))
     ;; ((t . (:background "#88aacf" :foreground "#3465a4"))))
     ((t . (:background ,(cdr (assoc 'blue colors)) :foreground "#002f6b"))))
   '(vterm-color-magenta
     ((t . (:background "#c497bf" :foreground "#a177a8"))))
   '(vterm-color-cyan
     ((t . (:background "#34e2e2" :foreground "#06989a"))))
   '(vterm-color-white
     ((t . (:background "#eeeeec" :foreground "#d3d7cf"))))
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
