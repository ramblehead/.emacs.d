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

   ;; ;; /b/; Emacs defaults
   ;; ;; /b/{

   ;; '(vterm-color-black
   ;;   ((t . (:foreground "black" :background "black"))))
   ;; '(vterm-color-red
   ;;   ((t . (:foreground "red3" :background "red3"))))
   ;; '(vterm-color-green
   ;;   ((t . (:foreground "green3" :background "green3"))))
   ;; '(vterm-color-yellow
   ;;   ((t . (:foreground "yellow3" :background "yellow3"))))
   ;; `(vterm-color-blue
   ;;   ((t . (:foreground "blue2" :background "blue2"))))
   ;; '(vterm-color-magenta
   ;;   ((t . (:foreground "magenta3" :background "magenta3"))))
   ;; '(vterm-color-cyan
   ;;   ((t . (:foreground "cyan3" :background "cyan3"))))
   ;; '(vterm-color-white
   ;;   ((t . (:foreground "grey90" :background "gray90"))))

   ;; '(vterm-color-bright-black
   ;;   ((t . (:foreground "gray30" :background "gray30"))))
   ;; '(vterm-color-bright-red
   ;;   ((t . (:foreground "red2" :background "red2"))))
   ;; '(vterm-color-bright-green
   ;;   ((t . (:foreground "green2" :background "green2"))))
   ;; '(vterm-color-bright-yellow
   ;;   ((t . (:foreground "yellow2" :background "yellow2"))))
   ;; `(vterm-color-bright-blue
   ;;   ((t . (:foreground "blue1" :background "blue1"))))
   ;; '(vterm-color-bright-magenta
   ;;   ((t . (:foreground "magenta2" :background "magenta2"))))
   ;; '(vterm-color-bright-cyan
   ;;   ((t . (:foreground "cyan2" :background "cyan2"))))
   ;; '(vterm-color-bright-white
   ;;   ((t . (:foreground "white" :background "white"))))

   ;; ;; /b/}

   ;; ;; /b/; ChatGPT version with my mods (original colors are commented-out)
   ;; ;; /b/{

   (custom-set-faces
    '(vterm-color-black
      ((t . (:foreground "#3a3a3a" :background "#232323"))))
    '(vterm-color-red
      ((t . (:foreground "#cc6666" :background "#752525"))))
      ;; ((t . (:foreground "#bf4040" :background "#752525"))))
    '(vterm-color-green
      ((t . (:foreground "#81db26" :background "#4b692f"))))
      ;; ((t . (:foreground "#67b11d" :background "#4b692f"))))
    '(vterm-color-yellow
      ((t . (:foreground "#ccaa22" :background "#907f4b"))))
    '(vterm-color-blue
      ((t . (:foreground "#0051b9" :background "#002f6b"))))
      ;; ((t . (:foreground "#3e82f7" :background "#2b4f91"))))
    '(vterm-color-magenta
      ((t . (:foreground "#c0a9e7" :background "#6b4d99"))))
      ;; ((t . (:foreground "#956dd6" :background "#6b4d99"))))
    '(vterm-color-cyan
      ((t . (:foreground "#3dc9b0" :background "#329b86"))))
    '(vterm-color-white
      ((t . (:foreground "#c6c6c6" :background "#9e9e9e"))))

    '(vterm-color-bright-black
      ((t . (:foreground "#666666" :background "#4d4d4d"))))
    '(vterm-color-bright-red
      ((t . (:foreground "#ff9898" :background "#a72929"))))
      ;; ((t . (:foreground "#ff4b4b" :background "#a72929"))))
    '(vterm-color-bright-green
      ((t . (:foreground "#9bcd4b" :background "#7b8e42"))))
    '(vterm-color-bright-yellow
      ((t . (:foreground "#fffb1a" :background "#cccc00"))))
      ;; ((t . (:foreground "#e7c547" :background "#b3a547"))))
    '(vterm-color-bright-blue
      ((t . (:foreground "#5da5e8" :background "#4387b0"))))
    '(vterm-color-bright-magenta
      ((t . (:foreground "#b88ae2" :background "#9179b0"))))
    '(vterm-color-bright-cyan
      ((t . (:foreground "#7adec9" :background "#56b8a2"))))
    '(vterm-color-bright-white
      ((t . (:foreground "#ffffff" :background "#cccccc")))))

   ;; /b/}

   '(vterm-color-underline
     ((t . (:underline t))))
   '(vterm-color-inverse-video
     ((t . (:background "#000d1e" :inverse-video t)))))

  (custom-theme-set-variables
   'rh-sanityinc-tomorrow-blue
   ;; Customize theme variables if needed
   ))

(provide-theme 'rh-sanityinc-tomorrow-blue)
