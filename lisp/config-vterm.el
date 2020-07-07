;;; ramblehead's vterm configuration

(face-spec-set
 'vterm-color-default
 '((((class color) (background light)) .
    (:inherit default))
   (((class color) (background dark)) .
    (:inherit default))))

(face-spec-set
 'vterm-color-inverse-video
 '((((class color) (background light)) .
    (:inherit vterm-color-default))
   (((class color) (background dark)) .
    (:inherit vterm-color-default))))

(face-spec-set
 'term-color-black
 '((((class color) (background light)) .
    (:background "#767875" :foreground "#2E3436"))
   (((class color) (background dark)) .
    (:background "#767875" :foreground "#2E3436"))))

(face-spec-set
 'term-color-blue
 '((((class color) (background light)) .
    (:background "#88AACF" :foreground "#3465A4"))
   (((class color) (background dark)) .
    (:background "#88AACF" :foreground "#3465A4"))))

(face-spec-set
 'term-color-cyan
 '((((class color) (background light)) .
    (:background "#34E2E2" :foreground "#06989A"))
   (((class color) (background dark)) .
    (:background "#34E2E2" :foreground "#06989A"))))

(face-spec-set
 'term-color-green
 '((((class color) (background light)) .
    (:background "#94C368" :foreground "medium sea green"))
   (((class color) (background dark)) .
    (:background "#94C368" :foreground "medium sea green"))))

(face-spec-set
 'term-color-magenta
 '((((class color) (background light)) .
    (:background "#C497BF" :foreground "#A177A8"))
   (((class color) (background dark)) .
    (:background "#C497BF" :foreground "#A177A8"))))

(face-spec-set
 'term-color-red
 '((((class color) (background light)) .
    (:background "light coral" :foreground "IndianRed1"))
   (((class color) (background dark)) .
    (:background "light coral" :foreground "IndianRed1"))))

(face-spec-set
 'term-color-white
 '((((class color) (background light)) .
    (:background "#EEEEEC" :foreground "#D3D7CF"))
   (((class color) (background dark)) .
    (:background "#EEEEEC" :foreground "#D3D7CF"))))

(face-spec-set
 'term-color-yellow
 '((((class color) (background light)) .
    (:background "#FCE94F" :foreground "#D3AC00"))
   (((class color) (background dark)) .
    (:background "#FCE94F" :foreground "#D3AC00"))))

(defun rh-vterm-send-end ()
  "Sends `<end>' to the libvterm."
  (interactive)
  (vterm-send-key "<end>"))

(defun rh-vterm-send-home ()
  "Sends `<home>' to the libvterm."
  (interactive)
  (vterm-send-key "<home>"))

(defun rh-vterm-send-insert ()
  "Sends `<insert>' to the libvterm."
  (interactive)
  (vterm-send-key "<insert>"))

(defun rh-vterm-send-C-home ()
  "Sends `C-<home>' to the libvterm."
  (interactive)
  (vterm-send-key "<home>" nil nil t))

(defun rh-vterm-send-C-end ()
  "Sends `C-<end>' to the libvterm."
  (interactive)
  (vterm-send-key "<end>" nil nil t))

(defun rh-vterm-send-* ()
  "Sends `*' to the libvterm."
  (interactive)
  (vterm-send-key "*"))

(defun rh-vterm-send-C-up ()
  "Sends `C-<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>" nil nil t))

(defun rh-vterm-send-C-down ()
  "Sends `C-<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>" nil nil t))

(defun rh-vterm-send-C-x ()
  "Sends `C-x' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t))

(defun rh-vterm-send-C-c ()
  "Sends `C-c' to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun rh-vterm-send-C-v ()
  "Sends `C-v' to the libvterm."
  (interactive)
  (vterm-send-key "v" nil nil t))

(defun rh-vterm-send-S-f2 ()
  "Sends `S-<f2>' to the libvterm."
  (interactive)
  (vterm-send-key "<f2>" t))

(defun rh-vterm-send-f1 ()
  "Sends `<f1>' to the libvterm."
  (interactive)
  (vterm-send-key "<f1>"))

(provide 'config-vterm)
