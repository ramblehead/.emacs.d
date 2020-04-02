;;; ramblehead's vterm configuration

;; (defun rh-vterm-send-end ()
;;   "Sends `<end>' to the libvterm."
;;   (interactive)
;;   (vterm-send-key "<end>"))

(defun rh-vterm-send-kp-end ()
  "Sends `<kp-end>' to the libvterm."
  (interactive)
  (vterm-send-key "<end>"))

;; (defun rh-vterm-send-home ()
;;   "Sends `<home>' to the libvterm."
;;   (interactive)
;;   (vterm-send-key "<home>"))

(defun rh-vterm-send-kp-home ()
  "Sends `<kp-home>' to the libvterm."
  (interactive)
  (vterm-send-key "<home>"))

(provide 'config-vterm)
