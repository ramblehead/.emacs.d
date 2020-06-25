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

(defun rh-vterm-send-insert ()
  "Sends `<insert>' to the libvterm."
  (interactive)
  (vterm-send-key "<insert>"))

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

(defun rh-vterm-send-S-f2 ()
  "Sends `S-<f2>' to the libvterm."
  (interactive)
  (vterm-send-key "<f2>" t))

(defun rh-vterm-send-f1 ()
  "Sends `<f1>' to the libvterm."
  (interactive)
  (vterm-send-key "<f1>"))

(provide 'config-vterm)
