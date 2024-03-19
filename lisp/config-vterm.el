;;; ramblehead's vterm configuration

(customize-set-value 'vterm-kill-buffer-on-exit nil)
(customize-set-value 'vterm-max-scrollback 100000)

;; (face-spec-set
;;  'vterm-color-default
;;  '((((class color) (background light)) .
;;     (:inherit default))
;;    (((class color) (background dark)) .
;;     (:inherit default))))

;; (face-spec-set
;;  'vterm-color-inverse-video
;;  '((((class color) (background light)) .
;;     (:inherit vterm-color-default))
;;    (((class color) (background dark)) .
;;     (:inherit vterm-color-default))))

;; (face-spec-set
;;  'term-color-black
;;  '((((class color) (background light)) .
;;     (:background "#767875" :foreground "#2E3436"))
;;    (((class color) (background dark)) .
;;     (:background "#767875" :foreground "#2E3436"))))

;; (face-spec-set
;;  'term-color-blue
;;  '((((class color) (background light)) .
;;     (:background "#88AACF" :foreground "#3465A4"))
;;    (((class color) (background dark)) .
;;     (:background "#88AACF" :foreground "#3465A4"))))

;; (face-spec-set
;;  'term-color-cyan
;;  '((((class color) (background light)) .
;;     (:background "#34E2E2" :foreground "#06989A"))
;;    (((class color) (background dark)) .
;;     (:background "#34E2E2" :foreground "#06989A"))))

;; (face-spec-set
;;  'term-color-green
;;  '((((class color) (background light)) .
;;     (:background "#94C368" :foreground "medium sea green"))
;;    (((class color) (background dark)) .
;;     (:background "#94C368" :foreground "medium sea green"))))

;; (face-spec-set
;;  'term-color-magenta
;;  '((((class color) (background light)) .
;;     (:background "#C497BF" :foreground "#A177A8"))
;;    (((class color) (background dark)) .
;;     (:background "#C497BF" :foreground "#A177A8"))))

;; (face-spec-set
;;  'term-color-red
;;  '((((class color) (background light)) .
;;     (:background "light coral" :foreground "IndianRed1"))
;;    (((class color) (background dark)) .
;;     (:background "light coral" :foreground "IndianRed1"))))

;; (face-spec-set
;;  'term-color-white
;;  '((((class color) (background light)) .
;;     (:background "#EEEEEC" :foreground "#D3D7CF"))
;;    (((class color) (background dark)) .
;;     (:background "#EEEEEC" :foreground "#D3D7CF"))))

;; (face-spec-set
;;  'term-color-yellow
;;  '((((class color) (background light)) .
;;     (:background "#FCE94F" :foreground "#D3AC00"))
;;    (((class color) (background dark)) .
;;     (:background "#FCE94F" :foreground "#D3AC00"))))

(defvar rh-vterm-here-buffer-name
  "*term-here*")

(defface rh-vterm-color-default
  '((((class color) (background light)) .
     (:inherit default))
    (((class color) (background dark)) .
     ;; (:background "#000204" :inherit default)))
     ;; (:background "#000d1e" :inherit default)))
     (:background "#001938" :inherit default)))
  "Default face to use in vterm mode."
  :group 'vterm)

(defun rh-vterm-mode-hook-handler ()
  (setq-local column-number-mode nil)
  (setq-local line-number-mode nil)
  (face-remap-add-relative 'default 'rh-vterm-color-default))

(defun rh-vterm-here ()
  (interactive)
  (if (and (eq major-mode 'vterm-mode)
           (local-variable-p 'rh-vterm-here-origin-window)
           (window-live-p rh-vterm-here-origin-window))
      (select-window rh-vterm-here-origin-window)
    (let ((vterm-buffer (get-buffer rh-vterm-here-buffer-name))
          (vterm-pwd (expand-file-name default-directory))
          (origin-window (frame-selected-window)))
      (if (and vterm-buffer (get-buffer-process vterm-buffer))
          (progn
            (rh-bs-display-buffer-in-botom-0-side-window vterm-buffer)
            (select-window (rh-bs-get-botom-0-side-window))
            (setq-local rh-vterm-here-origin-window origin-window)
            (unless (string= (expand-file-name default-directory) vterm-pwd)
              (setq-local default-directory vterm-pwd)
              (vterm-send-key "a" nil nil t)
              (vterm-send-key "k" nil nil t)
              (vterm-send-string (concat "cd " vterm-pwd))
              (vterm-send-return)))
        (when vterm-buffer (kill-buffer vterm-buffer))
        (setq vterm-buffer (get-buffer-create rh-vterm-here-buffer-name))
        (with-current-buffer vterm-buffer
          (setq-local vterm-kill-buffer-on-exit t)
          (setq-local default-directory vterm-pwd)
          (vterm-mode))
        (rh-bs-display-buffer-in-botom-0-side-window vterm-buffer)
        (select-window (rh-bs-get-botom-0-side-window))
        (setq-local rh-vterm-here-origin-window origin-window)))))

(defun rh-vterm-copy-mode (&optional arg)
  (interactive)
  (if (not (called-interactively-p))
      (vterm-copy-mode arg)
    (deactivate-mark)
    (call-interactively (symbol-function 'vterm-copy-mode))
    ;; (if vterm-copy-mode
    ;;     (compilation-minor-mode 1)
    ;;   (compilation-minor-mode -1))
    (unless vterm-copy-mode
      (compilation-minor-mode -1))))

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

(defun rh-vterm-send-<kp-multiply> ()
  "Sends `<kp-multiply>' to the libvterm."
  (interactive)
  (vterm-send-key "*"))

(defun rh-vterm-send-<kp-add> ()
  "Sends `<kp-add>' to the libvterm."
  (interactive)
  (vterm-send-key "+"))

(defun rh-vterm-send-<kp-subtract> ()
  "Sends `<kp-subtract>' to the libvterm."
  (interactive)
  (vterm-send-key "-"))

(defun rh-vterm-send-C-up ()
  "Sends `C-<up>' to the libvterm."
  (interactive)
  (vterm-send-key "<up>" nil nil t))

(defun rh-vterm-send-C-down ()
  "Sends `C-<down>' to the libvterm."
  (interactive)
  (vterm-send-key "<down>" nil nil t))

(defun rh-vterm-send-C-x_c ()
  "Sends `C-x c' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "c"))

(defun rh-vterm-send-C-x_s ()
  "Sends `C-x s' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "s"))

(defun rh-vterm-send-C-x_v ()
  "Sends `C-x v' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "v"))

(defun rh-vterm-send-C-x_C-s ()
  "Sends `C-x C-s' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t)
  (vterm-send-key "s" nil nil t))

(defun rh-vterm-send-C-x ()
  "Sends `C-x' to the libvterm."
  (interactive)
  (vterm-send-key "x" nil nil t))

(defun rh-vterm-send-C-c ()
  "Sends `C-c' to the libvterm."
  (interactive)
  (vterm-send-key "c" nil nil t))

(defun rh-vterm-send-C-d ()
  "Sends `C-d' to the libvterm."
  (interactive)
  (vterm-send-key "d" nil nil t))

(defun rh-vterm-send-M-<return> ()
  "Sends `C-<return>' to the libvterm."
  (interactive)
  (vterm-send-key (kbd "RET") nil t nil))

(defun rh-vterm-send-C-v ()
  "Sends `C-v' to the libvterm."
  (interactive)
  (vterm-send-key "v" nil nil t))

(defun rh-vterm-send-S-f2 ()
  "Sends `S-<f2>' to the libvterm."
  (interactive)
  (vterm-send-key "<f2>" t nil nil))

(defun rh-vterm-send-f1 ()
  "Sends `<f1>' to the libvterm."
  (interactive)
  (vterm-send-key "<f1>"))

(defun rh--vterm-set-excursion (window window-start point)
  (when (window-valid-p window)
    (with-selected-window window (goto-char point))
    (set-window-start window window-start)))

(defun rh--vterm-ensure-excursion (_text)
  (with-ivy-window
    (when (eq major-mode 'vterm-mode)
      (run-with-idle-timer
       0 nil
       #'rh--vterm-set-excursion
       (selected-window)
       (window-start)
       (point)))))

(provide 'config-vterm)
