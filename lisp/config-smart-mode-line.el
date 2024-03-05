;;; ramblehead's smart-mode-line configuration -*- lexical-binding: t -*-

(customize-set-value 'mode-line-percent-position nil)
(customize-set-value 'sml/position-percentage-format nil)
(defvar rh-sml/position-percentage-format nil)

(define-key-after
  (lookup-key mode-line-column-line-number-mode-map
              [mode-line down-mouse-1])
  [total-lines-mode]
  '(menu-item "Display Total Number of Lines" total-lines-mode
	      :help
              "Toggle displaying a total number of lines in the mode-line"
	      :button (:toggle . total-lines-mode))
  'line-number-mode)

;; (defun rh-sml/is-%p-p (x)
;;   "Non-nil if X matches \"%p\" in a very subjective sense."
;;   (or (and (listp x)
;;            (or (memq 'mode-line-percent-position x)
;;                (cl-remove-if-not
;;                 (lambda (y) (string-match-p ".*%p.*" y))
;;                 (cl-remove-if-not #'stringp x))))
;;       (and (stringp x)
;;            (string-match ".*%p.*" x))))

;; (advice-add #'sml/is-%p-p
;;             :override #'rh-sml/is-%p-p)

(defun rh-sml/compile-position-construct (&optional symbol value)
  "Recompile the `sml/position-construct' after one of the formats was edited.
Also sets SYMBOL to VALUE."
  (when (and symbol value) (set symbol value))
  (sml/generate-position-help)
  (setq
   sml/position-construct
   `((total-lines-mode
      (:propertize
       (:eval (format
               (let* ((width-min (if (and (integerp display-line-numbers-width)
                                          (> display-line-numbers-width 0))
                                     display-line-numbers-width
                                   0))
                      (width (max (length (number-to-string total-lines))
                                  width-min
                                  2)))
                 ;; left margin to align with actual
                 ;; display-line-numbers with as well as practical
                 (if (> width width-min)
                     (cl-incf width 1)
                   (cl-incf width 2))
                 (concat "%" (number-to-string width) "d"))
               total-lines))
       ;; face sml/col-number
       help-echo ,(concat "Total lines mode\n"
                          "mouse-1: Display Line "
                          "and Column Mode Menu")
       mouse-face mode-line-highlight
       local-map ,mode-line-column-line-number-mode-map))

     (total-lines-mode
      (:propertize " " face sml/numbers-separator))

     (line-number-mode
      (:propertize "[" face sml/numbers-separator))

     (line-number-mode
      (:propertize
       (:eval (let ((width
                     (if total-lines-mode
                         (max (length (number-to-string total-lines))
                              (or display-line-numbers-width 0))
                       (or display-line-numbers-width 0))))
                (concat "%" (number-to-string width) "l")))
       ;; face sml/line-number
       face mode-line-highlight
       help-echo ,(concat "Line number mode\n"
                          "mouse-1: Display Line "
                          "and Column Mode Menu")
       mouse-face mode-line-highlight
       local-map ,mode-line-column-line-number-mode-map))

     (column-number-mode
      (:propertize
       (:eval (if line-number-mode "," ""))
       face sml/numbers-separator))

     (column-number-mode
      ,(propertize sml/col-number-format
                   'face 'sml/col-number
                   'help-echo (concat "Column number mode\n"
                                      "nmouse-1: Display Line "
                                      "and Column Mode Menu")
                   'mouse-face 'mode-line-highlight
                   'local-map mode-line-column-line-number-mode-map))

     (line-number-mode
      ,(propertize "]" 'face 'sml/numbers-separator))

     ;; (,(or line-number-mode column-number-mode)
     ;;  (:propertize " " face 'sml/numbers-separator))

     ;; (t
     ;;  (:propertize
     ;;   (:eval (if (or line-number-mode column-number-mode) " " ""))
     ;;   face 'sml/numbers-separator))

     (:propertize
      (:eval (if (or line-number-mode column-number-mode) " " ""))
      face sml/numbers-separator)

     (size-indication-mode
      ,(propertize sml/size-indication-format
                   'face 'sml/col-number
                   'help-echo (concat "Size indication mode\n"
                                      "nmouse-1: Display Line "
                                      "and Column Mode Menu")
                   'mouse-face 'mode-line-highlight
                   'local-map mode-line-column-line-number-mode-map))

     (size-indication-mode
      ,(propertize " " 'face 'sml/numbers-separator))

     (rh-sml/position-percentage-format
      (-3 (:propertize (:eval rh-sml/position-percentage-format)
                       local-map ,mode-line-column-line-number-mode-map
                       mouse-face mode-line-highlight
                       face sml/position-percentage
                       help-echo ,(concat "Buffer Relative Position\n"
                                          "mouse-1: Display Line and "
                                          "Column Mode Menu"))))

     (rh-sml/position-percentage-format
      ,(propertize " " 'face 'sml/numbers-separator)))))

(advice-add #'sml/compile-position-construct :override
            #'rh-sml/compile-position-construct)

(defun rh-sml/setup (orig-fun &rest r)
  "Setup the mode-line to be smarter and sexier."

  ;; Preserve mode-line-buffer-identification when calling orig-fun()
  (let ((mode-line-buffer-identification))
    (apply orig-fun r))

  ;; vc-mode
  (eval-after-load "vc-hooks"
    '(defadvice vc-mode-line (after rh-vc-mode-line () activate)
       "Configure `vc-mode'."
       (when (stringp vc-mode)
         (let ((text-properties (text-properties-at 1 vc-mode))
               (noback
                (replace-regexp-in-string
                 (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
           (when (> (string-width noback) 20)
             (let (vc-mode-truncation-string noback-beg noback-end help-echo)
               (setq help-echo (plist-get text-properties 'help-echo))
               (setq help-echo (split-string help-echo "\n"))
               (push "" help-echo)
               (push (substring noback 1) help-echo)
               (setq help-echo (string-join help-echo "\n"))
               (plist-put text-properties 'help-echo help-echo)
               (setq vc-mode-truncation-string
                     (if (char-displayable-p ?…) "…" "..."))
               (setq noback-beg (substring noback 0 14))
               (setq noback-end (substring noback -5))
               (setq noback (concat noback-beg
                                    vc-mode-truncation-string
                                    noback-end))
               (add-text-properties 1 (length noback) text-properties noback)))
           (setq vc-mode
                 (propertize
                  (if sml/vc-mode-show-backend vc-mode noback)
                  'face
                  (cond ((string-match "^ -" noback) 'sml/vc)
                        ((string-match "^ [:@]" noback) 'sml/vc-edited)
                        ((string-match "^ [!\\?]" noback) 'sml/modified))))))))

  (run-hooks 'sml/after-setup-hook))

(advice-add #'sml/setup :around
            #'rh-sml/setup)

(provide 'config-smart-mode-line)
