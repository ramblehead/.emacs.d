;;; ramblehead's lsp-ui configuration

;;
;; 2022-03-28 - fix sideline height computation
;;
(defun lsp-ui-sideline--compute-height:override ()
  "Return a fixed size for text in sideline."
  (let ((fontHeight (face-attribute 'lsp-ui-sideline-global :height)))
    (if (null text-scale-mode-remapping)
        '(height
          (if (floatp fontHeight) fontHeight
            (/ (face-attribute 'lsp-ui-sideline-global :height) 100.0))
          ;; Readjust height when text-scale-mode is used
          (list 'height
                (/ 1 (or (plist-get (cdr text-scale-mode-remapping) :height)
                         1)))))))
;;
;; 2024-08-22 - fix sideline alignment
;;
(defun lsp-ui-sideline--align:override (&rest lengths)
  "Align sideline string by LENGTHS from the right of the window."
  (list (* (window-font-width nil 'lsp-ui-sideline-global)
           (+ (apply '+ lengths) (if (display-graphic-p) 1 2)))))


(provide 'config-lsp-ui)
