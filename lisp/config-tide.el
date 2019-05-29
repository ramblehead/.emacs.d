;;; ramblehead's tide configuration

(require 'company)
(require 'flycheck)

;; (defun rh-tide-company-display-permanent-doc-buffer ()
;;   (display-buffer (get-buffer-create "*tide-documentation*")))

(flycheck-add-mode 'typescript-tslint 'web-mode)

(defun rh-tide-documentation-quit ()
  (interactive)
  (let ((bufwin (get-buffer-window "*tide-documentation*"))
        (selwin (selected-window)))
    (when bufwin
      (select-window bufwin)
      (g2w-quit-window)
      (select-window selwin)
      t)))

(provide 'config-tide)
