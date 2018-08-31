;;; init-tide.el --- ramblehead's tide configuration

(require 'company)

(defun rh-tide-company-display-permanent-doc-buffer ()
  (display-buffer (get-buffer-create "*tide-documentation*")))

(defun rh-tide-documentation-quit ()
  (interactive)
  (let ((bufwin (get-buffer-window "*tide-documentation*"))
        (selwin (selected-window)))
    (when bufwin
      (select-window bufwin)
      (g2w-quit-window)
      (select-window selwin)
      t)))

(defun rh-config-tide ()
  (add-to-list 'display-buffer-alist
               `("*tide-references*"
                 ,(g2w-display #'display-buffer-below-selected t)
                 (inhibit-same-window . t)
                 (window-height . shrink-window-if-larger-than-buffer)))

  (add-to-list 'display-buffer-alist
               `("*tide-documentation*"
                 ,(g2w-display #'display-buffer-in-side-window t)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (add-to-list 'display-buffer-alist
               `((lambda (buffer-nm actions)
                   (with-current-buffer buffer-nm
                     (eq major-mode 'tide-project-errors-mode)))
                 ,(g2w-display #'display-buffer-below-selected t)
                 (inhibit-same-window . t)
                 (window-height . shrink-window-if-larger-than-buffer)))

  (setq tide-completion-ignore-case t)
  (setq tide-always-show-documentation t)

  (add-hook
   'tide-mode-hook
   (lambda ()
     (set (make-local-variable 'rh-company-display-permanent-doc-buffer)
          #'rh-tide-company-display-permanent-doc-buffer))))

(provide 'init-tide)
