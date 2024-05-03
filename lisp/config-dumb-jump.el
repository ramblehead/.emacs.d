;;; ramblehead's dumb-jump configuration

(defun rh-dumb-jump-xref-find-definitions ()
  (interactive)
  (let ((xref-backend-functions (copy-sequence xref-backend-functions)))
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t)
    (call-interactively #'xref-find-definitions)))

(add-to-list 'xref-prompt-for-identifier #'rh-dumb-jump-xref-find-definitions t)

(provide 'config-dumb-jump)
