;;; ramblehead's saveplace configuration

(defun rh-recenter-after-find-file ()
  (run-with-timer
   0 nil
   (lambda (buf)
     (dolist (win (get-buffer-window-list buf nil t))
       (with-selected-window win (recenter))))
   (current-buffer)))

(add-hook 'save-place-after-find-file-hook #'rh-recenter-after-find-file)

(provide 'config-saveplace)
