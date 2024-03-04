;;; ramblehead's config-prettier configuration

(defun rh-prettier-after-save-hook-handler ()
  (run-with-idle-timer
   3
   nil
   (lambda (buffer)
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and (bound-and-true-p prettier-mode)
                    (bound-and-true-p flycheck-mode))
           (flycheck-buffer)))))
   (current-buffer)))

(add-hook
 ;; 'before-save-hook
 'after-save-hook
 #'rh-prettier-after-save-hook-handler)

(provide 'config-prettier)
