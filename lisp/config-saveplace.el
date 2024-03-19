;;; ramblehead's saveplace configuration

(defun rh-recenter-after-find-file-timer-handler (buf)
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil t))
      (with-selected-window win (rh-recenter-sensibly)))))

(defun rh-recenter-after-find-file ()
  (run-with-idle-timer
   0 nil
   #'rh-recenter-after-find-file-timer-handler
   (current-buffer)))

(provide 'config-saveplace)
