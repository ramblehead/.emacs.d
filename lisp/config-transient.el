;;; ramblehead's transient library configuration

(defun rh-transient--fit-window-to-buffer:around (orig-fun window &rest rest)
  (cl-letf (((car (window-parameter window 'quit-restore)) nil))
    (apply orig-fun (cons window rest))))

(advice-add 'transient--fit-window-to-buffer :around
            #'rh-transient--fit-window-to-buffer:around)

(defvar rh-transient-orig-window)

(defun rh-transient-setup:before (&optional name &rest _rest)
  (let ((orig-window
         (get-window-with-predicate
          (lambda (window)
            (and (eq (window-parameter window 'window-side) 'bottom)
                 (eq (window-parameter window 'window-slot) 0))))))
    (if orig-window
        (setq rh-transient-orig-window
              (list :window orig-window
                    :height (window-total-height orig-window)))
      (setq rh-transient-orig-window nil))))

(advice-add 'transient-setup :before
            #'rh-transient-setup:before)

(defun rh-transient-exit-hook-handler ()
  (when-let* ((rh-transient-orig-window)
              (window (plist-get rh-transient-orig-window :window))
              ((window-live-p window))
              (orig-height (plist-get rh-transient-orig-window :height)))
    (with-selected-window window
      (let* ((current-height (window-total-height))
             (delta (- orig-height current-height)))
        (enlarge-window delta)))))

(add-hook 'transient-exit-hook #'rh-transient-exit-hook-handler)

(defun rh-transient-display-buffer-in-bottom-0-side-window (buffer _alist)
  (display-buffer-in-side-window
   buffer
   '((side . bottom)
     (inhibit-same-window . t))))

(provide 'config-transient)
