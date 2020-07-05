;;; ramblehead's edebug configuration

(defvar rh-edebug-window-buffer-before-step nil)
(defvar rh-edebug-window-start-before-step nil)

(defun rh-edebug-set-mode-before (mode shortmsg msg)
  (case mode
    ((go)
     ;; (setq inhibit-redisplay t)
     (setq rh-edebug-window-buffer-before-step (window-buffer))
     (run-with-idle-timer
      0 nil
      (lambda ()
        (when (eq rh-edebug-window-buffer-before-step (window-buffer))
          (rh-recenter-sensibly))
        ;; (setq inhibit-redisplay nil)
        )))
    ((step next)
     (setq rh-edebug-window-buffer-before-step (window-buffer))
     (setq rh-edebug-window-start-before-step (window-start))
     ;; (setq inhibit-redisplay t)
     (run-with-idle-timer
      0 nil
      (lambda ()
        (when (and rh-edebug-window-start-before-step
                   (eq rh-edebug-window-buffer-before-step (window-buffer)))
          (set-window-start (frame-selected-window)
                            rh-edebug-window-start-before-step)
          (setq rh-edebug-window-start-before-step nil)
          (run-with-idle-timer
           0 nil
           (lambda ()
             (goto-char edebug-point)
             (rh-recenter-sensibly))))
        ;; (setq inhibit-redisplay nil)
        )))))

(advice-add 'edebug-set-mode :before
            #'rh-edebug-set-mode-before)

(provide 'config-edebug)
