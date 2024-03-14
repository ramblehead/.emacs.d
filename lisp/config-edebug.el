;;; ramblehead's edebug configuration

(defvar rh-edebug-scroll-margin 5)

(defun rh-edebug-mode-hook-handler ()
  (if (bound-and-true-p edebug-mode)
      (setq-local scroll-margin rh-edebug-scroll-margin)
    (kill-local-variable 'scroll-margin)))

(provide 'config-edebug)
