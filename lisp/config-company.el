;;; ramblehead's company-mode configuration

(defun rh-company-pseudo-tooltip-on-explicit-action (command)
  (cl-case command
    (hide (company-pseudo-tooltip-frontend command)
          (company-preview-frontend command))
    (t (if (company-explicit-action-p)
           (company-pseudo-tooltip-frontend command)
         (company-preview-frontend command)))))

(defun company-preview-show-at-point:filter-args (r)
  (let ((pos (nth 0 r))
        (completion (nth 1 r)))
    (when (or (string-prefix-p " " completion)
              (string-prefix-p "•" completion))
      (setq completion (substring completion 1)))
    (list pos completion)))

(advice-add 'company-preview-show-at-point :filter-args
            #'company-preview-show-at-point:filter-args)

(provide 'config-company)
