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

(defun rh-company-select-next-or-abort (&optional arg)
  (interactive "p")
  (if (company-tooltip-visible-p)
      (company-select-next-or-abort arg)
    (next-line)))

(defun rh-company-select-previous-or-abort (&optional arg)
  (interactive "p")
  (if (company-tooltip-visible-p)
      (company-select-previous-or-abort arg)
    (previous-line)))

(provide 'config-company)
