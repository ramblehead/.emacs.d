;;; ramblehead's company-mode configuration

(setq company-backends
      '((company-keywords company-dabbrev-code)
        company-files (company-dabbrev company-ispell)))

;; TODO: write to https://github.com/company-mode/company-mode/issues/123
(defun rh-company-pseudo-tooltip-on-explicit-action (command)
  (cl-case command
    (hide (company-pseudo-tooltip-frontend command)
          (company-preview-frontend command))
    (t (if (company-explicit-action-p)
           (company-pseudo-tooltip-frontend command)
         (company-preview-frontend command)))))

(defmacro rh-company-tooltip-key (default-key cmd)
  `(lambda ()
     (interactive)
     (if (company-tooltip-visible-p)
         (funcall ,cmd)
       (let ((default-cmd (or (local-key-binding ,default-key)
                              (global-key-binding ,default-key))))
         (when (fboundp default-cmd)
           (funcall default-cmd)
           (company-abort))))))

(defmacro rh-company-tooltip-cmd (default-cmd cmd)
  `(lambda ()
     (interactive)
     (if (company-tooltip-visible-p)
         (funcall ,cmd)
       (funcall ,default-cmd))))

(setq company-tooltip-align-annotations t)
(setq company-echo-truncate-lines nil)
(company-echo-show)
(setq company-minimum-prefix-length 1)
(setq company-frontends
      '(rh-company-pseudo-tooltip-on-explicit-action
        company-preview-frontend
        company-echo-metadata-frontend))
(setq company-require-match nil)

(setq company-idle-delay 0)
(setq company-tooltip-maximum-width 80)
(setq company-tooltip-minimum-width 35)
(setq company-tooltip-offset-display 'lines)

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
