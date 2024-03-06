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

;; (defun rh-company-select-next-or-abort (&optional arg)
;;   (interactive "p")
;;   (if (company-tooltip-visible-p)
;;       (company-select-next-or-abort arg)
;;     (next-line)))

;; (defun rh-company-select-previous-or-abort (&optional arg)
;;   (interactive "p")
;;   (if (company-tooltip-visible-p)
;;       (company-select-previous-or-abort arg)
;;     (previous-line)))

(defun just-one-face (fn &rest args)
  (let ((orderless-match-faces [completions-common-part]))
    (apply fn args)))

(advice-add 'company-capf--candidates :around #'just-one-face)

(provide 'config-company)
