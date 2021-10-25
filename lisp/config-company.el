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

;; (defun company-preview-show-at-point:override (pos completion)
;;   (company-preview-hide)

;;   (let* ((company-common (and company-common
;;                               (string-prefix-p company-prefix company-common)
;;                               company-common))
;;          (common (company--common-or-matches completion)))
;;     (setq completion (copy-sequence (company--pre-render completion)))
;;     (add-face-text-property 0 (length completion) 'company-preview
;;                             nil completion)

;;     (cl-loop for (beg . end) in common
;;              do (add-face-text-property beg end 'company-preview-common
;;                                         nil completion))

;;     ;; Add search string
;;     (and (string-match (funcall company-search-regexp-function
;;                                 company-search-string)
;;                        completion)
;;          (pcase-dolist (`(,mbeg . ,mend) (company--search-chunks))
;;            (add-face-text-property mbeg mend 'company-preview-search
;;                                    nil completion)))

;;     (setq completion (if (string-prefix-p company-prefix completion)
;;                          (company-strip-prefix completion)
;;                        completion))

;;     ;; (setq completion
;;     ;;       (cond
;;     ;;        ((and common
;;     ;;              (= 0 (caar common)))
;;     ;;         (substring completion (min (cdar common)
;;     ;;                                    (length company-prefix))))
;;     ;;        (company-common
;;     ;;         (company-strip-prefix completion))
;;     ;;        (t completion)))

;;     ;; (setq completion
;;     ;;       (if company-common
;;     ;;           (company-strip-prefix completion)
;;     ;;         (if (cdr common)
;;     ;;             completion
;;     ;;           (let* ((first-common-range (car common))
;;     ;;                  (first-common (and first-common-range
;;     ;;                                     (substring completion
;;     ;;                                                (car first-common-range)
;;     ;;                                                (cdr first-common-range)))))
;;     ;;             (if (and first-common
;;     ;;                      (eq (car first-common-range) 0))
;;     ;;                 (substring completion (length first-common))
;;     ;;               completion)))))

;;     ;; (setq completion
;;     ;;       (if company-common
;;     ;;           (company-strip-prefix completion)
;;     ;;         completion))

;;     (and (equal pos (point))
;;          (not (equal completion ""))
;;          (add-text-properties 0 1 '(cursor 1) completion))

;;     (let* ((beg pos)
;;            (pto company-pseudo-tooltip-overlay)
;;            (ptf-workaround (and
;;                             pto
;;                             (char-before pos)
;;                             (eq pos (overlay-start pto)))))
;;       ;; Try to accommodate for the pseudo-tooltip overlay,
;;       ;; which may start at the same position if it's at eol.
;;       (when ptf-workaround
;;         (cl-decf beg)
;;         (setq completion (concat (buffer-substring beg pos) completion)))

;;       (setq company-preview-overlay (make-overlay beg pos))

;;       (let ((ov company-preview-overlay))
;;         (overlay-put ov (if ptf-workaround 'display 'after-string)
;;                      completion)
;;         (overlay-put ov 'window (selected-window))))))

;; (advice-add 'company-preview-show-at-point :override
;;             #'company-preview-show-at-point:override)

(provide 'config-company)
