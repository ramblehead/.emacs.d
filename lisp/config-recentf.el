;;; ramblehead's recentf configuration

;; (setq rh-ignore-recentf '(;; AUCTeX output files
;;                           "\\.aux\\'"
;;                           "\\.bbl\\'"
;;                           "\\.blg\\'"
;;                           " output\\*$"))

(setq rh-ignore-recentf '())

(defun rh-recentf-open-edit ()
  (interactive)
  (when (not (local-variable-p 'recentf-edit-list))
    (kill-buffer)
    (recentf-edit-list)))

(defun rh-file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (when buf
      (let ((display-count (buffer-local-value 'buffer-display-count buf)))
        (if (> display-count 0) display-count nil)))))

(defun rh-keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed, and FILE does not mach rh-ignore-recentf
regexp-list."
  (if (and (recentf-keep-default-predicate file)
           (not (seq-some (lambda (regex)
			    (string-match-p regex file))
			  rh-ignore-recentf)))
      (rh-file-was-visible-p file)))

(provide 'config-recentf)
