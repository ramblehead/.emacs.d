;;; ramblehead's typescript-mode configuration
(require 'company)

(setq typescript-indent-level 2)

(add-hook
 'typescript-mode-hook
 (lambda ()
   (setq-local company-backends (copy-tree company-backends))
   (company-mode 1)

   (setq-local rm-blacklist (seq-copy rm-blacklist))
   (add-to-list 'rm-blacklist " jsi-node")
   (rh-programming-minor-modes 1)))

(defun typescript-tsc-pretty-error--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      (move-beginning-of-line 1)
      (re-search-forward
       "^\\([^(\r\n)]+\\):[[:digit:]]+:[[:digit:]]+ - error TS[[:digit:]]+:")
      (let ((path (substring-no-properties (match-string 1))))
        (list (concat (file-truename (rh-project-get-root)) path))))))

(let ((form `(typescript-tsc-pretty
              ,(concat
                "^\\([^(\r\n)]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\) - "
                "error TS[[:digit:]]+:.*$")
              typescript-tsc-pretty-error--find-filename
              2 3 2 nil (1 'error))))
  (if (assq 'typescript-tsc-pretty compilation-error-regexp-alist-alist)
      (setf (cdr (assq 'typescript-tsc-pretty compilation-error-regexp-alist-alist)) (cdr form))
    (push form compilation-error-regexp-alist-alist)))

(provide 'config-typescript-mode)
