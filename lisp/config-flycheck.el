;;; ramblehead's flycheck mode configuration

(defun rh-flycheck-visible-buffers ()
  "Run `flycheck-buffer` on all visible buffers if the buffer is in a major mode
that supports Flycheck and is visible."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (bound-and-true-p flycheck-mode)
                 (flycheck-may-enable-mode)
                 (get-buffer-window buffer 'visible))
        (flycheck-buffer)))))

(defun rh-flycheck-eslint--find-working-directory (_checker)
  (let* ((regex-config (concat "\\`\\(\\.eslintrc"
                               "\\(\\.\\(js\\|ya?ml\\|json\\)\\)?\\|"
                               "eslint\\.config\\.[mc]?[jt]s\\)\\'")))
    (when buffer-file-name
      (or (locate-dominating-file
           (file-name-directory buffer-file-name)
           (lambda (directory)
             (> (length (directory-files directory nil regex-config t)) 0)))
          (locate-dominating-file buffer-file-name ".eslintignore")
          (locate-dominating-file buffer-file-name "node_modules")))))

(advice-add 'flycheck-eslint--find-working-directory
            :override #'rh-flycheck-eslint--find-working-directory)

(provide 'config-flycheck)
