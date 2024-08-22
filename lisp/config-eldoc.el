;;; ramblehead's eldoc configuration

;; (defun rh-eldoc-minibuffer-message-pre-processor (message-string)
;;   (cond
;;    ((seq-contains-p '(rust-mode rust-ts-mode) major-mode)
;;     (replace-regexp-in-string "[[:space:]]*//.*[\n\r]+" "" message-string))))

(defun rh-eldoc-minibuffer-message-with-ellipsis
    (oldfun format-string &rest args)
  "Wrapper function that combines original `eldoc-minibuffer-message` behavior
with ellipsis truncation."
  (let* ((message-string (apply #'format (or format-string "") args))
         ;; (message-string
         ;;  (rh-eldoc-minibuffer-message-pre-processor
         ;;   (apply #'format (or format-string "") args)))
         (first-line (string-trim (car (split-string message-string "\n"))))
         (max-length (- (frame-width) 3)))
    ;; TODO: find how to remove bold face from minibuffer
    ;; (remove-text-properties 0 (length first-line) '(face bold) first-line)
    (if (<= (length first-line) max-length)
        (apply oldfun first-line args)
      (funcall
       oldfun (format "%s..." (substring first-line 0 max-length))))))

(defun rh-eldoc-special-mode-hook-handler ()
  (when (string-prefix-p "*eldoc" (buffer-name))
    (visual-line-mode 1)))

(provide 'config-eldoc)
