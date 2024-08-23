;;; ramblehead's eldoc configuration

(require 's)
(require 'dash)

(defun rh-take-until-dash (lines)
  "Return a new list containing elements from STRINGS up to, but not including,
\"-\".  If \"-\" is not found, return the entire list."
  (let (result)
    (while (and lines (not (string-equal "---" (car lines))))
      (setq result (cons (car lines) result))
      (setq lines (cdr lines)))
    (nreverse result)))

(defun rh-eldoc-minibuffer-message-rust-filter-args (args)
  (if (and (member major-mode '(rust-mode rust-ts-mode))
           (car args)
           (cadr args))
      (let* ((format-string (car args))
             (message-string (cadr args))
             (lines (rh-take-until-dash
                     (seq-map #'string-trim
                              (s-lines (string-trim message-string)))))
             (non-comment-lines (--filter (not (or (string-empty-p it)
                                                   (s-prefix? "//" it)))
                                          lines)))
        (if non-comment-lines
            (append
             (list format-string
                   (funcall #'string-join non-comment-lines " ↵ "))
             (cddr args))
          args))
    args))

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

    ;; (add-text-properties 0 (length first-line)
    ;;                      '(face (:family (face-attribute 'default :family)))
    ;;                      first-line)

    (if (<= (length first-line) max-length)
        (apply oldfun first-line args)
      (funcall oldfun "%s..." (substring first-line 0 max-length)))))

(defun rh-eldoc-special-mode-hook-handler ()
  (when (string-prefix-p "*eldoc" (buffer-name))
    (visual-line-mode 1)))

(provide 'config-eldoc)

