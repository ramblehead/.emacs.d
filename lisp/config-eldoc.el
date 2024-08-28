;;; ramblehead's eldoc configuration

(require 's)
(require 'dash)

;; TODO: write to GH issue after testing this code for Rust
;; https://github.com/emacs-lsp/lsp-mode/issues/2613

(defun rh-take-until-dash (lines)
  "Return a new list containing elements from STRINGS up to, but not including,
\"---\" or \"___\".  If \"---\" or \"___\" is not found, return the entire
list."
  (let (result)
    (while (and lines (not (seq-contains-p
                            '("---" "___") (car lines) #'string-equal)))
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
                                          lines))
             (ret-colour (face-attribute 'secondary-selection :background))
             (ret (if (char-displayable-p ?↵)
                      (concat
                       " "
                       (propertize "↵" 'face (list :background ret-colour))
                       " ")
                    (concat
                       " "
                       (propertize "\\n" 'face (list :background ret-colour))
                       " "))))
        (if non-comment-lines
            (append
             (list format-string
                   (funcall #'string-join non-comment-lines ret))
             (cddr args))
          args))
    args))

(defun rh-eldoc-minibuffer-message-with-ellipsis
    (oldfun format-string &rest args)
  "Wrapper function that combines original `eldoc-minibuffer-message` behavior
with ellipsis truncation."
  (let* ((message-string (apply #'format (or format-string "") args))
         (first-line (string-trim (car (split-string message-string "\n"))))
         (max-length (- (frame-width) 4)))

    (if (<= (length first-line) max-length)
        (apply oldfun first-line args)
      (funcall oldfun "%s..." (substring first-line 0 max-length)))))

(defun rh-eldoc-special-mode-hook-handler ()
  (when (string-prefix-p "*eldoc" (buffer-name))
    (visual-line-mode 1)))

(provide 'config-eldoc)
