;;; ramblehead's nodejs-repl initialisation

(require 'nodejs-repl)

(defun js-scratch ()
  (interactive)
  (display-buffer
   (let ((bufp (get-buffer "*js-scratch*"))
         buf)
     (if bufp
         (setq buf bufp)
       (setq buf (get-buffer-create "*js-scratch*")))
     (with-current-buffer buf
       (js2-mode)
       (rh-nodejs-repl)
       (set (make-local-variable 'js2-strict-missing-semi-warning) nil)
       (unless bufp
         (insert "// This buffer is for text that is not saved, and for JavaScript evaluation.\n")
         (insert "// To create a file, visit it with <open> and enter text in its buffer.\n\n")))
     buf)
   '(display-buffer-same-window)))

(provide 'init-nodejs-repl)

(get-buffer "*js-scratch*")
