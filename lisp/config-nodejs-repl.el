;;; ramblehead's nodejs-repl configuration

(add-to-list 'display-buffer-alist
             '("*nodejs*"
               (display-buffer-same-window
                display-buffer-use-some-window
                display-buffer-pop-up-window)
               (inhibit-same-window . t)))

(defvar rh-nodejs-interaction-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") #'rh-nodejs-repl-send-line-or-region)
    map))

(define-minor-mode rh-nodejs-interaction
  "Minor mode for interacting with a nodejs from other (e.g js) buffers."
  :lighter " NodeJS Interaction"
  :keymap rh-nodejs-interaction-map)

(defadvice nodejs-repl-switch-to-repl
    (around rh-nodejs-repl-switch-to-repl () activate)
  (let ((buf (get-buffer "*nodejs*")))
    (if buf
        (pop-to-buffer buf)
      (nodejs-repl))))

(defun rh-nodejs-repl-send-line-or-region (start end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (let ((win (frame-selected-window)))
    (nodejs-repl-switch-to-repl)
    (select-window win)
    (if (/= start end)
        (nodejs-repl-send-region start end)
      (if (eq (point) (line-beginning-position))
          (nodejs-repl-send-line)
        (nodejs-repl-send-last-expression)))))

(provide 'config-nodejs-repl)
