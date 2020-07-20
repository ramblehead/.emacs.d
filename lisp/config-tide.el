;;; ramblehead's tide configuration

;; (defun rh-tide-company-display-permanent-doc-buffer ()
;;   (display-buffer (get-buffer-create "*tide-documentation*")))

(defun rh-tide-documentation-quit ()
  (interactive)
  (let ((bufwin (get-buffer-window "*tide-documentation*"))
        (selwin (selected-window)))
    (when bufwin
      (select-window bufwin)
      (g2w-quit-window)
      (select-window selwin)
      t)))

(defun rh-tide-jump-to-filespan:around
    (orig-fun filespan &optional reuse-window no-marker)
  (plist-put filespan ':file
             (replace-regexp-in-string
              (concat (regexp-quote "$$virtual/") ".*/0/") ""
              (plist-get filespan ':file)))
  (funcall orig-fun filespan reuse-window no-marker))

(advice-add 'tide-jump-to-filespan :around
            #'rh-tide-jump-to-filespan:around)

(provide 'config-tide)
