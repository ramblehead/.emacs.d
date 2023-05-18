;;; ramblehead's rich-minority configuration

(defun rh-rm-minor-modes ()
  (interactive)
  (message
   (substring-no-properties
    (mapconcat
     (lambda (pair)
       (format "%s (%S)" (string-trim-left (car pair)) (cdr pair)))
     (delq nil (mapcar #'rm-format-mode-line-entry minor-mode-alist))
     "\n"))))

(provide 'config-rich-minority)
