;;; ramblehead's json configuration

(defun rh-json-parse-buffer (orig &rest rest)
  (save-excursion
    (while (re-search-forward "\\\\u0000" nil t)
      (replace-match "")))
  (apply orig rest))

(advice-add 'json-parse-buffer :around #'rh-json-parse-buffer)

(provide 'config-json)
