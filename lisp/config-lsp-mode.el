;;; ramblehead's lsp-mode configuration

(require 'json)

(advice-add 'lsp--render-string :filter-return #'string-trim)

(advice-add 'json-parse-buffer :around
            (lambda (orig &rest rest)
              (save-excursion
                (while (re-search-forward "\\\\u0000" nil t)
                  (replace-match "")))
              (apply orig rest)))

(provide 'config-lsp-mode)
