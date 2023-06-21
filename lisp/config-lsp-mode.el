;;; ramblehead's lsp-mode configuration

(advice-add 'lsp--render-string :filter-return #'string-trim)

(provide 'config-lsp-mode)
