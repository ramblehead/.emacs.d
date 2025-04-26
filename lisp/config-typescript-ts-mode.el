;;; ramblehead's typescript-ts-mode configuration

(defun rh-treesit-fold-parsers-typescript-ts ()
  "Rule set for TypeScript with JSX."
  (append
   (treesit-fold-parsers-javascript)
   '((class_body . treesit-fold-range-seq)
     (enum_body . treesit-fold-range-seq)
     (named_imports . treesit-fold-range-seq)
     (object_type . treesit-fold-range-seq)
     (interface_declaration . treesit-fold-range-seq)
     (object_pattern . treesit-fold-range-seq)
     (parenthesized_expression . treesit-fold-range-seq)
     (formal_parameters . treesit-fold-range-seq))))

(defun rh-treesit-fold-parsers-tsx-ts ()
  "Rule set for TypeScript with JSX."
  (append
   (rh-treesit-fold-parsers-typescript-ts)
   '((jsx_expression . treesit-fold-range-seq)
     (jsx_element . treesit-fold-range-html)
     ;; (jsx_opening_element . (treesit-fold-range-html 3 0))
     (jsx_opening_element . treesit-fold-range-html)
     (jsx_self_closing_element . treesit-fold-range-html))))

(defun rh-augment-typescript-ts-mode-indent-rules (language)
  (unless (local-variable-p 'treesit-simple-indent-rules)
    (error "No buffer-local treesit-simple-indent-rules is defined"))

  (push
   '((parent-is "binary_expression") parent-bol 0)
   (alist-get language treesit-simple-indent-rules))

  (push
   `((parent-is ,(rx (or "variable" "lexical") "_"
                     (or "declaration" "declarator")))
     parent-bol typescript-ts-mode-indent-offset)
   (alist-get language treesit-simple-indent-rules)))

(provide 'config-typescript-ts-mode)
