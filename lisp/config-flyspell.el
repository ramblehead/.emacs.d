;;; ramblehead's flyspell configuration

(defun rh-markdown-flyspell-check-word-p (orig-fun &rest r)
  (save-excursion
    (goto-char (1- (point)))
    (if (markdown--face-p (point) '(markdown-metadata-key-face
                                    markdown-html-tag-name-face
                                    markdown-html-attr-name-face
                                    markdown-html-entity-face))
        (prog1 nil
          ;; If flyspell overlay is put, then remove it
          (let ((bounds (bounds-of-thing-at-point 'word)))
            (when bounds
              (cl-loop for ov in (overlays-in (car bounds) (cdr bounds))
                       when (overlay-get ov 'flyspell-overlay)
                       do
                       (delete-overlay ov)))))
      (apply orig-fun r))))

(eval-after-load 'markdown-mode
  (advice-add 'markdown-flyspell-check-word-p :around
              #'rh-markdown-flyspell-check-word-p))

(provide 'config-flyspell)
