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

;; (defun rh-tide-jump-to-filespan:around
;;     (orig-fun filespan &optional reuse-window no-marker)
;;   (plist-put filespan ':file
;;              (replace-regexp-in-string
;;               (concat (regexp-quote "$$virtual/") ".*/0/") ""
;;               (plist-get filespan ':file)))
;;   (funcall orig-fun filespan reuse-window no-marker))

;; (advice-add 'tide-jump-to-filespan :around
;;             #'rh-tide-jump-to-filespan:around)

;; Test strings
;; (setq path "/home/rh/box/artizanya/county/.yarn/cache/next-npm-9.4.5-canary.43-21cd24de92-0fbddd70d8.zip/node_modules/next/types/index.d.ts")
;; (setq path "/home/rh/box/artizanya/county/.yarn/cache/next-npm-9.4.5-canary.43-21cd24de92-0fbddd70d8.zip")
;; (setq path "/home/rh/box/artizanya/county/xxx.txt")
;; (setq path "70d8.zip/rh/xxx.txt")

(require 'arc-mode)

(defun tide--get-arc-path-pair (full-path)
  ;; \\|\\\\ in case of some substandard OSes ;)
  ;; Is there some safer/more-generic way to split/join paths in elisp?
  ;; Possibly, OS-neutral and with URL/protocol prefixes?
  (let ((path-components (split-string full-path "/\\|\\\\"))
        (arc-path "")
        (file-path-in-arc "")
        arc-found)
    ;; Distinguishing absolute and relative paths - i.e. trailing "/".
    (unless (string-empty-p (car path-components))
      (setq arc-path (car path-components)))
    (setq path-components (cdr path-components))
    (seq-do
     (lambda (component)
       (if arc-found
           (setq file-path-in-arc (concat file-path-in-arc "/" component))
         (setq arc-path (concat arc-path "/" component))
         (when (and (file-regular-p arc-path)
                    (with-temp-buffer
                      ;; 300000 is a magic number - it should
                      ;; be more than enough to recognise any achieve
                      ;; type header.
                      (insert-file-contents arc-path nil 0 300000)
                      (ignore-errors (archive-find-type))))
           (setq arc-found t))))
     path-components)
    (and arc-found
         (not (string-empty-p arc-path))
         (not (string-empty-p file-path-in-arc))
         (cons arc-path (substring file-path-in-arc 1)))))

(defun tide-get-file-buffer:override (file &optional new-file)
  "Returns a buffer associated with a file. This will return the
current buffer if it matches `file'. This way we can support
temporary and indirect buffers."
  ;; See https://yarnpkg.com/advanced/pnpapi#resolvevirtual
  ;; and https://github.com/yarnpkg/berry/issues/499#issuecomment-539458981 !!!
  ;; for file-virtual-resolved
  (let ((file-virtual-resolved
         (replace-regexp-in-string "\\.yarn/\\$\\$virtual.*/[0-9]+/" "" file))
        arc-path-pair)
    (cond
     ((equal file (tide-buffer-file-name)) (current-buffer))
     ((setq arc-path-pair
            (tide--get-arc-path-pair
             (replace-regexp-in-string "\\$\\$virtual.*cache/" "cache/" file)))
      (let ((arc-path (car arc-path-pair))
            (file-path-in-arc (cdr arc-path-pair))
            arc-buf)
        (setq arc-buf (find-file-noselect arc-path))
        (with-current-buffer arc-buf
          (goto-char (point-min))
          ;; This should fail in nested archives.
          (re-search-forward (concat " " file-path-in-arc "$"))
          (archive-extract))))
     ;; ((string-match-p ".*\\.zip/.*" file) ; (file-exists-p fullname)
     ;;  (let* ((full-path
     ;;          (replace-regexp-in-string "\\$\\$virtual.*cache/" "cache/" file))
     ;;         arc-path file-path-in-arc arc-buf)
     ;;    (save-match-data
     ;;      (string-match "\\(.*\\.zip\\)/\\(.*\\)" full-path)
     ;;      (setq arc-path (match-string 1 full-path))
     ;;      (setq file-path-in-arc (match-string 2 full-path)))
     ;;    (with-temp-buffer
     ;;      (insert-file-contents arc-path nil 0 300000)
     ;;      (archive-find-type))
     ;;    (setq arc-buf (find-file-noselect arc-path))
     ;;    (with-current-buffer arc-buf
     ;;      (goto-char (point-min))
     ;;      (search-forward file-path-in-arc)
     ;;      (archive-extract))))
     ((file-exists-p file) (find-file-noselect file))
     ((file-exists-p file-virtual-resolved)
      (find-file-noselect file-virtual-resolved))
     (new-file (let ((buffer (create-file-buffer file)))
                 (with-current-buffer buffer
                   (set-visited-file-name file)
                   (basic-save-buffer)
                   (display-buffer buffer t))
                 buffer))
     (t (error "Invalid file %S" file)))))

(advice-add 'tide-get-file-buffer :override
            #'tide-get-file-buffer:override)

(defun tide-eldoc-maybe-show:override (text)
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-no-interference-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
         ;; (eldoc-message (replace-regexp-in-string
         ;;                 "\\$\\$virtual.*\\(cache/\\)" "\\1" text))
         (eldoc-message (replace-regexp-in-string
                         "\\.yarn/\\$\\$virtual.*/[0-9]+/" "" text))
         )))

(advice-add 'tide-eldoc-maybe-show :override
            #'tide-eldoc-maybe-show:override)

(provide 'config-tide)
