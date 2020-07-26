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

(defun rh-tide-get-file-buffer:override (file &optional new-file)
  "Returns a buffer associated with a file. This will return the
current buffer if it matches `file'. This way we can support
temporary and indirect buffers."
  (cond
   ((equal file (tide-buffer-file-name)) (current-buffer))
   ((string-match-p ".*\\.zip/.*" file)
    (let* ((full-path
            (replace-regexp-in-string "\\$\\$virtual.*cache/" "cache/" file))
           arc-path file-path-in-arc arc-buf)
      (save-match-data
        (string-match "\\(.*\\.zip\\)/\\(.*\\)" full-path)
        (setq arc-path (match-string 1 full-path))
        (setq file-path-in-arc (match-string 2 full-path)))
      (setq arc-buf (find-file-noselect arc-path))
      (with-current-buffer arc-buf
        (beginning-of-buffer)
        (search-forward file-path-in-arc)
        (archive-extract))))
   ((file-exists-p file) (find-file-noselect file))
   (new-file (let ((buffer (create-file-buffer file)))
               (with-current-buffer buffer
                 (set-visited-file-name file)
                 (basic-save-buffer)
                 (display-buffer buffer t))
               buffer))
   (t (error "Invalid file %S" file))))

(advice-add 'tide-get-file-buffer :override
            #'rh-tide-get-file-buffer:around)

(provide 'config-tide)
