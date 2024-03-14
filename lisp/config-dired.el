;;; ramblehead's dired configuration

(require 'ace-window)

(defun rh-dired-find-file ()
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (filename-after
          (if (string= (file-name-nondirectory filename) "..")
              (dired-current-directory)
            (concat (file-name-as-directory filename) "..")))
         (find-file-respect-dired
          (lambda (fn)
            (if (= (length (get-buffer-window-list)) 1)
                (find-alternate-file fn)
              (bury-buffer)
              (find-file fn)))))
    (if (not (file-directory-p filename))
        (funcall find-file-respect-dired filename)
      (if (not (file-accessible-directory-p filename))
          (error (format "Directory '%s' is not accessible" filename))
        (funcall find-file-respect-dired filename))
      (dired-goto-file filename-after)
      (recenter))))

(defun rh-dired-change-to-file (filename)
  (interactive "FFind file: ")
  (find-alternate-file filename))

(defun rh-dired-ace-select-other-window ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (when (ace-select-window)
      (find-file file-name))))

(defun rh-get-buffer-mode-window-list (&optional buffer-or-name mode all-frames)
  (setq mode (or mode major-mode))
  (let ((window-list '()))
    (dolist (win (window-list-1 nil nil all-frames))
      (when (eq
             (with-selected-window win
               major-mode)
             mode)
        (push win window-list)))
    (nreverse window-list)))

(defun rh-dired-select-next-dired-window ()
  (interactive)
  (let ((window-list (rh-get-buffer-mode-window-list)))
    (if (> (length window-list) 1)
        (select-window (nth 1 window-list))
      (let ((directory (dired-current-directory))
            (pos (point)))
        (when (ace-select-window)
          (find-file directory)
          (goto-char pos))))))

(defun rh-dired-alt-ace-select-other-window ()
  (interactive)
  (with-selected-window (frame-selected-window)
    (let ((name (dired-get-file-for-visit)))
      (when (ace-select-window)
        (find-file name)))))

(defun rh-dired-change-to-parent-dir ()
  (interactive)
  (dired-goto-file (concat (dired-current-directory) ".."))
  (rh-dired-find-file))

(defun rh-dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)))

(setq dired-dwim-target t)

(put 'dired-find-alternate-file 'disabled nil)

(if (equal system-type 'windows-nt)
    ;; In MS Windows systems
    (progn
      (setq dired-listing-switches "-alhgG")
      (setq vr-dired-coding-system 'cp1251))
  ;; In unix-like systems
  ;; Sort dirs and files in dired as in "C"
  (setenv "LC_COLLATE" "C")
  (setq dired-listing-switches
        ;; "--group-directories-first --time-style=long-iso -alhD"
        "-alhDG1v --group-directories-first --time-style=long-iso")
  (setq vr-dired-coding-system nil))

;; (global-set-key (kbd "C-x d") 'rh-dired-guess-dir)
(global-set-key (kbd "C-x d") #'dired-jump)

(add-hook
 'dired-mode-hook
 (lambda ()
   (hl-line-mode 1)
   (setq-local coding-system-for-read vr-dired-coding-system)
   (setq-local find-file-visit-truename nil)
   ;; (add-hook
   ;;  'window-selection-change-functions
   ;;  (lambda (win)
   ;;    (when (eq (with-selected-window win major-mode) 'dired-mode)
   ;;      (if (eq win (frame-selected-window))
   ;;          (hl-line-mode 1)
   ;;        (with-selected-window win
   ;;          (when (= (length (get-buffer-window-list)) 1)
   ;;            (hl-line-mode -1))))))
   ;;  nil t)
   ))

(provide 'config-dired)
