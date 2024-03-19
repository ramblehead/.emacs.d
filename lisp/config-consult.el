;;; ramblehead's consult configuration -*- lexical-binding: t -*-

(defun rh-consult-line-from-isearch ()
  "Call `consult-line' with the search string from the last `isearch`."
  (interactive)
  (consult-line isearch-string))

(defun rh-consult--file-directory-safe (file)
  (if (or (and (file-remote-p file) (string-suffix-p "/" file))
          (file-directory-p file))
      (file-name-as-directory file)
    (file-name-directory file)))

(defun rh-consult--recentf-dirs ()
  "Return list of recentf dirs."
  (thread-last
    recentf-list
    (mapcar #'rh-consult--file-directory-safe)
    (delete-dups)
    (mapcar #'abbreviate-file-name)))

(defvar rh-consult-source-recentf-dirs
  `(:name "Directory"
    :narrow ?d
    :category file
    :face consult-file
    :history file-name-history
    ;; :action ,#'dired
    :action ,(lambda (file)
               ;; (with-current-buffer (dired file)
               ;;   (font-lock-mode 1)
               ;;   (font-lock-ensure)
               ;;   (redisplay t)
               ;;   (font-lock-default-fontify-buffer)
               ;;   (font-lock-update))

               ;; (let ((original-window (selected-window))
               ;;       (windows (get-buffer-window-list (dired file) nil t)))
               ;;   (dolist (window windows)
               ;;     (select-window window)
               ;;     (redisplay t))
               ;;   (select-window original-window))

               ;; (let ((original-window (selected-window))
               ;;       (windows (get-buffer-window-list (dired file) nil t)))
               ;;   (dolist (window windows)
               ;;     (let ((frame (window-frame window)))
               ;;       (select-frame-set-input-focus frame)
               ;;       (select-window window)
               ;;       (sit-for 0)))
               ;;   (select-window original-window))

               ;; (let ((original-window (selected-window))
               ;;       (windows (get-buffer-window-list (dired file) nil t)))
               ;;   (dolist (window windows)
               ;;     (select-window window)
               ;;     (redisplay t)
               ;;     (sit-for 0))
               ;;   (select-window original-window))

               (dired file))
    ;; :state ,#'consult--file-state
    :state ,#'consult--file-preview
    :enabled ,(lambda () recentf-mode)
    :items ,#'rh-consult--recentf-dirs)
  "Recentf directory source for `consult-buffer-sources'.")

(provide 'config-consult)
