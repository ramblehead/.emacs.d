;;; ramblehead's consult configuration -*- lexical-binding: t -*-

(defun rh-consult-line-from-isearch ()
  "Call `consult-line' with the search string from the last `isearch`."
  (interactive)
  (consult-line isearch-string))

(defun rh-consult--recentf-dirs ()
  "Return list of recentf dirs."
  (let* ((file-directory-safe
          (lambda (f)
            (or (and (if (file-remote-p f)
                         (string-suffix-p "/" f)
                       (file-directory-p f))
                     (file-name-as-directory f))
                (file-name-directory f)))))
    (thread-last recentf-list
      (mapcar file-directory-safe)
      (delete-dups)
      (mapcar #'abbreviate-file-name))))

(defvar rh-consult-source-recentf-dirs
  `(:name "Directory"
    :narrow ?d
    :category file
    :face consult-file
    :history file-name-history
    :action ,#'dired
    :state ,#'consult--file-preview
    :enabled ,(lambda () recentf-mode)
    :items ,#'rh-consult--recentf-dirs)
  "Recentf directory source for `consult-buffer-sources'.")

(provide 'config-consult)
