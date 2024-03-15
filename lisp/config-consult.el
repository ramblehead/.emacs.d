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
    :action ,#'dired
    :state ,#'consult--file-preview
    :enabled ,(lambda () recentf-mode)
    :items ,#'rh-consult--recentf-dirs)
  "Recentf directory source for `consult-buffer-sources'.")

(provide 'config-consult)
