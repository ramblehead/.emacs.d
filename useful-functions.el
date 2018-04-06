(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun count-words-region (start end)
  "Print and returns number of words in the region."
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      (message "Region has %d words" n)
      n)))

(defun word-count-region (start end)
  "Print number of lines words and characters in the region."
  (interactive "r")
  (message "Region has %d lines, %d words, %d characters"
           (count-lines start end)
           (count-words-region start end)
           (- end start)))

(defun latex-word-count ()
  (interactive)
  (shell-command (concat "texcount " "\"" (buffer-file-name) "\"")))

;; http://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs
(defun toggle-camelcase-underscores ()
  "Toggle between camcelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p
            (progn (goto-char start)
                   (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start end)))))

(defun insert-file-path-at-point ()
  (interactive)
  (insert (expand-file-name (buffer-file-name))))

(defun insert-file-name-at-point ()
  (interactive)
  (insert (file-name-nondirectory (buffer-file-name))))

(defun downcase-first-letter (obj)
  (concat (downcase (substring obj 0 1)) (substring obj 1)))

(defun wrap-in-quotes-region (start end)
  "Print and returns number of words in the region."
  (interactive "r")
  (save-excursion
    (replace-regexp "^\\(.*\\)$" "\"\\1\"" nil start end)))

(defun unwrap-from-quotes-region (start end)
  "Print and returns number of words in the region."
  (interactive "r")
  (save-excursion
    (replace-regexp
     "^\\([ \t]*\\)\"\\(.*\\)\"\\([ \t]*\\)$" "\\1\\2\\3"
     nil start end)))
