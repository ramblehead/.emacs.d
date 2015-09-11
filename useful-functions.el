(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
;; (define-key global-map "\M-Q" 'unfill-paragraph)
(global-set-key (kbd "M-Q") 'unfill-paragraph)

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
