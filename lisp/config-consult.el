;;; ramblehead's consult configuration -*- lexical-binding: t -*-

(defun consult-line-from-isearch ()
  "Call `consult-line` with the search string from the last `isearch`."
  (interactive)
  (consult-line isearch-string))

(provide 'config-consult)
