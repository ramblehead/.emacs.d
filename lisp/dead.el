;; It would be nice to have wcheck mode instead of speck,
;; however, I could not make it work under windows.
;; == wcheck mode ==

;; Could not make it work with Aspell on Windows 7.
;; Might try to fix it in future.
;; Folling back to the speck-mode for the time being.

;; (autoload 'wcheck-mode "wcheck-mode"
;;   "Toggle wcheck-mode." t)
;; (autoload 'wcheck-change-language "wcheck-mode"
;;   "Switch wcheck-mode languages." t)
;; (autoload 'wcheck-actions "wcheck-mode"
;;   "Open actions menu." t)
;; (autoload 'wcheck-jump-forward "wcheck-mode"
;;   "Move point forward to next marked text area." t)
;; (autoload 'wcheck-jump-backward "wcheck-mode"
;;   "Move point backward to previous marked text area." t)

;; (setq-default wcheck-language "British English")

;; (setq wcheck-language-data
;;       '(("British English"
;;          (program . "c:/Program Files (x86)/Aspell/bin/aspell.exe")
;;          (args "-l" "-d" "british"))
;;          ;; (action-program . "c:/Program Files (x86)/Aspell/bin/aspell.exe")
;;          ;; (action-args "-a" "-d" "british")
;;          ;; (action-parser . wcheck-parser-ispell-suggestions))
;;         ("Highlight FIXMEs"
;;          (program . (lambda (strings)
;;                       (when (member "FIXME" strings)
;;                         (list "FIXME"))))
;;          (face . highlight)
;;          (read-or-skip-faces
;;           ((emacs-lisp-mode c-mode) read font-lock-comment-face)
;;           (nil)))
;;         ))

;; (setq
;;  wcheck-read-or-skip-faces
;;  '((emacs-lisp-mode read font-lock-comment-face font-lock-doc-face)
;;    (message-mode read nil message-header-subject message-cited-text)
;;    (latex-mode read                     ; "read" the following faces
;;                nil                      ; nil means normal text
;;                font-latex-sectioning-1-face
;;                font-latex-sectioning-2-face
;;                font-latex-sectioning-3-face
;;                font-latex-sectioning-4-face
;;                font-latex-bold-face
;;                font-latex-italic-face
;;                font-lock-constant-face)
;;    (org-mode skip                       ; "skip" the following face(s):
;;              font-lock-comment-face)))

