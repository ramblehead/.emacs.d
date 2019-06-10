;;; ramblehead's nxml-mode configuration

(require 'sgml-mode)

(defun rh-nxml-code-folding-setup ()
  ;; see http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
  ;; see http://www.emacswiki.org/emacs/HideShow
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 ;; "<!--\\|<[^/>]*[^/]>"
                 "<!--\\|<[^/][^>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))

(defun rh-nxml-compute-indent-from-previous-line (orig-fun)
  (let (prev-bol ref-column looking-at->)
    (catch 'indent
      (save-excursion
	;; Move backwards until the start of a non-blank line that is
	;; not inside a token.
	(while (progn
		 (when (= (forward-line -1) -1)
		   (throw 'indent 0))
		 (back-to-indentation)
		 (if (looking-at "[ \t]*$")
		     t
		   (or prev-bol
		       (setq prev-bol (point)))
		   (nxml-token-after)
		   (not (or (= xmltok-start (point))
			    (eq xmltok-type 'data))))))
        (setq ref-column (current-column)))
      (save-excursion
        (forward-line 0)
        (skip-chars-forward " \t")
        (setq looking-at-> (looking-at ">")))
      (if looking-at-> ref-column (funcall orig-fun)))))

(advice-add 'nxml-compute-indent-from-previous-line :around
            #'rh-nxml-compute-indent-from-previous-line)

(provide 'config-nxml-mode)
