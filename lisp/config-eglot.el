;;; ramblehead's eglot configuration

(require 'company-capf)

(defun rh-eglot-display-local-help ()
  (interactive)
  (display-local-help))

(defun rh-eglot-completion-at-point ()
  "EGLOT's `completion-at-point' function."
  ;; Commit logs for this function help understand what's going on.
  (when-let (completion-capability (eglot--server-capable :completionProvider))
    (let* ((server (eglot--current-server-or-lose))
           (sort-completions (lambda (completions)
                               (sort completions
                                     (lambda (a b)
                                       (string-lessp
                                        (or (get-text-property 0 :sortText a) "")
                                        (or (get-text-property 0 :sortText b) ""))))))
           (metadata `(metadata . ((display-sort-function . ,sort-completions))))
           resp items (cached-proxies :none)
           (proxies
            (lambda ()
              (if (listp cached-proxies) cached-proxies
                (setq resp
                      (jsonrpc-request server
                                       :textDocument/completion
                                       (eglot--CompletionParams)
                                       :deferred :textDocument/completion
                                       :cancel-on-input t))
                (setq items (append
                             (if (vectorp resp) resp (plist-get resp :items))
                             nil))
                (setq cached-proxies
                      (mapcar
                       (jsonrpc-lambda
                           (&rest item &key label insertText insertTextFormat
                                  &allow-other-keys)
                         ;; Need to (setq company--capf-cache nil)
                         ;; when testing
                         (when (string= (substring-no-properties label 0 1) "•")
                           (setq label (concat (substring-no-properties label 1) "•")))
                         (let ((proxy
                                (cond ((and (eql insertTextFormat 2)
                                            (eglot--snippet-expansion-fn))
                                       (string-trim-left label))
                                      ((and insertText
                                            (not (string-empty-p insertText)))
                                       insertText)
                                      (t
                                       (string-trim-left label)))))
                           (unless (zerop (length item))
                             (put-text-property 0 1 'eglot--lsp-item item proxy))
                           proxy))
                       items)))))
           resolved
           (resolve-maybe
            ;; Maybe completion/resolve JSON object `lsp-comp' into
            ;; another JSON object, if at all possible.  Otherwise,
            ;; just return lsp-comp.
            (lambda (lsp-comp)
              (cond (resolved resolved)
                    ((and (eglot--server-capable :completionProvider
                                                 :resolveProvider)
                          (plist-get lsp-comp :data))
                     (setq resolved
                           (jsonrpc-request server :completionItem/resolve
                                            lsp-comp :cancel-on-input t)))
                    (t lsp-comp))))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (list
       (or (car bounds) (point))
       (or (cdr bounds) (point))
       (lambda (probe pred action)
         (cond
          ((eq action 'metadata) metadata)               ; metadata
          ((eq action 'lambda)                           ; test-completion
           (member probe (funcall proxies)))
          ((eq (car-safe action) 'boundaries) nil)       ; boundaries
          ((and (null action)                            ; try-completion
                (member probe (funcall proxies)) t))
          ((eq action t)                                 ; all-completions
           (cl-remove-if-not
            (lambda (proxy)
              (let* ((item (get-text-property 0 'eglot--lsp-item proxy))
                     (filterText (plist-get item :filterText)))
                (and (or (null pred) (funcall pred proxy))
                     (string-prefix-p
                      probe (or filterText proxy) completion-ignore-case))))
            (funcall proxies)))))
       :annotation-function
       (lambda (proxy)
         (eglot--dbind ((CompletionItem) detail kind)
             (get-text-property 0 'eglot--lsp-item proxy)
           (let* ((detail (and (stringp detail)
                               (not (string= detail ""))
                               detail))
                  (annotation
                   (or detail
                       (cdr (assoc kind eglot--kind-names)))))
             (when annotation
               (concat " "
                       (propertize annotation
                                   'face 'font-lock-function-name-face))))))
       :company-doc-buffer
       (lambda (proxy)
         (let* ((documentation
                 (let ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy)))
                   (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
                (formatted (and documentation
                                (eglot--format-markup documentation))))
           (when formatted
             (with-current-buffer (get-buffer-create " *eglot doc*")
               (erase-buffer)
               (insert formatted)
               (current-buffer)))))
       :company-require-match 'never
       :company-prefix-length
       (save-excursion
         (when (car bounds) (goto-char (car bounds)))
         (when (listp completion-capability)
           (looking-back
            (regexp-opt
             (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
            (line-beginning-position))))
       :exit-function
       (lambda (proxy _status)
         (eglot--dbind ((CompletionItem) insertTextFormat
                        insertText textEdit additionalTextEdits label)
             (funcall
              resolve-maybe
              (or (get-text-property 0 'eglot--lsp-item proxy)
                        ;; When selecting from the *Completions*
                        ;; buffer, `proxy' won't have any properties.
                        ;; A lookup should fix that (github#148)
                        (get-text-property
                         0 'eglot--lsp-item
                         (cl-find proxy (funcall proxies) :test #'string=))))
           (let ((snippet-fn (and (eql insertTextFormat 2)
                                  (eglot--snippet-expansion-fn))))
             (cond (textEdit
                    ;; Undo (yes, undo) the newly inserted completion.
                    ;; If before completion the buffer was "foo.b" and
                    ;; now is "foo.bar", `proxy' will be "bar".  We
                    ;; want to delete only "ar" (`proxy' minus the
                    ;; symbol whose bounds we've calculated before)
                    ;; (github#160).
                    (delete-region (+ (- (point) (length proxy))
                                      (if bounds (- (cdr bounds) (car bounds)) 0))
                                   (point))
                    (eglot--dbind ((TextEdit) range newText) textEdit
                      (pcase-let ((`(,beg . ,end) (eglot--range-region range)))
                        (delete-region beg end)
                        (goto-char beg)
                        (funcall (or snippet-fn #'insert) newText)))
                    (when (cl-plusp (length additionalTextEdits))
                      (eglot--apply-text-edits additionalTextEdits)))
                   (snippet-fn
                    ;; A snippet should be inserted, but using plain
                    ;; `insertText'.  This requires us to delete the
                    ;; whole completion, since `insertText' is the full
                    ;; completion's text.
                    (delete-region (- (point) (length proxy)) (point))
                    (funcall snippet-fn (or insertText label)))))
           (eglot--signal-textDocument/didChange)
           (eglot-eldoc-function)))))))

;; (advice-add 'eglot-completion-at-point :override
;;             #'rh-eglot-completion-at-point)

;; (advice-remove 'eglot-completion-at-point #'rh-eglot-completion-at-point)

;; (defun rh-company-preview-frontend (command)
;;   "`company-mode' frontend showing the selection as if it had been inserted."
;;   (pcase command
;;     (`pre-command (company-preview-hide))
;;     (`post-command
;;      (company-preview-show-at-point
;;       (point)
;;       (let ((selection (nth company-selection company-candidates)))
;;         (if (string= (substring-no-properties selection -1) "•")
;;             (substring selection 0 -1)
;;           selection))))
;;     (`hide (company-preview-hide))))

;; (advice-add 'company-preview-frontend :override
;;             #'rh-company-preview-frontend)

(defun rh-eglot-rename (newname)
  "Rename the current symbol to NEWNAME."
  (interactive
   (let ((initial (symbol-name (symbol-at-point))))
     ;; (run-with-timer
     ;;  0 nil
     ;;  (lambda ()
     ;;    (let ((old-mark (or (cdr-safe transient-mark-mode)
     ;;                        transient-mark-mode)))
     ;;      (set-mark (point-max))
     ;;      (goto-char (minibuffer-prompt-end))
     ;;      ;; see https://emacs.stackexchange.com/questions/22162/how-to-set-mark-in-elisp-and-have-shift-selection
     ;;      ;;     for the explanation of the following line
     ;;      (setq transient-mark-mode (cons 'only old-mark)))))
     ;; (list (read-from-minibuffer
     ;;        (format "Rename `%s' to: " initial)
     ;;        (substring-no-properties initial)))
     (list (read-from-minibuffer
            (format "Rename `%s' to: " initial)
            nil nil nil nil
            (substring-no-properties initial)))))
  (unless (eglot--server-capable :renameProvider)
    (eglot--error "Server can't rename!"))
  (eglot--apply-workspace-edit
   (jsonrpc-request (eglot--current-server-or-lose)
                    :textDocument/rename `(,@(eglot--TextDocumentPositionParams)
                                           :newName ,newname))
   current-prefix-arg))

(advice-add 'eglot-rename :override
            #'rh-eglot-rename)

(provide 'config-eglot)
