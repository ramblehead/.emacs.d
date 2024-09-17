;;; rh-buffers.el --- set of buffer grouping utilities
;;
;; Description: Buffer grouping helper utilities.
;; Author: Victor Rybynok
;; Copyright (C) 2024, Victor Rybynok, all rights reserved.

(defvar rh-buffers-groups nil
  "Buffer groups used to create filters for buffer selection
configurations.  The group item values can be either buffer name regex
or buffer major mode symbol")

(defvar rh-buffers-not-file-group nil
  "Not-file buffers used to create files filter in buffer selection
configurations.  The buffer value can be either buffer name regex or
buffer major mode symbol")

(defvar rh-buffers-semantic-not-file-groups nil
  "Not-file buffer groups used to create filters for buffer selection
configurations.  The group item values can be either buffer name regex
or buffer major mode symbol")

(setq
 rh-buffers-semantic-not-file-groups
 '(("dired"
    (dired-mode)
    "^\\*Dired log\\*$")
   ("compilation"
    (compilation-mode
     (lambda (buffer)
       (with-current-buffer buffer
         (or (eq major-mode 'compilation-mode)
             (and (eq major-mode 'vterm-mode)
                  (bound-and-true-p rh-project-compile)))))))
   ("REPLs"
    (jsi-log-mode
     jsi-node-repl-mode))
   ("shells"
    (shell-mode
     ;; vterm-mode
     ;; (lambda (buffer)
     ;;   (and
     ;;    (eq (with-current-buffer buffer major-mode) 'vterm-mode)
     ;;    (not (memq 'compilation-minor-mode
     ;;               (with-current-buffer buffer minor-mode-list)))))
     (lambda (buffer)
       (with-current-buffer buffer
         (and (eq major-mode 'vterm-mode)
              (not (bound-and-true-p rh-project-compile)))))))
   ("info"
    (Info-mode))
   ("magit"
    (magit-diff-mode
     magit-log-mode
     magit-process-mode
     magit-revision-mode
     magit-status-mode
     magit-submodule-list-mode
     magit-refs-mode))))

(setq
 rh-buffers-not-file-group
 '("\\` "
   "^\\*Completions\\*$"
   "^\\*Quail Completions\\*$"
   "^\\*Messages\\*$"
   "^\\*clang-output\\*$"
   "^\\*clang-error\\*$"
   "^\\*Semantic SymRef\\*$"
   "^\\*Recent Files\\*$"
   "^\\*Directory\\*$"
   "^\\*Ido Completions\\*$"
   "^\\*buffer-selection\\*$"
   "^\\*httpd\\*$"
   "^\\*Async-native-compile-log\\*$"
   "^\\*Native-compile-Log\\*$"
   "^\\*Compile-Log\\*$"
   "^\\*Backtrace\\*$"
   "^\\*Diff\\*$"
   Buffer-menu-mode
   help-mode
   debugger-mode
   special-mode
   ;; xref
   xref--xref-buffer-mode
   ;; paradox
   paradox-menu-mode
   "^\\*Paradox http\\*$"
   ;; eldoc
   "^\\*eldoc.*\\*$"
   ;; skewer
   "^\\*skewer-error\\*$"
   ;; lsp
   lsp-log-io-mode
   "^\\*lsp-log\\*$"
   "^\\*ts-ls\\*$"
   "^\\*clangd\\*$"
   "^\\*pyright\\*$"
   "^\\*ruff-lsp\\*$"
   "^\\*nix-nil\\*$"
   "^\\*rust-analyzer\\*$"
   ;; eglot
   "^\\*EGLOT .*$"
   ;; flymake
   "^\\*Flymake log\\*$"
   ;; flycheck
   "^\\*Flycheck error messages\\*$"
   ;; pretter
   "^\\*prettier (local)\\*$"
   ;; AUCTeX output files
   " output\\*$"
   ;; vterm
   VTerm
   ;; tailwind
   "^\\*tailwindcss\\*$"
   ;; straight
   "^\\*straight-byte-compilation\\*$"
   ;; embark
   "^\\*Embark Export:.*\\*$"))

(setq rh-buffers-groups '())

(dolist (buffer-group rh-buffers-semantic-not-file-groups)
  (setq rh-buffers-groups
        (append (list (copy-tree buffer-group)) rh-buffers-groups)))

(setq rh-buffers-groups (nreverse rh-buffers-groups))

(dolist (buffer-group rh-buffers-semantic-not-file-groups)
  (setq rh-buffers-not-file-group
        (append (car (cdr buffer-group))
                rh-buffers-not-file-group)))

(defun rh-buffers-match (regexp-or-mode-or-func-list buffer)
  "Return non-nil if buffer either matches anything in listed regexps
or has one of the listed major modes."
  (let ((case-fold-search nil))
    (seq-find
     (lambda (regexp-or-mode-or-func)
       (cond
        ((stringp regexp-or-mode-or-func)
         (string-match-p regexp-or-mode-or-func (buffer-name buffer)))
        ((symbolp regexp-or-mode-or-func)
         (eq (with-current-buffer buffer major-mode) regexp-or-mode-or-func))
        ((functionp regexp-or-mode-or-func)
         (funcall regexp-or-mode-or-func buffer))))
     regexp-or-mode-or-func-list)))

(defun rh-buffers-get-group-name (buffer)
  "Return group name to which BUFFER belongs or nil if BUFFER has no group."
  (catch 'found
    (dolist (buffer-group rh-buffers-semantic-not-file-groups)
      (when (rh-buffers-match (car (cdr buffer-group)) buffer)
        (throw 'found (car buffer-group))))
    (unless (rh-buffers-match rh-buffers-not-file-group buffer)
      (throw 'found "files"))
    (when (rh-buffers-match '("\\` ") buffer)
      (throw 'found "sys"))
    "none"))

(provide 'rh-buffers)
