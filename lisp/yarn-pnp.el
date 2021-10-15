;;; ramblehead's Yarn PnP virtual paths support for tide and lsp-mode

(require 'tide nil t)
(require 'lsp-mode nil t)
(require 'arc-mode)

;;; Common Functions
;;; /b/{

(defun yarn-pnp--get-arc-path-pair (full-path)
  ;; \\|\\\\ in case of some substandard OSes ;)
  ;; Is there some safer/more-generic way to split/join paths in elisp?
  ;; Possibly, OS-neutral and with URL/protocol prefixes?
  (let ((path-components (split-string full-path "/\\|\\\\"))
        (arc-path "")
        (file-path-in-arc "")
        arc-found)
    ;; Distinguishing absolute and relative paths - i.e. trailing "/".
    (unless (string-empty-p (car path-components))
      (setq arc-path (car path-components)))
    (setq path-components (cdr path-components))
    (seq-do
     (lambda (component)
       (if arc-found
           (setq file-path-in-arc (concat file-path-in-arc "/" component))
         (setq arc-path (concat arc-path "/" component))
         (when (and (file-regular-p arc-path)
                    (with-temp-buffer
                      ;; 300000 is a magic number - it should
                      ;; be more than enough to recognise any achieve
                      ;; type header.
                      (insert-file-contents arc-path nil 0 300000)
                      (ignore-errors (archive-find-type))))
           (setq arc-found t))))
     path-components)
    (and arc-found
         (not (string-empty-p arc-path))
         (not (string-empty-p file-path-in-arc))
         (cons arc-path (substring file-path-in-arc 1)))))

(defun yarn-pnp--resolve-virtual (path)
  ;; See https://yarnpkg.com/advanced/pnpapi#resolvevirtual
  ;; and https://github.com/yarnpkg/berry/issues/499#issuecomment-539458981
  (save-match-data
    (let ((sep "/")
          tail hash depth head)
      (if (not (string-match
                "\\(.*\\)/\\(?:__virtual__\\|\\$\\$virtual\\)/\\([^/]+\\)/\\([0-9]+\\)/\\(.*\\)"
                path))
          path
        ;; Strings
        (setq tail (match-string 1 path))
        (setq hash (match-string 2 path))
        (setq depth (match-string 3 path))
        (setq head (match-string 4 path))
        ;; Data
        (set-text-properties 0 (length sep) (text-properties-at 0 path) sep)
        (setq depth (string-to-number depth))
        (setq tail (split-string tail "/"))
        (setq tail (seq-subseq tail 0 (when (> depth 0) (- depth))))
        (setq head (split-string head "/"))
        (string-join (append tail head) sep)))))

(defun yarn-pnp--resolve-virtual-eldoc (msg)
  (when msg
    (save-match-data
      (let ((virtual-path))
        (string-match
         "\\(.*\\\"\\)\\(.*/\\(?:__virtual__\\|\\$\\$virtual\\)/[^/]+/[0-9]+/.*\\)\\(\\\".*\\)"
         msg)
        (setq virtual-path (match-string 2 msg))
        (if virtual-path
            (concat
             (match-string 1 msg)
             (yarn-pnp--resolve-virtual virtual-path)
             (match-string 3 msg))
          msg)))))

;;; /b/}

;;; lsp-mode Functions
;;; /b/{

(defun yarn-pnp--lsp-ts-ls-p ()
  "Tests if current buffer workspaces contain a workspace with server-id 'ts-ls
or 'jsts-ls"
  (seq-find
   (lambda (workspace)
     (or (eq (lsp--workspace-server-id workspace) 'ts-ls)
         (eq (lsp--workspace-server-id workspace) 'jsts-ls)))
   lsp--buffer-workspaces))

(defun find-buffer-visiting:yarn-pnp-around (orig-fun filename &rest args)
  "If FILENAME is in archive, use arc-mode to open it, otherwise use original
`find-buffer-visiting' function."
  (let* ((fn-resolved (yarn-pnp--resolve-virtual filename))
         (arc-path-pair (yarn-pnp--get-arc-path-pair fn-resolved)))
    (if (not arc-path-pair)
        (apply orig-fun fn-resolved args)
      (let ((arc-path (car arc-path-pair))
            (file-path-in-arc (cdr arc-path-pair)))
        (with-current-buffer (find-file-noselect arc-path)
          (goto-char (point-min))
          ;; This should fail in nested archives.
          (re-search-forward (concat " " file-path-in-arc "$"))
          (archive-extract))))))

(defun lsp--xref-make-item:yarn-pnp-around (orig-fun filename &rest args)
  "If FILENAME is in archive, convert it to arc-mode path style before passing
it to original `lsp--xref-make-item' function, otherwise pass it as it is."
  (let* ((fn-resolved (yarn-pnp--resolve-virtual filename))
         (arc-path-pair (yarn-pnp--get-arc-path-pair fn-resolved)))
    (if (not arc-path-pair)
        (apply orig-fun fn-resolved args)
      (let ((arc-path (car arc-path-pair))
            (file-path-in-arc (cdr arc-path-pair)))
        (apply orig-fun (concat arc-path ":" file-path-in-arc) args)))))

(defun lsp--locations-to-xref-items:yarn-pnp-around (orig-fun locations)
  "Running `lsp--locations-to-xref-items' function unmodified with
`find-buffer-visiting' being adviced with `find-buffer-visiting:yarn-pnp-around'"
  (if (not (yarn-pnp--lsp-ts-ls-p))
      (funcall orig-fun locations)
    (unwind-protect
        (progn
          (advice-add 'find-buffer-visiting :around
                      #'find-buffer-visiting:yarn-pnp-around)
          (funcall orig-fun locations))
      (advice-remove 'find-buffer-visiting
                     #'find-buffer-visiting:yarn-pnp-around))))

(defun lsp--eldoc-message:yarn-pnp-around (orig-fun &optional msg)
  "Show MSG in eldoc. When lsp server-is is ts-ls or jsts-ls, also resolving
substring with yarn pnp virtual path if any."
  (if (not (yarn-pnp--lsp-ts-ls-p))
      (funcall orig-fun msg)
    (setq lsp--eldoc-saved-message msg)
    (run-with-idle-timer
     0 nil
     (lambda (msg)
       (with-no-warnings
         (eldoc-message (yarn-pnp--resolve-virtual-eldoc msg))))
     msg)))

(defun yarn-pnp-lsp-enable ()
  (interactive)
  (advice-add 'lsp--locations-to-xref-items :around
              #'lsp--locations-to-xref-items:yarn-pnp-around)
  (advice-add 'lsp--xref-make-item :around
              #'lsp--xref-make-item:yarn-pnp-around)
  (advice-add 'lsp--eldoc-message :around
              #'lsp--eldoc-message:yarn-pnp-around)
  (message "Yarn PnP support for lsp enabled"))

(defun yarn-pnp-lsp-disable ()
  (interactive)
  (advice-remove 'lsp--locations-to-xref-items
                 #'lsp--locations-to-xref-items:yarn-pnp-around)
  (advice-remove 'lsp--xref-make-item
                 #'lsp--xref-make-item:yarn-pnp-around)
  (advice-remove 'lsp--eldoc-message
                 #'lsp--eldoc-message:yarn-pnp-around)
  (message "Yarn PnP support for lsp disabled"))

;;; /b/}

;;; tide Functions
;;; /b/{

(defun tide-get-file-buffer:yarn-pnp-around (orig-fun file &optional new-file)
  "Returns a buffer associated with a file. This will return the
current buffer if it matches FILE. Then it will try to resolve
yarn pnp virtual path in archives and unplugged. Then it will call
the original tide-get-file-buffer() function."
  (let ((file-virtual-resolved (yarn-pnp--resolve-virtual file))
        arc-path-pair)
    (cond
     ((equal file (tide-buffer-file-name)) (current-buffer))
     ((setq arc-path-pair (yarn-pnp--get-arc-path-pair file-virtual-resolved))
      (let ((arc-path (car arc-path-pair))
            (file-path-in-arc (cdr arc-path-pair))
            arc-buf)
        (setq arc-buf (find-file-noselect arc-path))
        (with-current-buffer arc-buf
          (goto-char (point-min))
          ;; This should fail in nested archives.
          (re-search-forward (concat " " file-path-in-arc "$"))
          (archive-extract))))
     ((file-exists-p file-virtual-resolved)
      (find-file-noselect file-virtual-resolved))
     (t (funcall orig-fun file new-file)))))

(defun tide-eldoc-maybe-show:yarn-pnp-around (orig-fun text)
  (funcall orig-fun (yarn-pnp--resolve-virtual-eldoc text)))

(defun yarn-pnp-tide-enable ()
  (interactive)
  (advice-add 'tide-get-file-buffer :around
              #'tide-get-file-buffer:yarn-pnp-around)
  (advice-add 'tide-eldoc-maybe-show :around
              #'tide-eldoc-maybe-show:yarn-pnp-around)
  (message "Yarn PnP support for tide enabled"))

(defun yarn-pnp-tide-disable ()
  (interactive)
  (advice-remove 'tide-get-file-buffer
                 #'tide-get-file-buffer:yarn-pnp-around)
  (advice-remove 'tide-eldoc-maybe-show
                 #'tide-eldoc-maybe-show:yarn-pnp-around)
  (message "Yarn PnP support for tide disabled"))

;;; /b/}

(provide 'yarn-pnp)
