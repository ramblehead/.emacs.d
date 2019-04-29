;;; inter-node.el --- Minor Node.JS Interaction Mode and Minimalist Node.JS REPL
;;
;; Description: Execute JavaScript commands in Node.JS
;;              directly from JavaScript buffers.
;; Author: Victor Rybynok (aka ramblehead)
;; Copyright (C) 2019, Victor Rybynok, all rights reserved.


;; (shell-command-to-string "ls")

;; (require 'json)

;; (setq-local ts-expr "const x: string = \"xxx\"")
;; (setq-local jsi-babel-dir nil) ; nil, 'file, 'pwd, ""
;; (setq-local jsi-babel-dir-default "/home/rh/projects/s600-solution/wtx/web/")
;; (setq-local jsi-babel-command nil) ; nil, ""
;; (setq-local jsi-babel-command-defaults '("babel" "npx --no-install babel"))
;; (setq-local jsi-ts-babel-config-file nil) ; nil, ""
;; (setq-local jsi-ts-babel-config-file-default
;;             "/home/rh/projects/s600-solution/wtx/web/jsi-ts.babel.config.js")

;; (setq-local
;;  jsi-babel-shell-command
;;  (concat
;;   "set -euo pipefail;"
;;   "cd " jsi-babel-dir ";"
;;   "echo \"" (json-encode-string ts-expr) "\""
;;   "|"
;;   jsi-babel-command
;;   " --no-babelrc --config-file " jsi-ts-babel-config-file
;;   " -f stdin.ts"))

;; echo "const x: number = 0"|npx babel --no-babelrc --config-file ./jsi-ts-babel-config-file -f stdin.ts

;; (shell-command-to-string jsi-babel-shell-command)


;; -------------------------------------------------------------------
;;; Minimalist Node.js REPL for inter-node minor mode
;; -------------------------------------------------------------------
;; /b/{

(defgroup inter-node nil
  "Node.js REPL and its minor interaction mode"
  :prefix "inter-node-"
  :group 'processes)

(defcustom inter-node-repl-prompt "> "
  "Node.js REPL prompt used in `inter-node-repl-mode'"
  :group 'nodejs-repl
  :type 'string)

(defvar inter-node-repl-process-name "inter-node-repl"
  "Process name of Node.js REPL")

(defvar inter-node-command "node"
  "Command to start Node.JS")

(defcustom inter-node-repl-start-js
  (concat
   "const repl = require('repl');"
   "const util = require('util');"
   ;; Do not split long lines to fit terminal width.
   ;; emacs should wrap or trim them instead.
   "process.stdout.columns = 0;"
   "process.stdout.rows = 0;"
   "process.stdout.on('resize', () => {"
   "  if(process.stdout.columns != 0) process.stdout.columns = 0;"
   "  if(process.stdout.rows != 0) process.stdout.rows = 0;"
   "});"
   "repl.start({"
   "  prompt: '" inter-node-repl-prompt "',"
   "  useGlobal: false,"
   "  replMode: repl.REPL_MODE_SLOPPY,"
   "  writer: output => util.inspect(output, { maxArrayLength: null }),"
   ;; "  writer: output => output,"
   "})")
  "JavaScript expression used to start Node.js REPL"
  :group 'nodejs-repl
  :type 'string)

(defun inter-node--strip-all-ascii-escapes (string)
  "Strip ASCII Terminal Escape Sequences"
  ;; \x1b is ^[ - RET ESCAPE
  ;; \x0d is ^M - RET CARRIAGE RETURN
  (replace-regexp-in-string "\x1b\\[[0-9;]*[a-zA-Z]\\|\x0d" "" string))

(defun inter-node--dedup-prompt (string)
  "Deduplicate string with prompt"
  (let* ((p inter-node-repl-prompt)
         (regexp (concat p "\\(.*\\)\\(" p "\\1\\)+")))
    (replace-regexp-in-string regexp "\\2" string)))

(defun inter-node--comint-preoutput-filter (output)
  (setq output (inter-node--strip-all-ascii-escapes output))
  (setq output (inter-node--dedup-prompt output))
  (if (and (string-match-p (concat "^" inter-node-repl-prompt) output)
           (string-match-p (concat "^" (regexp-quote output))
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position))))
      ""
    output))

(defun inter-node--wait-for-prompt (process)
  (with-current-buffer (process-buffer process)
    (let* ((buffer (current-buffer))
           (last-line (inter-node--get-buffer-last-line buffer))
           (prompt-regex (concat "^" inter-node-repl-prompt)))
      (while (not (string-match-p prompt-regex last-line))
        (unless (process-live-p process)
          (error "Node.js REPL process terminated"))
        (accept-process-output nil 0.01)
        (setq last-line (inter-node--get-buffer-last-line buffer)))
      (goto-char (point-max)))))

(define-derived-mode inter-node-repl-mode comint-mode "Node.js REPL"
  "Major mode for Node.js REPL"
  (add-hook 'comint-preoutput-filter-functions
            #'inter-node--comint-preoutput-filter nil t)

  (setq-local comint-process-echoes t)
  (setq-local comint-prompt-regexp (concat "^" inter-node-repl-prompt))
  (setq-local comint-use-prompt-regexp t)

  (add-hook 'completion-at-point-functions
            #'inter-node--completion-at-point-function nil t))

(defun inter-node--set-process-window-size (orig-fun process height width)
  (if (string= (process-name process) inter-node-repl-process-name)
      (funcall orig-fun process 0 0)
    (funcall orig-fun process height width)))

(advice-add 'set-process-window-size :around
            #'inter-node--set-process-window-size)

;;;###autoload
(defun inter-node-repl (&optional bury)
  "Run Node.js REPL"
  (interactive)
  (let ((process (get-process inter-node-repl-process-name))
        buffer)
    (if process
        (setq buffer (process-buffer process))
      (setq buffer (make-comint
                    inter-node-repl-process-name
                    inter-node-command nil "-e" inter-node-repl-start-js))
      (with-current-buffer buffer (inter-node-repl-mode))
      (setq process (get-buffer-process buffer))
      (inter-node--wait-for-prompt process))
    (if bury
        (bury-buffer buffer)
      (pop-to-buffer buffer))
    process))

(defun inter-node-repl-exit ()
  "Exit Node.js REPL"
  (interactive)
  (let ((process (get-process inter-node-repl-process-name)))
    (when process
      (with-current-buffer (process-buffer process)
        (insert ".exit")
        (comint-send-input)
        (current-buffer))
      (message "Process %s finished" inter-node-repl-process-name))))

;;; Minimalist Node.js REPL for inter-repl minor mode
;; /b/}

;; -------------------------------------------------------------------
;;; do-java-script and tab-completions in Node.js REPL
;; -------------------------------------------------------------------
;; /b/{

;; Shamelessly stolen from nodejs-repl-clear-line
(defun inter-node--clear-process-input (process)
  "Send ^U (NEGATIVE ACKNOWLEDGEMENT) to Node.js process."
  (process-send-string process "\x15"))

(defun inter-node--get-buffer-last-line (buffer)
  (let ((inhibit-field-text-motion t))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-max))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun inter-node--send-to-process-filter (process string)
  (with-current-buffer (process-buffer process)
    (goto-char (process-mark process))
    (insert (inter-node--comint-preoutput-filter string))
    (set-marker (process-mark process) (point))
    ;; Update window-point point - only useful for debugging.
    (dolist (window (get-buffer-window-list))
      (set-window-point window (point)))))

(defun inter-node--get-java-script-output (input-string)
  (unless (string-empty-p input-string)
    (let (beg end)
      (save-excursion
        (goto-char (point-min))
        (search-forward
         "// Entering editor mode (^D to finish, ^C to cancel)\n" nil t)
        (setq beg (search-forward input-string nil t))
        (setq beg (or (and beg (1+ beg)) (point)))
        (goto-char (point-max))
        (setq end (1- (line-beginning-position)))
        (buffer-substring-no-properties beg end)))))

(defun inter-node-do-java-script-sync (string)
  "Send string to Node.js process and return the output synchronously"
  (let* ((process (get-process inter-node-repl-process-name))
         (marker-position-orig (marker-position (process-mark process)))
         (process-filter-orig (process-filter process))
         (process-buffer-orig (process-buffer process))
         ;; \x04 is ^D - END OF TRANSMISSION
         (input (concat ".editor\n" string "\x04")))
    (with-temp-buffer
    ;; (with-current-buffer "dbg"
      (unwind-protect
          (progn
            (set-process-buffer process (current-buffer))
            (set-process-filter
             process #'inter-node--send-to-process-filter)
            (set-marker (process-mark process) (point-max))
            (process-send-string process input)
            (inter-node--wait-for-prompt process))
        (set-process-buffer process process-buffer-orig)
        (set-process-filter process process-filter-orig)
        (set-marker (process-mark process)
                    marker-position-orig process-buffer-orig))
      (inter-node--get-java-script-output string))))

(defun inter-node--get-tab-completions-output (input-string)
  (let (beg end output-string candidates candidate prefix-length)
    (save-excursion
      (goto-char (point-min))
      (move-beginning-of-line 2)
      (setq beg (point))
      (goto-char (point-max))
      (move-beginning-of-line 1)
      (unless (= (point) (point-min)) (left-char))
      (setq end (point))
      (setq output-string (buffer-substring-no-properties beg end))

      (setq candidates (split-string output-string "[\r\n]+"))
      ;; E.g. "Array." is a prefix in "Array.length" string
      (save-match-data
        (setq candidate (car candidates))
        (string-match "^\\(.+\\.\\)[^[:blank:][:cntrl:]\\.]+" candidate)
        (setq prefix-length (length (match-string 1 candidate))))

      (nreverse
       (seq-reduce
        (lambda (result candidate)
          (if (string-empty-p candidate)
              result
            (push (substring candidate prefix-length) result)))
        candidates ())))))

(defun inter-node-get-tab-completions-sync (string)
  "Get Node.js REPL tab-completions"
  (let* ((process (get-process inter-node-repl-process-name))
         (marker-position-orig (marker-position (process-mark process)))
         (process-filter-orig (process-filter process))
         (process-buffer-orig (process-buffer process)))
    (with-temp-buffer
    ;; (with-current-buffer "dbg"
      (unwind-protect
          (progn
            (set-process-buffer process (current-buffer))
            (set-process-filter
             process #'inter-node--send-to-process-filter)
            (set-marker (process-mark process) (point-min))
            (inter-node--clear-process-input process)
            (inter-node--wait-for-prompt process)
            (process-send-string process (concat string "\t"))
            (while (accept-process-output process 0.01))
            (process-send-string process "\t")
            (while (accept-process-output process 0.01))
            (inter-node--clear-process-input process)
            (inter-node--wait-for-prompt process))
        (set-process-buffer process process-buffer-orig)
        (set-process-filter process process-filter-orig)
        (set-marker (process-mark process)
                    marker-position-orig process-buffer-orig))
      (inter-node--get-tab-completions-output string))))

;;; do-java-script and tab-completions in Node.js REPL
;; /b/}

;; -------------------------------------------------------------------
;;; inter-node-mode - minor NodeJS REPL interaction mode
;; -------------------------------------------------------------------
;; /b/{

(defun inter-node--get-log-buffer ()
  "Returns `inter-node-log' buffer. Creates one if it does not already exit."
  (let* ((name (concat "*" "inter-node-log" "*"))
         (buffer (get-buffer name)))
    (or buffer
        (progn
          (setq buffer (get-buffer-create name))
          (with-current-buffer buffer
            (inter-node-log-mode))
          buffer))))

(defun inter-node-log ()
  "Displays `inter-node-log' buffer. Creates one if it does not already exit."
  (interactive)
  (display-buffer (inter-node--get-log-buffer)))

;;;###autoload
(defun inter-node-eval-expression (js-expr)
  (interactive "sEval NodeJS: ")
  (message (inter-node-do-java-script-sync js-expr)))

(defun inter-node--js2-forward-expression-p ()
  "Returns t if point is looking at \"=\" or \";\" excluding white space."
  (save-excursion
    (js2-forward-sws)
    (or (looking-at "=[^=]*")
        (looking-at "`")
        (looking-at "\\[")
        (looking-at "\\.")
        (looking-at "("))))

(defun inter-node--js2-forward-expression ()
  "Skip forward to the \"very end\" of sexp. Uses `js2-mode-forward-sexp' to
skip forward unconditionally first time and then while
`inter-node--js2-mode-forward-sexp-p' returns t."
  (js2-mode-forward-sexp)
  (while (inter-node--js2-forward-expression-p)
    (js2-mode-forward-sexp)))

(defun inter-node--js2-expression-at-pos-beg-end (pos)
  (let (beg end)
    (save-excursion
      (goto-char pos)
      (js2-forward-sws)
      (when (looking-at "var\\b\\|let\\b\\|const\\b")
        (right-word)
        (js2-forward-sws))
      (setq beg (point))
      (inter-node--js2-forward-expression)
      (setq end (point)))
    (cons beg end)))

(defun inter-node--pos-inside-symbol-p (pos)
  "Returns t if POS is in inside symbol."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (and bounds
         (> pos (car bounds))
         (< pos (cdr bounds)))))

(defun inter-node--pos-at-bol-p (pos)
  "Returns t if POS is in the beginning of line excluding white space."
  (string-blank-p
   (buffer-substring-no-properties (line-beginning-position) pos)))

(defun inter-node--expression-at-pos-beg-end (pos)
  (let (bounds)
    (if (eq major-mode 'js2-mode)
        (if (inter-node--pos-inside-symbol-p pos)
            (setq bounds (bounds-of-thing-at-point 'symbol))
          (setq bounds (inter-node--js2-expression-at-pos-beg-end pos)))
      (if (inter-node--pos-at-bol-p pos)
          (setq bounds (cons (line-beginning-position) (line-end-position)))
        (setq bounds (bounds-of-thing-at-point 'symbol))))
    bounds))

(defun inter-node--eval (beg end)
  (let* ((js-expr (buffer-substring-no-properties beg end))
         (process (get-process inter-node-repl-process-name)))
    (unless process
      (setq process (inter-node-repl t)))
    (with-current-buffer (process-buffer process)
      (inter-node-do-java-script-sync js-expr))))

;;;###autoload
(defun inter-node-eval (beg &optional end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) nil)))
  (when (null end)
    (let ((bounds (inter-node--expression-at-pos-beg-end beg)))
      (setq beg (car bounds)
            end (cdr bounds))))
  (let* ((log-buffer (inter-node--get-log-buffer))
         (input (buffer-substring-no-properties beg end))
         (output (inter-node--eval beg end)))
    (if current-prefix-arg
        (save-excursion
          (end-of-line)
          (newline)
          (insert output))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert (concat "// ["  (current-time-string) "] /b/{ js\n\n"))
        (insert input)
        (insert "\n\n")
        (insert "// /b/= node\n\n")
        (insert output)
        (insert "\n\n")
        (insert "// /b/}\n\n"))
      (unless (get-buffer-window log-buffer 'visible)
        (message output))))
  (unless (use-region-p)
    (pulse-momentary-highlight-region beg end 'next-error)))

;;;###autoload
(defun inter-node-eval-buffer ()
  (interactive)
  (message (inter-node--eval (point-min) (point-max))))

(defvar inter-node-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") #'inter-node-eval)
    (define-key map (kbd "S-<f5>") #'inter-node-eval-expression)
    (define-key map (kbd "C-<f5>") #'inter-node-eval-buffer)
    map))

;;;###autoload
(define-minor-mode inter-node-mode
  "Minor mode for interacting with NodeJS from other (e.g js) buffers."
  :lighter " inter-node"
  :keymap inter-node-mode-keymap
  (if inter-node-mode
      (progn
        (inter-node-repl t)
        (add-hook 'completion-at-point-functions
                  'inter-node--completion-at-point-function nil t))
    (remove-hook 'completion-at-point-functions
                 'inter-node--completion-at-point-function t)))

(define-derived-mode inter-node-log-mode fundamental-mode "inter-node-log"
  "Major mode for inter-node log."
  :lighter " inter-node-log"
  ;; (setq-local buffer-read-only t)
  (setq-local window-point-insertion-type t))

;;; inter-node-mode
;; /b/}

;; -------------------------------------------------------------------
;;; completion functions
;; -------------------------------------------------------------------
;; /b/{

(defun inter-node--extract-completion-input (raw-input)
  ;; Strip '// ... \n' - style comments
  (setq raw-input
        (replace-regexp-in-string "//.*$" "" raw-input))
  ;; Replace all control and white space characters with a single space
  (setq raw-input
        (replace-regexp-in-string "[[:blank:][:cntrl:]]+" " " raw-input))
  ;; Strip '/* ... */' - style comments
  (setq raw-input
        (replace-regexp-in-string "/\\*.*\\*/" "" raw-input))
  ;; Replace multiple space sequences with single space
  (setq raw-input
        (replace-regexp-in-string " +" " " raw-input))
  (setq raw-input (string-trim-left raw-input))
  ;; Remove spaces around '.' operator
  (setq raw-input (replace-regexp-in-string " ?\\. ?" "." raw-input))
  ;; get last valid JavaScript symbol
  (substring raw-input (string-match-p "[[:alnum:]_\\$\\.]*$" raw-input)))

(defun inter-node--extract-completion-prefix (input)
  (substring input (string-match-p "[^\\.]*$" input)))

(defun inter-node--get-completion-raw-input ()
  (let ((regex "[^[:alnum:][:blank:][:cntrl:]_/\\*\\$\\.]")
        end)
    (save-excursion
      (setq end (point))
      (if (fboundp 'js--re-search-backward)
          (js--re-search-backward regex)
        (search-backward-regexp regex))
      (buffer-substring-no-properties (point) end))))

(defun inter-node--in-string-p ()
  "Returns t if point is inside string
see http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html"
  (nth 3 (syntax-ppss)))

(defun inter-node--in-comment-p ()
  "Returns t if point is inside comment
see http://ergoemacs.org/emacs/elisp_determine_cursor_inside_string_or_comment.html"
  (nth 4 (syntax-ppss)))

(defun inter-node--completion-at-point-function ()
  (let (input)
    (if (eq major-mode 'inter-node-repl-mode)
        (when (comint-after-pmark-p)
          (setq input (buffer-substring-no-properties
                       (comint-line-beginning-position)
                       (point))))
      (unless (or (inter-node--in-string-p) (inter-node--in-comment-p))
        (setq input (inter-node--get-completion-raw-input))))
    (when input
      (setq input (inter-node--extract-completion-input input))
      (list (- (point) (length (inter-node--extract-completion-prefix input)))
            (point)
            (inter-node-get-tab-completions-sync input)))))

;; ;;;###autoload
;; (defun company-tide (command &optional arg &rest ignored)
;;   (interactive (list 'interactive))
;;   (cl-case command
;;     (interactive (company-begin-backend 'company-tide))
;;     (prefix (and
;;              (bound-and-true-p tide-mode)
;;              (-any-p #'derived-mode-p tide-supported-modes)
;;              (tide-current-server)
;;              (not (nth 4 (syntax-ppss)))
;;              (or (tide-completion-prefix) 'stop)))
;;     (candidates (cons :async
;;                       (lambda (cb)
;;                         (tide-command:completions arg cb))))
;;     (sorted t)
;;     (ignore-case tide-completion-ignore-case)
;;     (meta (tide-completion-meta arg))
;;     (annotation (tide-completion-annotation arg))
;;     (doc-buffer (tide-completion-doc-buffer arg))
;;     (post-completion (tide-post-completion arg))))

;; (eval-after-load 'company
;;   '(progn
;;      (cl-pushnew 'company-tide company-backends)))

;;; auto-completion functions
;; /b/}

(provide 'inter-node)
