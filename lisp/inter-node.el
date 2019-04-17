;; https://emacs.stackexchange.com/questions/692/asynchronously-wait-for-output-from-a-comint-process
;; https://emacs.stackexchange.com/questions/32449/content-was-split-in-the-functions-of-comint-preoutput-filter-functions

;; -------------------------------------------------------------------
;;; Minimalist Node.js REPL for inter-node minor mode
;; -------------------------------------------------------------------
;; /b/{

(defgroup inter-node nil
  "Node.js REPL and its minor interaction mode"
  :group 'processes)

(defcustom inter-node-repl-prompt "> "
  "Node.js REPL prompt used in `inter-node-repl-mode'"
  :group 'nodejs-repl
  :type 'string)

(defvar inter-node-repl-process-name "node-repl"
  "Process name of Node.js REPL")

(defcustom inter-node-repl-start-js
  (concat
   "let repl = require('repl');"
   ;; Do not split long lines to fit terminal width.
   ;; emacs should wrap or trim long lines instead.
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
   ;; "  writer: output => output,"
   "})")
  "JavaScript expression used to start Node.js REPL"
  :group 'nodejs-repl
  :type 'string)

(defun inter-node--strip-all-ascii-escapes (string)
  "Strip ASCII Terminal Escape Sequences"
  (replace-regexp-in-string "\\[[0-9;]*[a-zA-Z]\\|" "" string))

(defun inter-node--dedup-prompt (string)
  "Deduplicate string with prompt"
  (let* ((p inter-node-repl-prompt)
         (regexp (concat p "\\(.*\\)\\(" p "\\1\\)+")))
    (replace-regexp-in-string regexp "\\2" string)))

(defun inter-node--comint-preoutput-filter (output)
  (setq output (inter-node--strip-all-ascii-escapes output))
  (setq output (inter-node--dedup-prompt output)))

(define-derived-mode inter-node-repl-mode comint-mode "Node.js REPL"
  "Major mode for Node.js REPL"
  (add-hook 'comint-preoutput-filter-functions
            #'inter-node--comint-preoutput-filter nil t)

  ;; (setq-local font-lock-defaults '(nil nil t))

  (setq-local comint-process-echoes t)
  (setq-local comint-prompt-regexp (concat "^" inter-node-repl-prompt))
  (setq-local comint-use-prompt-regexp t)

  (add-hook 'completion-at-point-functions
            'inter-node--completion-at-point-function nil t))

(defun inter-node-repl (&optional bury)
  "Run Node.js REPL"
  (interactive)
  (let ((process (get-process inter-node-repl-process-name))
        buffer)
    (if process
        (setq buffer (process-buffer process))
      (setq buffer (make-comint
                    inter-node-repl-process-name
                    "node" nil "-e" inter-node-repl-start-js))
      (setq process (get-buffer-process buffer)))
    (with-current-buffer buffer (inter-node-repl-mode))
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
        (current-buffer)))))

;;; Minimalist Node.js REPL for inter-repl minor mode
;; /b/}

;; -------------------------------------------------------------------
;;; do-java-script and tab-completions in Node.js REPL
;; -------------------------------------------------------------------
;; /b/{

(defun inter-node--get-buffer-last-line (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-max))
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

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
         (input (concat ".editor\n" string "")))
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
  ;; (let ((input-string-len (length input-string))
  ;;       (prompt-regex (concat "^" inter-node-repl-prompt))
  ;;       beg end output-string)
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
      (nreverse (seq-reduce
       (lambda (result candidate)
         (if (string-empty-p candidate)
             result
           (push (substring candidate prefix-length) result)))
       candidates ()))

      ;; (unless (eq beg end)
      ;;   (delete "" (split-string output-string "[\r\n]+")))
      )))

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
            (process-send-string process "")
            (inter-node--wait-for-prompt process)
            (process-send-string process (concat string "\t"))
            (while (accept-process-output process 0.01))
            (process-send-string process "\t")
            (while (accept-process-output process 0.01))
            (process-send-string process "")
            (inter-node--wait-for-prompt process))
        (set-process-buffer process process-buffer-orig)
        (set-process-filter process process-filter-orig)
        (set-marker (process-mark process)
                    marker-position-orig process-buffer-orig))
      (inter-node--get-tab-completions-output string))))

;;; do-java-script and tab-completions in Node.js REPL
;; /b/}

;; -------------------------------------------------------------------
;;; inter-node mode
;; -------------------------------------------------------------------
;; /b/{

(defun inter-node--eval (beg end)
  (unless end (setq end beg))
  (let ((js-string (if (/= beg end)
                       (buffer-substring-no-properties beg end)
                     (if (eq (point) (line-beginning-position))
                         ;; consider using (js--forward-expression) here...
                         (thing-at-point 'line t)
                       (thing-at-point 'symbol t))))
        (process (get-process inter-node-repl-process-name)))
    (unless process
      (setq process (inter-node-repl t)))
    (with-current-buffer (process-buffer process)
      (inter-node-do-java-script-sync js-string))))

(defun inter-node-eval (beg &optional end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (if current-prefix-arg
      (save-excursion
        (setq output (inter-node--eval beg end))
        (end-of-line)
        (newline)
        (insert output))
    (message (inter-node--eval beg end))))

(defun inter-node-eval-buffer ()
  (interactive)
  (message (inter-node--eval (point-min) (point-max))))

(defvar inter-node-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") #'inter-node-eval)
    (define-key map (kbd "C-<f5>") #'inter-node-eval-buffer)
    map))

(define-minor-mode inter-node
  "Minor mode for interacting with a nodejs from other (e.g js) buffers."
  nil
  :lighter " NodeJS"
  :keymap inter-node-map
  (if inter-node
      (progn
        (inter-node-repl t)
        (add-hook 'completion-at-point-functions
                  'inter-node--completion-at-point-function nil t))
    (remove-hook 'completion-at-point-functions
                 'inter-node--completion-at-point-function t)))

;;; inter-node mode
;; /b/}

;; -------------------------------------------------------------------
;;; Auto-completion functions
;; -------------------------------------------------------------------
;; /b/{

(defun inter-node--extract-completion-input (raw-input)
  (setq raw-input
        (replace-regexp-in-string "[[:blank:][:cntrl:]]+" " " raw-input))
  (setq raw-input (string-trim-left raw-input))
  (setq raw-input (replace-regexp-in-string " ?\\. ?" "." raw-input))
  ;; (substring raw-input (string-match-p "[^[:blank:]]*$" raw-input))
  (substring raw-input (string-match-p "[[:alnum:]_\\$\\.]*$" raw-input)))

(defun inter-node--extract-completion-prefix (input)
  (substring input (string-match-p "[^\\.]*$" input)))

;; TODO: * Do not do anything if inside string
;;       * Remove comments

(defun inter-node--get-completion-raw-input ()
  (let (end)
    (save-excursion
      (setq end (point))
      (search-backward-regexp "[^[:alnum:][:blank:][:cntrl:]_/\\*\\$\\.]")
      (buffer-substring-no-properties (point) end))))

(defun inter-node--completion-at-point-function ()
  (let (input)
    (if (eq major-mode 'inter-node-repl-mode)
        (when (comint-after-pmark-p)
          (setq input (buffer-substring-no-properties
                       (comint-line-beginning-position)
                       (point))))
      (setq input (inter-node--get-completion-raw-input)))
    (when input
      (setq input (inter-node--extract-completion-input input))
      (list (- (point) (length (inter-node--extract-completion-prefix input)))
            (point)
            (inter-node-get-tab-completions-sync input)))))

;;; Auto-completion functions
;; /b/}
