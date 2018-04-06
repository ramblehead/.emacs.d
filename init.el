(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 0)
 '(LaTeX-item-indent 2)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(font-latex-fontify-script nil)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "empheq")))
 '(hfy-default-face-def
   (quote
    ((t :background "black" :foreground "white" :family "misc-fixed"))))
 '(indent-tabs-mode nil)
 '(longlines-show-hard-newlines t)
 '(make-backup-files nil)
 '(package-selected-packages
   (quote
    (total-lines flycheck-pos-tip smart-mode-line indium iflipb flycheck-typescript-tslint yasnippet-snippets tern typescript-mode flycheck company-tern company tide htmlize clang-format modern-cpp-font-lock which-key undo-tree google-c-style picture-mode nlinum-hl magit hlinum highlight-indent-guides nlinum ac-html web-mode async visual-regexp bs-ext popwin sr-speedbar gdb-mix realgud bm web-beautify ac-js2 skewer-mode moz js2-mode pos-tip fuzzy auto-complete paradox flx-ido use-package)))
 '(pop-up-windows nil)
 '(preview-scale-function 1.8)
 '(safe-local-variable-values (quote ((eval progn (linum-mode -1) (nlinum-mode 1)))))
 '(tab-stop-list
   (quote
    (8 4 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
 '(vr/match-separator-string " -> ")
 '(w32shell-cygwin-bin "c:\\tools\\cygwin\\bin")
 '(w32shell-msys-bin "c:\\tools\\mingw\\msys\\1.0\\bin"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-completion-face ((t (:background "light sky blue" :foreground "systemmenutext" :underline t))))
 '(ac-selection-face ((t (:background "light sky blue" :foreground "systemmenutext"))))
 '(completion-dynamic-common-substring-face ((((class color) (background light)) (:background "light steel blue" :foreground "systemmenutext"))))
 '(completion-dynamic-prefix-alterations-face ((((class color) (background light)) (:background "cyan" :foreground "systemmenutext"))))
 '(completion-highlight-face ((((class color) (background light)) (:background "light sky blue" :underline t))))
 '(rtags-errline ((((class color)) (:background "#ef8990"))))
 '(rtags-fixitline ((((class color)) (:background "#ecc5a8"))))
 '(rtags-skippedline ((((class color)) (:background "#c2fada"))))
 '(rtags-warnline ((((class color)) (:background "#efdd6f"))))
 '(speck-mode-line-specked ((((class color)) (:foreground "midnight blue"))))
 '(speck-mode-line-specking ((((class color)) (:foreground "maroon"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))

;; ------------------------------------------------------------------
;;; Emacs Version Variables
;; ------------------------------------------------------------------

;; (setq source-directory "~/rhpm/dotfiles/ubuntu-16.04-desktop/scripts/emacs-snapshot-94060-b7dfd39-emacs")

(setq vr-emacs-version-string
      (replace-regexp-in-string
       "GNU Emacs \\([0-9]+.[0-9]+.[0-9]+\\).*" "\\1"
       (replace-regexp-in-string "\n" "" (emacs-version))))

(setq vr-emacs-version
      (mapcar 'string-to-number (split-string vr-emacs-version-string "\\.")))

;; (setq vr-emacs-version ())
;; (dolist (num (split-string vr-emacs-version-string "\\.") vr-emacs-version)
;;   (setq vr-emacs-version (cons (string-to-number num) vr-emacs-version)))
;; (setq vr-emacs-version (reverse vr-emacs-version))

;; ------------------------------------------------------------------
;;; File Location Variables
;; ------------------------------------------------------------------

(setq vr-site-start-file-paths ())

(cond
 ((equal system-type 'windows-nt)
  (progn
    (setq vr-sumatra-pdf-path
          "C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe")
    (defun vr-getenv (VARIABLE &optional FRAME)
      (replace-regexp-in-string "\\\\" "/" (getenv VARIABLE FRAME)))

    (setq rh-recent-files-file-path
          (concat (vr-getenv "APPDATA") "/.local-emacs.d/recent-files"))
    (setq vr-saved-places-file-path
          (concat (vr-getenv "APPDATA") "/.local-emacs.d/saved-places"))
    (setq vr-bm-repository-file-path
          (concat (vr-getenv "APPDATA") "/.local-emacs.d/bm-repository"))
    (setq vr-ido-last-file-path
          (concat (vr-getenv "APPDATA") "/.emacs.d/ido-last"))))
 ((equal system-type 'gnu/linux)
  (progn
    (setq vr-user-data
          (expand-file-name "~/.local/share/"))
    ;; Make the "~/.local/share/emacs" directories if does not
    ;; already exist
    (if (not (file-exists-p
             (concat vr-user-data "emacs")))
       (make-directory (concat vr-user-data "emacs") t))
    (setq rh-recent-files-file-path
          (concat vr-user-data "emacs/recent-files"))
    (setq vr-saved-places-file-path
          (concat vr-user-data "emacs/saved-places"))
    (setq vr-bm-repository-file-path
          (concat vr-user-data "emacs/bm-repository"))
    (setq vr-ido-last-file-path
          (concat vr-user-data "emacs/ido-last"))
    ;; Paths for the site-start.el files, located in /usr/local/share/emacs/
    ;; which can be maintained by users who are members of group "staff"
    (let ((file-path "/usr/local/share/emacs/site-lisp/site-start.el")
          (ver-file-path (concat "/usr/local/share/emacs/"
                                 vr-emacs-version-string
                                 "/site-lisp/site-start.el")))
      (progn
       (if (file-exists-p file-path)
           (add-to-list 'vr-site-start-file-paths file-path))
       (if (file-exists-p ver-file-path)

           (add-to-list 'vr-site-start-file-paths ver-file-path)))))))

(setq vr-smex-save-file
      (concat user-emacs-directory "smex-items"))
(setq vr-user-lisp-directory-path
      (concat user-emacs-directory "lisp/"))
(setq vr-user-site-start-file-path
      (concat vr-user-lisp-directory-path "site-start.el"))

;; ------------------------------------------------------------------
;;; Load Emacs secret variables
;; ------------------------------------------------------------------

(load "~/.emacs.d/secret.el" t)

;; ------------------------------------------------------------------
;;; Helper functions and common modules
;; ------------------------------------------------------------------

;; == Package initialisation and 'use-package' bootstrap ==
;; see http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/"))

;; Temporary Emacs bug patch.
;; Should be removed after Emacs update (>25.1.50.1)
;; see http://stackoverflow.com/questions/26108655/error-updating-emacs-packages-failed-to-download-gnu-archive
(setq package-check-signature nil)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package paradox
  :config
  (setq paradox-automatically-star nil)
  (paradox-enable)

  (define-key paradox-menu-mode-map (kbd "q") #'rh-quit-window-kill)

  :demand
  :ensure t)

(use-package async
  :demand
  :ensure t)

(use-package cl
  :demand
  :ensure t)

;; == Auxiliary functions ==

(defvar rh-interactively-selected-window nil)

(add-hook
 'buffer-list-update-hook
 (lambda ()
   (setq rh-interactively-selected-window (frame-selected-window))))

(defun rh-window-selected-interactively-p ()
  (eq (selected-window) rh-interactively-selected-window))

(defun vr-point-or-region ()
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (point) (point))))

(defun rh-string-match-regexp-list (regexp-list str)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))

(cl-defun vr-toggle-display (buffer-name &optional (dedicated nil))
  (let* ((buffer (get-buffer buffer-name))
         (buffer-window (get-buffer-window buffer-name)))
    (if buffer
        (if buffer-window
            (delete-window buffer-window)
          (if dedicated
              (set-window-dedicated-p (display-buffer buffer-name) t)
            (display-buffer buffer-name)))
      (message (concat "\"" buffer-name "\""
                       " buffer does not exist.")))))

;; == Outline ellipsis (for org mode, hideshow mode) ==

;; see http://emacs.stackexchange.com/questions/10981/changing-the-appearance-of-org-mode-hidden-contents-ellipsis
;; see http://emacswiki.org/emacs/OutlineMode

;; (set-display-table-slot standard-display-table
;;                         'selective-display (string-to-vector " ◦◦◦ "))

(set-display-table-slot
 standard-display-table
 'selective-display
 (let ((face-offset (* (face-id 'shadow) (lsh 1 22))))
   (vconcat (mapcar (lambda (c) (+ face-offset c)) " [...] "))))

;; == Convenience interactive functions ==

;; My adaptation of the native emacs function balance-windows
(defun vr-balance-windows-horizontally (&optional window-or-frame)
  (interactive)
  (let* ((window
          (cond
           ((or (not window-or-frame)
                (frame-live-p window-or-frame))
            (frame-root-window window-or-frame))
           ((or (window-live-p window-or-frame)
                (window-child window-or-frame))
            window-or-frame)
           (t
            (error "Not a window or frame %s" window-or-frame))))
         (frame (window-frame window)))
    ;; Balance horizontally.
    (window--resize-reset (window-frame window) t)
    (balance-windows-1 window t)
    (when (window--resize-apply-p frame t)
      (window-resize-apply frame t)
      (window--pixel-to-total frame t)
      (run-window-configuration-change-hook frame))))

;; My adaptation of the native emacs function balance-windows
(defun vr-balance-windows-vertically (&optional window-or-frame)
  (interactive)
  (let* ((window
          (cond
           ((or (not window-or-frame)
                (frame-live-p window-or-frame))
            (frame-root-window window-or-frame))
           ((or (window-live-p window-or-frame)
                (window-child window-or-frame))
            window-or-frame)
           (t
            (error "Not a window or frame %s" window-or-frame))))
         (frame (window-frame window)))
    ;; Balance vertically.
    (window--resize-reset (window-frame window))
    (balance-windows-1 window)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame)
      (run-window-configuration-change-hook frame))))

(defun what-face (pos)
  "Alternative to what-cursor-position [C-u C-x =] function
when only symbol face names are needed."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(global-set-key (kbd "<f12>") 'what-face)

(defun rh-quit-window-kill ()
  (interactive)
  (quit-window t))

;; -------------------------------------------------------------------
;;; Emacs Packages Tree (where packages grow)
;; -------------------------------------------------------------------

;; /b/{ == rh-project ==

(setq rh-project-dir-name ".project")

(defun rh-project-get-path ()
  (let ((src-tree-root (locate-dominating-file
                        (file-truename default-directory)
                        rh-project-dir-name)))
    (when src-tree-root
      (file-name-as-directory (concat src-tree-root rh-project-dir-name)))))

(defun rh-project-get-root ()
  (let ((rh-project (rh-project-get-path)))
    (when rh-project
      (abbreviate-file-name
       (expand-file-name (concat rh-project "../"))))))

(cl-defun rh-project-setup (&optional (setup-file-name-base "setup" supplied-p))
  (let ((rh-project (rh-project-get-path)))
    (when rh-project
      (if supplied-p
          (load (concat rh-project setup-file-name-base "-setup.el"))
        (let ((setup-file-name (concat rh-project setup-file-name-base ".el")))
          (when (file-exists-p setup-file-name)
            (load setup-file-name)))))))

(defun rh-project-get-generators-path ()
  (let ((generators-path (concat
                          (rh-project-get-path)
                          "../generators/")))
    (when (file-directory-p generators-path)
      (expand-file-name generators-path))))

;; /b/} == rh-project ==

;; /b/{ == code-groups ==

(defvar-local cg-forward-list-original #'forward-list
  "Original forward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-n")

(defvar-local cg-backward-list-original #'backward-list
  "Original backward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-n")

(setq cg-doxygen-group-open-token "///@{")
(setq cg-doxygen-group-close-token "///@}")

(setq cg-auto-code-group-open-token "/a/{")
(setq cg-auto-code-group-close-token "/a/}")

(setq cg-custom-code-group-open-token "/c/{")
(setq cg-custom-code-group-close-token "/c/}")

(setq cg-block-code-group-open-token "/b/{")
(setq cg-block-code-group-close-token "/b/}")

(defun cg-group-head-regexp (open-token)
  (concat "^.*" open-token ".*$"))

(defun cg-group-tail-regexp (close-token)
  (concat "^.*" close-token ".*$"))

(defun cg-looking-at-group-head (open-token)
  (if (string-match-p
       (concat "^.*" open-token ".*$")
       (thing-at-point 'line t))
      open-token))

(defun cg-looking-at-group-tail (close-token)
  (if (string-match-p
       (concat "^.*" close-token ".*$")
       (thing-at-point 'line t))
      close-token))

(defun cg-group-head-or-tail-length (token line)
  (length
   (replace-regexp-in-string
    (concat "^.*\\(" token ".*\\)[\r\n]?$")
    "\\1"
    line)))

(defun cg-group-reverse-token (token)
  (cond
   ((string= cg-doxygen-group-open-token token)
    cg-doxygen-group-close-token)
   ((string= cg-doxygen-group-close-token token)
    cg-doxygen-group-open-token)
   ((string= cg-auto-code-group-open-token token)
    cg-auto-code-group-close-token)
   ((string= cg-auto-code-group-close-token token)
    cg-auto-code-group-open-token)
   ((string= cg-custom-code-group-open-token token)
    cg-custom-code-group-close-token)
   ((string= cg-custom-code-group-close-token token)
    cg-custom-code-group-open-token)
   ((string= cg-block-code-group-open-token token)
    cg-block-code-group-close-token)
   ((string= cg-block-code-group-close-token token)
    cg-block-code-group-open-token)))

(defun cg-looking-at-auto-code-group-head-or-tail ()
  (cond ((cg-looking-at-group-head
          cg-auto-code-group-open-token)
         cg-auto-code-group-open-token)
        ((cg-looking-at-group-head
          cg-auto-code-group-close-token)
         cg-auto-code-group-close-token)))

(defun cg-looking-at-any-group-head ()
  (cond ((cg-looking-at-group-head
          cg-doxygen-group-open-token)
         cg-doxygen-group-open-token)
        ((cg-looking-at-group-head
          cg-auto-code-group-open-token)
         cg-auto-code-group-open-token)
        ((cg-looking-at-group-head
          cg-custom-code-group-open-token)
         cg-custom-code-group-open-token)
        ((cg-looking-at-group-head
          cg-block-code-group-open-token)
         cg-block-code-group-open-token)))

(defun cg-looking-at-any-group-tail ()
  (cond ((cg-looking-at-group-tail
          cg-doxygen-group-close-token)
         cg-doxygen-group-close-token)
        ((cg-looking-at-group-tail
          cg-auto-code-group-close-token)
         cg-auto-code-group-close-token)
        ((cg-looking-at-group-tail
          cg-custom-code-group-close-token)
         cg-custom-code-group-close-token)
        ((cg-looking-at-group-tail
          cg-block-code-group-close-token)
         cg-block-code-group-close-token)))

(defun cg-search-backward-group-balanced-head ()
  (let ((open-token)
        (close-token)
        (mark-pos (point)))
    (setq close-token (cg-looking-at-any-group-tail))
    (when close-token
      (setq open-token (cg-group-reverse-token close-token))
      (move-beginning-of-line nil)
      (if (cg-looking-at-group-head open-token)
          (search-forward open-token)
        (let ((pos nil)
              (found nil)
              (skip-tail 0))
          (push-mark mark-pos t)
          (while (and (not found)
                      (setq pos (re-search-backward
                                 (concat (cg-group-head-regexp open-token)
                                         "\\|"
                                         (cg-group-tail-regexp close-token)))))
            (if (cg-looking-at-group-tail close-token)
                (incf skip-tail)
              (if (<= skip-tail 0)
                  (setq found t)
                (decf skip-tail))))
          (when (cg-looking-at-group-head open-token)
            (move-end-of-line nil)
            (backward-char (cg-group-head-or-tail-length
                            open-token (thing-at-point 'line t))))
          (point))))))

(defun cg-search-forward-group-balanced-tail ()
  (let ((open-token)
        (close-token)
        (mark-pos (point)))
    (setq open-token (cg-looking-at-any-group-head))
    (when open-token
      (setq close-token (cg-group-reverse-token open-token))
      (move-end-of-line nil)
      (if (cg-looking-at-group-tail close-token)
          (search-backward close-token)
        (let ((pos nil)
              (found nil)
              (skip-tail 0))
          (push-mark mark-pos t)
          (while (and (not found)
                      (setq pos (re-search-forward
                                 (concat (cg-group-head-regexp open-token)
                                         "\\|"
                                         (cg-group-tail-regexp close-token)))))
            (if (cg-looking-at-group-head open-token)
                (incf skip-tail)
              (if (<= skip-tail 0)
                  (setq found t)
                (decf skip-tail))))
          pos)))))

(defun cg-hs-hide-group ()
  (interactive)
  (let ((open-token)
        (close-token))
    (when (cg-looking-at-any-group-tail)
      (cg-search-backward-group-balanced-head))
    (setq open-token (cg-looking-at-any-group-head))
    (when open-token
      (setq close-token (cg-group-reverse-token open-token))
      (move-beginning-of-line nil)
      (let* ((beg (search-forward open-token))
             (end (- (cg-search-forward-group-balanced-tail)
                     (cg-group-head-or-tail-length
                      close-token (thing-at-point 'line t)))))
        (hs-make-overlay beg end 'comment beg end)
        (goto-char beg)))))

(defun cg-hs-toggle-hiding ()
  (interactive)
  (let ((open-token)
        (close-token))
    (setq open-token (cg-looking-at-any-group-head))
    (if open-token
        (setq close-token (cg-group-reverse-token open-token))
      (progn
        (setq close-token (cg-looking-at-any-group-tail))
        (when close-token
          (setq open-token (cg-group-reverse-token close-token)))))
    (if open-token
        (let ((hidden nil)
              (at-tail (cg-looking-at-group-tail close-token)))
          (save-excursion
            (move-beginning-of-line nil)
            (if (cg-looking-at-group-head open-token)
                (progn
                  (move-end-of-line nil)
                  (if (cg-looking-at-group-tail close-token)
                      (setq hidden t)))))
          (if hidden
              (progn
                (move-beginning-of-line nil)
                (search-forward open-token)
                (if (not at-tail)
                    (hs-show-block)))
            (cg-hs-hide-group)))
      (hs-toggle-hiding))))

(defun cg-generate-auto-code (data template)
  (let* ((generators-path (rh-project-get-generators-path))
         (code-gen (concat generators-path "auto-code")))
    (when (and generators-path
               (file-exists-p code-gen))
      (setq code-gen (concat code-gen " " data " " template))
      (insert (shell-command-to-string code-gen)))))

(defun cg-generate-auto-code-group ()
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (open-token cg-auto-code-group-open-token)
         (close-token cg-auto-code-group-close-token)
         (desc-regex (concat
                      "[[:blank:]]*auto-code[[:blank:]]+"
                      "\\([^[:blank:]]+\\)[[:blank:]]+\\([^[:blank:]\r\n]+\\)"
                      ".*[\r\n]?$"))
         (open-regex (concat "^.*" open-token desc-regex))
         (close-regex (concat "^.*" close-token desc-regex))
         (data)
         (template))
    (when (string-match close-regex current-line)
      (cg-search-backward-group-balanced-head)
      (setq current-line (thing-at-point 'line t)))
    (when (string-match open-regex current-line)
      (setq data (replace-regexp-in-string open-regex "\\1" current-line))
      (setq template
            (concat
             (replace-regexp-in-string open-regex "\\2" current-line)
             ".mako"))
      (let ((start) (end))
        (move-beginning-of-line 2)
        (setq start (point))
        (previous-line)
        (cg-search-forward-group-balanced-tail)
        (move-beginning-of-line nil)
        (setq end (point))
        (goto-char start)
        (delete-region start end))
      (cg-generate-auto-code data template))))

(defun cg-forward-list (arg)
  (interactive "^p")
  (if (cg-looking-at-any-group-head)
      (cg-search-forward-group-balanced-tail)
    (if cg-forward-list-original
        (funcall cg-forward-list-original arg)
      (forward-list arg))))

(defun cg-backward-list (arg)
  (interactive "^p")
  (if (cg-looking-at-any-group-tail)
      (cg-search-backward-group-balanced-head)
    (if cg-backward-list-original
        (funcall cg-backward-list-original arg)
      (backward-list arg))))

(defun cg-key-bindings-enable ()
  (local-set-key (kbd "C-S-j") #'cg-hs-toggle-hiding)
  (local-set-key (kbd "C-M-n") #'cg-forward-list)
  (local-set-key (kbd "C-M-p") #'cg-backward-list))

(defun cg-key-bindings-disable ()
  (local-unset-key (kbd "C-S-j"))
  (local-unset-key (kbd "C-M-n"))
  (local-unset-key (kbd "C-M-p")))

(defun code-groups-minor-mode-enable ()
  (hs-minor-mode 1)
  (cg-key-bindings-enable)
  (setq code-groups-minor-mode t))

(defun code-groups-minor-mode-disable ()
  (cg-key-bindings-disable)
  (setq code-groups-minor-mode nil))

(cl-defun code-groups-minor-mode (&optional (enable nil enable-supplied-p))
  (interactive)
  (make-local-variable 'code-groups-minor-mode)
  (if enable-supplied-p
      (if (eq enable -1)
          (code-groups-minor-mode-disable)
        (code-groups-minor-mode-enable))
    (if code-groups-minor-mode
        (code-groups-minor-mode-disable)
      (code-groups-minor-mode-enable))))

;; /b/} == code-groups ==

;; /b/{ == goto-window ==

(defvar g2w-reuse-visible-default t)

;; (defvar g2w-next-display-buffer-ref nil)

(defvar g2w-fallback-display-buffer-func
  ;; 'display-buffer-reuse-window)
  'display-buffer-same-window)

(defvar g2w-display-buffer-commands
  '())

(add-to-list
 'display-buffer-alist
 '((lambda (buffer actions)
     (memq this-command g2w-display-buffer-commands))
   (lambda (buffer alist)
     (if (and (boundp 'g2w-destination-window)
              (memq g2w-destination-window (window-list)))
         (let ((win g2w-destination-window))
           (when (and (bound-and-true-p g2w-reuse-visible)
                      (not (eq (window-buffer win) buffer)))
             (let ((win-reuse
                    (get-buffer-window buffer (selected-frame))))
               (when win-reuse (setq win win-reuse))))
           (window--display-buffer buffer win
                                   'reuse alist
                                   display-buffer-mark-dedicated))
       (funcall g2w-fallback-display-buffer-func buffer alist)))
   (inhibit-same-window . nil)))

(cl-defmacro g2w-display (display-buffer-func
                          &optional (kill-on-quit nil))
  `#'(lambda (buf alist)
       (let ((win (funcall ,display-buffer-func buf alist)))
         (when win
           (with-current-buffer buf
             (set (make-local-variable 'g2w-window-side)
                  (window-parameter win 'window-side))
             (put 'g2w-window-side 'permanent-local t)
             (set (make-local-variable 'g2w-window-slot)
                  (window-parameter win 'window-slot))
             (put 'g2w-window-slot 'permanent-local t)
             (set (make-local-variable 'g2w-quit-restore-parameter)
                  (window-parameter win 'quit-restore))
             (put 'g2w-quit-restore-parameter 'permanent-local t)
             (set (make-local-variable 'g2w-kill-on-quit)
                  ,kill-on-quit)
             (put 'g2w-kill-on-quit 'permanent-local t)))
         win)))

(defun g2w-same-side-and-slot-buffers (buf)
  (with-current-buffer buf
    (let ((buf-side (if (local-variable-p 'g2w-window-side)
                        g2w-window-side nil))
          (buf-slot (if (local-variable-p 'g2w-window-slot)
                        g2w-window-slot nil))
          (same '()) (different '()))
      (mapc (lambda (tbuf)
              (with-current-buffer tbuf
                (let ((tbuf-side (if (local-variable-p 'g2w-window-side)
                                     g2w-window-side nil))
                      (tbuf-slot (if (local-variable-p 'g2w-window-slot)
                                     g2w-window-slot nil)))
                  (if (and (eq tbuf-side buf-side)
                           (eq tbuf-slot buf-slot))
                      (push (buffer-name tbuf) same)
                    (push (buffer-name tbuf) different)))))
            (buffer-list))
      `[,same ,different])))

(defun g2w-quit-window ()
  (interactive)
  (when (local-variable-p 'g2w-quit-restore-parameter)
    (set-window-parameter (frame-selected-window)
                          'quit-restore g2w-quit-restore-parameter))
  (if (local-variable-p 'g2w-kill-on-quit)
      (quit-window g2w-kill-on-quit)
    (quit-window)))

(cl-defmacro g2w-condition
    (condition &optional (reuse-visible g2w-reuse-visible-default))
  `#'(lambda (buffer-nm actions)
       (when (if (stringp ,condition)
                 (string-match-p ,condition buffer-nm)
               (funcall ,condition buffer-nm actions))
         (let ((current-window (frame-selected-window)))
           (with-current-buffer buffer-nm
             (set (make-local-variable 'g2w-destination-window)
                  current-window)
             (put 'g2w-destination-window 'permanent-local t)
             (set (make-local-variable 'g2w-reuse-visible)
                  ,reuse-visible)
             (put 'g2w-reuse-visible 'permanent-local t))
           t))))

(defun g2w-set-destination-window (choice)
  (interactive
   (if (local-variable-p 'g2w-destination-window)
     (let* (value
            (choices (mapcar (lambda (w)
                               (list (format "%s" w) w))
                             (window-list)))
            (completion-ignore-case  t))
       (setq value (list (completing-read
                          "destination-window: " choices nil t)))
       (cdr (assoc (car value) choices 'string=)))
     '(nil)))
  (if (local-variable-p 'g2w-destination-window)
      (progn
        (setq g2w-destination-window choice)
        (select-window choice)
        (message "destination-window: %s" choice)
        choice)
    (progn
      (message "current buffer has no associated `destination-window'")
      nil)))

(defun g2w-select-destination-window ()
  (interactive)
  (if (local-variable-p 'g2w-destination-window)
      (if (member g2w-destination-window (window-list))
          (select-window g2w-destination-window)
        (message "`destination-window' window has been killed"))
    (progn
      (message "current buffer has no associated `destination-window'")
      nil)))

;; /b/} == goto-window ==

;; -------------------------------------------------------------------
;;; Basic System Setup
;; -------------------------------------------------------------------

(add-to-list 'load-path vr-user-lisp-directory-path)
(load vr-user-site-start-file-path nil t t)

(dolist (file-path vr-site-start-file-paths)
  (load file-path nil t t))

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq split-height-threshold nil)
;; (setq split-width-threshold nil)
(setq split-height-threshold 20)
(setq split-width-threshold 90)

;; see http://emacs.stackexchange.com/questions/12709/how-to-save-last-place-of-point-in-a-buffer
(setq save-place-file vr-saved-places-file-path)
(if (<= 25 (car vr-emacs-version))
    (progn
      (require 'saveplace)
      (save-place-mode))
  (progn
    (setq-default save-place t)
    (require 'saveplace)))

(setq default-input-method "russian-computer")

;; No ceremonies
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; emacs is not good in GUI. However, menu can be good for learning
;; new commands.
;; (menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; better frame titles
(setq frame-title-format
      (concat "%b - emacs@" system-name))

;; Zoom as in Firefox (almost)
(require 'zoom-frm)

(global-set-key (kbd "C--") 'zoom-frm-out)
(global-set-key (kbd "C-=") 'zoom-frm-in)
(global-set-key (kbd "C-0") 'zoom-frm-unzoom)
(global-set-key (kbd "C-<kp-subtract>") 'zoom-frm-out)
(global-set-key (kbd "C-<kp-add>") 'zoom-frm-in)
(global-set-key (kbd "C-<kp-0>") 'zoom-frm-unzoom)
(global-set-key (kbd "C-<wheel-up>") 'zoom-frm-in)
(global-set-key (kbd "C-<wheel-down>") 'zoom-frm-out)

;; http://www.emacswiki.org/emacs/SaveHist
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00213.html
(savehist-mode 1)

;; find-file-at-point
;; http://stackoverflow.com/questions/259354/goto-file-in-emacs
(ffap-bindings)

;; filter Echo area annoying messages
;; http://emacswiki.org/emacs/EchoArea
;; (defvar message-filter-regexp-list
;;  '("^Mark set$"
;;    "filter formatted message string to remove noisy messages"))
;;(defadvice message (around message-filter-by-regexp activate)
;;  (if (not (ad-get-arg 0))
;;      ad-do-it
;;    (let ((formatted-string (apply 'format (ad-get-args 0))))
;;      (if (and (stringp formatted-string)
;;               (some (lambda (re)
;;                       (string-match re formatted-string))
;;                message-filter-regexp-list))
;;          (save-excursion
;;            (set-buffer "*Messages*")
;;            (goto-char (point-max))
;;            (insert formatted-string "\n"))
;;        (progn
;;          (ad-set-args 0 `("%s" ,formatted-string))
;;          ad-do-it)))))

;; Do not copy font and font faces on yank
;; http://stackoverflow.com/questions/22024765/how-to-copy-paste-without-source-font-lock-in-emacs
(setq yank-excluded-properties (append '(font face font-lock-face)
                                       yank-excluded-properties))

(use-package help-mode
  :init
  (add-to-list 'display-buffer-alist
               `("*Help*"
                 ,(g2w-display #'display-buffer-in-side-window t)
                 (side . bottom)
                 (slot . 0)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  :config
  (setq help-window-select t)
  (define-key help-mode-map (kbd "q") #'g2w-quit-window)

  :demand t)

(use-package grep
  :init
  (add-to-list 'display-buffer-alist
               `(,(g2w-condition "*grep*")
                 ,(g2w-display #'display-buffer-in-side-window t)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (add-to-list 'g2w-display-buffer-commands 'compile-goto-error)

  :config
  (define-key grep-mode-map (kbd "q") #'g2w-quit-window)

  (add-hook
   'grep-mode-hook
   (lambda ()
     (setq truncate-lines t)))

  :demand t)

(use-package replace
  :init
  (add-to-list 'display-buffer-alist
               `(,(g2w-condition "*Occur*")
                 ,(g2w-display #'display-buffer-in-side-window t)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (add-to-list 'g2w-display-buffer-commands
               'occur-mode-goto-occurrence)

  :config
  (define-key occur-mode-map (kbd "q") #'g2w-quit-window)

  :demand t)

(add-to-list 'display-buffer-alist
             `("*Warnings*"
               ,(g2w-display #'display-buffer-in-side-window t)
               (side . bottom)
               (slot . 0)
               (inhibit-same-window . t)
               (window-height . 15)))

(use-package total-lines
  :config (global-total-lines-mode 1)
  :ensure t
  :demand t)

(use-package rich-minority
  :config
  ;; Common minor modes
  (add-to-list 'rm-blacklist " hs")
  (add-to-list 'rm-blacklist " Undo-Tree")
  (add-to-list 'rm-blacklist " yas")
  (add-to-list 'rm-blacklist " ElDoc")

  ;; Lisp minor modes
  (add-to-list 'rm-blacklist " SliNav")

  ;; JavaScript minor modes
  (add-to-list 'rm-blacklist " tide")
  (add-to-list 'rm-blacklist " js-interaction")

  ;; C++ minor modes
  (add-to-list 'rm-blacklist " mc++fl")

  :demand t)



(use-package smart-mode-line
  :config
  (defun sml/compile-position-construct (&optional symbol value)
    "Recompile the `sml/position-construct' after one of the formats was edited.
Also sets SYMBOL to VALUE."
    (when (and symbol value) (set symbol value))
    (sml/generate-position-help)
    (setq sml/position-construct
          `((total-lines-mode
             (:propertize
              (:eval (format
                      (let ((width (max (length (number-to-string total-lines))
                                        rh-linum-min-digits)))
                        (concat "%" (number-to-string width) "d"))
                      total-lines))
              face 'sml/col-number
              help-echo (concat "Total lines mode\n"
                                 "mouse-1: Display Line "
                                 "and Column Mode Menu")
              mouse-face 'mode-line-highlight
              local-map ,mode-line-column-line-number-mode-map))

            (total-lines-mode
             ,(propertize (concat rh-linum-right-space "/")
                          'face 'sml/numbers-separator
                          'help-echo (concat "Total lines mode\n"
                                            "mouse-1: Display Line "
                                            "and Column Mode Menu")
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map))

            (line-number-mode
             (:propertize
              (:eval (let ((width (max (length (number-to-string total-lines))
                                       rh-linum-min-digits)))
                       (concat "%" (number-to-string width) "l")))
              face 'sml/line-number
              help-echo (concat "Line number mode\n"
                                "mouse-1: Display Line "
                                "and Column Mode Menu")
              mouse-face 'mode-line-highlight
              local-map ,mode-line-column-line-number-mode-map))

            ;; (line-number-mode
            ;;   ,(propertize sml/line-number-format
            ;;                'face 'sml/line-number
            ;;                'help-echo 'sml/position-help-text
            ;;                'mouse-face 'mode-line-highlight
            ;;                'local-map mode-line-column-line-number-mode-map))

            (column-number-mode
             ,(propertize ","
                          'face 'sml/numbers-separator
                          'help-echo (concat "Column number mode\n"
                                             "mouse-1: Display Line "
                                             "and Column Mode Menu")
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map))

            (column-number-mode
             ,(propertize sml/col-number-format
                          'face 'sml/col-number
                          'help-echo (concat "Column number mode\n"
                                             "nmouse-1: Display Line "
                                             "and Column Mode Menu")
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map))

            (size-indication-mode
             ,(propertize sml/size-indication-format
                          'face 'sml/col-number
                          'help-echo 'sml/position-help-text
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map)))))

  ;; (setq sml/position-percentage-format "* %p *")
  ;; (format-mode-line "%c")

  (setq sml/theme 'light)
  (setq sml/show-eol t)
  (setq sml/line-number-format " %4l")
  (setq sml/col-number-format "%3c")
  (setq sml/size-indication-format " %I")

;; ((sml/buffer-identification-filling sml/buffer-identification-filling (:eval (setq sml/buffer-identification-filling (sml/fill-for-buffer-identification)))) (:propertize mode-line-percent-position local-map (keymap (mode-line keymap (down-mouse-1 keymap (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button (:toggle . column-number-mode)) (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button (:toggle . line-number-mode)) (size-indication-mode menu-item "Display Size Indication" size-indication-mode :help "Toggle displaying a size indication in the mode-line" :button (:toggle . size-indication-mode)) "Toggle Line and Column Number Display"))) mouse-face mode-line-highlight help-echo "Size indication mode
;; mouse-1: Display Line and Column Mode Menu"))

  (sml/setup)

  ;; (setq mode-line-front-space
  ;;       (add-to-list
  ;;        'mode-line-front-space
  ;;        '(:eval (propertize
  ;;                 ;; (format "%4d " (line-number-at-pos (point-max)))
  ;;                 (format "%4d" total-lines)
  ;;                 'face 'sml/col-number))))

;;   (setq mode-line-position `(:propertize(-2 (:eval "%p"))))
;;   (setq mode-line-position
;;     `((:propertize
;;        mode-line-percent-position
;;        local-map ,mode-line-column-line-number-mode-map
;;        mouse-face mode-line-highlight
;;        ;; XXX needs better description
;;        help-echo "Size indication mode\n\
;; mouse-1: Display Line and Column Mode Menu")
;;       (size-indication-mode
;;        (8 ,(propertize
;; 	    " of %I"
;; 	    'local-map mode-line-column-line-number-mode-map
;; 	    'mouse-face 'mode-line-highlight
;; 	    ;; XXX needs better description
;; 	    'help-echo "Size indication mode\n\
;; mouse-1: Display Line and Column Mode Menu")))
;;       (line-number-mode
;;        ((column-number-mode
;;          (column-number-indicator-zero-based
;;           (10 ,(propertize
;;                 " (%l,%c)"
;;                 'local-map mode-line-column-line-number-mode-map
;;                 'mouse-face 'mode-line-highlight
;;                 'help-echo "Line number and Column number\n\
;; mouse-1: Display Line and Column Mode Menu"))
;;           (10 ,(propertize
;;                 " (%l,%C)"
;;                 'local-map mode-line-column-line-number-mode-map
;;                 'mouse-face 'mode-line-highlight
;;                 'help-echo "Line number and Column number\n\
;; mouse-1: Display Line and Column Mode Menu")))
;;          (6 ,(propertize
;; 	      " L%l"
;; 	      'local-map mode-line-column-line-number-mode-map
;; 	      'mouse-face 'mode-line-highlight
;; 	      'help-echo "Line Number\n\
;; mouse-1: Display Line and Column Mode Menu"))))
;;        ((column-number-mode
;;          (column-number-indicator-zero-based
;;           (5 ,(propertize
;;                " C%c"
;;                'local-map mode-line-column-line-number-mode-map
;;                'mouse-face 'mode-line-highlight
;;                'help-echo "Column number\n\
;; mouse-1: Display Line and Column Mode Menu"))
;;           (5 ,(propertize
;;                " C%C"
;;                'local-map mode-line-column-line-number-mode-map
;;                'mouse-face 'mode-line-highlight
;;                'help-echo "Column number\n\
;; mouse-1: Display Line and Column Mode Menu"))))))))

  ;; (setq mode-line-front-space
  ;;       (add-to-list
  ;;        'mode-line-front-space
  ;;        '((:propertize (:eval " %p")
  ;;                       mouse-face mode-line-highlight
  ;;                       ;; face 'sml/col-number
  ;;                       face sml/position-percentage
  ;;                       help-echo "Buffer Relative Position"))
  ;;        t))

  (column-number-mode 1)
  (size-indication-mode -1)

  :after (linum total-lines)
  :demand t
  :ensure t)

;; (defvar rh-filtered-line-number-at-pos-interval 0.2)
;; (defvar rh-filtered-line-number-at-pos-waiting nil)
;; (defvar rh-filtered-line-number-at-pos-waiting-cached 0)
;; (defvar rh-filtered-line-number-at-pos-waiting-pos 0)
;; (defvar rh-filtered-line-number-at-pos-waiting-absolute nil)

;; (defun rh-filtered-line-number-at-pos-wait (&optional pos absolute)
;;   (when (not rh-line-number-at-pos-waiting)
;;     (setq rh-line-number-at-pos-waiting t)
;;     (run-at-time
;;      rh-filtered-line-number-at-pos-interval nil
;;      (lambda (pos absolute)
;;        (setq rh-line-number-at-pos-waiting nil)
;;        (setq rh-line-number-at-pos-waiting-cached
;;              (line-number-at-pos (pos absolute))))
;;      pos absolute)))

;; (defun rh-filtered-line-number-at-pos (&optional pos absolute)
;;   (setq rh-line-number-at-pos-waiting-pos pos)
;;   (setq rh-line-number-at-pos-waiting-absolute absolute)
;;   (if rh-line-number-at-pos-waiting
;;       rh-line-number-at-pos-waiting-cached
;;     (progn
;;       (setq rh-line-number-at-pos-waiting-cached
;;             (line-number-at-pos (pos absolute)))
;;       (rh-line-number-at-pos-wait)
;;       rh-line-number-at-pos-waiting-cached)))

(use-package autorevert
  :config
  (setq auto-revert-mode-text " ⭯")
  (setq auto-revert-tail-mode-text " ⭳")

  :demand t)

;; -------------------------------------------------------------------
;;; Text Editor
;; -------------------------------------------------------------------

;; /b/{ == unicode-fonts ==

;; see https://emacs.stackexchange.com/questions/251/line-height-with-unicode-characters
;; Disabled due to very long startup time (> 30 sec)
;; (use-package unicode-fonts
;;   :config
;;   (unicode-fonts-setup)

;;   :ensure t)

;; /b/} == unicode-fonts ==

;; == Basic Functionality ==

(setq undo-limit (* 1024 1024))
(setq undo-strong-limit (* undo-limit 2))
(setq undo-outer-limit (* undo-limit 100))

(prefer-coding-system 'utf-8-unix)
;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; Disable horizontal-scrolling "snap" behaviour, which prevents scrolling
;; when cursor is within a certain distance from the left edge.
(setq hscroll-snap-threshold 0)

(when (and (eq window-system 'x)
           (string-match "GTK+" (version)))
  (setq focus-follows-mouse t))

;; Allow cursor to be at a scrolled edge.
(setq hscroll-margin 0)

;; (setq default-tab-width 2)
(setq tab-width 8)
(setq standard-indent 2)
(setq-default fill-column 80)

(setq visible-bell t)
;; see http://emacs.stackexchange.com/questions/10307/how-to-center-the-current-line-vertically-during-isearch
(setq isearch-allow-scroll t)
(require 'fill-column-indicator)

;; Override text selection on typing
;; (i.e. non-persistent selection)
(delete-selection-mode t)

(defun vr-kill-ring-save-after-keep-mark (&rest _)
  (setq deactivate-mark nil))

(advice-add 'kill-ring-save :after #'vr-kill-ring-save-after-keep-mark)

;; see https://www.emacswiki.org/emacs/KillingAndYanking
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

;; == smooth scrolling ==

;; (setq-default scroll-up-aggressively 0.01)
;; (setq-default scroll-down-aggressively 0.01)
(setq scroll-conservatively 100000)
;; (setq scroll-step 5)
(setq scroll-margin 0)
(setq scroll-preserve-screen-position 1)

;; YA "Smooth" scrolling algorithm. Taken here:
;; http://web.archive.org/web/20061025212623/http://www.cs.utexas.edu/users/hllu/EmacsSmoothScrolling.html
;; It does not work very well. I keep it here because demonstrates how to
;; get and set point coordinates.

;; (defun point-of-beginning-of-bottom-line ()
;;   (save-excursion
;;     (move-to-window-line -1)
;;     (point)))

;; (defun point-of-beginning-of-line ()
;;   (save-excursion
;;     (beginning-of-line)
;;     (point)))

;; (defun point-of-beginning-of-top-line ()
;;   (save-excursion
;;     (move-to-window-line 0)
;;     (point)))

;; (defun next-one-line ()
;;   (interactive)
;;   (if (= (point-of-beginning-of-bottom-line) (point-of-beginning-of-line))
;;       (scroll-up 1)
;;     (next-line 1)))

;; (defun previous-one-line ()
;;   (interactive)
;;   (if (= (point-of-beginning-of-top-line) (point-of-beginning-of-line))
;;       (previous-line 1)
;;     (previous-line 1)))

;; (global-set-key (kbd "<down>") 'next-one-line)
;; (global-set-key (kbd "<up>") 'previous-one-line)

(defun vr-scroll-down-one-line ()
  (interactive)
  (scroll-down 1))

(defun vr-scroll-up-one-line ()
  (interactive)
  (scroll-up 1))

;; TODO: Review  these keys for windows resizing
;; up/down keys "like in Adobe Reader"
(global-set-key (kbd "M-<down>") 'vr-scroll-up-one-line)
(global-set-key (kbd "M-<kp-down>") 'vr-scroll-up-one-line)
(global-set-key (kbd "M-<up>") 'vr-scroll-down-one-line)
(global-set-key (kbd "M-<kp-up>") 'vr-scroll-down-one-line)

;; == Mouse Operations ==

(defadvice mouse-yank-at-click (around vr-mouse-yank-at-click () activate)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end)))
  ad-do-it)

(global-set-key (kbd "<mouse-2>") #'mouse-yank-at-click)
(global-set-key (kbd "<mouse-3>") #'kill-ring-save)

;; (ad-activate 'mouse-yank-at-click)

(setq mouse-wheel-scroll-amount '(5 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)
(setq mouse-drag-copy-region nil)
(setq mouse-yank-at-point t)

;; == whitespace mode ==

(setq whitespace-style
      '(face
        tabs
        spaces
        trailing
        newline
        space-before-tab
        space-after-tab
        space-mark
        tab-mark
        newline-mark))

;; see http://xahlee.org/emacs/whitespace-mode.html
;; make whitespace-mode to use "MS Word-style" characters.
;; together with the rest of its defaults
(setq whitespace-display-mappings
  '((space-mark 32 [183] [46])          ; normal space, ·
    ;; (space-mark 32 [32] [32])           ; visual-line-mode friendly space
    (space-mark 160 [164] [95])
    (space-mark 2208 [2212] [95])
    (space-mark 2336 [2340] [95])
    (space-mark 3616 [3620] [95])
    (space-mark 3872 [3876] [95])
    (newline-mark 10 [182 10] [36 10])  ; newlne, ¶
    (tab-mark 9 [187 9] [92 9])         ; tab, »
    ;; (tab-mark 9 [9654 9] [92 9])        ; tab, ▶
    ;; (tab-mark 9 [9655 9] [92 9])        ; tab, ▷
))

;; == SrSpeedbar Mode ==

(use-package sr-speedbar
  :ensure t)

;; == visual-regexp ==

(use-package visual-regexp
  :config
  (setq vr/match-separator-use-custom-face t)
  (custom-set-variables '(vr/match-separator-string " -> "))

  (define-key vr/minibuffer-keymap (kbd "C-j") 'newline)
  (define-key vr/minibuffer-keymap (kbd "C-<return>") 'newline)

  (global-set-key (kbd "C-c v") 'vr/replace)
  (global-set-key (kbd "C-c q") 'vr/query-replace)
  ;; (define-key global-map (kbd "C-c v") 'vr/replace)
  ;; (define-key global-map (kbd "C-c q") 'vr/query-replace)
  ;; (define-key global-map (kbd "M-%") 'vr/replace)
  ;; (define-key global-map (kbd "C-M-%") 'vr/query-replace)

  :ensure t)

;; == picture ==

(use-package picture
  :config
  (add-hook
   'picture-mode-hook
   (lambda ()
     (set (make-local-variable 'vr-picture-show-trailing-whitespace)
          show-trailing-whitespace)
     (setq show-trailing-whitespace nil))

   (defadvice picture-mode-exit (after vr-picture-mode-exit (&optional nostrip))
     ;; Suppress "Warning (bytecomp): reference to free variable ..."
     (defvar vr-picture-show-trailing-whitespace)
     (setq show-trailing-whitespace vr-picture-show-trailing-whitespace))))

;; == undo operations ==

(use-package undo-tree
  :config
  ;; (global-undo-tree-mode)
  ;; (global-set-key (kbd "C-z") #'undo-tree-undo)
  ;; (global-set-key (kbd "C-S-z") #'undo-tree-redo)
  ;; (global-set-key (kbd "C-M-z") #'undo-tree-visualize)

  (define-key undo-tree-map (kbd "C-z") #'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-S-z") #'undo-tree-redo)
  (define-key undo-tree-map (kbd "C-M-z") #'undo-tree-visualize)

  ;; (add-hook
  ;;  'undo-tree-mode-hook
  ;;  (lambda ()
  ;;    (global-set-key (kbd "C-z") #'undo-tree-undo)
  ;;    (global-set-key (kbd "C-z") #'undo-tree-mode)
  ;;    (global-set-key (kbd "C-S-z") #'undo-tree-redo)
  ;;    (global-set-key (kbd "C-M-z") #'undo-tree-visualize))

  ;;  (define-key c-mode-base-map (kbd "C-c r d") #'vr-c++-rtags-toggle-rdm-display)
  ;;  (local-set-key (kbd "C-S-j") #'cg-hs-toggle-hiding)

  :ensure t)

(use-package which-key
  :init
  (setq which-key-side-window-max-height 0.2)

  :ensure t)

;; -------------------------------------------------------------------
;;; File Management
;; -------------------------------------------------------------------

;; == dired ==

(put 'dired-find-alternate-file 'disabled nil)

(if (equal system-type 'windows-nt)
    ;; In MS Windows systems
    (progn
      (setq dired-listing-switches "-alhgG")
      (setq vr-dired-coding-system 'cp1251))
  (progn
    ;; In unix-like systems
    ;; Sort dirs and files in dired as in "C"
    (setenv "LC_COLLATE" "C")
    (setq dired-listing-switches
          "--group-directories-first --time-style=long-iso -alhD")
    (setq vr-dired-coding-system nil)))

(defun vr-no-ido-find-alternate-file (file)
  (interactive "FFind file: ")
  (find-alternate-file file))

(defun vr-dired-cancel ()
  (interactive)
  (kill-buffer (current-buffer))
  (message "dired canceled"))

(defun vr-dired-guess-dir ()
  "Starts dired in buffer-file-name directory or in '~', if buffer has no
filename associated with it."
  (interactive)
  (progn
    (dired (let ((fnm (buffer-file-name)))
             (if fnm
                 (file-name-directory fnm)
               "~")))))

(defun vr-move-to-parent-dir ()
  (interactive)
  (find-alternate-file ".."))

(defun vr-dired-mode-setup ()
  (make-local-variable 'coding-system-for-read)
  (setq coding-system-for-read vr-dired-coding-system)
  (hl-line-mode 1)
  (define-key dired-mode-map (kbd "C-x C-f") 'vr-no-ido-find-alternate-file)
  (define-key dired-mode-map (kbd "<escape>") 'vr-dired-cancel)
  (define-key dired-mode-map (kbd "<backspace>") 'vr-move-to-parent-dir)
  ;; The following solution does not work,
  ;; investigate alternative implementations.
  ;;  (define-key dired-mode-map (kbd "<mouse-1>") 'dired-mouse-find-file-other-window)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<kp-enter>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "C-<return>") 'dired-find-file)
  (define-key dired-mode-map (kbd "C-<kp-enter>") 'dired-find-file))

(add-hook 'dired-mode-hook 'vr-dired-mode-setup)

(global-set-key (kbd "C-x d") 'vr-dired-guess-dir)

;; /b/{ == recentf ==

(use-package recentf
  :config
  (defun recentf-open-files-item (menu-element)
    "Return a widget to display MENU-ELEMENT in a dialog buffer."
    (if (consp (cdr menu-element))
        ;; Represent a sub-menu with a tree widget
        `(tree-widget
          :open t
          :match ignore
          :node (item :tag ,(car menu-element)
                      :sample-face bold
                      :format "%{%t%}:\n")
          ,@(mapcar 'recentf-open-files-item
                    (cdr menu-element)))
      ;; Represent a single file with a link widget
      `(link :tag ,(car menu-element)
             :button-prefix ""
             :button-suffix ""
             :button-face default
             ;; :button-face highlight
             ;; TODO: I corrected format string to show right mouse hovering
             ;;       highlight. Find how to send patch/bug report
             ;;       to recenff maintainer after more testing.
             :format "%[%t%]\n"
             :help-echo ,(concat "Open " (cdr menu-element))
             :action recentf-open-files-action
             ;; Override the (problematic) follow-link property of the
             ;; `link' widget (bug#22434).
             :follow-link nil
             ,(cdr menu-element))))

  (setq recentf-save-file rh-recent-files-file-path)
  (setq recentf-kill-buffer-on-open t)
  (setq recentf-max-saved-items 100)

  (setq rh-ignore-recentf '(;; AUCTeX output files
                            "\\.aux\\'"
                            "\\.bbl\\'"
                            "\\.blg\\'"
                            " output\\*$"))

  (defun rh-recentf-open-edit ()
    (interactive)
    (when (not (local-variable-p 'recentf-edit-list))
      (kill-buffer)
      (recentf-edit-list)))

  (defun rh-recentf-nil-if-recentf-edit ()
    (interactive)
    (if (local-variable-p 'recentf-edit-list) nil
      (rh-recentf-open-edit)))

  (defsubst rh-file-was-visible-p (file)
    "Return non-nil if FILE's buffer exists and has been displayed."
    (let ((buf (find-buffer-visiting file)))
      (when buf
        (let ((display-count (buffer-local-value 'buffer-display-count buf)))
          (if (> display-count 0) display-count nil)))))

  (defsubst rh-keep-default-and-visible-recentf-p (file)
    "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed, and FILE does not mach rh-ignore-recentf
regexp-list."
    (if (and (recentf-keep-default-predicate file)
             (not (rh-string-match-regexp-list rh-ignore-recentf file)))
        (rh-file-was-visible-p file)))

  (global-set-key (kbd "<f4>") 'recentf-open-files)
  (define-key recentf-dialog-mode-map (kbd "<escape>") 'recentf-cancel-dialog)
  (define-key recentf-dialog-mode-map (kbd "<space>") 'widget-button-press)
  (define-key recentf-dialog-mode-map (kbd "<f4>")
    'rh-recentf-nil-if-recentf-edit)

  (add-hook
   'recentf-dialog-mode-hook
   (lambda ()
     (setq cursor-type normal-cursor-type)))

  (recentf-mode 1)

  ;; When a buffer is closed, remove the associated file from the recentf
  ;; list if (1) recentf would have, by default, removed the file, or
  ;; (2) the buffer was never displayed.
  ;; see http://www.emacswiki.org/RecentFiles#toc16
  (setq recentf-keep '(rh-keep-default-and-visible-recentf-p))

  :demand t)

;; (require 'recentf)

;; (defun recentf-open-files-item (menu-element)
;;   "Return a widget to display MENU-ELEMENT in a dialog buffer."
;;   (if (consp (cdr menu-element))
;;       ;; Represent a sub-menu with a tree widget
;;       `(tree-widget
;;         :open t
;;         :match ignore
;;         :node (item :tag ,(car menu-element)
;;                     :sample-face bold
;;                     :format "%{%t%}:\n")
;;         ,@(mapcar 'recentf-open-files-item
;;                   (cdr menu-element)))
;;     ;; Represent a single file with a link widget
;;     `(link :tag ,(car menu-element)
;;            :button-prefix ""
;;            :button-suffix ""
;;            :button-face default
;;            ;; :button-face highlight
;;            ;; TODO: I corrected format string to show right mouse hovering
;;            ;;       highlight. Find how to send patch/bug report
;;            ;;       to recenff maintainer after more testing.
;;            :format "%[%t%]\n"
;;            :help-echo ,(concat "Open " (cdr menu-element))
;;            :action recentf-open-files-action
;;            ;; Override the (problematic) follow-link property of the
;;            ;; `link' widget (bug#22434).
;;            :follow-link nil
;;            ,(cdr menu-element))))

;; (setq rh-ignore-recentf '(;; AUCTeX output files
;;                           "\\.aux\\'"
;;                           "\\.bbl\\'"
;;                           "\\.blg\\'"
;;                           " output\\*$"))

;; (setq recentf-save-file rh-recent-files-file-path)
;; (setq recentf-kill-buffer-on-open t)
;; (setq recentf-max-saved-items 100)

;; (recentf-mode 1)

;; (defun rh-recentf-open-edit ()
;;   (interactive)
;;   (when (not (local-variable-p 'recentf-edit-list))
;;     (kill-buffer)
;;     (recentf-edit-list)))

;; (defun rh-recentf-nil-if-recentf-edit ()
;;   (interactive)
;;   (if (local-variable-p 'recentf-edit-list) nil
;;     (rh-recentf-open-edit)))

;; (defsubst rh-file-was-visible-p (file)
;;   "Return non-nil if FILE's buffer exists and has been displayed."
;;   (let ((buf (find-buffer-visiting file)))
;;     (if buf
;;       (let ((display-count (buffer-local-value 'buffer-display-count buf)))
;;         (if (> display-count 0) display-count nil)))))

;; (defsubst rh-keep-default-and-visible-recentf-p (file)
;;   "Return non-nil if recentf would, by default, keep FILE, and
;; FILE has been displayed, and FILE does not mach rh-ignore-recentf
;; regexp-list."
;;   (if (and (recentf-keep-default-predicate file)
;;            (not (rh-string-match-regexp-list rh-ignore-recentf file)))
;;       (rh-file-was-visible-p file)))

;; ;; When a buffer is closed, remove the associated file from the recentf
;; ;; list if (1) recentf would have, by default, removed the file, or
;; ;; (2) the buffer was never displayed.
;; ;; see http://www.emacswiki.org/RecentFiles#toc16
;; (setq recentf-keep '(rh-keep-default-and-visible-recentf-p))

;; (global-set-key (kbd "<f4>") 'recentf-open-files)
;; (define-key recentf-dialog-mode-map (kbd "<escape>") 'recentf-cancel-dialog)
;; (define-key recentf-dialog-mode-map (kbd "<space>") 'widget-button-press)
;; (define-key recentf-dialog-mode-map (kbd "<f4>")
;;   'rh-recentf-nil-if-recentf-edit)

;; (defun rh-recentf-dialog-mode-hook-function ()
;;   (setq cursor-type normal-cursor-type))

;; (add-hook 'recentf-dialog-mode-hook 'rh-recentf-dialog-mode-hook-function)

;; /b/} == recentf ==

;; == Internal ls (ls-lisp) ==

;; ls-lisp is mainly set for windows

;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/ls-in-Lisp.html
;; giving this as an option to sr-listing-sw barfs
(setq ls-lisp-dirs-first t)
;; ignores sorting case
(setq ls-lisp-ignore-case t)
;; this turns off the owner and group
(setq ls-lisp-verbosity '(links))
;; time and data format
(setq ls-lisp-use-localized-time-format t)
(setq ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M"))

;; The following code removes files permissions from internal ls.
;; see http://www.emacswiki.org/emacs/LsLispToggleVerbosity
(defadvice ls-lisp-format (around my-ls-lisp-format)
  "Advice definition which removes unnecessary information
during file listing in dired.  For such purposes
`ls-lisp-verbosity' customized variable can be used, but
even if it is equal to nil dired will display file
permissions field like \"drwxrwxrwx\".\.  So here we just
get full control to what dired shows and leave only those
fields which we need."
  (progn
    ad-do-it
    (setq ad-return-value (concat
      (substring ad-return-value 0 1)
      (substring ad-return-value 13)))))

(ad-activate 'ls-lisp-format)

;; -------------------------------------------------------------------
;;; Smart Autocompletion and IntelliSense Tools
;; /b/{ +++++++++ ----------------------------------------------------

;; /b/{ == yasnippet ==

(use-package yasnippet
  :config
  (yas-reload-all)
  :ensure t)

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

;; /b/} == yasnippet ==

;; /b/{ == auto-complete ==

(defun vr-ac-add-buffer-dict (dict)
  (when (not (local-variable-p 'ac-dictionary-files))
    (let ((ac-dictionary-files-global (append ac-dictionary-files)))
      (make-local-variable 'ac-dictionary-files)
      (setq ac-dictionary-files ac-dictionary-files-global)))
  (if (file-exists-p dict)
      (add-to-list 'ac-dictionary-files dict t)
    (setq ac-dictionary-files
          (append ac-dictionary-files
                  (mapcar (lambda (dir)
                            (let ((file (concat dir "/" dict)))
                              (if (file-exists-p file) file nil)))
                          ac-dictionary-directories)))))

(defun vr-ac-remove-buffer-dict (dict)
  (when (local-variable-p 'ac-dictionary-files)
    (setq ac-dictionary-files
          (remove-if (lambda (elem)
                       (string-match-p dict elem))
                     ac-dictionary-files))))

(use-package fuzzy
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package ac-html
  :ensure t)

(use-package auto-complete
  :config
  (setq ac-modes (delq 'js2-mode ac-modes))

  (ac-config-default)

  (setq ac-fuzzy-enable t)
  (setq ac-use-quick-help nil)
  (setq ac-auto-show-menu nil)
  (setq ac-use-menu-map t)

  (setq ac-user-dictionary-files
        (delete "~/.dict" ac-user-dictionary-files))

  (define-key ac-completing-map (kbd "<tab>") 'ac-complete)
  (define-key ac-completing-map (kbd "<escape>") 'ac-stop)
  (define-key ac-completing-map (kbd "<delete>") 'ac-stop)
  (define-key ac-completing-map (kbd "<kp-delete>") 'ac-stop)
  (define-key ac-completing-map (kbd "<return>") 'newline)
  (define-key ac-completing-map (kbd "<kp-enter>") 'newline)

  (define-key ac-completing-map (kbd "<up>") nil)
  (define-key ac-completing-map (kbd "<down>") nil)
  (define-key ac-completing-map (kbd "<kp-up>") nil)
  (define-key ac-completing-map (kbd "<kp-down>") nil)

  ;; quick help scrolling only works in text mode tooltips (i.e. no pos-tip)
  (define-key ac-completing-map (kbd "C-<up>") 'ac-quick-help-scroll-up)
  (define-key ac-completing-map (kbd "C-<down>") 'ac-quick-help-scroll-down)

  (define-key ac-completing-map (kbd "C-<tab>") 'auto-complete)

  (define-key ac-menu-map (kbd "C-<tab>") 'ac-next)
  (define-key ac-menu-map (kbd "C-S-<tab>") 'ac-previous)
  (define-key ac-menu-map (kbd "C-S-<iso-lefttab>") 'ac-previous)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "<kp-up>") 'ac-previous)
  (define-key ac-menu-map (kbd "<kp-down>") 'ac-next)
  (define-key ac-menu-map (kbd "<up>") 'ac-previous)
  (define-key ac-menu-map (kbd "<down>") 'ac-next)
  (define-key ac-menu-map (kbd "<return>") 'ac-complete)
  (define-key ac-menu-map (kbd "<kp-enter>") 'ac-complete)

  (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  (define-key ac-completing-map (kbd "M-H") 'ac-persist-help)

  (define-key ac-mode-map (kbd "M-h") 'ac-last-quick-help)
  (define-key ac-mode-map (kbd "M-H") 'ac-last-persist-help)

  :ensure t)

(defun vr-ac-start-if-ac-mode ()
  (interactive)
  (cond
   ((cg-looking-at-auto-code-group-head-or-tail)
    (cg-generate-auto-code-group))
   ((bound-and-true-p auto-complete-mode)
    (auto-complete))
   ((bound-and-true-p company-mode)
    (company-complete))

    ;; (ignore-errors
    ;;   (auto-complete))
    ;; nil)

    ;; (unwind-protect
    ;;     (auto-complete)
    ;;   (message "Cleaning up...")))

    ;; (message "Cleaning up..."))
   (t
    (message "No auto-completion engine is running or nothing to complete.")))

  ;; (if (bound-and-true-p auto-complete-mode)
  ;;     (auto-complete)
  ;;   (message "No auto-completion running or nothing to complete."))
  )

(global-set-key (kbd "C-<tab>") 'vr-ac-start-if-ac-mode)
(global-set-key (kbd "<f7>") 'auto-complete-mode)

;; /b/} == auto-complete ==

;; /b/{ == company ==

;; (defun rh-test ()
;;   (interactive)
;;   (if (company-tooltip-visible-p)
;;       (message "visible")
;;     (message "invisible")))

;; (global-set-key (kbd "C-t") 'rh-test)

(use-package company
  :init
  (defvar rh-company-display-permanent-doc-buffer nil)

  :config
  (setq company-lighter-base "CA")

  ;; TODO: write to https://github.com/company-mode/company-mode/issues/123
  (defun rh-company-pseudo-tooltip-on-explicit-action (command)
    (cl-case command
      (hide (company-pseudo-tooltip-frontend command)
            (company-preview-frontend command))
      (t (if (company-explicit-action-p)
             (company-pseudo-tooltip-frontend command)
           (company-preview-frontend command)))))

  (defmacro rh-company-tooltip-key (default-key cmd)
    `#'(lambda ()
         (interactive)
         (if (company-tooltip-visible-p)
             (funcall ,cmd)
           (let ((default-cmd (or (local-key-binding ,default-key)
                                  (global-key-binding ,default-key))))
             (when (fboundp default-cmd) (funcall default-cmd))))))

  (defmacro rh-company-tooltip-cmd (default-cmd cmd)
    `#'(lambda ()
         (interactive)
         (if (company-tooltip-visible-p)
             (funcall ,cmd)
           (funcall ,default-cmd))))

  (setq company-tooltip-align-annotations t)
  (setq company-echo-truncate-lines nil)
  (setq company-minimum-prefix-length 1)
  (setq company-frontends '(rh-company-pseudo-tooltip-on-explicit-action
                            company-preview-frontend
                            company-echo-metadata-frontend))
  (setq company-require-match nil)

  (setq company-idle-delay 0)

  ;; Use "M-h" for company-show-doc-buffer
  (define-key company-active-map (kbd "<f1>") nil)
  (define-key company-active-map (kbd "C-h") nil)
  ;; Use company-filter-candidates by default, i.e. C-s
  ;; In search mode use C-o to switch between filtered and unfiltered
  (define-key company-active-map (kbd "C-M-s") nil)
  ;; Use some other tools for code navigation
  (define-key company-active-map (kbd "C-w") nil)

  (define-key company-active-map (kbd "<escape>") #'company-abort)

  (define-key company-active-map (kbd "C-n")
    (rh-company-tooltip-key (kbd "C-n") #'company-select-next))
  (define-key company-active-map (kbd "C-p")
    (rh-company-tooltip-key (kbd "C-p") #'company-select-previous))
  (define-key company-active-map (kbd "<down>")
    (rh-company-tooltip-key (kbd "<down>") #'company-select-next))
  (define-key company-active-map (kbd "<up>")
    (rh-company-tooltip-key (kbd "<up>") #'company-select-previous))

  (define-key company-active-map (kbd "<return>")
    (rh-company-tooltip-key (kbd "RET") #'company-complete-selection))
  (define-key company-active-map (kbd "<kp-return>")
    (rh-company-tooltip-key (kbd "RET") #'company-complete-selection))

  (define-key company-active-map (kbd "C-s")
    (rh-company-tooltip-key (kbd "C-s") #'company-filter-candidates))

  (define-key company-active-map (kbd "M-h")
    (lambda ()
      (interactive)
      (when (fboundp rh-company-display-permanent-doc-buffer)
        (funcall rh-company-display-permanent-doc-buffer))
      (company-show-doc-buffer)))

  ;; (define-key company-active-map (kbd "M-h") #'company-show-doc-buffer)
  ;; (define-key company-active-map (kbd "M-i") #'company-show-doc-buffer)

  (define-key company-active-map [remap scroll-up-command]
    (rh-company-tooltip-cmd #'scroll-up-command #'company-next-page))
  (define-key company-active-map [remap scroll-down-command]
    (rh-company-tooltip-cmd #'scroll-down-command #'company-previous-page))

  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)

  (define-key company-active-map (kbd "C-<tab>") #'company-select-next)
  (define-key company-active-map (kbd "C-S-<tab>") #'company-select-previous)
  (define-key company-active-map
    (kbd "C-S-<iso-lefttab>") #'company-select-previous)

  ;; (define-key company-search-map (kbd "M-n") 'nil)
  ;; (define-key company-search-map (kbd "M-p") 'nil)

  (define-key company-search-map (kbd "<escape>") #'company-search-abort)

  (define-key company-search-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-search-map (kbd "TAB") #'company-complete-selection)
  (define-key company-search-map (kbd "C-n") #'company-select-next)
  (define-key company-search-map (kbd "C-p") #'company-select-previous)
  (define-key company-search-map (kbd "<down>") #'company-select-next)
  (define-key company-search-map (kbd "<up>") #'company-select-previous)
  (define-key company-search-map (kbd "<kp-down>") #'company-select-next)
  (define-key company-search-map (kbd "<kp-up>") #'company-select-previous)

  ;; (custom-set-faces
  ;;  '(company-preview
  ;;    ((t (:foreground "darkgray" :underline t))))
  ;;  '(company-preview-common
  ;;    ((t (:inherit company-preview))))
  ;;  '(company-tooltip
  ;;    ((t (:background "lightgray" :foreground "black"))))
  ;;  '(company-tooltip-selection
  ;;    ((t (:background "steelblue" :foreground "white"))))
  ;;  '(company-tooltip-common
  ;;    ((((type x)) (:inherit company-tooltip :weight bold))
  ;;     (t (:inherit company-tooltip))))
  ;;  '(company-tooltip-common-selection
  ;;    ((((type x)) (:inherit company-tooltip-selection :weight bold))
  ;;     (t (:inherit company-tooltip-selection)))))

  :ensure t)

;; /b/} == company ==

;; /b/{ == flycheck ==

(use-package flycheck
  :config
  (setq flycheck-mode-line-prefix "Φ")
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; (require 'web-mode)
  ;; (require 'tide)
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)

  :ensure t)

(use-package flycheck-pos-tip
  :config
  (setq flycheck-pos-tip-timeout -1)
  (flycheck-pos-tip-mode)

  :after flycheck
  :ensure t)

;; (use-package flycheck-popup-tip
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)
;;   (setq flycheck-popup-tip-error-prefix "> ")

;;   :after flycheck
;;   :ensure t
;;   :disabled)

;; /b/} == flycheck ==

;; /b/{ == Common Functions ==

(defvar rh-echo-metadata-enabled t)

(defun rh-echo-metadata-enabled-toggle ()
  (interactive)
  (if company-mode
      (if (memq 'company-echo-metadata-frontend company-frontends)
          (progn
            (setq company-frontends
                  (delete 'company-echo-metadata-frontend company-frontends))
            (setq rh-echo-metadata-enabled nil)
            (message "echo-metadata disabled"))
        (progn
          (add-to-list 'company-frontends 'company-echo-metadata-frontend)
          (setq rh-echo-metadata-enabled t)
          (message "echo-metadata enabled")))
    (setq rh-echo-metadata-enabled (not rh-echo-metadata-enabled))))

(global-set-key (kbd "M-i") #'rh-echo-metadata-enabled-toggle)

;; /b/} == Common Functions ==

;; -------------------------------------------------------------------
;;; Smart Autocompletion and IntelliSense Tools
;; ++++++++++ /b/} ---------------------------------------------------

;; -------------------------------------------------------------------
;;; Programming Languages, Debuggers, Profilers, Shells etc.
;; -------------------------------------------------------------------

;; /b/{ == compile ==

(use-package compile
  :init
  ;; Idea is taken from:
  ;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*compilation*"
  ;;                (display-buffer-in-side-window)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 15)))

  ;; (add-to-list 'display-buffer-alist
  ;;              '((lambda (buffer-nm actions)
  ;;                  (when (string-match-p "*compilation*" buffer-nm)
  ;;                    (let ((current-window (get-buffer-window
  ;;                                           (current-buffer)
  ;;                                           (selected-frame))))
  ;;                      (with-current-buffer buffer-nm
  ;;                        (set (make-local-variable 'g2w-destination-window)
  ;;                             current-window)))
  ;;                    t))
  ;;                (display-buffer-in-side-window)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 15)))

  (add-to-list 'display-buffer-alist
               `(,(g2w-condition "*compilation*")
                 ,(g2w-display #'display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (add-to-list 'g2w-display-buffer-commands 'compile-goto-error)

  :config
  (define-key compilation-mode-map (kbd "q") #'g2w-quit-window))

;; /b/} == compile ==

;; /b/{ == eshell mode ==

(use-package eshell
  :config
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook
   'eshell-mode-hook
   (lambda ()
     (define-key eshell-mode-map (kbd "<up>") #'previous-line)
     (define-key eshell-mode-map (kbd "<down>") #'next-line)

     (define-key eshell-mode-map (kbd "C-<up>")
       #'eshell-previous-matching-input-from-input)
     (define-key eshell-mode-map (kbd "C-<kp-up>")
       #'eshell-previous-matching-input-from-input)
     (define-key eshell-mode-map (kbd "C-<down>")
       #'eshell-next-matching-input-from-input)
     (define-key eshell-mode-map (kbd "C-<kp-down>")
       #'eshell-next-matching-input-from-input)))

  :ensure t)

;; /b/} == eshell mode ==

;; /b/{ == Line numbering ==

(use-package linum
  :init
  (setq rh-linum-right-space " ")
  (setq rh-linum-min-digits 4)

  (setq linum-format (concat "%" (number-to-string 4) "d"
                             rh-linum-right-space))
  :demand t
  :ensure t)

(use-package hlinum
  :init
  ;; (hlinum-activate)
  :ensure t)

(use-package nlinum
  :init
  (setq nlinum-format (concat "%" (number-to-string 4) "d"
                              rh-linum-right-space))
  ;; (setq nlinum-highlight-current-line t)

  :config
  (require 'nlinum-hl)

  (global-set-key (kbd "C-<f12>")
                  (lambda ()
                    (interactive)
                    (nlinum--flush)))

  :after linum
  :demand t
  :ensure t)

(use-package nlinum-hl
  :after nlinum
  :demand t
  :ensure t)

;; /b/} == Line numbering ==

;; == Exporting buffers to other formats (html, pdf etc.)

(use-package htmlize
  :ensure t)

;; == Indentation highlighting ==

(use-package highlight-indent-guides
  :ensure t)

;; == Code folding ==

(require 'hideshow)
(setq hs-allow-nesting t)
(setq hs-isearch-open t)

(global-set-key (kbd "C-S-j") 'hs-toggle-hiding)
(global-set-key (kbd "C-S-e") 'hs-show-all)
;; (define-key hs-minor-mode-map (kbd "C-S-e") 'hs-show-all)
;; (define-key hs-minor-mode-map (kbd "C-S-j") 'hs-toggle-hiding)

;; Activate the needed timer.
(show-paren-mode)

;; The timer will do nothing if this is nil.
(setq show-paren-mode nil)

(cl-defun show-paren-local-mode (&optional (value nil value-supplied-p))
  (interactive)
  ;; The value of show-paren-mode will be local to this buffer.
  (if (not (local-variable-p 'show-paren-mode))
      (make-local-variable 'show-paren-mode))
  (let ((show))
    (if (null value-supplied-p)
        (setq show (if (null show-paren-mode) t nil))
      (setq show value))
    (setq show-paren-mode show)))

;; (cl-defun vr-programming-minor-modes (&optional (value nil value-supplied-p))
;;   "Enables some minor modes, useful for programming."
;;   (interactive)
;;   (if (null value-supplied-p)
;;       (progn
;;         ;; (message "*** in vr-programming-minor-modes")
;;         (if (local-variable-p 'vr-prog-mode)
;;             (progn
;;               ;; (message "*** killing vr-prog-mode")
;;               (kill-local-variable 'vr-prog-mode)
;;               (linum-mode -1)
;;               (show-paren-local-mode -1)
;;               (hs-minor-mode -1))
;;           (progn
;;             ;; (message "*** setting vr-prog-mode")
;;             (set (make-local-variable 'vr-prog-mode) t)
;;             (linum-mode 1)
;;             (show-paren-local-mode 1)
;;             (hs-minor-mode 1)
;;             ;; ;; Use case-sensitive search (buffer-local)
;;             ;; (setq case-fold-search nil)
;;             )))
;;     (progn
;;       (if value
;;           (progn
;;             (set (make-local-variable 'vr-prog-mode) t)
;;             (linum-mode 1)
;;             (show-paren-local-mode 1)
;;             (hs-minor-mode 1)
;;             ;; ;; Use case-sensitive search (buffer-local)
;;             ;; (setq case-fold-search nil)
;;             )
;;         (progn
;;           (if (local-variable-p 'vr-prog-mode)
;;               (kill-local-variable 'vr-prog-mode))
;;           (linum-mode -1)
;;           (show-paren-local-mode nil)
;;           (hs-minor-mode -1))))))

(cl-defun vr-programming-minor-modes (&optional (enable nil enable-supplied-p))
  "Enables some minor modes, useful for programming."
  (interactive)
  (let* ((toggle (not enable-supplied-p))
         (enabled (local-variable-p 'vr-prog-modes))
         (enabling (if toggle (if enabled nil t) enable))
         (disabling (not enabling)))
    (if (and (not enabled) enabling)
        (progn
          (set (make-local-variable 'vr-prog-modes) t)
          ;; (linum-mode 1)
          (nlinum-mode 1)
          (show-paren-local-mode 1)
          (hs-minor-mode 1)
          (undo-tree-mode 1)
          (code-groups-minor-mode 1)
          ;; (fci-mode 1)
          (set (make-local-variable 'show-trailing-whitespace) t)
          ;; (highlight-indent-guides-mode 1)
          ;; ;; Use case-sensitive search (buffer-local)
          ;; (setq case-fold-search nil)
          ;; (message "Enablibling programming modes")
          )
      (when (and enabled disabling)
        (kill-local-variable 'vr-prog-modes)
        ;; (linum-mode -1)
        (nlinum-mode -1)
        (show-paren-local-mode -1)
        (hs-minor-mode -1)
        (undo-tree-mode -1)
        (code-groups-minor-mode -1)
        ;; (fci-mode -1)
        (kill-local-variable 'show-trailing-whitespace)
        ;; (highlight-indent-guides-mode -1)
        ;; (message "Disabling programming modes")
        ))))

;; == Magit Mode ==

(use-package magit
  :config
  ;; (magit-auto-revert-mode -1)
  :ensure t)

;; == GUD Mode ==

;; TODO: move the following function into a separate section for gud-mode
;; see http://stackoverflow.com/questions/3473134/emacs-23-1-1-with-gdb-forcing-source-windows
;; see http://stackoverflow.com/questions/24386672/use-gdb-within-emacs-always-show-the-source-code
;; see http://nurpax.github.io/posts/2014-10-12-fixing-gdb-many-windows-source-buffer.html

;; (defadvice gud-display-line (before one-source-window activate)
;;   "Always use the same window to show source code."
;;   (let ((buf (get-file-buffer true-file)))
;;     (when (and buf gdb-source-window)
;;       (set-window-buffer gdb-source-window buf))))

;; (defadvice gud-display-line (around vr-gud-display-line activate)
;;   (let* ((last-nonmenu-event t)         ; Prevent use of dialog box for questions.
;;          (buffer
;;           (with-current-buffer gud-comint-buffer
;;             (gud-find-file true-file)))
;;          (window (and buffer
;;                       (or (get-buffer-window buffer)
;;                           (display-buffer buffer '(nil (inhibit-same-window . t))))))
;;          ;; (window (and buffer
;;          ;;              (or (if (eq gud-minor-mode 'gdbmi)
;;          ;;                      (unless (gdb-display-source-buffer buffer)
;;          ;;                        (gdb-display-buffer buffer)))
;;          ;;                  (get-buffer-window buffer)
;;          ;;                  (display-buffer buffer))))
;;          (pos))
;;     (when buffer
;;       (with-current-buffer buffer
;;         (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
;;           (if (yes-or-no-p
;;                (format "File %s changed on disk.  Reread from disk? "
;;                        (buffer-name)))
;;               (revert-buffer t t)
;;             (setq gud-keep-buffer t)))
;;         (save-restriction
;;           (widen)
;;           (goto-char (point-min))
;;           (forward-line (1- line))
;;           (setq pos (point))
;;           (or gud-overlay-arrow-position
;;               (setq gud-overlay-arrow-position (make-marker)))
;;           (set-marker gud-overlay-arrow-position (point) (current-buffer))
;;           ;; If they turned on hl-line, move the hl-line highlight to
;;           ;; the arrow's line.
;;           (when (featurep 'hl-line)
;;             (cond
;;              (global-hl-line-mode
;;               (global-hl-line-highlight))
;;              ((and hl-line-mode hl-line-sticky-flag)
;;               (hl-line-highlight)))))
;;         (cond ((or (< pos (point-min)) (> pos (point-max)))
;;                (widen)
;;                (goto-char pos))))
;;       (when window
;;         (set-window-point window gud-overlay-arrow-position)
;;         (if (eq gud-minor-mode 'gdbmi)
;;             (setq gdb-source-window window))))))

(use-package gud
  :config
  (defun vr-gud-call-func (begin end func)
    (let ((pos (point)))
      (funcall func '(begin end))
      ;; Allow debugger to run and return to the source buffer.
      ;; TODO: find how to wait on debugger instead of guessing the time.
      (sleep-for 0.1)
      (goto-char pos)))

  (defun vr-gud-print (begin end)
    (interactive (vr-point-or-region))
    (vr-gud-call-func begin end 'gud-print))

  (defun vr-gud-break (begin end)
    (interactive (vr-point-or-region))
    (vr-gud-call-func begin end 'gud-break))

  (defun vr-gud-tbreak (begin end)
    (interactive (vr-point-or-region))
    (vr-gud-call-func begin end 'gud-tbreak))

  (defun vr-gud-remove (begin end)
    (interactive (vr-point-or-region))
    (vr-gud-call-func begin end 'gud-remove))

  (define-key gud-minor-mode-map (kbd "<f5>") 'vr-gud-print)
  (define-key gud-minor-mode-map (kbd "S-<f5>") 'gud-watch)
  (define-key gud-minor-mode-map (kbd "<f9>") 'vr-gud-break)
  (define-key gud-minor-mode-map (kbd "S-<f9>") 'vr-gud-tbreak)
  (define-key gud-minor-mode-map (kbd "C-<f9>") 'vr-gud-remove)
  (define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)
  (define-key gud-minor-mode-map (kbd "<f11>") 'gud-step))

;; (gdb-registers-buffer      gdb-registers-buffer-name   gdb-registers-mode   gdb-invalidate-registers  )
;; (gdb-locals-buffer         gdb-locals-buffer-name      gdb-locals-mode      gdb-invalidate-locals     )
;; (gdb-stack-buffer          gdb-stack-buffer-name       gdb-frames-mode      gdb-invalidate-frames     )
;; (gdb-disassembly-buffer    gdb-disassembly-buffer-name gdb-disassembly-mode gdb-invalidate-disassembly)
;; (gdb-memory-buffer         gdb-memory-buffer-name      gdb-memory-mode      gdb-invalidate-memory     )
;; (gdb-threads-buffer        gdb-threads-buffer-name     gdb-threads-mode     gdb-invalidate-threads    )
;; (gdb-breakpoints-buffer    gdb-breakpoints-buffer-name gdb-breakpoints-mode gdb-invalidate-breakpoints)
;; (gdb-inferior-io           gdb-inferior-io-name        gdb-inferior-io-mode                           )
;; (gdb-partial-output-buffer gdb-partial-output-name                                                    )

(use-package gdb-mi
  :init
  (defvar vr-gdb-original-buffer nil)

  :config
  (defadvice gdb-setup-windows (around vr-gdb-setup-windows ())
    "Layout the window pattern for option `gdb-many-windows'."
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (gdb-get-buffer-create 'gdb-stack-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) t)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let ((win0 (selected-window))
          (win1 (split-window nil ( / ( * (window-height) 4) 5)))
          (win2 (split-window nil ( / (window-height) 4)))
          ;; (win3 (split-window-right))
          )
      ;; (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
      (select-window win2)
      (set-window-buffer
       win2
       (if gud-last-last-frame
           (gud-find-file (car gud-last-last-frame))
         (if gdb-main-file
             (gud-find-file gdb-main-file)
           ;; Put buffer list in window if we
           ;; can't find a source file.
           (list-buffers-noselect))))
      ;; (set-window-dedicated-p (selected-window) nil)
      (setq gdb-source-window (selected-window))
      (select-window (split-window-below))
      (switch-to-buffer vr-gdb-original-buffer)
      ;; (let ((win4 (split-window-right)))
      ;;   (gdb-set-window-buffer
      ;;    (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
      (select-window win1)
      (gdb-set-window-buffer (gdb-stack-buffer-name))
      (let ((win5 (split-window-right)))
        (gdb-set-window-buffer (if gdb-show-threads-by-default
                                   (gdb-threads-buffer-name)
                                 (gdb-breakpoints-buffer-name))
                               nil win5))
      (select-window win0)))

  (defadvice gdb (before vr-gdb (command-line))
    (setq vr-gdb-original-buffer (window-buffer)))

  ;; use gdb-many-windows by default
  (setq gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  ;; (setq gdb-show-main t)
  (setq gdb-delete-out-of-scope nil)
  (gdb-speedbar-auto-raise))

;; == RealGUD Mode ==

(use-package realgud
  ;; :disabled
  :ensure t)

;; /b/{ == C++ ==

;; (use-package cc-mode
;;   :load-path "/home/ramblehead/cc-mode"
;;   :pin manual)

; https://github.com/ludwigpacifici/modern-cpp-font-lock

(use-package modern-cpp-font-lock
  :ensure t)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")

(setq vr-c++std "-std=c++1z")

;; TODO: Use rtags or .project to get current compiler
;; Adopted from http://www.emacswiki.org/emacs/auto-complete-clang-extension.el
(defun vr-get-g++-isystem-path ()
  (let* ((command-result (shell-command-to-string
                          "echo \"\" | g++ -v -x c++ -E -"))
         (start-string "#include <...> search starts here:\n")
         (end-string "End of search list.\n")
         (start-pos (string-match start-string command-result))
         (end-pos (string-match end-string command-result))
         (include-string (substring command-result
                                    (+ start-pos (length start-string))
                                    end-pos)))
    (split-string include-string)))

(defun vr-c++-get-project-include-path ()
  (let* ((project-path (rh-project-get-path))
         (src-tree-root (concat project-path "../"))
         (c++-include-path (concat project-path "c++-include-path")))
    (when (and project-path (file-exists-p c++-include-path))
      ;; (setq src-tree-root (file-truename src-tree-root))
      (setq src-tree-root (expand-file-name src-tree-root))
      (with-temp-buffer
        (insert-file-contents c++-include-path)
        (mapcar (lambda (item)
                  (replace-regexp-in-string
                   (regexp-quote "../") src-tree-root item nil 'literal))
                (split-string (buffer-string)))))))

(defun vr-c++-compilation-toggle-display ()
  (interactive)
  (vr-toggle-display "*compilation*"))

(defun vr-c++-compilation-setup ()
  (setq compilation-scroll-output t)
  (let ((path (rh-project-get-path)))
    (if path
        (progn
          (set (make-local-variable 'compile-command)
               (concat path "make -k"))
          (message (concat "vr-project: " path))))
    (define-key c-mode-base-map (kbd "C-c b")
      'vr-c++-compilation-toggle-display)))

(cl-defun vr-c++-header-line (&optional
                              (header-line-trim-indicator "›")
                              (header-line-beginning-indicator " "))
  (when (not (local-variable-p 'rh-window-current-container-alist))
    (set (make-local-variable 'rh-window-current-container-alist) nil))
  (cl-delete-if (lambda (win-cont)
                  (not (window-live-p (car win-cont))))
                rh-window-current-container-alist)
  (let* ((current-container (if (null rtags-cached-current-container)
                                "" rtags-cached-current-container))
         (win (selected-window))
         (win-cont (assoc win rh-window-current-container-alist)))
    (if (rh-window-selected-interactively-p)
        (if (null win-cont)
            (add-to-list 'rh-window-current-container-alist
                         `(,win . ,current-container))
          (setf (cdr win-cont) (copy-sequence current-container)))
      (when (not (null win-cont))
        (setq current-container (cdr win-cont))))
    (propertize
     (let* ((header-string (concat header-line-beginning-indicator
                                   current-container))
            (header-string-width (string-width header-string))
            (header-filler-width (- (window-total-width) header-string-width)))
       (if (< header-filler-width 0)
           (concat (substring header-string
                              0 (- (window-total-width)
                                   (string-width header-line-trim-indicator)))
                   header-line-trim-indicator)
         (concat header-string (make-string header-filler-width ?\ ))))
     'face (if (rh-window-selected-interactively-p)
               'mode-line
             'mode-line-inactive))))

(defun vr-c++-rtags-toggle-rdm-display ()
  (interactive)
  (vr-toggle-display "*rdm*" t))

(use-package rtags
  :init
  ;; Idea is taken from:
  ;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
               `(,(g2w-condition "*RTags*" nil)
                 (display-buffer-below-selected)
                 (inhibit-same-window . t)
                 (window-height . 0.3)))

  (add-to-list 'g2w-display-buffer-commands
               'rtags-select-and-remove-rtags-buffer)
  (add-to-list 'g2w-display-buffer-commands
               'rtags-select-other-window)

  (add-to-list 'g2w-display-buffer-commands
               'rtags-next-match)
  (add-to-list 'g2w-display-buffer-commands
               'rtags-previous-match)

  (add-to-list 'g2w-display-buffer-commands
               'rtags-find-symbol-at-point)
  (add-to-list 'g2w-display-buffer-commands
               'rtags-references-tree)
  (add-to-list 'g2w-display-buffer-commands
               'rtags-find-virtuals-at-point)

  (add-to-list 'display-buffer-alist
               '("*rdm*"
                 (display-buffer-in-side-window)
                 (side . top)
                 (inhibit-same-window . t)
                 (window-height . 6)))

  :config
  ;; see https://github.com/Andersbakken/rtags/issues/304
  ;; for flag '-M'
  ;; (setq rtags-process-flags "-M")
  ;; see https://stackoverflow.com/questions/41962611/how-to-select-a-particular-gcc-toolchain-in-clang
  ;; for gcc-toolchain explanations
  (setq rtags-process-flags
        (concat "--default-argument \"--gcc-toolchain="
                (expand-file-name "clang-gcc-toolchain" "~")
                "/\""))

  (setq rtags-autostart-diagnostics t)

  (custom-set-faces
   '(rtags-errline ((((class color)) (:background "#ef8990"))))
   '(rtags-fixitline ((((class color)) (:background "#ecc5a8"))))
   '(rtags-warnline ((((class color)) (:background "#efdd6f"))))
   '(rtags-skippedline ((((class color)) (:background "#c2fada")))))

  (setq rtags-other-window-function (lambda () (other-window -1)))
  (setq rtags-results-buffer-other-window t)
  (setq rtags-bury-buffer-function (lambda () (quit-window t)))

  (rtags-enable-standard-keybindings)
  ;; (define-key c-mode-base-map (kbd "<f6>") 'rtags-rename-symbol)
  (define-key c-mode-base-map (kbd "C-c r d") 'vr-c++-rtags-toggle-rdm-display)
  (define-key c-mode-base-map (kbd "M-[") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "M-]") 'rtags-location-stack-forward)

  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)

  (define-key c-mode-base-map (kbd "M->") 'rtags-next-match)
  (define-key c-mode-base-map (kbd "M-<") 'rtags-previous-match)
  ;; (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  ;; (define-key c-mode-base-map (kbd "M-,") 'vr-c++rtags-references-tree)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-references-tree)
  (define-key c-mode-base-map (kbd "C-M-,") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "M-i") 'rtags-imenu)
  (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references)

  :pin manual)

;; see https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
(defun vr-c++-rtags-setup ()
  (require 'rtags)

  (rtags-start-process-unless-running)
  ;; Does not work with my clang-auto-complete setting
  ;; (setq rtags-display-current-error-as-tooltip t)

  ;; Display current function name at the top of the window (header-line).
  ;; https://github.com/Andersbakken/rtags/issues/435
  (set (make-local-variable 'rtags-cached-current-container) "")
  ;; (setq rtags-track-container t)
  (set (make-local-variable 'rtags-track-container) t)

  (add-hook
   'find-file-hook
   (lambda ()
     (set (make-local-variable 'header-line-format)
          '(:eval (vr-c++-header-line))))
   nil t))

(defun vr-c++-auto-complete-clang ()
  (interactive)
  (message "auto-completing with clang...")
  (auto-complete (append '(ac-source-clang) ac-sources)))

;; TODO: make all auto-complete settings buffer local
(defun vr-c++-ac-setup ()
  ;; see https://github.com/mooz/auto-complete-c-headers
  (require 'auto-complete-c-headers)
  ;; #include auto-completion search paths
  (set (make-local-variable 'achead:include-directories)
       (append (vr-c++-get-project-include-path)
               (vr-get-g++-isystem-path)
               achead:include-directories))

  ;; 'rtags-ac' is not as polished as 'auto-complete-clang',
  ;; so using 'auto-complete-clang'
  ;; (require 'rtags-ac)
  ;; (setq rtags-completions-enabled t)

  ;; ;; see https://github.com/brianjcj/auto-complete-clang
  (require 'auto-complete-clang)

  ;; i.e. 'echo "" | g++ -v -x c++ -E -'
  ;; (setq clang-completion-suppress-error 't)
  ;; (setq ac-clang-executable (executable-find "clang-3.6"))
  (set (make-local-variable 'ac-clang-flags)
       (append `(,vr-c++std)
               (mapcar (lambda (item) (concat "-I" item))
                       (vr-c++-get-project-include-path))
               (mapcar (lambda (item) (concat "-isystem" item))
                       (vr-get-g++-isystem-path))))

  (set (make-local-variable 'ac-sources)
       (append '(ac-source-c-headers
                 ;; Dynamic auto-completion is slow and interferes with typing,
                 ;; whether it is 'c-source-clang' or 'ac-source-rtags',
                 ;; therefore it is only activated on 'C-x C-<tab>' (see key
                 ;; definitions below in this function) in
                 ;; 'vr-c++-auto-complete-clang' function.
                 ;; ac-source-clang
                 ;; ac-source-rtags
                 )
               ac-sources))

  (let ((local-yas-minor-mode-map (copy-keymap yas-minor-mode-map)))
    (set (make-local-variable 'yas-minor-mode-map) local-yas-minor-mode-map))

  ;; use ac for for yas-expand
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; or another shortcut:
  (define-key yas-minor-mode-map (kbd "C-`") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-~") 'yas-prev-field)

  (let ((local-ac-completing-map (copy-keymap ac-completing-map)))
    (set (make-local-variable 'ac-completing-map) local-ac-completing-map))

  ;; <C-iso-lefttab> is C-S-<tab>
  (define-key ac-completing-map (kbd "C-x C-<tab>") 'vr-c++-auto-complete-clang)
  (local-set-key (kbd "C-x C-<tab>") 'vr-c++-auto-complete-clang))

(defun vr-c++-forward-list ()
  (interactive)
  (if (cg-looking-at-any-group-head)
      ;; (unwind-protect
      ;;     (let (result)
      ;;       (condition-case ex
      ;;           (setq result (cg-search-forward-group-balanced-tail))
      ;;         ('error (message (format "Caught exception: [%s]" ex))))
      ;;       result)
      ;;   (message "Cleaning up..."))
      (cg-search-forward-group-balanced-tail)
    (forward-list)))

(defun vr-c++-backward-list ()
  (interactive)
  (if (cg-looking-at-any-group-tail)
      (cg-search-backward-group-balanced-head)
    (backward-list)))

(defun vr-c++-code-folding-setup ()
  (hs-minor-mode 1)
  (local-set-key (kbd "C-S-j") 'cg-hs-toggle-hiding)
  (local-set-key (kbd "C-M-n") 'vr-c++-forward-list)
  (local-set-key (kbd "C-M-p") 'vr-c++-backward-list))

(defun vr-c++-yas-setup ()
  ;; Use yast instead of abbrev-mode
  (abbrev-mode -1)
  (let* ((project-path (rh-project-get-path))
         (snippets-path (concat project-path "snippets")))
    (if (and project-path (file-exists-p snippets-path))
        (if (not (equal yas-snippet-dirs
                        (add-to-list 'yas-snippet-dirs snippets-path)))
            (yas-reload-all))))
  (yas-minor-mode 1))

(defun vr-c++-looking-at-lambda_as_param (langelem)
  "Return t if text after point matches '[...](' or '[...]{'"
  (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-looking-at-lambda_in_uniform_init (langelem)
  "Return t if text after point matches '{[...](' or '{[...]{'"
  (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-looking-at-guess-macro-definition (langelem)
  "Return t if the guess is that we are looking at macro definition"
  (let ((case-fold-search nil))
    (or (looking-at "\\b[A-Z0-9_]+(.*)[ \t]*$")
        (looking-at "\\b[A-Z0-9_]+[ \t]*$"))))

(defun vr-c++-looking-at-return (langelem)
  "Return t if looking at return statement"
  (looking-at "return"))

(defun vr-c++-get-offset-return (langelem)
  "Returns offset for the next line after sptit return"
  (ignore-errors
    (save-excursion
      (goto-char (c-langelem-pos langelem))
      (save-match-data
        (let ((line (thing-at-point 'line t)))
          (if (string-match "\\(return[[:space:]]+\\)[^[:space:]]+.*\n" line)
              ;; `(add ,(length (match-string 1 line)) +)

              ;; e.g.
              ;; return glowplugMeasurement > glowplugMin &&
              ;;        glowplugMeasurement < glowplugMax;
              (length (match-string 1 line))

            '+))))))

(defun vr-c++-looking-at-uniform_init_block_closing_brace_line (langelem)
  "Return t if cursor if looking at C++11 uniform init block T v {xxx}
closing brace"
  (back-to-indentation)
  (looking-at "}"))

(defun vr-c++-looking-at-uniform_init_block_cont_item (langelem)
  "Return t if cursor if looking at C++11 uniform init block
continuing (not first) item"
  (back-to-indentation)
  (c-backward-syntactic-ws)
  (looking-back ","))

(defun vr-c++-looking-at-class_in_namespace (langelem)
  "Return t if looking at topmost-intro class in namespace"
  (back-to-indentation)
  (let* ((c-parsing-error nil)
         (syntax (c-save-buffer-state nil
                   (c-guess-basic-syntax))))
    (and (equal (car (nth 0 syntax)) 'innamespace)
         (equal (car (nth 1 syntax)) 'topmost-intro)
         (looking-at "class"))))

(defun vr-c++-looking-at-template (langelem)
  "Return t if looking at class template"
  (if (looking-at "template") t nil))

(defun vr-c++-indentation-examine (langelem looking-at-p)
  (and (equal major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (when langelem
             (goto-char (c-langelem-pos langelem)))
           (funcall looking-at-p langelem)))))

;; Adapted from google-c-lineup-expression-plus-4
(defun vr-c++-lineup-expression-plus-tab-width (langelem)
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (cond
     ;; We are making a reasonable assumption that if there is a control
     ;; structure to indent past, it has to be at the beginning of the line.
     ((looking-at "\\(\\(if\\|for\\|while\\)\\s *(\\)")
      (goto-char (match-end 1)))
     ;; For constructor initializer lists, the reference point for line-up is
     ;; the token after the initial colon.
     ((looking-at ":\\s *")
      (goto-char (match-end 0))))
    (vector (+ tab-width (current-column)))))

(use-package clang-format
  :load-path "/usr/share/emacs/site-lisp/clang-format-5.0"
  :pin manual)

(use-package google-c-style
  :ensure)

(defun vr-c++-indentation-setup ()
  (setq tab-width 2)
  (require 'google-c-style)
  (google-set-c-style)

  ;; (c-set-offset 'statement-cont '(nil c-lineup-assignments +))
  (c-set-offset 'arglist-intro 'vr-c++-lineup-expression-plus-tab-width)
  ;; (c-set-offset 'inher-intro '+)
  (c-set-offset 'func-decl-cont '+)
  (c-set-offset 'inher-intro '++)
  (c-set-offset 'member-init-intro '++)

  (c-set-offset
   'topmost-intro-cont
   (lambda (langelem)
     (cond
      ((vr-c++-indentation-examine
        langelem
        #'vr-c++-looking-at-guess-macro-definition)
       nil)
      ((vr-c++-indentation-examine
        langelem
        #'vr-c++-looking-at-template)
       0)
      (t '+))))

  ;; (c-set-offset 'topmost-intro-cont '+)

  ;; (c-set-offset
  ;;  'topmost-intro-cont
  ;;  (lambda (langelem)
  ;;    (if (vr-c++-indentation-examine
  ;;         langelem
  ;;         #'vr-c++-looking-at-guess-macro-definition)
  ;;        nil
  ;;      0)))

  ;; (c-set-offset
  ;;  'topmost-intro-cont
  ;;  (lambda (langelem)
  ;;    (message "%s" langelem)
  ;;    (if (vr-c++-indentation-examine
  ;;         langelem
  ;;         #'vr-c++-looking-at-template)
  ;;        nil
  ;;      '+)))

  ;; (c-set-offset
  ;;  'inher-intro
  ;;  (lambda (langelem)
  ;;    (if (vr-c++-indentation-examine
  ;;         langelem
  ;;         #'vr-c++-looking-at-class_in_namespace)
  ;;        '++
  ;;      '++)))

  (c-set-offset
   'statement-cont
   (lambda (langelem)
     (cond
      ((vr-c++-indentation-examine
        langelem
        #'vr-c++-looking-at-return)
       (vr-c++-get-offset-return langelem))
      ((vr-c++-indentation-examine
        nil
        ;; see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_170.html for '#''
        #'vr-c++-looking-at-uniform_init_block_closing_brace_line)
       '-)
      ((vr-c++-indentation-examine
        nil
        #'vr-c++-looking-at-uniform_init_block_cont_item)
       0)
      (t '(nil c-lineup-assignments +)))))

  (c-set-offset
   'defun-block-intro
   (lambda (langelem)
     (let ((syntax (if (boundp 'c-syntactic-context)
		       c-syntactic-context
		     (c-save-buffer-state nil
		       (c-guess-basic-syntax)))))
       (if (member '(inlambda) syntax)
           (lambda (langelem)
             (save-excursion
               (goto-char (c-langelem-pos langelem))
               `(add [,(current-column)] +)))
         '+))))

  (c-set-offset
   'inline-close
   (lambda (langelem)
     (let ((syntax (if (boundp 'c-syntactic-context)
		       c-syntactic-context
		     (c-save-buffer-state nil
		       (c-guess-basic-syntax)))))
       (if (member '(inlambda) syntax)
           (lambda (langelem)
             (save-excursion
               (goto-char (c-langelem-pos langelem))
               `[,(current-column)]))
         nil))))

  ;; (c-set-offset
  ;;  'statement-block-intro
  ;;  (lambda (langelem)
  ;;    (if (vr-c++-indentation-examine
  ;;         langelem
  ;;         #'vr-c++-looking-at-lambda_in_uniform_init)
  ;;        0
  ;;      '+)))

  ;; see http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  ;; (defadvice c-lineup-arglist (around
  ;;                              vr-c++-c-lineup-arglist (_langelem)
  ;;                              activate)
  ;;   "Improve indentation of continued C++11 lambda function opened as argument."
  ;;   (setq ad-return-value
  ;;         (if (vr-c++-indentation-examine
  ;;              _langelem
  ;;              #'vr-c++-looking-at-lambda_as_param)
  ;;             0
  ;;           ad-do-it)))
  )

(defun vr-c++-font-lock-setup ()
  (modern-c++-font-lock-mode 1))

(add-to-list 'auto-mode-alist '("/hpp\\'\\|\\.ipp\\'\\|\\.h\\'" . c++-mode))

(add-hook
 'c++-mode-hook
 (lambda ()
   ;; For some reason c++-mode-hook is getting executed twice.
   ;; The following if-condition is preventing the
   ;; second execution.
   ;; (message "c++-mode-hook call")

   (if (not (local-variable-p 'vr-c++-mode-hook-called-before))
       (progn
         (set (make-local-variable 'vr-c++-mode-hook-called-before) t)
         (vr-programming-minor-modes t)
         (vr-c++-font-lock-setup)
         (vr-c++-indentation-setup)
         (vr-c++-yas-setup)
         (vr-c++-compilation-setup)
         (vr-c++-rtags-setup)
         (vr-c++-ac-setup)
         (vr-c++-code-folding-setup)
         ;; (vr-c++-debug-setup)

         ;; (setq hs-special-modes-alist
         ;;       (delete '(c-mode "{" "}" "/[*/]" nil nil)
         ;;               hs-special-modes-alist))
         ;; (setq hs-special-modes-alist
         ;;       (delete '(c++-mode "{" "}" "/[*/]" nil nil)
         ;;               hs-special-modes-alist))
         ;; (add-to-list 'hs-special-modes-alist
         ;;              '(c++-mode
         ;;                "///@{\\|{" "///@}\\|}" "##" nil nil))

         ;; Build Solution - just like MSVS ;)
         (local-set-key (kbd "C-S-b") 'recompile)))))

;; /b/} == C++ ==

;; /b/{ == js-mode ==

(use-package js-mode
  :mode "\\.js\\'"
  :config
  ;; Indentation style ajustments
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)

  (add-hook
   'js-mode-hook
   (lambda ()
     (vr-programming-minor-modes)
     (rh-project-setup))))

;; /b/} == js-mode ==

;; /b/{ == js2-mode ==

;; (defun vr-js2-scratch-config ()
;;   (interactive)
;;   (set (make-local-variable
;;         'js2-highlight-external-variables)
;;        nil)
;;   (ac-js2-mode 1))

;; (defadvice js2-enter-key (around vr-js2-enter-key ())
;;   (progn
;;     (if (use-region-p)
;;         (delete-region (region-beginning) (region-end)))
;;     ad-do-it))
;; (ad-activate 'js2-enter-key)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  ;; Indentation style ajustments
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)

  ;; see http://emacswiki.org/emacs/Js2Mode After js2 has parsed a js file, we
  ;; look for jslint globals decl comment ("/* global Fred, _, Harry */") and
  ;; add any symbols to a buffer-local var of acceptable global vars Note that
  ;; we also support the "symbol: true" way of specifying names via a hack
  ;; (remove any ":true" to make it look like a plain decl, and any ':false' are
  ;; left behind so they'll effectively be ignored as you can;t have a symbol
  ;; called "someName:false"

  (add-hook
   'js2-post-parse-callbacks
   (lambda ()
     (when (> (buffer-size) 0)
       (let ((btext (replace-regexp-in-string
                     ": *true" " "
                     (replace-regexp-in-string
                      "[\n\t ]+"
                      " "
                      (buffer-substring-no-properties 1 (buffer-size))
                      t t))))
         (mapc (apply-partially 'add-to-list 'js2-additional-externs)
               (split-string
                (if (string-match
                     "/\\* *global *\\(.*?\\) *\\*/" btext)
                    (match-string-no-properties 1 btext) "")
                " *, *" t))))))

  ;; (defun js2-moz-send-region-or-defun ()
  ;;   (interactive)
  ;;   (if (use-region-p)
  ;;       (progn
  ;;         (message "moz-send-region")
  ;;         (moz-send-region (region-beginning) (region-end)))
  ;;     (moz-send-defun)))

  (add-hook
   'js2-mode-hook
   (lambda ()
     (vr-programming-minor-modes)
     (rh-project-setup)
     ;; (skewer-mode 1)
     ;; (moz-minor-mode -1)
     ;; (abbrev-mode -1)
     ;; (yas-minor-mode 1)
     ;; (ac-js2-mode 1)
     ;; (js2-refactor-mode 1)

     ;; (setq ac-sources
     ;;       (remove 'ac-source-abbrev ac-sources))
     ;; (add-to-list 'ac-sources 'ac-source-yasnippet)
     ;; (vr-ac-add-buffer-dict "js-mode")

     ;; (when (or (string-suffix-p ".scratch.js" (buffer-name))
     ;;           (string-equal "scratch.js" (buffer-name)))
     ;;   (vr-js2-scratch-config))

     ;; (vr-ac-add-buffer-dict "js-mode")

     ;; (local-set-key (kbd "<f6>") 'vr-js2r-rename-var-start/stop)
     ;; (local-set-key (kbd "M-[") 'pop-tag-mark)
     ;; (local-set-key (kbd "<f5>") #'vr-skewer-eval-last-expression-or-region)
     ;; (local-set-key (kbd "M-<f5>")
     ;;                #'vr-skewer-eval-print-last-expression-or-region)
     ;; (local-set-key (kbd "<f5>") 'moz-send-region)
     ;; (local-set-key (kbd "S-<f5>") 'skewer-repl)
     ;; (local-set-key (kbd "S-<f5>") 'inferior-moz-switch-to-mozilla)
     ))

  :disabled
  :ensure t)

;; /b/} == js2-mode ==

;; /b/{ == typescript-mode ==

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)

  (put 'major-mode 'display "xxx")

  (add-hook
   'typescript-mode-hook
   (lambda ()
     (set (make-local-variable 'mode-name) "ts")
     (vr-programming-minor-modes t)
     (rh-project-setup)

     (local-set-key (kbd "<S-f5>") 'rh-indium-interaction-and-run)))

  :after indium
  :ensure t)

;; /b/} == typescript-mode ==

;; /b/{ == ac-js2 ==

(use-package ac-js2
  :commands (ac-js2-mode)
  :init
  ;; (setq ac-js2-evaluate-calls t)

  :disabled
  :ensure t)

;; /b/} == ac-js2 ==

;; /b/{ == moz-minor-mode ==

;; (use-package moz
;;   :commands moz-minor-mode
;;   :interpreter ("moz" . moz-minor-mode)
;;   :ensure t)

;; /b/} == moz-minor-mode ==

;; /b/{ == skewer-mode ==

(defun vr-skewer-eval-last-expression-or-region (start end)
  (interactive (vr-point-or-region))
  (if (/= start end)
      (progn
        (deactivate-mark)
        (skewer-flash-region start end)
        (skewer-eval (buffer-substring-no-properties start end)
                     #'skewer-post-minibuffer))
    (if js2-mode-buffer-dirty-p
        (js2-mode-wait-for-parse
         ;; (skewer--save-point #'skewer-eval-last-expression))
         (skewer--save-point #'vr-skewer-eval-last-expression-or-region))
      (cl-destructuring-bind (string start end) (skewer-get-last-expression)
        (skewer-flash-region start end)
        (skewer-eval string #'skewer-post-minibuffer)))))

;; The following (vr-skewer-eval-print-last-expression-or-region) requires
;; cache-table from skewer package before evaluatiot
;; (require 'cache-table)

(defun vr-skewer-eval-print-last-expression-or-region (start end)
  (interactive (vr-point-or-region))
  (if (/= start end)
      (progn
        (deactivate-mark)
        (skewer-flash-region start end)
        (goto-char end)
        (move-end-of-line nil)
        (insert "\n")
        (let* ((request (skewer-eval
                         (buffer-substring-no-properties start end)
                         #'skewer-post-print :verbose t))
               (id (cdr (assoc 'id request)))
               (pos (cons (current-buffer) (point))))
          (setf (cache-table-get id skewer-eval-print-map) pos)))
    (if js2-mode-buffer-dirty-p
        (js2-mode-wait-for-parse
         ;; (skewer--save-point #'skewer-eval-print-last-expression))
         (skewer--save-point #'vr-skewer-eval-print-last-expression-or-region))
      (cl-destructuring-bind (string start end) (skewer-get-defun)
        (skewer-flash-region start end)
        (move-end-of-line nil)
        (insert "\n")
        (let* ((request (skewer-eval string #'skewer-post-print :verbose t))
               (id (cdr (assoc 'id request)))
               (pos (cons (current-buffer) (point))))
          (setf (cache-table-get id skewer-eval-print-map) pos))))))

(use-package skewer-mode
  :commands (skewer-mode skewer-css-mode skewer-html-mode)
  :config (httpd-start)
  :ensure t)

;; /b/} == skewer-mode ==

;; /b/{ == indium ==

(use-package indium
  :init
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*JS REPL*"
  ;;                (display-buffer-below-selected)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 0.3)))

  :config

  (defun rh-indium-eval-print-region (start end)
    "Evaluate the region between START and END; and print result below region."
    (interactive "r")
    (if (use-region-p)
        (indium-eval
         (buffer-substring-no-properties start end)
         `(lambda (value _error)
            (let ((result (indium-render-value-to-string value)))
              (save-excursion
                (goto-char ,(if (< start end) end start))
                (when (> (current-column) 0) (next-line))
                (move-beginning-of-line 1)
                (newline)
                (previous-line)
                (insert (substring-no-properties result))))))
      (message "No active region to evaluate")))

  (defun rh-indium-eval-region (start end)
    "Evaluate the region between START and END; and print result in the echo
area."
    (interactive "r")
    (if (use-region-p)
        (indium-eval
         (buffer-substring-no-properties start end)
         (lambda (value _error)
           (let ((result (indium-render-value-to-string value)))
             (message result))))
      (message "No active region to evaluate")))

  (defun rh-indium-interaction-and-run ()
    (interactive)
    (indium-interaction-mode 1)
    (split-window)
    (other-window 1)
    (indium-run-node "node"))
  ;; (defun rh-indium-interaction-and-run ()
  ;;   (interactive)
  ;;   (let ((buf (current-buffer)))
  ;;     (indium-interaction-mode 1)
  ;;     (indium-run-node "node ")
  ;;     (split-window)
  ;;     (set-window-buffer (current-window) buf)))

  (add-hook
   'indium-interaction-mode-hook
   (lambda ()
     (local-set-key (kbd "<f5>") #'rh-indium-eval-region)
     (local-set-key (kbd "M-<f5>") #'rh-indium-eval-print-region)))

  (add-hook
   'indium-repl-mode-hook
   (lambda ()
     (define-key indium-repl-mode-map (kbd "C-<kp-up>")
       #'indium-repl-previous-input)
     (define-key indium-repl-mode-map (kbd "C-<kp-down>")
       #'indium-repl-next-input)))

  :ensure t)

;; /b/} == indium ==

;; /b/{ == css-mode ==

(defun vr-skewer-css-clear-all ()
  (interactive)
  (skewer-css-clear-all)
  (message "All skewer CSS modifications are cleared"))

(defun vr-skewer-css-eval-current-declaration ()
  (interactive)
  (if (not (looking-back "}[\n\r\t\s]*"))
      (skewer-css-eval-current-declaration)
    (skewer-css-eval-current-rule)))

;; (defun vr-skewer-css-eval-current-declaration ()
;;   (interactive)
;;   (if (not (looking-back "}[\n\r\t\s]*"))
;;       (message "looking at declaration")
;;     (message "looking at rule")))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2)
  (add-hook 'css-mode-hook
            (lambda ()
              (vr-programming-minor-modes)
              (skewer-css-mode 1)
              (abbrev-mode -1)
              (yas-minor-mode 1)
              (local-set-key (kbd "<f6>") 'vr-skewer-css-clear-all)
              (local-set-key (kbd "<f5>")
                             'vr-skewer-css-eval-current-declaration)))
  :ensure t)

;; /b/} == css-mode ==

;; /b/{ == web-beautify ==

(use-package web-beautify
  :ensure t)

;; /b/} == web-beautify ==

;; /b/{ == Emacs Lisp ==

(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)

(defun el-eval-region-or-last-sexp ()
  (interactive)
  (if (use-region-p)
      (progn
        (message "eval-region")
        (eval-region (region-beginning) (region-end)))
    (eval-last-sexp current-prefix-arg)))

(defun ielm-split-window ()
  (interactive)
  (split-window)
  (other-window 1)
  (ielm))

(defun vr-elisp-slime-nav-setup ()
  (require 'elisp-slime-nav)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))
  (define-key elisp-slime-nav-mode-map (kbd "M-[") 'pop-tag-mark))

(add-hook
 'emacs-lisp-mode-hook
 (lambda ()
   (vr-programming-minor-modes)
   (eldoc-mode 1)
   (vr-elisp-slime-nav-setup)
   (set (make-local-variable 'vr-elisp-mode) t)
   (local-set-key (kbd "<f5>") 'el-eval-region-or-last-sexp)
   (local-set-key (kbd "M-<f5>") 'eval-print-last-sexp)
   (local-set-key (kbd "S-<f5>") 'ielm-split-window)))

;; /b/} == Emacs Lisp ==

;; /b/{ == python-mode ==

(use-package python-mode
  :mode "\\.py\\'"
  :init
  (setq python-indent-offset 2)

  ;; (defun vr-python-forward-element (&optional arg)
  ;;   (interactive "^p")
  ;;   (if (cg-looking-at-any-group-head)
  ;;       (cg-search-forward-group-balanced-tail)
  ;;     (forward-list arg)))

  ;; (defun vr-python-backward-element (&optional arg)
  ;;   (interactive "^p")
  ;;   (if (cg-looking-at-any-group-tail)
  ;;       (cg-search-backward-group-balanced-head)
  ;;     (backward-list arg)))

  ;; (defun vr-python-code-folding-setup ()
  ;;   (hs-minor-mode 1)
  ;;   (local-set-key (kbd "C-S-j") 'cg-hs-toggle-hiding)
  ;;   (local-set-key (kbd "C-M-n") 'vr-python-forward-element)
  ;;   (local-set-key (kbd "C-M-p") 'vr-python-backward-element))

  (add-hook
   'python-mode-hook
   (lambda ()
     (vr-programming-minor-modes))))

;; /b/} == python-mode ==

;; /b/{ == visual-basic-mode ==

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;; /b/} == visual-basic-mode ==

;; /b/{ == nxml-mode ==

;; (defun vr-nxml-forward-element (&optional arg)
;;   (interactive "^p")
;;   (if (cg-looking-at-any-group-head)
;;       (cg-search-forward-group-balanced-tail)
;;     (nxml-forward-element arg)))

;; (defun vr-nxml-backward-element (&optional arg)
;;   (interactive "^p")
;;   (if (cg-looking-at-any-group-tail)
;;       (cg-search-backward-group-balanced-head)
;;     (nxml-backward-element arg)))

(defun vr-nxml-code-folding-setup ()
  (require 'sgml-mode)
  ;; see http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
  ;; see http://www.emacswiki.org/emacs/HideShow
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 ;; "<!--\\|<[^/>]*[^/]>"
                 "<!--\\|<[^/][^>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil))

  (setq cg-forward-list-original #'nxml-forward-element)
  (setq cg-backward-list-original #'nxml-backward-element)

  ;; (setq vr-nxml-code-folding-initialised t)
  ;; (hs-minor-mode 1)
  ;; (local-set-key (kbd "C-S-j") 'cg-hs-toggle-hiding)
  ;; (local-set-key (kbd "C-M-n") 'vr-nxml-forward-element)
  ;; (local-set-key (kbd "C-M-p") 'vr-nxml-backward-element)
  )

(use-package nxml-mode
  ;; :mode "\\.xml\\'\\|\\.html\\'\\|\\.htm\\'"
  :mode "\\.xml\\'"
  :config
  (add-hook
   'nxml-mode-hook
   (lambda ()
     (vr-nxml-code-folding-setup)
     (vr-programming-minor-modes))))

;; /b/} == nxml-mode ==

;; /b/{ == web-mode ==

(defun vr-web-hs-html ()
  ;; hs-forward-sexp-func is equal to web-mode-forward-sexp by default
  ;; hs-adjust-block-beginning is nil by default
  (setq hs-block-start-regexp "<!--\\|<[^/][^>]*[^/]>")
  (setq hs-block-end-regexp "-->\\|</[^/>]*[^/]>")
  (setq hs-c-start-regexp "<!--")
  ;; (setq hs-forward-sexp-func 'sgml-skip-tag-forward)
  )

(defun vr-web-hs-default ()
  (setq hs-block-start-regexp "{")
  (setq hs-block-end-regexp "}")
  (setq hs-c-start-regexp "/[*/]")
  ;; (setq hs-forward-sexp-func 'web-mode-forward-sexp)
  )

(defun vr-web-hs-html-toggle-hiding ()
  (interactive)
  (vr-web-hs-html)
  (hs-toggle-hiding))

(defun vr-web-skewer-eval-region (start end)
  (interactive "r")
  (let ((web-mode-cur-language (web-mode-language-at-pos)))
    (if (use-region-p)
        (cond
         ((string-equal web-mode-cur-language "javascript")
          (vr-skewer-eval-last-expression-or-region start end))
         (t (message "Can't evaluate region in browser")))
      (progn
        (cond
         ((string-equal web-mode-cur-language "css")
          (vr-skewer-css-eval-current-declaration))
         (t (message "Can't evaluate last expression in browser")))))))

(defun vr-web-skewer-eval-print-region (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((web-mode-cur-language (web-mode-language-at-pos)))
        (cond
         ((string-equal web-mode-cur-language "javascript")
          (vr-skewer-eval-print-last-expression-or-region start end))
         (t (message "Can't evaluate region in browser"))))
    (message "Can only evaluate active regions")))

(defun vr-web-skewer-css-clear-all ()
  (interactive)
  (let ((web-mode-cur-language (web-mode-language-at-pos)))
    (when (string-equal web-mode-cur-language "css")
      (skewer-css-clear-all))))

(cl-defun vr-web-skewer-mode (&optional (value nil value-supplied-p))
  (interactive)
  (let ((enable))
    (if (null value-supplied-p)
        (setq enable (if (bound-and-true-p skewer-mode) -1 1))
      (setq enable (if (eq value 1) 1 -1)))
    (if (eq enable 1)
        (progn
          (skewer-mode 1)
          (skewer-css-mode 1)
          (local-set-key (kbd "<f5>") 'vr-web-skewer-eval-region)
          (local-set-key (kbd "M-<f5>") 'vr-web-skewer-eval-print-region)
          (local-set-key (kbd "S-<f5>") 'skewer-repl)
          (local-set-key (kbd "<f6>") 'vr-web-skewer-css-clear-all))
      (progn
        (skewer-mode -1)
        (skewer-css-mode -1)
        (local-unset-key (kbd "<f5>"))
        (local-unset-key (kbd "M-<f5>"))
        (local-unset-key (kbd "S-<f5>"))
        (local-unset-key (kbd "<f6>"))))))

;; (defun vr-web-hs-default-toggle-hiding ()
;;   (interactive)
;;   (vr-web-hs-default)
;;   (hs-toggle-hiding))

(defun vr-web-hs-toggle-hiding ()
  (interactive)
  (let ((web-mode-cur-language (web-mode-language-at-pos)))
    (if (string-equal web-mode-cur-language "html")
        (progn
          (vr-web-hs-html)
          (hs-toggle-hiding))
      (progn
        (if (string-match
             (concat "^[[:space:]]*<[^/][^>]*[^/]>[[:space:]]*$"
                     "\\|"
                     "^[[:space:]]*</[^/>]*[^/]>[[:space:]]*$")
             (thing-at-point 'line t))
            (vr-web-hs-html)
          (vr-web-hs-default))
        (hs-toggle-hiding)))))

(defun vr-web-ac-setup ()
  (require 'ac-html)
  (require 'ac-html-default-data-provider)
  (ac-html-enable-data-provider 'ac-html-default-data-provider)
  (ac-html-setup)
  (setq ac-sources
        (append '(ac-source-html-tag
                  ac-source-html-attr
                  ac-source-html-attrv)
                ac-sources)))

(use-package web-mode
  :mode "\\.html\\'\\|\\.mako\\'\\|\\.json\\'\\|\\.tsx\\'"
  :config
  (add-to-list
   'web-mode-ac-sources-alist
   '("html" . (ac-source-html-tag
               ac-source-html-attr
               ac-source-html-attrv
               ac-source-words-in-same-mode-buffers)))

  (add-to-list
   'web-mode-ac-sources-alist
   '("javascript" . (ac-source-yasnippet
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers)))

  (add-to-list
   'web-mode-ac-sources-alist
   '("css" . (ac-source-css-property
              ac-source-words-in-same-mode-buffers)))

  (add-hook
   'web-mode-before-auto-complete-hooks
   '(lambda ()
      (let ((web-mode-cur-language (web-mode-language-at-pos)))
        (if (string-equal web-mode-cur-language "javascript")
            (progn
              (vr-ac-add-buffer-dict "js-mode")
              ;; (yas-activate-extra-mode 'js2-mode)
              )
          (progn
            (vr-ac-remove-buffer-dict "js-mode")
            ;; (yas-deactivate-extra-mode 'js2-mode)
            )))))

  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-block-padding 2)

  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-auto-indentation nil)

  (setq web-mode-comment-formats
        (remove-if (lambda (comment-format)
                     (string-equal (car comment-format) "javascript"))
                   web-mode-comment-formats))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))

  (copy-face 'show-paren-match 'web-mode-current-element-highlight-face)

  (add-hook
   'web-mode-hook
   (lambda ()
     (vr-programming-minor-modes t)
     (auto-complete-mode -1)
     (rh-project-setup)
     ;; (vr-web-ac-setup)
     ;; (abbrev-mode -1)
     ;; (yas-minor-mode 1)

     (local-set-key (kbd "C-S-j") 'vr-web-hs-toggle-hiding)
     (local-set-key (kbd "C-x C-S-j") 'vr-web-hs-html-toggle-hiding)
     (local-set-key (kbd "C-M-n") 'forward-sexp)
     (local-set-key (kbd "C-M-p") 'backward-sexp)))

  :ensure t)

;; /b/} == web-mode ==

;; /b/{ == tide ==

(use-package tide
  :init
  (add-to-list 'display-buffer-alist
               '("*tide-references*"
                 (display-buffer-below-selected)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (add-to-list 'display-buffer-alist
               `("*tide-documentation*"
                 ,(g2w-display #'display-buffer-in-side-window t)
                 (inhibit-same-window . t)
                 (window-height . 15)))

  (defun rh-tide-company-display-permanent-doc-buffer ()
    (display-buffer (get-buffer-create "*tide-documentation*")))

  :config
  (defadvice tide-doc-buffer (around rh-tide-doc-buffer activate)
    (with-current-buffer ad-do-it
      (local-set-key "q" #'g2w-quit-window)))

  (setq tide-completion-ignore-case t)
  (setq tide-always-show-documentation t)

  (define-key tide-mode-map (kbd "M-.") #'tide-jump-to-definition)
  (define-key tide-mode-map (kbd "M-/") #'tide-jump-to-implementation)
  (define-key tide-mode-map (kbd "M-,") #'tide-references)
  (define-key tide-mode-map (kbd "M-[") #'tide-jump-back)
  (define-key tide-mode-map (kbd "M-h") #'tide-documentation-at-point)

  (define-key tide-references-mode-map (kbd "q") #'rh-quit-window-kill)

  (add-hook
   'tide-mode-hook
   (lambda ()
     (set (make-local-variable 'rh-company-display-permanent-doc-buffer)
          #'rh-tide-company-display-permanent-doc-buffer)))

  :after company
  :ensure t)

;; /b/} == tide ==

;; /b/{ == tern ==

(use-package tern
  :config
  ;; (setq tern-command
  ;;       '("/home/rh/artizanya/arango/arangodb-typescript-setup/node_modules/.bin/tern"))

  (defvar rh-tern-argument-hints-enabled t)

  (defun tern-argument-hint-at-point ()
    (interactive)
    (tern-update-argument-hints-async))

  (defun tern-post-command ()
    (unless (eq (point) tern-last-point-pos)
      (setf tern-last-point-pos (point))
      (setf tern-activity-since-command tern-command-generation)
      (when rh-tern-argument-hints-enabled
        (tern-update-argument-hints-async))))

  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil)
  (define-key tern-mode-keymap (kbd "C-c M-.") #'tern-find-definition)
  (define-key tern-mode-keymap (kbd "C-c M-[") #'tern-pop-find-definition)
  (define-key tern-mode-keymap (kbd "C-c M-i") #'tern-argument-hint-at-point)

  :ensure t)

;; /b/} == tern ==

;; /b/{ == company-tern ==

(use-package company-tern
  :config
  (defun company-tern-annotation (candidate)
    "Return simplified type annotation. 'f' for functions and
'p' for anything else."
    (if (company-tern-function-p candidate) "f trn" "p trn"))

  :ensure t)

;; /b/} == company-tern ==

;; /b/{ == xref-js2 ==

;; (use-package xref-js2
;;   :config
;;   (setq xref-js2-ignored-dirs '("build"))

;;   (defun xref-js2--root-dir ()
;;     "Return the root directory of the project."
;;     (or (rh-project-get-root)
;;         (ignore-errors
;;           (projectile-project-root))
;;         (ignore-errors
;;           (vc-root-dir))
;;         (user-error "You are not in a project")))

;;   :ensure t)

;; /b/} == xref-js2 ==

;; /b/{ == JavaScript Environments Setup ==

(defun rh-typescript-setup ()
  (interactive)
  (company-mode 1)
  (flycheck-mode 1)
  (abbrev-mode -1)
  (yas-minor-mode 1)
  ;; (tern-mode 1)
  (tide-setup)
  (tide-hl-identifier-mode 1)
  (eldoc-mode 1))

(defun rh-javascript-setup ()
  (interactive)
  (auto-complete-mode -1)
  (company-mode 1)
  (flycheck-mode 1)
  (abbrev-mode -1)
  (yas-minor-mode 1)
  ;; (tern-mode 1)
  (tide-setup)
  (tide-hl-identifier-mode 1)
  (eldoc-mode 1))

;; /b/} == JavaScript Environments Setup ==

;; -------------------------------------------------------------------
;;; Structured Text and Markup (Meta) Languages
;; -------------------------------------------------------------------

;; == Org mode ==

(setq org-replace-disputed-keys t)
(setq org-completion-use-ido t)
(setq org-support-shift-select t)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<kp-right>") 'org-metaright)
            (local-set-key (kbd "M-<kp-left>") 'org-metaleft)
            (local-set-key (kbd "M-<kp-up>") 'org-metaup)
            (local-set-key (kbd "M-<kp-down>") 'org-metadown)
            (local-set-key (kbd "C-<kp-enter>")
                           'org-insert-heading-respect-content)
            (visual-line-mode)
            (org-indent-mode)))

;; == LaTeX mode ==

(add-hook 'latex-mode-hook (lambda ()
                             (make-local-variable 'vr-tex-mode)
                             (visual-line-mode t)))
;; == AUCTeX mode ==

(cond
 ((and (equal system-type 'windows-nt) (boundp 'AUCTeX-version))
  (progn
    (require 'sumatra-forward)
    (require 'tex-mik)
    (setq TeX-PDF-mode t)
    (setq TeX-source-correlate-method 'synctex)
    (setq TeX-source-correlate-mode t)
    (setq TeX-source-correlate-start-server t)
    (let (CL)
      (setq CL (concat "\"" vr-sumatra-pdf-path "\""))
      (setq CL (concat CL " -bg-color #999999 -reuse-instance %o"))
      (setq TeX-view-program-list (list (list "Sumatra PDF" CL))))
    (setq TeX-view-program-selection '((output-pdf "Sumatra PDF")))
    (setq reftex-plug-into-AUCTeX t)
    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (progn
                                   (make-local-variable 'vr-tex-mode)
                                   (local-set-key(kbd "<f3>")
                                                  'sumatra-jump-to-line)
                                   (reftex-mode t)
                                   (TeX-fold-mode t)
                                   (visual-line-mode t))))))
 ((and (equal system-type 'gnu/linux) (boundp 'AUCTeX-version))
  (progn
    (setq TeX-PDF-mode t)
    (defun un-urlify (fname-or-url)
      "Transform file:///absolute/path from Gnome into /absolute/path
with very limited support for special characters."
      (if (string-equal (substring fname-or-url 0 8) "file:///")
          (url-unhex-string (substring fname-or-url 7))
        fname-or-url))

    (defun urlify-escape-only (path)
      "Handle special characters for urlify."
      (replace-regexp-in-string " " "%20" path))

    (defun urlify (absolute-path)
      "Transform /absolute/path to file:///absolute/path for Gnome
with very limited support for special characters."
      (if (string-equal (substring absolute-path 0 1) "/")
          (concat "file://" (urlify-escape-only absolute-path))
        absolute-path))

    ;; SyncTeX backward search - based on
    ;; http://emacswiki.org/emacs/AUCTeX#toc20,
    ;; reproduced on http://tex.stackexchange.com/a/49840/21017

    (defun th-evince-sync (file linecol &rest ignored)
      (let* ((fname (un-urlify file))
             (buf (find-file fname))
             (line (car linecol))
             (col (cadr linecol)))
        (if (null buf)
            (message "[Synctex]: Could not open %s" fname)
          (switch-to-buffer buf)
          (goto-line (car linecol))
          (unless (= col -1)
            (move-to-column col)))))

    (defvar *dbus-evince-signal* nil)

    (defun enable-evince-sync ()
      (require 'dbus)
      (when (and
             (eq window-system 'x)
             (fboundp 'dbus-register-signal))
        (unless *dbus-evince-signal*
          (setf *dbus-evince-signal*
                (dbus-register-signal
                 :session nil "/org/gnome/evince/Window/0"
                 "org.gnome.evince.Window" "SyncSource"
                 'th-evince-sync)))))

    ;; SyncTeX forward search - based on
    ;; http://tex.stackexchange.com/a/46157

    ;; universal time, need by evince
    (defun utime ()
      (let ((high (nth 0 (current-time)))
            (low (nth 1 (current-time))))
        (+ (* high (lsh 1 16) ) low)))

    ;; Forward search.
    ;; Adapted from http://dud.inf.tu-dresden.de/~ben/evince_synctex.tar.gz
    (defun auctex-evince-forward-sync (pdffile texfile line)
      (let ((dbus-name
             (dbus-call-method :session
                               "org.gnome.evince.Daemon"  ; service
                               "/org/gnome/evince/Daemon" ; path
                               "org.gnome.evince.Daemon"  ; interface
                               "FindDocument"
                               (urlify pdffile)
                               ;; Open a new window if the file is not opened.
                               t
                               )))
        (dbus-call-method :session
                          dbus-name
                          "/org/gnome/evince/Window/0"
                          "org.gnome.evince.Window"
                          "SyncView"
                          (urlify-escape-only texfile)
                          (list :struct :int32 line :int32 1)
                          (utime))))

    (defun auctex-evince-view ()
      (let ((pdf (file-truename
                  (concat default-directory
                          (TeX-master-file (TeX-output-extension)))))
            (tex (buffer-file-name))
            (line (line-number-at-pos)))
        (auctex-evince-forward-sync pdf tex line)))

    (setq TeX-view-program-list '(("EvinceDbus" auctex-evince-view)))
    (setq TeX-view-program-selection '((output-pdf "EvinceDbus")))
    (add-hook 'LaTeX-mode-hook (lambda ()
                                 (progn
                                   (make-local-variable 'vr-tex-mode)
                                   (enable-evince-sync)
                                   (reftex-mode t)
                                   (TeX-fold-mode t)
                                   (visual-line-mode t)))))))

;; -------------------------------------------------------------------
;;; Spell Checking and Natural Language Utilities
;; -------------------------------------------------------------------

;; == speck mode ==

(autoload 'speck-mode "speck"
  "Toggle speck-mode." t)
(autoload 'speck-activate "speck")
(autoload 'speck-deactivate "speck")

;; This seems to work better with Unicode buffers.
(setq speck-aspell-coding-system 'utf-8)

(setq speck-doublets t)
(setq speck-aspell-default-dictionary-name "en_GB")
;; (setq speck-personal-dictionary-file t)

(defun vr-conf-speck-prog ()
  (make-local-variable 'speck-syntactic)
  (setq speck-syntactic t))

(defun vr-conf-speck-prog-elisp ()
  (make-local-variable 'speck-face-inhibit-list)
  (setq speck-face-inhibit-list
        '(font-lock-string-face
          font-lock-constant-face)))

(defun vr-conf-speck-tex ()
  (set (make-local-variable 'speck-filter-mode) 'TeX))

(defun vr-smart-speck-mode (&optional value)
  (interactive)
  (if (not (local-variable-p 'vr-speck-confed))
      (progn
        (if (local-variable-p 'vr-prog-mode)
            (progn
              (vr-conf-speck-prog)
              (if (local-variable-p 'vr-elisp-mode)
                  (vr-conf-speck-prog-elisp)))
          (if (local-variable-p 'vr-tex-mode)
              (vr-conf-speck-tex)))
        (set (make-local-variable 'vr-speck-confed) t)))
  (if value
      (speck-mode value)
    (progn
      (if (not (local-variable-p 'speck-mode))
          (speck-activate)
        (if speck-mode
            (speck-deactivate)
          (speck-activate))))))

(global-set-key (kbd "<f8>") 'vr-smart-speck-mode)

;; (global-set-key (kbd "S-<f8>") 'speck-add-next)
;; (global-set-key (kbd "M-S-<f8>") 'speck-add-previous)
;; (global-set-key (kbd "C-<f8>") 'speck-replace-previous)
;; (global-set-key (kbd "M-<f8>") 'speck-replace-next)

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

;; == ispell mode ==

;; http://www.emacswiki.org/emacs/FlySpell
(setq ispell-program-name "aspell")
(setq ispell-silently-savep t)
(setq ispell-use-framepop-p t)
;; (setq ispell-personal-dictionary (expand-file-name "~/en.pws"))
;(setq ispell-personal-dictionary vr-ispell-en-dictionary)
;; (setq ispell-process-directory "~/.ispell/")

(global-set-key (kbd "C-x w") 'ispell-word)

;; Since discovering speck, I do not use flyspell any more.
;; == flyspell mode ==

;; (setq flyspell-always-use-popup t)

;; ;; "flyspell-mode nil" is recognized as no parameters and toggles the
;; ;; mode instead of deactivating it.  Thus "flyspell-mode 0" and
;; ;; "flyspell-mode 1" are used instead.
;; (cl-defun smart-flyspell-mode (&optional (value nil value-supplied-p))
;;   (interactive)
;;   (let ((prog-mode (if (local-variable-p 'vr-prog-mode) t nil))
;;         (flyspell-on (if (and (boundp 'flyspell-mode) flyspell-mode) t nil)))
;;     (if (null value-supplied-p)
;;         (if prog-mode
;;             (if flyspell-on
;;                 (flyspell-mode 0)
;;               (flyspell-prog-mode))
;;           (if flyspell-on
;;               (flyspell-mode 0)
;;             (flyspell-mode 1)))
;;       (if value
;;           (if prog-mode
;;               (flyspell-prog-mode)
;;             (flyspell-mode 1))
;;         (flyspell-mode 0)))))

;; (defun smart-flyspell-buffer ()
;;   (interactive)
;;   (smart-flyspell-mode 1)
;;   (flyspell-buffer))

;; (defun ispell-word-or-smart-flyspell-region ()
;;   (interactive)
;;   (smart-flyspell-mode 1)
;;   (if (use-region-p)
;;       (flyspell-region (point) (mark))
;;     (flyspell-auto-correct-word)))

;; (defun flyspell-goto-previous-error (arg)
;;   "Go to arg previous spelling error."
;;   (interactive "p")
;;   (while (not (= 0 arg))
;;     (let ((pos (point))
;;           (min (point-min)))
;;       (if (and (eq (current-buffer) flyspell-old-buffer-error)
;;                (eq pos flyspell-old-pos-error))
;;           (progn
;;             (if (= flyspell-old-pos-error min)
;;                 ;; goto beginning of buffer
;;                 (progn
;;                   (message "Restarting from end of buffer")
;;                   (goto-char (point-max)))
;;               (backward-word 1))
;;             (setq pos (point))))
;;       ;; seek the next error
;;       (while (and (> pos min)
;;                   (let ((ovs (overlays-at pos))
;;                         (r '()))
;;                     (while (and (not r) (consp ovs))
;;                       (if (flyspell-overlay-p (car ovs))
;;                           (setq r t)
;;                         (setq ovs (cdr ovs))))
;;                     (not r)))
;;         (backward-word 1)
;;         (setq pos (point)))
;;       ;; save the current location for next invocation
;;       (setq arg (1- arg))
;;       (setq flyspell-old-pos-error pos)
;;       (setq flyspell-old-buffer-error (current-buffer))
;;       (goto-char pos)
;;       (if (= pos min)
;;           (progn
;;             (message "No more miss-spelled word!")
;;             (setq arg 0))))))

;; (global-set-key (kbd "<f7>") 'ispell-word-or-smart-flyspell-region)
;; (global-set-key (kbd "<f6>") 'ispell-word)
;; (global-set-key (kbd "<f6>") 'flyspell-correct-word-before-point)
;; (global-set-key (kbd "<f6>") 'flyspell-auto-correct-word)
;; (global-set-key (kbd "C-S-<f7>") 'smart-flyspell-mode)
;; (global-set-key (kbd "C-M-<f7>") 'smart-flyspell-buffer)
;; (global-set-key (kbd "C-<f7>") 'flyspell-goto-previous-error)
;; (global-set-key (kbd "M-<f7>") 'flyspell-goto-next-error)
;; (global-set-key (kbd "S-<f7>") 'ispell-word)

;; -------------------------------------------------------------------
;;; Windows system interaction
;; -------------------------------------------------------------------

(cond
 ((equal system-type 'windows-nt)
  (progn
    (defun vr-maximize-frame ()
      (interactive)
      (w32-send-sys-command #xf030))

    (defun vr-restore-frame ()
      (interactive)
      (w32-send-sys-command #xF120))

    (defun vr-toggle-max-res-frame ()
      (interactive)
      (if (not (boundp 'vr-frame-maximized))
          ;; Guessing that frame is not maximized.
          (setq vr-frame-maximized nil))
      (if vr-frame-maximized
          (progn
            (vr-restore-frame)
            (setq vr-frame-maximized nil))
        (progn
          (vr-maximize-frame)
          (setq vr-frame-maximized t))))

    (defalias 'fullscreen 'vr-toggle-max-res-frame)))

 ((equal system-type 'gnu/linux)
  (progn
    (defun fullscreen ()
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

    (defun vr-toggle-max-res-frame (&optional f)
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))))))

(global-set-key (kbd "<f11>") 'fullscreen)
(global-set-key (kbd "M-<return>") 'vr-toggle-max-res-frame)
(global-set-key (kbd "M-<kp-enter>") 'vr-toggle-max-res-frame)

;; Change cursor type according to mode.
;; http://emacs-fu.blogspot.co.uk/2009/12/changing-cursor-color-and-shape.html
(setq read-only-cursor-type 'hbar)
(setq overwrite-cursor-type 'box)
(setq normal-cursor-type 'bar)

(defun set-cursor-according-to-mode ()
  "Change cursor type according to some minor modes."
  (cond
   (buffer-read-only
    (setq cursor-type read-only-cursor-type))
   (overwrite-mode
    (setq cursor-type overwrite-cursor-type))
   (t
    (setq cursor-type normal-cursor-type))))

(add-hook 'post-command-hook 'set-cursor-according-to-mode)

;; -------------------------------------------------------------------
;;; General Emacs enhancement modes
;; -------------------------------------------------------------------

(setq vr-ignore-buffers '("\\` "
                          "^\\*Completions\\*$"
                          "^\\*Quail Completions\\*$"
                          "^\\*Messages\\*$"
                          "^\\*clang-output\\*$"
                          "^\\*clang error\\*$"
                          "^\\*Semantic SymRef\\*$"
                          "^\\*Recent Files\\*$"
                          "^\\*Directory\\*$"
                          "^\\*Ido Completions\\*$"
                          "^\\*buffer-selection\\*$"
                          "^\\*httpd\\*$"
                          ;; compile/script outputs
                          "^\\*skewer-error\\*$"
                          "^\\*tide-server\\*$"
                          ;; rtags buffers
                          "^\\*rdm\\*$"
                          "^\\*RTags\\*$"
                          "^\\*RTags Diagnostics\\*$"
                          "^\\*RTags Log\\*$"
                          ;; AUCTeX output files
                          " output\\*$"))

;; Example:
;; Makefile.am, Makefile.am<3> etc.  to
;; Makefile.am|path1, Makefile.am|path2
;; http://emacs-fu.blogspot.co.uk/2009/11/making-buffer-names-unique.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Provides additional help functions such as describe-keymap bound to C-h M-k
(require 'help-fns+)

;; == ido mode ==

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-filename-at-point nil)
(setq ido-use-url-at-point nil)
(setq ido-save-directory-list-file vr-ido-last-file-path)
;; (setq ido-auto-merge-work-directories-length -1)

(setq ido-ignore-buffers vr-ignore-buffers)
(ido-everywhere 1)

;; (setq ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src"))
;; (setq ido-enable-last-directory-history t)
;; (setq ido-max-work-directory-list 30)
;; (setq ido-max-work-file-list 50)
;; (setq ido-max-prospects 8)
(setq ido-confirm-unique-completion t)
;; When using ido, the confirmation is not needed.
(setq confirm-nonexistent-file-or-buffer nil)

;; (defun vr-ido-choose-from-recentf ()
;;   "Use ido to select a recently opened file from the \"recentf-list\""
;;   (interactive)
;;   (find-file (ido-completing-read "Open file: " recentf-list nil t)))

;; (defun vr-ido-keys ()
;;   ;; (define-key ido-completion-map (kbd "C-<tab>") 'ido-next-match)
;;   ;; (define-key ido-completion-map (kbd "C-S-<tab>") 'ido-prev-match))
;;   ;; (define-key ido-completion-map (kbd "<escape>") 'minibuffer-keyboard-quit)
;;   (define-key ido-completion-map (kbd "C-<next>") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-<kp-next>") 'ido-prev-match)
;;   (define-key ido-completion-map (kbd "C-<prior>") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-<kp-prior>") 'ido-prev-match))

;; == ido-ubiquitous mode ==

;; (defvar ido-ubiquitous-debug-mode nil)
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; == smex mode ==

(setq smex-save-file vr-smex-save-file)
(require 'smex)
(smex-initialize)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Default M-x command
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; == ido-vertical mode ==

;; (use-package ido-vertical-mode
;;   :config
;;   (ido-vertical-mode 1)
;;   :ensure t)

;; == flx-ido mode ==

(use-package flx-ido
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode 1)
  :ensure t)

;; /b/{ == ivy mode ==

;; (use-package ivy
;;   :ensure t)

;; /b/} == ivy mode ==

;; /b/{ == helm mode ==

;; (defun vr-helm-toggle-header-line ()
;;   (if (= (length helm-sources) 1)
;;       (set-face-attribute 'helm-source-header nil :height 0.1)
;;     (set-face-attribute 'helm-source-header nil :height 1.0)))

;; (use-package helm
;;   :init
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx bos "*helm" (* not-newline) "*" eos)
;;                  (display-buffer-in-side-window)
;;                  (inhibit-same-window . t)
;;                  (window-height . 0.2)))

;;   (setq helm-display-header-line nil)

;;   (add-hook 'helm-before-initialize-hook 'vr-helm-toggle-header-line)
;;   :ensure t)

;; /b/} == helm mode ==

;; /b/{ == ifilipb mode ==

;; (require 'iflipb)
;; (setq iflipb-ignore-buffers vr-ignore-buffers)
;; (setq iflipb-wrap-around t)

;; (global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
;; (global-set-key (kbd "C-<kp-next>") 'iflipb-next-buffer)
;; (global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
;; (global-set-key (kbd "C-<kp-prior>") 'iflipb-previous-buffer)

(use-package iflipb
  :config
  (defadvice iflipb-next-buffer
      (around g2w-iflipb-next-buffer () activate)
    (let ((iflipb-ignore-buffers
           (append iflipb-ignore-buffers
                   (aref (g2w-same-side-and-slot-buffers
                          (current-buffer))
                         1))))
      ad-do-it))

  (defadvice iflipb-previous-buffer
      (around g2w-iflipb-previous-buffer () activate)
    (let ((iflipb-ignore-buffers
           (append iflipb-ignore-buffers
                   (aref (g2w-same-side-and-slot-buffers
                          (current-buffer))
                         1))))
      ad-do-it))

  (setq iflipb-ignore-buffers vr-ignore-buffers)
  (setq iflipb-wrap-around t)

  (global-set-key (kbd "C-<next>") #'iflipb-next-buffer)
  (global-set-key (kbd "C-<kp-next>") #'iflipb-next-buffer)
  (global-set-key (kbd "C-<prior>") #'iflipb-previous-buffer)
  (global-set-key (kbd "C-<kp-prior>") #'iflipb-previous-buffer)

  :demand
  :ensure t)

;; /b/} == ifilipb mode ==

;; /b/{ == bs mode ==

;; ;; see http://scottfrazersblog.blogspot.co.uk/2010/01/emacs-filtered-buffer-switching.html
;; (setq bs-configurations
;;       '(("all" nil nil nil nil nil)
;;         ("files" nil nil nil
;;          (lambda (buf)
;;            (rh-string-match-regexp-list
;;             vr-ignore-buffers
;;             (buffer-name buf)))
;;          nil)))

;; (setq bs-cycle-configuration-name "files")

;; (setq bs-mode-font-lock-keywords
;;       '(;; Headers
;;         ("^[ ]+\\([-M].*\\)$" 1 font-lock-keyword-face)
;;         ;; Boring buffers
;;         ("^\\(.*\\*.*\\*.*\\)$" 1 font-lock-comment-face)
;;         ;; Dired buffers
;;         ("^[ .*%]+\\(Dired.*\\)$" 1 font-lock-type-face)
;;         ;; Modified buffers
;;         ("^[ .]+\\(\\*\\)" 1 font-lock-warning-face)
;;         ;; Read-only buffers
;;         ("^[ .*]+\\(\\%\\)" 1 font-lock-variable-name-face)))

;; ;; see http://www.warmenhoven.org/src/emacs.el/ew-buffer.el.html
;; (defun vr-bs--get-size-string (&rest ignored)
;;   (let* ((size (buffer-size))
;;          (str (number-to-string size)))
;;     (when (> (length str) 3)
;;       (setq size (/ size 1024.0)
;;             str (format "%.1fk" size)))
;;     (when (> (length str) 6)
;;       (setq size (/ size 1024.0)
;;             str (format "%.1fM" size)))
;;     (when (> (length str) 6)
;;       (setq size (/ size 1024.0)
;;             str (format "%.1fG" size)))
;;     str))

;; (setq bs-attributes-list
;;       '(("" 2 2 left bs--get-marked-string)
;;         ("M" 1 1 left bs--get-modified-string)
;;         ("R" 2 2 left bs--get-readonly-string)
;;         ("Size" 6 6 right vr-bs--get-size-string)
;;         ("" 2 2 left "  ")
;;         ("Mode" 16 16 left bs--get-mode-name)
;;         ("" 2 2 left "  ")
;;         ("Buffer" bs--get-name-length 100 left bs--get-name)
;;         ("" 2 2 left "  ")
;;         ("File" 1 255 left bs--get-file-name)))

;; (add-hook 'bs-mode-hook
;;           (lambda ()
;;             (hl-line-mode 1)
;;             (define-key bs-mode-map (kbd "<escape>") 'bs-kill)))

;; (global-set-key (kbd "C-x C-b") 'bs-show)

(use-package bs
  :config
  ;; see http://scottfrazersblog.blogspot.co.uk/2010/01/emacs-filtered-buffer-switching.html
  (setq
   bs-configurations
   '(("all" nil nil nil nil nil)
     ("files" nil nil nil
      (lambda (buf)
        (rh-string-match-regexp-list
         vr-ignore-buffers
         (buffer-name buf)))
      nil)))

  (setq bs-cycle-configuration-name "files")

  (setq
   bs-mode-font-lock-keywords
   '(;; Headers
     ("^[ ]+\\([-M].*\\)$" 1 font-lock-keyword-face)
     ;; Boring buffers
     ("^\\(.*\\*.*\\*.*\\)$" 1 font-lock-comment-face)
     ;; Dired buffers
     ("^[ .*%]+\\(Dired.*\\)$" 1 font-lock-type-face)
     ;; Modified buffers
     ("^[ .]+\\(\\*\\)" 1 font-lock-warning-face)
     ;; Read-only buffers
     ("^[ .*]+\\(\\%\\)" 1 font-lock-variable-name-face)))

  ;; see http://www.warmenhoven.org/src/emacs.el/ew-buffer.el.html
  (defun vr-bs--get-size-string (&rest ignored)
    (let* ((size (buffer-size))
           (str (number-to-string size)))
      (when (> (length str) 3)
        (setq size (/ size 1024.0)
              str (format "%.1fk" size)))
      (when (> (length str) 6)
        (setq size (/ size 1024.0)
              str (format "%.1fM" size)))
      (when (> (length str) 6)
        (setq size (/ size 1024.0)
              str (format "%.1fG" size)))
      str))

  (setq
   bs-attributes-list
   '(("" 2 2 left bs--get-marked-string)
     ("M" 1 1 left bs--get-modified-string)
     ("R" 2 2 left bs--get-readonly-string)
     ("Size" 6 6 right vr-bs--get-size-string)
     ("" 2 2 left "  ")
     ("Mode" 16 16 left bs--get-mode-name)
     ("" 2 2 left "  ")
     ("Buffer" bs--get-name-length 100 left bs--get-name)
     ("" 2 2 left "  ")
     ("File" 1 255 left bs--get-file-name)))

  (defun vr-bs-show (arg)
    (interactive "P")
    (let* ((up-window (selected-window))
           (up-window-parent (window-parent up-window))
           (down-height-orig -1)
           (down-height-new -1)
           (down-window (window-in-direction 'below))
           (down-windows-preserved '())
           (bs-show-result nil)
           ;; (bs-window nil)
           )
      (when down-window
        (setq down-height-orig (window-height down-window))
        (select-window down-window)
        (while (and (window-in-direction 'below)
                    (eq up-window-parent
                        (window-parent (window-in-direction 'below))))
          (select-window (window-in-direction 'below))
          (push (cons (selected-window) (window-preserved-size nil nil))
                down-windows-preserved)
          (window-preserve-size nil nil t))
        (select-window up-window))
      (setq bs-show-result (bs-show arg))
      ;; (setq bs-window (selected-window))
      (when down-window
        (setq down-height-new (window-height down-window))
        (if (> down-height-new down-height-orig)
            (adjust-window-trailing-edge
             up-window
             (- down-height-new down-height-orig)))
        (dolist (pair down-windows-preserved)
          (window-preserve-size (car pair) nil (cdr pair))))
      bs-show-result))

  (add-hook
   'bs-mode-hook
   (lambda ()
     (hl-line-mode 1)))

  ;; (define-key bs-mode-map (kbd "<escape>") 'bs-kill)
  (global-set-key (kbd "C-x C-b") 'vr-bs-show)

  :demand t
  :ensure t)

(use-package bs-ext
  :demand t
  :ensure t)

;; /b/} == bs mode ==

;; == popwin mode ==

;; (use-package popwin
;;   :config
;;   ;; Find how to search and replace within lists in elisp and
;;   ;; generalise the following functions
;;   (delete 'help-mode popwin:special-display-config)
;;   (delete '(compilation-mode :noselect t) popwin:special-display-config)
;;   ;; (push '(help-mode :stick t) popwin:special-display-config)
;;   ;; (push '(help-mode :dedicated t :stick t) popwin:special-display-config)
;;   ;; (push '(compilation-mode :dedicated t :noselect t :stick t)
;;   ;;       popwin:special-display-config)
;;   (push '("*skewer-error*" :noselect t :stick t) popwin:special-display-config)
;;   (push '("*skewer-repl*" :stick t) popwin:special-display-config)
;;   ;; (push '("*RTags*" :noselect t :stick t) popwin:special-display-config)
;;   ;; (push '("*rdm*" :noselect t :dedicated t :stick t :height 6 :position top)
;;   ;;       popwin:special-display-config)

;;   ;; see https://www.emacswiki.org/emacs/OneWindow
;;   ;; (add-to-list 'same-window-buffer-names "*Help*")

;;   ;; (defun vr-popwin:popup-smart ()
;;   ;;   (interactive)
;;   ;;   (if popwin:popup-window
;;   ;;       (popwin:select-popup-window)
;;   ;;     (if popwin:popup-last-config
;;   ;;         (popwin:popup-last-buffer)
;;   ;;       (popwin:messages))))

;;   ;; (global-set-key (kbd "C-x SPC") 'vr-popwin:popup-smart)
;;   ;; (global-set-key (kbd "C-x C-SPC") 'popwin:close-popup-window)
;;   (global-set-key (kbd "C-x p") popwin:keymap)

;;   (popwin-mode 1)
;;   :demand t
;;   :ensure t)

;; -------------------------------------------------------------------
;;; Bookmarks
;; -------------------------------------------------------------------

(use-package bm
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  ;; where to store persistant files
  (setq bm-repository-file vr-bm-repository-file-path)

  :config
  ;; Allow cross-buffer 'next'
  ;; (setq bm-cycle-all-buffers t)

  ;; Only highlight the fringe of the line
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)

  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook
   'kill-emacs-hook
   (lambda ()
     (bm-buffer-save-all)
     (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle))
  :demand t
  :ensure t)

;; -------------------------------------------------------------------
;;; My key bindings
;; -------------------------------------------------------------------

;; Disable annoying key binding for (suspend-frame) function
(global-unset-key (kbd "C-x C-z"))
(global-set-key (kbd "C-z") #'undo)

;; Re-map obsolete emacs exit key to "Really Quit"
(global-unset-key (kbd "C-x C-c"))
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Prevent translation from <kp-bebin> to <begin>
(global-set-key (kbd "<kp-begin>") (lambda () (interactive)))

(global-set-key (kbd "C-<insert>") 'kill-ring-save)
(global-set-key (kbd "C-<kp-insert>") 'kill-ring-save)
(global-set-key (kbd "S-<insert>") 'yank)
(global-set-key (kbd "S-<kp-insert>") 'yank)
(global-set-key (kbd "M-<insert>") 'yank-pop)
(global-set-key (kbd "M-Y") 'yank-pop-forwards)
(global-set-key (kbd "M-S-<insert>") 'yank-pop-forwards)
(global-set-key (kbd "M-S-<insert>") 'yank-pop-forwards)
(global-set-key (kbd "M-S-<kp-insert>") 'yank-pop-forwards)
(global-set-key (kbd "C-<delete>") 'kill-word)
(global-set-key (kbd "C-<kp-delete>") 'kill-word)
(global-set-key (kbd "S-<delete>") 'kill-region)
(global-set-key (kbd "S-<kp-delete>") 'kill-region)
(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<kp-home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)
(global-set-key (kbd "C-<kp-end>") 'end-of-buffer)

(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)

(global-set-key (kbd "M-s-<up>") 'enlarge-window)
(global-set-key (kbd "M-s-<kp-up>") 'enlarge-window)
(global-set-key (kbd "M-s-<down>") 'shrink-window)
(global-set-key (kbd "M-s-<kp-down>") 'shrink-window)
(global-set-key (kbd "M-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<kp-left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<kp-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-s-<kp-begin>") 'vr-balance-windows-horizontally)
(global-set-key (kbd "S-M-s-<kp-begin>") 'vr-balance-windows-vertically)
(global-set-key (kbd "M-s-'") 'vr-balance-windows-horizontally)
(global-set-key (kbd "M-s-\"") 'vr-balance-windows-vertically)

;; see http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <kp-up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <kp-down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <kp-right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <kp-left>") 'windmove-left)
(global-set-key (kbd "C-x o") 'other-frame)

;; see http://superuser.com/questions/498533/how-to-alias-keybindings-in-emacs
;; for keybindings aliases. Can also be used with (current-local-map)
(define-key (current-global-map) (kbd "C-<kp-up>")
  (lookup-key (current-global-map) (kbd "C-<up>")))
(define-key (current-global-map) (kbd "C-<kp-down>")
  (lookup-key (current-global-map) (kbd "C-<down>")))
(define-key (current-global-map) (kbd "C-<kp-left>")
  (lookup-key (current-global-map) (kbd "C-<left>")))
(define-key (current-global-map) (kbd "C-<kp-right>")
  (lookup-key (current-global-map) (kbd "C-<right>")))

;; -------------------------------------------------------------------
;;; Load unscoped (e.g. without vr-) useful functions
;; -------------------------------------------------------------------

(load-file (concat user-emacs-directory "useful-functions.el"))

;; -------------------------------------------------------------------
;;; Post intit - to ensure correct settings if they were changed.
;; -------------------------------------------------------------------

;; (transient-mark-mode -1)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
