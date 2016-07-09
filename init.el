(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-indent-level 0)
 '(LaTeX-item-indent 2)
 '(font-latex-fontify-script nil)
 '(font-latex-fontify-sectioning (quote color))
 '(font-latex-math-environments
   (quote
    ("display" "displaymath" "equation" "eqnarray" "gather" "multline" "align" "alignat" "xalignat" "empheq")))
 '(indent-tabs-mode nil)
 '(longlines-show-hard-newlines t)
 '(make-backup-files nil)
 '(package-selected-packages (quote (paradox flx-ido use-package)))
 '(pop-up-windows t)
 '(preview-scale-function 1.8)
 '(tab-stop-list
   (quote
    (8 4 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow)))
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
 '(rtags-skippedline ((((class color)) (:background "#34ef85"))))
 '(rtags-warnline ((((class color)) (:background "#efdd6f"))))
 '(speck-mode-line-specked ((((class color)) (:foreground "midnight blue"))))
 '(speck-mode-line-specking ((((class color)) (:foreground "maroon"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))

;; ------------------------------------------------------------------
;;; Emacs Version Variables
;; ------------------------------------------------------------------

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

    (setq vr-recent-files-file-path
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
    (setq vr-recent-files-file-path
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
(setq vr-predictive-dict-directory-path
      (concat vr-user-lisp-directory-path "predictive-dict/"))
(setq vr-user-site-start-file-path
      (concat vr-user-lisp-directory-path "site-start.el"))

;; ------------------------------------------------------------------
;;; Helper functions and common modules
;; ------------------------------------------------------------------

;; == Package initialisation and 'use-package' bootstrap ==
;; see http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package paradox
  :init
  (setq paradox-github-token "75ba7430f095a91fddb501c146c76d2aeaee1ae6")
  :ensure t)

;; == Modules ==

(require 'cl)                           ; defines defun* etc.

;; == Auxiliary functions ==

(defun vr-string-match-regexp-list (regexp-list str)
  "Return non-nil if str matches anything in regexp-list."
  (let ((case-fold-search nil))
    (catch 'done
      (dolist (regexp regexp-list)
        (when (string-match regexp str)
          (throw 'done t))))))

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

;; -------------------------------------------------------------------
;;; Basic System Setup
;; -------------------------------------------------------------------

(add-to-list 'load-path vr-user-lisp-directory-path)
(load vr-user-site-start-file-path nil t t)
;; (load vr-usr-local-ver-site-start-file-path nil t t)

(dolist (file-path vr-site-start-file-paths)
  (load file-path nil t t))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq split-width-threshold nil)
;; (setq split-height-threshold 0)

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

;; -------------------------------------------------------------------
;;; Text Editor
;; -------------------------------------------------------------------

;; == Basic Functionality ==

(prefer-coding-system 'utf-8-unix)
;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; Disable horizontal-scrolling "snap" behaviour, which prevents scrolling
;; when cursor is within a certain distance from the left edge.
(setq hscroll-snap-threshold 0)

;; Allow cursor to be at a scrolled edge.
(setq hscroll-margin 0)

(setq default-tab-width 2)
(setq-default fill-column 80)

(setq column-number-mode t)
(setq visible-bell t)
(size-indication-mode t)
;; see http://emacs.stackexchange.com/questions/10307/how-to-center-the-current-line-vertically-during-isearch
(setq isearch-allow-scroll t)
(require 'fill-column-indicator)

;; Override text selection on typing
;; (i.e. non-persistent selection)
(delete-selection-mode t)

;; Selection behaviour as in MS Windows Notepad etc.
(defun kill-ring-save-keep-mark (BEG END)
  "For emacs-style selection (e.g. \\[set-mark-command]),
deactivate mark after `kill-ring-save' (i.e. copy). This is
standard emacs behaviour. For 'MS Windows Notepad'-style
selection (e.g. S-arrows), keep mark activated after
`kill-ring-save' (i.e. copy)."
  (interactive "r")
  (prog1 (kill-ring-save BEG END)
    (if (eq (car-safe transient-mark-mode) 'only)
        (setq deactivate-mark nil))))

(global-set-key [remap kill-ring-save] 'kill-ring-save-keep-mark)

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

(defadvice mouse-yank-at-click (around vr-mouse-yank-at-click ())
  (progn
    (if (use-region-p)
        (delete-region (region-beginning) (region-end)))
    ad-do-it))

(ad-activate 'mouse-yank-at-click)

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

;; I do not use Sunrise Commander any more - mc is better for file operarions
;; and dired is sufficient for emacs.
;; == Sunrise Commander ==

;; For sunrise use M-x customize-group [enter] sunrise [enter]
;; (require 'sunrise-commander)
;; (require 'sunrise-x-modeline)
;; (require 'sunrise-x-w32-addons)
;; ;;(require 'sunrise-x-buttons)
;; ;; does not seem to be needed
;; (add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
;; ;; defines panel view mask for sr-toggle-attributes
;; ;;(setq sr-attributes-display-mask '(nil t t t t))

;; (defun vr-sr-mode-keys ()
;;   (define-key sr-mode-map (kbd "S-<f4>") 'sr-find-file)
;;   (define-key sr-mode-map (kbd "<escape>") 'sr-quit)
;;   (define-key sr-mode-map (kbd "<return>") 'sr-advertised-find-file)
;;   (define-key sr-mode-map (kbd "<kp-enter>") 'sr-advertised-find-file)
;;   (define-key sr-mode-map (kbd "C-<return>") 'sr-advertised-execute-file)
;;   (define-key sr-mode-map (kbd "C-<kp-enter>") 'sr-advertised-execute-file)
;;   (define-key sr-mode-map (kbd "M-<f4>") 'sr-recent-files))

;; (defun vr-sr-virtual-mode-keys ()
;;   (local-set-key (kbd "M-<f4>") 'sr-virtual-dismiss))

;; (add-hook 'sr-mode-hook 'vr-sr-mode-keys)
;; (add-hook 'sr-virtual-mode-hook 'vr-sr-virtual-mode-keys)

;; (global-set-key (kbd "S-<f4>") 'sunrise)

;; == recentf mode ==

(setq recentf-save-file vr-recent-files-file-path)
(setq recentf-kill-buffer-on-open t)
(setq recentf-max-saved-items 100)
(require 'recentf)
(recentf-mode 1)

(defun vr-recentf-open-edit ()
  (interactive)
  (if (not (local-variable-p 'recentf-edit-list))
      (progn
        ;; (recentf-cancel-dialog)
        (kill-buffer)
        (recentf-edit-list))))

(defun vr-recentf-nil-if-recentf-edit ()
  (interactive)
  (if (local-variable-p 'recentf-edit-list) nil
    (vr-recentf-open-edit)))

(add-hook 'recentf-dialog-mode-hook 'set-cursor-according-to-mode)

;; (defadvice mouse-yank-at-click (around vr-mouse-yank-at-click ())
;;   (progn
;;     (if (use-region-p)
;;         (delete-region (region-beginning) (region-end)))
;;     ad-do-it))

;; (ad-activate 'mouse-yank-at-click)

(global-set-key (kbd "<f4>") 'recentf-open-files)
;; (global-set-key (kbd "<f4>") (lambda ()
;;                                (interactive)
;;                                (prog1
;;                                    (recentf-open-files)
;;                                  (menu-bar-mode -1)
;;                                  (menu-bar-mode 1))))
(define-key recentf-dialog-mode-map (kbd "<escape>") 'recentf-cancel-dialog)
(define-key recentf-dialog-mode-map (kbd "<SPC>") 'widget-button-press)
(define-key recentf-dialog-mode-map (kbd "<f4>") 'vr-recentf-nil-if-recentf-edit)

(setq vr-ignore-recentf '(;; AUCTeX output files
                          "\\.aux\\'"
                          "\\.bbl\\'"
                          "\\.blg\\'"
                          " output\\*$"))

(defsubst file-was-visible-p (file)
  "Return non-nil if FILE's buffer exists and has been displayed."
  (let ((buf (find-buffer-visiting file)))
    (if buf
      (let ((display-count (buffer-local-value 'buffer-display-count buf)))
        (if (> display-count 0) display-count nil)))))

(defsubst keep-default-and-visible-recentf-p (file)
  "Return non-nil if recentf would, by default, keep FILE, and
FILE has been displayed, and FILE does not mach vr-ignore-recentf
regexp-list."
  (if (and (recentf-keep-default-predicate file)
           (not (vr-string-match-regexp-list vr-ignore-recentf file)))
      (file-was-visible-p file)))

;; (defsubst keep-default-and-visible-recentf-p (file)
;;   "Return non-nil if recentf would, by default, keep FILE, and
;; FILE has been displayed."
;;   (if (recentf-keep-default-predicate file)
;;       (file-was-visible-p file)))

;; When a buffer is closed, remove the associated file from the recentf
;; list if (1) recentf would have, by default, removed the file, or
;; (2) the buffer was never displayed.
;; see http://www.emacswiki.org/RecentFiles#toc16
(setq recentf-keep '(keep-default-and-visible-recentf-p))

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
;;; Programming Languages
;; -------------------------------------------------------------------

;; TODO: Investigate mixed language modes, e.g. js or css in html, python macro in xml etc.
;; e.g. https://vxlabs.com/2014/04/08/syntax-highlighting-markdown-fenced-code-blocks-in-emacs/

;; ;; Better line numbering
;; (require 'nlinum)
;; (setq nlinum-format "%4d ")
(setq linum-format "%4d ")

;; Code folding
(require 'hideshow)
(setq hs-allow-nesting t)
(setq hs-isearch-open t)

(define-key hs-minor-mode-map (kbd "C-S-e") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-S-j") 'hs-toggle-hiding)

;; Activate the needed timer.
(show-paren-mode)

;; The timer will do nothing if this is nil.
(setq show-paren-mode nil)

(defun* show-paren-local-mode (&optional (value nil value-supplied-p))
  (interactive)
  ;; The value of show-paren-mode will be local to this buffer.
  (if (not (local-variable-p 'show-paren-mode))
      (make-local-variable 'show-paren-mode))
  (let ((show))
    (if (null value-supplied-p)
        (setq show (if (null show-paren-mode) t nil))
      (setq show value))
    (setq show-paren-mode show)))

;; (defun* vr-programming-minor-modes (&optional (value nil value-supplied-p))
;;   "Enables some minor modes, useful for programming."
;;   (interactive)
;;   (if (null value-supplied-p)
;;       (progn
;;         ;; (message "*** in vr-programming-minor-modes")
;;         (if (local-variable-p 'vr-prog-mode)
;;             (progn
;;               ;; (message "*** killing vr-prog-mode")
;;               (kill-local-variable 'vr-prog-mode)
;;               ;; (nlinum-mode -1)
;;               (linum-mode -1)
;;               (show-paren-local-mode -1)
;;               (hs-minor-mode -1)
;;               ;; (subword-mode -1))
;;           (progn
;;             ;; (message "*** setting vr-prog-mode")
;;             (set (make-local-variable 'vr-prog-mode) t)
;;             ;; (nlinum-mode 1)
;;             (linum-mode 1)
;;             (show-paren-local-mode 1)
;;             (hs-minor-mode 1)
;;             ;; (subword-mode 1)
;;             ;; ;; Use case-sensitive search (buffer-local)
;;             ;; (setq case-fold-search nil)
;;             )))
;;     (progn
;;       (if value
;;           (progn
;;             (set (make-local-variable 'vr-prog-mode) t)
;;             ;; (nlinum-mode 1)
;;             (linum-mode 1)
;;             (show-paren-local-mode 1)
;;             (hs-minor-mode 1)
;;             ;; (subword-mode 1)
;;             ;; ;; Use case-sensitive search (buffer-local)
;;             ;; (setq case-fold-search nil)
;;             )
;;         (progn
;;           (if (local-variable-p 'vr-prog-mode)
;;               (kill-local-variable 'vr-prog-mode))
;;           ;; (nlinum-mode -1)
;;           (linum-mode -1)
;;           (show-paren-local-mode nil)
;;           (hs-minor-mode -1)
;;           ;; (subword-mode -1)
;;           )))))

(defun* vr-programming-minor-modes (&optional (value nil value-supplied-p))
  "Enables some minor modes, useful for programming."
  (interactive)
  (if (null value-supplied-p)
      (progn
        ;; (message "*** in vr-programming-minor-modes")
        (if (local-variable-p 'vr-prog-mode)
            (progn
              ;; (message "*** killing vr-prog-mode")
              (kill-local-variable 'vr-prog-mode)
              (linum-mode -1)
              (show-paren-local-mode -1)
              (hs-minor-mode -1))
          (progn
            ;; (message "*** setting vr-prog-mode")
            (set (make-local-variable 'vr-prog-mode) t)
            (linum-mode 1)
            (show-paren-local-mode 1)
            (hs-minor-mode 1)
            ;; ;; Use case-sensitive search (buffer-local)
            ;; (setq case-fold-search nil)
            )))
    (progn
      (if value
          (progn
            (set (make-local-variable 'vr-prog-mode) t)
            (linum-mode 1)
            (show-paren-local-mode 1)
            (hs-minor-mode 1)
            ;; ;; Use case-sensitive search (buffer-local)
            ;; (setq case-fold-search nil)
            )
        (progn
          (if (local-variable-p 'vr-prog-mode)
              (kill-local-variable 'vr-prog-mode))
          (linum-mode -1)
          (show-paren-local-mode nil)
          (hs-minor-mode -1))))))

;; == C++ Mode ==

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/rtags/")
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/irony/")

(setq vr-project-dir-name ".project")
(setq vr-c++std "-std=c++1y")

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

;; TODO: Find how to extract the following includes from rtags
(setq vr-c++-include-path (split-string
                           "
/home/rh/s600/s600-host/build/root/include
/home/rh/s600/s600-host/server"))

;; The following patch is only required for emacs < 25.
;; Once transition to emacs 25 is completed, this patch should be removed.
;; This patch if far from perfect. It is although better than nothing.
;; see http://stackoverflow.com/questions/8549351/c11-mode-or-settings-for-emacs
(defun vr-c++-11-partial-patch ()
  (message "vr-c++-11-partial-patch")
  (require 'font-lock)
  (defun --copy-face (new-face face)
    "Define NEW-FACE from existing FACE."
    (copy-face face new-face)
    (eval `(defvar ,new-face nil))
    (set new-face new-face))
  ;; labels, case, public, private, proteced, namespace-tags
  (--copy-face 'font-lock-label-face
               'font-lock-keyword-face)
  ;; comment markups such as Javadoc-tags
  (--copy-face 'font-lock-doc-markup-face
               'font-lock-doc-face)
  ;; comment markups
  (--copy-face 'font-lock-doc-string-face
               'font-lock-comment-face)
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)
  ;; We could place some regexes into `c-mode-common-hook', but note that their evaluation order
  ;; matters.
  (font-lock-add-keywords
   nil '(;; complete some fundamental keywords
         ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
         ;; namespace names and tags - these are rendered as constants by cc-mode
         ("\\<\\(\\w+::\\)" . font-lock-function-name-face)
         ;;  new C++11 keywords
         ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" 1 font-lock-keyword-face)
         ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-keyword-face)
         ;; PREPROCESSOR_CONSTANT, PREPROCESSORCONSTANT
         ("\\<[A-Z]*_[A-Z_]+\\>" . font-lock-constant-face)
         ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face)
         ;; hexadecimal numbers
         ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
         ;; integer/float/scientific numbers
         ;; ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
         ;; integer/float/scientific literals (improved)
         ("\\<[-+]?[0-9]*\\.?[0-9]+\\([uUlL]+\\|[eE][-+]?[0-9]+\\)?[fFlL]?\\>" . font-lock-constant-face)
         ;; c++11 string literals
         ;;       L"wide string"
         ;;       L"wide string with UNICODE codepoint: \u2018"
         ;;       u8"UTF-8 string", u"UTF-16 string", U"UTF-32 string"
         ("\\<\\([LuU8]+\\)\".*?\"" 1 font-lock-keyword-face)
         ;;       R"(user-defined literal)"
         ;;       R"( a "quot'd" string )"
         ;;       R"delimiter(The String Data" )delimiter"
         ;;       R"delimiter((a-z))delimiter" is equivalent to "(a-z)"

         ;; ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)[[:ascii:][:nonascii:]]*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 0 font-lock-keyword-face t) ; start/end delimiter
         ;; (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\([[:ascii:][:nonascii:]]*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face t)  ; actual string

         ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\)" 1 font-lock-keyword-face t) ; start delimiter
         (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}(\\([[:ascii:][:nonascii:]]*?\\))[^\\s-\\\\()]\\{0,16\\}\"" 1 font-lock-string-face prepend)  ; actual string
         (   "\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}([[:ascii:][:nonascii:]]*?\\()[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-keyword-face t) ; end delimiter

         ;; ("\\(\\<[uU8]*R\"[^\\s-\\\\()]\\{0,16\\}([[:ascii:][:nonascii:]]*?)[^\\s-\\\\()]\\{0,16\\}\"\\)" 1 font-lock-string-face t)

         ;; user-defined types (rather project-specific)
         ;; ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(type\\|ptr\\)\\>" . font-lock-type-face)
         ;; ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
         )
   t))

;; TODO: make all auto-complete settings buffer local
(defun vr-c++-ac-setup ()
  ;; see https://github.com/mooz/auto-complete-c-headers
  (require 'auto-complete-c-headers)
  ;; #include auto-completion search paths
  (setq achead:include-directories
        (append vr-c++-include-path
                (vr-get-g++-isystem-path)
                achead:include-directories))

  ;; ;; see https://github.com/brianjcj/auto-complete-clang
  (require 'auto-complete-clang)

  ;; i.e. 'echo "" | g++ -v -x c++ -E -'
  ;; (setq clang-completion-suppress-error 't)
  (setq ac-clang-executable (executable-find "clang-3.6"))
  (setq ac-clang-flags (append `(,vr-c++std)
                               (mapcar (lambda (item) (concat "-I" item))
                                       vr-c++-include-path)
                               (mapcar (lambda (item) (concat "-isystem" item))
                                       (vr-get-g++-isystem-path))))

  ;; (require 'rtags-ac)
  ;; (setq rtags-completions-enabled t)

  (setq ac-sources
        (append '(ac-source-c-headers
                  ac-source-clang
                  ;; ac-source-rtags
                  )
                ac-sources))
  ;; use clang-ac for for yas-expand
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; or another shortcut:
  (define-key yas-minor-mode-map (kbd "C-`") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "C-~") 'yas-prev-field))

;; (defun vr-c++-ac-setup ()
;;   (require 'ac-irony)
;;   (require 'irony-cdb)
;;   ;; (setq ac-sources
;;   ;;       (append '(ac-source-irony ac-source-yasnippet)
;;   ;;               ac-sources))
;;   (add-to-list 'ac-sources 'ac-source-irony)
;;   (irony-cdb-autosetup-compile-options)
;;   (irony-mode 1))

(defun vr-c++-get-project-path ()
  (let ((src-tree-root (locate-dominating-file
                        (file-truename default-directory)
                        vr-project-dir-name)))
    (if src-tree-root
        (concat src-tree-root vr-project-dir-name "/")
      nil)))

;; TODO: move the following function into a separate section for gud-mode
;; see http://stackoverflow.com/questions/3473134/emacs-23-1-1-with-gdb-forcing-source-windows
;; see http://stackoverflow.com/questions/24386672/use-gdb-within-emacs-always-show-the-source-code
;; see http://nurpax.github.io/posts/2014-10-12-fixing-gdb-many-windows-source-buffer.html
(defadvice gud-display-line (before one-source-window activate)
  "Always use the same window to show source code."
  (let ((buf (get-file-buffer true-file)))
    (when (and buf gdb-source-window)
      (set-window-buffer gdb-source-window buf))))

(defun vr-c++-debug-setup ()
  ;; use gdb-many-windows by default
  (setq gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  (setq gdb-show-main t))

(defun vr-c++-compile-setup ()
  (setq compilation-scroll-output t)
  (let ((path (vr-c++-get-project-path)))
    (if path
        (progn
          (set (make-local-variable 'compile-command)
               (concat path "make -k"))
          (message (concat "vr-project: " path))))))

(cl-defun vr-c++-header-line (&optional
                              (header-line-trim-indicator "\x203a")
                              (header-line-beginning-indicator " "))
  (propertize
   (let* ((header-string (concat header-line-beginning-indicator
                                 rtags-cached-current-container))
          (header-string-width (string-width header-string))
          (header-filler-width (- (window-total-width) header-string-width)))
     (if (< header-filler-width 0)
         (concat (substring header-string
                            0 (- (window-total-width)
                                 (string-width header-line-trim-indicator)))
                 header-line-trim-indicator)
       (concat header-string (make-string header-filler-width ?\ ))))
   'face 'mode-line-inactive))

;; (defun vr-c++-header-line-inactive ()
;;   (propertize
;;    (let* ((header-string (concat header-line-beginning-indicator
;;                                  rtags-cached-current-container))
;;           (header-string-width (string-width header-string))
;;           (header-filler-width (- (window-total-width) header-string-width)))
;;      (if (< header-filler-width 0)
;;          (concat (substring header-string
;;                             0 (- (window-total-width)
;;                                  (string-width header-line-trim-indicator)))
;;                  header-line-trim-indicator)
;;        (concat header-string (make-string header-filler-width ?\ ))))
;;    'face 'mode-line-inactive))

;; (defun my-update-header ()
;;   (mapc
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        (if (eq window (selected-window))
;;            (message "active")
;;            ;; (when (and (boundp 'rtags-is-indexed) (rtags-is-indexed))
;;            ;;   (setq header-line-format '(:eval (vr-c++-header-line))))
;;          ;; (when (and (boundp 'rtags-is-indexed) (rtags-is-indexed))
;;          ;;     (setq header-line-format '(:eval (vr-c++-header-line-inactive)))))))
;;          (message "inactive"))))
;;    (window-list)))

;; (defun my-update-header ()
;;   (mapc
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        (if (eq window (selected-window))
;;            (when (and (boundp 'rtags-is-indexed) (rtags-is-indexed))
;;              (setq header-line-format '(:eval (vr-c++-header-line))))
;;          (when (and (boundp 'rtags-is-indexed) (rtags-is-indexed))
;;            (setq header-line-format "xxx-xxx-xxx")))))
;;    (window-list)))

;; (setq buffer-list-update-hook nil)
;; (add-hook 'buffer-list-update-hook 'my-update-header)


;; see http://stackoverflow.com/questions/33195122/highlight-current-active-window

;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window)) 
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#111"))))))
;;   (buffer-face-set 'default))

;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)

;; (add-hook 'buffer-list-update-hook 'my-update-header)

;; (cl-defun vr-c++-header-line (&optional
;;                               (header-line-trim-indicator "\x203a")
;;                               (header-line-beginning-indicator " "))
;;   (propertize
;;    (let* ((header-string (concat header-line-beginning-indicator
;;                                  rtags-cached-current-container))
;;           (header-string-width (string-width header-string))
;;           (header-filler-width (- (window-total-width) header-string-width)))
;;      (if (< header-filler-width 0)
;;          (concat (substring header-string
;;                             0 (- (window-total-width)
;;                                  (string-width header-line-trim-indicator)))
;;                  header-line-trim-indicator)
;;        (concat header-string (make-string header-filler-width ?\ ))))
;;    'face (if (eq (window-buffer (selected-window)) (current-buffer))
;;              'mode-line-inactive
;;            'mode-line)))


;; TODO: investigave rtags settings refactoring to use flycheck, company-mode,
;; and use-package using the following guide
;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
(defun vr-c++-rtags-setup ()
  (require 'rtags)
  (setq rtags-autostart-diagnostics t)
  (rtags-start-process-unless-running)

  ;; Does not work with my clang-auto-complete setting
  ;; (setq rtags-display-current-error-as-tooltip t)

  ;; Display current function name at the top of the window (header-line).
  ;; https://github.com/Andersbakken/rtags/issues/435
  (set (make-local-variable 'rtags-cached-current-container) "")
  (setq rtags-track-container t)
  (add-hook 'find-file-hook
            (lambda ()
              ;; (when (rtags-is-indexed)
              ;;   (set (make-local-variable 'header-line-format)
              ;;        '(:eval (vr-c++-header-line)))))

              (set (make-local-variable 'header-line-format)
                   '(:eval (vr-c++-header-line))))

              ;; (setq header-line-format '(:eval (vr-c++-header-line))))
            nil t)

  (custom-set-faces
   '(rtags-errline ((((class color)) (:background "#ef8990"))))
   '(rtags-fixitline ((((class color)) (:background "#ecc5a8"))))
   '(rtags-warnline ((((class color)) (:background "#efdd6f"))))
   '(rtags-skippedline ((((class color)) (:background "#34ef85")))))

  ;; rtags-ac does not seem to be working...
  ;; (require 'rtags-ac)

  (rtags-enable-standard-keybindings)
  (define-key c-mode-base-map (kbd "M-[") 'rtags-location-stack-back)
  (define-key c-mode-base-map (kbd "M-]") 'rtags-location-stack-forward)
  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M->") 'rtags-next-match)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  ;; (define-key c-mode-base-map (kbd "M-,") 'rtags-references-tree)
  (define-key c-mode-base-map (kbd "M-<") 'rtags-find-virtuals-at-point)
  (define-key c-mode-base-map (kbd "M-i") 'rtags-imenu)
  (define-key c-mode-base-map (kbd "C-.") 'rtags-find-symbol)
  (define-key c-mode-base-map (kbd "C-,") 'rtags-find-references))

(defun vr-c++-code-folding-setup ()
  (hs-minor-mode 1))

(defun vr-c++-yas-setup ()
  ;; Use yast instead of abbrev-mode
  (abbrev-mode -1)
  (let* ((project-path (vr-c++-get-project-path))
         (snippets-path (concat project-path "snippets")))
    (if (and project-path (file-exists-p snippets-path))
        (if (not (equal yas-snippet-dirs
                        (add-to-list 'yas-snippet-dirs snippets-path)))
            (yas-reload-all))))
  (yas-minor-mode 1))

(defun vr-c++-looking-at-lambda_as_param ()
  "Return t if text after point matches '[...](' or '[...]{'"
  (looking-at ".*[,(][ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-looking-at-lambda_in_uniform_init ()
  "Return t if text after point matches '{[...](' or '{[...]{'"
  (looking-at ".*{[ \t]*\\[[^]]*\\][ \t]*[({][^}]*?[ \t]*[({][^}]*?$"))

(defun vr-c++-indentation-examine (langelem looking-at-p)
  (and (equal major-mode 'c++-mode)
       (ignore-errors
         (save-excursion
           (goto-char (c-langelem-pos langelem))
           (funcall looking-at-p)))))

(defun vr-c++-indentation-setup ()
  (require 'google-c-style)
  (google-set-c-style)

  (c-set-offset
   'block-close
   (lambda (langelem)
     (if (vr-c++-indentation-examine
          langelem
          ;; see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_170.html for '#''
          #'vr-c++-looking-at-lambda_in_uniform_init)
         '-
       0)))

  (c-set-offset
   'statement-block-intro
   (lambda (langelem)
     (if (vr-c++-indentation-examine
          langelem
          #'vr-c++-looking-at-lambda_in_uniform_init)
         0
       '+)))

  ;; see http://stackoverflow.com/questions/23553881/emacs-indenting-of-c11-lambda-functions-cc-mode
  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (vr-c++-indentation-examine
               langelem
               #'vr-c++-looking-at-lambda_as_param)
              0
            ad-do-it))))

(add-to-list 'auto-mode-alist '("/hpp\\'\\|\\.ipp\\'\\|\\.h\\'" . c++-mode))

(add-hook 'c++-mode-hook
          (lambda ()
            ;; For some reason c++-mode-hook is getting executed twice.
            ;; The following if-condition is preventing the
            ;; second execution.
            ;; (message "c++-mode-hook call")
            (if (not (local-variable-p 'vr-c++-mode-hook-called-before))
                (progn
                  (set (make-local-variable 'vr-c++-mode-hook-called-before) t)
                  (vr-programming-minor-modes t)
                  ;; (require 'google-c-style)
                  ;; (google-set-c-style)
                  (vr-c++-indentation-setup)
                  (vr-c++-yas-setup)
                  (if (< (car vr-emacs-version) 25)
                      (vr-c++-11-partial-patch))
                  (vr-c++-compile-setup)
                  (vr-c++-debug-setup)
                  (vr-c++-ac-setup)
                  (vr-c++-rtags-setup)
                  (vr-c++-code-folding-setup)

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

;; == Enhanced Java Script Mode ==

(autoload 'js2-mode "js2-mode" "JavaScript mode." t)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-to-list 'auto-mode-alist '("\\.jse?\\'" . js2-mode))
(require 'js2-highlight-vars)

(defadvice js2-enter-key (around vr-js2-enter-key ())
 (progn
   (if (use-region-p)
       (delete-region (region-beginning) (region-end)))
   ad-do-it))

(ad-activate 'js2-enter-key)

;; see http://emacswiki.org/emacs/Js2Mode
;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

(defun js2-moz-send-region-or-defun ()
  (interactive)
  (if (use-region-p)
      (progn
        (message "moz-send-region")
        (moz-send-region (region-beginning) (region-end)))
    (moz-send-defun)))

(add-hook 'js2-mode-hook (lambda ()
                           (vr-programming-minor-modes)
                           (js2-highlight-vars-mode)
                           (moz-minor-mode t)
                           (local-set-key (kbd "<f5>") 'js2-moz-send-region-or-defun)
                           (local-set-key (kbd "S-<f5>") 'inferior-moz-switch-to-mozilla)))

;; == Emacs Lisp Mode ==

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (vr-programming-minor-modes)
            (eldoc-mode 1)
            (vr-elisp-slime-nav-setup)
            (set (make-local-variable 'vr-elisp-mode) t)
            (local-set-key (kbd "<f5>") 'el-eval-region-or-last-sexp)
            (local-set-key (kbd "M-<f5>") 'eval-print-last-sexp)
            (local-set-key (kbd "S-<f5>") 'ielm-split-window)))

;; == Visual Basic Mode ==

(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vbs\\'" . visual-basic-mode))

;; == nXML mode ==

(autoload 'nxml-mode "nxml-mode" "nXML mode." t)
(add-to-list 'auto-mode-alist '("\\.xml\\'\\|\\.html\\'\\|\\.htm\\'" . nxml-mode))

(defun vr-nxml-code-folding-setup ()
  (if (not (boundp 'vr-nxml-code-folding-initialised))
      (progn
        (require 'sgml-mode)
        ;; see http://emacs.stackexchange.com/questions/2884/the-old-how-to-fold-xml-question
        ;; see http://www.emacswiki.org/emacs/HideShow
        (add-to-list 'hs-special-modes-alist
                     '(nxml-mode
                       "<!--\\|<[^/>]*[^/]>"
                       "-->\\|</[^/>]*[^/]>"
                       "<!--"
                       sgml-skip-tag-forward
                       nil))
        (setq vr-nxml-code-folding-initialised t)))
  (hs-minor-mode 1))

(add-hook 'nxml-mode-hook
          (lambda ()
            (vr-programming-minor-modes)
            (vr-nxml-code-folding-setup)))

;; -------------------------------------------------------------------
;;; Structured Text and Markup (Meta) Languages
;; -------------------------------------------------------------------

;; == Org mode ==

(setq org-replace-disputed-keys t)
(setq org-completion-use-ido t)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-<kp-right>") 'org-metaright)
            (local-set-key (kbd "M-<kp-left>") 'org-metaleft)
            (local-set-key (kbd "M-<kp-up>") 'org-metaup)
            (local-set-key (kbd "M-<kp-down>") 'org-metadown)
            (local-set-key (kbd "C-<kp-enter>") 'org-insert-heading-respect-content)
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
      (if (string= (substring fname-or-url 0 8) "file:///")
          (url-unhex-string (substring fname-or-url 7))
        fname-or-url))

    (defun urlify-escape-only (path)
      "Handle special characters for urlify."
      (replace-regexp-in-string " " "%20" path))

    (defun urlify (absolute-path)
      "Transform /absolute/path to file:///absolute/path for Gnome
with very limited support for special characters."
      (if (string= (substring absolute-path 0 1) "/")
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
;; (defun* smart-flyspell-mode (&optional (value nil value-supplied-p))
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

;; == thesaurus mode (on-line) ==

(require 'thesaurus)
;; from registration
(setq thesaurus-bhl-api-key "1c46d90bbca27832162e8b9fd70df99a")
(global-set-key (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

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
;;; Smart Autocompletion and IntelliSense Tools
;; -------------------------------------------------------------------

;; == yasnippet ==

;; see https://github.com/capitaomorte/yasnippet
(setq yas-snippet-dirs
      '("~/.emacs.d/local-snippets" ;; local snippets
        "~/.emacs.d/snippets"       ;; default collection
        ))
(require 'yasnippet)
(yas-reload-all)

;; == predictive mode ==

;; predictive install location
(add-to-list 'load-path "~/.emacs.d/lisp/predictive/")
;; predictive dictionaries install location
(add-to-list 'load-path "~/.emacs.d/lisp/predictive-dict/")
;; predictive dictionary locations
(add-to-list 'load-path "~/.emacs.d/lisp/predictive-dict/latex/")
(add-to-list 'load-path "~/.emacs.d/lisp/predictive-dict/texinfo/")
(add-to-list 'load-path "~/.emacs.d/lisp/predictive-dict/html/")
;; predictive configuration
(setq completion-overwrite nil)
(setq completion-ui-use-echo nil)
(setq predictive-auto-learn t)
(setq completion-accept-or-reject-by-default '(global . reject))
(setq completion-how-to-resolve-old-completions 'reject)
(setq predictive-auxiliary-file-location "~/.emacs.d/.predictive/")
;; load predictive package
(autoload 'predictive-mode "predictive"
  "Turn on Predictive Completion Mode." t)
;; (require 'predictive)

;; (defadvice completion-show-tooltip (around vr-completion-show-tooltip ())
;;   (progn
;;     ;; (message "completion-show-tooltip overload works")
;;     (if (not (local-variable-p 'vr-completion-tooltip-active))
;;         (progn
;;           (make-local-variable 'vr-completion-tooltip-active)
;;           (message "make-local-variable 'vr-completion-tooltip-active")))
;;     ad-do-it))
;; (ad-activate 'completion-show-tooltip)

;; (defadvice completion-cancel-tooltip (around vr-completion-cancel-tooltip ())
;;   (progn
;;     ;; (message "completion-cancel-tooltip overload works")
;;     (if (local-variable-p 'vr-completion-tooltip-active)
;;         (progn
;;           (kill-local-variable 'vr-completion-tooltip-active)
;;           (message "kill-local-variable 'vr-completion-tooltip-active")))
;;     ad-do-it))
;; (ad-activate 'completion-cancel-tooltip)

;; (defun vr-completion-accept ()
;;   (interactive)
;;   (if (local-variable-p 'vr-completion-tooltip-active)
;;       (progn
;;         ;; (message "completion-tooltip-active is t")
;;         (completion-accept))
;;     (progn
;;       ;; (message "completion-tooltip-active is nil")
;;       (completion-reject)
;;       (newline))))

(defun vr-auto-completion-keys ()
  (setq completion-auto-show nil)       ; If this variable is set outside the
                                        ; auto-completion-mode-enable-hook,
                                        ; for some reason it is not initialized.
  (define-key completion-overlay-map (kbd "<tab>") 'completion-accept)
  (define-key completion-overlay-map (kbd "<escape>") 'completion-reject)
  (define-key completion-overlay-map (kbd "<delete>") 'completion-reject)
  (define-key completion-overlay-map (kbd "<kp-delete>") 'completion-reject)

  (define-key completion-overlay-map (kbd "C-<tab>") 'completion-show-tooltip)

  (define-key completion-tooltip-map (kbd "C-n") 'completion-tooltip-cycle)
  (define-key completion-tooltip-map (kbd "C-p") 'completion-tooltip-cycle-backwards)
  (define-key completion-tooltip-map (kbd "C-<tab>") 'completion-tooltip-cycle)
  (define-key completion-tooltip-map (kbd "C-S-<tab>") 'completion-tooltip-cycle-backwards))
  ;; (define-key completion-tooltip-map (kbd "<return>") 'completion-accept)
  ;; (define-key completion-tooltip-map (kbd "<kp-enter>") 'completion-accept))

(add-hook 'auto-completion-mode-enable-hook 'vr-auto-completion-keys)
(global-set-key (kbd "<S-f7>") 'predictive-mode)

;; == auto-complete mode ==

;; used by auto-complete to display help tips
(require 'pos-tip)
;; (pos-tip-w32-max-width-height)   ; Maximize frame temporarily

;; auto-complete install location
(add-to-list 'load-path (concat vr-user-lisp-directory-path "ac/"))

;; (require 'popup-pos-tip)

(defadvice popup-tip
  (around popup-pos-tip-wrapper (string &rest args) activate)
  (if (or (eq window-system 'x) (eq window-system 'w32))
      (apply 'popup-pos-tip string args)
    ad-do-it))

(require 'auto-complete-config)
(ac-config-default)
;; requires yasnippet
;; (require 'auto-complete-auctex)

;; personal dictionary
;;(add-to-list 'ac-user-dictionary-files vr-ispell-en-dictionary)
;; no need for a default "~/.dict" file
(setq ac-user-dictionary-files
      (delete "~/.dict" ac-user-dictionary-files))
(setq ac-use-quick-help nil)
(setq ac-auto-show-menu nil)
(setq ac-use-menu-map t)

;; ac dictionaries location
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict/")

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

;; (defun vr-auto-complete-mode-keys ()
;;   (if auto-complete-mode
;;       (message "auto-complete-mode-hook works")))

;; ;; Move auto-complete-mode settings into hook, so it can be used
;; ;; with autoload.
;; (add-hook 'auto-complete-mode-hook 'vr-auto-complete-mode-keys)

(defun vr-ac-start-if-ac-mode ()
  (interactive)
  (if auto-complete-mode
      (auto-complete)
    ;; Should change the following code to
    ;; fall back to default "C-<tab>" behaviour.
    (message "No auto-completion mode running or nothing to complete.")))

(global-set-key (kbd "C-<tab>") 'vr-ac-start-if-ac-mode)
(global-set-key (kbd "<f7>") 'auto-complete-mode)

;; -------------------------------------------------------------------
;;; General Emacs enhancement modes
;; -------------------------------------------------------------------

(setq vr-ignore-buffers '("\\` " "^\\*Completions\\*$"
                          "^\\*Quail Completions\\*$"
                          "^\\*Messages\\*$" ;
                          "^\\*clang-output\\*$" "^\\*clang error\\*$"
                          "^\\*Semantic SymRef\\*$"
                          "^\\*Recent Files\\*$" "^\\*Directory\\*$"
                          "^\\*Ido Completions\\*$" "^\\*buffer-selection\\*$"
                          ;; compile
                          "^\\*compilation\\*$"
                          ;; rtags buffers
                          "^\\*rdm\\*$"
                          "^\\*RTags\\*$"
                          "^\\*RTags Diagnostics\\*$"
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

;; == ifilipb mode ==

(require 'iflipb)
(setq iflipb-ignore-buffers vr-ignore-buffers)
(setq iflipb-wrap-around t)

(global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<kp-next>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
(global-set-key (kbd "C-<kp-prior>") 'iflipb-previous-buffer)

;; == bs mode ==

;; see http://scottfrazersblog.blogspot.co.uk/2010/01/emacs-filtered-buffer-switching.html
(setq bs-configurations
      '(("all" nil nil nil nil nil)
        ("files" nil nil nil
         (lambda (buf)
           (vr-string-match-regexp-list
            vr-ignore-buffers
            (buffer-name buf)))
         nil)))

(setq bs-cycle-configuration-name "files")

(setq bs-mode-font-lock-keywords
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

(setq bs-attributes-list
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

(add-hook 'bs-mode-hook
          (lambda ()
            (hl-line-mode 1)
            (define-key bs-mode-map (kbd "<escape>") 'bs-kill)))

(global-set-key (kbd "C-x C-b") 'bs-show)

;; == popwin mode ==

(require 'popwin)
(popwin-mode 1)

;; Find how to search and replace within lists in elisp and
;; generalise the following functions
(delete 'help-mode popwin:special-display-config)
(delete '(compilation-mode :noselect t) popwin:special-display-config)
;; (push '(help-mode :stick t) popwin:special-display-config)
(push '(help-mode :stick t) popwin:special-display-config)
(push '(compilation-mode :noselect t :stick t) popwin:special-display-config)
(push '("*RTags*" :noselect t :stick t) popwin:special-display-config)

(defun vr-popwin:popup-smart ()
  (interactive)
  (if popwin:popup-window
      (popwin:select-popup-window)
    (if popwin:popup-last-config
        (popwin:popup-last-buffer)
      (popwin:messages))))

(global-set-key (kbd "C-x SPC") 'vr-popwin:popup-smart)
(global-set-key (kbd "C-x C-SPC") 'popwin:close-popup-window)
(global-set-key (kbd "C-x p") popwin:keymap)

;; see https://www.emacswiki.org/emacs/OneWindow
(add-to-list 'same-window-buffer-names "*Help*")

;; -------------------------------------------------------------------
;;; Bookmarks
;; -------------------------------------------------------------------

;; bm mode
;; see http://emacsblog.org/2007/03/22/bookmark-mania/

(setq bm-repository-file vr-bm-repository-file-path)
(setq bm-restore-repository-on-load t)
(require 'bm)

;; Shortcuts as in MS Visual Studio
(global-set-key (kbd "M-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "S-<f2>") 'bm-previous)

;; On Ubuntu unity <S-f2> is busy, so
;; using alternative shortcut
(global-set-key (kbd "C-<f2>") 'bm-toggle)

;; Only highlight the fringe of the line
(setq bm-highlight-style 'bm-highlight-only-fringe)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Loading the repository from file when on start up.
(add-hook' after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; -------------------------------------------------------------------
;;; My key bindings
;; -------------------------------------------------------------------

;; Disable annoying key binding for (suspend-frame) function
(global-unset-key (kbd "C-x C-z"))

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

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "M-v") 'yank-pop)

;; (global-set-key (kbd "M-<up>") 'shrink-window)
;; (global-set-key (kbd "M-<kp-up>") 'shrink-window)
;; (global-set-key (kbd "M-<down>") 'enlarge-window)
;; (global-set-key (kbd "M-<kp-down>") 'enlarge-window)
;; (global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "M-<kp-left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "M-<kp-right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "C-<kp-begin>") 'vr-balance-windows-horizontally)
;; (global-set-key (kbd "C-M-<kp-begin>") 'vr-balance-windows-vertically)
;; (global-set-key (kbd "C-'") 'vr-balance-windows-horizontally)
;; (global-set-key (kbd "C-M-'") 'vr-balance-windows-vertically)

;; see http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <kp-up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <kp-down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <kp-right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <kp-left>") 'windmove-left)

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
