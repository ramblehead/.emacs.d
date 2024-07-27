;;; init.el --- ramblehead's emacs configuration  -*- lexical-binding: t; -*-
;;
;; Author: Victor Rybynok
;; Copyright (C) 2019-2023, Victor Rybynok, all rights reserved.

;; https://github.com/oantolin/orderless/issues/80
;; https://github.com/radian-software/prescient.el

(customize-set-value
 'custom-file
 (file-name-concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  ;; Do not load custom themes yet. They will be loaded manually later.
  (cl-letf (((symbol-function 'load-theme) (lambda (&rest args) nil)))
    (load custom-file)))

(defvar rh-site-start-file-paths ())

(defvar rh-emacs-config-name
  (file-name-base (directory-file-name user-emacs-directory)))

(cond
 ((equal system-type 'gnu/linux)
  (progn
    (setq rh-user-data-dir (expand-file-name "~/.local/share/"))

    ;; Make the "~/.local/share/emacs" directories
    ;; if does not already exist
    (if (not (file-exists-p (concat rh-user-data-dir "emacs")))
        (make-directory (concat rh-user-data-dir "emacs") t))

    (defvar rh-savehist-file
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "emacs-history.el"))

    (defvar rh-recent-files-file-path
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "recent-files.el"))

    (defvar rh-saved-places-file-path
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "saved-places.el"))

    (defvar rh-prescient-save-file
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "prescient-save.el"))

    (defvar rh-bm-repository-file-path
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "bm-repository.el"))

    (defvar rh-transient-history-file-path
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "transient-history.el"))

    (defvar rh-lsp-session-file-path
      (file-name-concat rh-user-data-dir
                        rh-emacs-config-name
                        "lsp-session-v1"))

    ;; Paths for the site-start.el files,
    ;; located in /usr/local/share/emacs/
    (let ((file-path "/usr/local/share/emacs/site-lisp/site-start.el")
          (ver-file-path
           (concat
            "/usr/local/share/emacs/"
            emacs-version
            "/site-lisp/site-start.el")))
      (progn
        (when (file-exists-p file-path)
          (add-to-list 'rh-site-start-file-paths file-path))
        (when (file-exists-p ver-file-path)
          (add-to-list 'rh-site-start-file-paths ver-file-path)))))))

(setq rh-user-lisp-dir
      (concat (expand-file-name user-emacs-directory) "lisp/"))

(setq rh-user-site-start-file-path
      (concat rh-user-lisp-dir "site-start.el"))

(load "~/.config/emacs-private/secret.el" t)
(load (concat "~/.config/emacs-private/systems/" system-name ".el") t)

(customize-set-value
 'custom-theme-directory
 (file-name-concat (expand-file-name user-emacs-directory) "themes/"))

(add-to-list 'load-path rh-user-lisp-dir)
(add-to-list 'load-path "/usr/share/emacs/site-lisp")
(load rh-user-site-start-file-path nil t t)

;;; /b/; straight.el
;;; /b/{

;; see
;; https://jeffkreeftmeijer.com/emacs-straight-use-package/

(defvar bootstrap-version)
(let ((bootstrap-file
       (file-name-concat user-emacs-directory
                         "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (setq straight-recipes-gnu-elpa-use-mirror t)

;;; /b/}

;;; /b/; package
;;; /b/{

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("gnu" . "http://elpa.gnu.org/packages/"))

;; (setq package-check-signature nil)

(package-initialize)

;;; /b/}

;;; /b/; use-package
;;; /b/{

(unless (require 'use-package nil 'noerror)
  (straight-use-package 'use-package)
  (require 'use-package))

(use-package gnu-elpa-keyring-update
  :config
  (gnu-elpa-keyring-update)

  :demand t
  ;; :straight t
  :ensure t)

(use-package use-package-ensure-system-package
  :demand t
  :straight t
  :ensure t)

;;; /b/}

;;; /b/; Some Ramblehead's Elisp Functions
;;; /b/{

;; see https://www.emacswiki.org/emacs/KillingAndYanking
(defun rh-yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defun rh-scroll-down-one-line ()
  (interactive)
  (let* ((point (point))
         (point-line (1+ (count-lines 1 point)))
         (window-end-line (1- (count-lines 1 (window-end)))))
    (scroll-down 1)
    (unless (= point-line window-end-line)
      (goto-char point))))

(defun rh-scroll-up-one-line ()
  (interactive)
  (let* ((point (point))
         (point-line (1+ (count-lines 1 point)))
         (window-start-line (1+ (count-lines 1 (window-start)))))
    (scroll-up 1)
    (unless (= point-line window-start-line)
      (goto-char point))))

(defun rh-scroll-left-one-line ()
  (interactive)
  (scroll-left 1))

(defun rh-scroll-right-one-line ()
  (interactive)
  (scroll-right 1))

(defvar rh-recenter-sensibly-window-height-min 20)
(defvar rh-recenter-sensibly-margin 10)

(defun rh-recenter-sensibly ()
  (interactive)
  (if (< (window-height) rh-recenter-sensibly-window-height-min)
      (recenter)
    (cond
     ((let ((top-margin (1- (- (line-number-at-pos (point))
                               (line-number-at-pos (window-start))))))
        (< top-margin rh-recenter-sensibly-margin))
      (recenter rh-recenter-sensibly-margin))
     ((let ((bottom-margin (1- (- (line-number-at-pos (window-end))
                                  (line-number-at-pos (point))))))
        (if (= bottom-margin 0)
            (recenter)
          (recenter (- rh-recenter-sensibly-margin))))))))

(defun rh-recenter--sensibly-idle-timer-handler (buf)
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil t))
      (with-selected-window win (rh-recenter-sensibly)))))

(defun rh-recenter-sensibly-with-idle-timer ()
  (run-with-idle-timer
   0 nil
   #'rh-recenter--sensibly-idle-timer-handler
   (current-buffer)))

(defun rh-recenter--idle-timer-handler (buf)
  (when (buffer-live-p buf)
    (dolist (win (get-buffer-window-list buf nil t))
      (with-selected-window win (recenter)))))

(defun rh-recenter-with-idle-timer ()
  (run-with-idle-timer
   0 nil
   #'rh-recenter--idle-timer-handler
   (current-buffer)))

(defun rh-what-face (pos)
  "Alternative to what-cursor-position [C-u C-x =] function
when only symbol face names are needed."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos)))
  (beacon-blink))

(defun rh-kill-ring-save-keep-mark (&rest _)
  (setq deactivate-mark nil))

(advice-add 'kill-ring-save :after #'rh-kill-ring-save-keep-mark)

(defun rh-window-for-display-at-direction
    (direction &optional arg window)
  (let ((win (windmove-find-other-window direction arg window)))
    (when win
      ;; Check if win is not dedicated, not side window and not minibuffer
      (if (or (window-dedicated-p win)
              (window-parameter win 'window-side)
              (window-minibuffer-p win))
          (rh-window-for-display-at-direction direction arg win)
        win))))

(defun rh-display-buffer-reuse-up (buffer alist)
  (let ((win (rh-window-for-display-at-direction 'up)))
    (when win
      (window--display-buffer buffer win 'reuse alist)
      win)))

(defun rh-display-buffer-reuse-down (buffer alist)
  (let ((win (rh-window-for-display-at-direction 'down)))
    (when win
      (window--display-buffer buffer win 'reuse alist)
      win)))

(defun rh-display-buffer-reuse-right (buffer alist)
  (let ((win (rh-window-for-display-at-direction 'right)))
    (when win
      (window--display-buffer buffer win 'reuse alist)
      win)))

(defun rh-display-buffer-reuse-left (buffer alist)
  (let ((win (rh-window-for-display-at-direction 'left)))
    (when win
      (window--display-buffer buffer win 'reuse alist)
      win)))

;; My adaptation of the native emacs function balance-windows
(defun rh-balance-windows-horizontally (&optional window-or-frame)
  (interactive)
  (let* ((window (cond
                  ((or (not window-or-frame)
                       (frame-live-p window-or-frame))
                   (frame-root-window window-or-frame))
                  ((or (window-live-p window-or-frame)
                       (window-child window-or-frame))
                   window-or-frame)
                  (t (error "Not a window or frame %s" window-or-frame))))
         (frame (window-frame window)))
    ;; Balance horizontally.
    (window--resize-reset (window-frame window) t)
    (balance-windows-1 window t)
    (when (window--resize-apply-p frame t)
      (window-resize-apply frame t)
      (window--pixel-to-total frame t)
      (run-window-configuration-change-hook frame))))

;; My adaptation of the native emacs function balance-windows
(defun rh-balance-windows-vertically (&optional window-or-frame)
  (interactive)
  (let* ((window (cond
                  ((or (not window-or-frame)
                       (frame-live-p window-or-frame))
                   (frame-root-window window-or-frame))
                  ((or (window-live-p window-or-frame)
                       (window-child window-or-frame))
                   window-or-frame)
                  (t (error "Not a window or frame %s" window-or-frame))))
         (frame (window-frame window)))
    ;; Balance vertically.
    (window--resize-reset (window-frame window))
    (balance-windows-1 window)
    (when (window--resize-apply-p frame)
      (window-resize-apply frame)
      (window--pixel-to-total frame)
      (run-window-configuration-change-hook frame))))

(defun rh-clangd-executable-find ()
  "Finds clangd executable if present."
  (let ((path (or (executable-find "clangd-17")
                  (executable-find "clangd-16")
                  (executable-find "clangd-15")
                  (executable-find "clangd-14")
                  (executable-find "clangd-13")
                  (executable-find "clangd-12")
                  (executable-find "clangd-11")
                  (executable-find "clangd-10")
                  (executable-find "clangd-9")
                  (executable-find "clangd-8")
                  (executable-find "clangd"))))
    (when path (file-name-nondirectory path))))

(defun rh-clang-format-package-find ()
  "Finds clang-format.el package."
  (or (locate-library "clang-format-17/clang-format.el")
      (locate-library "clang-format-16/clang-format.el")
      (locate-library "clang-format-15/clang-format.el")
      (locate-library "clang-format-14/clang-format.el")
      (locate-library "clang-format-13/clang-format.el")
      (locate-library "clang-format-12/clang-format.el")
      (locate-library "clang-format-11/clang-format.el")
      (locate-library "clang-format-10/clang-format.el")
      (locate-library "clang-format-9/clang-format.el")
      (locate-library "clang-format-8/clang-format.el")
      (locate-library "clang-format.el")))

(defun get-cpu-threads-count ()
  "Get the number of CPU threads."
  (string-to-number
   (shell-command-to-string "nproc")))

;;; /b/}

;;; /b/; Basic System Setup
;;; /b/{

(defvar overwrite-cursor-type 'box)
(defvar read-only-cursor-type 'hbar)
(defvar normal-cursor-type 'bar)

(defun rh-frame-configure (frame)
  ;; ;; Alternative approach would be with-selected-frame()
  ;; (with-selected-frame frame
  ;;   ;; Place your GUI settings here.
  ;;   ))

  (when (display-graphic-p frame)
    ;; (setq-default line-spacing nil)
    ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
    ;; (add-to-list 'default-frame-alist '(font . "Hack-10.5"))

    ;; face-font-family-alternatives

    ;; HiDPI
    (let ((width-pixels
           (elt (assoc 'geometry (car (display-monitor-attributes-list frame)))
                3)))
      (cond
       ((or (= width-pixels 2560) (= width-pixels 2400))
        ;; (add-to-list 'default-frame-alist '(font . "Hack-9"))
        (set-face-attribute 'default frame
                            :family "Hack"
                            :height 90
                            ;; :width 'semi-condensed
                            :weight 'normal))
       (t
        ;; (fringe-mode '(16 . 16))
        ;; (setq read-only-cursor-type '(hbar . 4))
        ;; (setq normal-cursor-type '(bar . 4))
        ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
        ;; (add-to-list 'default-frame-alist '(font . "Hack-10.5"))
        (set-face-attribute 'default (selected-frame)
                            :family "Hack"
                            :height 105
                            ;; :width 'semi-condensed
                            :weight 'normal))))))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions #'rh-frame-configure)
;;   (rh-frame-configure (selected-frame)))

(add-hook 'after-make-frame-functions #'rh-frame-configure)
(rh-frame-configure (selected-frame))

(use-package emacs
  :config
  (defconst
   display-buffer-fallback-action
   '((display-buffer-reuse-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      rh-display-buffer-reuse-down
      rh-display-buffer-reuse-up
      ;; display-buffer--maybe-pop-up-frame-or-window
      display-buffer-in-previous-window
      ;; display-buffer-use-some-window
      display-buffer-pop-up-window
      display-buffer-pop-up-frame)))

  ;; If the option load-prefer-newer is non-nil, then when searching
  ;; suffixes, load selects whichever version of a file (‘.elc’,
  ;; ‘.el’, etc.) has been modified most recently. In this case, load
  ;; doesn’t load the ‘.eln’ natively-compiled file even if it exists.
  ;; (setq load-prefer-newer t)

  ;; No ceremony
  (customize-set-value 'inhibit-startup-screen t)
  (customize-set-value 'inhibit-startup-message t)

  ;; No mice
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; No global parentheses mode
  (show-paren-mode -1)

  ;; No tabs in indentations
  (customize-set-variable 'indent-tabs-mode nil)

  ;; (customize-set-variable 'tab-stop-list
  ;;  '(4 8 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

  ;; Enable some default-disabled functions
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; No automatic backup files
  (customize-set-variable 'make-backup-files nil)

  (delete-selection-mode 1)
  (column-number-mode 1)

  ;; Do not copy font and font faces on yank
  (customize-set-value
   'yank-excluded-properties
   (append '(font face font-lock-face) yank-excluded-properties))

  ;; Scroll line Notepad in Windows! :)
  (setq-default scroll-conservatively 100000)
  (setq-default scroll-margin 0)

  (setq-default hscroll-margin 0)

  (when (and (eq window-system 'x)
             (string-match "GTK+" (version)))
    (setq focus-follows-mouse t))

  (customize-set-value 'standard-indent 2)

  (setq-default tab-width 8)
  (setq-default fill-column 80)

  (setq-default display-line-numbers-width 4)

  ;; Be quite
  (setq-default visible-bell t)

  ;; undo behaviour
  (setq-default undo-limit (* 1024 1024))
  (setq-default undo-strong-limit (* undo-limit 2))
  (setq-default undo-outer-limit (* undo-limit 100))

  (prefer-coding-system 'utf-8-unix)
  (customize-set-value 'default-input-method "russian-computer")
  ;; (setq-default default-process-coding-system '(utf-8 . utf-8))

  (setq-default frame-title-format
                (concat "%b - emacs@" system-name))

  (customize-set-value 'mouse-wheel-scroll-amount '(5 ((shift) . 1)))
  (customize-set-value 'mouse-wheel-progressive-speed nil)
  (customize-set-value 'mouse-wheel-follow-mouse t)
  (customize-set-value 'mouse-drag-copy-region nil)
  (customize-set-value 'mouse-yank-at-point t)

  (add-hook 'next-error-hook #'rh-recenter-sensibly-with-idle-timer)

  ;; http://stackoverflow.com/questions/259354/goto-file-in-emacs
  (ffap-bindings)

  ;; Windows splitting
  (customize-set-value 'split-height-threshold nil)
  ;; (setq split-width-threshold 170)
  (customize-set-value 'split-width-threshold 170)

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g.,
  ;; [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; http://emacs-fu.blogspot.co.uk/2009/12/changing-cursor-color-and-shape.html
  (defun rh-set-cursor-according-to-mode ()
    "Change cursor type according to some minor modes."
    (cond
     (buffer-read-only
      (setq cursor-type read-only-cursor-type))
     (overwrite-mode
      (setq cursor-type overwrite-cursor-type))
     (t
      (setq cursor-type normal-cursor-type))))

  (add-hook 'post-command-hook 'rh-set-cursor-according-to-mode)

  ;; see https://github.com/shosti/.emacs.d/blob/master/personal/p-display.el#L9
  (set-fontset-font t (decode-char 'ucs #x2d5b) "Noto Sans Tifinagh-9") ; ⵛ
  (set-fontset-font t (decode-char 'ucs #x2d59) "Noto Sans Tifinagh-9") ; ⵙ
  (set-fontset-font t (decode-char 'ucs #x2605) "Noto Sans Mono CJK SC-8") ; ★
  (set-fontset-font t (decode-char 'ucs #o20434) "Symbola-8.5") ; ℜ
  (set-fontset-font t (decode-char 'ucs #x2b6f) "Symbola-8.5") ; ⭯
  (set-fontset-font t (decode-char 'ucs #x2b73) "Symbola-8.5") ; ⭳
  (set-fontset-font t (decode-char 'ucs #x1f806) "Symbola-8.5") ; 🠆
  ;; (set-fontset-font t (decode-char 'ucs #x1f426) "Symbola-9.5") ; 🐦

  ;; (customize-set-variable 'find-file-visit-truename t)
  (customize-set-value 'find-file-visit-truename t)

  ;; Disable annoying key binding for (suspend-frame) function and quit
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-x C-c"))

  ;; Prevent translation from <kp-bebin> to <begin>
  (global-set-key (kbd "<kp-begin>") (lambda () (interactive)))

  ;; see http://superuser.com/questions/498533/how-to-alias-keybindings-in-emacs
  ;; for keybindings aliases. Can also be used with (current-local-map)
  (define-key
   (current-global-map)
   (kbd "C-<kp-up>")
   (lookup-key (current-global-map) (kbd "C-<up>")))

  (define-key
   (current-global-map)
   (kbd "C-<kp-down>")
   (lookup-key (current-global-map) (kbd "C-<down>")))

  (define-key
   (current-global-map)
   (kbd "C-<kp-left>")
   (lookup-key (current-global-map) (kbd "C-<left>")))

  (define-key
   (current-global-map)
   (kbd "C-<kp-right>")
   (lookup-key (current-global-map) (kbd "C-<right>")))

  (when (display-graphic-p)
    (bind-key "S-<insert>" #'yank)
    ;; (bind-key "S-<kp-insert>" #'yank)
    (bind-key "M-S-<insert>" #'rh-yank-pop-forwards)
    ;; (bind-key "M-S-<kp-insert>" #'rh-yank-pop-forwards)
    )

  ;; (eval-after-load 'term/xterm
  ;;   '(bind-key "<xterm-paste>" #'yank))

  :bind
  (;; Exit Emacs!
   ("C-x r q" . save-buffers-kill-terminal)
   ;; Editor
   ("C-v" . yank)
   ("M-v" . consult-yank-pop)
   ("M-V" . rh-yank-pop-forwards)
   ("M-Y" . rh-yank-pop-forwards)
   ;; ("C-z" . undo)
   ("C-x f" . find-file-at-point)
   ("C-=" . text-scale-adjust)
   ("M-<up>" . rh-scroll-down-one-line)
   ("M-<down>" . rh-scroll-up-one-line)
   ("M-<right>" . rh-scroll-left-one-line)
   ("M-<left>" . rh-scroll-right-one-line)
   ("<f12>" . rh-what-face)
   ;; Resize windows
   ("M-s-<up>" . enlarge-window)
   ("M-s-<kp-up>" . enlarge-window)
   ("M-s-<down>" . shrink-window)
   ("M-s-<kp-down>" . shrink-window)
   ("M-s-<left>" . shrink-window-horizontally)
   ("M-s-<kp-left>" . shrink-window-horizontally)
   ("M-s-<right>" . enlarge-window-horizontally)
   ("M-s-<kp-right>" . enlarge-window-horizontally)
   ("M-s-<kp-begin>" . rh-balance-windows-horizontally)
   ("S-M-s-<kp-begin>" . rh-balance-windows-vertically))

  :demand t)

(use-package minibuffer
  :bind
  (:map
   read--expression-map
   ("C-x <up>" . nil))
  :demand t)

(use-package windmove
  :bind
  (("C-x <up>" . windmove-up)
   ;; ("C-x <kp-up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ;; ("C-x <kp-down>" . windmove-down)
   ("C-x <right>" . windmove-right)
   ;; ("C-x <kp-right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ;; ("C-x <kp-left>" . windmove-left)
   )

  :demand t)

(use-package iflipb
  :config
  (require 'rh-buffers)

  (customize-set-value
   'iflipb-ignore-buffers
   '((lambda (buffer-nm)
       (rh-buffers-match
        rh-buffers-not-file-group
        (get-buffer buffer-nm)))))

  (customize-set-value 'iflipb-wrap-around t)

  :bind (("C-<next>" . iflipb-next-buffer)
         ("C-<kp-next>" . iflipb-next-buffer)
         ("C-<prior>" . iflipb-previous-buffer)
         ("C-<kp-prior>" . iflipb-previous-buffer))

  :straight t
  :ensure t
  :demand t)

(use-package bs
  :config
  (require 'config-bs)

  (setq
   bs-configurations
   '(("sys" nil nil nil
      (lambda (buffer)
        (not (rh-buffers-match '("\\` ") buffer)))
      rh-bs-sort-by-file-path-interns-are-last)
     ("all" nil nil nil
      (lambda (buffer)
        (rh-buffers-match '("\\` ") buffer))
      rh-bs-sort-by-file-path-interns-are-last)
     ("files" nil nil nil
      (lambda (buffer)
        (rh-buffers-match rh-buffers-not-file-group buffer))
      rh-bs-sort-by-file-path-interns-are-last)))

  (dolist (buffer-group rh-buffers-groups)
    (add-to-list
     'bs-configurations
     (rh-bs-make-configuration-from-buffer-group (car buffer-group))
     t))

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

  (add-hook
   'bs-mode-hook
   (lambda ()
     (hl-line-mode 1)))

  :bind
  (("C-x C-b" . rh-bs-show)
   ("C-c C-b" . rh-bs-toggle-bs-in-bottom-0-side-window)
   ("C-c b" . rh-bs-tmp-toggle-bottom-0-side-window))

  :after (ace-window)
  :demand t)

(use-package bm
  ;; :init
  ;; ;; restore on load (even before you require bm)
  ;; (defvar bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  ;; (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file rh-bm-repository-file-path)

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

  :bind
  (("<f2>" . bm-next)
   ("S-<f2>" . bm-previous)
   ("C-<f2>" . bm-toggle))

  :straight t
  :demand t
  :ensure t)

;; The following function is experimental and is not used at the moment
(defun configure-default-mode-line ()
  (define-key-after
    (lookup-key mode-line-column-line-number-mode-map
                [mode-line down-mouse-1])
    [total-lines-mode]
    '(menu-item "Display Total Number of Lines" total-lines-mode
		:help
                "Toggle displaying a total number of lines in the mode-line"
		:button (:toggle . total-lines-mode))
    'line-number-mode)

  (setq-default
   mode-line-format
   `((total-lines-mode
      (:propertize
       (:eval (format
               (let ((width (max (length (number-to-string total-lines))
                                 (or display-line-numbers-width 0))))
                 (concat "%" (number-to-string width) "d"))
               total-lines))
       ;; face sml/col-number
       help-echo ,(concat "Total lines mode\n"
                          "mouse-1: Display Line "
                          "and Column Mode Menu")
       mouse-face mode-line-highlight
       local-map ,mode-line-column-line-number-mode-map))

     (total-lines-mode
      (:propertize
       " "
       ;; face sml/numbers-separator
       ))

     (line-number-mode
      (:propertize
       "["
       ;; face sml/numbers-separator
       ))

     (line-number-mode
      (:propertize
       (:eval (let ((width
                     (if total-lines-mode
                         (max (length (number-to-string total-lines))
                              (or display-line-numbers-width 0))
                       (or display-line-numbers-width 0))))
                (concat "%" (number-to-string width) "l")))
       ;; face sml/line-number
       face mode-line-highlight
       help-echo ,(concat "Line number mode\n"
                          "mouse-1: Display Line "
                          "and Column Mode Menu")
       mouse-face mode-line-highlight
       local-map ,mode-line-column-line-number-mode-map))

     (column-number-mode
      (:propertize
       (:eval (if line-number-mode "," ""))
       face sml/numbers-separator))

     (column-number-mode
      (:propertize
       "%3c"
       ;; 'face 'sml/col-number
       help-echo ,(concat "Column number mode\n"
                          "nmouse-1: Display Line "
                          "and Column Mode Menu")
       mouse-face 'mode-line-highlight
       local-map ,mode-line-column-line-number-mode-map))

     (line-number-mode
      (:propertize
       "]"
       ;; 'face 'sml/numbers-separator
       )))))

;; (configure-default-mode-line)

(use-package smart-mode-line
  :config
  (require 'config-smart-mode-line)

  (customize-set-value 'sml/theme 'automatic)
  (customize-set-value 'sml/name-width 0)
  (customize-set-value 'sml/show-eol t)
  (customize-set-value 'sml/col-number-format "%3c")
  (customize-set-value 'sml/size-indication-format "%I")
  (customize-set-value 'sml/shorten-mode-string "")
  (customize-set-value 'sml/shorten-modes nil)

  (sml/setup)

  :straight t
  :after (total-lines)
  :ensure t
  :demand t)

(use-package color-theme-sanityinc-tomorrow
  :config
  (require 'config-color-theme-sanityinc-tomorrow)

  :straight t
  :demand t
  :ensure t)

;; (use-package modus-themes
;;   :straight t
;;   :demand t
;;   :ensure t)

(defun configure-colour-themes ()
  ;; (load-theme 'rh-default-customisations t)
  (color-theme-sanityinc-tomorrow-blue)
  
  ;; (modus-themes-load-theme 'modus-operandi-tinted)
  ;; (load-theme 'modus-operandi)

  ;; (customize-set-variable 'custom-enabled-themes
  ;;                         (list 'sanityinc-tomorrow-blue))

  ;; (customize-set-value 'custom-enabled-themes
  ;;                      (list 'sanityinc-tomorrow-blue))
  ;; (load-theme 'sanityinc-tomorrow-blue t)

  ;; (when (display-graphic-p)
  ;;   ;; (load-theme 'rh-sanityinc-tomorrow-blue t)
  ;;   ;; (disable-theme 'rh-sanityinc-tomorrow-blue)
  ;;   ;; (enable-theme 'rh-sanityinc-tomorrow-blue)
  ;;   (color-theme-sanityinc-tomorrow-blue))
  )

(add-hook 'emacs-startup-hook #'configure-colour-themes)

;; (use-package ctrlf
;;   :config
;;   (ctrlf-mode 1)

;;   :straight t
;;   :ensure t
;;   :demand t)

(use-package vertico
  :config
  (vertico-mode 1)

  ;; (customize-set-value 'vertico-resize t)
  ;; (setq read-minibuffer-restore-windows t)

  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  (add-to-list
   ' load-path
   (file-name-concat
    (file-name-directory (locate-library "vertico"))
    "extensions")
   t)

  :bind
  (:map
   vertico-map
   ("<next>" . vertico-scroll-up)
   ("<prior>" . vertico-scroll-down)
   ("C-x <up>" . windmove-up))

  ;; :straight (:flavor elpa)
  :straight (:flavor elpa)
  :ensure t
  :demand t)

(use-package vertico-repeat
  :config
  (require 'vertico-repeat)

  (add-to-list 'savehist-additional-variables 'vertico-repeat-history)

  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  :bind (("M-R" . vertico-repeat))
  :after (vertico savehist)
  :ensure nil)

(use-package marginalia
  :config
  (marginalia-mode 1)

  :bind
  (:map
   minibuffer-local-map
   ("M-A" . marginalia-cycle))

  :straight t
  :ensure t
  :demand t)

(use-package orderless
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)

  (customize-set-value 'completion-styles '(orderless basic))
  ;; (customize-set-value 'completion-styles '(basic orderless))

  (customize-set-value 'completion-category-defaults nil)
  ;; (customize-set-value
  ;;  'orderless-matching-styles '(orderless-regexp orderless-flex))
  (customize-set-value
   'completion-category-overrides '((file (styles basic partial-completion))))

  ;; Add "&" separator for company-mode multicomponent completion
  ;; see https://github.com/oantolin/orderless?tab=readme-ov-file#company
  (customize-set-value 'orderless-component-separator "[ &]")

  :straight t
  :ensure t)

(use-package embark
  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  :bind
  (("<f9>" . embark-act)         ;; pick some comfortable binding
   ("M-<f9>" . embark-dwim)      ;; good alternative: M-.
   ("C-h B" . embark-bindings))  ;; alternative for `describe-bindings'

  :straight t
  :ensure t
  :demand t)

;; The main purpose of installing embark package is the
;; following functionality:
;;
;; consult-line -> embark-export to occur-mode buffer ->
;; occur-edit-mode for editing of matches in buffer.
;;
;; consult-grep -> embark-export to grep-mode buffer ->
;; wgrep for editing of all matches.
;;
;; consult-find -> embark-export to dired-mode buffer ->
;; wdired-change-to-wdired-mode for editing.
(use-package embark-consult
  ;; :load-path "site-lisp/ess/lisp/"

  :hook
  (embark-collect-mode . consult-preview-at-point-mode)

  :after (consult)
  :straight t
  :ensure t
  :demand t)

;; (use-package consult-dir
;;   :config
;;   (defvar rh-consult-dir--source-recentf
;;   `(:name "Directory"
;;     :narrow ?d
;;     :category file
;;     :face consult-file
;;     :history file-name-history
;;     :action ,#'dired
;;     :state ,#'consult--file-preview
;;     :enabled ,(lambda () recentf-mode)
;;     :items ,#'consult-dir--recentf-dirs)
;;   "Recentf directory source for `consult-buffer-sources'.")

;;   (add-to-list 'consult-buffer-sources 'rh-consult-dir--source-recentf t)
  
;;   ;; :bind (("C-x C-d" . consult-dir)
;;   ;;        :map minibuffer-local-completion-map
;;   ;;        ("C-x C-d" . consult-dir)
;;   ;;        ("C-x C-j" . consult-dir-jump-file))

;;   :after (consult)
;;   :straight t
;;   :ensure t
;;   :demand t)

(use-package consult
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (customize-set-value 'register-preview-delay 0.5)
  (setq register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (customize-set-value 'xref-show-xrefs-function #'consult-xref)
  (customize-set-value 'xref-show-definitions-function #'consult-xref)

  :config
  (require 'consult)
  (require 'patch-consult)
  (require 'config-consult)

  (add-to-list 'consult-buffer-sources 'rh-consult-source-recentf-dirs t)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (customize-set-value 'consult-narrow-key "`") ;; "<", "C-+"

  ;; (customize-set-value 'consult-line-start-from-top t)

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind
  (;; C-c bindings in `mode-specific-map'
   ("C-c M-x" . consult-mode-command)
   ("C-c h" . consult-history)
   ("C-c k" . consult-kmacro)
   ("C-c m" . consult-man)
   ("C-c i" . consult-info)
   ([remap Info-search] . consult-info)
   ;; C-x bindings in `ctl-x-map'
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-<insert>" . consult-yank-pop)
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ;; M-s bindings in `search-map'
   ("M-s d" . consult-find)
   ;; ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("C-c s" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("C-s"   . consult-line)                  ;; Use consult instead of isearch-forward
   ("M-s s" . isearch-forward)               ;; Re-map isearch-forward
   ("M-s L" . consult-line-multi)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch-history)
   :map
   isearch-mode-map
   ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
   ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
   ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
   ;; Minibuffer history
   :map
   minibuffer-local-map
   ("C-s" . goto-history-element)
   ("M-s" . consult-history)                 ;; orig. next-matching-history-element
   ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  :straight t
  :ensure t
  :demand t)

(use-package total-lines
  :config
  (global-total-lines-mode 1)

  :straight t
  :demand t
  :ensure t)

(use-package rich-minority
  :config
  (require 'config-rich-minority)

  (add-to-list 'rm-blacklist " $")

  (rich-minority-mode 1)

  :straight t
  :demand t
  :ensure t)

(use-package delight
  :straight t
  :ensure t
  :demand t)

(use-package rainbow-mode
  :config
  (dolist (mode
           '(jtsx-jsx-mode
             jtsx-tsx-mode
             jtsx-typescript-mode
             python-mode
             python-ts-mode))
    (add-to-list 'rainbow-x-colors-major-mode-list mode t))

  :straight t
  :ensure t
  :defer t)

(use-package avy
  :config
  (customize-set-value 'avy-all-windows 'all-frames)

  :bind (("M-f" . avy-goto-char)
         ("M-d" . avy-goto-line))

  :straight t
  :ensure t
  :demand t)

(use-package ace-window
  :config
  (customize-set-value 'aw-dispatch-when-more-than 1)

  :bind
  (("C-c a a" . ace-window)
   ("C-c a o" . ace-select-window)
   ("C-c a s" . ace-swap-window)
   ("C-c a d" . ace-delete-window))

  :straight t
  :ensure t
  :demand t)

;; http://www.emacswiki.org/emacs/SaveHist
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00213.html
(use-package savehist
  :config
  (customize-set-value 'savehist-file rh-savehist-file)
  (savehist-mode 1))

(use-package dired
  :config
  (require 'config-dired)

  :bind
  (:map
   dired-mode-map
   ("RET" . rh-dired-find-file)
   ("<return>" . rh-dired-find-file)
   ("f" . rh-dired-find-file)
   ("<backspace>" . rh-dired-change-to-parent-dir)
   ("TAB" . rh-dired-select-next-dired-window)
   ("<kp-return>" . rh-dired-find-file)
   ("C-x C-f" . rh-dired-change-to-file)
   ("M-<return>" . rh-dired-alt-ace-select-other-window)
   ("M-<enter>" . rh-dired-alt-ace-select-other-window)
   ("C-M-<return>" . rh-dired-ace-select-other-window)
   ("C-M-<enter>" . rh-dired-ace-select-other-window)
   ("e" . rh-dired-open-file))

  :demand t)

(use-package recentf
  :config
  (require 'config-recentf)

  (customize-set-value 'recentf-save-file rh-recent-files-file-path)
  (customize-set-value 'recentf-kill-buffer-on-open t)
  (customize-set-value 'recentf-max-saved-items 1000)

  (add-hook
   'recentf-dialog-mode-hook
   (lambda ()
     (when (display-graphic-p)
       (setq cursor-type normal-cursor-type))))

  (recentf-mode 1)

  ;; When a buffer is closed, remove the associated file from the recentf
  ;; list if (1) recentf would have, by default, removed the file, or
  ;; (2) the buffer was never displayed.
  ;; see http://www.emacswiki.org/RecentFiles#toc16
  (customize-set-value 'recentf-keep '(rh-keep-default-and-visible-recentf-p))

  (when (display-graphic-p)
    (bind-key "<escape>" #'recentf-cancel-dialog))

  :bind
  (("<f4>" . recentf-open-files)
   :map
   recentf-dialog-mode-map
   ("<space>" . widget-button-press)
   ("<f4>" . rh-recentf-open-edit))

  :demand t)

(use-package saveplace
  :init
  (customize-set-value 'save-place-file rh-saved-places-file-path)

  :config
  (save-place-mode 1)

  (add-hook 'save-place-after-find-file-hook #'rh-recenter-with-idle-timer)

  (remove-hook 'dired-initial-position-hook #'save-place-dired-hook)

  :demand t)

(use-package xref
  :config
  (when (display-graphic-p)
    (bind-key "M-[" #'xref-go-back))

  :bind (("M-]" . xref-go-forward))
  :demand t)

(use-package autorevert
  :config
  (setq auto-revert-mode-text " ⭯")
  (setq auto-revert-tail-mode-text " ⭳")

  (add-to-list 'rm-blacklist " ⭯")

  :defer t)

(use-package undo-tree
  :config
  (add-to-list 'rm-blacklist " Undo-Tree")

  (global-undo-tree-mode 1)
  (customize-set-value 'undo-tree-auto-save-history nil)

  (when (display-graphic-p)
    (bind-key "C-S-z" #'undo-tree-redo))

  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))

  :straight t
  :demand t
  :ensure t)

(use-package which-key
  :init
  (customize-set-value 'which-key-is-verbose t)

  ;; This conflicts with which-key "C-h" popup
  (global-unset-key (kbd "C-h C-h"))

  :config
  (add-to-list 'rm-blacklist " WK")

  (customize-set-value 'which-key-show-prefix 'mode-line)
  (customize-set-value 'which-key-max-description-length 30)
  ;; (customize-set-value 'which-key-show-transient-maps t)

  (customize-set-value 'which-key-sort-order 'which-key-description-order)
  ;; (customize-set-value 'which-key-side-window-max-height 15)

  (which-key-mode 1)

  ;; (which-key-add-key-based-replacements
  ;;   "M-s"   "interactive-search"
  ;;   "M-s h" "highlight"
  ;;   "M-g"   "goto"
  ;;   "C-x 8" "unicode-keys")

  :bind (("<f1>" . which-key-show-top-level))

  :straight t
  :demand t
  :ensure t)

(use-package beacon
  :config
  (require 'rich-minority)

  (customize-set-value 'beacon-lighter
    (cond
     ((char-displayable-p ?Λ) " Λ")
     (t " (*)")))

  (add-to-list 'rm-blacklist " (*)")
  (add-to-list 'rm-blacklist " Λ")

  (beacon-mode 1)

  (customize-set-value 'beacon-dont-blink-commands
                       '(mouse-set-point
                         mouse-drag-region
                         ;; pop-tag-mark
                         ;; xref-pop-marker-stack
                         compile-goto-error
                         compilation-display-error))

  (add-to-list 'beacon-dont-blink-major-modes 'dired-mode t)
  ;; (add-to-list 'beacon-dont-blink-major-modes 'paradox-menu-mode t)
  (add-to-list 'beacon-dont-blink-major-modes 'bs-mode t)

  (customize-set-value 'beacon-blink-delay 0.2)
  ;; (customize-set-value 'beacon-color "gtk_selection_bg_color")
  (customize-set-value 'beacon-color 0.3)
  (customize-set-value 'beacon-blink-when-window-scrolls nil)
  ;; (customize-set-value 'beacon-blink-when-focused t)
  ;; (customize-set-value 'beacon-push-mark 1)

  :straight t
  :demand t
  :ensure t)

(use-package hideshow
  :config
  (add-to-list 'rm-blacklist " hs")

  (setq hs-allow-nesting t)
  (setq hs-isearch-open t)

  (defun rh-hs-set-up-overlay-handler (ov)
    (overlay-put ov 'display
                 (format (if (char-displayable-p ?•)
                             " [• %d •] "
                           " [* %d *] ")
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))
    (overlay-put ov 'face 'shadow))

  (setq hs-set-up-overlay 'rh-hs-set-up-overlay-handler)

  :demand t
  :ensure t)

(use-package code-groups
  :config
  (add-to-list 'rm-blacklist " code-groups")

  :straight
  (code-groups
   :type git
   :host github
   :repo "ramblehead/code-groups.el")

  :demand t
  :ensure t)

(use-package rh-project
  :config
  (add-to-list 'rm-blacklist " rh-project")

  :straight
  (rh-project
   :type git
   :host github
   :repo "ramblehead/rh-project.el")

  :demand t
  :ensure t)

(use-package hydra
  :straight t
  :demand t
  :ensure t)

(use-package pcre2el
  :straight t
  :demand t
  :ensure t)

(use-package visual-regexp
  :config
  ;; see https://stackoverflow.com/questions/15895313/let-emacs-move-the-cursor-off-screen

  (setq vr/match-separator-use-custom-face t)
  ;; (custom-set-variables '(vr/match-separator-string " -> "))

  :bind
  (("C-c q" . vr/query-replace)
   :map
   vr/minibuffer-keymap
   ("S-<return>" . newline))

  :straight t
  :defer t
  :ensure t)

(use-package multiple-cursors
  :straight t
  :ensure t
  :demand t)

;; Might need "gsettings set org.freedesktop.ibus.panel.emoji hotkey ['']"
;; see https://www.reddit.com/r/emacs/comments/wwyrgs/ctrl_semicolon_behaves_strangely/
(use-package iedit
  :config
  (setq iedit-auto-save-occurrence-in-kill-ring nil)

  ;; (custom-set-faces
  ;;  '(iedit-occurrence
  ;;    ((((background light)) (:background "deep sky blue"))))
  ;;  '(iedit-read-only-occurrence
  ;;    ((((background light)) (:background "pale turquoise")))))

  ;; (when (display-graphic-p)
  ;;   (cl-case
  ;;    (car custom-enabled-themes)
  ;;    (rh-sanityinc-tomorrow-blue
  ;;     (let ((colors
  ;;            (or (cdr (assoc 'blue color-theme-sanityinc-tomorrow-colors))
  ;;                (error "no such theme flavor"))))
  ;;       (custom-set-faces
  ;;        '(iedit-occurrence ((t (:background "dark blue"))))
  ;;        '(iedit-read-only-occurrence
  ;;          ((t (:background "dark slate blue")))))))))

  ;; (custom-set-faces
  ;;  '(iedit-occurrence ((t (:inherit highlight))))
  ;;  '(iedit-read-only-occurrence ((t (:inherit highlight)))))

  :bind
  (("M-i" . iedit-mode))

  :straight t
  :after (multiple-cursors)
  :ensure t
  :demand t)

(use-package wgrep
  :straight t
  :ensure t
  :demand t)

(use-package yasnippet-snippets
  :straight t
  :demand t
  :ensure t)

(use-package yasnippet
  :delight (yas-minor-mode " ⵙ")
  :config
  (add-to-list 'rm-blacklist " ⵙ")

  (yasnippet-snippets-initialize)
  (abbrev-mode -1)
  (yas-global-mode 1)

  :bind
  (:map
   yas-minor-mode-map
   ("<tab>" . nil)
   ("TAB" . nil)
   ("C-`" . yas-expand)
   ("C-~" . yas-prev-field))

  :straight t
  :after (yasnippet-snippets)
  :demand t
  :ensure t)

(use-package company
  :config
  (add-to-list
   'display-buffer-alist
   '("*company-documentation*"
     (display-buffer-reuse-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (customize-set-value 'company-lighter-base "CA")
  (add-to-list 'rm-blacklist " CA")

  (require 'config-company)

  (customize-set-value
   'company-format-margin-function
   ;; #'company-text-icons-margin
   #'company-detect-icons-margin)

  (customize-set-value 'company-tooltip-align-annotations t)
  (customize-set-value 'company-echo-truncate-lines nil)
  (company-echo-show)
  (customize-set-value 'company-minimum-prefix-length 1)
  (customize-set-value 'company-frontends
                       '(rh-company-pseudo-tooltip-on-explicit-action
                         company-preview-frontend
                         company-echo-metadata-frontend))
  (customize-set-value 'company-require-match nil)

  (customize-set-value 'company-idle-delay 0)
  (customize-set-value 'company-tooltip-maximum-width 80)
  (customize-set-value 'company-tooltip-minimum-width 35)
  (customize-set-value 'company-tooltip-offset-display 'lines)

  (customize-set-value
   'company-backends
   #'((company-keywords company-dabbrev-code)
      company-files (company-dabbrev company-ispell)))

  (global-set-key [remap company-complete] #'rh-complete-dwim)

  (bind-key
   [remap scroll-up-command]
   (rh-company-tooltip-cmd #'scroll-up-command #'company-next-page)
   company-active-map)

  (bind-key
   [remap scroll-down-command]
   (rh-company-tooltip-cmd #'scroll-down-command #'company-previous-page)
   company-active-map)
  
  (bind-key
   (kbd "<down>")
   (rh-company-tooltip-key (kbd "<down>") #'company-select-next)
   company-active-map)

  (bind-key
   (kbd "<up>")
   (rh-company-tooltip-key (kbd "<up>") #'company-select-previous)
   company-active-map)

  (bind-key
   (kbd "RET")
   (rh-company-tooltip-key (kbd "RET") #'company-complete-selection)
   company-active-map)

  (bind-key
   (kbd "<return>")
   (rh-company-tooltip-key (kbd "RET") #'company-complete-selection)
   company-active-map)

  (bind-key
   (kbd "<kp-return>")
   (rh-company-tooltip-key (kbd "RET") #'company-complete-selection)
   company-active-map)

  (bind-key
   (kbd "<kp-enter>")
   (rh-company-tooltip-key (kbd "RET") #'company-complete-selection)
   company-active-map)

  (bind-key
   (kbd "C-s")
   (rh-company-tooltip-key (kbd "C-s") #'company-filter-candidates)
   company-active-map)

  :bind
  (:map
   company-active-map
  ;; Use "M-h" for company-show-doc-buffer
   ("<f1>" . nil)
   ("C-h" . nil)
  ;; Use company-filter-candidates by default, i.e. C-s
  ;; In search mode use C-o to switch between filtered and unfiltered
   ("C-M-s" . nil)
  ;; Use M-l for code navigation
   ("C-w" . nil)
   ("M-l" . company-show-location)
   ("<escape>" . company-abort)
   ("<delete>" . company-abort)
   ("<kp-delete>" . company-abort)
   ("M-h" . company-show-doc-buffer)
   ("<tab>" . company-complete-selection)
   ("TAB" . company-complete-selection)
   ("C-<tab>" . company-select-next)
   ("<backtab>" . company-select-next)
   ;; ("C-S-<tab>" . company-select-previous)
   ;; ("C-S-<iso-lefttab>" . company-select-previous)
   ;; ("<down>" . rh-company-select-next-or-abort)
   ;; ("<up>" . rh-company-select-previous-or-abort)
   ;; ([return] . newline)
   ;; ("RET" . newline)
   :map
   company-search-map
   ("<escape>" . company-search-abort))

  :straight t
  :demand t
  :ensure t)

(defun rh-complete-dwim ()
  (interactive)
  (cond
   ((cgs-looking-at-auto-code-group-head-or-tail)
    (let ((lsp-mode-enabled (bound-and-true-p lsp-mode)))
      (unwind-protect
          (progn
            (when lsp-mode-enabled (lsp-disconnect))
            (cgs-generate-auto-code-group))
        (when lsp-mode-enabled (lsp)))))
   ((bound-and-true-p company-mode)
    (company-complete))
   (t
    (message "No completion candidates."))))

(global-set-key (kbd "C-<tab>") #'rh-complete-dwim)
(global-set-key (kbd "<backtab>") #'rh-complete-dwim)

(use-package prescient
  :config
  (customize-set-value 'prescient-save-file rh-prescient-save-file)

  :straight t
  :defer t
  :ensure t)

(use-package company-prescient
  :config
  (add-hook 'company-prescient-mode-hook 'prescient-persist-mode)

  :straight t
  :after (company prescient)
  :defer t
  :ensure t)

(use-package display-fill-column-indicator
  :config
  (setq-default display-fill-column-indicator-column 80)
  ;; (setq-default display-fill-column-indicator-character ?|)

  :demand t)

(use-package htmlize
  :straight t
  :defer t
  :ensure t)

(use-package sudo-edit
  :straight t
  :defer t
  :ensure t)

(use-package vterm
  :if (locate-library "vterm")
  :commands (vterm vterm-mode)
  :config
  (require 'config-vterm)

  ;; (setq vterm-use-vterm-prompt-detection-method nil)
  (setq term-prompt-regexp
        (concat "^" user-login-name "@" system-name ":.*\\$ *"))
  ;; (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] ")

  (add-hook 'vterm-mode-hook #'rh-vterm-mode-hook-handler)

  :bind
  (("<menu>" . rh-vterm-here)
   ("C-c v" . rh-vterm-here)
   ("C-c C-v" . vterm)
   :map
   vterm-mode-map
   ("<kp-end>" . rh-vterm-send-end)
   ("<kp-home>" . rh-vterm-send-home)
   ("<deletechar>" . rh-vterm-send-C-d)
   ("<kp-begin>" . rh-vterm-copy-mode)
   ("<insert>" . rh-vterm-send-insert)
   ("C-<home>" . rh-vterm-send-C-home)
   ("C-<kp-home>" . rh-vterm-send-C-home)
   ("C-<end>" . rh-vterm-send-C-end)
   ("C-<kp-end>" . rh-vterm-send-C-end)
   ("<kp-multiply>" . rh-vterm-send-<kp-multiply>)
   ("<kp-add>" . rh-vterm-send-<kp-add>)
   ("<kp-subtract>" . rh-vterm-send-<kp-subtract>)
   ("C-<up>" . rh-vterm-send-C-up)
   ("C-<down>" . rh-vterm-send-C-down)
   ("C-<kp-up>" . rh-vterm-send-C-up)
   ("C-<kp-down>" . rh-vterm-send-C-down)
   ("C-<kp-down>" . rh-vterm-send-C-down)
   ("C-x c" . rh-vterm-send-C-x_c)
   ("C-x s" . rh-vterm-send-C-x_s)
   ("C-x v" . rh-vterm-send-C-x_v)
   ("C-x C-s" . rh-vterm-send-C-x_C-s)
   ("C-< C-x" . rh-vterm-send-C-x)
   ("C-< C-c" . rh-vterm-send-C-c)
   ("C-< C-v" . rh-vterm-send-C-v)
   ("S-<f2>" . rh-vterm-send-S-f2)
   ("M-<return>" . rh-vterm-send-M-<return>)
   ("<f1>" . rh-vterm-send-f1)
   ("<f12>" . what-face)
   :map
   vterm-copy-mode-map
   ("RET" . nil)
   ("<return>" . nil)
   ("<kp-begin>" . rh-vterm-copy-mode)
   ("<kp-multiply>" . compilation-minor-mode))

  :after (bs)
  :demand t
  :pin manual)

;;; /b/}

;;; /b/; Human Languages
;;; /b/{

(use-package flyspell
  :commands flyspell-visible-mode
  :config
  (require 'config-flyspell)

  :bind
  (:map
   flyspell-mode-map
   ("C-;" . nil))
  :defer t)

(use-package ispell
  :config
  ;; This call is required for spell-fu to work correctly. It
  ;; initialises ispell-aspell-dictionary-alist before
  ;; ispell-program-name is set to "hunspell".
  (ispell-find-aspell-dictionaries)

  ;; In case if it is not already set by OS or set to another value.
  ;; (setenv "LANG" "en_GB.UTF-8")

  (setq ispell-silently-savep t)
  (setq ispell-use-framepop-p t)

  ;; (setq ispell-program-name "aspell")

  ;; (setq-default ispell-program-name "enchant-2")
  ;; (setq ispell-really-enchant t)

  (setq ispell-program-name "hunspell")
  (setq ispell-really-hunspell t)

  ;; Configure two variants of English and Russian.
  ;; ispell-set-spellchecker-params has to be called before
  ;; ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic "en_GB,en_US,ru_RU")
  (setq ispell-dictionary "en_GB,en_US,ru_RU")

  ;; (setq ispell-local-dictionary-alist
  ;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US") nil utf-8)
  ;;         ("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB") nil utf-8)
  ;;         ("ru_RU" "[а-я]" "[^[:alpha:]]" "[']" t ("-d" "ru_RU") nil utf-8)))

  (setq-default
   ispell-local-dictionary-alist
   '(("en_GB"
      "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_GB") nil utf-8)
     ("en_US"
      "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en_US") nil utf-8)
     ("ru_RU"
      "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
      "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯабвгдеёжзийклмнопрстуфхцчшщьыъэюя]"
      "['’]" nil ("-d" "ru_RU") nil utf-8)
     ("en_GB,en_US,ru_RU" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t
      ("-d" "en_GB,en_US,ru_RU") nil utf-8)))

  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal")

  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

  :bind (("C-x w" . 'ispell-word))
  :defer t)

(use-package markdown-mode
  :config
  (require 'flyspell)

  (defun rh-markdown-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1)
    (flyspell-visible-mode 1)
    (visual-line-mode 1))

  (add-hook 'markdown-mode-hook #'rh-markdown-mode-hook-handler)

  :defer t)

;;; /b/}

;;; /b/; Version Control
;;; /b/{

(use-package transient
  :config
  (require 'config-transient)

  (customize-set-value
   'transient-display-buffer-action
   ;; '(display-buffer-below-selected (side . bottom)))
   '(rh-transient-display-buffer-in-bottom-0-side-window))

  (customize-set-value 'transient-history-file rh-transient-history-file-path)

  ;; (customize-set-value 'transient-show-common-commands t)

  :straight t
  :demand t
  :ensure t)

(use-package magit
  :init
  (defvar magit-log-margin
    '(t "%F %H:%M " magit-log-margin-width t 10))

  (defvar magit-log-section-arguments
    '("--graph" "--color" "--decorate" "-n256"))

  :config
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-nm action)
       (and (not (eq major-mode 'magit-diff-mode))
            (eq (with-current-buffer buffer-nm major-mode)
                'magit-status-mode)))
     (display-buffer-same-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      display-buffer-pop-up-window)))

  (customize-set-value 'magit-bury-buffer-function #'quit-window)

  (add-hook
   'magit-process-find-password-functions
   #'magit-process-password-auth-source)

  :straight t
  :after (transient)
  :demand t
  :ensure t)

(use-package git-timemachine
  :straight t
  :defer t
  :ensure t)

;;; /b/}

;;; /b/; Programming Languages (Compilers, Debuggers, Profilers, VMs etc.)
;;; /b/{

(define-minor-mode rh-programming-minor-modes
  "Enables some minor modes, useful for programming."
  :lighter " rh-prog"
  (if rh-programming-minor-modes
      (progn
        (show-paren-local-mode 1)
        (code-groups-mode 1)
        (display-fill-column-indicator-mode 1))
    (code-groups-mode -1)
    (show-paren-local-mode -1)
    (display-fill-column-indicator-mode -1)))

(add-to-list 'rm-blacklist " rh-prog")

(use-package flycheck
  :config
  (customize-set-value 'flycheck-mode-line-prefix "Φ")

  (customize-set-value 'flycheck-check-syntax-automatically
                       '(save mode-enabled))

  ;; (customize-set-value 'flycheck-indication-mode nil)

  (customize-set-value 'flycheck-checker-error-threshold 5000)

  :straight t
  :defer t
  :ensure t)

(use-package dumb-jump
  :config
  (require 'config-dumb-jump)
  (require 'hydra)

  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

  (customize-set-value
   'dumb-jump-selector
   'xref-show-definitions-completing-read)

  (customize-set-value 'dumb-jump-prefer-searcher 'rg)

  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  :bind
  (("C-M-." . rh-dumb-jump-xref-find-definitions)
   ("C-M-/" . dumb-jump-hydra/body))

  :straight t
  :demand t
  :ensure t)

(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-auto-enabled nil)

  :config
  (add-to-list 'rm-blacklist " h-i-g")

  (customize-set-value 'highlight-indent-guides-auto-enabled nil)

  ;; (customize-set-value 'highlight-indent-guides-method 'character)
  (customize-set-value 'highlight-indent-guides-method 'column)
  ;; (customize-set-value 'highlight-indent-guides-responsive 'top)

  :straight t
  :defer t
  :ensure t)

(use-package sh-script
  :config
  (customize-set-value 'sh-basic-offset 2)

  (add-hook
   'sh-set-shell-hook
   (lambda ()
     (flycheck-mode 1)
     (rh-programming-minor-modes 1)))

  :defer t)

(use-package ielm
  :config
  (customize-set-value 'eval-expression-print-length nil)
  (customize-set-value 'eval-expression-print-level nil)

  :demand t)

;; TODO: Remove this after more treesit tests
(use-package json
  :config
  (require 'config-json)

  :defer t)

(use-package just-mode
  :straight t
  :defer t
  :ensure t)

(use-package lisp-mode
  :delight
  (emacs-lisp-mode "ξλ")
  (lisp-interaction-mode "ξλ")

  :config
  (defun rh-lisp-eval-region-or-last-sexp ()
    (interactive)
    (if (use-region-p)
        (progn
          (message "eval-region")
          (eval-region (region-beginning) (region-end)))
      (eval-last-sexp current-prefix-arg)))

  (defun rh-emacs-lisp-mode-hook-handler ()
    (when (or (eq major-mode 'emacs-lisp-mode)
              (eq major-mode 'lisp-interaction-mode))
      (setq-local company-backends
                  '((company-capf company-files) company-ispell))
      (company-prescient-mode 1))
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'emacs-lisp-mode-hook #'rh-emacs-lisp-mode-hook-handler)

  :bind
  (:map
   lisp-mode-shared-map
   ("<f5>" . rh-lisp-eval-region-or-last-sexp)
   :map
   emacs-lisp-mode-map
   ("M-TAB" . nil)
   ("C-c C-b" . nil)
   :map
   lisp-interaction-mode-map
   ("M-TAB" . nil)
   ("C-c C-b" . nil))

  :after (ielm)
  :demand t)

(use-package edebug
  :config
  (require 'config-edebug)

  (customize-set-value 'edebug-print-length -1)
  (add-hook 'edebug-mode-hook 'rh-edebug-mode-hook-handler)

  :demand t)

(use-package eldoc
  :delight (eldoc-mode " ε")
  :config
  (add-to-list 'rm-blacklist " ε")

  (require 'config-eldoc)

  (advice-add
   'eldoc-minibuffer-message
   :around #'rh-eldoc-minibuffer-message-with-ellipsis)

  (add-hook
   'special-mode-hook
   #'rh-eldoc-special-mode-hook-handler)

  :defer t)

;; see https://github.com/mickeynp/combobulate?tab=readme-ov-file#complete-example-with-tree-sitter-grammar-installation
;; and https://github.com/renzmann/treesit-auto?tab=readme-ov-file#a-rough-vanilla-equivalent
(use-package treesit
  :config
  (customize-set-value
   'treesit-language-source-alist
   '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
     ;; (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
     (css-in-js . ("https://github.com/orzechowskid/tree-sitter-css-in-js.git"))
     (python . ("https://github.com/tree-sitter/tree-sitter-python"))
     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
     (c . ("https://github.com/tree-sitter/tree-sitter-c.git"))
     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
     (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
     (nix . ("git@github.com:nix-community/tree-sitter-nix.git"))

     ;; (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
     ;; (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
     ;; (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))

     ))

  (dolist (grammar treesit-language-source-alist)
    (unless (treesit-ready-p (car grammar))
      (treesit-install-language-grammar (car grammar))))

  (dolist (mapping
           '((typescript-mode . jtsx-typescript-mode)
             ;; (js2-mode . js-ts-mode)
             (js2-mode . jtsx-jsx-mode)
             ;; (css-mode . css-ts-mode)
             ;; (bash-mode . bash-ts-mode)
             (python-mode . python-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (c-mode . c-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  (customize-set-value 'treesit-font-lock-level 4)

  :demand t)

(use-package combobulate
  :preface
  (setq combobulate-key-prefix "C-c o")

  :straight
  (combobulate
   :type git
   :host github
   :repo "mickeynp/combobulate")

  :defer t
  :ensure t)

(defun rh-clang-format-package-find ()
  "Finds clang-format.el package."
  (or (locate-library "clang-format-17/clang-format.el")
      (locate-library "clang-format-16/clang-format.el")
      (locate-library "clang-format-15/clang-format.el")
      (locate-library "clang-format-14/clang-format.el")
      (locate-library "clang-format-13/clang-format.el")
      (locate-library "clang-format-12/clang-format.el")
      (locate-library "clang-format-11/clang-format.el")
      (locate-library "clang-format-10/clang-format.el")
      (locate-library "clang-format-9/clang-format.el")
      (locate-library "clang-format-8/clang-format.el")
      (locate-library "clang-format.el")))

(use-package clang-format
  :if (rh-clang-format-package-find)
  :load-path (lambda ()
               (file-name-directory
                (rh-clang-format-package-find)))
  :defer t
  :pin manual)

(use-package c-ts-mode
  :mode "\\.c\\'\\|\\.h\\'"
  :config
  (defun rh-c-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'c-ts-mode-hook #'rh-c-ts-mode-hook-handler)

  :defer t)

(use-package toml-ts-mode
  :mode "\\.toml\\'"
  :config
  (defun rh-toml-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'toml-ts-mode-hook #'rh-toml-ts-mode-hook-handler)

  :defer t)

(use-package yaml-ts-mode
  :mode "\\.yaml\\'\\|\\.yml\\'"
  :config
  (defun rh-yaml-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'yaml-ts-mode-hook #'rh-yaml-ts-mode-hook-handler)

  :defer t)

(use-package dockerfile-ts-mode
  :mode "[dD]ockerfile\\'"
  :config
  (defun rh-dockerfile-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'dockerfile-ts-mode-hook #'rh-dockerfile-ts-mode-hook-handler)

  :defer t)

(use-package nix-ts-mode
  :mode "\\.nix\\'"
  :config
  (defun rh-nix-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1)
    (lsp-format-buffer-mode)
    (lsp-deferred))

  (add-hook 'nix-ts-mode-hook #'rh-nix-ts-mode-hook-handler)

  :straight t
  :defer t
  :ensure t)

;; see https://nixos.wiki/wiki/Editor_Modes_for_Nix_Files
(use-package lsp-nix
  :config
  (customize-set-value 'lsp-nix-nil-formatter ["alejandra" "--"])

  :after (lsp-mode)
  :defer t
  ;; Should be a part of lsp-mode
  :pin manual)

(use-package web-mode
  :mode "\\.mako\\'"
  :config
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-block-padding 2)

  (setq web-mode-enable-auto-indentation nil)
  (setq web-mode-enable-auto-closing nil)
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-auto-opening nil)
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-enable-current-element-highlight nil)

  (add-hook
   'web-mode-hook
   (lambda ()
     (setq-local company-backends (copy-tree company-backends))
     (company-mode 1)

     (setq-local electric-indent-inhibit t)
     (setq-local require-final-newline nil)

     (rh-programming-minor-modes 1)))

  :bind (:map web-mode-map
         ("C-c C-b" . nil))
  :straight t
  :defer t
  :ensure t)

(use-package python
  :config
  (setq python-indent-def-block-scale 1)

  (defun rh-python-ts-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'python-ts-mode-hook #'rh-python-ts-mode-hook-handler)

  :defer t)

(use-package blacken
  :delight (blacken-mode " B")
  :config
  (add-to-list 'rm-blacklist " Black")

  :straight t
  :defer t
  :ensure t)

;; (use-package typescript-ts-mode
;;   :mode 
;;   ("\\.ts\\'\\|\\.cts\\'\\|\\.mts\\'" . typescript-ts-mode)
;;   ("\\.tsx\\'" . tsx-ts-mode)

;;   :delight
;;   (typescript-ts-mode "ts")
;;   (tsx-ts-mode "tsx")

;;   :config
;;   (defun rh-typescript-ts-mode-handler ()
;;     (company-mode 1)
;;     (rh-programming-minor-modes 1))

;;   (add-hook 'typescript-ts-mode-hook 'rh-typescript-ts-mode-handler)

;;   (defun rh-tsx-ts-mode-hook-handler ()
;;     (company-mode 1)
;;     (rh-programming-minor-modes 1))

;;   (add-hook 'tsx-ts-mode-hook 'rh-tsx-ts-mode-hook-handler)

;;   :defer t)

(use-package jtsx
  :mode
  (("\\.jsx?\\'\\|\\.cjs\\'\\|\\.mjs\\'" . jtsx-jsx-mode)
   ("\\.tsx\\'" . jtsx-tsx-mode)
   ("\\.ts\\'\\|\\.cts\\'\\|\\.mts\\'" . jtsx-typescript-mode))

  :config
  (defun rh-jtsx-jsx-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'jtsx-jsx-mode-hook #'rh-jtsx-jsx-mode-hook-handler)

  (defun rh-jtsx-tsx-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'jtsx-tsx-mode-hook #'rh-jtsx-tsx-mode-hook-handler)

  (defun rh-jtsx-typescript-mode-hook-handler ()
    (company-mode 1)
    (rh-programming-minor-modes 1))

  (add-hook 'jtsx-typescript-mode-hook #'rh-jtsx-typescript-mode-hook-handler)

  (bind-key
   [remap backward-up-list]
   #'jtsx-backward-up-list
   jtsx-jsx-mode-map)

  (bind-key
   [remap backward-up-list]
   #'jtsx-backward-up-list
   jtsx-tsx-mode-map)

  :bind
  (:map
   jtsx-jsx-mode-map
   ("M-." . nil))

  :straight
  (jtsx
   :type git
   :host github
   :repo "ramblehead/jtsx")

  :defer t
  :ensure t)

(use-package prettier
  :config
  (customize-set-value 'prettier-lighter " P")

  :straight t
  :defer t
  :ensure t)

(use-package lsp-mode
  :init
  (defvar lsp-keymap-prefix "C-c l")
  ;; (setq lsp-use-plists t)

  :config
  (require 'company-capf)

  ;; /b/; common
  ;; /b/{

  (setq gc-cons-threshold (* 100 1024 1024)) ; 100MB
  (setq read-process-output-max (* 1024 1024)) ; 1MB

  (advice-add 'lsp--render-string :filter-return #'string-trim)

  (customize-set-value 'lsp-session-file rh-lsp-session-file-path)
  (customize-set-value 'lsp-eldoc-render-all t)
  (customize-set-value 'lsp-enable-on-type-formatting nil)
  (customize-set-value 'lsp-signature-auto-activate
                       '(:on-trigger-char :after-completion))

  (lsp-enable-which-key-integration t)

  (customize-set-value 'lsp-headerline-breadcrumb-icons-enable nil)
  (customize-set-value 'lsp-headerline-breadcrumb-segments '(symbols))

  ;; /b/}

  ;; /b/; typescript, javascript and all in between
  ;; /b/{

  (setq lsp-clients-typescript-server-args '("--stdio" "--log-level=4"))

  (add-to-list 'lsp-language-id-configuration
               '("\\.cjs\\'\\|\\.mjs\\'" . "javascript") t)

  (add-to-list 'lsp-language-id-configuration
               '("\\.cts\\'\\|\\.mts\\'" . "typescript") t)

  ;; /b/}

  :bind
  (:map
   lsp-mode-map
   ("C-c C-a" . lsp-signature-activate)
   ("C-c C-<tab>" . company-capf))

  :straight t
  :defer t
  :ensure t)

(define-minor-mode lsp-format-buffer-mode
  "Minor mode to call lsp-format-buffer on save."
  :lighter  " P"
  (if lsp-format-buffer-mode
      (progn
        (add-hook 'before-save-hook #'lsp-format-buffer nil t))
    (remove-hook 'before-save-hook #'lsp-format-buffer t)))

(use-package lsp-clangd
  :config
  (customize-set-value
   'lsp-clients-clangd-args
   `(,(concat "-j="
              (number-to-string
               (let ((cpu-threads-count (1- (get-cpu-threads-count))))
                 (if (< 1 cpu-threads-count) cpu-threads-count 1))))
     "--clang-tidy"
     "--background-index"
     "--all-scopes-completion"
     "--limit-results=0"
     "--limit-references=0"
     "--rename-file-limit=0"
     "--header-insertion=iwyu"
     ;; "--completion-style=detailed"
     "--completion-style=bundled"
     ;; "--header-insertion-decorators=false"
     "--log=info"))

  :after (lsp-mode)
  :defer t
  ;; Should be a part of lsp-mode
  :pin manual)

(use-package lsp-pyright
  :config
  (customize-set-value 'lsp-pyright-auto-search-paths nil)
  ;; (customize-set-value 'lsp-pyright-disable-language-services t)
  (customize-set-value 'lsp-file-watch-threshold 100000)

  :straight t
  :after (lsp-mode)
  :defer t
  :ensure t)

(use-package lsp-tailwindcss
  :init
  (customize-set-value 'lsp-tailwindcss-add-on-mode t)

  (customize-set-value 'lsp-tailwindcss-class-attributes
                       ["class" "className" "UNSAFE_className" "ngClass"])

  :straight t
  :after (lsp-mode)
  :defer t
  :ensure t)

(use-package clang-format
  :if (rh-clang-format-package-find)
  :load-path (lambda ()
               (file-name-directory
                (rh-clang-format-package-find)))
  :defer t
  :pin manual)

(define-minor-mode clang-format-mode
  "Minor mode to call clang-format on save."
  :lighter  " P"
  (if clang-format-mode
      (progn
        (add-hook 'before-save-hook #'clang-format-buffer nil t))
    (remove-hook 'before-save-hook #'clang-format-buffer t)))

;; see https://gist.github.com/rangeoshun/67cb17392c523579bc6cbd758b2315c1
(use-package mmm-mode
  :straight t
  :defer t
  :ensure t)

;; (use-package scss-mode
;;   :straight t
;;   :defer t
;;   :ensure t)

;;; /b/}

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
