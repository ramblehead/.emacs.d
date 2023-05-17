;;; init.el --- ramblehead's emacs configuration  -*- lexical-binding: t; -*-
;;
;; Author: Victor Rybynok
;; Copyright (C) 2019-2023, Victor Rybynok, all rights reserved.

(setq custom-file (file-name-concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(setq rh-site-start-file-paths ())

(setq emacs-config-name
      (file-name-base (directory-file-name user-emacs-directory)))

(cond
 ((equal system-type 'gnu/linux)
  (progn
    (setq rh-user-data-dir (expand-file-name "~/.local/share/"))

    ;; Make the "~/.local/share/emacs" directories if does not already exist
    (if (not (file-exists-p (concat rh-user-data-dir "emacs")))
        (make-directory (concat rh-user-data-dir "emacs") t))
    (setq rh-savehist-file
          (file-name-concat rh-user-data-dir emacs-config-name "emacs-history"))
    (setq rh-recent-files-file-path
          (file-name-concat rh-user-data-dir emacs-config-name "recent-files"))
    (setq rh-saved-places-file-path
          (file-name-concat rh-user-data-dir emacs-config-name "saved-places"))
    (setq rh-bm-repository-file-path
          (file-name-concat rh-user-data-dir emacs-config-name "bm-repository"))
    (setq rh-ido-last-file-path
          (file-name-concat rh-user-data-dir emacs-config-name "ido-last"))

    ;; Paths for the site-start.el files, located in /usr/local/share/emacs/
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

(setq rh-user-lisp-dir (concat (expand-file-name user-emacs-directory) "lisp/"))

(setq rh-user-site-start-file-path (concat rh-user-lisp-dir "site-start.el"))

(load "~/.config/emacs-private/secret.el" t)
(load (concat "~/.config/emacs-private/systems/" system-name ".el") t)

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

;;; /b/}

;;; /b/; package
;;; /b/{

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gnu-elpa-keyring-update
  :config (gnu-elpa-keyring-update)

  :demand t
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

;;; /b/}

;;; /b/; Extending Some Basic Elisp Functions
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

(defun rh-recenter-sensibly ()
  (interactive)
  (cond
   ((let ((top-margin
           (1+ (- (line-number-at-pos (point))
                  (line-number-at-pos (window-start))))))
      (< top-margin 10))
    (if (< (window-height) 20)
        (recenter)
      (recenter 10)))
   ((let ((bottom-margin
           (1+ (- (line-number-at-pos (window-end))
                  (line-number-at-pos (point))))))
      (< bottom-margin 10))
    (if (< (window-height) 20)
        (recenter)
      (recenter -10)))))


(defun rh-kill-ring-save-keep-mark (&rest _)
  (setq deactivate-mark nil))

(advice-add 'kill-ring-save :after #'rh-kill-ring-save-keep-mark)

;;; /b/}

;;; /b/; Basic System Setup
;;; /b/{

(use-package emacs
  :config
  ;; If the option load-prefer-newer is non-nil, then when searching suffixes,
  ;; load selects whichever version of a file (‘.elc’, ‘.el’, etc.) has been
  ;; modified most recently. In this case, load doesn’t load the ‘.eln’
  ;; natively-compiled file even if it exists.
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

  (delete-selection-mode 1)

  ;; Do not copy font and font faces on yank
  (customize-set-value
   'yank-excluded-properties
   (append '(font face font-lock-face) yank-excluded-properties))

  ;; Scroll line Notepad in Windows! :)
  (setq scroll-conservatively 100000)
  (setq scroll-margin 0)

  (customize-set-value 'default-input-method "russian-computer")

  (setq frame-title-format
	(concat "%b - emacs@" system-name))

  (customize-set-value 'mouse-wheel-scroll-amount '(5 ((shift) . 1)))
  (customize-set-value 'mouse-wheel-progressive-speed nil)
  (customize-set-value 'mouse-wheel-follow-mouse t)
  (customize-set-value 'mouse-drag-copy-region nil)
  (customize-set-value 'mouse-yank-at-point t)

  (add-hook
   'next-error-hook
   (lambda ()
     (rh-recenter-sensibly)))

  ;; http://stackoverflow.com/questions/259354/goto-file-in-emacs
  (ffap-bindings)

  ;; Windows splitting
  (customize-set-value 'split-height-threshold nil)
  (setq split-width-threshold 170)

  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
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

  (when (display-graphic-p)
    ;; Change cursor type according to mode
    ;; http://emacs-fu.blogspot.co.uk/2009/12/changing-cursor-color-and-shape.html
    (setq overwrite-cursor-type 'box)
    (setq read-only-cursor-type 'hbar)
    (setq normal-cursor-type 'bar)

    ;; (setq-default line-spacing nil)
    ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
    ;; (add-to-list 'default-frame-alist
    ;;              '(font . "Hack-10.5"))

    ;; HiDPI
    (let ((width-pixels
           (elt (assoc 'geometry (car (display-monitor-attributes-list))) 3)))
      (cond
       ((= width-pixels 1920)
        ;; (fringe-mode '(16 . 16))
        ;; (setq read-only-cursor-type '(hbar . 4))
        ;; (setq normal-cursor-type '(bar . 4))
        ;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
        (add-to-list 'default-frame-alist '(font . "Hack-10.5")))

       ((= width-pixels 2560)
        (add-to-list 'default-frame-alist '(font . "Hack-9")))))

    (set-face-attribute 'region nil
                        :box
                        '(:line-width
                          (-1 . -1)
                          ;; :color "gtk_selection_bg_color"
                          :color "#ea5e30"
                          :style nil)
                        :background "#ea5e30")

    ;; face-font-family-alternatives

    ;; (set-face-attribute 'default nil :font "Noto Mono" :height 90)
    ;; (set-face-attribute 'default nil
    ;;                     :family "Hack"
    ;;                     :height 90
    ;;                     :width 'semi-condensed
    ;;                     :weight 'normal)

    ;; see https://github.com/shosti/.emacs.d/blob/master/personal/p-display.el#L9
    (set-fontset-font t (decode-char 'ucs #x2d5b) "Noto Sans Tifinagh-9") ; ⵛ
    (set-fontset-font t (decode-char 'ucs #x2d59) "Noto Sans Tifinagh-9") ; ⵙ
    (set-fontset-font t (decode-char 'ucs #x2605) "Noto Sans Mono CJK SC-8") ; ★
    (set-fontset-font t (decode-char 'ucs #o20434) "Symbola-8.5") ; ℜ
    (set-fontset-font t (decode-char 'ucs #x2b6f) "Symbola-8.5") ; ⭯
    (set-fontset-font t (decode-char 'ucs #x2b73) "Symbola-8.5") ; ⭳
    (set-fontset-font t (decode-char 'ucs #x1f806) "Symbola-8.5") ; 🠆
    ;; (set-fontset-font t (decode-char 'ucs #x1f426) "Symbola-9.5") ; 🐦

    (defun rh-set-cursor-according-to-mode ()
      "Change cursor type according to some minor modes."
      (cond
       (buffer-read-only
        (setq cursor-type read-only-cursor-type))
       (overwrite-mode
        (setq cursor-type overwrite-cursor-type))
       (t
        (setq cursor-type normal-cursor-type))))

    (add-hook 'post-command-hook 'rh-set-cursor-according-to-mode))

  ;; TODO: Fix trailing-whitespace face in color-theme-sanityinc-tomorrow-blue
  (color-theme-sanityinc-tomorrow-blue)
  ;; (load-theme 'sanityinc-tomorrow-blue t)
  ;; (disable-theme 'sanityinc-tomorrow-blue)
  ;; (enable-theme 'sanityinc-tomorrow-blue)

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

  :bind
  (("C-x r q" . save-buffers-kill-terminal) ; Exit Emacs!
   ("C-z" . undo)
   ("C-x f" . find-file-at-point)
   ("M-<down>" . rh-scroll-up-one-line)
   ("M-<kp-down>" . rh-scroll-up-one-line)
   ("M-<up>" . rh-scroll-down-one-line)
   ("M-<kp-up>" . rh-scroll-down-one-line))

  :after color-theme-sanityinc-tomorrow
  :demand t)

(use-package ctrlf
  :config
  (ctrlf-mode 1)

  :straight t
  :ensure t
  :demand t)

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :ensure t)

(use-package vertico
  :config
  (vertico-mode 1)

  :straight t
  :ensure t
  :demand t)

(use-package marginalia
  :config
  (marginalia-mode 1)

  :bind (:map minibuffer-local-map
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
  (customize-set-value 'completion-category-defaults nil)
  (customize-set-value
   'completion-category-overrides '((file (styles partial-completion))))

  :straight t
  :ensure t)

(use-package delight
  :straight t
  :ensure t
  :demand t)

;; http://www.emacswiki.org/emacs/SaveHist
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00213.html
(use-package savehist
  :config
  (customize-set-value 'savehist-file rh-savehist-file)
  (savehist-mode 1))

(use-package ace-window
  :config (setq aw-dispatch-when-more-than 1)

  :bind
  (("C-c a a" . ace-window)
   ("C-c a o" . ace-select-window)
   ("C-c a s" . ace-swap-window)
   ("C-c a d" . ace-delete-window)
   ("C-x <up>" . windmove-up)
   ("C-x <kp-up>" . windmove-up)
   ("C-x <down>" . windmove-down)
   ("C-x <kp-down>" . windmove-down)
   ("C-x <right>" . windmove-right)
   ("C-x <kp-right>" . windmove-right)
   ("C-x <left>" . windmove-left)
   ("C-x <kp-left>" . windmove-left))

  :straight t
  :ensure t
  :demand t)

(use-package dired
  :config
  (require 'config-dired)

  :bind
  (:map dired-mode-map
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
     (setq cursor-type normal-cursor-type)))

  (recentf-mode 1)

  ;; When a buffer is closed, remove the associated file from the recentf
  ;; list if (1) recentf would have, by default, removed the file, or
  ;; (2) the buffer was never displayed.
  ;; see http://www.emacswiki.org/RecentFiles#toc16
  (customize-set-value 'recentf-keep '(rh-keep-default-and-visible-recentf-p))

  :bind
  (("<f4>" . recentf-open-files)
   :map recentf-dialog-mode-map
   ("<escape>" . recentf-cancel-dialog)
   ("<space>" . widget-button-press)
   ("<f4>" . rh-recentf-open-edit))

  :demand t)

(use-package saveplace
  :init
  (customize-set-value 'save-place-file rh-saved-places-file-path)

  :config
  (require 'config-saveplace)

  (save-place-mode 1)
  (remove-hook 'dired-initial-position-hook #'save-place-dired-hook)

  :demand t)

;;; /b/}

;;; /b/; Programming Languages (Compilers, Debuggers, Profilers etc.)
;;; /b/{

(use-package ielm
  :config
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)

  :demand t)

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

  (add-hook
   'emacs-lisp-mode-hook
   (lambda ()
     (show-paren-local-mode 1)))


  :bind
  (:map lisp-mode-shared-map
   ("<f5>" . rh-lisp-eval-region-or-last-sexp))
  :after ielm
  :demand t)

;;; /b/}
