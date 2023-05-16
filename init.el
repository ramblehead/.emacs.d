;;; init.el --- ramblehead's emacs configuration  -*- lexical-binding: t; -*-
;;
;; Author: Victor Rybynok
;; Copyright (C) 2019-2023, Victor Rybynok, all rights reserved.

(setq custom-file (file-name-concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file) (load custom-file))

(setq rh-site-start-file-paths ())

(setq emacs-config-name (file-name-base (directory-file-name user-emacs-directory)))

(cond
 ((equal system-type 'gnu/linux)
  (progn
    (setq rh-user-data-dir (expand-file-name "~/.local/share/"))

    ;; Make the "~/.local/share/emacs" directories if does not already exist
    (if (not (file-exists-p (concat rh-user-data-dir "emacs")))
        (make-directory (concat rh-user-data-dir "emacs") t))
    (setq rh-savehist-file (file-name-concat rh-user-data-dir emacs-config-name "emacs-history"))
    (setq rh-recent-files-file-path (file-name-concat rh-user-data-dir emacs-config-name "recent-files"))
    (setq rh-saved-places-file-path (file-name-concat rh-user-data-dir emacs-config-name "saved-places"))
    (setq rh-bm-repository-file-path
          (file-name-concat rh-user-data-dir emacs-config-name "bm-repository"))
    (setq rh-ido-last-file-path (file-name-concat rh-user-data-dir emacs-config-name "ido-last"))

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

(setq rh-user-lisp-dir
      (concat (expand-file-name user-emacs-directory) "lisp/"))

(setq rh-user-site-start-file-path
      (concat rh-user-lisp-dir "site-start.el"))

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
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
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

;;; /b/; Basic System Setup
;;; /b/{

(use-package color-theme-sanityinc-tomorrow
  :straight t
  :ensure t)

(use-package emacs
  :config
  ;; If the option load-prefer-newer is non-nil, then when searching suffixes,
  ;; load selects whichever version of a file (‘.elc’, ‘.el’, etc.) has been
  ;; modified most recently. In this case, load doesn’t load the ‘.eln’
  ;; natively-compiled file even if it exists.
  ;; (setq load-prefer-newer t)

  ;; No ceremony
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)

  ;; No mice
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; No global parentheses mode
  (show-paren-mode -1)

  (setq default-input-method "russian-computer")

  (setq frame-title-format
	(concat "%b - emacs@" system-name))

  ;; http://stackoverflow.com/questions/259354/goto-file-in-emacs
  (ffap-bindings)

  ;; Windows splitting
  (setq split-height-threshold nil)
  (setq split-width-threshold 170)

  (defalias 'yes-or-no-p 'y-or-n-p)

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
   ("C-x f" . find-file-at-point))

  :demand t)

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
  (setq save-place-file rh-saved-places-file-path)

  ;; TODO: update to the new advice style
  ;; (defadvice save-place-find-file-hook
  ;;     (around rh-save-place-find-file-hook activate)
  ;;   (when ad-do-it
  ;;     (run-with-timer
  ;;      0 nil
  ;;      (lambda (buf)
  ;;        (dolist (win (get-buffer-window-list buf nil t))
  ;;          (with-selected-window win (recenter))))
  ;;      (current-buffer))))

  ;; TODO: remove old emacs support
  ;; (if (version< emacs-version "25.0")
  ;;     (progn
  ;;       (require 'saveplace)
  ;;       (setq-default save-place t))
  ;;   (save-place-mode 1))

  :config
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


  :bind (:map lisp-mode-shared-map
         ("<f5>" . rh-lisp-eval-region-or-last-sexp))
  :after ielm
  :demand t)

;;; /b/}
