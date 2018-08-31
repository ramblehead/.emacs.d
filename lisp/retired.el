;; /b/{ ido

;; (ido-mode 1)
;; (setq ido-enable-flex-matching t)
;; (setq ido-case-fold t)
;; (setq ido-use-filename-at-point nil)
;; (setq ido-use-url-at-point nil)
;; (setq ido-save-directory-list-file vr-ido-last-file-path)
;; (setq ido-ignore-buffers vr-ignore-buffers)

;; (ido-everywhere 1)

;; (setq ido-confirm-unique-completion t)
;; (setq confirm-nonexistent-file-or-buffer nil)

;; ido-ubiquitous mode

;; (defvar ido-ubiquitous-debug-mode nil)

;; (require 'ido-ubiquitous)
;; (ido-ubiquitous-mode 1)
;; (setq ido-ubiquitous-max-items 50000)

;; smex mode

;; (setq smex-save-file vr-smex-save-file)
;; (require 'smex)
;; (smex-initialize)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; (use-package ido-vertical-mode
;;   :config
;;   (ido-vertical-mode 1)
;;   :ensure t)

;; (use-package flx-ido
;;   :init
;;   (setq ido-enable-flex-matching t)
;;   (setq ido-use-faces nil)
;;   :config
;;   (flx-ido-mode 1)
;;   :ensure t)

;; /b/} ido

;; /b/{ helm

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

;; /b/} helm
