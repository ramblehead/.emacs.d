;;; init.el --- ramblehead's emacs configuration
;;
;; Author: Victor Rybynok
;; Copyright (C) 2019, 2020, Victor Rybynok, all rights reserved.

;; ------------------------------------------------------------------
;;; Bugs, TODO and R&D
;; ------------------------------------------------------------------
;; /b/{

;; TODO:
;; [ ] Switch to color-theme-sanityinc-tomorrow-blue.
;; [ ] Refactor skewer-mode config to rh-style.
;; [ ] Refactor web-mode config to rh-style.
;; [ ] Remove rh-scratch-js mode after js-interaction scratches are implemented.
;; [ ] Convert rh-project functions to rh-project mode.
;; [ ] Convert code-groups functions to code-groups mode.

;; R&D:
;; [?] Bring Emacs init back to terminal-friendly state. Possibly, switch to
;;     text terminal as the default...
;; [?] Investigate how to fix beacon and hide-show overlays to overlap
;;     without shifting one-another.
;; [?] Try using tabs for side windows:
;;     https://github.com/manateelazycat/awesome-tab
;; [?] Use instead of/with which-key?: https://gitlab.com/jjzmajic/hercules.el

;; /b/}

;; ------------------------------------------------------------------
;;; New packages to consider
;; ------------------------------------------------------------------
;; /b/{

;; * Emacs
;;   * https://www.emacswiki.org/emacs/GccEmacs

;; * Ido, Ivy, etc.
;;   * https://github.com/raxod502/selectrum
;;   * https://github.com/minad/vertico

;; * Async
;;   * https://github.com/jwiegley/emacs-async

;; * Reference emacs configs
;;   * See the following for LSP
;;     https://github.com/ianpan870102/.personal-emacs.d/blob/master/init.el

;; * Package managers
;;   https://github.com/raxod502/straight.el
;;   https://github.com/emacscollective/borg

;; * REST clients
;;   https://github.com/federicotdn/verb
;;   https://github.com/pashky/restclient.el
;;   https://emacs.stackexchange.com/questions/2427/how-to-test-rest-api-with-emacs

;; * Terminal
;;   https://github.com/CyberShadow/term-keys
;;   http://rash-lang.org/
;;   https://github.com/emacsmirror/equake

;; * Javascript/TypeScript/CSS/...
;;   https://github.com/taiju/ob-deno
;;   https://github.com/orzechowskid/css-in-js.el

;; https://github.com/polymode/polymode
;; https://github.com/10sr/git-walktree-el

;; * Static elisp analyser: https://github.com/emacs-elsa/Elsa
;;   https://emacs.stackexchange.com/questions/12997/how-do-i-use-nadvice
;;   https://github.com/bmag/emacs-purpose
;;   https://github.com/raxod502/straight.el
;;   https://github.com/yuutayamada/suggestion-box-el
;;   https://github.com/syohex/emacs-quickrun
;;   https://github.com/zk-phi/indent-guide

;; * Steal backups and auto-saving strategy from the following config:
;;   https://sriramkswamy.github.io/dotemacs/#orgheadline451

;; see https://www.quicklisp.org/beta/ for lisp libraries
;; Can then do magic like this:
;; (ql:quickload "alexandria")
;; (alexandria:flatten list)

;; /b/}

;; ------------------------------------------------------------------
;;; Emacs custom file
;; ------------------------------------------------------------------
;; /b/{

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; /b/}

;; ------------------------------------------------------------------
;;; Emacs Version Variables
;; ------------------------------------------------------------------
;; /b/{

(setq rh-emacs-version-string
      (replace-regexp-in-string
       "GNU Emacs \\([0-9]+.[0-9]+.[0-9]+\\).*" "\\1"
       (replace-regexp-in-string "\n" "" (emacs-version))))

(setq rh-emacs-version
      (mapcar 'string-to-number (split-string rh-emacs-version-string "\\.")))

;; /b/}

;; ------------------------------------------------------------------
;;; File Location Variables
;; ------------------------------------------------------------------
;; /b/{

(setq rh-site-start-file-paths ())

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
    (setq rh-bm-repository-file-path
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
    (setq rh-bm-repository-file-path
          (concat vr-user-data "emacs/bm-repository"))
    (setq vr-ido-last-file-path
          (concat vr-user-data "emacs/ido-last"))
    ;; Paths for the site-start.el files, located in /usr/local/share/emacs/
    (let ((file-path "/usr/local/share/emacs/site-lisp/site-start.el")
          (ver-file-path (concat "/usr/local/share/emacs/"
                                 rh-emacs-version-string
                                 "/site-lisp/site-start.el")))
      (progn
       (when (file-exists-p file-path)
         (add-to-list 'rh-site-start-file-paths file-path))
       (when (file-exists-p ver-file-path)
         (add-to-list 'rh-site-start-file-paths ver-file-path)))))))

(setq rh-user-lisp-directory-path
      (concat (expand-file-name user-emacs-directory) "lisp/"))
(setq rh-user-site-start-file-path
      (concat rh-user-lisp-directory-path "site-start.el"))

;; /b/}

;; ------------------------------------------------------------------
;;; Helper functions and common modules
;; ------------------------------------------------------------------
;; /b/{

;; TODO: Remove cond clause when all rh systems are
;;       migrated from `~/.emacs-private.d' to `~/box/woods/pit/emacs'
(cond
 ((file-directory-p "~/.emacs-private.d")
  (load "~/.emacs-private.d/secret.el" t)
  (load (concat "~/.emacs-private.d/systems/" system-name ".el") t))
 ((file-directory-p "~/.config/emacs-private")
  (load "~/.config/emacs-private/secret.el" t)
  (load (concat "~/.config/emacs-private/systems/" system-name ".el") t)))

;; /b/{ Package initialisation and `use-package' bootstrap

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

(if (version< "27.0" emacs-version)
    (unless package--initialized
      (package-initialize t))
  (progn
    (setq package-enable-at-startup nil)
    (package-initialize)))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gnu-elpa-keyring-update
  :config
  (gnu-elpa-keyring-update)
  :demand t
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

;; /b/} Package initialisation and `use-package' bootstrap

(use-package paradox
  :config
  (customize-set-variable 'paradox-github-token
                          (if (boundp 'rh-paradox-github-token)
                              rh-paradox-github-token
                            t))

  (setq paradox-automatically-star nil)
  (paradox-enable)

  :bind (:map paradox-menu-mode-map
         ("q" . rh-quit-window-kill))
  :demand t
  :ensure t)

(use-package async
  :ensure t)

(use-package cl-lib
  :demand t)

;; (use-package cl
;;   :demand t)

(use-package list-utils
  :ensure t)

;; (use-package test-simple
;;   :ensure t)

;; == Auxiliary functions ==

(defvar rh-interactively-selected-window nil)

(add-hook
 'buffer-list-update-hook
 (lambda ()
   (setq rh-interactively-selected-window (frame-selected-window))))

(defun rh-window-selected-interactively-p ()
  (eq (selected-window) rh-interactively-selected-window))

(defun rh-point-or-region ()
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

(cl-defun rh-toggle-display (buffer-name
                             &optional
                             dedicated display-buffer-alist-override)
  (let* ((buffer (get-buffer buffer-name))
         (buffer-window-list (get-buffer-window-list buffer-name))
         (buffer-window (seq-find
                         (lambda (win)
                           (window-parameter win 'window-side))
                         buffer-window-list)))
    (if buffer
        (if buffer-window
            (delete-window buffer-window)
          (let ((display-buffer-alist (or display-buffer-alist-override
                                          display-buffer-alist)))
            (if dedicated
                (set-window-dedicated-p (display-buffer buffer-name) t)
              (display-buffer buffer-name))))
      (message (concat "\"" buffer-name "\""
                       " buffer does not exist.")))))

;; == Convenience interactive functions ==

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

(defun what-face (pos)
  "Alternative to what-cursor-position [C-u C-x =] function
when only symbol face names are needed."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos)))
  (beacon-blink))

(global-set-key (kbd "<f12>") 'what-face)

(defun rh-quit-window-kill ()
  (interactive)
  (quit-window t))

;; /b/}

;; -------------------------------------------------------------------
;;; Emacs Packages Tree (that is where ramblehead's packages grow)
;; -------------------------------------------------------------------

;; /b/{ rh-project

(defvar rh-project-initialised-projects '())
(defvar rh-project-trusted-dir-marker ".rh-trusted")
(defvar rh-project-dir-name ".project")
(defvar rh-project-generators-relative-path "../auto-code/")

(defun rh-project-buffer-dir ()
  (or (and buffer-file-name
           (file-name-directory buffer-file-name))
      ;; (and (eq major-mode 'compilation-mode)
      (and (or (eq major-mode 'compilation-mode)
               (eq major-mode 'dired-mode))
           default-directory)))

;; (defun rh-project-get-path ()
;;   (let ((src-tree-root (or (and buffer-file-name
;;                                 (locate-dominating-file
;;                                  (file-name-directory buffer-file-name)
;;                                  rh-project-dir-name))
;;                            (and (or (eq major-mode 'compilation-mode)
;;                                     (eq major-mode 'dired-mode))
;;                                 (locate-dominating-file
;;                                  default-directory
;;                                  rh-project-dir-name)))))
;;     (when src-tree-root
;;       (file-name-as-directory (concat src-tree-root rh-project-dir-name)))))

(defun rh-project-get-path ()
  (let* ((buffer-dir (rh-project-buffer-dir))
         (src-tree-root (and buffer-dir
                             (locate-dominating-file
                              buffer-dir
                              rh-project-dir-name))))
    (when src-tree-root
      (file-name-as-directory (concat src-tree-root rh-project-dir-name)))))

;; (defun rh-project-in-trusted-dir ()
;;   (and buffer-file-name
;;        (locate-dominating-file
;;         (file-name-directory buffer-file-name)
;;         rh-project-trusted-dir-marker)))

(defun rh-project-in-trusted-dir ()
  (locate-dominating-file
   (rh-project-buffer-dir)
   rh-project-trusted-dir-marker))

(defun rh-project-get-root ()
  (let ((rh-project (rh-project-get-path)))
    (when rh-project
      (abbreviate-file-name
       (expand-file-name (concat rh-project "../"))))))

;; (cl-defun rh-project-setup (&optional (setup-file-name-base "setup" supplied-p))
;;   (let ((rh-project (rh-project-get-path)))
;;     (when rh-project
;;       (if supplied-p
;;           (load (concat rh-project setup-file-name-base "-setup.el"))
;;         (let ((setup-file-name (concat rh-project setup-file-name-base ".el")))
;;           (when (file-exists-p setup-file-name)
;;             (load setup-file-name)))))))

(cl-defun rh-project-setup ()
  (let ((rh-project-path (rh-project-get-path)))
    (when rh-project-path
      (when buffer-file-name
        (message (concat "rh-project: " rh-project-path)))
      (let ((setup-file-path (concat rh-project-path "setup.el"))
            (init-file-path (concat rh-project-path "init.el"))
            (rh-project-id (directory-file-name
                            (expand-file-name rh-project-path))))
        (if (and (not (member rh-project-id rh-project-trusted-ids))
                 (not (rh-project-in-trusted-dir)))
            (message (concat "rh-project: '" rh-project-id
                             "' is not trusted. "
                             "Ignoring its 'init.el' and 'setup.el' files."))
          (when (and (file-exists-p init-file-path)
                     (not (member rh-project-id rh-project-initialised-projects)))
            (add-to-list 'rh-project-initialised-projects rh-project-id)
            (load init-file-path))
          (when (file-exists-p setup-file-path)
            (load setup-file-path nil t)))))))

(defun rh-project-get-generators-path ()
  (let ((generators-path (concat
                          (rh-project-get-path)
                          rh-project-generators-relative-path)))
    (when (file-directory-p generators-path)
      (expand-file-name generators-path))))

;; (defun rh-project-compile (command compilation-buffer-name)
;;   (let* ((project-path (rh-project-get-path))
;;          (full-command (concat project-path command))
;;          (compilation-buffer-name-function
;;           (lambda (name-of-mode) compilation-buffer-name)))
;;     (setq rh-bs-bottom-0-side-window-buffer
;;           (compile full-command))))

(defun rh-project-compile (command compilation-buffer-or-name)
  (let* ((project-path (rh-project-get-path))
         (full-command (concat project-path command))
         (current-window (frame-selected-window))
         compilation-buffer)
    (when (get-buffer compilation-buffer-or-name)
      (with-current-buffer compilation-buffer-or-name
        (when (vterm-check-proc)
          (vterm-send-C-c)
          ;; Giving 1 sec for complication process to end; if it does not end the
          ;; following kill-buffer() should ask whether to kill it with live
          ;; process.
          (sit-for 1))
        (kill-buffer)))
    (setq compilation-buffer (generate-new-buffer compilation-buffer-or-name))
    (g2w-buffer-destination-window-init compilation-buffer current-window nil)
    (with-current-buffer compilation-buffer
      (vterm-mode)
      (setq-local rh-project-compile t)
      ;; (compilation-minor-mode)
      (vterm-send-string
       (concat "TIMEFORMAT=\"\nCompilation took %Rs\" && time "
               full-command
               "; exit"))
      (vterm-send-return))
    (rh-bs-display-buffer-in-bootom-0-side-window compilation-buffer)))

(defalias 'rh-project-run #'rh-project-compile)

;; (defun rh-project-run
;;     (command compilation-buffer-or-name &optional compilation)
;;   (let* ((project-path (rh-project-get-path))
;;          (full-command (concat project-path command))
;;          (current-window (frame-selected-window))
;;          compilation-buffer)
;;     (when (get-buffer compilation-buffer-or-name)
;;       (kill-buffer compilation-buffer-or-name))
;;     (setq compilation-buffer (generate-new-buffer compilation-buffer-or-name))
;;     (g2w-buffer-destination-window-init compilation-buffer current-window nil)
;;     (with-current-buffer compilation-buffer
;;       (vterm-mode)
;;       (when compilation (compilation-minor-mode))
;;       (vterm-send-string
;;        (concat "TIMEFORMAT=\"\nCompilation took %Rs\" && time "
;;                full-command
;;                "; exit"))
;;       (vterm-send-return))
;;     (rh-bs-display-buffer-in-bootom-0-side-window compilation-buffer)))

;; (defun rh-project-compile (command compilation-buffer-or-name)
;;   (rh-project-run command compilation-buffer-or-name t))

(defun rh-project-term (term-buffer-or-name &optional pwd)
  (let* ((vterm-pwd (or pwd (rh-project-get-root)))
         (vterm-buffer (get-buffer term-buffer-or-name)))
    (if (and vterm-buffer (get-buffer-process vterm-buffer))
        (rh-bs-display-buffer-in-bootom-0-side-window vterm-buffer)
      (if vterm-buffer (kill-buffer vterm-buffer))
      (setq vterm-buffer (get-buffer-create term-buffer-or-name))
      (with-current-buffer vterm-buffer
        (setq-local vterm-kill-buffer-on-exit t)
        (setq-local default-directory vterm-pwd)
        (vterm-mode)))
    (rh-bs-display-buffer-in-bootom-0-side-window vterm-buffer)
    (select-window (rh-bs-get-bootom-0-side-window))))

;; (defun rh-project-restart-shell-command (command output-buffer-or-name)
;;   (let* ((project-path (rh-project-get-path))
;;          (output-buffer (get-buffer-create output-buffer-or-name))
;;          (full-command (concat project-path command))
;;          (proc (get-buffer-process output-buffer)))
;;     (unless (get-buffer-window output-buffer 'visible)
;;       (rh-bs-display-buffer-in-bootom-0-side-window output-buffer))
;;     (when proc (delete-process proc))
;;     (rh-bs-async-shell-command full-command output-buffer)
;;     (with-current-buffer output-buffer
;;       (setq-local buffer-read-only t)
;;       (local-set-key (kbd "C-q") #'rh-bs-delete-sibling-windows)
;;       (local-set-key (kbd "q") #'delete-window))
;;     (setq rh-bs-bottom-0-side-window-buffer output-buffer)
;;     (get-buffer-process output-buffer)))

(defun rh-project-run-shell-command (command output-buffer-or-name)
  (let* ((project-path (rh-project-get-path))
         (full-command (concat project-path command))
         output-buffer)
    (when (get-buffer output-buffer-or-name)
      (with-current-buffer output-buffer-or-name
        (vterm-send-C-c)
        (sit-for 0.1)
        (vterm-send-C-c)
        ;; (sit-for 1)
        (vterm-send-string "exit")
        (vterm-send-return)
        (sit-for 1)
        (kill-buffer)))
    (setq output-buffer (generate-new-buffer output-buffer-or-name))
    (with-current-buffer output-buffer
      (vterm-mode)
      ;; (compilation-minor-mode)
      (vterm-send-string full-command)
      (vterm-send-return))
    (rh-bs-display-buffer-in-bootom-0-side-window output-buffer)
    (get-buffer-process output-buffer)))

(defun rh-project-kill-shell-process
    (output-buffer-or-name &optional interrupt)
  (let* ((buffer (get-buffer output-buffer-or-name))
         (proc (get-buffer-process buffer)))
    (when (and buffer (not (get-buffer-window buffer 'visible)))
      (rh-bs-display-buffer-in-bootom-0-side-window buffer))
    (when proc
      (if interrupt
          (interrupt-process proc)
        (kill-process proc)))))

;; /b/} rh-project

;; /b/{ code-groups

(defvar-local cg-forward-list-original #'forward-list
  "Original forward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-n")

(defvar-local cg-backward-list-original #'backward-list
  "Original backward-list function used by the major mode before loading
code-groups minor mode - i.e. the function usually bound to C-M-p")

(defvar cg-doxygen-group-open-token "///@{")
(defvar cg-doxygen-group-close-token "///@}")

(defvar cg-auto-code-group-open-token "/a/{")
(defvar cg-auto-code-group-close-token "/a/}")

(defvar cg-block-code-group-open-token "/b/{")
(defvar cg-block-code-group-close-token "/b/}")

(defvar cg-custom-code-group-open-token "/c/{")
(defvar cg-custom-code-group-close-token "/c/}")

(defvar cg-auto-code-group-param-token "/a/$")

(defun cg-group-head-regexp (open-token)
  (concat "^.*" open-token ".*$"))

(defun cg-group-tail-regexp (close-token)
  (concat "^.*" close-token ".*$"))

(defun cg-looking-at-group-head (open-token)
  (let ((line (thing-at-point 'line t)))
    (when (and line
               (string-match-p (concat "^.*" open-token ".*$") line))
      open-token)))

(defun cg-looking-at-group-tail (close-token)
  (let ((line (thing-at-point 'line t)))
    (when (and line
               (string-match-p (concat "^.*" close-token ".*$") line))
      close-token)))

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
  (let (open-token close-token)
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
  (let (open-token close-token)
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
                  (end-of-visual-line)
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

(defun cg-delete-code-group ()
  (interactive)
  (when (cg-looking-at-any-group-tail)
    (cg-search-backward-group-balanced-head))
  (when (cg-looking-at-any-group-head)
    (let ((start) (end))
      (move-beginning-of-line 2)
      (setq start (point))
      (previous-line)
      (cg-search-forward-group-balanced-tail)
      (move-beginning-of-line nil)
      (setq end (point))
      (goto-char start)
      (delete-region start end))))

;; (defun cg-generate-auto-code (generator data template indent-str)
;;   (let ((generators-path (rh-project-get-generators-path))
;;         (auto-code-command generator))
;;     (when generators-path
;;       (setq auto-code-command (concat generators-path auto-code-command)))
;;     (when (file-exists-p auto-code-command)
;;       (setq auto-code-command (concat auto-code-command " " data " " template))
;;       (when indent-str
;;         (setq auto-code-command (concat auto-code-command " '" indent-str "'")))
;;       (insert (shell-command-to-string auto-code-command)))))

(defun cg-generate-auto-code (generator data template indentation-str)
  (let ((generators-path (rh-project-get-generators-path))
        (auto-code-command generator)
        lines last-line)
    (when generators-path
      (setq auto-code-command (concat generators-path auto-code-command)))
    (when (file-exists-p auto-code-command)
      (setq auto-code-command (concat auto-code-command " " data " " template))
      (setq lines (split-string
                   (shell-command-to-string auto-code-command) "\n"))
      (setq last-line (car (last lines)))
      (setq lines (nbutlast lines))
      (seq-each
       (lambda (line)
         (if (string-empty-p line)
             (insert "\n")
           (insert (concat indentation-str line "\n"))))
       lines)
      (unless (string-empty-p last-line)
        (insert (concat indentation-str line "\n"))))))

(defun cg-generate-auto-code-group ()
  (interactive)
  (let* ((current-line (thing-at-point 'line t))
         (open-token (regexp-quote cg-auto-code-group-open-token))
         (close-token (regexp-quote cg-auto-code-group-close-token))
         (param-token (regexp-quote cg-auto-code-group-param-token))
         (regex-begin
          "^\\([[:blank:]]*\\)[^[:blank:]\r\n]+[[:blank:]]*")
         (regex-params-end
          (concat
           "[[:blank:]]*\\([^[:blank:]\r\n]+\\)[[:blank:]]+"
           "\\([^[:blank:]\r\n]+\\)[[:blank:]]+\\([^[:blank:]\r\n]+\\)"))
         (regex-end "$")
         (close-regex
          (concat regex-begin close-token))
         (open-regex
          (concat regex-begin open-token))
         (open-single-line-regex
          (concat regex-begin open-token regex-params-end))
         (open-multi-line-regex
          (concat regex-begin open-token regex-end))
         (param-single-line-regex
          (concat regex-begin param-token regex-params-end))
         generator data template open-indent-str close-indent-str indent-str)
    (save-match-data
      (cond ((string-match close-regex current-line)
             (setq close-indent-str (match-string 1 current-line))
             (cg-search-backward-group-balanced-head)
             (setq current-line (thing-at-point 'line t)))
            ((string-match open-regex current-line)
             (save-excursion
               (cg-search-forward-group-balanced-tail)
               (let ((line (thing-at-point 'line t)))
                 (string-match close-regex line)
                 (setq close-indent-str (match-string 1 line))))))
      (cond ((string-match open-single-line-regex current-line)
             (setq open-indent-str (match-string 1 current-line))
             (unless (string= open-indent-str close-indent-str)
               (error "auto-code open and close indentations do not match." ))
             (setq generator (match-string 2 current-line))
             (setq data (match-string 3 current-line))
             (setq template (match-string 4 current-line))
             ;; The following condition should be removed
             ;; once all templates are moved to automatic indentation
             (when (string= (substring template -2) ".i")
               (setq indent-str (match-string 1 current-line)))
             (cg-delete-code-group)
             (cg-generate-auto-code generator data template indent-str))
            ((string-match open-multi-line-regex current-line)
             (setq open-indent-str (match-string 1 current-line))
             (unless (string= open-indent-str close-indent-str)
               (error "auto-code open and close indentations do not match." ))
             (save-excursion
               (move-beginning-of-line 0)
               (setq current-line (thing-at-point 'line t)))
             (if (not (string-match param-single-line-regex current-line))
                 (error "Invalid auto-code open multi-line block." )
               (setq generator (match-string 2 current-line))
               (setq data (match-string 3 current-line))
               (setq template (match-string 4 current-line))
               ;; The following condition should be removed
               ;; once all templates are moved to automatic indentation
               (unless (string= open-indent-str (match-string 1 current-line))
                 (error (concat "auto-code open multi line block "
                                "is not uniformly indented.")))
               (when (string= (substring template -2) ".i")
                 (setq indent-str (match-string 1 current-line)))
               (cg-delete-code-group)
               (cg-generate-auto-code generator data template indent-str)))))))

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

;; /b/} code-groups

;; /b/{ goto-window

(defvar g2w-reuse-visible-default t)

(defvar g2w-side-window-height 15)

(defvar g2w-fallback-display-buffer-func
  ;; 'display-buffer-reuse-window)
  'display-buffer-pop-up-window)

(defvar g2w-display-buffer-reuse-window-commands
  '())

;; (defvar g2w-display-buffer-same-window-commands
;;   '())

;; (defun g2w-reuse-same-window-p (buffer-nm actions)
;;   (with-current-buffer buffer-nm
;;     (and (not (string= buffer-nm "*RTags*"))
;;          (not (boundp 'g2w-destination-window))
;;          (memq this-command
;;                g2w-display-buffer-same-window-commands))))

(defun g2w-reuse-command-window-p (buffer-nm actions)
  (and (boundp 'g2w-destination-window)
       (memq this-command
             g2w-display-buffer-reuse-window-commands)))

(defun g2w-display-buffer-reuse-command-window (buffer alist)
  (if (and (boundp 'g2w-destination-window)
           (memq g2w-destination-window (window-list)))
      (let ((win g2w-destination-window))
        (when (and (bound-and-true-p g2w-reuse-visible)
                   (not (eq (window-buffer win) buffer)))
          (let ((win-reuse
                 (get-buffer-window buffer (selected-frame))))
            (when win-reuse (setq win win-reuse))))
        (window--display-buffer buffer win
                                'reuse alist))
    (funcall g2w-fallback-display-buffer-func buffer alist)))

(add-to-list
 'display-buffer-alist
 '(g2w-reuse-command-window-p
   g2w-display-buffer-reuse-command-window))

;; (add-to-list
;;  'display-buffer-alist
;;  '(g2w-reuse-same-window-p
;;    display-buffer-same-window))

;; (add-to-list
;;  'display-buffer-alist
;;  '((lambda (buffer actions)
;;      (memq this-command g2w-display-buffer-reuse-window-commands))
;;    (lambda (buffer alist)
;;      (if (and (boundp 'g2w-destination-window)
;;               (memq g2w-destination-window (window-list)))
;;          (let ((win g2w-destination-window))
;;            (when (and (bound-and-true-p g2w-reuse-visible)
;;                       (not (eq (window-buffer win) buffer)))
;;              (let ((win-reuse
;;                     (get-buffer-window buffer (selected-frame))))
;;                (when win-reuse (setq win win-reuse))))
;;            (window--display-buffer buffer win
;;                                    'reuse alist
;;                                    display-buffer-mark-dedicated))
;;        (funcall g2w-fallback-display-buffer-func buffer alist)))
;;    (inhibit-same-window . nil)))

(cl-defmacro g2w-display (display-buffer-func
                          &optional (kill-on-quit nil))
  `(lambda (buf alist)
     (let ((win (funcall ,display-buffer-func buf alist)))
       (when win
         (with-current-buffer buf
           (set (make-local-variable 'g2w-window-side)
                (window-parameter win 'window-side))
           (put 'g2w-window-side 'permanent-local t)
           (set (make-local-variable 'g2w-window-slot)
                (window-parameter win 'window-slot))
           (put 'g2w-window-slot 'permanent-local t)
           ;; (set (make-local-variable 'g2w-quit-restore-parameter)
           ;;      (window-parameter win 'quit-restore))
           ;; (put 'g2w-quit-restore-parameter 'permanent-local t)
           (set (make-local-variable 'g2w-kill-on-quit)
                ,kill-on-quit)
           (put 'g2w-kill-on-quit 'permanent-local t)))
       win)))

(defun g2w-buffer-destination-window-init (buffer-or-name window reuse-visible)
  (with-current-buffer buffer-or-name
    (setq-local g2w-destination-window window)
    (put 'g2w-destination-window 'permanent-local t)
    (setq-local g2w-reuse-visible reuse-visible)
    (put 'g2w-reuse-visible 'permanent-local t)))

(cl-defmacro g2w-condition
    (condition
     &optional
     (reuse-visible g2w-reuse-visible-default))
  `(lambda (buffer-nm actions)
     (when (if (stringp ,condition)
               (string-match-p ,condition buffer-nm)
             (funcall ,condition buffer-nm actions))
       (let ((current-window (frame-selected-window)))
         (g2w-buffer-destination-window-init buffer-nm current-window
                                             ,reuse-visible)
         ;; (with-current-buffer buffer-nm
         ;;   (setq-local g2w-destination-window current-window)
         ;;   (put 'g2w-destination-window 'permanent-local t)
         ;;   (setq-local g2w-reuse-visible ,reuse-visible)
         ;;   (put 'g2w-reuse-visible 'permanent-local t))
         t))))

;; (defun g2w-set-destination-window (choice)
;;   (interactive
;;    (if (local-variable-p 'g2w-destination-window)
;;      (let* (value
;;             (choices (mapcar (lambda (w)
;;                                (list (format "%s" w) w))
;;                              (window-list)))
;;             (completion-ignore-case  t))
;;        (setq value (list (completing-read
;;                           "destination-window: " choices nil t)))
;;        (cdr (assoc (car value) choices 'string=)))
;;      '(nil)))
;;   (if (local-variable-p 'g2w-destination-window)
;;       (progn
;;         (setq g2w-destination-window choice)
;;         (select-window choice)
;;         (message "destination-window: %s" choice)
;;         choice)
;;     (progn
;;       (message "current buffer has no associated `destination-window'")
;;       nil)))

(defun g2w-set-destination-window ()
  (interactive)
  (with-selected-window (frame-selected-window)
    (let ((buffer (current-buffer)))
      (when (ace-select-window)
        (with-current-buffer buffer
          (setq-local g2w-destination-window (frame-selected-window)))))))

(defun g2w-select-destination-window ()
  (interactive)
  (if (local-variable-p 'g2w-destination-window)
      (if (member g2w-destination-window (window-list))
          (select-window g2w-destination-window)
        (message "`destination-window' window has been killed"))
    (progn
      (message "current buffer has no associated `destination-window'")
      nil)))

;; /b/} goto-window

;; -------------------------------------------------------------------
;;; Basic System Setup
;; -------------------------------------------------------------------

(use-package emacs
  :config
  (setq split-height-threshold nil)
  (setq split-width-threshold 170)

  (setq
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

  ;; (add-to-list 'display-buffer-alist
  ;;              `("*Warnings*"
  ;;                ,(g2w-display #'display-buffer-in-side-window t)
  ;;                (side . bottom)
  ;;                (slot . 0)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 15)))

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
        (add-to-list 'default-frame-alist
                     '(font . "Hack-10.5")))

       ((= width-pixels 2560)
        (add-to-list 'default-frame-alist
                     '(font . "Hack-9")))))

    ;; see http://emacs.1067599.n8.nabble.com/bug-13011-24-2-Text-flickering-moving-cursor-with-box-around-text-enabled-td270885.html
    ;;     https://emacs.stackexchange.com/questions/47002/adding-box-around-text-without-changing-the-text-width
    ;; (set-face-attribute 'region nil
    ;;                     :box '(:line-width (-1 . -1)
    ;;                            :color "gtk_selection_bg_color"
    ;;                            :style nil))

    (unwind-protect
        (condition-case ex
            (progn
              (set-face-attribute
               'region nil
               :box '(:line-width (-1 . -1)
                      ;; :color "gtk_selection_bg_color"
                      :color "#ea5e30"
                      :style nil)
               :background "#ea5e30"))
          ('error
           (set-face-attribute
            'region nil
            :box '(:line-width -1
                   ;; :color "gtk_selection_bg_color"
                   :color "#ea5e30"
                   :style nil)
            :background "#ea5e30"))))

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

  ;; (setq next-error-recenter '(4))

  (add-hook
   'next-error-hook
   (lambda ()
     (rh-recenter-sensibly)))

  ;; TODO: Fix trailing-whitespace face in color-theme-sanityinc-tomorrow-blue
  ;; (color-theme-sanityinc-tomorrow-blue)
  ;; (load-theme 'sanityinc-tomorrow-blue t)
  ;; (disable-theme 'sanityinc-tomorrow-blue)
  ;; (enable-theme 'sanityinc-tomorrow-blue)

  ;; (customize-set-variable 'find-file-visit-truename t)
  (customize-set-value 'find-file-visit-truename t)

  (column-number-mode 1)
  (size-indication-mode -1)

  (ivy-mode 1)
  (counsel-mode 1)

  (abbrev-mode -1)
  (yas-global-mode 1)

  ;; Disable annoying key binding for (suspend-frame) function and quit
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-x C-c"))

  ;; Prevent translation from <kp-bebin> to <begin>
  (global-set-key (kbd "<kp-begin>") (lambda () (interactive)))

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

  ;; Sort bindings to appropriate packages
  :bind (;; Exit Emacs!
         ("C-x r q" . save-buffers-kill-terminal)
         ("C-x f" . find-file-at-point)
         ;; Editor
         ("C-<insert>" . kill-ring-save)
         ("C-<kp-insert>" . kill-ring-save)
         ("S-<insert>" . yank)
         ("S-<kp-insert>" . yank)
         ("M-<insert>" . yank-pop)
         ;; ("M-Y" . yank-pop-forwards)
         ("M-S-<insert>" . yank-pop-forwards)
         ("M-S-<kp-insert>" . yank-pop-forwards)
         ("C-<delete>" . kill-word)
         ("C-<kp-delete>" . kill-word)
         ("S-<delete>" . kill-region)
         ("S-<kp-delete>" . kill-region)
         ("C-<home>" . beginning-of-buffer)
         ("C-<kp-home>" . beginning-of-buffer)
         ("C-<end>" . end-of-buffer)
         ("C-<kp-end>" . end-of-buffer)
         ("C-v" . yank)
         ("M-v" . yank-pop)
         ("C-z" . undo)
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
         ("S-M-s-<kp-begin>" . rh-balance-windows-vertically)
         ("M-s-'" . rh-balance-windows-horizontally)
         ("M-s-\"" . rh-balance-windows-vertically)
         ;; Move point between windows
         ;; see http://stackoverflow.com/questions/91071/emacs-switch-to-previous-window
         ("C-x <up>" . windmove-up)
         ("C-x <kp-up>" . windmove-up)
         ("C-x <down>" . windmove-down)
         ("C-x <kp-down>" . windmove-down)
         ("C-x <right>" . windmove-right)
         ("C-x <kp-right>" . windmove-right)
         ("C-x <left>" . windmove-left)
         ("C-x <kp-left>" . windmove-left))
  :after (yasnippet ivy counsel)
  :demand t)

(setq load-prefer-newer t)
(add-to-list 'load-path rh-user-lisp-directory-path)
(load rh-user-site-start-file-path nil t t)

(dolist (file-path rh-site-start-file-paths)
  (load file-path nil t t))

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package saveplace
  :init
  (setq save-place-file vr-saved-places-file-path)

  ;; (defadvice save-place-find-file-hook
  ;;     (around rh-save-place-find-file-hook activate)
  ;;   (when ad-do-it
  ;;     (run-with-idle-timer 0 nil #'recenter)))

  ;; TODO: update to the new advice style
  (defadvice save-place-find-file-hook
      (around rh-save-place-find-file-hook activate)
    (when ad-do-it
      (run-with-timer
       0 nil
       (lambda (buf)
         (dolist (win (get-buffer-window-list buf nil t))
           (with-selected-window win (recenter))))
       (current-buffer))))

  ;; TODO: remove old emacs support
  (if (version< emacs-version "25.0")
      (progn
        (require 'saveplace)
        (setq-default save-place t))
    (save-place-mode 1))

  :config
  (remove-hook 'dired-initial-position-hook #'save-place-dired-hook)

  :demand t)

(setq default-input-method "russian-computer")

;; No ceremonies
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; emacs is not good in GUI. However, menu can be good for learning
;; new commands.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; better frame titles
(setq frame-title-format
      (concat "%b - emacs@" system-name))

;; http://www.emacswiki.org/emacs/SaveHist
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-11/msg00213.html
(savehist-mode 1)

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
(setq yank-excluded-properties
      (append '(font face font-lock-face) yank-excluded-properties))

(defface rh-hl-line-inactive
  '((t nil))
  "Inactive variant of `hl-line'."
  :group 'hl-line)

(defun rh-hl-line-update-face (window)
  "Update the `hl-line' face in WINDOW to indicate whether
the window is selected."
  (with-current-buffer (window-buffer window)
    (when hl-line-mode
      (if (eq (current-buffer) (window-buffer (selected-window)))
          (face-remap-reset-base 'hl-line)
        (face-remap-set-base
         'hl-line (face-all-attributes 'rh-hl-line-inactive))))))

;; see https://emacs.stackexchange.com/questions/14638/change-highlight-color-when-window-isnt-in-focus/14658#14658
;; This hook is required as hl-line-sticky-flag alone is not sufficient - it
;; does not always disable line highlight in e.g. windows displaying different
;; dired buffers (although works fine when different windows are showing the
;; same dired buffer).
;; TODO: Such behaviour is probably a bug in hl-line mode - report it to
;;       upstream...
(add-hook
 'buffer-list-update-hook
 (lambda ()
   (run-with-timer
    0 nil
    (lambda ()
      (walk-windows #'rh-hl-line-update-face nil t)))))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  :ensure t)

(use-package total-lines
  :config
  (global-total-lines-mode 1)
  :ensure t)

(use-package beacon
  :config
  (setq beacon-lighter
    (cond
     ((char-displayable-p ?Λ) " Λ")
     (t " (*)")))

  (add-to-list 'rm-blacklist " (*)")
  (add-to-list 'rm-blacklist " Λ")

  (beacon-mode 1)

  (setq beacon-dont-blink-commands
        '(mouse-set-point
          mouse-drag-region
          ;; pop-tag-mark
          ;; xref-pop-marker-stack
          compile-goto-error
          compilation-display-error
          ivy-done))

  (add-to-list 'beacon-dont-blink-major-modes 'dired-mode t)
  (add-to-list 'beacon-dont-blink-major-modes 'paradox-menu-mode t)
  (add-to-list 'beacon-dont-blink-major-modes 'bs-mode t)

  (setq beacon-blink-delay 0.2)
  ;; (setq beacon-color "gtk_selection_bg_color")
  (setq beacon-color 0.3)
  (setq beacon-blink-when-window-scrolls nil)
  ;; (setq beacon-blink-when-focused t)
  ;; (setq beacon-push-mark 1)

  :after rich-minority
  :ensure t)

(defun rh-rm-minor-modes ()
  (interactive)
  (message
   (substring-no-properties
    (mapconcat
     (lambda (pair)
       (format "%s (%S)" (string-trim-left (car pair)) (cdr pair)))
     (delq nil (mapcar #'rm-format-mode-line-entry minor-mode-alist))
     "\n"))))

(use-package rich-minority
  :demand t
  :ensure t)

(use-package diminish
  :demand t
  :ensure t)

(use-package delight
  :demand t
  :ensure t)

(use-package smart-mode-line
  :config
  (require 'config-smart-mode-line)

  (define-key-after
    (lookup-key mode-line-column-line-number-mode-map
                [mode-line down-mouse-1])
    [total-lines-mode]
    '(menu-item "Display Total Number of Lines" total-lines-mode
		:help
                "Toggle displaying a total number of lines in the mode-line"
		:button (:toggle . total-lines-mode))
    'line-number-mode)

  (defun sml/is-%p-p (x)
    "Non-nil if X matches \"%p\" in a very subjective sense."
    (or (and (listp x)
             (or (memq 'mode-line-percent-position x)
                 (cl-remove-if-not
                  (lambda (y) (string-match-p ".*%p.*" y))
                  (cl-remove-if-not #'stringp x))))
        (and (stringp x)
             (string-match ".*%p.*" x))))

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
              face sml/col-number
              help-echo ,(concat "Total lines mode\n"
                                 "mouse-1: Display Line "
                                 "and Column Mode Menu")
              mouse-face mode-line-highlight
              local-map ,mode-line-column-line-number-mode-map))

            (total-lines-mode
             ,(propertize " " 'face 'sml/numbers-separator))

            (line-number-mode
             ,(propertize "[" 'face 'sml/numbers-separator))

            (line-number-mode
             (:propertize
              (:eval (let ((width
                            (if total-lines-mode
                                (max (length (number-to-string total-lines))
                                     rh-linum-min-digits)
                              rh-linum-min-digits)))
                       (concat "%" (number-to-string width) "l")))
              face sml/line-number
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
             ,(propertize sml/col-number-format
                          'face 'sml/col-number
                          'help-echo (concat "Column number mode\n"
                                             "nmouse-1: Display Line "
                                             "and Column Mode Menu")
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map))

            (line-number-mode
             ,(propertize "]" 'face 'sml/numbers-separator))

            ;; (,(or line-number-mode column-number-mode)
            ;;  (:propertize " " face 'sml/numbers-separator))

            ;; (t
            ;;  (:propertize
            ;;   (:eval (if (or line-number-mode column-number-mode) " " ""))
            ;;   face 'sml/numbers-separator))

            (:propertize
             (:eval (if (or line-number-mode column-number-mode) " " ""))
             face sml/numbers-separator)

            (size-indication-mode
             ,(propertize sml/size-indication-format
                          'face 'sml/col-number
                          'help-echo (concat "Size indication mode\n"
                                             "nmouse-1: Display Line "
                                             "and Column Mode Menu")
                          'mouse-face 'mode-line-highlight
                          'local-map mode-line-column-line-number-mode-map))

            (size-indication-mode
             ,(propertize " " 'face 'sml/numbers-separator))

            (rh-sml/position-percentage-format
             (-3 (:propertize (:eval rh-sml/position-percentage-format)
                              local-map ,mode-line-column-line-number-mode-map
                              mouse-face mode-line-highlight
                              face sml/position-percentage
                              help-echo ,(concat "Buffer Relative Position\n"
                                                 "mouse-1: Display Line and "
                                                 "Column Mode Menu"))))

            (rh-sml/position-percentage-format
             ,(propertize " " 'face 'sml/numbers-separator)))))

  ;; (setq sml/theme 'light)
  (setq sml/theme 'automatic)
  ;; (setq sml/theme 'respectful)
  ;; (setq sml/theme nil)
  (setq sml/name-width (cons 0 44))
  (setq sml/show-eol t)
  (setq sml/col-number-format "%3c")
  (setq sml/size-indication-format "%I")
  (setq sml/shorten-mode-string "")
  (setq sml/shorten-modes nil)

  ;; (setq-local sml/name-width 10)

  ;; (setq rh-sml/position-percentage-format sml/position-percentage-format)
  (setq rh-sml/position-percentage-format nil)
  (setq sml/position-percentage-format nil)

  (rh-sml/setup)
  ;; (let ((buffer-id (default-value 'mode-line-buffer-identification)))
  ;;   (setq-default mode-line-buffer-identification buffer-id))

  :after (linum total-lines)
  :demand t
  ;; :disabled t
  :ensure t)

(use-package rainbow-mode
  :demand t
  :ensure t)

(use-package facemenu
  :config
  (add-to-list
   'display-buffer-alist
   '("*Colors*"
     (display-buffer-same-window)))
  :defer t)

(use-package faces
  :config
  (add-to-list
   'display-buffer-alist
   '("*Faces*"
     (display-buffer-same-window))))

(use-package help-mode
  :config
  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  '("*Help*"
  ;;    (display-buffer-reuse-window
  ;;     rh-display-buffer-reuse-right
  ;;     rh-display-buffer-reuse-left
  ;;     rh-display-buffer-reuse-down
  ;;     rh-display-buffer-reuse-up
  ;;     display-buffer-pop-up-window)))

  (setq help-window-select t)

  :defer t)

(use-package apropos
  :defer t)

(use-package grep
  :config
  (add-to-list
   'display-buffer-alist
   `(,(g2w-condition "*grep*")
     (display-buffer-reuse-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      ;; display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (add-to-list
   'g2w-display-buffer-reuse-window-commands
   'compile-goto-error)

  (add-to-list
   'g2w-display-buffer-reuse-window-commands
   'compilation-display-error)

  (add-hook
   'grep-mode-hook
   (lambda ()
     (setq truncate-lines t)))

  :defer t)

(use-package wgrep
  :defer t
  :ensure t)

(use-package replace
  :init
  (add-to-list
   'display-buffer-alist
   `(,(g2w-condition "*Occur*")
     (display-buffer-reuse-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      ;; display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'occur-mode-goto-occurrence)

  :config
  ;; C++-mode ocasionally fails when occur-excluded-properties is nil.
  ;; Need to investigate or wait until modern font lock is more reliable.
  (setq occur-excluded-properties t)

  ;; :bind (:map occur-mode-map
  ;;        ("q" . quit-window))
  :defer t)

(use-package findr
  :demand t
  :ensure t)

(use-package iedit
  :config
  (custom-set-faces
   '(iedit-occurrence
     ((((background light)) (:background "deep sky blue"))))
   '(iedit-read-only-occurrence
     ((((background light)) (:background "pale turquoise")))))

  :demand t
  :ensure t)

(use-package xref
  :config
  (require 'config-xref)

  (add-to-list
   'display-buffer-alist
   `(,(g2w-condition "*xref*")
     (display-buffer-below-selected)
     ;; (window-height . 0.3)
     (window-height . shrink-window-if-larger-than-buffer)
     (inhibit-same-window . t)))

  (add-to-list
   'g2w-display-buffer-reuse-window-commands
   'xref-goto-xref)

  (add-to-list
   'g2w-display-buffer-reuse-window-commands
   'xref-show-location-at-point)

  :bind (("M-[" . rh-xref-return)
         ("M-]" . rh-xref-undo-return))
  :demand t)

(use-package bind-key
  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(g2w-condition "*Personal Keybindings*")
  ;;                ,(g2w-display #'display-buffer-in-side-window t)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 15)))

  (add-to-list 'display-buffer-alist
               `("*Personal Keybindings*"
                 ,(g2w-display #'display-buffer-same-window t)))

  :demand t
  :ensure t)

(use-package autorevert
  :config
  (setq auto-revert-mode-text " ⭯")
  (setq auto-revert-tail-mode-text " ⭳")

  (add-to-list 'rm-blacklist " ⭯")

  :defer t)

(use-package avy
  :config
  ;; (defadvice avy-goto-subword-1 (around rh-avy-goto-subword-1 () activate)
  ;;   ad-do-it
  ;;   (font-lock-flush))
  (setq avy-all-windows 'all-frames)

  :bind (("M-f" . avy-goto-char)
         ("M-d" . avy-goto-line))
  :demand t
  :ensure t)

(use-package ace-window
  :config
  (setq aw-dispatch-when-more-than 1)

  :bind (("C-c a a" . ace-window)
         ("C-c a o" . ace-select-window)
         ("C-c a s" . ace-swap-window)
         ("C-c a d" . ace-delete-window))
  :demand t
  :ensure t)

;; (use-package tramp
;;   :config
;;   (setq tramp-mode nil))

;; (use-package arc-mode)

;; -------------------------------------------------------------------
;;; Text Editor
;; -------------------------------------------------------------------

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

;; (defun rh-color-theme-sanityinc-tomorrow-after (mode)
;;   (when (eq mode 'blue)
;;     (face-spec-reset-face 'term-color-blue)
;;     (face-spec-set
;;      'term-color-blue
;;      '((((class color) (background light)) .
;;         (:background "#88AACF" :foreground "#3465A4"))
;;        (((class color) (background dark)) .
;;         (:background "#88AACF" :foreground "#3465A4"))))

;;     (face-spec-reset-face 'vterm-color-blue)
;;     (face-spec-set
;;      'vterm-color-blue
;;      '((((class color) (background light)) .
;;         (:inherit term-color-blue))
;;        (((class color) (background dark)) .
;;         (:inherit term-color-blue))))))

;; (advice-add 'color-theme-sanityinc-tomorrow :after
;;             #'rh-color-theme-sanityinc-tomorrow-after)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package isearch
  :config
  (setq isearch-allow-scroll t)

  ;; Recentring screen on isearch
  ;; see https://emacs.stackexchange.com/a/10432
  ;; (defadvice isearch-update (before my-isearch-update activate)
  ;;   (sit-for 0)
  ;;   (if (and
  ;;        ;; not the scrolling command
  ;;        (not (eq this-command 'isearch-other-control-char))
  ;;        ;; not the empty string
  ;;        (> (length isearch-string) 0)
  ;;        ;; not the first key (to lazy highlight all matches w/o recenter)
  ;;        (> (length isearch-cmds) 2)
  ;;        ;; the point in within the given window boundaries
  ;;        (let ((line (count-screen-lines (point) (window-start))))
  ;;          (or (> line (* (/ (window-height) 4) 3))
  ;;              (< line (* (/ (window-height) 9) 1)))))
  ;;       (let ((recenter-position 0.3))
  ;;         (recenter '(4)))))
  :demand t)

(use-package electric
  :config
  (defun rh-electric-indent-post-self-insert-function ()
    (when (and electric-indent-mode
               (eq last-command 'newline))
      (save-excursion
        (move-beginning-of-line 0)
        (when (looking-at "^[[:blank:]]+$")
          (delete-region (point) (line-end-position))))))

  (add-hook 'post-self-insert-hook
            #'rh-electric-indent-post-self-insert-function))

;; Override text selection on typing
;; (i.e. non-persistent selection)
(delete-selection-mode t)

(defun rh-kill-ring-save-after-keep-mark (&rest _)
  (setq deactivate-mark nil))

(advice-add 'kill-ring-save :after #'rh-kill-ring-save-after-keep-mark)

;; see https://www.emacswiki.org/emacs/KillingAndYanking
(defun yank-pop-forwards (arg)
  (interactive "p")
  (yank-pop (- arg)))

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

;; TODO: Review  these keys for windows resizing
;; up/down keys "like in Adobe Reader"
(global-set-key (kbd "M-<down>") 'rh-scroll-up-one-line)
(global-set-key (kbd "M-<kp-down>") 'rh-scroll-up-one-line)
(global-set-key (kbd "M-<up>") 'rh-scroll-down-one-line)
(global-set-key (kbd "M-<kp-up>") 'rh-scroll-down-one-line)

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

;; /b/{ whitespace

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
    (space-mark 160 [164] [95])
    (space-mark 2208 [2212] [95])
    (space-mark 2336 [2340] [95])
    (space-mark 3616 [3620] [95])
    (space-mark 3872 [3876] [95])
    (newline-mark 10 [182 10] [36 10])  ; newlne, ¶
    ;; (tab-mark 9 [8594 9] [92 9])        ; tab, →
    ;; (tab-mark 9 [187 9] [92 9])         ; tab, »
    (tab-mark 9 [9654 9] [92 9])        ; tab, ▶
    ;; (tab-mark 9 [9655 9] [92 9])        ; tab, ▷
))

;; /b/} whitespace

;; /b/{ sr-speedbar

(use-package sr-speedbar
  :ensure t)

;; /b/} sr-speedbar

;; /b/{ picture

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

;; /b/} picture

;; /b/{ undo-tree

;; TODO: Adjust how mode-line looks in "*undo-tree*" buffer window
(use-package undo-tree
  :init
  ;; undo-tree-visualizer
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*undo-tree*"
  ;;                (display-buffer-in-side-window)
  ;;                (side . right)
  ;;                (slot . 0)
  ;;                (window-width . 30)))

  :config
  (add-to-list 'rm-blacklist " Undo-Tree")

  (define-key undo-tree-map (kbd "C-z") #'undo-tree-undo)
  (define-key undo-tree-map (kbd "C-S-z") #'undo-tree-redo)
  (define-key undo-tree-map (kbd "C-M-z") #'undo-tree-visualize)

  :defer t
  :ensure t)

;; /b/} undo-tree

;; /b/{ which-key

(use-package which-key
  :init
  (setq which-key-is-verbose t)
  (setq which-key-enable-extended-define-key t)

  ;; This conflicts with which-key "C-h" popup
  (global-unset-key (kbd "C-h C-h"))

  :config
  (add-to-list 'rm-blacklist " WK")

  (setq which-key-show-prefix 'mode-line)
  (setq which-key-max-description-length 30)
  ;; (setq which-key-show-transient-maps t)

  (setq which-key-sort-order 'which-key-description-order)

  (which-key-add-key-based-replacements
    "M-s"   "interactive-search"
    "M-s h" "highlight"
    "M-g"   "goto"
    "C-x 8" "unicode-keys")

  (run-with-idle-timer
   1 nil
   (lambda ()
     (which-key-mode 1)
     ;; (setq which-key-side-window-slot 1)
     (setq which-key-side-window-max-height 15)))

  ;; :bind (:map which-key-mode-map
  ;;         ("<f1>" . which-key-show-top-level))
  :bind (("<f1>" . which-key-show-top-level))
  :demand t
  :ensure t)

;; /b/} which-key

;; -------------------------------------------------------------------
;;; File Management
;; -------------------------------------------------------------------

;; /b/{ dired

(defun rh-dired-find-file ()
  (interactive)
  (let* ((filename (dired-get-file-for-visit))
         (filename-after
          (if (string= (file-name-nondirectory filename) "..")
              (dired-current-directory)
            (concat (file-name-as-directory filename) "..")))
         (find-file-respect-dired
          (lambda (fn)
            (if (= (length (get-buffer-window-list)) 1)
                (find-alternate-file fn)
              (bury-buffer)
              (find-file fn)))))
    (if (not (file-directory-p filename))
        (funcall find-file-respect-dired filename)
      (if (not (file-accessible-directory-p filename))
          (error (format "Directory '%s' is not accessible" filename))
        (funcall find-file-respect-dired filename))
      (dired-goto-file filename-after)
      (recenter))))

(defun rh-dired-change-to-file (filename)
  (interactive "FFind file: ")
  (find-alternate-file filename))

(defun rh-dired-ace-select-other-window ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (when (ace-select-window)
      (find-file file-name))))

(defun rh-get-buffer-mode-window-list (&optional buffer-or-name mode all-frames)
  (setq mode (or mode major-mode))
  (let ((window-list '()))
    (dolist (win (window-list-1 nil nil all-frames))
      (when (eq (with-selected-window win major-mode) mode)
        (push win window-list)))
    (nreverse window-list)))

(defun rh-dired-select-next-dired-window ()
  (interactive)
  (let ((window-list (rh-get-buffer-mode-window-list)))
    (if (> (length window-list) 1)
        (select-window (nth 1 window-list))
      (let ((directory (dired-current-directory))
            (pos (point)))
        (when (ace-select-window)
          (find-file directory)
          (goto-char pos))))))

(defun rh-dired-alt-ace-select-other-window ()
  (interactive)
  (with-selected-window (frame-selected-window)
    (let ((name (dired-get-file-for-visit)))
      (when (ace-select-window)
        (find-file name)))))

(defun rh-dired-change-to-parent-dir ()
  (interactive)
  (dired-goto-file (concat (dired-current-directory) ".."))
  (rh-dired-find-file))

(defun rh-dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)))

;; (defun rh-dired-guess-dir ()
;;   "Starts dired in buffer-file-name directory or in '~', if buffer has no
;; filename associated with it."
;;   (interactive)
;;   (progn
;;     (dired (let ((fnm (buffer-file-name)))
;;              (if fnm
;;                  (file-name-directory fnm)
;;                "~")))))

(use-package dired
  :config
  (require 'ace-window)

  ;; copy from one dired dir to the next dired dir shown in a split window
  (setq dired-dwim-target t)

  (add-hook
   'dired-mode-hook
   (lambda ()
     (hl-line-mode 1)
     (setq-local coding-system-for-read vr-dired-coding-system)
     (setq-local find-file-visit-truename nil)
     ;; (add-hook
     ;;  'window-selection-change-functions
     ;;  (lambda (win)
     ;;    (when (eq (with-selected-window win major-mode) 'dired-mode)
     ;;      (if (eq win (frame-selected-window))
     ;;          (hl-line-mode 1)
     ;;        (with-selected-window win
     ;;          (when (= (length (get-buffer-window-list)) 1)
     ;;            (hl-line-mode -1))))))
     ;;  nil t)
     ))

  :bind (:map dired-mode-map
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

(put 'dired-find-alternate-file 'disabled nil)

(if (equal system-type 'windows-nt)
    ;; In MS Windows systems
    (progn
      (setq dired-listing-switches "-alhgG")
      (setq vr-dired-coding-system 'cp1251))
  ;; In unix-like systems
  ;; Sort dirs and files in dired as in "C"
  (setenv "LC_COLLATE" "C")
  (setq dired-listing-switches
        ;; "--group-directories-first --time-style=long-iso -alhD"
        "--group-directories-first --time-style=long-iso -alh"
        )
  (setq vr-dired-coding-system nil))

;; (global-set-key (kbd "C-x d") 'rh-dired-guess-dir)
(global-set-key (kbd "C-x d") #'dired-jump)

;; (use-package dired-subtree
;;   :config
;;   (setq dired-subtree-use-backgrounds nil)
;;   ;; :bind (:map which-key-mode-map
;;   ;;         ("<f1>" . which-key-show-top-level))

;;   :after dired
;;   :demand t
;;   :ensure t)

;; (use-package dired-open
;;   :after dired
;;   :demand t
;;   :ensure t)

;; /b/} dired

(use-package recentf
  :config
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
  ;;            :size 50
  ;;            :help-echo ,(concat "Open " (cdr menu-element))
  ;;            :action recentf-open-files-action
  ;;            ;; Override the (problematic) follow-link property of the
  ;;            ;; `link' widget (bug#22434).
  ;;            :follow-link nil
  ;;            ,(cdr menu-element))))

  (setq recentf-save-file rh-recent-files-file-path)
  (setq recentf-kill-buffer-on-open t)
  (setq recentf-max-saved-items 1000)

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

  ;; If Emacs exits abruptly for some reason the recent file list will be lost.
  ;; Therefore call `recentf-save-list` periodically every 5 minutes.
  ;; (run-at-time nil (* 5 60) 'recentf-save-list)

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

;; /b/{ Internal ls (ls-lisp - used in Windows)

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
    (setq ad-return-value
          (concat
           (substring ad-return-value 0 1)
           (substring ad-return-value 13)))))

(ad-activate 'ls-lisp-format)

;; /b/} Internal ls (ls-lisp - used in Windows)

;; -------------------------------------------------------------------
;;; Completion, Regexps, Patterns and Highlighting
;; -------------------------------------------------------------------
;;; /b/{

;;; DWIM modes such as ivy, swiper, counsel
;;; /b/{

;; See the following links on some ivy hints
;; https://writequit.org/denver-emacs/presentations/2017-04-11-ivy.html
;; https://oremacs.com/2015/04/16/ivy-mode/

(defun rh-ivy--virtual-buffers ()
  "Adapted from `ido-add-virtual-buffers-to-list'."
  (require 'bookmark)
  (unless recentf-mode
    (recentf-mode 1))
  (bookmark-maybe-load-default-file)
  (let* ((vb-bkm (delete "   - no file -"
                         (delq nil (mapcar #'bookmark-get-filename
                                           bookmark-alist))))
         (vb-recent-dirs (delete-dups
                          (mapcar (lambda (file)
                                    (if (file-directory-p file)
                                        file
                                      (file-name-directory file)))
                                  recentf-list)))
         (vb-list (cond ((eq ivy-use-virtual-buffers 'recentf)
                         recentf-list)
                        ((eq ivy-use-virtual-buffers 'bookmarks)
                         vb-bkm)
                        (ivy-use-virtual-buffers
                         (append recentf-list vb-bkm vb-recent-dirs))
                        (t nil)))
         virtual-buffers)
    (dolist (head vb-list)
      (let* ((file-name (if (stringp head)
                            head
                          (cdr head)))
             (name (cond ((eq ivy-virtual-abbreviate 'name)
                          (file-name-nondirectory file-name))
                         ((eq ivy-virtual-abbreviate 'abbreviate)
                          (abbreviate-file-name file-name))
                         (t
                          (expand-file-name file-name)))))
        (when (equal name "")
          (setq name
                (if (consp head)
                    (car head)
                  (file-name-nondirectory (directory-file-name file-name)))))
        (unless (or (equal name "")
                    (get-file-buffer file-name)
                    (assoc name virtual-buffers))
          (push (cons (copy-sequence name) file-name) virtual-buffers))))
    (when virtual-buffers
      (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
                           'face 'ivy-virtual
                           (car comp)))
      (setq ivy--virtual-buffers (nreverse virtual-buffers))
      (mapcar #'car ivy--virtual-buffers))))

;; Adding recent dirs to virtual buffers.
;; TODO: Send patch/PR to ivy upstream.
(advice-add 'ivy--virtual-buffers :override
            #'rh-ivy--virtual-buffers)

(use-package ivy
  :config
  (add-to-list 'rm-blacklist " ivy")

  (add-to-list
   'display-buffer-alist
   `(,(g2w-condition
       (lambda (buffer-nm action)
         (eq (with-current-buffer buffer-nm major-mode)
             'ivy-occur-grep-mode))
       nil)
     (display-buffer-reuse-window
      rh-display-buffer-reuse-right
      rh-display-buffer-reuse-left
      ;; display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'ivy-occur-press-and-switch)

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'compile-goto-error)

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'compilation-display-error)

  ;; (defadvice ivy-occur-press-and-switch
  ;;     (after rh-ivy-occur-press-and-switch activate)
  ;;   (run-hooks 'next-error-hook))

  ;; (defadvice ivy-occur-press-and-switch
  ;;     (around rh-ivy-occur-press-and-switch activate)
  ;;   (setq compilation-current-error (point))
  ;;   (next-error 0))

  (defun rh-ivy-forward-char:override ()
    "Forward to `forward-char' ARG."
    (interactive)
    (unless (eolp)
      (call-interactively 'forward-char)))

  (advice-add 'ivy-forward-char :override
              #'rh-ivy-forward-char:override)

  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'abbreviate)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-height 8)

  (setq ivy-ignore-buffers
        '((lambda (buffer-nm)
            (let ((buffer (get-buffer buffer-nm)))
              (when buffer
                (rh-buffers-match
                 rh-buffers-not-file-group
                 buffer))))))

  (setq ivy-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map [remap switch-to-buffer-other-window]
            'ivy-switch-buffer-other-window)
          map))

  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-immediate-done)
         ("C-<return>" . ivy-alt-done)
         ("C-<kp-enter>" . ivy-alt-done)
         ("C-v" . nil)
         ("M-v" . nil)
         :map ivy-occur-grep-mode-map
         ("RET" . ivy-occur-press-and-switch)
         ("M-<return>" . ivy-occur-press)
         ("M-<kp-enter>" . ivy-occur-press))

  :demand t
  :ensure t)

(use-package ivy-hydra
  :demand t
  :ensure t)

(use-package swiper
  :config
  (defun rh-swiper-deduce (&optional initial-input do-not-shift-select)
    (interactive)
    (unless initial-input
      (setq initial-input (rh-deduce-default-text t)))
    (if do-not-shift-select
        (swiper initial-input)
      (minibuffer-with-setup-hook
          #'rh-shift-select-current-line
        (swiper initial-input))))

  :bind (("C-s" . 'swiper)
         ("C-S-s" . 'rh-swiper-deduce)
         ("M-s s" . 'isearch-forward)
         :map swiper-map
         ("C-g" . abort-recursive-edit)
         ("M-y" . yank-pop))

  :demand t
  :ensure t)

(use-package amx
  :demand t
  :ensure t)

(use-package counsel
  :config
  (add-to-list 'rm-blacklist " counsel")

  (define-key counsel-mode-map [remap yank-pop] nil)

  (defun rh-counsel-yank-pop (&optional arg)
    (interactive "P")
    (if (minibufferp)
        (if (eq last-command 'yank)
            (yank-pop arg)
          (yank arg))
      (counsel-yank-pop)))

  (defun rh-counsel-ag-deduce (&optional initial-input)
    (interactive)
    (let* ((extra-ag-args (if current-prefix-arg nil ""))
           (default-text (or initial-input (rh-deduce-default-text t)))
           (orig-current-prefix-arg current-prefix-arg)
           (current-prefix-arg (or orig-current-prefix-arg '(4))))
      (counsel-ag default-text nil extra-ag-args)))

  (defun rh-counsel-git-deduce (&optional initial-input)
    (interactive)
    (let* ((extra-ag-args (if current-prefix-arg nil ""))
           (default-text (or initial-input (rh-deduce-default-text t)))
           (current-prefix-arg '(4)))
      (counsel-git default-text)))

  (defun rh-counsel-file-jump ()
    (interactive)
    (let* ((current-prefix-arg t))
      (call-interactively #'counsel-file-jump)))

  (defun rh-counsel-ag ()
    (interactive)
    (rh-counsel-ag-deduce ""))

  :bind (:map counsel-mode-map
         ("C-c s" . rh-counsel-ag)
         ("C-c S" . rh-counsel-ag-deduce)
         ;; TODO: This key-bind should try rh-rpoject first
         ;;       then git then offer dir selector.
         ("C-c f" . counsel-git)
         ("C-c F" . rh-counsel-git-deduce)
         ("C-c C-f" . rh-counsel-file-jump)
         ("M-y" . rh-counsel-yank-pop))

  :demand t
  :ensure t)

(use-package lacarte
  :demand t)

(use-package ivy-rich
  :config
  (setq ivy-format-function #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev)

  ;; (defun rh-ivy-rich-switch-buffer-path (candidate)
  ;;   (let ((result (ivy-rich-switch-buffer-path candidate)))
  ;;     (if (string-empty-p result)
  ;;         (propertize
  ;;          (ivy-rich-switch-buffer-major-mode candidate)
  ;;          'face 'shadow)
  ;;       (concat result " " (propertize
  ;;                           (ivy-rich-switch-buffer-major-mode candidate)
  ;;                           'face 'shadow)))))

  ;; (plist-put
  ;;  ivy-rich-display-transformers-list
  ;;  'ivy-switch-buffer
  ;;  '(:columns
  ;;    ((ivy-rich-candidate (:width 30))
  ;;     (rh-ivy-rich-switch-buffer-path
  ;;      (:width
  ;;       (lambda (path)
  ;;         (ivy-rich-switch-buffer-shorten-path
  ;;          path (- (ivy-rich-minibuffer-width 1.0) 30))))))
  ;;    :predicate
  ;;    (lambda (cand)
  ;;      (get-buffer cand))))

  (plist-put
   ivy-rich-display-transformers-list
   'ivy-switch-buffer
   '(:columns
     ((ivy-rich-candidate (:width 30))
      (ivy-rich-switch-buffer-major-mode (:width 20 :face shadow))
      (ivy-rich-switch-buffer-path))
     :predicate
     (lambda (cand)
       (get-buffer cand))))

  (ivy-rich-mode 1)

  :demand t
  :ensure t)

;;; /b/}

(use-package hi-lock-mode
  :init
  (defvar hi-lock-map nil)

  :defer t)

(use-package pcre2el
  :demand t
  :ensure t)

(use-package visual-regexp
  :config
  ;; see https://stackoverflow.com/questions/15895313/let-emacs-move-the-cursor-off-screen

  (setq vr/match-separator-use-custom-face t)
  ;; (custom-set-variables '(vr/match-separator-string " -> "))

  ;; (define-key vr/minibuffer-keymap (kbd "C-j") #'newline)
  ;; (define-key vr/minibuffer-keymap (kbd "C-<return>") #'newline)

  ;; (global-set-key (kbd "C-c v") #'vr/replace)
  ;; (global-set-key (kbd "C-c q") #'vr/query-replace)

  :bind (("C-c v" . vr/replace)
         ("C-c q" . vr/query-replace)
         :map vr/minibuffer-keymap
         ("C-j" . newline)
         ("C-<return>" . newline))
  :defer t
  :ensure t)

;;; yasnippet
;;; /b/{

(use-package yasnippet-snippets
  :commands yasnippet-snippets-initialize
  :defer t
  :ensure t)

(use-package yasnippet
  :delight (yas-minor-mode " ⵙ")
  :config
  (add-to-list 'rm-blacklist " ⵙ")

  (yasnippet-snippets-initialize)

  :bind (:map yas-minor-mode-map
         ("<tab>" . nil)
         ("TAB" . nil)
         ("C-`" . yas-expand)
         ("C-~" . yas-prev-field))
  :demand t
  :ensure t)

;;; /b/}

(use-package pos-tip
  :config
  ;; (defvar pos-tip-foreground-color "#839496"
  ;;   "Default foreground color of pos-tip's tooltip.")

  ;; (defvar pos-tip-background-color "#073642"
  ;;   "Default background color of pos-tip's tooltip.")

  :ensure t)

(use-package popup
  :config
  (defvar rh-popup-direction 'default
    "Possible values are:
 'default' lets `popup-calculate-direction' function to determine direction;
 'company' selects direction opposite to company tooltip overlay if such
    overlay exists or uses `popup-calculate-direction' if company overlay does
    not exist or company mode is not enabled;
  1 is above selected row
  -1 is below selected row.")

  (defadvice popup-calculate-direction
      (around rh-popup-calculate-direction (height row) activate)
    (cl-case rh-popup-direction
      (1 (setq ad-return-value 1))
      (-1 (setq ad-return-value -1))
      ('company
       (if (and (bound-and-true-p company-mode)
                (bound-and-true-p company-pseudo-tooltip-overlay))
           (setq ad-return-value
                 (if (< (company--pseudo-tooltip-height) 0) 1 -1))
         ad-do-it))
      ('default ad-do-it)))

  :defer t
  :ensure t)

;;; auto-complete
;;; /b/{

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

(use-package auto-complete
  :config
  (add-to-list 'rm-blacklist " AC")

  (setq ac-modes (delq 'js2-mode ac-modes))
  (setq ac-modes (delq 'js-mode ac-modes))
  (setq ac-modes (delq 'javascript-mode ac-modes))
  (setq ac-modes (delq 'emacs-lisp-mode ac-modes))
  (setq ac-modes (delq 'lisp-interaction-mode ac-modes))
  (setq ac-modes (delq 'cc-mode ac-modes))
  (setq ac-modes (delq 'c++-mode ac-modes))
  (setq ac-modes (delq 'c-mode ac-modes))
  (setq ac-modes (delq 'js-jsx-mode ac-modes))
  (setq ac-modes (delq 'js2-jsx-mode ac-modes))
  (setq ac-modes (delq 'python-mode ac-modes))
  (setq ac-modes (delq 'scss-mode ac-modes))
  (setq ac-modes (delq 'web-mode ac-modes))

  (ac-config-default)

  (custom-set-faces
   '(ac-completion-face
     ((t (:background "light sky blue"
          :foreground "systemmenutext"
          :underline t))))
   '(ac-selection-face
     ((t (:background "light sky blue" :foreground "systemmenutext"))))
   '(completion-dynamic-common-substring-face
     ((((class color) (background light))
       (:background "light steel blue" :foreground "systemmenutext"))))
   '(completion-dynamic-prefix-alterations-face
     ((((class color) (background light))
       (:background "cyan" :foreground "systemmenutext"))))
   '(completion-highlight-face
     ((((class color) (background light))
       (:background "light sky blue" :underline t)))))

  (setq ac-fuzzy-enable t)
  (setq ac-use-quick-help nil)
  (setq ac-auto-show-menu nil)
  (setq ac-use-menu-map t)
  (setq ac-quick-help-prefer-pos-tip t)

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

  ;; (define-key ac-completing-map (kbd "M-h") (lambda () (interactive) (ac-quick-help t)))
  (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)
  (define-key ac-completing-map (kbd "M-H") 'ac-persist-help)

  (define-key ac-mode-map (kbd "M-h") 'ac-last-quick-help)
  (define-key ac-mode-map (kbd "M-H") 'ac-last-persist-help)

  :after (fuzzy pos-tip)
  :ensure t)

;;; /b/}

(use-package company
  :init
  (defvar rh-company-display-permanent-doc-buffer nil)

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

  (setq company-lighter-base "CA")

  (add-to-list 'rm-blacklist " CA")

  (setq company-backends
        '((company-keywords company-dabbrev-code)
          company-files (company-dabbrev company-ispell)))

  ;; TODO: write to https://github.com/company-mode/company-mode/issues/123
  (defun rh-company-pseudo-tooltip-on-explicit-action (command)
    (cl-case command
      (hide (company-pseudo-tooltip-frontend command)
            (company-preview-frontend command))
      (t (if (company-explicit-action-p)
             (company-pseudo-tooltip-frontend command)
           (company-preview-frontend command)))))

  (defmacro rh-company-tooltip-key (default-key cmd)
    `(lambda ()
       (interactive)
       (if (company-tooltip-visible-p)
           (funcall ,cmd)
         (let ((default-cmd (or (local-key-binding ,default-key)
                                (global-key-binding ,default-key))))
           (when (fboundp default-cmd)
             (funcall default-cmd)
             (company-abort))))))

  (defmacro rh-company-tooltip-cmd (default-cmd cmd)
    `(lambda ()
       (interactive)
       (if (company-tooltip-visible-p)
           (funcall ,cmd)
         (funcall ,default-cmd))))

  (setq company-tooltip-align-annotations t)
  (setq company-echo-truncate-lines nil)
  (company-echo-show)
  (setq company-minimum-prefix-length 1)
  (setq company-frontends
        '(rh-company-pseudo-tooltip-on-explicit-action
          company-preview-frontend
          company-echo-metadata-frontend))
  (setq company-require-match nil)

  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 80)
  (setq company-tooltip-minimum-width 35)
  (setq company-tooltip-offset-display 'lines)

  ;; Use "M-h" for company-show-doc-buffer
  (define-key company-active-map (kbd "<f1>") nil)
  (define-key company-active-map (kbd "C-h") nil)
  ;; Use company-filter-candidates by default, i.e. C-s
  ;; In search mode use C-o to switch between filtered and unfiltered
  (define-key company-active-map (kbd "C-M-s") nil)
  ;; Use some other tools for code navigation
  (define-key company-active-map (kbd "C-w") nil)

  (define-key company-active-map (kbd "<escape>") #'company-abort)
  (define-key company-active-map (kbd "<delete>") #'company-abort)
  (define-key company-active-map (kbd "<kp-delete>") #'company-abort)

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
  (define-key company-active-map (kbd "<kp-enter>")
    (rh-company-tooltip-key (kbd "RET") #'company-complete-selection))

  (define-key company-active-map (kbd "C-s")
    (rh-company-tooltip-key (kbd "C-s") #'company-filter-candidates))

  (define-key company-active-map (kbd "M-h")
    (lambda ()
      (interactive)
      (when (fboundp rh-company-display-permanent-doc-buffer)
        (funcall rh-company-display-permanent-doc-buffer))
      (company-show-doc-buffer)))

  (define-key company-active-map (kbd "M-l") #'company-show-location)

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

  (setq company-lighter
        '(" "
          (company-candidates
           (:eval
            (if (consp company-backend)
                (company--group-lighter (nth company-selection
                                             company-candidates)
                                        company-lighter-base)
              (cond
               ((eq company-backend 'company-tern) "CA-ρ")
               ((eq company-backend 'company-tide) "CA-τ")
               (t (symbol-name company-backend)))))
           company-lighter-base)))

  ;; fci-mode interaction temporary patch
  ;; see https://github.com/company-mode/company-mode/issues/180
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  :ensure t)

(defun rh-complete-dwim ()
  (interactive)
  (cond
   ((cg-looking-at-auto-code-group-head-or-tail)
    (let ((lsp-mode-enabled (bound-and-true-p lsp-mode)))
      (unwind-protect
          (progn
            (when lsp-mode-enabled (lsp-disconnect))
            (cg-generate-auto-code-group))
        (when lsp-mode-enabled (lsp)))))
   ((bound-and-true-p auto-complete-mode)
    (auto-complete))
   ((bound-and-true-p company-mode)
    (company-complete))
   (t
    (message "No completion candidates."))))

(global-set-key (kbd "C-<tab>") #'rh-complete-dwim)
(global-set-key [remap auto-complete] #'rh-complete-dwim)
(global-set-key [remap company-complete] #'rh-complete-dwim)

;;; /b/; flymake
;;; /b/{

(use-package flymake
  :config
  (setq flymake-no-changes-timeout nil)
  :defer t)

(defun rh-flymake--mode-line-format (result)
  (setf (nth 1 (car result)) " φ")
  result)

(advice-add 'flymake--mode-line-format :filter-return
            #'rh-flymake--mode-line-format)

;;; /b/}

;;; /b/; flycheck
;;; /b/{

(use-package flycheck
  ;; :custom
  ;; (flycheck-mode-line-prefix "Φ")
  ;; (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (flycheck-indication-mode nil)

  ;; :custom-face
  ;; (flycheck-warning ((t (:underline (:color "orange" :style wave)))))
  ;; (flycheck-warning ((t (:underline (:color "deep sky blue" :style wave)))))

  :config
  ;; (customize-set-variable 'flycheck-mode-line-prefix "Φ")
  ;; (customize-set-variable 'flycheck-check-syntax-automatically
  ;;                         '(save mode-enabled))
  ;; (customize-set-variable 'flycheck-indication-mode nil)

  (setq flycheck-mode-line-prefix "Φ")
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; (setq flycheck-indication-mode nil)

  (flycheck-add-mode 'javascript-eslint 'web-mode)

  :defer t
  :ensure t)

(use-package flycheck-popup-tip
  :config
  (add-hook
   'flycheck-mode-hook
   (lambda ()
     (setq-local rh-popup-direction 'company)
     (flycheck-popup-tip-mode 1)))

  ;; (setq flycheck-popup-tip-error-prefix "> ")

  :after (flycheck popup)
  :ensure t)

;; (use-package flycheck-pos-tip
;;   :config
;;   (defun flycheck-pos-tip-hide-messages ()
;;     "Hide messages currently being shown if any."
;;     (flycheck-hide-error-buffer))

;;   (setq flycheck-pos-tip-timeout -1)
;;   (flycheck-pos-tip-mode)

;;   :after (flycheck pos-tip)
;;   :ensure t)

;;; /b/}

(use-package dumb-jump
  :config
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

  (setq dumb-jump-selector 'ivy)

  :bind (("C-M-/" . dumb-jump-hydra/body)
         ("C-M-." . dumb-jump-go)
         ("C-M-," . dumb-jump-back)
         ("C-M-[" . dumb-jump-back))
  :defer t
  :ensure t)

;;; /b/}

;; -------------------------------------------------------------------
;;; Programming Languages (Compilers, Debuggers, Profilers etc.)
;; -------------------------------------------------------------------
;; /b/{

(use-package vterm
  :if (locate-library "vterm")
  :commands (vterm vterm-mode)
  :config
  (require 'config-vterm)

  ;; (setq vterm-use-vterm-prompt-detection-method nil)
  (setq term-prompt-regexp
        (concat "^" user-login-name "@" system-name ":.*\\$ *"))
  ;; (setq term-prompt-regexp "^[^#$%>\\n]*[#$%>] ")

  ;; (vterm-send-C-d)
  :bind (("<menu>" . vterm-here)
         :map vterm-mode-map
         ("<kp-end>" . rh-vterm-send-end)
         ("<kp-home>" . rh-vterm-send-home)
         ("<deletechar>" . vterm-send-C-d)
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
         :map vterm-copy-mode-map
         ("RET" . nil)
         ("<return>" . nil)
         ("<kp-begin>" . rh-vterm-copy-mode)
         ("<kp-multiply>" . compilation-minor-mode))
  :defer t
  :pin manual)

(use-package sh-script
  :config
  (add-hook
   'sh-set-shell-hook
   (lambda ()
     (setq sh-basic-offset 2)
     (setq sh-indentation 2)
     (flycheck-mode 1)
     (rh-programming-minor-modes 1)))

  :defer t)

(cl-defun rh-compile-toggle-display
    (&optional (compilation-buffer-name "*compilation*"))
  (interactive)
  (rh-toggle-display compilation-buffer-name))


(use-package compile
  :config
  (setq compilation-scroll-output t)

  (let ((progress-in-minor-mode-alist
         (assq 'compilation-in-progress minor-mode-alist)))
    (if progress-in-minor-mode-alist
        ;; Before Emacs 28
        (setf (cdr progress-in-minor-mode-alist) '(" ⵛ"))
      ;; Since Emacs 28
      (setf (car (cdr (assq 'compilation-in-progress mode-line-modes)))
            (propertize "ⵛ "
	                'help-echo "Compiling; mouse-2: Goto Buffer"
                        'mouse-face 'mode-line-highlight
                        'local-map
                        (make-mode-line-mouse-map
                         'mouse-2
		         #'compilation-goto-in-progress-buffer)))))

  (require 'compile-eslint)

  ;; TODO: Move the following push to (use-package compile-eslint ...)
  ;;       config section and make `compile-eslint' load after `compile'
  (push 'eslint compilation-error-regexp-alist)
  ;; (push 'typescript-eslint compilation-error-regexp-alist)

  ;; This occasionally hungs emacs for some reason...
  ;; (require 'compile-nextjs)
  ;; (push 'nextjs-typecheck compilation-error-regexp-alist)
  ;; (push 'nextjs-compiling compilation-error-regexp-alist)

  (require 'ansi-color)
  (defun rh-ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'rh-ansi-colorize-buffer)

  (add-to-list
   'display-buffer-alist
   `(,(g2w-condition
       (lambda (buffer-nm action)
         (with-current-buffer buffer-nm
           (or (eq major-mode 'compilation-mode)
               compilation-minor-mode)))
       nil)
     (display-buffer-reuse-window
      display-buffer-in-side-window)
     (inhibit-same-window . t)
     (window-height . ,g2w-side-window-height)))

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'compile-goto-error)

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'compilation-display-error)

  :bind (:map compilation-mode-map
         ("q" . rh-bs-bury-buffer-and-delete-window-if-bottom-0-side)
         ("M-q" . rh-bs-kill-buffer-and-delete-window-if-bottom-0-side)
         ;; ("M-RET" . compilation-display-error)
         :map compilation-minor-mode-map
         ;; ("C-c C-c" . nil)
         ;; ("RET" . nil)
         ("q" . rh-bs-bury-buffer-and-delete-window-if-bottom-0-side)
         ("M-q" . rh-bs-kill-buffer-and-delete-window-if-bottom-0-side)
         :map compilation-button-map
         ;; ("RET" . compile-goto-error)
         ("M-RET" . compilation-display-error))
  :defer)

(use-package compile-eslint
  :defer t
  :pin manual)

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

(use-package shell
  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              ;; '((lambda (buffer-nm action)
  ;;              ;;     (eq (with-current-buffer buffer-nm major-mode)
  ;;              ;;         'shell-mode))
  ;;              '("*shell*"
  ;;                (display-buffer-same-window
  ;;                 rh-display-buffer-reuse-right
  ;;                 rh-display-buffer-reuse-left
  ;;                 rh-display-buffer-reuse-down
  ;;                 rh-display-buffer-reuse-up
  ;;                 display-buffer-pop-up-window)))

  :bind (:map shell-mode-map
          ("C-c C-b" . nil))
  :defer
  :ensure t)

(use-package man
  :config
  (setq Man-notify-method 'pushy)

  :defer
  :ensure t)

;;; Line Numbers
;;; /b/{

(use-package linum
  :init
  (setq rh-linum-right-space " ")
  (setq rh-linum-min-digits 4)

  (setq linum-format (concat "%" (number-to-string 4) "d"
                             rh-linum-right-space))
  :config
  (set-face-attribute 'linum nil :weight 'thin)

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
  (global-set-key (kbd "C-<f12>") (lambda ()
                                    (interactive)
                                    (nlinum--flush)))
  ;; (run-with-idle-timer 3 t #'nlinum--flush)

  ;; For some reason (bug?) when a new frame is open (e.g. C-x 5 2)
  ;; nlinum numbers become invisible. The following hook makes
  ;; them visible again.
  (defun rh-nlinum-flush-if-enabled ()
    (when (bound-and-true-p nlinum-mode) (nlinum--flush)))

  (add-hook 'focus-in-hook  #'rh-nlinum-flush-if-enabled)

  ;; For some reason (bug?) edebug hides nlinum after each operation.
  ;; This hook makes nlinum numbers visible again. It does cause
  ;; some flickering, in long term it worth finding why
  ;; edebug/nlinum behave this way.
  (defun rh-nlinum-flush-if-enabled-and-edebug ()
    (when (bound-and-true-p nlinum-mode)
      (run-at-time
       0 nil
       (lambda ()
         (when (and (bound-and-true-p edebug-mode)
                    (bound-and-true-p nlinum-mode))
           (nlinum--flush))))))

  (add-hook 'buffer-list-update-hook #'rh-nlinum-flush-if-enabled-and-edebug)

  :after linum
  :demand t
  :ensure t)

(use-package nlinum-hl
  :disabled t
  :config
  ;; Update linum when
  ;; whenever Emacs loses/gains focus
  ;; ...or switches windows
  ;; (advice-add #'select-window :before #'nlinum-hl-do-select-window-flush)
  ;; (advice-add #'select-window :after  #'nlinum-hl-do-select-window-flush)

  ;; (run-with-idle-timer 10 t #'nlinum-hl-flush-window)
  ;; (run-with-idle-timer 30 t #'nlinum-hl-flush-all-windows)

  :after nlinum
  :demand t
  :ensure t)

;;; /b/}

(use-package fill-column-indicator
  :ensure)

(use-package eldoc
  :delight (eldoc-mode " ε")
  :config
  (add-to-list 'rm-blacklist " ε")
  (setq eldoc-echo-area-use-multiline-p nil)

  :defer t)

(use-package htmlize
  :defer t
  :ensure t)

(use-package highlight-indent-guides
  :defer t
  :ensure t)

(use-package hideshow
  :config
  (add-to-list 'rm-blacklist " hs")

  (setq hs-allow-nesting t)
  (setq hs-isearch-open t)

  (defun rh-hs-set-up-overlay-handler (ov)
    (overlay-put ov 'display
                 (format (if (char-displayable-p ?•) " [• %d •] " " [* %d *] ")
                         (count-lines (overlay-start ov)
                                      (overlay-end ov))))
    (overlay-put ov 'face 'shadow))

  (setq hs-set-up-overlay 'rh-hs-set-up-overlay-handler)

  ;; This should be uncommented when cg becomes a mode which overrides
  ;; this key map
  ;; (define-key hs-minor-mode-map (kbd "C-S-j") #'hs-toggle-hiding)

  :bind (:map hs-minor-mode-map
         ("C-S-e" . hs-show-all))
  :demand t
  :ensure t)

(use-package paren
  :config
  ;; Activate the needed timer.
  (show-paren-mode)

  ;; The timer will do nothing if this is nil.
  (setq show-paren-mode nil)

  (cl-defun rh-show-paren-local-mode (&optional (value nil value-supplied-p))
    (interactive)
    (if (not (local-variable-p 'show-paren-mode))
        (make-local-variable 'show-paren-mode))
    (let ((show nil))
      (if (not value-supplied-p)
          (setq show (if show-paren-mode nil t))
        (setq show (not (eq value -1))))
      (setq show-paren-mode show))))

(cl-defun rh-programming-minor-modes (&optional (enable nil enable-supplied-p))
  "Enables some minor modes, useful for programming."
  (interactive)
  (let* ((toggle (not enable-supplied-p))
         (enabled (local-variable-p 'rh-prog-modes))
         (enabling (if toggle (if enabled nil t) enable))
         (disabling (not enabling)))
    (if (and (not enabled) enabling)
        (progn
          (set (make-local-variable 'rh-prog-modes) t)
          ;; (linum-mode 1)
          ;; (nlinum-mode 1)
          (rh-show-paren-local-mode 1)
          (hs-minor-mode 1)
          (undo-tree-mode 1)
          (code-groups-minor-mode 1)
          (hi-lock-mode 1)
          ;; (fci-mode 1)
          (set (make-local-variable 'show-trailing-whitespace) t)
          ;; (highlight-indent-guides-mode 1)
          ;; ;; Use case-sensitive search (buffer-local)
          ;; (setq case-fold-search nil)
          ;; (message "Enablibling programming modes")
          )
      (when (and enabled disabling)
        (kill-local-variable 'rh-prog-modes)
        ;; (linum-mode -1)
        ;; (nlinum-mode -1)
        (rh-show-paren-local-mode -1)
        (hs-minor-mode -1)
        (undo-tree-mode -1)
        (code-groups-minor-mode -1)
        (hi-lock-mode -1)
        ;; (fci-mode -1)
        (kill-local-variable 'show-trailing-whitespace)
        ;; (highlight-indent-guides-mode -1)
        ;; (message "Disabling programming modes")
        ))))

(use-package magit
  :init
  (defvar magit-log-margin '(t "%F %H:%M " magit-log-margin-width t 10))
  (defvar magit-log-section-arguments
    '("--graph" "--color" "--decorate" "-n256"))

  :config
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-nm action)
                   (and (not (eq major-mode 'magit-diff-mode))
                        (eq (with-current-buffer buffer-nm major-mode)
                            'magit-status-mode)))
                 (display-buffer-same-window
                  rh-display-buffer-reuse-right
                  rh-display-buffer-reuse-left
                  display-buffer-pop-up-window)))

  (add-to-list 'display-buffer-alist
               '((lambda (buffer-nm action)
                   (and (eq major-mode 'magit-diff-mode)
                        (eq (with-current-buffer buffer-nm major-mode)
                            'magit-status-mode)))
                 (rh-display-buffer-reuse-right
                  rh-display-buffer-reuse-left
                  display-buffer-pop-up-window)
                 (inhibit-same-window . t)))

  (add-to-list 'display-buffer-alist
               '((lambda (buffer-nm action)
                   (and (eq last-command 'magit-commit-create)
                        (eq (with-current-buffer buffer-nm major-mode)
                            'magit-diff-mode)))
                 (display-buffer-reuse-mode-window
                  rh-display-buffer-reuse-right
                  rh-display-buffer-reuse-left
                  display-buffer-pop-up-window)
                 (inhibit-same-window . t)))

  (setq transient-display-buffer-action
        ;; '(display-buffer-below-selected (side . bottom)))
        '(display-buffer-in-side-window (side . bottom)))

  (setq magit-bury-buffer-function #'quit-window)

  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source)

  ;; See https://github.com/magit/magit/issues/2541
  ;; (setq magit-display-buffer-function
  ;;       (lambda (buffer)
  ;;         (display-buffer
  ;;          buffer (if (and (derived-mode-p 'magit-mode)
  ;;                          (memq (with-current-buffer buffer major-mode)
  ;;                                '(magit-process-mode
  ;;                                  magit-revision-mode
  ;;                                  magit-diff-mode
  ;;                                  magit-stash-mode
  ;;                                  magit-status-mode)))
  ;;                     nil
  ;;                   '(display-buffer-same-window)))))
  :ensure t)

(use-package ghub
  :ensure t)

(use-package forge
  :init
  (customize-set-variable
   'forge-bug-reference-hooks
   '(git-commit-setup-hook magit-mode-hook))

  :config
  ;; TODO: Remove the following function after https with user name issue is
  ;;       resolved.
  ;; see https://github.com/magit/forge/issues/169
  ;; (defun rh-forge--url-regexp ()
  ;;   (concat "\\`\\(?:git://\\|[^/@]+@\\|ssh://\\(?:[^/@]+@\\)?"
  ;;           "\\|https?://\\(?:[^/@]+@\\)?\\)"
  ;;           (regexp-opt (mapcar #'car forge-alist) t)
  ;;           "[:/]\\(.+?\\)"
  ;;           "\\(?:\\.git\\|/\\)?\\'"))

  ;; (advice-add 'forge--url-regexp :override #'rh-forge--url-regexp)

  :after ghub
  :ensure t)

(use-package github-review
  :ensure t)

(use-package git-timemachine
  :ensure t)

(use-package elf-mode
  :ensure t)

(use-package gud
  :config
  ;; (defun vr-gud-call-func (begin end func)
  ;;   (let ((pos (point)))
  ;;     (funcall func '(begin end))
  ;;     ;; Allow debugger to run and return to the source buffer.
  ;;     ;; TODO: find how to wait on debugger instead of guessing the time.
  ;;     (sleep-for 0.1)
  ;;     (goto-char pos)))

  ;; (defun vr-gud-print (begin end)
  ;;   (interactive (rh-point-or-region))
  ;;   (vr-gud-call-func begin end 'gud-print))

  ;; (defun vr-gud-break (begin end)
  ;;   (interactive (rh-point-or-region))
  ;;   (vr-gud-call-func begin end 'gud-break))

  ;; (defun vr-gud-tbreak (begin end)
  ;;   (interactive (rh-point-or-region))
  ;;   (vr-gud-call-func begin end 'gud-tbreak))

  ;; (defun vr-gud-remove (begin end)
  ;;   (interactive (rh-point-or-region))
  ;;   (vr-gud-call-func begin end 'gud-remove))

  ;; (define-key gud-minor-mode-map (kbd "<f5>") 'vr-gud-print)
  ;; (define-key gud-minor-mode-map (kbd "S-<f5>") 'gud-watch)
  ;; (define-key gud-minor-mode-map (kbd "<f9>") 'vr-gud-break)
  ;; (define-key gud-minor-mode-map (kbd "S-<f9>") 'vr-gud-tbreak)
  ;; (define-key gud-minor-mode-map (kbd "C-<f9>") 'vr-gud-remove)
  ;; (define-key gud-minor-mode-map (kbd "<f10>") 'gud-next)
  ;; (define-key gud-minor-mode-map (kbd "<f11>") 'gud-step)
  )

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
  ;; :init
  ;; (defvar vr-gdb-original-buffer nil)

  :config
  ;; (defadvice gdb-setup-windows (around vr-gdb-setup-windows ())
  ;;   "Layout the window pattern for option `gdb-many-windows'."
  ;;   (gdb-get-buffer-create 'gdb-locals-buffer)
  ;;   (gdb-get-buffer-create 'gdb-stack-buffer)
  ;;   (gdb-get-buffer-create 'gdb-breakpoints-buffer)
  ;;   (set-window-dedicated-p (selected-window) t)
  ;;   (switch-to-buffer gud-comint-buffer)
  ;;   (delete-other-windows)
  ;;   (let ((win0 (selected-window))
  ;;         (win1 (split-window nil ( / ( * (window-height) 4) 5)))
  ;;         (win2 (split-window nil ( / (window-height) 4)))
  ;;         ;; (win3 (split-window-right))
  ;;         )
  ;;     ;; (gdb-set-window-buffer (gdb-locals-buffer-name) nil win3)
  ;;     (select-window win2)
  ;;     (set-window-buffer
  ;;      win2
  ;;      (if gud-last-last-frame
  ;;          (gud-find-file (car gud-last-last-frame))
  ;;        (if gdb-main-file
  ;;            (gud-find-file gdb-main-file)
  ;;          ;; Put buffer list in window if we
  ;;          ;; can't find a source file.
  ;;          (list-buffers-noselect))))
  ;;     ;; (set-window-dedicated-p (selected-window) nil)
  ;;     (setq gdb-source-window (selected-window))
  ;;     (select-window (split-window-below))
  ;;     (switch-to-buffer vr-gdb-original-buffer)
  ;;     ;; (let ((win4 (split-window-right)))
  ;;     ;;   (gdb-set-window-buffer
  ;;     ;;    (gdb-get-buffer-create 'gdb-inferior-io) nil win4))
  ;;     (select-window win1)
  ;;     (gdb-set-window-buffer (gdb-stack-buffer-name))
  ;;     (let ((win5 (split-window-right)))
  ;;       (gdb-set-window-buffer (if gdb-show-threads-by-default
  ;;                                  (gdb-threads-buffer-name)
  ;;                                (gdb-breakpoints-buffer-name))
  ;;                              nil win5))
  ;;     (select-window win0)))

  ;; (defadvice gdb (before vr-gdb (command-line))
  ;;   (setq vr-gdb-original-buffer (window-buffer)))

  ;; use gdb-many-windows by default
  (setq gdb-many-windows t)
  ;; Non-nil means display source file containing the main routine at startup
  ;; (setq gdb-show-main t)
  (setq gdb-delete-out-of-scope nil)
  (gdb-speedbar-auto-raise))

;; Autoinstall from init is disabled until the following problem is solved:
;; https://github.com/syl20bnr/spacemacs/issues/5917
;; (use-package realgud
;;   :commands (realgud:gdb realgud:gdb-pid realgud:pdb realgud:ipdb)

;;   :pin melpa
;;   :ensure t)

;;; /b/; C++
;;; /b/{

(defun rh-clangd-executable-find ()
  "Finds clangd executable if present."
  (let ((path (or (executable-find "clangd-11")
                  (executable-find "clangd-10")
                  (executable-find "clangd-9")
                  (executable-find "clangd-8")
                  (executable-find "clangd"))))
    (when path (file-name-nondirectory path))))

(use-package eglot
  :if (rh-clangd-executable-find)

  :config
  (require 'config-eglot)

  (setf
   (cdr (assoc '(c++-mode c-mode) eglot-server-programs))
   `(,(or (rh-clangd-executable-find) "clangd")
     "-j=6"
     "--background-index"
     "--all-scopes-completion"
     "--limit-results=0"
     "--suggest-missing-includes"
     "--completion-style=detailed"
     "--log=info"))

  :bind (:map eglot-mode-map
         ("C-<tab>" . company-complete)
         ("C-h C-." . rh-eglot-display-local-help))

  :defer t
  :ensure t)

(use-package lsp-mode
  :if (rh-clangd-executable-find)

  :init
  (defvar lsp-keymap-prefix "C-c l")

  :config
  (require 'company-capf)

  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-clients-clangd-executable (rh-clangd-executable-find))
  (setq lsp-signature-auto-activate '(:on-trigger-char :after-completion))
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (lsp-enable-which-key-integration t)

  (setq lsp-clients-clangd-args
        '("-j=6"
          "--background-index"
          "--all-scopes-completion"
          "--limit-results=0"
          ;; "--suggest-missing-includes"
          ;; "--header-insertion=never"
          ;; "--completion-style=detailed"
          "--completion-style=bundled"
          "--header-insertion-decorators=false"
          "--log=info"))

  :bind (:map lsp-mode-map
         ("C-x C-<tab>" . company-capf))

  :defer t
  :ensure t)

(use-package rtags
  :if (locate-library "rtags")
  :config
  (require 'config-rtags)

  ;; Idea is taken from:
  ;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
               `(,(g2w-condition "*RTags*" nil)
                 (display-buffer-below-selected)
                 (window-height . 0.3)
                 (inhibit-same-window . t)))

  ;; If following symbol without completion buffer, do it in the same window
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-nm actions)
                   (unless (string= (buffer-name) "*RTags*")
                     (with-current-buffer buffer-nm
                       (and (memq this-command
                                  '(rtags-find-symbol-at-point
                                    rtags-find-references-at-point
                                    rtags-find-virtuals-at-point
                                    rtags-references-tree))
                            (not (boundp 'g2w-destination-window))
                            (not (string= buffer-nm "*RTags*"))))))
                 (display-buffer-same-window)))

  (add-to-list 'display-buffer-alist
               '("*rdm*"
                 (display-buffer-in-side-window)
                 (side . top)
                 (inhibit-same-window . t)
                 (window-height . 6)))

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-select-and-remove-rtags-buffer)
  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-select-other-window)

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-next-match)
  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-previous-match)

  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-find-symbol-at-point)
  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-find-references-at-point)
  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-find-virtuals-at-point)
  (add-to-list 'g2w-display-buffer-reuse-window-commands
               'rtags-references-tree)

  (setq rtags-autostart-diagnostics t)
  (setq rtags-reindex-on-save t)
  (setq rtags-tramp-enabled t)
  ;; (setq rtags-completions-enabled t)
  ;; (setq rtags-process-flags "-R")
  (setq rtags-process-flags "--job-count=12")

  ;; see https://github.com/Andersbakken/rtags/issues/304
  ;; for flag '-M'
  ;; (setq rtags-process-flags "-M")
  ;; see https://stackoverflow.com/questions/41962611/how-to-select-a-particular-gcc-toolchain-in-clang
  ;; for gcc-toolchain explanations
  (let ((custom-gcc-toolchain
         (file-name-as-directory
          (expand-file-name "clang-gcc-toolchain" "~"))))
    (when (file-directory-p custom-gcc-toolchain)
      (setq rtags-process-flags
            (concat "--default-argument \"--gcc-toolchain="
                    custom-gcc-toolchain
                    "\""))))

  (rtags-enable-standard-keybindings)

  :defer t
  :pin manual)

(use-package modern-cpp-font-lock
  :commands modern-c++-font-lock-mode
  :config
  (add-to-list 'rm-blacklist " mc++fl")

  :defer t
  :ensure t)

(use-package rh-rtags-header-line
  :commands rh-rtags-header-line-setup
  :defer t
  :pin manual)

(use-package clang-format
  :if (locate-library "clang-format")
  ;; :load-path "/usr/share/emacs/site-lisp/clang-format-9.0"
  :defer t
  :pin manual)

(use-package google-c-style
  :defer t
  :ensure t)

(use-package rh-c-style
  :commands rh-c-style-setup
  :defer t
  :pin manual)

(use-package company-c-headers
  :defer t
  :ensure t)

(use-package cc-mode
  ;; :mode "/hpp\\'\\|\\.ipp\\'\\|\\.h\\'"
  :mode (("/hpp\\'\\|\\.ipp\\'" . c++-mode))
  :config
  (require 'rh-cc-mode-config)

  ;; Adopted from http://www.emacswiki.org/emacs/auto-complete-clang-extension.el
  (defun rh-gcc-get-isystem-path (compiler)
    (let* ((command-result (shell-command-to-string
                            (concat "echo \"\" | " compiler " -v -x c++ -E -")))
           (start-string "#include <...> search starts here:\n")
           (end-string "End of search list.\n")
           (start-pos (string-match start-string command-result))
           (end-pos (string-match end-string command-result))
           (include-string (substring command-result
                                      (+ start-pos (length start-string))
                                      end-pos))
           result)
      (setq result (split-string include-string))
      (add-to-list 'result "/usr/include" t)
      (add-to-list 'result "/usr/local/include" t)))

  (defun rh-cc-compile-setup ()
    (let ((project-path (rh-project-get-path)))
      (when project-path
        (set (make-local-variable 'compile-command)
             (concat project-path "make -k")))))

  (defun rh-c++-yas-setup ()
    (let* ((project-path (rh-project-get-path))
           (snippets-path (concat project-path "snippets")))
      (when (and project-path (file-exists-p snippets-path))
        (add-to-list 'yas-snippet-dirs snippets-path)
        (yas-reload-all))))

  (defun rh-c++-font-lock-setup ()
    (modern-c++-font-lock-mode 1))

  (defun rh-c++-indentation-setup ()
    (rh-c-style-setup))

  ;; (defun rh-cc-rtags-setup ()
  ;;   (rtags-start-process-unless-running)
  ;;   ;; (setq rtags-display-current-error-as-tooltip t)
  ;;   (rh-rtags-header-line-setup))

  (add-hook
   'c++-mode-hook
   (lambda ()
     ;; Using yas instead
     (abbrev-mode -1)
     (rh-programming-minor-modes t)
     ;; (rh-cc-rtags-setup)
     ;; (rh-rtags-eldoc-setup)
     (rh-c++-indentation-setup)
     (rh-c++-font-lock-setup)
     (rh-c++-yas-setup)
     (rh-cc-compile-setup)))

  (add-hook
   'c-mode-hook
   (lambda ()
     ;; Using yas instead
     (abbrev-mode -1)
     (rh-programming-minor-modes t)
     (rh-cc-compile-setup)))

  :bind (:map c-mode-base-map
         ("C-c C-b" . nil)
         ("C-S-b" . recompile)
         :map c++-mode-map
         ("," . nil)
         (";" . nil))

  :defer t)

;;; /b/}

(use-package js
  ;; :mode ("\\.js\\'" . js-mode)
  ;; :interpreter "node"
  ;; "λ" stands for interactive and "n" for Node.JS
  :delight '((:eval (if (bound-and-true-p jsi-node-mode)
                        "jsλn"
                      "js"))
             :major)
  ;; :delight '((:eval (if (bound-and-true-p indium-interaction-mode)
  ;;                       "jsλi"
  ;;                     "js"))
  :init
  (defvar js-mode-map (make-sparse-keymap))

  :config
  (setq interpreter-mode-alist
        (cl-delete-if (lambda (pair)
                        (or (equal pair '("node" . js-mode))
                            (equal pair '("nodejs" . js-mode))))
                      interpreter-mode-alist))

  (setq auto-mode-alist
        (cl-delete-if (lambda (pair)
                        (equal pair '("\\.jsx\\'" . js-jsx-mode)))
                      auto-mode-alist))

  ;; Indentation style ajustments
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)

  (add-hook
   'js-mode-hook
   (lambda ()
     (setq-local company-backends (copy-tree company-backends))
     (company-mode 1)

     (rh-programming-minor-modes 1)))

  ;; :bind (:map js-mode-map
  ;;        ("<f7>" . rh-nodejs-interaction))
  :defer t)

(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  ;; "λ" stands for interactive and "n" for Node.JS
  :delight '((:eval (if (bound-and-true-p jsi-node-mode)
                        "js2λn"
                      "js2"))
             :major)
  :config
  (require 'config-js2-mode)
  (require 'company)

  ;; Indentation style ajustments
  (setq js-indent-level 2)
  (setq js-switch-indent-offset 2)
  (setq js2-skip-preprocessor-directives t)

  ;; js-mode-hook is executed before js2-mode-hook
  ;; (add-hook
  ;;  'js2-mode-hook
  ;;  (lambda ()
  ;;    (setq-local rm-blacklist (seq-copy rm-blacklist))
  ;;    (add-to-list 'rm-blacklist " jsi-node")))

  :ensure t)

(use-package js2-refactor
  :defer t
  ;; :ensure t
  :disabled t)

(use-package typescript-mode
  ;; :mode "\\.ts\\'\\|\\.tsx\\'|\\.js\\'|\\.jsx\\'"
  :mode "\\.ts\\'\\|\\.tsx\\'"
  ;; :interpreter "node"
  :delight '((:eval (if (bound-and-true-p jsi-node-mode)
                        "tsλn"
                      "ts"))
             :major)
  :config
  (require 'config-typescript-mode)

  ;; (setq typescript-indent-level 2)

  ;; (add-hook
  ;;  'typescript-mode-hook
  ;;  (lambda ()
  ;;    (setq-local company-backends (copy-tree company-backends))
  ;;    (company-mode 1)

  ;;    (setq-local rm-blacklist (seq-copy rm-blacklist))
  ;;    (add-to-list 'rm-blacklist " jsi-node")
  ;;    (rh-programming-minor-modes 1)))

  :bind (:map typescript-mode-map
         ("{" . nil)
         ("}" . nil)
         ("(" . nil)
         (")" . nil)
         (":" . nil)
         (";" . nil)
         ("," . nil)
         ("\"" . nil)
         ("'" . nil))
  :defer t
  :ensure t)

(use-package tern
  :delight (tern-mode " ρ")
  :config

  (setq tern-command (list "npx" "tern"))
  (setq tern-update-argument-hints-timer -1)

  (defun rh-company-tern-display-permanent-doc-buffer ()
    (let ((buf (get-buffer-create "*company-documentation*")))
      (display-buffer buf)
      ;; (run-with-timer
      ;;  0 nil
      ;;  (lambda (buf)
      ;;    (dolist (win (get-buffer-window-list buf nil t))
      ;;      (shrink-window-if-larger-than-buffer win)))
      ;;  buf)
      ))

  ;; (add-hook
  ;;  'tern-mode-hook
  ;;  (lambda ()
  ;;    (set (make-local-variable 'rh-company-display-permanent-doc-buffer)
  ;;         #'rh-company-tern-display-permanent-doc-buffer)))

  :bind (:map tern-mode-keymap
         ("M-." . nil)
         ("C-M-." . nil)
         ("M-," . nil)
         ("C-c c-r" . nil)
         ("C-." . tern-find-definition)
         ("C-/" . tern-find-definition-by-name)
         ("C-," . tern-pop-find-definition)
         ;; ("M-h" . tern-get-docs)
         ;; ("C-c C-R" . tern-rename-variable)
         )
  :defer t
  :ensure t)

(use-package ac-js2
  :commands (ac-js2-mode)
  :init
  ;; (setq ac-js2-evaluate-calls t)

  :disabled t
  :ensure t)

;;; skewer-mode
;;; /b/{

(defun vr-skewer-eval-last-expression-or-region (start end)
  (interactive (rh-point-or-region))
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
  (interactive (rh-point-or-region))
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
  :config
  ;; Add host IP address to
  ;; (concat "~/.emacs-private.d/systems/" system-name ".el")
  ;; so httpd will use that IP instead of localhost
  ;;
  ;; e.g. (setq httpd-host "10.0.100.180")

  (httpd-start)
  :defer t
  :ensure t)

;;; /b/}

(use-package rh-scratch-js
  :commands rh-scratch-js
  :pin manual)

(use-package js-interaction
  :commands (jsi-node-mode
             jsi-node-repl
             jsi-node-eval
             jsi-node-eval-buffer)
  :config
  (add-to-list
   'display-buffer-alist
   '("*jsi-node-repl*"
     (display-buffer-reuse-window
      display-buffer-same-window)))

  (add-to-list 'rm-blacklist " jsi-node")

  (setq jsi-use-yarn2 nil)
  (setq jsi-node-command-require-esm nil)
  (setq jsi-babel-skip-import t)

  ;; Using company-capf until a proper company back-end is implemented
  ;; (require 'company-capf)
  ;; (bind-key "C-c C-<tab>" #'company-capf jsi-node-mode-keymap)

  :defer t
  :pin manual)

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (require 'env-css)
  (setq css-indent-offset 2)
  (add-hook
   'css-mode-hook
   (lambda ()
     (rh-programming-minor-modes 1)))

  :ensure t)

(use-package scss-mode
  :mode "\\.scss\\'"
  :config

  (add-hook
   'scss-mode-hook
   (lambda ()
     (rh-programming-minor-modes 1)
     (company-mode 1)))

  :bind (:map scss-mode-map
         ("C-S-b" . recompile)
         ("C-c b" . rh-compile-toggle-display))
  :defer t
  :ensure t)

(use-package web-beautify
  :ensure t)

;;; lisp-mode
;;; /b/{

(use-package ielm
  :config
  (setq eval-expression-print-length nil)
  (setq eval-expression-print-level nil)

  (defun rh-ielm-split-window ()
    (interactive)
    (split-window)
    (other-window 1)
    (ielm))

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
     (rh-programming-minor-modes 1)
     (eldoc-mode 1)
     (auto-complete-mode 1)
     (set (make-local-variable 'vr-elisp-mode) t)))

  :bind (:map lisp-mode-shared-map
         ("<f5>" . rh-lisp-eval-region-or-last-sexp)
         ("C-<f5>" . eval-print-last-sexp)
         ("S-<f5>" . rh-ielm-split-window)
         ("C-<tab>" . auto-complete))
  :after ielm
  :demand t)

(use-package elisp-slime-nav
  :config
  (add-to-list 'rm-blacklist " SliNav")

  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode))

  (define-key elisp-slime-nav-mode-map (kbd "M-[") 'pop-tag-mark)

  :after (lisp-mode ielm)
  :demand t
  :ensure t)

(use-package edebug
  :config
  (require 'config-edebug)
  (setq edebug-print-length -1)

  :demand t)

(use-package lispy
  :ensure t)

;; (use-package eval-sexp-fu
;;   :config
;;   (setq eval-sexp-fu-flash-duration 0.25)
;;   (setq eval-sexp-fu-flash-error-duration 0.7)

;;   :demand t
;;   :ensure t)

;;; /b/}

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :commands (python-repl run-python)
  :config
  (require 'rh-python-mode-config)

  (add-to-list
   'display-buffer-alist
   '("*Python*"
     (display-buffer-reuse-window
      display-buffer-same-window)))

  (setq python-indent-offset 2)
  (setq python-shell-interpreter "python3")

  (add-hook
   'python-mode-hook
   (lambda ()
     (rh-programming-minor-modes 1)
     (rh-python-company-setup)))

  :defer t)

(use-package bazel
  :mode "\\.bazel\\'\\|\\.bzl\\'\\|WORKSPACE\\'\\|\\.?BUILD\\'"
  :config
  (setq python-indent-offset 2)

  (add-hook
   'bazel-mode-hook
   (lambda ()
     (rh-programming-minor-modes 1)))

  :ensure t)

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  (require 'config-nxml-mode)

  (setq nxml-child-indent 2)
  (setq nxml-attribute-indent 2)
  (setq nxml-sexp-element-flag nil)

  (rh-nxml-code-folding-setup)

  (add-hook
   'nxml-mode-hook
   (lambda ()
     (setq cg-forward-list-original #'nxml-forward-element)
     (setq cg-backward-list-original #'nxml-backward-element)

     (rh-programming-minor-modes 1)))

  :bind (:map nxml-mode-map
         ("C-c C-b" . nil))
  :defer t)

;;; web-mode
;;; /b/{

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
          (rh-css-skewer-eval-current-declaration))
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
  :mode "\\.html\\'\\|\\.mako\\'\\|\\.tsx\\'\\|\\.jsx\\'"
  ;; :mode "\\.html\\'\\|\\.mako\\'"
  :config
  (require 'company)

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

  ;; (setq web-mode-comment-formats
  ;;       (remove-if (lambda (comment-format)
  ;;                    (string-equal (car comment-format) "javascript"))
  ;;                  web-mode-comment-formats))
  ;; (add-to-list 'web-mode-comment-formats '("javascript" . "//"))

  (setcdr (assoc "javascript" web-mode-comment-formats #'string=) "//")
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))

  (copy-face 'show-paren-match 'web-mode-current-element-highlight-face)

  (add-hook
   'web-mode-hook
   (lambda ()
     (setq-local company-backends (copy-tree company-backends))
     (company-mode 1)

     (rh-programming-minor-modes 1)
     (setq-local electric-indent-inhibit t)

     (local-set-key (kbd "C-S-j") #'vr-web-hs-toggle-hiding)
     (local-set-key (kbd "C-x C-S-j") #'vr-web-hs-html-toggle-hiding)
     (local-set-key (kbd "C-M-n") #'forward-sexp)
     (local-set-key (kbd "C-M-p") #'backward-sexp)

     ;; (local-set-key (kbd "C-S-b") #'recompile)
     ;; (local-set-key (kbd "C-c b") #'rh-compile-toggle-display)
     ))

  :bind (:map web-mode-map
         ("C-c C-b" . nil))
  :ensure t)

;;; /b/}

(use-package graphql-mode
  :config
  (add-hook
   'graphql-mode-hook
   (lambda ()
     ;; TODO: run-with-timer is a temporary workaround the following error
     ;;       "Error during redisplay: (jit-lock-function 1) signaled (end-of-buffer)"
     ;;       which occurs when graphql-mode is loaded.
     (run-with-timer
      0 nil
      (lambda ()
        (rh-programming-minor-modes t)))))

  :ensure t)

(use-package json-mode
  :ensure)

(use-package protobuf-mode
  :ensure)

;; (use-package flatbuffers-mode
;;   :ensure)

(use-package tide
  :delight (tide-mode " τ")
  :config
  (require 'company)
  (require 'flycheck)
  (require 'tide-yarn2)
  (require 'config-tide)

  (tide-yarn2-enable)

  (add-to-list
   'display-buffer-alist
   `("*tide-references*"
     ,(g2w-display #'display-buffer-below-selected t)
     (inhibit-same-window . t)
     (window-height . shrink-window-if-larger-than-buffer)))

  (add-to-list
   'display-buffer-alist
   '("*tide-documentation*"
     (display-buffer-reuse-window
      display-buffer-use-some-window
      display-buffer-pop-up-window)
     (inhibit-same-window . t)))

  (add-to-list
   'display-buffer-alist
   `((lambda (buffer-nm actions)
       (with-current-buffer buffer-nm
         (eq major-mode 'tide-project-errors-mode)))
     ,(g2w-display #'display-buffer-below-selected)
     (inhibit-same-window . t)
     ;; (window-height . shrink-window-if-larger-than-buffer)
     ))

  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-nm actions)
       (and (with-current-buffer buffer-nm
              (memq 'tide-mode minor-mode-list))
            (eq last-command 'tide-jump-to-definition)))
     (display-buffer-same-window)))

  ;; (flycheck-add-mode 'typescript-tslint 'web-mode)

  ;; (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

  (flycheck-add-next-checker 'javascript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)

  ;; (flycheck-add-next-checker 'typescript-tide
  ;;                            '(warning . javascript-eslint) 'append)
  ;; (flycheck-add-next-checker 'tsx-tide
  ;;                            '(warning . javascript-eslint) 'append)

  (setq tide-completion-ignore-case t)
  (setq tide-always-show-documentation t)
  (setq tide-completion-enable-autoimport-suggestions nil)
  ;; (setq tide-jump-to-definition-reuse-window nil)

  ;; company-tide is loaded after company
  (bind-key "C-x C-<tab>" #'company-tide tide-mode-map)

  ;; (add-hook
  ;;  'tide-mode-hook
  ;;  (lambda ()
  ;;    (set (make-local-variable 'rh-company-display-permanent-doc-buffer)
  ;;         #'rh-tide-company-display-permanent-doc-buffer)))

  ;; :after (company flycheck)
  :bind (:map tide-mode-map
         ("M-." . tide-jump-to-definition)
         ("M-/" . tide-jump-to-implementation)
         ("M-," . tide-references)
         ("M-[" . tide-jump-back)
         ("M-h" . tide-documentation-at-point)
         ;; ("C-x M-h" . rh-tide-documentation-quit)
         :map tide-references-mode-map
         ("q" . rh-quit-window-kill)
         ;; :map tide-project-errors-mode-map
         ;; ("q" . rh-quit-window-kill)
         )
  :defer t
  :ensure t)

;;; JavaScript Environments Setup
;;; /b/{

(defun rh-setup-typescript-tide ()
  (interactive)
  (tide-setup)
  (company-mode 1)
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  ;; TODO: should remove company-tide from default company-backends
  ;;       when company-keywords include typescript.
  ;; The keywords could be taken from the following link:
  ;; https://github.com/Microsoft/TypeScript/issues/2536
  ;; (setq-local company-backends (delq 'company-tide company-backends))
  (add-to-list 'company-backends 'company-tide)
  (local-set-key (kbd "C-x C-<tab>") #'company-tide))

(defun rh-setup-javascript-tide ()
  (interactive)
  (tide-setup)
  (company-mode 1)
  (flycheck-mode 1)
  (eldoc-mode 1)
  (tide-hl-identifier-mode 1)
  (add-to-list 'company-backends 'company-tide)
  (local-set-key (kbd "C-x C-<tab>") #'company-tide))

(defun rh-setup-javascript-tern ()
  (interactive)
  (tern-mode 1)
  (setq company-backends (delq 'company-tern company-backends))
  (local-set-key (kbd "C-c C-<tab>") #'company-tern))

(defun rh-setup-javascript-tern-tide ()
  (interactive)
  (tern-mode 1)
  (rh-setup-javascript-tide)
  (setq company-backends (delq 'company-tern company-backends))
  (local-set-key (kbd "C-c C-<tab>") #'company-tern))

;;; /b/}

;; /b/}

;; -------------------------------------------------------------------
;;; Structured Text and Markup (Meta) Languages
;; -------------------------------------------------------------------

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :ensure t)

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
;;; Natural Language Utilities and Spell Checking
;; -------------------------------------------------------------------

;; == /b/{ speck ==

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

;; == /b/} speck ==

;; == /b/{ ispell ==

(use-package ispell
  :config
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t)
  (setq ispell-use-framepop-p t)

  :bind (("C-x w" . 'ispell-word))
  :defer t)

;; == /b/} ispell ==

;; -------------------------------------------------------------------
;;; MS Windows interaction
;; -------------------------------------------------------------------

(cond
 ((eq system-type 'windows-nt)
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

 ((eq system-type 'gnu/linux)
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

;; (global-set-key (kbd "<f11>") 'fullscreen)
;; (global-set-key (kbd "M-<return>") 'vr-toggle-max-res-frame)
;; (global-set-key (kbd "M-<kp-enter>") 'vr-toggle-max-res-frame)

;; -------------------------------------------------------------------
;;; General Emacs enhancement modes
;; -------------------------------------------------------------------

(defvar rh-buffers-groups nil
  "Buffer groups used to create filters for bs-configurations.
The group item values can be either buffer name regex
or buffer major mode symbol")

(defvar rh-buffers-not-file-group nil
  "Not-file buffers used to create files filter in bs-configurations.
The buffer value can be either buffer name regex or buffer major mode symbol")

(defvar rh-buffers-semantic-not-file-groups nil
  "Not-file buffer groups used to create filters for bs-configurations.
The group item values can be either buffer name regex
or buffer major mode symbol")

(setq
 rh-buffers-semantic-not-file-groups
 '(("dired"
    (dired-mode))
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
   help-mode
   debugger-mode
   special-mode
   ;; paradox
   paradox-menu-mode
   "^\\*Paradox http\\*$"
   ;; eldoc
   "^\\*eldoc .*\\*$"
   ;; tide
   "^\\*tide-server\\*.*$"
   "^\\*node process\\*$"
   ;; skewer
   "^\\*skewer-error\\*$"
   ;; lsp
   lsp-log-io-mode
   "^\\*lsp-log\\*$"
   "^\\*clangd\\*$"
   ;; eglot
   "^\\*EGLOT .*$"
   ;; flymake
   "^\\*Flymake log\\*$"
   ;; rtags buffers
   "^\\*rdm\\*$"
   "^\\*RTags\\*$"
   "^\\*RTags Diagnostics\\*$"
   "^\\*RTags Log\\*$"
   ;; AUCTeX output files
   " output\\*$"))

(setq rh-buffers-groups '())

(dolist (buffer-group rh-buffers-semantic-not-file-groups)
  (setq rh-buffers-groups
        (append (list (copy-tree buffer-group)) rh-buffers-groups)))

(setq rh-buffers-groups (nreverse rh-buffers-groups))

(dolist (buffer-group rh-buffers-semantic-not-file-groups)
  (setq rh-buffers-not-file-group
        (append (car (cdr buffer-group))
                rh-buffers-not-file-group)))

;; (defun rh-buffers-match (regexp-or-mode-list buffer)
;;   "Return non-nil if buffer either matches anything in listed regexps
;; or has one of the listed major modes."
;;   (let ((case-fold-search nil))
;;     (catch 'done
;;       (dolist (regexp-or-mode regexp-or-mode-list)
;;         (if (stringp regexp-or-mode)
;;             (let ((regexp regexp-or-mode)
;;                   (name (buffer-name buffer)))
;;               (when (string-match-p regexp name)
;;                 (throw 'done t)))
;;           (let ((mode regexp-or-mode))
;;             (when (eq (with-current-buffer buffer major-mode) mode)
;;               (throw 'done t))))))))

(defun rh-buffers-match (regexp-or-mode-or-func-list buffer)
  "Return non-nil if buffer either matches anything in listed regexps
or has one of the listed major modes."
  (let ((case-fold-search nil))
    (seq-find
     ;; (lambda (regexp-or-mode)
     ;;   (if (stringp regexp-or-mode)
     ;;       (string-match-p regexp-or-mode (buffer-name buffer))
     ;;     (eq (with-current-buffer buffer major-mode) regexp-or-mode)))
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

;; Example:
;; Makefile.am, Makefile.am<3> etc.  to
;; Makefile.am|path1, Makefile.am|path2
;; http://emacs-fu.blogspot.co.uk/2009/11/making-buffer-names-unique.html
;; (require 'uniquify)
;; (setq uniquify-buffer-name-style 'post-forward)

;; Provides additional help functions such as describe-keymap bound to C-h M-k
;; (require 'help-fns+)

;; /b/{ ifilipb

(use-package iflipb
  :config
  (setq iflipb-ignore-buffers
        '((lambda (buffer-nm)
            ;; (let ((buffer (get-buffer buffer-nm)))
            ;;   (not (and (string=
            ;;              (rh-buffers-get-group-name (window-buffer))
            ;;              (rh-buffers-get-group-name buffer))
            ;;             (rh-context-show-buffer-p buffer))))

            (rh-buffers-match
             rh-buffers-not-file-group
             (get-buffer buffer-nm))

            )))

  (setq iflipb-wrap-around t)

  (global-set-key (kbd "C-<next>") #'iflipb-next-buffer)
  (global-set-key (kbd "C-<kp-next>") #'iflipb-next-buffer)
  (global-set-key (kbd "C-<prior>") #'iflipb-previous-buffer)
  (global-set-key (kbd "C-<kp-prior>") #'iflipb-previous-buffer)

  :demand t
  :ensure t)

;; /b/} ifilipb

;; /b/{ rh-bs

(use-package rh-bs
  :config
  ;; (require 'rh-bs)

  ;; (add-to-list
  ;;  'display-buffer-alist
  ;;  `(,(g2w-condition
  ;;      (lambda (buffer-nm action)
  ;;        (eq (with-current-buffer buffer-nm major-mode)
  ;;            'bs-mode))
  ;;      nil)
  ;;    (display-buffer-reuse-window
  ;;     rh-display-buffer-reuse-right
  ;;     rh-display-buffer-reuse-left
  ;;     ;; rh-display-buffer-reuse-down
  ;;     ;; rh-display-buffer-reuse-up
  ;;     ;; display-buffer-use-some-window
  ;;     display-buffer-pop-up-window)
  ;;    (inhibit-same-window . t)))

  ;; (add-to-list 'g2w-display-buffer-reuse-window-commands
  ;;              'bs-tmp-select-other-window)

  (setq
   bs-configurations
   '(("sys" nil nil nil
      (lambda (buffer)
        (not (rh-buffers-match '("\\` ") buffer)))
      rh-bs-sort-by-file-path-interns-are-last)
     ("all" nil nil nil
      (lambda (buffer)
        (or (not (rh-context-show-buffer-p buffer))
            (rh-buffers-match '("\\` ") buffer)))
      rh-bs-sort-by-file-path-interns-are-last)
     ("files" nil nil nil
      (lambda (buffer)
        (or (not (rh-context-show-buffer-p buffer))
            (rh-buffers-match rh-buffers-not-file-group buffer)))
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

  :bind (("C-x C-b" . rh-bs-show)
         ("C-c C-b" . rh-bs-toggle-bs-in-bottom-0-side-window)
         ("C-c b" . rh-bs-tmp-toggle-bottom-0-side-window)
         ("S-<menu>" . rh-context-window-config-switch))
  :demand t
  :pin manual)

;; /b/} rh-bs

;; /b/{ bm

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

  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle))
  :demand t
  :ensure t)

;; /b/} bm

;; -------------------------------------------------------------------
;;; Load unscoped (e.g. without rh-) useful functions
;; -------------------------------------------------------------------

(load-file (concat user-emacs-directory "useful-functions.el"))

;; -------------------------------------------------------------------
;;; Post intit - to ensure correct settings if they were changed.
;; -------------------------------------------------------------------

;; (transient-mark-mode -1)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
