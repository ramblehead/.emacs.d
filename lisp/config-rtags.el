;;; ramblehead's rtags configuration

(require 'rh-rtags-header-line)

(defun rh-rtags-toggle-rdm-display ()
  (interactive)
  (rh-toggle-display "*rdm*" t))

(custom-set-faces
 '(rtags-errline ((((class color)) (:background "#ef8990"))))
 '(rtags-fixitline ((((class color)) (:background "#ecc5a8"))))
 '(rtags-warnline ((((class color)) (:background "#efdd6f"))))
 '(rtags-skippedline ((((class color)) (:background "#c2fada")))))

(setq rtags-other-window-function (lambda () (other-window -1)))
(setq rtags-results-buffer-other-window t)
(setq rtags-bury-buffer-function 'quit-window)

(add-hook
 'rtags-references-tree-mode-hook
 (lambda ()
   (setq-local truncate-lines t)))

(add-hook
 'rtags-diagnostics-mode-hook
 (lambda ()
   (setq-local truncate-lines t)))

(add-hook
 'rtags-mode-hook
 (lambda ()
   (setq-local truncate-lines t)))

(defvar rh-rtags-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r d") #'rh-rtags-toggle-rdm-display)
    (define-key map (kbd "s-[") #'rtags-location-stack-back)
    (define-key map (kbd "s-]") #'rtags-location-stack-forward)
    (define-key map (kbd "s-.") #'rtags-find-symbol-at-point)
    (define-key map (kbd "s->") #'rtags-next-match)
    (define-key map (kbd "s-<") #'rtags-previous-match)
    (define-key map (kbd "s-,") #'rtags-references-tree)
    (define-key map (kbd "M-s-,") #'rtags-find-virtuals-at-point)
    ;; (define-key map (kbd "M-i") #'rtags-imenu)
    ;; (define-key map (kbd "C-.") #'rtags-find-symbol)
    ;; (define-key map (kbd "C-,") #'rtags-find-references)
    map))

(defun rh-rtags-mode-toggle-default-enabled ()
  "Toggle `rh-rtags-mode-default-enabled'"
  (interactive)
  (setq rh-rtags-mode-default-enabled (not rh-rtags-mode-default-enabled))
  (if rh-rtags-mode-default-enabled
      (message "rh-rtags-mode should be enabled by default")
    (message "rh-rtags-mode should be disabled by default")))

(define-minor-mode rh-rtags-mode
  "Minor mode to start rtags."
  :lighter " ℜ"
  :keymap rh-rtags-mode-map
  (rtags-start-process-unless-running)
  (rh-rtags-header-line-setup))

(defun rh-rtags-after-save-hook (orig-fun)
  (when rh-rtags-mode (funcall orig-fun)))

(advice-add 'rtags-after-save-hook :around
            #'rh-rtags-after-save-hook)

(provide 'config-rtags)
