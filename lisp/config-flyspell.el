;;; ramblehead's flyspell configuration

(defun rh-markdown-flyspell-check-word-p (orig-fun &rest r)
  (save-excursion
    (goto-char (1- (point)))
    (if (markdown--face-p (point) '(markdown-metadata-key-face
                                    markdown-html-tag-name-face
                                    markdown-html-attr-name-face
                                    markdown-html-entity-face))
        (prog1 nil
          ;; If flyspell overlay is put, then remove it
          (let ((bounds (bounds-of-thing-at-point 'word)))
            (when bounds
              (cl-loop for ov in (overlays-in (car bounds) (cdr bounds))
                       when (overlay-get ov 'flyspell-overlay)
                       do
                       (delete-overlay ov)))))
      (apply orig-fun r))))

(eval-after-load 'markdown-mode
  (advice-add 'markdown-flyspell-check-word-p :around
              #'rh-markdown-flyspell-check-word-p))

(defvar-local flyspell-visible-last-window-range nil)

(defun flyspell-visible--overlay-refresh-from-timer ()
  (with-local-quit
    (when (equal flyspell-visible--delay-buffer (current-buffer))
      (let* ((start (window-start))
             (end (window-end))
             (range (list start end)))
        (when (not (equal range flyspell-visible-last-window-range))
          (setq inhibit-redisplay t)
          (let ((flyspell-issue-message-flag nil))
            (flyspell-small-region start end))
          (setq-local flyspell-visible-last-window-range range)
          (setq inhibit-redisplay nil))))))

;; Timer
(defvar flyspell-visible--delay-timer nil)
(defvar flyspell-visible--delay-buffer nil)

(defun flyspell-visible-turn-on ()
  (when (timerp flyspell-visible--delay-timer)
    (cancel-timer flyspell-visible--delay-timer))
  ;; Delay for spelling - should be ok, only runs when scroll changes.
  (setq flyspell-visible--delay-buffer (current-buffer))
  (setq flyspell-visible--delay-timer
    (run-with-idle-timer 1.0 t 'flyspell-visible--overlay-refresh-from-timer)))

(defun flyspell-visible-turn-off ()
  (when (timerp flyspell-visible--delay-timer)
    (cancel-timer flyspell-visible--delay-timer))
  (setq flyspell-visible--delay-timer nil)
  (setq flyspell-visible--delay-buffer nil)
  (setq flyspell-visible-last-window-range nil))

(provide 'config-flyspell)
