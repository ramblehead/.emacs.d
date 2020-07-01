;;; ramblehead's xref configuration

(defcustom xref-after-undo-return-hook
  '(rh-recenter-sensibly
    xref-pulse-momentarily)
  "Functions called after undoing return to a pre-jump location."
  :type 'hook)

(defcustom rh-xref--return-undo-ring
  (make-ring (ring-size xref--marker-ring))
  "Ring of markers to implement the return undo stack.")

(remove-hook 'xref-after-jump-hook #'recenter)

(add-hook 'xref-after-return-hook #'rh-recenter-sensibly)
(add-hook 'xref-after-jump-hook #'rh-recenter-sensibly)

(defun rh-xref-undo-return ()
  (interactive)
  (let* ((undo-ring rh-xref--return-undo-ring)
         (undo-ring-not-empty (not (ring-empty-p undo-ring)))
         (undo-ring-0
          (and undo-ring-not-empty (ring-ref undo-ring 0))))
    (if (not (equal undo-ring-0 (point-marker)))
        (progn
          (switch-to-buffer
           (or (marker-buffer undo-ring-0)
               (user-error "The marked buffer has been deleted"))
           (goto-char (marker-position undo-ring-0)))
          (run-hooks 'xref-after-undo-return-hook))
      (let ((undo-ring rh-xref--return-undo-ring))
        (when (< (ring-length undo-ring) 2)
          (user-error "Return undo stack is empty"))
        (let* ((undo-marker (ring-remove undo-ring 0))
               (goto-marker (ring-ref undo-ring 0))
               (goto-buffer (marker-buffer goto-marker)))
          (when (not goto-buffer)
            (user-error "The marked buffer has been deleted"))
          (ring-insert xref--marker-ring (copy-marker undo-marker))
          (switch-to-buffer goto-buffer)
          (goto-char (marker-position goto-marker))
          (set-marker undo-marker nil nil)
          (run-hooks 'xref-after-undo-return-hook))))))

(defun rh-xref-return ()
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The marked buffer has been deleted")))
      (ring-insert rh-xref--return-undo-ring (copy-marker marker))
      (goto-char (marker-position marker))
      (set-marker marker nil nil)
      (run-hooks 'xref-after-return-hook))))

(advice-add 'xref-pop-marker-stack :override
            #'rh-xref-return)

(defun rh-xref-reset-return-undo-ring ()
  (setf (car rh-xref--return-undo-ring) 0)
  (setf (cadr rh-xref--return-undo-ring) 0)
  (setf (cddr rh-xref--return-undo-ring)
        (make-vector (ring-size xref--marker-ring) nil))
  (ring-insert rh-xref--return-undo-ring (copy-marker (point-marker)))
  rh-xref--return-undo-ring)

(add-hook 'xref-after-jump-hook #'rh-xref-reset-return-undo-ring)

(defun rh-xref-run-after-jump-hook (&rest _)
  (run-hooks 'xref-after-jump-hook))

(eval-after-load 'elisp-slime-nav
  (advice-add 'elisp-slime-nav-find-elisp-thing-at-point :after
              #'rh-xref-run-after-jump-hook))

(eval-after-load 'tide
  (advice-add 'tide-jump-to-filespan :after
              #'rh-xref-run-after-jump-hook))

(provide 'config-xref)
