;;; ramblehead's xref configuration

(add-hook 'xref-after-return-hook #'recenter -100)

(defcustom xref-after-undo-return-hook
  '(recenter
    xref-pulse-momentarily)
  "Functions called after undoing return to a pre-jump location."
  :type 'hook)

(defcustom rh-xref--return-undo-ring
  (make-ring (ring-size xref--marker-ring))
  "Ring of markers to implement the return undo stack.")

;; (setq rh-xref--return-undo-ring
;;   (make-ring (ring-size xref--marker-ring)))

;; (setq xref--marker-ring
;;   (make-ring (ring-size xref--marker-ring)))

;; (defun rh-xref-erase-return-undo-ring ()
;;   (setf (car rh-xref--return-undo-ring) 0)
;;   (setf (cadr rh-xref--return-undo-ring) 0)
;;   (setf (cddr rh-xref--return-undo-ring)
;;         (make-vector (ring-size xref--marker-ring) nil))
;;   rh-xref--return-undo-ring)

(defun rh-xref-undo-return ()
  (interactive)
  (let ((undo-ring rh-xref--return-undo-ring))
    (when (ring-empty-p undo-ring)
      (user-error "Return undo stack is empty"))
    (let ((undo-marker (ring-remove undo-ring 0)))
      ;; (ring-insert xref--marker-ring (copy-marker undo-marker))
      (switch-to-buffer (or (marker-buffer undo-marker)
                            (user-error "The marked buffer has been deleted")))
      (goto-char (marker-position undo-marker))
      (set-marker undo-marker nil nil)
      (run-hooks 'xref-after-undo-return-hook))))

(defun xref-pop-marker-stack ()
  "Pop back to where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let* ((undo-ring rh-xref--return-undo-ring)
         (undo-ring-not-empty (not (ring-empty-p undo-ring)))
         (undo-ring-0
          (when undo-ring-not-empty (ring-ref undo-ring 0))))
    (if (not (equal undo-ring-0 (point-marker)))
        (goto-char (marker-position undo-ring-0))
      (let ((ring xref--marker-ring))
        (when (ring-empty-p ring)
          (user-error "Marker stack is empty"))
        (let ((marker (ring-remove ring 0)))
          (switch-to-buffer (or (marker-buffer marker)
                                (user-error "The marked buffer has been deleted")))
          (ring-insert rh-xref--return-undo-ring (copy-marker marker))
          (goto-char (marker-position marker))
          (set-marker marker nil nil)
          (run-hooks 'xref-after-return-hook))))))

(defun rh-xref-reset-return-undo-ring ()
  (setf (car rh-xref--return-undo-ring) 0)
  (setf (cadr rh-xref--return-undo-ring) 0)
  (setf (cddr rh-xref--return-undo-ring)
        (make-vector (ring-size xref--marker-ring) nil))
  (ring-insert rh-xref--return-undo-ring (copy-marker (point-marker)))
  rh-xref--return-undo-ring)

(add-hook 'xref-after-jump-hook #'rh-xref-reset-return-undo-ring)

(provide 'config-xref)
