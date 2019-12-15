;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with <open> and enter text in its buffer.

(ace-select-window)
(let* ((window (frame-selected-window))
       (windows '()))
  (push window windows)
  (dotimes (i 2)
    (setq window (split-window))
    (balance-windows (window-parent window))
    (push window windows))
  (window-make-atom (window-parent window)))

(balance-windows (window-atom-root (frame-selected-window)))
