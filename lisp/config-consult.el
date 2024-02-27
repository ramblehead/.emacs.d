;;; consult-patch.el --- Consulting completing-read -*- lexical-binding: t -*-

(require 'consult)

(defun consult-line-from-isearch ()
  "Call `consult-line` with the search string from the last `isearch`."
  (interactive)
  (consult-line isearch-string))

;; ;; Rest is a set of patched functions from main consult codebase, with some new helpers

;; ;; Reuse highlight overlays to reduce allocations
;; (defvar consult--highlight-overlays (make-vector 1 nil))
;; (defvar consult--highlight-overlays-last-index nil)

;; (defun consult--reset-highlights ()
;;   "Start new iteration of highlighting. `consult--cleanup-highlights' must be called after all
;; highlights are added"
;;   (setq consult--highlight-overlays-last-index 0))

;; (defun consult--cleanup-highlights ()
;;   "Delete overlays while they are still attached to buffers.
;; Overlays are guaranteed to be set sequentially. So there will be no live overlays after first
;; deleted overlay."
;;   (let ((num-highlights (length consult--highlight-overlays)))
;;     (while (< consult--highlight-overlays-last-index num-highlights)
;;       (let* ((overlay (elt consult--highlight-overlays consult--highlight-overlays-last-index))
;;              (buf (when overlay (overlay-buffer overlay))))
;;         (if (not buf)
;;             (setq consult--highlight-overlays-last-index num-highlights)
;;           (setq consult--highlight-overlays-last-index (1+ consult--highlight-overlays-last-index))
;;           (delete-overlay overlay)
;;           (overlay-put overlay 'window nil))))))

;; (defun consult--highlight (beg end)
;;   (let* ((num-highlights (length consult--highlight-overlays))
;;          (ov (when (< consult--highlight-overlays-last-index num-highlights)
;;               (elt consult--highlight-overlays consult--highlight-overlays-last-index))))
;;     (if ov
;;         (move-overlay ov beg end)
;;       (setq ov (consult--make-overlay beg end
;;                                       'face 'consult-preview-match
;;                                       'priority 100))
;;       (when (>= consult--highlight-overlays-last-index num-highlights)
;;         (let ((new-highlights (make-vector (* num-highlights 2) nil)))
;;           (dotimes (i num-highlights)
;;             (aset new-highlights i (elt consult--highlight-overlays i)))
;;           (setq consult--highlight-overlays new-highlights)))
;;       (aset consult--highlight-overlays consult--highlight-overlays-last-index ov))
;;     (setq consult--highlight-overlays-last-index (1+ consult--highlight-overlays-last-index))
;;     (overlay-put ov 'window (selected-window))
;;     ov))

;; ;; Returns position of candidate as number, even if it was converted to marker already
;; (defun consult--get-candidate-position (candidate)
;;   (let ((loc (get-text-property 0 'consult-location candidate)))
;;     (if (consp (car loc))
;;         (cdar loc)
;;       (marker-position (car loc)))))

;; ;; All candidates are assumed to come from current buffer!
;; (defun consult--highlight-by-links (link-key candidate highlight-func)
;;   (let ((wnd-start (window-start))
;;         (wnd-end (window-end (selected-window) t)))
;;     (while candidate
;;       (let ((candidate-position (consult--get-candidate-position candidate)))
;;         (if (or (> candidate-position wnd-end)
;;                 (< (+ candidate-position (length candidate)) wnd-start))
;;             (setq candidate nil)
;;           (let* ((matched (funcall highlight-func candidate))
;;                  (matches-start (+ candidate-position (car matched)))
;;                  (matches (cdr matched)))
;;             (dolist (match matches)
;;               (consult--highlight (+ matches-start (car match)) (+ matches-start (cdr match)))))
;;           (setq candidate (get-text-property 0 link-key candidate)))))))

;; (defun consult--jump-preview ()
;;   "The preview function used if selecting from a list of candidate positions.
;; The function can be used as the `:state' argument of `consult--read'."
;;   (let ((saved-min (point-min-marker))
;;         (saved-max (point-max-marker))
;;         (saved-pos (point-marker))
;;         restore)
;;     (set-marker-insertion-type saved-max t) ;; Grow when text is inserted
;;     (lambda (action cand)
;;       (when (eq action 'preview)
;;         (mapc #'funcall restore)
;;         (setq restore nil)
;;         (consult--reset-highlights)
;;         (if (not cand)
;;             ;; If position cannot be previewed, return to saved position
;;             (let ((saved-buffer (marker-buffer saved-pos)))
;;               (if (not saved-buffer)
;;                   (message "Buffer is dead")
;;                 (set-buffer saved-buffer)
;;                 (narrow-to-region saved-min saved-max)
;;                 (goto-char saved-pos)))
;;           ;; Candidate can be previewed
;;           (consult--jump-1 (or (car-safe cand) cand))
;;           (run-hooks 'consult-after-jump-hook)
;;           (setq restore (consult--invisible-open-temporarily))
;;           ;; Ensure that cursor is properly previewed (gh:minad/consult#764)
;;           (unless (eq cursor-in-non-selected-windows 'box)
;;             (let ((orig cursor-in-non-selected-windows)
;;                   (buf (current-buffer)))
;;               (push
;;                (if (local-variable-p 'cursor-in-non-selected-windows)
;;                    (lambda ()
;;                      (when (buffer-live-p buf)
;;                        (with-current-buffer buf
;;                          (setq-local cursor-in-non-selected-windows orig))))
;;                  (lambda ()
;;                    (when (buffer-live-p buf)
;;                      (with-current-buffer buf
;;                        (kill-local-variable 'cursor-in-non-selected-windows)))))
;;                restore)
;;               (setq-local cursor-in-non-selected-windows 'box)))
;;           ;; Match previews
;;           (let ((overlay (save-excursion
;;                            (let ((vbeg (progn (beginning-of-visual-line) (point)))
;;                                  (vend (progn (end-of-visual-line) (point)))
;;                                  (end (pos-eol)))
;;                              (consult--make-overlay vbeg (if (= vend end) (1+ end) vend)
;;                                                     'face 'consult-preview-line
;;                                                     'window (selected-window)
;;                                                     'priority 1)))))
;;             (dolist (match (cdr-safe cand))
;;               (cond
;;                ((eq (car-safe match) 'orig)
;;                 (consult--highlight-by-links 'consult-prev (cadr match) (caddr match))
;;                 (consult--highlight-by-links 'consult-next (cadr match) (caddr match)))
;;                ((not (symbolp (car-safe match)))
;;                 (consult--highlight (+ (point) (car match)) (+ (point) (cdr match))))))
;;             (push (lambda () (delete-overlay overlay)) restore)))
;;         (consult--cleanup-highlights)))))

;; (defun consult--line-candidates (top curr-line)
;;   "Return list of line candidates.
;; Start from top if TOP non-nil.
;; CURR-LINE is the current line number."
;;   (consult--forbid-minibuffer)
;;   (consult--fontify-all)
;;   (let* ((buffer (current-buffer))
;;          (line (line-number-at-pos (point-min) consult-line-numbers-widen))
;;          default-cand candidates)
;;     (let ((prev-candidate nil))
;;       (consult--each-line beg end
;;         (unless (looking-at-p "^\\s-*$")
;;           (let ((candidate (consult--location-candidate
;;                             (consult--buffer-substring beg end)
;;                             (cons buffer beg) line line)))
;;             (push candidate candidates)
;;             (when prev-candidate
;;               (add-text-properties 0 1 `(consult-prev ,prev-candidate) candidate)
;;               (add-text-properties 0 1 `(consult-next ,candidate) prev-candidate))
;;             (setq prev-candidate candidate)
;;             (when (and (not default-cand) (>= line curr-line))
;;               (setq default-cand candidates))))
;;         (cl-incf line)))
;;     (unless candidates
;;       (user-error "No lines"))
;;     (nreverse
;;      (if (or top (not default-cand))
;;          candidates
;;        (let ((before (cdr default-cand)))
;;          (setcdr default-cand nil)
;;          (nconc before candidates))))))

;; (defun consult--line-point-placement (selected candidates highlighted &rest ignored-faces)
;;   "Find point position on matching line.
;; SELECTED is the currently selected candidate.
;; CANDIDATES is the list of candidates.
;; HIGHLIGHTED is the highlighted string to determine the match position.
;; IGNORED-FACES are ignored when determining the match position."
;;   (when-let (pos (consult--lookup-location selected candidates))
;;     (if highlighted
;;         (let* ((matches (apply #'consult--point-placement highlighted 0 ignored-faces))
;;                (dest (+ pos (car matches))))
;;           ;; Only create a new marker when jumping across buffers (for example
;;           ;; `consult-line-multi').  Avoid creating unnecessary markers, when
;;           ;; scrolling through candidates, since creating markers is not free.
;;           (when (and (markerp pos) (not (eq (marker-buffer pos) (current-buffer))))
;;             (setq dest (move-marker (make-marker) dest (marker-buffer pos))))
;;           (cons dest (cdr matches)))
;;       pos)))

;; (defun consult--line-match (selected candidates input &rest _)
;;   "Lookup position of match.
;; SELECTED is the currently selected candidate.
;; CANDIDATES is the list of candidates.
;; INPUT is the input string entered by the user."
;;   (let* ((highlighted (and (not (string-blank-p input))
;;                            (car (consult--completion-filter
;;                                  input
;;                                  (list (substring-no-properties selected))
;;                                  'consult-location 'highlight))))
;;          (match (consult--line-point-placement selected candidates
;;                                                highlighted
;;                                                'completions-first-difference)))
;;     (when (and highlighted match)
;;       (let ((orig-selected (car (member selected candidates)))
;;             (highlight-func (lambda (cand)
;;                               (let* ((highlighted-cand (car (consult--completion-filter
;;                                                              input
;;                                                              (list (substring-no-properties cand))
;;                                                              'consult-location 'highlight)))
;;                                      (matches (apply #'consult--point-placement highlighted-cand 0 '(completions-first-difference))))
;;                                 matches))))
;;         ;; Append original candidate + highlight function to result
;;         (setq match (cons (car match) (cons `(orig ,orig-selected ,highlight-func) (cdr match))))))
;;     match))

(provide 'config-consult)
