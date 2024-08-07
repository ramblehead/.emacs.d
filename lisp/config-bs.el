;;; rh-bs.el --- set of bs module extensions
;;
;; Description: Quickly switch between buffer configurations.
;; Author: Victor Rybynok
;; Copyright (C) 2019-2024, Victor Rybynok, all rights reserved.

;; (require 'compile)
(require 'ace-window)
(require 'rh-buffers)

;; -------------------------------------------------------------------
;;; bs patches
;; -------------------------------------------------------------------
;; /b/{

(setq bs--intern-show-never "\\*buffer-selection\\*")

(defun rh-bs-buffer-list (&optional list sort-description)
  "Return a list of buffers to be shown.  LIST is a list of buffers to test for
appearance in Buffer Selection Menu.  The result list depends on the global
variables `bs-dont-show-regexp', `bs-must-show-regexp', `bs-dont-show-function',
`bs-must-show-function' and `bs-buffer-sort-function'.  If SORT-DESCRIPTION
isn't nil the list will be sorted by a special function.  SORT-DESCRIPTION is an
element of `bs-sort-functions'.

rh had to patch this funtion to remove current buffer from the configs which
originally do not list it."
  (setq sort-description (or sort-description bs--current-sort-function)
	list (or list (buffer-list)))
  (let ((result nil))
    (dolist (buf list)
      (let* ((buffername (buffer-name buf))
	     (int-show-never (string-match-p bs--intern-show-never buffername))
	     (ext-show-never (and bs-dont-show-regexp
				  (string-match-p bs-dont-show-regexp
					          buffername)))
	     (extern-must-show (or (and bs-must-always-show-regexp
					(string-match-p
					 bs-must-always-show-regexp
					 buffername))
				   (and bs-must-show-regexp
					(string-match-p bs-must-show-regexp
							buffername))))
	     (extern-show-never-from-fun (and bs-dont-show-function
					      (funcall bs-dont-show-function
						       buf)))
	     (extern-must-show-from-fun (and bs-must-show-function
					     (funcall bs-must-show-function
						      buf)))
	     (show-flag (buffer-local-value 'bs-buffer-show-mark buf)))
	(when (or (eq show-flag 'always)
		  (and (or bs--show-all (not (eq show-flag 'never)))
		       (not int-show-never)
		       (or bs--show-all
			   extern-must-show
			   extern-must-show-from-fun
			   (and (not ext-show-never)
				(not extern-show-never-from-fun)))))
	  (setq result (cons buf result)))))
    (setq result (reverse result))
    ;; ***** ramblehead does not agree with the following statement as
    ;;       the current buffer should respect bs-configurations - the same as
    ;;       all other buffers - no exceptions!
    ;; The current buffer which was the start point of bs should be an element
    ;; of result list, so that we can leave with space and be back in the
    ;; buffer we started bs-show.
    ;; (when (and bs--buffer-coming-from
    ;;            (buffer-live-p bs--buffer-coming-from)
    ;;            (not (memq bs--buffer-coming-from result)))
    ;;   (setq result (cons bs--buffer-coming-from result)))
    ;; sorting
    (if (and sort-description
	     (nth 1 sort-description))
	(setq result (sort result (nth 1 sort-description)))
      ;; else standard sorting
      (bs-buffer-sort result))))

(advice-add 'bs-buffer-list :override
            #'rh-bs-buffer-list)

(defun rh-bs--set-window-height (orig-fun) nil)

(advice-add 'bs--set-window-height :around
            #'rh-bs--set-window-height)

(defun rh-bs--get-file-name (orig-fun _start-buffer _all-buffers)
  (abbreviate-file-name (funcall orig-fun _start-buffer _all-buffers)))

(advice-add 'bs--get-file-name :around
            #'rh-bs--get-file-name)

;; see http://www.warmenhoven.org/src/emacs.el/ew-buffer.el.html
(defun rh-bs--get-size-string (&rest ignored)
  (let* ((size (buffer-size))
         (str (number-to-string size)))
    (when (> (length str) 3)
      (setq size (/ size 1024.0)
            str (format "%.1fk" size)))
    (when (> (length str) 6)
      (setq size (/ size 1024.0)
            str (format "%.1fM" size)))
    (when (> (length str) 6)
      (setq size (/ size 1024.0)
            str (format "%.1fG" size)))
    str))

(defun rh-bs--get-buffer-process (&rest ignored)
  (if (get-buffer-process (current-buffer)) "ⵛ" " "))

;; /b/}

;; -------------------------------------------------------------------
;;; bs settings alteration
;; -------------------------------------------------------------------
;; /b/{

(customize-set-value
 'bs-attributes-list
 '(("" 2 2 left bs--get-marked-string)
   ("M" 1 1 left bs--get-modified-string)
   ("R" 2 2 left bs--get-readonly-string)
   ("Size" 6 6 right rh-bs--get-size-string)
   ("" 2 2 left "  ")
   ("Mode" 16 16 left bs--get-mode-name)
   ("" 2 2 left "  ")
   ("P" 1 1 left rh-bs--get-buffer-process)
   ("" 1 1 left " ")
   ("Buffer" bs--get-name-length 100 left bs--get-name)
   ("" 2 2 left "  ")
   ("File" 1 255 left bs--get-file-name)))

(let ((map bs-mode-map))
  (define-key map (kbd "<") #'rh-bs-select-previous-configuration)
  (define-key map (kbd ">") #'rh-bs-select-next-configuration)
  (define-key map (kbd "M-<return>") #'rh-bs-tmp-ace-select-other-window)
  (define-key map (kbd "M-<kp-enter>") #'rh-bs-tmp-ace-select-other-window)
  (define-key map (kbd "C-<return>") #'rh-bs-ace-select-other-window)
  (define-key map (kbd "C-<kp-enter>") #'rh-bs-ace-select-other-window)
  (define-key map (kbd "S-<return>") #'rh-bs-tmp-select-bottom-0-side-window)
  (define-key map (kbd "S-<kp-enter>") #'rh-bs-tmp-select-bottom-0-side-window)
  (define-key map (kbd "S-C-<return>") #'rh-bs-select-bottom-0-side-window)
  (define-key map (kbd "S-C-<kp-enter>") #'rh-bs-select-bottom-0-side-window)
  (define-key map (kbd "A") #'rh-bs-display-all)
  (define-key map (kbd "M") #'rh-bs-display-marked)
  (define-key map (kbd "q") #'rh-bs-bury-buffer-and-delete-window-if-bottom-0-side)
  (define-key map (kbd "M-q") #'rh-bs-kill-buffer-and-delete-window-if-bottom-0-side))

;; /b/}

;; -------------------------------------------------------------------
;;; rh-bs implementation
;; -------------------------------------------------------------------
;; /b/{

(defvar rh-bs--interactively-selected-window nil)

(add-hook
 'buffer-list-update-hook
 (lambda ()
   (setq rh-bs--interactively-selected-window (frame-selected-window))))

(defun rh-bs--window-selected-interactively-p ()
  (eq (selected-window) rh-bs--interactively-selected-window))

(defvar rh-bs-side-window-height 15)

(defvar rh-bs-bottom-0-side-window-buffer
  nil
  "Last buffer displayed in bottom slot 0 side window.")

(defvar rh-bs-header-line-trim-indicator "›")

(defface rh-bs-current-config-face
  '((t :inherit mode-line :weight bold))
  "Face used for the current bs-configuration name
when bs window is active."
  :group 'bs)

(defface rh-bs-current-config-face-inactive
  '((t :inherit mode-line-inactive :weight bold))
  "Face used for the current bs-configuration name
when bs window is inactive."
  :group 'bs)

(defface rh-bs-other-config-face
  '((t :inherit mode-line))
  "Face used for a non-current bs-configuration name
when bs window is active."
  :group 'bs)

(defface rh-bs-other-config-face-inactive
  '((t :inherit mode-line-inactive))
  "Face used for a non-current bs-configuration name
when bs window is inactive."
  :group 'bs)

(defcustom rh-bs-other-config-template
  "%s"
  "String template for displaying other bs-configurations.

This is the template string that will be applied to a non-current
bs-configuration name. Use string `%s' to refer to the bs-configuration
name."
  :group 'bs)

(defcustom rh-bs-current-config-template
  "[%s]"
  "String template for displaying the current bs-configuration.

This is the template string that will be applied to the current
bs-configuration name. Use string `%s' to refer to the bs-configuration
name."
  :group 'bs)

(defun rh--bs-skip-header-lines ()
  (when (< (count-lines 1 (point)) bs-header-lines-length)
    (goto-line (1+ bs-header-lines-length))))

(defun rh-bs-show (arg)
  (interactive "P")
  (let ((current-window (frame-selected-window)))
    (setq bs--buffer-coming-from (current-buffer))
    (switch-to-buffer "*buffer-selection*" t t)
    (bs-show arg)
    (select-window current-window)
    (rh--bs-skip-header-lines)))

;; (defun rh-bs-refresh-if-visible ()
;;   (interactive)
;;   (walk-windows
;;    (lambda (window)
;;      (with-current-buffer (window-buffer window)
;;        (when (eq major-mode 'bs-mode)
;;          (with-selected-window window
;;            (bs-refresh)
;;            (rh--bs-skip-header-lines)))))
;;    nil t))

(defun rh-bs-refresh-all ()
  (interactive)
  (let ((buffers (buffer-list)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'bs-mode)
          (bs-refresh))))))

(defun rh-bs-show-bs-in-bottom-0-side-window (&optional configuration-name)
  (interactive)
  (unless configuration-name
    (setq configuration-name bs-default-configuration))
  (setq bs--buffer-coming-from (current-buffer))
  (select-window
   (rh-bs-display-buffer-in-botom-0-side-window "*buffer-selection*" t))
  (bs-set-configuration configuration-name)
  (setq bs-default-configuration bs-current-configuration)
  (setq bs--marked-buffers nil)
  (bs--show-with-configuration configuration-name)
  (select-window (rh-bs-get-botom-0-side-window))
  (rh--bs-skip-header-lines))

(defun rh-bs-toggle-bs-in-bottom-0-side-window (&optional configuration-name)
  (interactive)
  (let* ((botom-0-side-window (rh-bs-get-botom-0-side-window))
         (bootom-0-side-buffer-major-mode
          (when bootom-0-side-window
            (with-current-buffer (window-buffer bootom-0-side-window)
              major-mode))))
    (if (eq bootom-0-side-buffer-major-mode 'bs-mode)
        (rh-bs-reopen-bottom-0-side-window)
      (rh-bs-show-bs-in-bottom-0-side-window configuration-name))))

(defun rh-bs-make-configuration-from-buffer-group (buffer-group-name)
  `(,buffer-group-name
    nil nil nil
    (lambda (buffer)
      (not (rh-buffers-match
            (car (cdr
                  (seq-find
                   (lambda (buffer-group)
                     (string= (car buffer-group) ,buffer-group-name))
                   rh-buffers-groups)))
            buffer)))
    rh-bs-sort-by-file-path-interns-are-last))

(defun rh-bs-sort-by-file-path-interns-are-last (b1 b2)
  (let* ((b1-buffer-name (buffer-name b1))
         (b2-buffer-name (buffer-name b2))
         (b1-intern-p (string-match-p "^\\*" b1-buffer-name))
         (b2-intern-p (string-match-p "^\\*" b2-buffer-name))
         (b1-file-name (with-current-buffer b1
                         (bs--get-file-name nil nil)))
         (b2-file-name (with-current-buffer b2
                         (bs--get-file-name nil nil))))
    (setq b1-buffer-name (when b1-buffer-name (downcase b1-buffer-name)))
    (setq b2-buffer-name (when b2-buffer-name (downcase b2-buffer-name)))
    (setq b1-file-name (when b1-file-name (downcase b1-file-name)))
    (setq b2-file-name (when b2-file-name (downcase b2-file-name)))
    (if (and (not (string-empty-p b1-file-name))
             (not (string-empty-p b2-file-name)))
        (if (string= b1-file-name b2-file-name)
            (string< b1-buffer-name b2-buffer-name)
          (string< b1-file-name b2-file-name))
      (if (and (string-empty-p b1-file-name)
               (string-empty-p b2-file-name))
          (if (string= b2-buffer-name "*scratch*") nil
            (string< b1-buffer-name b2-buffer-name))
        (string-empty-p b2-file-name)))))

(defun rh-bs-ace-select-other-window ()
  (interactive)
  (let* ((bs-window (frame-selected-window))
         (target-buffer (bs--current-buffer)))
    (ace-select-window)
    (switch-to-buffer target-buffer)
    (delete-window bs-window)))

(defun rh-bs-tmp-ace-select-other-window ()
  (interactive)
  (with-selected-window (frame-selected-window)
    (let ((buffer (bs--current-buffer)))
      (ace-select-window)
      (switch-to-buffer buffer))))

(defun rh-bs-display-buffer-in-botom-0-side-window
    (buffer-or-name &optional do-not-set-bottom-0-side-window)
  (let ((buffer (get-buffer-create buffer-or-name)))
    (unless do-not-set-bottom-0-side-window
      (setq rh-bs-bottom-0-side-window-buffer buffer))
    (display-buffer-in-side-window
     buffer
     `((side . bottom)
       (slot . 0)
       (inhibit-same-window . t)
       (window-height . ,rh-bs-side-window-height)))))

(defun rh-bs-get-botom-0-side-window ()
  (let ((windows (window-list)))
    (seq-find
     (lambda (window)
       (and (eq (window-parameter window 'window-side) 'bottom)
            (eq (window-parameter window 'window-slot) 0)))
     windows)))

(defun rh-bs-window-bootom-0-side-p (window)
  (and (eq (window-parameter window 'window-side) 'bottom)
       (eq (window-parameter window 'window-slot) 0)))

(defun rh-bs-tmp-select-bottom-0-side-window ()
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (rh-bs-display-buffer-in-botom-0-side-window buffer)))

(defun rh-bs-select-bottom-0-side-window ()
  (interactive)
  (let ((buffer (bs--current-buffer)))
    (select-window
     (rh-bs-display-buffer-in-botom-0-side-window buffer))))

(defun rh-bs-delete-bottom-0-side-window ()
  (interactive)
  (let ((side-window (rh-bs-get-botom-0-side-window)))
    (when side-window (delete-window side-window))))

(defun rh-bs-reopen-bottom-0-side-window ()
  (interactive)
  (if (buffer-live-p rh-bs-bottom-0-side-window-buffer)
      (select-window
       (rh-bs-display-buffer-in-botom-0-side-window
        rh-bs-bottom-0-side-window-buffer))
    (rh-bs-show-bs-in-bottom-0-side-window)))

(defun rh-bs-tmp-reopen-bottom-0-side-window ()
  (interactive)
  (if (buffer-live-p rh-bs-bottom-0-side-window-buffer)
      (select-window
       (rh-bs-display-buffer-in-botom-0-side-window
        rh-bs-bottom-0-side-window-buffer))
    (rh-bs-show-bs-in-bottom-0-side-window)))

(defun rh-bs-tmp-toggle-bottom-0-side-window ()
  (interactive)
  (let ((side-window (rh-bs-get-botom-0-side-window)))
    (if side-window
        (rh-bs-delete-bottom-0-side-window)
      (rh-bs-tmp-reopen-bottom-0-side-window))))

(defun rh-bs-kill-buffer-and-delete-window-if-bottom-0-side ()
  (interactive)
  (kill-buffer)
  (let ((window (frame-selected-window)))
    (when (and (eq (window-parameter window 'window-side) 'bottom)
               (eq (window-parameter window 'window-slot) 0))
      (delete-window))))

(defun rh-bs-bury-buffer-and-delete-window-if-bottom-0-side ()
  (interactive)
  (bury-buffer)
    (let ((window (frame-selected-window)))
    (when (and (eq (window-parameter window 'window-side) 'bottom)
               (eq (window-parameter window 'window-slot) 0))
      (delete-window))))

(defun rh-bs-make-header-string ()
  (let* ((active (rh-bs--window-selected-interactively-p))
         (face (if active
                   'rh-bs-other-config-face
                 'rh-bs-other-config-face-inactive))
         (current-face (if active
                           'rh-bs-current-config-face
                         'rh-bs-current-config-face-inactive)))
    (concat
     (propertize " " 'face face)
     (mapconcat
      (lambda (conf)
        (let ((name (car conf)))
          name
          (if (string= name bs-current-configuration)
              (format
               (propertize rh-bs-current-config-template 'face face)
               (propertize name 'face current-face))
            (format
             (propertize rh-bs-other-config-template 'face face)
             (propertize name 'face face)))))
      (seq-filter
       (lambda (conf)
         (let ((name (car conf)))
           (or (string= name bs-current-configuration)
               (not (rh-bs-buffer-list-empty-p conf)))))
       bs-configurations)
      (propertize " " 'face face)))))

(defun rh-bs-header-line ()
  (let* ((header-string (rh-bs-make-header-string))
         (header-string-width (string-width header-string))
         (header-filler-width (- (window-total-width) header-string-width)))
    (if (< header-filler-width 0)
        (concat
         (substring header-string
                    0 (- (window-total-width)
                         (string-width rh-bs-header-line-trim-indicator)))
         rh-bs-header-line-trim-indicator)
      (concat header-string
              (propertize
               (make-string header-filler-width ?\ )
               'face (if (rh-bs--window-selected-interactively-p)
                         'mode-line
                       'mode-line-inactive))))))

(defun rh-bs-mode-line ()
  (concat
   " "
   (number-to-string (length (bs-buffer-list)))
   " buffers"))

(defun rh-bs-buffer-list-empty-p (conf)
  (let ((bs-current-configuration (nth 0 conf))
        (bs-must-show-regexp      (nth 1 conf))
        (bs-must-show-function    (nth 2 conf))
        (bs-dont-show-regexp      (nth 3 conf))
        (bs-dont-show-function    (nth 4 conf))
        (bs-buffer-sort-function  (nth 5 conf)))
    (null (bs-buffer-list))))

(defun rh-bs-prev-config-aux (start-name list)
  "Get the previous assoc before START-NAME in list LIST.
Will return the last if START-NAME is at start."
  (let ((assocs list)
	(length (length list))
	pos)
    (while (and assocs (not pos))
      (when (string= (car (car assocs)) start-name)
	(setq pos (- length (length assocs))))
      (setq assocs (cdr assocs)))
    (if (eq pos 0)
	(nth (1- length) list)
      (nth (1- pos) list))))

(defun rh-bs-prev-config (name)
  "Return previous configuration with respect to configuration with name NAME."
  (rh-bs-prev-config-aux name bs-configurations))

(defun rh-bs-select-previous-configuration (&optional start-name)
  "Apply previous configuration to START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let* ((conf-first
          (rh-bs-prev-config (or start-name bs-current-configuration)))
         (conf conf-first)
         (check-prev t))
    (while (and (rh-bs-buffer-list-empty-p conf) check-prev)
      (setq conf (rh-bs-prev-config (car conf)))
      (when (eq conf conf-first)
        (setq check-prev nil)
        (setq conf bs-current-configuration)))
    (bs-set-configuration (car conf))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car conf))))

(defun rh-bs-select-next-configuration (&optional start-name)
  "Apply next configuration START-NAME and refresh buffer list.
If START-NAME is nil the current configuration `bs-current-configuration'
will be used."
  (interactive)
  (let* ((conf-first
          (bs-next-config (or start-name bs-current-configuration)))
         (conf conf-first)
         (check-next t))
    (while (and (rh-bs-buffer-list-empty-p conf) check-next)
      (setq conf (bs-next-config (car conf)))
      (when (eq conf conf-first)
        (setq check-next nil)
        (setq conf bs-current-configuration)))
    (bs-set-configuration (car conf))
    (setq bs-default-configuration bs-current-configuration)
    (bs--redisplay t)
    (bs--set-window-height)
    (bs-message-without-log "Selected configuration: %s" (car conf))))

(defun rh-bs-split-window-n (window count)
  (with-selected-window (frame-selected-window)
    (let ((first-window window)
          (windows '())
          (n (1- count)))
      (dotimes (i n)
        (setq window (split-window))
        (balance-windows (window-parent window))
        (push window windows))
      (push first-window windows)
      windows)))

(defun rh-bs-delete-sibling-windows (&optional window predicate)
  (interactive)
  (let* ((window (or window
                     (frame-selected-window)))
         (first (window-child (window-parent window)))
         (next (window-next-sibling first))
         windows)
    (while next
      (if predicate
          (when (funcall predicate next)
            (push next windows))
        (push next windows))
      (setq next (window-next-sibling next)))
    (dolist (window windows)
      (delete-window window))
    (when (window-live-p first)
      (switch-to-prev-buffer first))))

(defun rh-bs-ace-select-display-multiple (buffers)
  (let* ((count (length buffers))
         windows)
    (ace-select-window)
    (setq windows
          (rh-bs-split-window-n (frame-selected-window) count))
    (dotimes (i count)
      (set-window-buffer (nth i windows) (nth i buffers)))))

(defun rh-bs-display-all ()
  (interactive)
  (let ((orig-window (frame-selected-window))
        (buffers (bs-buffer-list)))
    (when buffers
      (rh-bs-ace-select-display-multiple buffers)
      (when (string= (buffer-name (window-buffer orig-window))
	             "*buffer-selection*")
        (with-selected-window orig-window
          (rh-bs-bury-buffer-and-delete-window-if-bottom-0-side))))))

(defun rh-bs-display-marked ()
  (interactive)
  (let ((orig-window (frame-selected-window)))
    (when bs--marked-buffers
      (setq bs--marked-buffers
            (sort bs--marked-buffers #'rh-bs-sort-by-file-path-interns-are-last))
      (rh-bs-ace-select-display-multiple bs--marked-buffers)
      (when (string= (buffer-name (window-buffer orig-window))
	             "*buffer-selection*")
        (with-selected-window orig-window
          (rh-bs-bury-buffer-and-delete-window-if-bottom-0-side))))))

(defun rh-bs-mode-and-header-lines-setup ()
  (setq-local mode-line-format '(:eval (rh-bs-mode-line)))
  (setq-local header-line-format '(:eval (rh-bs-header-line))))

(add-hook
 'bs-mode-hook
 #'rh-bs-mode-and-header-lines-setup)

;; /b/}

(provide 'config-bs)
