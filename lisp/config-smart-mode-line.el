;;; ramblehead's smart-mode-line configuration

(defun rh-sml/setup (&optional arg)
  "Setup the mode-line to be smart and sexy.

ARG is ignored. Just call this function in your init file, and
the mode-line will be setup."
  (interactive)
  (sml/-debug "Entering setup")
  (sml/-debug custom-enabled-themes)

  ;; Just a couple of useful variables
  (setq sml/simplified nil)
  (setq battery-mode-line-format sml/battery-format)

  ;; Activate rich-minority, and configure it for us.
  (setq rm-base-text-properties
        (append rm-base-text-properties '('face 'sml/minor-modes)))

  ;; Set the theme the user requested.
  (sml/-setup-theme)

  ;;;; And this is where the magic happens.
  ;; Remove elements we implement separately, and improve the ones not removed.
  (sml/filter-mode-line-list 'mode-line-mule-info)
  (setq-default mode-line-client sml/mode-line-client)
  (sml/filter-mode-line-list 'mode-line-modified)
  (sml/filter-mode-line-list 'mode-line-remote)
  (setq-default mode-line-frame-identification
                '("" (sml/show-frame-identification "%F")
                  sml/pre-id-separator))

  ;; (setq-default mode-line-buffer-identification '("%b"))

  ;; (setq-default mode-line-buffer-identification
  ;;               sml/mode-line-buffer-identification)
  (sml/filter-mode-line-list 'mode-line-position)
  (sml/filter-mode-line-list 'mode-line-modes)
  (setq-default mode-line-end-spaces nil)

  ;; Add position descriptions on the left (they were already removed
  ;; from the middle). Since this is the very first symbol to be
  ;; evaluated, we also use it for calculating variables that need to
  ;; be updated
  (setq-default mode-line-front-space '((:eval (sml/generate-buffer-identification-if-necessary))
                                        (sml/position-help-text
                                         nil
                                         (:eval (let ((sml/-this-buffer-changed-p t))
                                                  (sml/generate-position-help))))
                                        (sml/position-construct
                                         sml/position-construct
                                         (:eval (sml/compile-position-construct)))))

  (add-hook 'after-save-hook 'sml/generate-buffer-identification)
  (ad-activate 'rename-buffer)
  (ad-activate 'set-visited-file-name)
  (add-hook 'clone-indirect-buffer-hook 'sml/generate-buffer-identification)
  ;; (ad-activate 'set-buffer-modified-p)
  (add-hook 'after-change-functions 'sml/-this-buffer-changed)
  (add-hook 'post-command-hook 'sml/generate-position-help)

  ;; This is to ensure fixed name width. The reason we do this manually
  ;; is that some major-modes change `mode-line-buffer-identification'
  ;; (so we can't fill inside the variable), and we want this
  ;; symbol to be an element in `mode-line-format' for compatibility
  ;; with other packages which hack into the mode-line.

  (add-to-list 'mode-line-position
               '(sml/buffer-identification-filling
                 sml/buffer-identification-filling
                 (:eval (setq sml/buffer-identification-filling
                              (sml/fill-for-buffer-identification)))))

  ;; Remove some annoying big spaces
  (setq-default mode-line-format
                (mapcar
                 (lambda (x) (cond
                         ;; ((eq x 'mode-line-buffer-identification)
                         ;;  '(:propertize mode-line-buffer-identification face sml/id))
                         ((and (stringp x) (string= x "   "))
                          'sml/pos-id-separator)
                         ((and (stringp x) (string= x "  "))
                          'sml/pre-modes-separator)
                         (t x)))
                 mode-line-format))

      ;;;; And here comes support for a bunch of extra stuff. Some of
      ;;;; these are just needed for coloring.

  ;; Shell and eshell support
  (add-hook 'comint-output-filter-functions 'sml/generate-buffer-identification)
  (add-hook 'eshell-directory-change-hook 'sml/generate-buffer-identification)

  ;; ;; Term support - Disabled for now because of Issue#198
  ;; (defadvice term-command-hook (after sml/term-advice-1 activate)
  ;;   (sml/generate-buffer-identification))

  ;; (defadvice term-handle-ansi-terminal-messages (after sml/term-advice-2 activate)
  ;;   (sml/generate-buffer-identification))

  ;; Dired overrides the buffer-identification (which we would
  ;; normally respect) but doesn't actually do anything useful with
  ;; it, so we overoverride back.
  (add-hook 'dired-mode-hook 'sml/set-buffer-identification)

  ;; Display time
  (add-hook 'display-time-hook 'sml/propertize-time-string)

  ;; Battery support
  (eval-after-load 'battery
    '(defadvice battery-update (after sml/after-battery-update-advice () activate)
       "Change battery color."
       (when battery-mode-line-string
         (setq battery-mode-line-string
               (propertize battery-mode-line-string
                           'face 'sml/battery)))))

  ;; Projectile support
  (eval-after-load "projectile"
    '(progn
       (setq sml/projectile-loaded-p t)
       (defcustom sml/projectile-replacement-format "[%s]"
         "Format used for replacements derived from projectile."
         :type 'string
         :group 'smart-mode-line-others
         :package-version '(smart-mode-line . "2.4"))
       (defcustom sml/use-projectile-p 'after-prefixes
         "Whether we should use projectile to guess path prefixes.

If this is non-nil, and if current buffer is inside a project (as
defined by projectile), we use the project's name as a
prefix (with the `sml/projectile-replacement-format' variable).

If this is 'after-prefix, then this replacement will only be used
if no other prefixes (defined in `sml/replacer-regexp-list') were
found to match the current file path."
         :type '(choice (const :tag "Use projectile only if current path doesn't match any prefixes." after-prefixes)
                        (const :tag "Use projectile before checking prefixes." before-prefixes)
                        (const :tag "Don't use projectile." nil))
         :group 'smart-mode-line-others
         :package-version '(smart-mode-line . "2.4.1"))
       (defface sml/projectile '((t :inherit sml/git)) "" :group 'smart-mode-line-faces)
       (add-to-list 'sml/prefix-regexp (format (regexp-quote sml/projectile-replacement-format) ".*"))))

  ;; vc-mode
  (eval-after-load "vc-hooks"
    '(defadvice vc-mode-line (after sml/after-vc-mode-line-advice () activate)
       "Color `vc-mode'."
       (when (stringp vc-mode)
         (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
           (setq vc-mode
                 (propertize (if sml/vc-mode-show-backend vc-mode noback)
                             'face (cond ((string-match "^ -" noback)    'sml/vc)
                                         ((string-match "^ [:@]" noback) 'sml/vc-edited)
                                         ((string-match "^ [!\\?]" noback) 'sml/modified))))))))

  ;; Mew support
  (eval-after-load "mew-net"
    '(progn
       (defgroup smart-mode-line-mew '() "Group for editing the mew-support variables." :group 'smart-mode-line)
       (defcustom sml/mew-support t
         "Whether to flash the mode-line when mew detects new mail."
         :type 'boolean :group 'smart-mode-line-mew
         :package-version '(smart-mode-line . "1.11"))
       (defcustom sml/new-mail-background-color "#110000"
         "When new mail arrives, mode-line background will be tinted this color.

Only works with mew-biff. Right now it stays colored until you
read the mail, so this color should probably be something sutil.
Might implement a quick flash eventually."
         :type 'color :group 'smart-mode-line-mew
         :package-version '(smart-mode-line . "1.11"))
       (defcustom sml/mew-biff-format (concat "%2d" (if (char-displayable-p ?✉) "✉" "M"))
         "Format used for new-mail notifications if you use mew with biff."
         :type 'string :group 'smart-mode-line-mew
         :package-version '(smart-mode-line . "1.11"))
       (defadvice mew-biff-clear (around sml/mew-biff-clear-advice activate)
         "Advice used to customize mew-biff-bark to fit sml's style."
         ad-do-it
         (when sml/mew-support
           ;; Remove the color
           (set-face-attribute 'mode-line nil :background sml/active-background-color)))
       (defadvice mew-biff-bark (around sml/mew-biff-bark-advice (n) activate)
         "Advice used to customize mew-biff-bark to fit sml's style."
         ad-do-it
         (when sml/mew-support
           ;; Remove the color if mail has been read.
           (if (= n 0) (set-face-attribute 'mode-line nil :background sml/active-background-color)
             ;; Apply color if there's mail. (mew-biff-bark 100)
             (set-face-attribute 'mode-line nil :background sml/new-mail-background-color)
             (setq mew-biff-string (format sml/mew-biff-format n)))))))

  (unless (and (boundp 'erc-track-position-in-mode-line)
               (null erc-track-position-in-mode-line))
    (setq erc-track-position-in-mode-line t))

  (run-hooks 'sml/after-setup-hook))

(eval-after-load "vc-hooks"
  '(defadvice vc-mode-line (after rh-vc-mode-line () activate)
     (when (stringp vc-mode)
       (let ((text-properties (text-properties-at 1 vc-mode))
             (noback
              (replace-regexp-in-string
               (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
         (when (> (string-width noback) 20)
           (let (vc-mode-truncation-string noback-beg noback-end help-echo)
             (setq help-echo (plist-get text-properties 'help-echo))
             (setq help-echo (split-string help-echo "\n"))
             (push "" help-echo)
             (push (substring noback 1) help-echo)
             (setq help-echo (string-join help-echo "\n"))
             (plist-put text-properties 'help-echo help-echo)
             (setq vc-mode-truncation-string
                   (if (char-displayable-p ?…) "…" "..."))
             (setq noback-beg (substring noback 0 14))
             (setq noback-end (substring noback -5))
             (setq noback (concat noback-beg
                                  vc-mode-truncation-string
                                  noback-end))
             (add-text-properties 1 (length noback) text-properties noback)))
         (setq vc-mode
               (propertize
                (if sml/vc-mode-show-backend vc-mode noback)
                'face
                (cond ((string-match "^ -" noback) 'sml/vc)
                      ((string-match "^ [:@]" noback) 'sml/vc-edited)
                      ((string-match "^ [!\\?]" noback) 'sml/modified))))))))

(provide 'config-smart-mode-line)
