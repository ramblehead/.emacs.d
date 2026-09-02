;;; early-init.el --- ramblehead's emacs configuration  -*- lexical-binding: t; -*-
;;
;; Author: Victor Rybynok
;; Copyright (C) 2019-2023, Victor Rybynok, all rights reserved.

;; (advice-add
;;  'display-warning
;;  :before
;;  (lambda (type message &rest _)
;;    (when (string-match-p
;;           "Missing ‘lexical-binding’ cookie in"
;;           message)
;;      (debug))))

;; Disable package.el in favor of straight.el and manual init
(customize-set-value 'package-enable-at-startup nil)
(setenv "LSP_USE_PLISTS" "true")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: nil
;; no-update-autoloads: t
;; End:
