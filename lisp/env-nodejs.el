;;; env-nodejs.el --- ramblehead's nodejs environment

;; Copyright (C) 2018 Victor Rybynok

;; Author: Victor Rybynok
;; URL: non yet
;; Package-Version: 20181214
;; Version: 0.0.0
;; Keywords: node nodejs repl
;; Package-Requires: ((emacs "26"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; nodejs-repl provides environment for node REPL interaction
;; this packet customises its functionality
;;
;;; Code:

(defun rh-nodejs-repl-send-line-or-region (start end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point))))
  (if (/= start end)
      (nodejs-repl-send-region start end)
    (nodejs-repl-send-line)))

;;;###autoload
(defun rh-nodejs-repl ()
  (interactive)
  (message "xxxx"))

;; ;;;###autoload
;; (cl-defun rh-nodejs-repl (&optional (value nil value-supplied-p))
;;   (interactive)
;;   (message "xxxx"))

;; ;;;###autoload
;; (cl-defun rh-nodejs-repl (&optional (value nil value-supplied-p))
;;   (interactive)
;;   (let ((enable))
;;     (if (null value-supplied-p)
;;         (setq enable (if (bound-and-true-p rh-nodejs-repl) -1 1))
;;       (setq enable (if (eq value 1) 1 -1)))
;;     (if (eq enable 1)
;;         (progn
;;           (setq rh-nodejs-repl t)
;;           (local-set-key (kbd "<f5>") #'rh-nodejs-repl-send-line-or-region)
;;           (message "nodejs-repl enabled"))
;;       (progn
;;         (setq rh-nodejs-repl nil)
;;         (local-unset-key (kbd "<f5>"))
;;         (message "nodejs-repl disabled")))))

(provide 'env-nodejs)
;;; env-nodejs.el ends here
