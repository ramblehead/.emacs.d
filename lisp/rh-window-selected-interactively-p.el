;;; rh-window-selected-interactively-p.el --- windows selected interactively predicate function

;; Copyright (C) 2018 Victor Rybynok

;; Author: Victor Rybynok
;; URL: non yet
;; Package-Version: 20180430
;; Version: 0.0.0
;; Keywords: convenience c++ rtags
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
;; This module provides (rh-window-selected-interactively-p) - windows selected
;; interactively predicate function predicate function.
;;
;;; Code:

(defvar rh-interactively-selected-window nil)

(defun rh-window-selected-interactively-p ()
  (eq (selected-window) rh-interactively-selected-window))

(defun rh-update-interactively-selected-window ()
  (setq rh-interactively-selected-window (frame-selected-window)))

(add-hook 'buffer-list-update-hook #'rh-update-interactively-selected-window)

(provide 'rh-window-selected-interactively-p)
;;; rh-window-selected-interactively-p.el ends here
