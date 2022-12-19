;;; compile-eslint.el --- compilation-error-regexp-alist rules for yarn-workspaces -*- lexical-binding: t -*-

;; Copyright (C) 2020 Victor Rybynok

;; Author: Victor Rybynok <v.rybynok@gmail.com>
;; Maintainer: Victor Rybynok <v.rybynok@gmail.com>
;; Version: 0.0.1
;; Created: 2020-01-10
;; Keywords: convenience, languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Rules to match yarn-workspaces output in `compilation-mode'.

;;; Code:

(require 'compile)

(let ((form '(yarn-workspaces-typecheck
              "^\\[ error \\] ERROR in \\(\\(.*\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\))\\):$"
              2 3 4 2 1)))
  (if (assq 'yarn-workspaces-typecheck compilation-error-regexp-alist-alist)
      (setf (cdr (assq 'yarn-workspaces-typecheck compilation-error-regexp-alist-alist)) (cdr form))
    (push form compilation-error-regexp-alist-alist)))

(let ((form '(yarn-workspaces-compiling
              "\\[ error ].*\n.*[eE]rror: \\(\\(.+\\): .+(\\([[:digit:]]+\\):\\([[:digit:]]\\)+)\\)"
              2 3 4 2 1)))
  (if (assq 'yarn-workspaces-compiling compilation-error-regexp-alist-alist)
      (setf (cdr (assq 'yarn-workspaces-compiling compilation-error-regexp-alist-alist)) (cdr form))
    (push form compilation-error-regexp-alist-alist)))

(provide 'compile-yarn-workspaces)
;;; compile-yarn-workspaces.el ends here
