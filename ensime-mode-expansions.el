;;; ensime-mode-expansions.el --- Scala expansions for Expand Region  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Sebastian Wiesner

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds expansions for Scala Mode.

;;; Code:

(require 'expand-region-core)

(defun er/mark-ensime-syntactic-context ()
  "Mark the next outer syntactic context."
  (when (and (fboundp 'ensime-connected-p)
             (ensime-connected-p)
             (fboundp 'ensime-expand-selection))
    (ensime-expand-selection (mark) (point))))

(defun er/add-ensime-mode-expansions ()
  "Enable Ensime Mode expansions."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(er/mark-ensime-syntactic-context))))

(er/enable-mode-expansions 'ensime-mode 'er/add-ensime-mode-expansions)

(provide 'ensime-mode-expansions)

;;; ensime-mode-expansions.el ends here
