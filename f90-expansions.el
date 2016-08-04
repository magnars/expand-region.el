;;; f90-expansions.el --- f90-mode expansions for expand-region

;; Copyright (C) 2015 Johan S Hysing

;; Author: Johan S Hysing
;; Keywords: marking region

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

;; Feel free to contribute any other expansions for Fortran 90 at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/f90-mark-block ()
  "Mark the current f90 block."
  (interactive)
  (f90-beginning-of-block)
  (set-mark (point))
  (f90-end-of-block)
  (exchange-point-and-mark))

(defun er/add-f90-expansions ()
  "Adds f90-specific expansions for buffers in f90-mode"
    (set (make-local-variable 'er/try-expand-list)
         (append er/try-expand-list '(er/f90-mark-block))))

(er/enable-mode-expansions 'f90-mode 'er/add-f90-expansions)

(provide 'f90-expansions)
;;; f90-expansions.el ends here
