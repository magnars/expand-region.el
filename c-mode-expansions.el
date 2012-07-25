;;; c-mode-expansions.el --- C-specific expansions for expand-region

;; Copyright (C) 2012 François Févotte

;; Author: François Févotte
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
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
;;
;; Extra expansions for C-like modes that I've found useful so far:
;;
;;     c-mark-function
;;     er/c-mark-statement
;;     er/c-mark-fully-qualified-name
;;     er/c-mark-next-block
;;     er/c-mark-prev-statement
;;
;; Feel free to contribute any other expansions for C at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/c-mark-statement ()
  "Mark the current C statement."
  (interactive)
  (c-end-of-statement 1)
  (set-mark (point))
  (c-beginning-of-statement 1))

(defun er/c-mark-fully-qualified-name ()
  "Mark the current C++ fully qualified name (i.e. with namespaces)."
  (interactive)
  (when (use-region-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (while (looking-back "::")
      (backward-char 2)
      (skip-syntax-backward "_w"))
    (exchange-point-and-mark)
    (while (looking-at "::")
      (forward-char 2)
      (skip-syntax-forward "_w"))
    (exchange-point-and-mark)))

(defun er/c-mark-next-block ()
  "Mark the current statement and the following block."
  (interactive)
  (when (use-region-p)
    (er/c-mark-statement)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((oldpos (point)))
      (skip-syntax-forward " ")
      (if (looking-at "{")
	  (progn (forward-sexp)
		 (exchange-point-and-mark))
	(goto-char oldpos)))))

(defun er/c-mark-prev-statement ()
  "Mark the statement preceding current block."
  (interactive)
  (when (use-region-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (when (looking-at "{")
      (skip-syntax-backward " ")
      (backward-char)
      (c-beginning-of-statement 1))))

(defun er/add-c-mode-expansions ()
  "Adds expansions for buffers in c-mode."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
	       '(c-mark-function
		 er/c-mark-statement
		 er/c-mark-fully-qualified-name
		 er/c-mark-next-block
		 er/c-mark-prev-statement))))

(add-hook 'c-mode-common-hook 'er/add-c-mode-expansions)

(provide 'c-mode-expansions)

;; c-mode-expansions.el ends here
