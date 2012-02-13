;;; python-mode-expansions.el --- Python-specific expansions for expand-region

;; Copyright (C) 2012 Felix Geller

;; Author: Felix Geller
;; Based on python-mode-expansions by: Ivan Andrus
;; Keywords: marking region Python python-mode

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

;; These expansion don't work with the built-in python.el but rather
;; python-mode: https://launchpad.net/python-mode

;; Features:
;;  - Mark functionality taken from python-mode:
;;    - `py-mark-expression'
;;    - `py-mark-statement'
;;    - `py-mark-block'
;;    - `py-mark-class'
;;  - Additions implemented here:
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;    - `er/mark-outer-python-block'
;;  - Supports multi-line strings
;;  - Supports incremental expansion of nested blocks

;; Feel free to contribute any other expansions for Python at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(defvar er--python-string-delimiter "'\"")

(defun er/mark-outside-python-string ()
  "Marks region outside a (possibly multi-line) Python string"
  (interactive)
  (let ((string-beginning (py-in-string-p)))
    (when string-beginning
      (goto-char string-beginning)
      (set-mark (point))
      (forward-sexp)
      (exchange-point-and-mark))))

(defun er/mark-inside-python-string ()
  "Marks region inside a (possibly multi-line) Python string"
  (interactive)
  (let ((string-beginning (py-in-string-p)))
    (when string-beginning
      (goto-char string-beginning)
      (forward-sexp)
      (skip-chars-backward er--python-string-delimiter)
      (set-mark (point))
      (goto-char string-beginning)
      (skip-chars-forward er--python-string-delimiter))))

(defun er--move-to-beginning-of-outer-python-block (start-column)
  "Assumes that point is in a python block that is surrounded by
another that is not the entire module. Uses `py-indent-offset' to
find the beginning of the surrounding block because
`py-beginning-of-block-position' just looks for the previous
block-starting key word syntactically."
  (while (> (current-column) (- start-column py-indent-offset))
    (previous-line)
    (py-beginning-of-block)))

(defun er/mark-outer-python-block ()
  "Attempts to mark a surrounding block by moving to the previous
line and selecting the surrounding block."
  (interactive)
  (let ((start-column (current-column)))
    (when (> start-column 0) ; outer block is the whole buffer
      (er--move-to-beginning-of-outer-python-block start-column)
      (let ((block-beginning (point)))
        (py-end-of-block)
        (set-mark (point))
        (goto-char block-beginning)))))

(defun er/add-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list)
       (setq er/try-expand-list
             '(
               er/mark-word
               er/mark-symbol
               er/mark-symbol-with-prefix
               er/mark-next-accessor
               er/mark-method-call
               er/mark-comment
               er/mark-comment-block
               er/mark-inside-python-string
               er/mark-outside-python-string
               er/mark-inside-pairs
               er/mark-outside-pairs
               py-mark-expression
               py-mark-statement
               py-mark-block
               er/mark-outer-python-block
               py-mark-class
               ))))

(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
