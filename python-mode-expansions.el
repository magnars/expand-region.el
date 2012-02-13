;;; python-mode-expansions.el --- Python-specific expansions for expand-region

;; Copyright (C) 2012 Ivan Andrus

;; Author: Ivan Andrus
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

;; There is no need for a er/mark-python-defun since
;; er/mark-python-block will mark it

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

(defun er/mark-outer-python-block ()
  "Attempts to mark a surrounding block by moving to the previous
line and selecting the surrounding block."
  (interactive)
  (previous-line)
  (let ((block-beginning (py-beginning-of-block-position)))
    (when block-beginning
      (py-end-of-block)
      (set-mark (point))
      (goto-char block-beginning))))

(defun er/mark-python-block ()
  "Marks the surrounding Python block"
  (interactive)
  (if (= (region-end) (py-end-of-block))
      (er/mark-outer-python-block)
    (py-mark-block)))

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
               ))))

(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
