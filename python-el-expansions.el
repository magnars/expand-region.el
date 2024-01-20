;;; python-el-expansions.el --- Python-specific expansions for expand-region  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2023  Free Software Foundation, Inc

;; Authors: Ivan Andrus, Felix Geller, @edmccard
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region python

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

;; For python.el included with GNU Emacs
;;  - Mark functionality implemented here:
;;    - `er/mark-python-statement'
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;    - `er/mark-python-numbers'
;;    - `er/mark-python-defun-or-class'
;;    - `er/mark-python-inside-block'
;;    - `er/mark-python-outside-block'
;;  - Supports multi-line strings

;; Feel free to contribute any other expansions for Python at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'python)

;; Statements
;; ----------
(defun er/mark-python-statement ()
  "Mark one Python statement, eg. x = 3."
  (python-nav-end-of-statement)
  (set-mark (point))
  (python-nav-beginning-of-statement))

;; Numbers
;; -------
(defun er/mark-python-numbers ()
  "Mark one Python number, eg. '1', '10_000', '-4.2', '1e-4', '5j'."
  (er/mark-word)
  ;; in case we are on the decimal side
  (when (looking-back "[0-9]+\\." (line-beginning-position) t)
    (goto-char (match-beginning 0)))
  ;; thousands delimiters upstream
  (when (looking-back "\\([0-9]+_\\)+" (line-beginning-position) t)
    (goto-char (match-beginning 0)))
  ;; +/- signs before numbers
  (when (looking-back "[+-]" (- (point) 2))
    (goto-char (match-beginning 0)))
  ;; Other direction
  (exchange-point-and-mark)
  ;; thousands delimiters downstream
  (when (looking-at "\\(_[0-9]+\\)+")
    (goto-char (match-end 0)))
  ;; floating numbers
  (when (looking-at "\\.[0-9j]+")
    (goto-char (match-end 0)))
  ;; exponential notation
  (when (save-excursion (forward-char -1) (looking-at "[eE]-?[0-9]+"))
    (goto-char (match-end 0)))
  (exchange-point-and-mark))

;; Strings
;; -------
(defun er--move-point-backward-out-of-python-string ()
  "Move point backward until it exits the current python string."
  (goto-char (nth 8 (syntax-ppss)))
  ;; Out of multi-quoted string
  (when (looking-back "\\(\"\"\\|''\\)" (- (point) 3))
    (goto-char (match-beginning 0)))
  ;; Out of the string prefix
  (when (looking-back "[rfubBRFU]+" (- (point) 3) t)
    (goto-char (match-beginning 0))))

(defun er--move-point-forward-out-of-python-string ()
  "Move point backward until it exits the current python string."
  (goto-char (nth 8 (syntax-ppss)))
  (forward-sexp)
  (cond
   ((looking-at "\\(\"\"\\|''\\)")
    (goto-char (match-end 0)))))

(defun er/mark-outside-python-string ()
  "Mark the current string, including the quotation marks and specifiers."
  (when (er--point-inside-string-p)
    (er--move-point-backward-out-of-python-string)
    (when (looking-at "[rfubRFUB]*\\s\"")
      (set-mark (point))
      (goto-char (match-end 0))
      ;; (forward-char)
      (er--move-point-forward-out-of-python-string)
      (exchange-point-and-mark))))

(defun er/mark-inside-python-string ()
  "Mark the current inside string."
  (er/mark-inside-quotes))

;; Functions / Classes
;; -------------------
(defun er/mark-python-defun-or-class ()
  "Mark the current function or class."
  (let ((starting-indent (current-indentation))
        moved)
    (if (python-info-looking-at-beginning-of-defun)
        (if (= starting-indent 0)
            (setq moved nil)
          (while (>= (current-indentation) starting-indent)
            (python-nav-beginning-of-defun))
          (setq moved t))
      (setq moved (python-nav-beginning-of-defun)))
    ;; other side
    (when moved
      (set-mark (point))
      (python-nav-end-of-defun)
      (exchange-point-and-mark))))

;; Blocks
;; ------
(defun er/mark-python-inside-block ()
  "Mark the current inside indentation block."
  (let ((indentation (current-indentation)))
    (if (= indentation 0)
        (progn
          (push-mark (point))
          (push-mark (point-max) nil t)
          (goto-char (point-min)))
      (while (<= indentation (current-indentation))
        (forward-line -1))
      (forward-line 1)
      (push-mark (point) nil t)
      (while (<= indentation (current-indentation))
        (forward-line 1))
      (backward-char)))
  (exchange-point-and-mark)
  (python-nav-beginning-of-statement))

(defun er/mark-python-outside-block ()
  "Mark the current outside indentation block."
  (python-nav-backward-block)
  (set-mark (point))
  (python-nav-end-of-block)
  (exchange-point-and-mark))

;; Hook it up
;; ----------
(defun er/add-python-mode-expansions ()
  "Add Python-specific expansions for buffers in `python-mode'."
  (let ((try-expand-list-additions '(er/mark-python-statement
                                     er/mark-inside-python-string
                                     er/mark-outside-python-string
                                     er/mark-python-numbers
                                     er/mark-python-defun-or-class
                                     er/mark-python-inside-block
                                     er/mark-python-outside-block)))
    (set (make-local-variable 'expand-region-skip-whitespace) nil)
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-inside-quotes
         (remove 'er/mark-defun
         (remove 'er/mark-defun-or-class
                 (remove 'er/mark-outside-quotes
                         (append er/try-expand-list try-expand-list-additions))))))))

(er/enable-mode-expansions 'python-mode #'er/add-python-mode-expansions)

(provide 'python-el-expansions)
;;; python-el-expansions.el ends here
