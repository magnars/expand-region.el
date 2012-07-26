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
;; er/c-mark-statement
;;     captures simple and more complex statements
;;
;; er/c-mark-fully-qualified-name
;;     captures identifiers composed of several '::'-separated parts
;;
;; er/c-mark-next-block
;;     if a statement is followed by a '{}'-enclosed block, mark it
;;     captures function definitions and if/for/... constructs
;;
;; er/c-mark-prev-statement
;;     if a '{}'-enclosed block is preceded by a statement, mark it
;;     captures function definitions and if/for/... constructs
;;
;; Feel free to contribute any other expansions for C at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)

(defun er/c-mark-statement ()
  "Mark the current C statement.

This function tries to ensure that pair-delimited substring are
either fully inside or fully outside the statement."
  (interactive)
  (unless (use-region-p)
    (set-mark (point)))

  (if (< (point) (mark))
      (exchange-point-and-mark))

  ;; Contract the region a bit to make the
  ;; er/c-mark-statement function idempotent
  (when (> (- (point) (mark)) 2)
    (exchange-point-and-mark)
    (forward-char)
    (exchange-point-and-mark)
    (backward-char))

  (let (beg end)
    ;; Determine boundaries of the outside-pairs region
    (save-excursion
      (er/mark-outside-pairs)
      (setq beg (point)
            end (mark)))

    ;; Determine boundaries of the statement as given
    ;; by c-beginning-of-statement/c-end-of-statement
    (c-end-of-statement)
    (exchange-point-and-mark)
    (c-beginning-of-statement 1)

    ;; If the two regions overlap, expand the region
    (cond ((and (<= (point) beg)
                (<  (mark)  end))
           (set-mark end))
          ((and (>  (point) beg)
                (>= (mark)  end))
           (goto-char beg)
           (c-beginning-of-statement 1)))))

(defun er/c-mark-fully-qualified-name ()
  "Mark the current C++ fully qualified identifier.

This function captures identifiers composed of multiple
'::'-separated parts."
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
  "Mark the current statement and the following block.

This captures function definitions and if/for/... constructs
when point and mark are in the statement part."
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
  "Mark the statement preceding current block.

This captures function definitions and if/for/... constructs
when the body is already marked."
  (interactive)
  (when (use-region-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (when (looking-at "{")
      (let ((end (mark)))
        (skip-syntax-backward " ")
        (backward-char)
        (set-mark (point))
        (er/c-mark-statement)
        (set-mark end)))))

(defun er/add-c-mode-expansions ()
  "Adds expansions for buffers in c-mode."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(er/c-mark-statement
                 er/c-mark-fully-qualified-name
                 er/c-mark-next-block
                 er/c-mark-prev-statement))))

(add-hook 'c-mode-common-hook 'er/add-c-mode-expansions)

(provide 'c-mode-expansions)

;; c-mode-expansions.el ends here
