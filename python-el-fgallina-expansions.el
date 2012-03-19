;;; python-el-fgallina-expansions.el --- fgallina/python.el-specific expansions for expand-region

;; Copyright (C) 2012 Felix Geller

;; Author: Felix Geller
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
;;
;;  - Additions implemented here:
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;    - `er/mark-python-sentence'
;;  - Supports multi-line strings

;;; Code:

(require 'expand-region-core)

(defvar er--python-string-delimiter "'\"")

(defun er/mark-inside-python-string ()
  (interactive)
  (let ((beginning-of-string (python-info-ppss-context 'string (syntax-ppss))))
    (when beginning-of-string
      (goto-char beginning-of-string)
      (forward-sexp)
      (skip-chars-backward er--python-string-delimiter)
      (set-mark (point))
      (goto-char beginning-of-string)
      (skip-chars-forward er--python-string-delimiter))))

(defun er/mark-outside-python-string ()
  (interactive)
  (let ((beginning-of-string (python-info-ppss-context 'string (syntax-ppss))))
    (when beginning-of-string
      (goto-char beginning-of-string)
      (set-mark (point))
      (forward-sexp)
      (exchange-point-and-mark))))

(defun er/mark-python-sentence ()
  (interactive)
  (python-nav-sentence-end)
  (set-mark (point))
  (python-nav-sentence-start))

(defun er/mark-python-block ()
  (interactive)
  (let ((rx-block-start (python-rx block-start)))
    (back-to-indentation)
    (unless (looking-at rx-block-start)
      (re-search-backward rx-block-start))
    (set-mark (point))  ; mark beginnig-of-block
    (let ((block-indentation (current-indentation)))
      (end-of-line)
      (while (and (re-search-forward rx-block-start (point-max) 'goto-end)
                  (> (current-indentation) block-indentation)))
      (beginning-of-line)
      (python-util-forward-comment -1)
      (exchange-point-and-mark))))

(defun er/mark-outer-python-block ()
  (interactive)
  (forward-line -1)
  (er/mark-python-block))

(defun er/add-python-mode-expansions ()
  "Adds python-mode-specific expansions for buffers in python-mode"
  (let ((try-expand-list-additions '(
                                     er/mark-inside-python-string
                                     er/mark-outside-python-string
                                     er/mark-python-sentence
                                     er/mark-python-block
                                     er/mark-outer-python-block
                                     )))
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-inside-quotes
                 (remove 'er/mark-outside-quotes
                         (append er/try-expand-list try-expand-list-additions))))))


(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-el-fgallina-expansions)

;; python-el-fgallina-expansions.el ends here
