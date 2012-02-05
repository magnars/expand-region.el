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

;;; Bugs:

;; Doesn't properly handle triple quoted strings -- need to patch or
;; replace regular er/mark-inside-pairs since it marks inside the
;; outermost pair of quotes.

;;; Code:

(defun er/mark-python-statement ()
  "Marks one Python statement, eg. x = 3"
  (interactive)
  (python-end-of-statement)
  (set-mark (point))
  (python-beginning-of-statement))

(defun er/add-python-mode-expansions ()
  "Adds Python-specific expansions for buffers in python-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-python-statement
                                                    python-mark-block))))

(add-hook 'python-mode-hook 'er/add-python-mode-expansions)

(provide 'python-mode-expansions)

;; python-mode-expansions.el ends here
