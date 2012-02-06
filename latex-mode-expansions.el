;;; latex-mode-expansions.el --- LaTeX-specific expansions for expand-region

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

;; This is for AUCTeX, not the builtin latex-mode.

;; Feel free to contribute any other expansions for LaTeX at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(defun er/add-latex-mode-expansions ()
  "Adds expansions for buffers in latex-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(LaTeX-mark-environment
                                                    LaTeX-mark-section))))

(add-hook 'LaTeX-mode-hook 'er/add-latex-mode-expansions)

(provide 'latex-mode-expansions)

;; latex-mode-expansions.el ends here
