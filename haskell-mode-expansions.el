;;; haskell-mode-expansions.el --- Haskell-specific expansions for expand-region

;; Copyright (C) 2018 Joseph Morag

;; Author: Joseph Morag <jm4157@columbia.edu>
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

;; For now I have only found the need for mark-haskell-declaration.

;;; Code:

(require 'expand-region-core)
(require 'haskell-mode)

(defun er/mark-haskell-declaration ()
  (interactive)
  (haskell-ds-forward-decl)
  (set-mark (point))
  (haskell-ds-backward-decl))

(defun er/add-haskell-mode-expansions ()
  "Adds Haskell-specific expansions for buffers in haskell-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list '(er/mark-haskell-declaration))))

(er/enable-mode-expansions 'haskell-mode 'er/add-haskell-mode-expansions)

(provide 'haskell-mode-expansions)

;; haskell-mode-expansions.el ends here
