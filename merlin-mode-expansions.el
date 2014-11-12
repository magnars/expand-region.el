;;; merlin-mode-expansions.el --- OCaml-specific expansions for expand-region

;; Copyright (C) 2014 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Keywords: marking region merlin

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

;;; Code:

(require 'expand-region-core)
(require 'merlin)

(defun er/add-merlin-mode-expansions ()
  "Add merlin-mode expansions"
  (set (make-local-variable 'er/try-expand-list)
       '(merlin-enclosing-expand)))

(er/enable-mode-expansions 'merlin-mode 'er/add-merlin-mode-expansions)

(provide 'merlin-mode-expansions)

;; merlin-mode-expansions.el ends here
