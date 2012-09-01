;;; expand-region-custom.el --- Increase selected region by semantic units.

;; Copyright (C) 2012 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
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

;; This file holds customization variables.

;;; Code:

(defgroup expand-region nil
  "Increase selected region by semantic units."
  :group 'tools)

(defcustom expand-region-preferred-python-mode 'python
  "The name of your preferred python mode"
  :group 'expand-region
  :type '(choice (const :tag "Emacs' python.el" 'python)
                 (const :tag "fgallina's python.el" 'fgallina-python)
                 (const :tag "python-mode.el" 'python-mode)))

(defcustom expand-region-guess-python-mode t
  "If expand-region should attempt to guess your preferred python mode"
  :group 'expand-region
  :type '(choice (const :tag "Guess" t)
                 (const :tag "Do not guess" nil)))

(defun expand-region-guess-python-mode ()
  "Guess the user's preferred python mode."
  (setq expand-region-preferred-python-mode
        (if (fboundp 'python-setup-brm)
            'python
          'fgallina-python)))

(provide 'expand-region-custom)

;;; expand-region-custom.el ends here
