;;; expand-region-custom.el --- Increase selected region by semantic units.  -*- lexical-binding: t; -*-

;; Copyright (C) 2012-2023  Free Software Foundation, Inc

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

;;;###autoload
(defgroup expand-region nil
  "Increase selected region by semantic units."
  :group 'tools)

;;;###autoload
(defcustom expand-region-preferred-python-mode 'python
  "The name of your preferred python mode."
  :type '(choice (const :tag "Emacs' python.el" python)
                 (const :tag "fgallina's python.el" fgallina-python)
                 (const :tag "python-mode.el" python-mode)))

;;;###autoload
(defcustom expand-region-guess-python-mode t
  "If expand-region should attempt to guess your preferred python mode."
  :type '(choice (const :tag "Guess" t)
                 (const :tag "Do not guess" nil)))

(defun expand-region-guess-python-mode ()
  "Guess the user's preferred python mode."
  (setq expand-region-preferred-python-mode
        (if (fboundp 'python-setup-brm)
            'python
          'fgallina-python)))

;;;###autoload
(defcustom expand-region-autocopy-register ""
  "Register to copy most recent expand or contract to.

Activated when set to a string of a single character (for example, \"e\")."
  :type 'string)

;;;###autoload
(defcustom expand-region-skip-whitespace t
  "If expand-region should skip past whitespace on initial expansion."
  :type '(choice (const :tag "Skip whitespace" t)
                 (const :tag "Do not skip whitespace" nil)))

;;;###autoload
(defcustom expand-region-fast-keys-enabled t
  "If expand-region should bind fast keys after initial expand/contract."
  :type '(choice (const :tag "Enable fast keys" t)
                 (const :tag "Disable fast keys" nil)))

;;;###autoload
(defcustom expand-region-contract-fast-key "-"
  "Key to use after an initial expand/contract to contract once more."
  :type 'string)

;;;###autoload
(defcustom expand-region-reset-fast-key "0"
  "Key to use after an initial expand/contract to undo."
  :type 'string)

;;;###autoload
(defcustom expand-region-exclude-text-mode-expansions
  '(html-mode nxml-mode)
  "List of modes derived from `text-mode' to exclude from text mode expansions."
  :type '(repeat (symbol :tag "Major Mode" unknown)))

;;;###autoload
(defcustom expand-region-smart-cursor nil
  "Defines whether the cursor should be placed intelligently after expansion.

If set to t, and the cursor is already at the beginning of the new region,
keep it there; otherwise, put it at the end of the region.

If set to nil, always place the cursor at the beginning of the region."
  :type '(choice (const :tag "Smart behaviour" t)
                 (const :tag "Standard behaviour" nil)))

;;;###autoload
(define-obsolete-variable-alias 'er/enable-subword-mode?
  'expand-region-subword-enabled "2019-03-23")

;;;###autoload
(defcustom expand-region-subword-enabled nil
  "Whether expand-region should use subword expansions."
  :type '(choice (const :tag "Enable subword expansions" t)
                 (const :tag "Disable subword expansions" nil)))

(defcustom expand-region-show-usage-message t
  "Whether expand-region should show usage message."
  :group 'expand-region
  :type 'boolean)

(provide 'expand-region-custom)

;;; expand-region-custom.el ends here
