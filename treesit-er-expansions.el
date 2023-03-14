;;; treesit-er-expansions.el ---  -*- lexical-binding: t; -*-
;; Copyright (C) 2023 Aleksandar Dimitrov


;; Author: Aleksandar Dimitrov <git@aleks.bg>
;; Created: 2023-03-13
;; Keywords: marking region

;; This file is not part of GNU Emacs.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'treesit)
(require 'expand-region-core)

(defun er/treesit-er--get-node-between (a b)
  "Find the node that sits above any node in the region (A B)."
  (let* ((start (min a b))
         (end (max a b)))
    (treesit-parent-until
     (treesit-node-at start)
     (lambda (node) (< end (treesit-node-end node))))))

(defun er/treesit-er-parent-node ()
  "Expand to the node above point, or to the node above the active region."
  (interactive)
  (let ((node
          (if (region-active-p)
              (er/treesit-er--get-node-between (mark) (point))
            (treesit-node-at (point)))))
    (goto-char (treesit-node-start node))
    (set-mark (treesit-node-end node))
    (activate-mark)))

(defun er/add-treesit-er-expansion ()
  "Add the expansion for treesit mode."
  (set (make-local-variable 'er/try-expand-list)
       ;; we don't need any other expansion rules once we have treesit
       '(er/treesit-er-parent-node)))

(er/enable-mode-expansions 'c-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'js-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'go-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'css-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'c++-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'html-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'yaml-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'bash-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'ruby-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'java-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'toml-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'json-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'rust-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'cmake-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'csharp-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'python-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'go-mod-ts-mode 'er/add-treesit-er-expansion)
(er/enable-mode-expansions 'tsx-ts-mode 'er/add-treesit-er-expansion)

(provide 'treesit-er-expansions)

;;; treesit-er-expansions.el ends here

