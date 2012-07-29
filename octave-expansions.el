;;; octave-expansions.el --- octave-mode expansions for expand-region

;; Copyright (C) 2012 Mark Hepburn

;; Author: Mark Hepburn
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

;; Feel free to contribute any other expansions for Octave at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'octave-mod)

(message "loading octave-specific expansions")

;;; mark statement (var = exp;)

(defun er/octave-mark-assignment ()
  "Mark a single assignment statement, including the trailing semi-colon."
  (interactive)
  ;; search back until on first non-whiteespace not a ';'
  ;; search forward for <word><whitespace>=<anything not ';'>
  ;; (probably need to exchange point and mark at this stage)

  ;; Hmm, actually, probably need to do a bit more parsing using syntax-tables...
  ;; Difficulty: LHS could be [vars], or arr(exp), or sym...
  ;; See, eg, er/mark-method-call
  ;;
  ;; Perhaps: find the =, and then find the trailing ;.  Then, from
  ;; the = again look to the left and go by cases:
  ;; - if we see ']' can just go backward-list
  ;; - if ')' then backwards-list and look again (eg, wind up with | in sym|(idx))
  ;; - if sym then backwards-sexp I think would do it.

  (let ((assignment-re "\\w+\\s *=\\s *.*;"))
    (unless (looking-at (concat "\\s *" assignment-re))
      (re-search-backward ";[ \t\n\r]*\\w+"))
    (when (re-search-forward assignment-re nil t)
      (push-mark (match-beginning 0))
      (exchange-point-and-mark))))


(defun er/octave-mark-up-block ()
  "Mark the containing block, assuming the current block has
already been marked."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (octave-up-block -1)              ; -1 means move up to _beginning_
    (octave-mark-block)))


(defun er/add-octave-expansions ()
  "Adds octave/matlab-specific expansions for buffers in octave-mode"
  (let ((try-expand-list-additions '(octave-mark-block
                                     er/octave-mark-up-block
                                     octave-mark-defun)))
    (set (make-local-variable 'er/try-expand-list)
         (append er/try-expand-list try-expand-list-additions))))

(add-hook 'octave-mode-hook 'er/add-octave-expansions)

(provide 'octave-expansions)

