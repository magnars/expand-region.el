;;; ruby-mode-expansions.el --- ruby-specific expansions for expand-region

;; Copyright (C) 2011 Magnar Sveen

;; Author: Matt Briggs
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


;; Idiomatic ruby has a lot of nested blocks, and its function marking seems a bit buggy.
;;
;; LeWang:
;;
;;      I think `er/ruby-backward-up' and `er/ruby-forward-up' are very
;;      nifty functions in their own right.
;;
;;      I would bind them to C-M-u and C-M-n respectively.

;; Expansions:
;;
;;
;;  er/mark-ruby-block-up
;;

;;; Code:

(require 'expand-region-core)

(defvar er/ruby-block-end-re
  "like ruby-mode's but also for '}'"
  (concat ruby-block-end-re "\\|}"))

(defsubst er/ruby-skip-past-block-end ()
  "ensure that point is at bol"
  (if (looking-at-p er/ruby-skip-past-block-end)
      (forward-line 1)
    (forward-line 0)))

(defun er/ruby-backward-up ()
  "a la `paredit-backward-up'"
  (interactive)
  (loop do
        (let ((orig-point (point))
              progress-beg
              progress-end)
          (ruby-beginning-of-block)
          (setq progress-beg (point))
          (ruby-end-of-block)
          (ruby-skip-past-block-end)
          (setq progress-end (point))
          (goto-char progress-beg)
          (if (> progress-end orig-point)
              (return)))))

;;; This command isn't used here explicitly, but it's symmetrical with
;;; `er/ruby-backward-up', and nifty for interactive use.
(defun er/ruby-forward-up ()
  "a la `paredit-forward-up'"
  (interactive)
  (ruby-backward-up)
  (ruby-end-of-block)
  (ruby-skip-past-block-end))

(defun er/mark-ruby-block-up ()
  (interactive)
  "mark the next level up."
  (ruby-backward-up)
  (set-mark (point-at-bol 1))
  (ruby-end-of-block)
  (ruby-skip-past-block-end)
  (exchange-point-and-mark))

(defun er/add-ruby-mode-expansions ()
  "Adds Ruby-specific expansions for buffers in ruby-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-ruby-block-up))))

(add-hook 'ruby-mode-hook 'er/add-ruby-mode-expansions)

(provide 'ruby-mode-expansions)
