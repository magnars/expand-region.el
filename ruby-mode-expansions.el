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
;;      I would bind them to C-M-u and C-M-d respectively.

;; Expansions:
;;
;;
;;  er/mark-ruby-block-up
;;

;;; Code:

(require 'expand-region-core)

(defvar er/ruby-block-end-re
  (concat ruby-block-end-re "\\|}")
  "like ruby-mode's but also for '}'")

(defun er/ruby-skip-past-block-end ()
  "ensure that point is at bol"
  (when (looking-at er/ruby-block-end-re)
    (goto-char (match-end 0))))

(defun er/ruby-backward-up ()
  "a la `paredit-backward-up'"
  (interactive)
  (let ((orig-point (point))
        progress-beg
        progress-end)
    ;; cover the case when point is in begining of block
    (unless (progn (ruby-end-of-block)
                   (ruby-beginning-of-block)
                   (< (point) orig-point))
      (loop do
            (ruby-beginning-of-block)
            (setq progress-beg (point))
            (ruby-end-of-block)
            (setq progress-end (if (looking-at-p er/ruby-block-end-re)
                                   (point-at-bol 2)
                                 (point-at-bol 1)))
            (goto-char progress-beg)
            (if (> progress-end orig-point)
                (return))))))

;;; This command isn't used here explicitly, but it's symmetrical with
;;; `er/ruby-backward-up', and nifty for interactive use.
(defun er/ruby-forward-up ()
  "a la `paredit-forward-up'"
  (interactive)
  (er/ruby-backward-up)
  (ruby-end-of-block)
  (er/ruby-skip-past-block-end))

(defun er/get-ruby-block (&optional pos)
  "return (beg . end) of current block"
  (setq pos (or pos (point)))
  (save-excursion
    (goto-char pos)
    (let (beg end)
      (cons (progn
              (er/ruby-backward-up)
              (point))
            (progn
              (ruby-end-of-block)
              (er/ruby-skip-past-block-end)
              (point))))))

(defun er/mark-ruby-block-up-1 ()
  (er/ruby-backward-up)
  (set-mark (point))
  (ruby-end-of-block)
  (er/ruby-skip-past-block-end)
  (exchange-point-and-mark))

(defun er/mark-ruby-block-up ()
  "mark the next level up."
  (interactive)
  (if (use-region-p)
      (let ((old-end (region-end)))
        (if (> (cdr (er/get-ruby-block old-end)) old-end)
            (progn
              (deactivate-mark)
              (goto-char old-end)
              (er/mark-ruby-block-up))
          (er/mark-ruby-block-up-1)))
    (er/mark-ruby-block-up-1)))

(defun er/add-ruby-mode-expansions ()
  "Adds Ruby-specific expansions for buffers in ruby-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-ruby-block-up))))

(add-hook 'ruby-mode-hook 'er/add-ruby-mode-expansions)

(provide 'ruby-mode-expansions)
