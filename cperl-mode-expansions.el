;;; cperl-mode-expansions.el --- perl-specific expansions for expand-region
;; Copyright (C) 2012 Kang-min Liu
;;
;; Author: Kang-min Liu <gugod@gugod.org>
;; Keywords: marking region cperl
;;
;; This is free software, licensed under:
;;
;;   The MIT (X11) License
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Code:

(require 'expand-region-core)

(defun er/mark-cperl-variable-name ()
  "Marks one perl variable"
  (interactive)
  (forward-word)
  (backward-word)
  (search-backward-regexp "[@$%]" (line-beginning-position))
  (set-mark (point))
  (forward-char)
  (search-forward-regexp "[^a-z_]" (line-end-position))
  (backward-char)
  (exchange-point-and-mark))

(defun er/mark-cperl-package-name ()
  "Marks one perl package name"
  (interactive)
  (forward-sexp)
  (backward-sexp)
  (set-mark (point))
  (forward-sexp)
  (search-backward "::" (line-beginning-position))
  (exchange-point-and-mark))

(defun er/add-cperl-mode-expansions ()
  "Add cprel mode expansinos"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-cperl-variable-name
                                                    er/mark-cperl-package-name
                                                    ))))

(er/enable-mode-expansions 'cperl-mode 'er/add-cperl-mode-expansions)

(provide 'cperl-mode-expansions)

;; css-mode-expansions.el ends here
