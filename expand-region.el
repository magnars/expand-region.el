;;; expand-region.el --- Increase selected region by semantic units.

;; Copyright (C) 2011 Magnar Sveen

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

;; Expand region increases the selected region by semantic units. Just keep
;; pressing the key until it selects what you want.

;; An example:

;;     (setq alphabet-start "abc def")

;; With the cursor at the `c`, it starts by marking the entire word `abc`, then
;; expand to the contents of the quotes `abc def`, then to the entire quote
;; `"abc def"`, then to the contents of the sexp `setq alphabet-start "abc def"`
;; and finally to the entire sexp.

;; You can set it up like this:

;;     (require 'expand-region)
;;     (global-set-key (kbd "C-@") 'er/expand-region)

;; ## Language support

;; Expand region works fairly well with most languages, due to the general
;; nature of the basic expansions:

;;     er/mark-word
;;     er/mark-symbol
;;     er/mark-method-call
;;     er/mark-inside-quotes
;;     er/mark-outside-quotes
;;     er/mark-inside-pairs
;;     er/mark-outside-pairs

;; However, most languages also will benefit from some specially crafted
;; expansions. For instance, expand-region comes with these extra expansions for
;; html-mode:

;;     er/mark-html-attribute
;;     er/mark-inner-tag
;;     er/mark-outer-tag

;; You can add your own expansions to the languages of your choice simply by
;; creating a function that looks around point to see if it's inside or looking
;; at the construct you want to mark, and if so - mark it.

;; There's plenty of examples to look at in these files.

;; After you make your function, add it to a buffer-local version of
;; the `er/try-expand-list`.

;; **Example:**

;; Let's say you want expand-region to also mark paragraphs and pages in
;; text-mode. Incidentally Emacs already comes with `mark-paragraph` and
;; `mark-page`. To add it to the try-list, do this:

;;     (defun er/add-text-mode-expansions ()
;;       (make-variable-buffer-local 'er/try-expand-list)
;;       (setq er/try-expand-list (append
;;                                 er/try-expand-list
;;                                 '(mark-paragraph
;;                                   mark-page))))

;;     (add-hook 'text-mode-hook 'er/add-text-mode-expansions)

;; Add that to its own file, and require it at the bottom of this one,
;; where it says "Mode-specific expansions"

;; **Warning:** Badly written expansions might slow down expand-region
;; dramatically. Remember to exit quickly before you start traversing
;; the entire document looking for constructs to mark.

;; ## Contribute

;; If you make some nice expansions for your favorite mode, it would be
;; great if you opened a pull-request. The repo is at:

;;     https://github.com/magnars/expand-region.el

;;; Code:

(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp))
      (while (looking-at word-regexp)
        (forward-char))
      (set-mark (point))
      (while (looking-back word-regexp)
        (backward-char)))))

(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (while (looking-at symbol-regexp)
        (forward-char))
      (set-mark (point))
      (while (looking-back symbol-regexp)
        (backward-char)))))

(defun er/mark-symbol-with-prefix ()
  "Mark the entire symbol around or in front of point, including prefix."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw")
        (prefix-regexp "\\s'"))
    (when (or (looking-at prefix-regexp)
              (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (while (looking-at prefix-regexp)
        (forward-char))
      (while (looking-at symbol-regexp)
        (forward-char))
      (set-mark (point))
      (while (or (looking-back symbol-regexp)
                 (looking-back prefix-regexp))
        (backward-char)))))

;; Mark method call (can be improved further)

(defun er/mark-method-call ()
  "Mark the current symbol (including dots) and then paren to closing paren."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw\\|\\."))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (while (looking-back symbol-regexp)
        (backward-char))
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(")
          (forward-list))
      (exchange-point-and-mark))))

;; Quotes

(defun er--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'er--point-is-in-string-p 'er--current-quotes-char)

(defun er--move-point-forward-out-of-string ()
  "Move point forward until it exits the current quoted string."
  (while (er--point-is-in-string-p) (forward-char)))

(defun er--move-point-backward-out-of-string ()
  "Move point backward until it exits the current quoted string."
  (while (er--point-is-in-string-p) (backward-char)))

(defun er/mark-inside-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (when (er--point-is-in-string-p)
    (er--move-point-backward-out-of-string)
    (forward-char)
    (set-mark (point))
    (er--move-point-forward-out-of-string)
    (backward-char)
    (exchange-point-and-mark)))

(defun er/mark-outside-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (if (er--point-is-in-string-p)
      (er--move-point-backward-out-of-string))
  (when (looking-at "\\s\"")
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

;; Pairs - ie [] () {} etc

(defun er--inside-pairs-p ()
  "Is point inside any pairs?"
  (> (car (syntax-ppss)) 0))

(defun er/mark-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs."
  (interactive)
  (when (er--inside-pairs-p)
      (goto-char (nth 1 (syntax-ppss)))
      (set-mark (1+ (point)))
      (forward-list)
      (backward-char)
      (exchange-point-and-mark)))

(defun er--looking-at-pair ()
  "Is point looking at an opening pair char?"
  (looking-at "\\s("))

(defun er--looking-at-marked-pair ()
  "Is point looking at a pair that is entirely marked?"
  (and (er--looking-at-pair)
       (eq (mark)
           (save-excursion
             (forward-list)
             (point)))))

(defun er/mark-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars."
  (interactive)
  (when (and (er--inside-pairs-p)
             (or (not (er--looking-at-pair))
                 (er--looking-at-marked-pair)))
    (goto-char (nth 1 (syntax-ppss))))
  (when (er--looking-at-pair)
    (set-mark (point))
    (forward-list)
    (exchange-point-and-mark)))

;; Methods to try expanding to

(setq er/try-expand-list '(er/mark-word
                           er/mark-symbol
                           er/mark-symbol-with-prefix
                           er/mark-method-call
                           er/mark-inside-quotes
                           er/mark-outside-quotes
                           er/mark-inside-pairs
                           er/mark-outside-pairs))

;; The magic expand-region method

(defun er/expand-region ()
  "Increase selected region by semantic units.
Basically it runs all the mark-functions in the er/try-expand-list
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
  (interactive)
  (let ((start (point))
        (end (if (region-active-p) (mark) (point)))
        (try-list er/try-expand-list)
        (best-start 0)
        (best-end (buffer-end 1)))
    (while try-list
      (save-excursion
        (when (bolp)
          (back-to-indentation)
          (setq start (point)))
        (condition-case nil
            (progn
              (funcall (car try-list))
              (when (and (region-active-p)
                         (<= (point) start)
                         (>= (mark) end)
                         (> (- (mark) (point)) (- end start))
                         (or (> (point) best-start)
                             (and (= (point) best-start)
                                  (< (mark) best-end))))
                (setq best-start (point))
                (setq best-end (mark))
                (unless (minibufferp)
                  (message "%S" (car try-list)))))
          (error nil)))
      (setq try-list (cdr try-list)))
    (goto-char best-start)
    (set-mark best-end)))

;; Mode-specific expansions
(require 'js-mode-expansions)
(require 'html-mode-expansions)
(require 'css-mode-expansions)

(provide 'expand-region)

;;; expand-region.el ends here