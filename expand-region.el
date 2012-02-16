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

;; There's also `er/contract-region` if you expand too far.

;; ## Video
;;
;; You can [watch an intro to expand-region at Emacs Rocks](http://emacsrocks.com/e09.html).

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
;;       (set (make-local-variable 'er/try-expand-list)
;;            (append
;;             er/try-expand-list
;;             '(mark-paragraph
;;               mark-page))))
;;
;;     (add-hook 'text-mode-hook 'er/add-text-mode-expansions)

;; Add that to its own file, and require it at the bottom of this one,
;; where it says "Mode-specific expansions"

;; **Warning:** Badly written expansions might slow down expand-region
;; dramatically. Remember to exit quickly before you start traversing
;; the entire document looking for constructs to mark.

;; ## Contribute
;;
;; If you make some nice expansions for your favorite mode, it would be
;; great if you opened a pull-request. The repo is at:
;;
;;     https://github.com/magnars/expand-region.el

;; ## Contributors
;;
;; * [Josh Johnston](https://github.com/joshwnj) contributed `er/contract-region`
;; * [Le Wang](https://github.com/lewang) contributed consistent handling of the mark ring, expanding into pairs/quotes just left of the cursor, and general code clean-up.
;; * [Matt Briggs](https://github.com/mbriggs) contributed expansions for ruby-mode.
;; * [Ivan Andrus](https://github.com/gvol) contributed expansions for various modes.
;;
;; Thanks!

;;; Code:

(defvar er/history '()
  "A history of start and end points so we can contract after expanding.")

;; history is always local to a single buffer
(make-variable-buffer-local 'er/history)

(defvar er--space-str " \t\n")
(defvar er--blank-list (append er--space-str nil))

;; Default expansions

(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (let ((word-regexp "\\sw"))
    (when (or (looking-at word-regexp)
              (looking-back word-regexp))
      (skip-syntax-forward "w")
      (set-mark (point))
      (while (looking-back word-regexp)
        (backward-char)))))

(defun er/mark-symbol ()
  "Mark the entire symbol around or in front of point."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw"))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-forward "_w")
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
      (skip-syntax-forward "'")
      (skip-syntax-forward "_w")
      (set-mark (point))
      (while (or (looking-back symbol-regexp)
                 (looking-back prefix-regexp))
        (backward-char)))))

;; Mark method call

(defun er/mark-next-accessor ()
  "Presumes that current symbol is already marked, skips over one
period and marks next symbol."
  (interactive)
  (when (use-region-p)
    (when (< (point) (mark))
      (exchange-point-and-mark))
    (let ((symbol-regexp "\\s_\\|\\sw"))
      (when (looking-at "\\.")
        (forward-char 1)
        (skip-syntax-forward "_w")
        (exchange-point-and-mark)))))

(defun er/mark-method-call ()
  "Mark the current symbol (including dots) and then paren to closing paren."
  (interactive)
  (let ((symbol-regexp "\\s_\\|\\sw\\|\\."))
    (when (or (looking-at symbol-regexp)
              (looking-back symbol-regexp))
      (skip-syntax-backward "_w.")
      (set-mark (point))
      (while (looking-at symbol-regexp)
        (forward-char))
      (if (looking-at "(")
          (forward-list))
      (exchange-point-and-mark))))

;; Comments

(defun er--point-is-in-comment-p ()
  "t if point is in comment, otherwise nil"
  (nth 4 (syntax-ppss)))

(defun er--move-point-forward-out-of-comment ()
  "Move point forward until it exits the current quoted comment."
  (while (er--point-is-in-comment-p) (forward-char)))

(defun er--move-point-backward-out-of-comment ()
  "Move point backward until it exits the current quoted comment."
  (while (er--point-is-in-comment-p) (backward-char)))

(defun er/mark-comment ()
  "Mark the current comment."
  (interactive)
  (when (or (er--point-is-in-comment-p)
            (looking-at "\\s<"))
    (er--move-point-backward-out-of-comment)
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-comment)
    (backward-char)
    (exchange-point-and-mark)))

(defun er/mark-comment-block ()
  "Mark the current block of comments."
  (interactive)
  (when (or (er--point-is-in-comment-p)
            (looking-at "\\s<"))
    (er--move-point-backward-out-of-comment)
    (while (save-excursion
             (skip-syntax-backward " ")
             (backward-char)
             (er--point-is-in-comment-p))
      (skip-syntax-backward " ")
      (backward-char)
      (er--move-point-backward-out-of-comment))
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-comment)
    (while (looking-at "\\s *\\s<")
      (back-to-indentation)
      (forward-char)
      (er--move-point-forward-out-of-comment))
    (exchange-point-and-mark)))

;; Quotes

(defun er--current-quotes-char ()
  "The char that is the current quote delimiter"
  (nth 3 (syntax-ppss)))

(defalias 'er--point-inside-string-p 'er--current-quotes-char)

(defun er--move-point-forward-out-of-string ()
  "Move point forward until it exits the current quoted string."
  (while (er--point-inside-string-p) (forward-char)))

(defun er--move-point-backward-out-of-string ()
  "Move point backward until it exits the current quoted string."
  (while (er--point-inside-string-p) (backward-char)))

(defun er/mark-inside-quotes ()
  "Mark the inside of the current string, not including the quotation marks."
  (interactive)
  (when (er--point-inside-string-p)
    (er--move-point-backward-out-of-string)
    (forward-char)
    (set-mark (point))
    (er--move-point-forward-out-of-string)
    (backward-char)
    (exchange-point-and-mark)))

(defun er/mark-outside-quotes ()
  "Mark the current string, including the quotation marks."
  (interactive)
  (if (er--point-inside-string-p)
      (er--move-point-backward-out-of-string)
    (when (and (not (use-region-p))
               (looking-back "\\s\""))
      (backward-char)
      (er--move-point-backward-out-of-string)))
  (when (looking-at "\\s\"")
    (set-mark (point))
    (forward-char)
    (er--move-point-forward-out-of-string)
    (exchange-point-and-mark)))

;; Pairs - ie [] () {} etc

(defun er--point-inside-pairs-p ()
  "Is point inside any pairs?"
  (> (car (syntax-ppss)) 0))

(defun er/mark-inside-pairs ()
  "Mark inside pairs (as defined by the mode), not including the pairs."
  (interactive)
  (when (er--point-inside-pairs-p)
    (goto-char (nth 1 (syntax-ppss)))
    (set-mark (save-excursion
                (forward-char 1)
                (skip-chars-forward er--space-str)
                (point)))
    (forward-list)
    (backward-char)
    (skip-chars-backward er--space-str)
    (exchange-point-and-mark)))

(defun er--looking-at-pair ()
  "Is point looking at an opening pair char?"
  (looking-at "\\s("))

(defun er--looking-at-marked-pair ()
  "Is point looking at a pair that is entirely marked?"
  (and (er--looking-at-pair)
       (use-region-p)
       (>= (mark)
           (save-excursion
             (forward-list)
             (point)))))

(defun er/mark-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars."
  (interactive)
  (progn
    (if (looking-back "\\s)+\\=")
        (ignore-errors (backward-list 1))
      (skip-chars-forward er--space-str)))
  (when (and (er--point-inside-pairs-p)
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
                           er/mark-next-accessor
                           er/mark-method-call
                           er/mark-comment
                           er/mark-comment-block
                           er/mark-inside-quotes
                           er/mark-outside-quotes
                           er/mark-inside-pairs
                           er/mark-outside-pairs))

;; The magic expand-region method

;;;###autoload
(defun er/expand-region (arg)
  "Increase selected region by semantic units.
Basically it runs all the mark-functions in `er/try-expand-list'
and chooses the one that increases the size of the region while
moving point or mark as little as possible.

With prefix argument expands the region that many times.
If prefix argument is negative calls `er/contract-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time."
  (interactive "p")
  (if (< arg 1)
      ;; `er/contract-region' will take care of negative and 0 arguments
      (er/contract-region (- arg))
    ;; We handle everything else

    (when (and (er--first-invocation)
               (not (use-region-p)))
      (push-mark nil t)  ;; one for keeping starting position
      (push-mark nil t)) ;; one for replace by set-mark in expansions

    (when (not (eq t transient-mark-mode))
      (setq transient-mark-mode (cons 'only transient-mark-mode)))

    (while (>= arg 1)
      (setq arg (- arg 1))
      (let* ((p1 (point))
             (p2 (if (use-region-p) (mark) (point)))
             (start (min p1 p2))
             (end (max p1 p2))
             (try-list er/try-expand-list)
             (best-start 1)
             (best-end (buffer-end 1)))

        ;; add hook to clear history on buffer changes
        (unless er/history
          (add-hook 'after-change-functions 'er/clear-history t t))

        ;; remember the start and end points so we can contract later
        ;; unless we're already at maximum size
        (unless (and (= start best-start)
                     (= end best-end))
          (push (cons start end) er/history))

        (when (and (er--point-is-surrounded-by-white-space)
                   (= start end))
          (skip-chars-forward er--space-str)
          (setq start (point)))

        (while try-list
          (save-excursion
            (ignore-errors
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
                  (message "%S" (car try-list))))))
          (setq try-list (cdr try-list)))

        (goto-char best-start)
        (set-mark best-end)

        (when (and (= best-start 0)
                   (= best-end (buffer-end 1))) ;; We didn't find anything new, so exit early
          (setq arg 0))))))

(defun er/contract-region (arg)
  "Contract the selected region to its previous size.
With prefix argument contracts that many times.
If prefix argument is negative calls `er/expand-region'.
If prefix argument is 0 it resets point and mark to their state
before calling `er/expand-region' for the first time."
  (interactive "p")
  (if (< arg 0)
      (er/expand-region (- arg))
    (when er/history
      ;; Be sure to reset them all if called with 0
      (when (= arg 0)
        (setq arg (length er/history)))

      (when (not transient-mark-mode)
        (setq transient-mark-mode (cons 'only transient-mark-mode)))

      ;; Advance through the list the desired distance
      (while (and (cdr er/history)
                  (> arg 1))
        (setq arg (- arg 1))
        (setq er/history (cdr er/history)))
      ;; Reset point and mark
      (let* ((last (pop er/history))
             (start (car last))
             (end (cdr last)))
        (goto-char start)
        (set-mark end)
        (when (eq start end)
          (deactivate-mark)
          (er/clear-history))))))

(defun er/clear-history (&rest args)
  "Clear the history."
  (setq er/history '())
  (remove-hook 'after-change-functions 'er/clear-history t))

(defsubst er--first-invocation ()
  "t if this is the first invocation of er/expand-region or er/contract-region"
  (not (memq last-command '(er/expand-region er/contract-region))))

(defun er--point-is-surrounded-by-white-space ()
  (and (or (memq (char-before) er--blank-list)
           (eq (point) (point-min)))
       (memq (char-after) er--blank-list)))

;; Mode-specific expansions
(require 'js-mode-expansions)
(require 'ruby-mode-expansions)
(require 'html-mode-expansions)
(require 'css-mode-expansions)
(require 'clojure-mode-expansions)
(require 'python-mode-expansions)
(require 'text-mode-expansions)
(require 'latex-mode-expansions)
(require 'erlang-mode-expansions)
(require 'feature-mode-expansions)

(provide 'expand-region)

;;; expand-region.el ends here
