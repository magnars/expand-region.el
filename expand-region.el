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

;; ## Todo
;;
;; * `er/expand-region` should take ARGS (negative contracts, 0 resets to pre-expansion state)

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
;;
;; Thanks!

;;; Code:

(defvar er/history '()
  "A history of start and end points so we can contract after expanding.")

;; history is always local to a single buffer
(make-variable-buffer-local 'er/history)

(defvar er--pushed-mark-p nil
  "t when mark has been pushed for this command.")

(defvar er--pushed-mark-p nil
  "t when mark has been pushed for this command.")

(defvar er--cmds '(er/expand-region er/contract-region))
(defvar er--space-str " \t\n")

(defsubst er--first-invocation ()
  "return t if this is the first invocation of er/* command"
  (not (memq last-command er--cmds)))

(defsubst er--is-invocation ()
  "return t if this is the first invocation of er/* command"
  (memq this-command er--cmds))

(defun er--post-command-func ()
  "function to be run on `post-command-hook'"
  (setq er--pushed-mark-p nil)
  (remove-hook 'post-command-hook 'er--post-command-func t))

(defun er--setup ()
  "push mark and add post-command-hook"

  (when (and (not er--pushed-mark-p)
             (or (not (er--is-invocation))
                 (er--first-invocation)))
    (push-mark nil t)
    (push-mark nil t))
  (setq er--pushed-mark-p t)
  (add-hook 'post-command-hook 'er--post-command-func nil t))

;; Default expansions

(defun er/mark-word ()
  "Mark the entire word around or in front of point."
  (interactive)
  (er--setup)
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
  (er--setup)
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
  (er--setup)
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

;; Mark method call (can be improved further)

(defun er/mark-method-call ()
  "Mark the current symbol (including dots) and then paren to closing paren."
  (interactive)
  (er--setup)
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
  (er--setup)
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
  (er--setup)
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
  (er--setup)
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
  (er--setup)
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
  (er--setup)
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
       (eq (mark)
           (save-excursion
             (forward-list)
             (point)))))

(defun er/mark-outside-pairs ()
  "Mark pairs (as defined by the mode), including the pair chars."
  (interactive)
  (er--setup)
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
                           er/mark-method-call
                           er/mark-comment
                           er/mark-comment-block
                           er/mark-inside-quotes
                           er/mark-outside-quotes
                           er/mark-inside-pairs
                           er/mark-outside-pairs))

;; The magic expand-region method

;;;###autoload
(defun er/expand-region ()
  "Increase selected region by semantic units.
Basically it runs all the mark-functions in the er/try-expand-list
and chooses the one that increases the size of the region while
moving point or mark as little as possible."
  (interactive)
  (er--setup)
  (let ((start (point))
        (end (if (use-region-p) (mark) (point)))
        (try-list er/try-expand-list)
        (best-start 0)
        (best-end (buffer-end 1)))

    ;; add hook to clear history on buffer changes
    (unless er/history
      (add-hook 'after-change-functions 'er/clear-history t t))

    ;; remember the start and end points so we can contract later
    (push (cons start end) er/history)

    (while try-list
      (save-excursion
        (let ((blank-list (append er--space-str nil)))
          (when (and (memq (char-before) blank-list)
                     (memq (char-after) blank-list))
            (skip-chars-forward er--space-str)
            (setq start (point))))
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

(defun er/contract-region ()
  "Contract the selected region to its previous size."
  (interactive)

  (if (and er/history
           (not (er--first-invocation)))
      (let* ((last (pop er/history))
             (start (car last))
             (end (cdr last)))
        (goto-char start)
        (set-mark end)
        (when (eq start end)
          (deactivate-mark)
          (er/clear-history)))))

(defun er/clear-history (&rest args)
  "Clear the history."
  (setq er/history '())
  (remove-hook 'after-change-functions 'er/clear-history t))

;; Mode-specific expansions
(require 'js-mode-expansions)
(require 'html-mode-expansions)
(require 'css-mode-expansions)
(require 'clojure-mode-expansions)

(provide 'expand-region)

;;; expand-region.el ends here
