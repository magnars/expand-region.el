(require 'expand-region-core)

(defun er/lisp-mark-prefix ()
  (interactive)
  "Enhancement for lisp like #'() ,@()"
  (when (looking-at "\\(\\(`,?\\)\\|\\('?,?\\)\\|\\(,?@?\\)\\|\\(#?'?\\)\\)?(")
    (cond ((er/looking-back-on-line ",\\|@\\|?\\|#\\|'\\|`") (backward-char 1)))
    (set-mark (point))
    (forward-list)
    (exchange-point-and-mark)))

(defun er/add-lisp-mode-expansions ()
  "Adds expansions for buffers in lisp-mode"
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
	       '(er/lisp-mark-prefix))))

(er/enable-mode-expansions 'lisp-mode 'er/add-lisp-mode-expansions)
(er/enable-mode-expansions 'slime-mode 'er/add-lisp-mode-expansions)
(er/enable-mode-expansions 'slime-repl-mode 'er/add-lisp-mode-expansions)

(provide 'lisp-mode-expansions)
