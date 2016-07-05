(require 'expand-region-core)

(declare-function fsharp-mark-phrase "fsharp-mode")

(defun er/add-fsharp-mode-expansions ()
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list '(fsharp-mark-phrase))))

(er/enable-mode-expansions 'fsharp-mode 'er/add-fsharp-mode-expansions)

(provide 'fsharp-mode-expansions)
