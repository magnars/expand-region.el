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

