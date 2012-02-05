(When "^I expand the region$"
      (lambda ()
        (flet ((message (&rest args) nil))
          (er/expand-region 1))))

(When "^I expand the region \\([0-9]+\\) times$"
      (lambda (arg)
        (flet ((message (&rest args) nil))
          (er/expand-region (string-to-number arg)))))

(And "^I contract the region$"
     (lambda ()
       (er/contract-region 1)))

(When "^I pop the mark$"
      (lambda ()
        (set-mark-command 4)))

(When "^I deactivate the mark$"
      (lambda ()
        (deactivate-mark)))

(Then "^the region should not be active$"
       (lambda ()
         (should
          (not (region-active-p)))))

(Then "^cursor should be at point \"\\(.+\\)\"$"
      (lambda (arg)
        (should
         (=
          (string-to-number arg)
          (point)))))
