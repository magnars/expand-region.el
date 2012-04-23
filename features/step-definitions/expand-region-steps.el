(Given "^mark is inactive by default$"
       (lambda ()
         (setq set-mark-default-inactive t)))

(When "^I expand the region$"
      (lambda ()
        (flet ((message (&rest args) nil))
          (er/expand-region 1))))

(When "^I quit$"
      (lambda ()
        (flet ((signal (&rest args) nil))
          (keyboard-quit))))

(When "^I expand the region \\([0-9]+\\) times$"
      (lambda (arg)
        (flet ((message (&rest args) nil))
          (er/expand-region (string-to-number arg)))))

(And "^I contract the region$"
     (lambda ()
       (er/contract-region 1)))

(When "^I place the cursor after \"\\(.+\\)\"$"
      (lambda (arg)
        (goto-char (point-min))
        (let ((search (search-forward arg nil t))
              (message "Can not place cursor after '%s', because there is no such point: '%s'"))
          (assert search nil message arg (espuds-buffer-contents)))))

(When "^I pop the mark$"
      (lambda ()
        (set-mark-command 4)))

(When "^I deactivate the mark$"
      (lambda ()
        (deactivate-mark)))

(When "^I activate the mark$"
      (lambda ()
        (activate-mark)))

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
