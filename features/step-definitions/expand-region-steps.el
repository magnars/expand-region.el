(When "I expand the region"
      (lambda ()
        (flet ((message (&rest args) nil))
          (er/expand-region 1))))
