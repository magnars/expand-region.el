(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq expand-region-root-path project-directory)
  (setq expand-region-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path expand-region-root-path)
(add-to-list 'load-path (expand-file-name "espuds" expand-region-util-path))

(require 'expand-region)
(require 'espuds)
(require 'ert)

(Before
 (switch-to-buffer
  (get-buffer-create "*expand-region*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (deactivate-mark))

(After)
