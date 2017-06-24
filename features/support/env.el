(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq expand-region-root-path project-directory))

(add-to-list 'load-path expand-region-root-path)

(require 'undercover)
(undercover "*.el")

(require 'expand-region)
(require 'espuds)
(require 'ert)
(require 'merlin)

;; GNU Emacs 25.1.1 automatically enters debug-on-error for some reason and
;; tests become extremely unreadable.
(setq debug-on-error nil)

(Before
 (global-set-key (kbd "C-@") 'er/expand-region)
 (global-set-key (kbd "C-S-@") 'er/contract-region)

 (switch-to-buffer (create-file-buffer "expand-region"))
 ;; merlin-mode-expansion gets upset if the buffer is not backed by file
 (write-file (make-temp-file "expand-region" nil nil))

 (fundamental-mode)
 (transient-mark-mode 1)
 (cua-mode 0)
 (setq er--show-expansion-message t)
 (setq expand-region-smart-cursor nil)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
;; Local Variables:
;; no-byte-compile: t
;; End:
