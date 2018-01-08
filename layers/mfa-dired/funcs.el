(define-minor-mode dired-dwim-target-mode ()
  "Toggle dired-dwim-target mode."
  :lighter "ⓣ"
  (set (make-local-variable 'dired-dwim-target) dired-dwim-target-mode))

(defun mfa-dired/toggle-dwim-target ()
  (interactive)
  (setq dired-dwim-target (not dired-dwim-target)))
