(define-minor-mode dired-dwim-target-mode ()
  "Toggle dired-dwim-target mode."
  :lighter "ⓣ"
  (set (make-local-variable 'dired-dwim-target) dired-dwim-target-mode))
