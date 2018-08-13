(defun mfa-treemacs/find-file-select-window ()
  "Finds the current file in Treemacs and selects the Treemacs window."
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))
