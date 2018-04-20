(defun mfa-treemacs/find-file-select-window ()
  "Finds the current file in Treemacs and selects the Treemacs window."
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))

(defun mfa-treemacs/goto-parent-node-maybe ()
  "Select parent of selected node, if possible."
  (interactive)
  (-some-> (treemacs-current-button) (button-get :parent) (goto-char)))
