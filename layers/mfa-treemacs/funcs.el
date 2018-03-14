(defun mfa-treemacs/display-scaling-factor ()
  "Reads the display scaling factor from the Cinnamon dconf database.
This will return 2 on Hi-DPI displays, 1 otherwise."
  (string-to-number
   (string-trim-right
    (shell-command-to-string
     "dconf read /org/cinnamon/active-display-scale"))))

(defun mfa-treemacs/find-file-select-window ()
  "Finds the current file in Treemacs and selects the Treemacs window."
  (interactive)
  (treemacs-find-file)
  (treemacs-select-window))

(defun mfa-treemacs/goto-parent-node-maybe ()
  "Select parent of selected node, if possible."
  (interactive)
  (-some-> (treemacs-current-button) (button-get :parent) (goto-char)))
