(defun dired-find-file-by-prefix (&optional arg)
  "In Dired, visit the file or directory named on this line.

By default the new buffer replaces the dired buffer in the current window, but a
different target window number can be specified via the prefix argument."
  (interactive "P")
  (let ((file (dired-get-file-for-visit))
        (dired-window (selected-window)))
    (when (and (integerp arg) (bound-and-true-p winum-mode))
      (winum-select-window-by-number arg))
    (if (not (file-directory-p file))
        (find-file file)
      (let ((find-file-run-dired t))
        (if (and vinegar-reuse-dired-buffer (eq dired-window (selected-window)))
            (find-alternate-file file)
          (find-file file))))))
