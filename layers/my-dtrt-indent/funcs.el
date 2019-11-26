;; -*- lexical-binding: t -*-

(defun dtrt-indent//maybe-enable ()
  "Enable `dtrt-indent' unless the current major mode is blacklisted by
`dtrt-indent-mode-blacklist'."
  (unless (or (apply #'derived-mode-p dtrt-indent-mode-blacklist)
              (string= (buffer-name) "*scratch*"))
    (dtrt-indent-mode)))

(defun dtrt-indent//adjust-evil-shift-width ()
  "Propagate adjustment of indent level to `evil-shift-width'."
  (let ((indent-offset-variable (nth 2 (dtrt-indent--search-hook-mapping major-mode))))
    (when (local-variable-p indent-offset-variable)
      (set (make-local-variable 'evil-shift-width) (symbol-value indent-offset-variable))
      (when (>= dtrt-indent-verbosity 1)
        (message "evil-shift-width adjusted to %d" evil-shift-width)))))
