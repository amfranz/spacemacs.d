;; -*- lexical-binding: t -*-

(defun mfa-org/org-index (prefix)
  "Open the org index.
Giving the command a PREFIX arg will open the index in another window."
  (interactive "P")
  (let ((index (concat org-directory "index.org")))
    (if prefix
        (find-file-other-window index)
      (find-file index))))
