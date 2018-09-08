;; -*- lexical-binding: t -*-

(defun mfa-org/org-index (prefix)
  "Open the org index.
Giving the command a PREFIX arg will open the index in another window."
  (interactive "P")
  (let ((index (concat org-directory "index.org")))
    (if prefix
        (find-file-other-window index)
      (find-file index))))

(defun mfa-org//custom-theme-set-variables ()
  (when (eq 'zenburn spacemacs--cur-theme)
    (zenburn-with-color-variables
      (custom-theme-set-variables
       'zenburn
       `(org-priority-faces '((?A . (:foreground ,zenburn-magenta))
                              (?B . (:foreground ,zenburn-yellow))
                              (?C . (:foreground ,zenburn-cyan))))))))
