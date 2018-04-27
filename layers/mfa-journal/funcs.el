;; -*- lexical-binding: t -*-

(defun view-journal ()
  (interactive)
  (org-journal-new-entry t))

(defun search-all-journals ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively 'org-journal-search))
