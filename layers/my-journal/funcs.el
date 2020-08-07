;; -*- lexical-binding: t -*-

(defun my-journal//file-header (time)
  (concat "#+TITLE: " (format-time-string "W%W %Y" time) "\n"
          "#+STARTUP: content\n"))

(defun my-journal//set-org-journal-startup-folded ()
  (setq-local org-startup-folded 'content))

(defun my-journal//save-after-carryover ()
  (unless (file-exists-p (buffer-file-name))
    (save-buffer)))

(defun my-journal/org-journal-today ()
  (interactive)
  (org-journal-new-entry t))
