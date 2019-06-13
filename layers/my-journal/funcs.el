;; -*- lexical-binding: t -*-

(defun my-journal//save-carryover ()
  (unless (file-exists-p (buffer-file-name))
    (save-buffer)))

(defun my-journal/org-journal-today ()
  (interactive)
  (org-journal-new-entry t))
