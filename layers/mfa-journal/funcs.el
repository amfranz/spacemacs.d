;; -*- lexical-binding: t -*-

(defun mfa-journal//save-carryover ()
  (unless (file-exists-p (buffer-file-name))
    (save-buffer)))

(defun mfa-journal/org-journal-today ()
  (interactive)
  (org-journal-new-entry t))
