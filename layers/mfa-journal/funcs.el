;; -*- lexical-binding: t -*-

(defun mfa-journal//save-carryover ()
  (unless (file-exists-p (buffer-file-name))
    (save-buffer)))
