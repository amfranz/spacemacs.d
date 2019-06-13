;; -*- lexical-binding: t -*-

(defun yadm-status ()
  "Show the status of the yadm Git repository in a buffer."
  (interactive)
  (magit-status "/yadm::"))
