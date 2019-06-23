;; -*- lexical-binding: t -*-

(defun yadm-status ()
  "Show the status of the yadm Git repository in a buffer."
  (interactive)
  (magit-status "/yadm::"))

(defun yadm--magit-todos-manual-scan-ad (orig-fun &rest args)
  "Turn off automatic scanning by `magit-todos' in `magit-status' buffers that
visit the yadm repository.

Since the working directory of the yadm repository is the users home directory,
a scan would search through all the users files, not just those managed by
yadm."
  (if (string-prefix-p "/yadm:" default-directory)
      (let (magit-todos-update)
        (apply orig-fun args))
    (apply orig-fun args)))
