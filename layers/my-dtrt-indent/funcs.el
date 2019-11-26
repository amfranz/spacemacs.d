;; -*- lexical-binding: t -*-

(defun dtrt-indent//maybe-enable ()
  "Enable `dtrt-indent' unless the current major mode is blacklisted by
`dtrt-indent-mode-blacklist'."
  (unless (or (apply #'derived-mode-p dtrt-indent-mode-blacklist)
              (string= (buffer-name) "*scratch*"))
    (dtrt-indent-mode)))
