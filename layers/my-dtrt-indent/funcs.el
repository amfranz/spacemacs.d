;; -*- lexical-binding: t -*-

(defun my-dtrt-indent//maybe-enable ()
  "Enable `dtrt-indent' unless the current major mode is blacklisted by
`dtrt-indent-mode-blacklist'."
  (when (and buffer-file-name
             (not (apply #'derived-mode-p dtrt-indent-mode-blacklist)))
    (dtrt-indent-mode)))
