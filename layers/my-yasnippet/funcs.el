;; -*- lexical-binding: t -*-

(defun my-yasnippet//load-yasnippet-unless-scratch ()
  (unless (string= (buffer-name) "*scratch*")
    (spacemacs/load-yasnippet)))
