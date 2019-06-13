;; -*- lexical-binding: t -*-

(defun mfa-yasnippet//load-yasnippet-unless-scratch ()
  (unless (string= (buffer-name) "*scratch*")
    (spacemacs/load-yasnippet)))
