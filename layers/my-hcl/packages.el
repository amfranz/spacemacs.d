;; -*- lexical-binding: t -*-

(defconst my-hcl-packages '(hcl-mode))

(defun my-hcl/init-hcl-mode ()
  (use-package hcl-mode
    :defer t))
