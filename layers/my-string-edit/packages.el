;; -*- lexical-binding: t -*-

(defconst my-string-edit-packages '(string-edit))

(defun my-string-edit/init-string-edit ()
  (use-package string-edit
    :defer t
    :init
    (spacemacs/set-leader-keys "xs" #'string-edit-at-point)))
