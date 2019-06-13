;; -*- lexical-binding: t -*-

(defconst my-lice-packages '(lice))

(defun my-lice/init-lice ()
  (use-package lice
    :defer t
    :init
    (spacemacs/set-leader-keys "ol" #'lice)))
