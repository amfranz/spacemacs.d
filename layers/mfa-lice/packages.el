;; -*- lexical-binding: t -*-

(defconst mfa-lice-packages '(lice))

(defun mfa-lice/init-lice ()
  (use-package lice
    :defer t
    :init
    (spacemacs/set-leader-keys "ol" #'lice)))
