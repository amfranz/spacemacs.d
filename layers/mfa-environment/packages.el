;; -*- lexical-binding: t -*-

(defconst mfa-environment-packages '(list-environment))

(defun mfa-environment/init-list-environment ()
  (use-package list-environment
    :defer t
    :init (spacemacs/safe-set-leader-keys "he" #'list-environment)))
