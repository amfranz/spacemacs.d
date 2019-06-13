;; -*- lexical-binding: t -*-

(defconst my-environment-packages '(list-environment))

(defun my-environment/init-list-environment ()
  (use-package list-environment
    :defer t
    :init (spacemacs/safe-set-leader-keys "he" #'list-environment)))
