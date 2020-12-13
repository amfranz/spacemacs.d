;; -*- lexical-binding: t -*-

(defconst my-trashed-packages '(trashed))

(defun my-trashed/init-trashed ()
  (use-package trashed
    :defer t
    :init (spacemacs/safe-set-leader-keys "aT" #'trashed)))
