;; -*- lexical-binding: t -*-

(setq my-multi-line-packages '(multi-line))

(defun my-multi-line/init-multi-line ()
  (use-package multi-line
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "xmm" #'multi-line)))
