;; -*- lexical-binding: t -*-

(defconst my-multi-line-packages '(fill-function-arguments))

(defun my-multi-line/init-fill-function-arguments ()
  (use-package fill-function-arguments
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "xlf" #'fill-function-arguments-dwim)))
