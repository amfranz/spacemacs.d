;; -*- lexical-binding: t -*-

(defconst my-visual-ascii-mode-packages '(visual-ascii-mode))

(defun my-visual-ascii-mode/init-visual-ascii-mode ()
  (use-package visual-ascii-mode
    :defer t))
