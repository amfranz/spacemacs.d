;; -*- lexical-binding: t -*-

(defconst mfa-visual-ascii-mode-packages '(visual-ascii-mode))

(defun mfa-visual-ascii-mode/init-visual-ascii-mode ()
  (use-package visual-ascii-mode
    :defer t))
