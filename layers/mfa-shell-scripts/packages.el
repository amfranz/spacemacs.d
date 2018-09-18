;; -*- lexical-binding: t -*-

(defconst mfa-shell-scripts-packages '(sh-script))

(defun mfa-shell-scripts/post-init-sh-script ()
  (add-hook 'sh-mode-hook #'mfa-shell-scripts//indent-tabs-mode t))
