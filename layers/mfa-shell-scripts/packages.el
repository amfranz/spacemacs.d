;; -*- lexical-binding: t -*-

(defconst mfa-shell-scripts-packages
  '(sh-script flycheck flycheck-checkbashisms))

(defun mfa-shell-scripts/init-flycheck-checkbashisms ()
  (use-package flycheck-checkbashisms
    :if (configuration-layer/package-usedp 'flycheck)
    :defer t))

(defun mfa-shell-scripts/post-init-flycheck ()
  (with-eval-after-load 'sh-script
    (flycheck-checkbashisms-setup)))

(defun mfa-shell-scripts/post-init-sh-script ()
  (add-hook 'sh-mode-hook #'mfa-shell-scripts//indent-tabs-mode t))
