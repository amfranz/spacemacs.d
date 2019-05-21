;; -*- lexical-binding: t -*-

(defconst mfa-markdown-packages '(flycheck
                                  markdown-mode))

(defun mfa-markdown/post-init-flycheck ()
  (spacemacs/enable-flycheck 'markdown-mode)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'markdown-markdownlint-cli 'proselint)))

(defun mfa-markdown/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook #'auto-fill-mode))
