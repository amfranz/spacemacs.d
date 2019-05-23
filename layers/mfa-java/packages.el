;; -*- lexical-binding: t -*-

(defconst mfa-java-packages '(cc-mode
                              flycheck
                              kotlin-mode
                              lsp-java))

(defun mfa-java/post-init-cc-mode ()
  (add-hook 'java-mode-hook #'mfa-java//setup-java-mode t))

(defun mfa-java/post-init-flycheck ()
  (add-hook 'java-mode-hook #'flycheck-mode)
  (add-hook 'kotlin-mode-hook #'flycheck-mode))

(defun mfa-java/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t))

(defun mfa-java/post-init-lsp-java ()
  (setq lsp-java-autobuild-enabled nil))
