;; -*- lexical-binding: t -*-

(defconst my-java-packages '(cc-mode
                             flycheck
                             kotlin-mode
                             lsp-java))

(defun my-java/post-init-cc-mode ()
  (add-hook 'java-mode-hook #'my-java//setup-java-mode t))

(defun my-java/post-init-flycheck ()
  (add-hook 'java-mode-hook #'flycheck-mode)
  (add-hook 'kotlin-mode-hook #'flycheck-mode))

(defun my-java/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t))

(defun my-java/post-init-lsp-java ()
  (setq lsp-java-autobuild-enabled nil))
