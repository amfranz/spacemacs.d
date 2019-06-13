;; -*- lexical-binding: t -*-

(defconst my-lsp-packages '(lsp-mode))

(defun my-lsp/post-init-lsp-mode ()
  (setq lsp-restart 'ignore))
