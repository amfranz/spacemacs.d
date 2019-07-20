;; -*- lexical-binding: t -*-

(defconst my-lsp-packages '(company-lsp lsp-mode))

(defun my-lsp/post-init-company-lsp ()
  (setq company-lsp-cache-candidates 'auto))

(defun my-lsp/post-init-lsp-mode ()
  (setq lsp-restart 'ignore))
