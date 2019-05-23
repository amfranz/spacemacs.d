;; -*- lexical-binding: t -*-

(defconst mfa-lsp-packages '(lsp-mode))

(defun mfa-lsp/post-init-lsp-mode ()
  (setq lsp-restart 'ignore))
