;; -*- lexical-binding: t -*-

(defconst my-lsp-packages '(company-lsp
                            lsp-mode
                            lsp-treemacs))

(defun my-lsp/post-init-company-lsp ()
  (setq company-lsp-cache-candidates 'auto))

(defun my-lsp/post-init-lsp-mode ()
  (setq lsp-restart 'ignore))

(defun my-lsp/post-init-lsp-treemacs ()
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'lsp-mode
      (lsp-treemacs-sync-mode))))
