;; -*- lexical-binding: t -*-

(defconst my-rust-packages '(lsp-mode
                             rust-mode))

(defun my-rust/post-init-lsp-mode ()
  ;; Save CPU cycles by avoiding compilation after every keypress.
  ;; It does cause the laptop to get hot fast.
  (setq lsp-rust-wait-to-build 1500))

(defun my-rust/post-init-rust-mode ()
  (add-hook 'rust-mode-hook #'my-rust//sync-fill-column-with-rustcmd))
