;; -*- lexical-binding: t -*-

(defvar-local lsp--rust-analyzer-unblock-inlay-hints nil)

(defun my-rust//lsp-rust-analyzer-unblock-inlay-hints ()
  (setq lsp--rust-analyzer-unblock-inlay-hints t))

(defun my-rust//lsp-rust-analyzer-hide-inlay-hints-by-default (orig-fun &rest args)
  (when lsp--rust-analyzer-unblock-inlay-hints
    (apply orig-fun args)))

(advice-add 'lsp-rust-analyzer-inlay-hints-mode
            :around #'my-rust//lsp-rust-analyzer-hide-inlay-hints-by-default)

(defun my-rust//sync-fill-column-with-rustcmd ()
  "Sets `fill-column' to the rustfmt default value of max_width."
  (setq fill-column 100))
