;; -*- lexical-binding: t -*-

(defconst my-rust-packages '(rust-mode))

(defun my-rust/post-init-rust-mode ()
  ;; Sets `fill-column' to the rustfmt default value of max_width.
  (add-hook 'rust-mode-hook #'my-rust//sync-fill-column-with-rustcmd)

  ;; Automatically format rust files on save.
  (setq rust-format-on-save t
        rust-rustfmt-switches '("--edition" "2021"))

  ;; When automatic rustfmt on save is turned on and there are syntax errors in
  ;; the content of the file, rustfmt will open a buffer and move the cursor to
  ;; the issue. This is a very annoying behavior for a chronic saver like me.
  ;; Let's not do that.
  (setq rust-format-show-buffer nil
        rust-format-goto-problem nil)

  ;; Enable rust-analyzers inlay hints mode, but hide it by default and
  ;; establish a key binding to toggle it.
  (when (configuration-layer/layer-used-p 'lsp)
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (add-hook 'lsp-rust-analyzer-after-open-hook
              #'my-rust//lsp-rust-analyzer-unblock-inlay-hints)
    (spacemacs/safe-set-leader-keys-for-major-mode 'rust-mode
      "Tli" #'lsp-rust-analyzer-inlay-hints-mode)))
