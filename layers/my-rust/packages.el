;; -*- lexical-binding: t -*-

(defconst my-rust-packages '(lsp-mode
                             rust-mode
                             rust-rustfmt))

(defun my-rust/post-init-lsp-mode ()
  ;; Save CPU cycles by avoiding compilation after every keypress.
  ;; It does cause the laptop to get hot fast.
  (setq lsp-rust-wait-to-build 1500))

(defun my-rust/post-init-rust-mode ()
  (add-hook 'rust-mode-hook #'my-rust//sync-fill-column-with-rustcmd))

(defun my-rust/post-init-rust-rustfmt ()
  ;; When automatic rustfmt on save is turned on and there are syntax errors in
  ;; the content of the file, rustfmt will open a buffer and move the cursor to
  ;; the issue. This is a very annoying behavior for a chronic saver like me.
  ;; Let's not do that.
  (setq rust-format-show-buffer nil
        rust-format-goto-problem nil))
