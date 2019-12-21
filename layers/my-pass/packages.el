;; -*- lexical-binding: t -*-

(defconst my-pass-packages '(auth-source-pass password-store))

(defun my-pass/init-auth-source-pass ()
  (use-package auth-source-pass
    :after (auth-source)
    :config (auth-source-pass-enable)))

(defun my-pass/post-init-password-store ()
  (advice-add 'password-store--save-field-in-kill-ring :around
              #'my-pass//no-copy-to-clipboard))
