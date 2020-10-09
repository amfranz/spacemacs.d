;; -*- lexical-binding: t -*-

(defconst my-pass-packages '(password-store))

(defun my-pass/post-init-password-store ()
  (advice-add 'password-store--save-field-in-kill-ring :around
              #'my-pass//no-copy-to-clipboard))
