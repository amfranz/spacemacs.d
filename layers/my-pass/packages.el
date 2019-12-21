;; -*- lexical-binding: t -*-

(defconst my-pass-packages '(auth-source-pass))

(defun my-pass/init-auth-source-pass ()
  (use-package auth-source-pass
    :after (auth-source)
    :config (auth-source-pass-enable)))
