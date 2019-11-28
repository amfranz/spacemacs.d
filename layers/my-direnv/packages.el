;; -*- lexical-binding: t -*-

(defconst my-direnv-packages '(direnv))

(defun my-direnv/init-direnv ()
  (use-package direnv
    :config
    (progn
      (setq direnv-always-show-summary nil)
      (direnv-mode))))
