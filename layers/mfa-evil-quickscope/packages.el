;; -*- lexical-binding: t -*-

(defconst mfa-evil-quickscope-packages '(evil-quickscope))

(defun mfa-evil-quickscope/init-evil-quickscope ()
  (use-package evil-quickscope
    :after evil
    :config (global-evil-quickscope-mode)))
