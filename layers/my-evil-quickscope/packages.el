;; -*- lexical-binding: t -*-

(defconst my-evil-quickscope-packages '(evil-quickscope))

(defun my-evil-quickscope/init-evil-quickscope ()
  (use-package evil-quickscope
    :after evil
    :config (global-evil-quickscope-mode)))
