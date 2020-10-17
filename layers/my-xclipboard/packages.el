;; -*- lexical-binding: t -*-

(defconst my-xclipboard-packages '(xclip))

(defun my-xclipboard/init-xclip ()
  (use-package xclip
    :diminish
    :init (spacemacs|add-toggle xclip-mode
            :mode xclip-mode
            :documentation "Enable X clipboard integration in the terminal."
            :evil-leader "tx")
    :config (xclip-mode)))
