;; -*- lexical-binding: t -*-

(defconst my-xclipboard-packages '(xclip))

(defun my-xclipboard/init-xclip ()
  (use-package xclip
    :if (not (display-assume-graphic-p))
    :diminish
    :init (spacemacs|add-toggle xclip-mode
            :status xclip-mode
            :on (xclip-mode)
            :off (xclip-mode -1)
            :documentation "Enable X clipboard integration in the terminal."
            :evil-leader "tx")
    :config (xclip-mode)))
