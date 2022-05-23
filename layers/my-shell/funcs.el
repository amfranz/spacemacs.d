;; -*- lexical-binding: t -*-

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun my-shell//helm-kill-ring-action-yank-1 (orig-fun str)
  (if (derived-mode-p 'vterm-mode)
      (vterm-send-string str t)
    (funcall orig-fun str)))
