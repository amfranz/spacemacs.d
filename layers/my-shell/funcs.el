;; -*- lexical-binding: t -*-

(defun term-send-esc ()
  "Send ESC in term mode."
  (interactive)
  (term-send-raw-string "\e"))

(defun my-shell//vterm-insert-for-yank (orig-fun string)
  (if (derived-mode-p 'vterm-mode)
      (vterm-insert string)
    (funcall orig-fun string)))
