;; -*- lexical-binding: t -*-

;; Clear search highlights when pressing Esc in normal mode.
(define-key evil-normal-state-map (kbd "<escape>")
  #'evil-force-normal-state-nohighlight)
