;; -*- lexical-binding: t -*-

(defun mfa-fill-function-arguments//emacs-lisp-mode ()
  (setq-local fill-function-arguments-first-argument-same-line t)
  (setq-local fill-function-arguments-second-argument-same-line t)
  (setq-local fill-function-arguments-last-argument-same-line t)
  (setq-local fill-function-arguments-argument-separator " "))

(defun mfa-fill-function-arguments//prog-mode ()
  (local-set-key (kbd "M-q") #'fill-function-arguments-dwim))

(defun mfa-fill-function-arguments//sgml-mode ()
  (setq-local fill-function-arguments-first-argument-same-line t)
  (setq-local fill-function-arguments-argument-sep " ")
  (local-set-key (kbd "M-q") #'fill-function-arguments-dwim))
