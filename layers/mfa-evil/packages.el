;; -*- lexical-binding: t -*-

(defconst mfa-evil-packages '(evil
                              (evil-escape :excluded t)
                              evil-nerd-commenter))

(defun mfa-evil/post-init-evil ()
  ;; Do not move cursor back when exiting insert mode.
  (setq evil-move-cursor-back nil))

(defun mfa-evil/post-init-evil-nerd-commenter ()
  ;; Register text object for comments.
  (autoload 'evilnc-inner-comment "evil-nerd-commenter-operator")
  (define-key evil-inner-text-objects-map "c" #'evilnc-inner-comment)
  (autoload 'evilnc-outer-commenter "evil-nerd-commenter-operator")
  (define-key evil-outer-text-objects-map "c" #'evilnc-outer-commenter))
