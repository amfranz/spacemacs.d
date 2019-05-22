;; -*- lexical-binding: t -*-

(defconst mfa-evil-textobj-anyblock-packages '(evil-textobj-anyblock))

(defun mfa-evil-textobj-anyblock/init-evil-textobj-anyblock ()
  (use-package evil-textobj-anyblock
    :defer t
    :init
    (progn
      (define-key evil-inner-text-objects-map "b"
        #'evil-textobj-anyblock-inner-block)
      (define-key evil-outer-text-objects-map "b"
        #'evil-textobj-anyblock-a-block))))
