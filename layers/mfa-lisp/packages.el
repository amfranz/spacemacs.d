;; -*- lexical-binding: t -*-

(defconst mfa-lisp-packages '(expand-region))

(defun mfa-lisp/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'emacs-lisp-mode
                               #'mfa-lisp//add-emacs-lisp-mode-expansions)))
