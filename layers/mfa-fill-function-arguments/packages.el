;; -*- lexical-binding: t -*-

(defconst mfa-fill-function-arguments-packages '(fill-function-arguments))

(defun mfa-fill-function-arguments/init-fill-function-arguments ()
  (use-package fill-function-arguments
    :hook ((emacs-lisp-mode . mfa-fill-function-arguments//emacs-lisp-mode)
           (prog-mode . mfa-fill-function-arguments//prog-mode)
           (sgml-mode . mfa-fill-function-arguments//sgml-mode))))
