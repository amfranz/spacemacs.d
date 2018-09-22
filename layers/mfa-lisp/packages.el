;; -*- lexical-binding: t -*-

(defconst mfa-lisp-packages '((emacs-lisp :location built-in)
                              expand-region))

(defun mfa-lisp/post-init-emacs-lisp ()
  (when (configuration-layer/package-used-p 'aggressive-indent)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(defun mfa-lisp/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'emacs-lisp-mode
                               #'mfa-lisp//add-emacs-lisp-mode-expansions)))
