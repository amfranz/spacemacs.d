;; -*- lexical-binding: t -*-

(defconst mfa-lisp-packages '((emacs-lisp :location built-in)
                              evil-lisp-state
                              expand-region))

(defun mfa-lisp/post-init-emacs-lisp ()
  (when (configuration-layer/package-used-p 'aggressive-indent)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(defun mfa-lisp/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map "A" #'mfa-lisp/insert-at-end-of-sexp)
    (define-key evil-lisp-state-map "I" #'mfa-lisp/insert-at-beginning-of-sexp)
    (define-key evil-lisp-state-map "C" #'sp-clone-sexp)))

(defun mfa-lisp/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'emacs-lisp-mode
                               #'mfa-lisp//add-emacs-lisp-mode-expansions)))
