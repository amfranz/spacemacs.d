;; -*- lexical-binding: t -*-

(defconst my-lisp-packages '((emacs-lisp :location built-in)
                             evil-lisp-state
                             expand-region))

(defun my-lisp/post-init-emacs-lisp ()
  (when (configuration-layer/package-used-p 'aggressive-indent)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(defun my-lisp/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map "A" #'my-lisp/insert-at-end-of-sexp)
    (define-key evil-lisp-state-map "I" #'my-lisp/insert-at-beginning-of-sexp)
    (define-key evil-lisp-state-map "C" #'sp-clone-sexp)))

(defun my-lisp/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'emacs-lisp-mode
                               #'my-lisp//add-emacs-lisp-mode-expansions)))
