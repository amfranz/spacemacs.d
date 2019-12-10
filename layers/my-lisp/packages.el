;; -*- lexical-binding: t -*-

(defconst my-lisp-packages '((edebug :location built-in)
                             (emacs-lisp :location built-in)
                             evil-lisp-state
                             evil-surround
                             expand-region
                             faceup
                             font-lock-studio
                             (which-func :location built-in)))

(defun my-lisp/post-init-edebug ()
  ;; Ensures that the visual flash of the current defun always works when a
  ;; function is evaluated interactively in a lisp buffer. It doesn't always
  ;; work because it depends on the order of libraries being loaded and hooks
  ;; being registered.
  (defun my-reapply-eval-sexp-fu-advice ()
    (when (bound-and-true-p eval-sexp-fu-flash-mode)
      (esf-initialize)))
  (advice-add 'edebug-install-read-eval-functions
              :after #'my-reapply-eval-sexp-fu-advice))

(defun my-lisp/post-init-emacs-lisp ()
  (when (configuration-layer/package-used-p 'aggressive-indent)
    (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)))

(defun my-lisp/post-init-evil-surround ()
  ;; `evil-surround' should use "'" as end delimiter for "`" in lisp modes. This
  ;; brings it in line with the behavior of `smartparens', which does the same.
  (with-eval-after-load 'evil-surround
    (defun my-evil-surround-pairs-emacs-lisp-mode ()
      (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
    (add-lazy-hook 'emacs-lisp-mode #'my-evil-surround-pairs-emacs-lisp-mode)))

(defun my-lisp/post-init-evil-lisp-state ()
  (with-eval-after-load 'evil-lisp-state
    (define-key evil-lisp-state-map "A" #'my-lisp/insert-at-end-of-sexp)
    (define-key evil-lisp-state-map "I" #'my-lisp/insert-at-beginning-of-sexp)
    (define-key evil-lisp-state-map "C" #'sp-clone-sexp)))

(defun my-lisp/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'emacs-lisp-mode
                               #'my-lisp//add-emacs-lisp-mode-expansions)))

(defun my-lisp/init-faceup ()
  (use-package faceup
    :defer t))

(defun my-lisp/init-font-lock-studio ()
  (use-package font-lock-studio
    :defer t))

(defun my-lisp/init-which-func ()
  (add-hook 'spacemacs-post-user-config-hook #'add-which-function-mode-hooks))
