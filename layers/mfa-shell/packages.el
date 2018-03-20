(defconst mfa-shell-packages '(eterm-256color term))

(defun mfa-shell/init-eterm-256color ()
  (use-package eterm-256color
    :hook (term-mode . eterm-256color-mode)))

(defun mfa-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-set-initial-state 'term-mode 'emacs)
    (define-key term-raw-map (kbd "s-v") #'term-paste)))
