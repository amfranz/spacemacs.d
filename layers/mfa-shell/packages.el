(defconst mfa-shell-packages '(bash-completion eterm-256color shell term))

(defun mfa-shell/init-bash-completion ()
  (use-package bash-completion
    :if (configuration-layer/package-usedp 'shell)
    :defer t))

(defun mfa-shell/init-eterm-256color ()
  (use-package eterm-256color
    :if (configuration-layer/package-usedp 'shell)
    :defer t
    :init (add-hook 'term-mode-hook #'eterm-256color-mode)))

(defun mfa-shell/post-init-shell ()
  (when (configuration-layer/package-usedp 'bash-completion)
    (with-eval-after-load 'shell
      (add-hook 'shell-dynamic-complete-functions
                #'bash-completion-dynamic-complete))))

(defun mfa-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-set-initial-state 'term-mode 'emacs)))
