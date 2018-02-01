(defconst mfa-shell-packages '(bash-completion eterm-256color multi-term shell term))

(defun mfa-shell/init-bash-completion ()
  (use-package bash-completion
    :if (configuration-layer/package-usedp 'shell)
    :defer t))

(defun mfa-shell/init-eterm-256color ()
  (use-package eterm-256color
    :if (configuration-layer/package-usedp 'shell)
    :defer t
    :init (add-hook 'term-mode-hook #'eterm-256color-mode)))

(defun mfa-shell/post-init-multi-term ()
  (with-eval-after-load 'multi-term
    (dolist (key '("C-r" "C-s" "M-r" "C-n" "C-p"))
      (setq term-bind-key-alist (delq (assoc key term-bind-key-alist) term-bind-key-alist)))
    (push '("C-r" . term-send-reverse-search-history) term-bind-key-alist)
    (push '("M-DEL" . term-send-backward-kill-word) term-bind-key-alist)))

(defun mfa-shell/post-init-shell ()
  (when (configuration-layer/package-usedp 'bash-completion)
    (with-eval-after-load 'shell
      (add-hook 'shell-dynamic-complete-functions
                #'bash-completion-dynamic-complete))))

(defun mfa-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-set-initial-state 'term-mode 'emacs)))
