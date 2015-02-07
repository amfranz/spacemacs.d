(defconst mfa-shell-packages '(bash-completion multi-term shell term))

(defun mfa-shell/init-bash-completion ()
  (use-package bash-completion
    :if (configuration-layer/package-usedp 'shell)
    :defer t))

(defun mfa-shell/post-init-multi-term ()
  (with-eval-after-load 'multi-term
    (dolist (key '("C-r" "C-s" "M-r" "C-n" "C-p"))
      (setq term-bind-key-alist (delq (assoc "C-r" term-bind-key-alist) term-bind-key-alist)))
    (push '("C-r" . term-send-reverse-search-history) term-bind-key-alist)))

(defun mfa-shell/post-init-shell ()
  (when (configuration-layer/package-usedp 'bash-completion)
    (with-eval-after-load 'shell
      (add-hook 'shell-dynamic-complete-functions
                #'bash-completion-dynamic-complete))))

(defun mfa-shell/post-init-term ()
  (with-eval-after-load 'term
    (evil-set-initial-state 'term-mode 'emacs)))
