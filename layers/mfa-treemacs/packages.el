(defconst mfa-treemacs-packages
  '((treemacs
     :location (recipe
                :fetcher github
                :repo "amfranz/treemacs"
                :files (:defaults
                        "icons"
                        "src/elisp/treemacs*.el"
                        "src/scripts/treemacs*.py"
                        (:exclude "src/elisp/treemacs-evil.el"
                                  "src/elisp/treemacs-projectile.el"))))
    treemacs-evil treemacs-projectile))

(defun mfa-treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (progn
      (setq treemacs-resize-icons (* 16 (display-scaling-factor)))
      (spacemacs/set-leader-keys "ft" #'mfa-treemacs/find-file-select-window)
      (advice-add 'winum-select-window-0-or-10 :override #'treemacs-select-window))
    :config
    (progn
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t))))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after (treemacs)
    :config
    (progn
      (define-key evil-treemacs-state-map (kbd "gi") #'treemacs-change-root)
      (define-key evil-treemacs-state-map (kbd "gu") #'treemacs-uproot)
      (define-key evil-treemacs-state-map (kbd "h") #'mfa-treemacs/goto-parent-node-maybe)
      (define-key evil-treemacs-state-map (kbd "l") #'treemacs-RET-action))))

(defun mfa-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :commands (treemacs-projectile-create-header)
    :init
    (progn
      (setq treemacs-header-function #'treemacs-projectile-create-header)
      (spacemacs/set-leader-keys "pt" #'treemacs-projectile))))
