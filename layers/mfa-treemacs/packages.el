(defconst mfa-treemacs-packages
  '((treemacs
     :location (recipe
                :fetcher github
                :repo "amfranz/treemacs"
                :commit "645b3ac375d6f859599cd7fbbd10d0803ff24d90"
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
      (spacemacs/set-leader-keys "ft" #'mfa-treemacs/find-file-select-window)
      (advice-add 'winum-select-window-0-or-10 :override #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-silent-refresh t)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-resize-icons (* 16 (display-scaling-factor))))))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after (treemacs)
    :config
    (progn
      (define-key evil-treemacs-state-map (kbd "gi") #'treemacs-change-root)
      (define-key evil-treemacs-state-map (kbd "gu") #'treemacs-uproot)
      (define-key evil-treemacs-state-map (kbd "h") #'mfa-treemacs/goto-parent-node-maybe)
      (define-key evil-treemacs-state-map (kbd "l") #'treemacs-TAB-action))))

(defun mfa-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :commands (treemacs-projectile-create-header)
    :init
    (progn
      (setq treemacs-header-function #'treemacs-projectile-create-header)
      (spacemacs/set-leader-keys "pt" #'treemacs-projectile))))
