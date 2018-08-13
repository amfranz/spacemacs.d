(defconst mfa-treemacs-packages '(treemacs treemacs-evil treemacs-projectile))

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
      (treemacs-fringe-indicator-mode t)
      (treemacs-resize-icons (* 16 (display-scaling-factor))))))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :after treemacs))

(defun mfa-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t
    :init (spacemacs/set-leader-keys "pt" #'treemacs-projectile)))
