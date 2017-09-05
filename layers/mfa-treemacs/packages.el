(defconst mfa-treemacs-packages '(treemacs treemacs-evil))

(defun mfa-treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (progn
      (setq treemacs-header-function #'treemacs--create-header-projectile)
      (spacemacs/set-leader-keys
        "fT" #'treemacs
        "ft" #'treemacs-toggle
        "pt" #'treemacs-projectile))
    :config
    (require 'treemacs-evil)))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :defer t))
