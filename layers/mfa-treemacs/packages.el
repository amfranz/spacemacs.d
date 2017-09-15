(defconst mfa-treemacs-packages '(treemacs treemacs-evil treemacs-projectile))

(defun mfa-treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "fT" #'treemacs
      "ft" #'treemacs-toggle
      "pt" #'treemacs-projectile)
    :config
    (require 'treemacs-evil)
    (require 'treemacs-projectile)))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :defer t))

(defun mfa-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t))
