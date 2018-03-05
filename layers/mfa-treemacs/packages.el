(defconst mfa-treemacs-packages '(treemacs treemacs-evil treemacs-projectile))

(defun mfa-treemacs/init-treemacs ()
  (use-package treemacs
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "ft" #'treemacs-select-window)
      (with-eval-after-load 'winum
        (advice-add 'winum-select-window-0-or-10 :override #'treemacs-select-window)))
    :config
    (progn
      (require 'treemacs-evil)
      (define-key evil-treemacs-state-map (kbd "gi") #'treemacs-change-root)
      (define-key evil-treemacs-state-map (kbd "gu") #'treemacs-uproot)
      (define-key evil-treemacs-state-map (kbd "h") #'treemacs-goto-parent-node)
      (define-key evil-treemacs-state-map (kbd "l") #'treemacs-RET-action)
      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t))))

(defun mfa-treemacs/init-treemacs-evil ()
  (use-package treemacs-evil
    :defer t))

(defun mfa-treemacs/init-treemacs-projectile ()
  (use-package treemacs-projectile
    :defer t
    :init
    (spacemacs/set-leader-keys "pt" #'treemacs-projectile)
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)))
