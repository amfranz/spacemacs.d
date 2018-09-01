(defconst mfa-treemacs-packages '(treemacs))

(defun mfa-treemacs/post-init-treemacs ()
  (setq treemacs-silent-refresh t
        treemacs-resize-icons (* 16 (display-scaling-factor)))
  (with-eval-after-load 'treemacs
    (treemacs-fringe-indicator-mode t)))
