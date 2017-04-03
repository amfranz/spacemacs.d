(defconst mfa-pass-packages '(helm-pass))

(defun mfa-pass/init-helm-pass ()
  (use-package helm-pass
    :defer t
    :init
    (spacemacs/set-leader-keys "op" #'helm-pass)))
