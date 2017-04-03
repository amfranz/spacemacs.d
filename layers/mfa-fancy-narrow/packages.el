(defconst mfa-fancy-narrow-packages '(fancy-narrow))

(defun mfa-fancy-narrow/init-fancy-narrow ()
  (use-package fancy-narrow
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "nF" #'fancy-narrow-to-defun)
      (spacemacs/set-leader-keys "nP" #'fancy-narrow-to-page)
      (spacemacs/set-leader-keys "nR" #'fancy-narrow-to-region)
      (spacemacs/set-leader-keys "nW" #'fancy-widen))))
