(defconst mfa-poporg-packages '(poporg))

(defun mfa-poporg/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/set-leader-keys "xp" #'poporg-dwim)))
