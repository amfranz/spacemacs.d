(setq mfa-multi-line-packages '(multi-line))

(defun mfa-multi-line/init-multi-line ()
  (use-package multi-line
    :defer t
    :init
    (spacemacs/set-leader-keys "xm" #'multi-line)))
