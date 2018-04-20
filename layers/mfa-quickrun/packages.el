(defconst mfa-quickrun-packages '(quickrun))

(defun mfa-quickrun/init-quickrun ()
  (use-package quickrun
    :defer t
    :init
    (progn
      (evilified-state-evilify quickrun--mode quickrun--mode-map)
      (spacemacs/declare-prefix "ox" "quickrun")
      (spacemacs/set-leader-keys
        "oxa" #'quickrun-with-arg
        "oxc" #'quickrun-compile-only
        "oxh" #'helm-quickrun
        "oxr" #'quickrun-region
        "oxx" #'quickrun))))
