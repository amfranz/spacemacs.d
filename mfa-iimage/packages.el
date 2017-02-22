(defconst mfa-iimage-packages '(iimage))

(defun mfa-iimage/init-iimage ()
  (use-package iimage
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "ot" "toggles")
      (spacemacs/set-leader-keys "oti" #'iimage-mode))))
