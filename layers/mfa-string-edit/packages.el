(defconst mfa-string-edit-packages '(string-edit))

(defun mfa-string-edit/init-string-edit ()
  (use-package string-edit
    :defer t
    :init
    (spacemacs/set-leader-keys "xs" #'string-edit-at-point)))
