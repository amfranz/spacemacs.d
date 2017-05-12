(defconst mfa-dired-packages '(dired-narrow))

(defun mfa-dired/init-dired-narrow ()
  (use-package dired-narrow
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode "/" #'dired-narrow)))
