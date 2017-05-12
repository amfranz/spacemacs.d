(defconst mfa-dired-packages '(dired-narrow dired))

(defun mfa-dired/init-dired-narrow ()
  (use-package dired-narrow
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode "/" #'dired-narrow)))

(defun mfa-dired/post-init-dired ()
  (spacemacs|use-package-add-hook dired
    :post-config
    (evil-define-key 'evilified dired-mode-map
      "-" #'dired-up-directory)))
