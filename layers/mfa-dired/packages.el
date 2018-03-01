(defconst mfa-dired-packages '(all-the-icons all-the-icons-dired dired-narrow dired))

(defun mfa-dired/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun mfa-dired/init-all-the-icons-dired ()
  (use-package all-the-icons-dired
    :defer t
    :diminish
    :init
    (with-eval-after-load 'dired
      (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))))

(defun mfa-dired/init-dired-narrow ()
  (use-package dired-narrow
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'dired-mode "/" #'dired-narrow)))

(defun mfa-dired/post-init-dired ()
  (spacemacs|use-package-add-hook dired
    :post-config
    (progn
      (add-hook 'dired-mode-hook #'dired-dwim-target-mode)
      (spacemacs/set-leader-keys-for-major-mode 'dired-mode
        "ot" #'dired-dwim-target-mode)
      (evil-define-key 'evilified dired-mode-map
        (kbd "RET") #'dired-find-file-by-prefix)
      (evil-define-key 'evilified dired-mode-map
        "-" #'dired-up-directory))))
