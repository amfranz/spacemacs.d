(defconst mfa-dired-packages '(all-the-icons all-the-icons-dired dired-narrow dired))

(defun mfa-dired/init-all-the-icons ()
  (use-package all-the-icons
    :defer t
    :init
    (setq inhibit-compacting-font-caches t)))

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

(defun mfa-dired/pre-init-dired ()
  (spacemacs|use-package-add-hook dired
    :post-config
    (progn
      (with-eval-after-load 'font-lock
        (require 'font-lock+))
      (evil-define-key 'evilified dired-mode-map
        (kbd "RET") #'dired-find-file-by-prefix)
      (evil-define-key 'evilified dired-mode-map
        "-" #'dired-up-directory
        "gt" #'eyebrowse-next-window-config
        "gT" #'eyebrowse-prev-window-config))))

(defun mfa-dired/post-init-dired ()
  ;; Customize sort order of files in dired.
  (setq dired-listing-switches "-ahlv --group-directories-first")

  ;; Avoid unnecessary prompts by dired.
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always))
