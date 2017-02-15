(defconst mfa-org-packages '(fontawesome helm-org-rifle helm-orgcard org poporg))

(defun mfa-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/set-leader-keys "xf" #'helm-fontawesome)))

(defun mfa-org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :init
    (spacemacs/set-leader-keys "o/" #'helm-org-rifle-org-directory)))

(defun mfa-org/init-helm-orgcard ()
  (use-package helm-orgcard
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "?" #'helm-orgcard)))

(defun mfa-org/org-index ()
  (interactive)
  (find-file (concat org-directory "index.org")))

(defun mfa-org/org-from-mobile ()
  (interactive)
  (find-file org-mobile-inbox-for-pull))

(defun mfa-org/post-init-org ()
  ;; share org files over Dropbox.
  (setq org-directory "~/Dropbox/Workspace/org/")

  ;; set up agendas.
  (setq org-agenda-files (list (concat org-directory "agenda/")))

  ;; set up org-mobile.
  (autoload 'org-mobile-pull "org")
  (autoload 'org-mobile-push "org")

  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg/"
        org-mobile-inbox-for-pull (concat org-directory "from-mobile.org")
        org-mobile-files (list 'org-agenda-files (concat org-directory "trove/")))

  (spacemacs/declare-prefix "om" "org-mobile")
  (spacemacs/set-leader-keys "omc" #'mfa-org/org-from-mobile)
  (spacemacs/set-leader-keys "omf" #'org-mobile-pull)
  (spacemacs/set-leader-keys "omp" #'org-mobile-push)

  ;; set up org-babel.
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      ;; enable additional languages babel should handle.
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((ditaa . t)
         (dot . t)
         (emacs-lisp . t)))

      ;; ditaa converts ascii images to real images.
      (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

      ;; graphviz creates graphs based on descriptions.
      (push '("dot" . graphviz-dot) org-src-lang-modes)))

  ;; extra keybindings for org functionality.
  (spacemacs/set-leader-keys "oi" #'mfa-org/org-index))

(defun mfa-org/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/set-leader-keys "xp" #'poporg-dwim)))
