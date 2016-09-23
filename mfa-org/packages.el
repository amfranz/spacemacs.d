(defconst mfa-org-packages '(fontawesome helm-org-rifle helm-orgcard org org-indent))

(defun mfa-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/set-leader-keys "xf" #'helm-fontawesome)))

(defun mfa-org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :init
    (progn
      (defun mfa-org/helm-org-rifle-org-directory ()
        "Rifle through the org directory."
        (interactive)
        (helm-org-rifle-directories `(,org-directory)))
      (spacemacs/set-leader-keys "o/" #'mfa-org/helm-org-rifle-org-directory))))

(defun mfa-org/init-helm-orgcard ()
  (use-package helm-orgcard
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "?" #'helm-orgcard)))

(defun mfa-org/post-init-org ()
  ;; share org files over Dropbox.
  (setq org-directory "~/Dropbox/Org/")

  ;; install a shortcut to be able to quickly open the top org file.
  (defun mfa-org/org-index ()
    (interactive)
    (find-file (concat org-directory "index.org")))
  (spacemacs/set-leader-keys "oi" #'mfa-org/org-index)

  (with-eval-after-load 'org
    ;; Set up agendas.
    (setq org-agenda-files (list (concat org-directory "agenda/")))

    ;; Enable additional languages babel should handle.
    (org-babel-do-load-languages
      'org-babel-load-languages
        '((ditaa . t)
          (dot . t)
          (emacs-lisp . t)))

    ;; ditaa converts ascii images to real images.
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

    ;; graphviz creates graphs based on descriptions.
    (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))

(defun mfa-org/post-init-org-indent ()
  (with-eval-after-load 'org-indent
    (spacemacs|hide-lighter 'org-indent-mode)))
