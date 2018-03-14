(defconst mfa-org-packages '(fontawesome helm-org-rifle helm-orgcard org orglink poporg))

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

(defun mfa-org/post-init-org ()
  ;; automatically indent org sections.
  (setq org-startup-indented t)

  ;; share org files over Dropbox.
  (setq org-directory "~/Dropbox/Workspace/org/")

  ;; set up agendas.
  (setq org-agenda-files (list (concat org-directory "agenda/"))
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file)

  ;; additional leader key bindings for org functionality.
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)

  ;; ditaa converts ascii images to real images.
  (with-eval-after-load 'org
    (push '(ditaa . t) org-babel-load-languages)
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

  ;; graphviz creates graphs based on descriptions.
  (with-eval-after-load 'org
    (push '(dot . t) org-babel-load-languages)
    (push '("dot" . graphviz-dot) org-src-lang-modes))

  ;; plantuml creates UML diagrams.
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        plantuml-output-type "png")
  (setq org-plantuml-jar-path plantuml-jar-path)
  (with-eval-after-load 'org
    (push '(plantuml . t) org-babel-load-languages)
    (push '("plantuml" . plantuml) org-src-lang-modes))

  ;; extra keybindings for org functionality.
  (spacemacs/set-leader-keys "oi" #'mfa-org/org-index))

(defun mfa-org/init-orglink ()
  (use-package orglink
    :defer t
    :init
    (add-hook 'prog-mode-hook #'orglink-mode)
    :config
    (spacemacs|hide-lighter orglink-mode)))

(defun mfa-org/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/set-leader-keys "xp" #'poporg-dwim)))
