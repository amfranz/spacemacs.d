;; -*- lexical-binding: t -*-

(defconst mfa-org-packages '(fontawesome
                             helm-ag
                             helm-org-rifle
                             helm-orgcard
                             org
                             org-trello
                             orglink
                             plantuml
                             poporg))

(defun mfa-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "of" #'helm-fontawesome)))

(defun mfa-org/post-init-helm-ag ()
  ;; Automatically unfold the `org-mode' section at point after following a
  ;; match found by `helm-ag'.
  (advice-add 'helm-ag--find-file-action :after #'mfa-org//org-show-entry))

(defun mfa-org/init-helm-org-rifle ()
  (use-package helm-org-rifle
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "o/" #'helm-org-rifle-org-directory)))

(defun mfa-org/init-helm-orgcard ()
  (use-package helm-orgcard
    :defer t
    :init
    (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
      "?" #'helm-orgcard)))

(defun mfa-org/pre-init-org ()
  ;; customize org priority faces.
  (add-hook 'spacemacs-post-theme-change-hook
            #'mfa-org//custom-theme-set-variables))

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
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)

  ;; ditaa converts ascii images to real images.
  (with-eval-after-load 'org
    (push '(ditaa . t) org-babel-load-languages)
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

  ;; configure org-download.
  (setq org-download-heading-lvl nil)

  ;; extra keybindings for org functionality.
  (spacemacs/safe-set-leader-keys "oi" #'mfa-org/org-index)
  (spacemacs/declare-prefix-for-mode 'org-mode "mot" "toggles")
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "oti" #'org-toggle-inline-images))

(defun mfa-org/post-init-org-trello ()
  (setq org-trello-input-completion-mechanism 'helm)
  (with-eval-after-load 'org-trello
    (setq orgtrello-log-level orgtrello-log-warn)))

(defun mfa-org/init-orglink ()
  (use-package orglink
    :hook (prog-mode . orglink-mode)
    :diminish))

(defun mfa-org/post-init-plantuml ()
  (setq plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-plantuml-jar-path plantuml-jar-path
        plantuml-output-type "png")
  (with-eval-after-load 'org-src
    (push '("plantuml" . plantuml) org-src-lang-modes)))

(defun mfa-org/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "xp" #'poporg-dwim)))
