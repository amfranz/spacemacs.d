;; -*- lexical-binding: t -*-

(defconst mfa-org-packages '(fontawesome
                             helm-ag
                             helm-org-rifle
                             helm-orgcard
                             org
                             org-trello
                             orglink
                             plantuml
                             poporg
                             side-notes))

(defun mfa-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "of" #'helm-fontawesome)))

(defun mfa-org/post-init-helm-ag ()
  ;; Automatically unfold the `org-mode' section at point after following a
  ;; match found by `helm-ag'.
  (advice-add 'helm-ag--find-file-action :after #'mfa-org//org-show-entry))

(defun mfa-org/post-init-helm-org-rifle ()
  (spacemacs/safe-set-leader-keys "o/" #'helm-org-rifle-org-directory))

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
  ;; share org files over Syncthing.
  (setq org-directory "~/Sync/org/")

  ;; Spacemacs sets this before knowing our value for `org-directory',
  ;; therefore we are correcting it here.
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; automatically indent org sections.
  (setq org-startup-indented t)

  ;; set up agendas.
  (setq org-agenda-files (list (concat org-directory "agenda/"))
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file)

  (setq org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{})

  (setq org-hide-emphasis-markers t
        org-pretty-entities t)

  ;; Warn about editing an invisible (folded) area.
  (setq org-catch-invisible-edits 'show-and-error)

  ;; customize org source block editing.
  (setq org-src-window-setup 'current-window
        org-src-preserve-indentation t)

  ;; TODO https://emacs.stackexchange.com/questions/20574/default-inline-image-background-in-org-mode?rq=1

  ;; TODO ob-blockdiag: --no-transparency --antialias -T (type)

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
  (spacemacs/declare-prefix "oo" "org")
  (spacemacs/safe-set-leader-keys
    "ooa" #'mfa-org/org-agenda
    "oob" #'mfa-org/org-backlog
    "ooi" #'mfa-org/org-index)
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "or" #'org-redisplay-inline-images
    "oe" #'counsel-org-entity)

  (advice-add 'er/expand-region :around
              #'mfa-org//ad-preserve-outline-visibility)

  ;; Automatically redisplay inline images.
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

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

(defun mfa-org/init-side-notes ()
  (use-package side-notes
    :defer t
    :init
    (progn
      (setq-default side-notes-file "notes.org")
      (spacemacs/safe-set-leader-keys "on" #'side-notes-toggle-notes)
      (autoload 'side-notes-toggle-notes "side-notes"))))
