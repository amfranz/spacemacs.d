;; -*- lexical-binding: t -*-

(defconst my-org-packages '(fontawesome
                            helm-org-rifle
                            helm-orgcard
                            org
                            org-sidebar
                            org-trello
                            ox-gfm
                            plantuml-mode
                            poporg
                            side-notes))

(defun my-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "if" #'helm-fontawesome)))

(defun my-org/post-init-helm-org-rifle ()
  (spacemacs/safe-set-leader-keys "o/" #'helm-org-rifle-org-directory))

(defun my-org/init-helm-orgcard ()
  (use-package helm-orgcard
    :defer t
    :init
    (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
      "?" #'helm-orgcard)))

(defun my-org/pre-init-org ()
  ;; customize org priority faces.
  (add-hook 'spacemacs-post-theme-change-hook
            #'my-org//custom-theme-set-variables))

(defun my-org/post-init-org ()
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

  ;; Wrap long lines by default.
  (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)

  ;; TODO: https://emacs.stackexchange.com/questions/20574/default-inline-image-background-in-org-mode?rq=1

  ;; TODO: ob-blockdiag: --no-transparency --antialias -T (type)

  ;; additional leader key bindings for org functionality.
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)

  ;; ditaa converts ascii images to real images.
  (with-eval-after-load 'org
    (push '(ditaa . t) org-babel-load-languages)
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

  ;; calc for evaluating formulas
  (with-eval-after-load 'org
    (push '(calc . t) org-babel-load-languages))

  ;; configure org-download.
  (setq org-download-heading-lvl nil)

  ;; extra keybindings for org functionality.
  (spacemacs/declare-prefix "oo" "org")
  (spacemacs/safe-set-leader-keys
    "ooa" #'my-org/org-agenda
    "oob" #'my-org/org-backlog
    "ooi" #'my-org/org-index)
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "or" #'org-redisplay-inline-images
    "oe" #'counsel-org-entity)

  (advice-add 'er/expand-region :around
              #'my-org//ad-preserve-outline-visibility)

  ;; Automatically redisplay inline images.
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

(defun my-org/post-init-org-trello ()
  (setq org-trello-input-completion-mechanism 'helm)
  (with-eval-after-load 'org-trello
    (setq orgtrello-log-level orgtrello-log-warn)))

(defun my-org/init-org-sidebar ()
  (use-package org-sidebar
    :defer t))

(defun my-org/post-init-ox-gfm ()
  (el-patch-feature ox-gfm)
  (with-eval-after-load 'ox-gfm
    (defvar org-gfm-src-lang-overrides '(("conf-javaprop" . "properties"))
      "An association list that maps language names as specified in the first
argument of an org mode source block to language names as specified in the
beginning of a Markdown fenced code block. For languages that have no entry in
this list the language is name is passed on from Org to Markdown as-is.")

    (defun org-gfm-src-block-lang (src-block)
      (when-let (lang (org-element-property :language src-block))
        (alist-get lang org-gfm-src-lang-overrides lang nil #'string=)))

    (el-patch-defun org-gfm-src-block (src-block contents info)
      "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
      (let* ((lang (el-patch-swap (org-element-property :language src-block)
                                  (org-gfm-src-block-lang src-block)))
             (code (org-export-format-code-default src-block info))
             (prefix (concat "```" lang "\n"))
             (suffix "```"))
        (concat prefix code suffix)))))

(defun my-org/post-init-plantuml-mode ()
  (setq plantuml-default-exec-mode 'jar
        plantuml-jar-path (concat user-home-directory ".plantuml/plantuml.1.2019.12.jar")
        org-plantuml-jar-path plantuml-jar-path
        plantuml-output-type "png")
  (with-eval-after-load 'org-src
    (push '("plantuml" . plantuml) org-src-lang-modes)))

(defun my-org/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "xp" #'poporg-dwim)))

(defun my-org/init-side-notes ()
  (use-package side-notes
    :defer t
    :init
    (progn
      (setq-default side-notes-file "notes.org")
      (spacemacs/safe-set-leader-keys "on" #'side-notes-toggle-notes)
      (autoload 'side-notes-toggle-notes "side-notes"))))
