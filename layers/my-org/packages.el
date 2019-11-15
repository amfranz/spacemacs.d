;; -*- lexical-binding: t -*-

(defconst my-org-packages '(fontawesome
                            helm-orgcard
                            ob-blockdiag
                            org
                            org-sidebar
                            ox-gfm
                            plantuml-mode
                            poporg
                            side-notes))

(defun my-org/init-fontawesome ()
  (use-package fontawesome
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "if" #'helm-fontawesome)))

(defun my-org/init-helm-orgcard ()
  (use-package helm-orgcard
    :defer t
    :init
    (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
      "?" #'helm-orgcard)))

(defun my-org/init-ob-blockdiag ()
  (use-package ob-blockdiag
    :defer t
    :init
    (progn
      (with-eval-after-load 'org
        (push '(blockdiag . t) org-babel-load-languages))

      ;; Add support for passing additional command line arguments to blockdiag
      ;; tools as configured in `org-babel-blockdiag-arguments'.
      (el-patch-feature ob-blockdiag)
      (with-eval-after-load 'ob-blockdiag
        (eval
         '(el-patch-defun org-babel-execute:blockdiag (body params)
            (let ((file (cdr (assoc :file params)))
                  (tool (cdr (assoc :tool params)))
                  (font (cdr (assoc :font params)))
                  (size (cdr (assoc :size params)))
                  (type (cdr (assoc :type params)))
                  (el-patch-add
                    (xarg (cdr (assoc :args params))))

                  (buffer-name "*ob-blockdiag*")
                  (error-template "Subprocess '%s' exited with code '%d', see output in '%s' buffer"))
              (save-window-excursion
                (let ((buffer (get-buffer buffer-name)))(if buffer (kill-buffer buffer-name) nil))
                (let ((data-file (org-babel-temp-file "blockdiag-input"))
                      (args (append (list "-o" file)
                                    (if size (list "--size" size) (list))
                                    (if font (list "--font" (el-patch-swap size font)) (list))
                                    (if type (list "-T" type) (list))
                                    (el-patch-add xarg)))
                      (buffer (get-buffer-create buffer-name)))
                  (with-temp-file data-file (insert body))
                  (let
                      ((exit-code (apply 'call-process tool nil buffer nil (append args (list data-file)))))
                    (if (= 0 exit-code) nil (error (format error-template tool exit-code buffer-name)))))))))))))

(defun my-org/post-init-org ()
  ;; Share org files over Syncthing.
  (setq org-directory "~/Sync/org/")

  ;; Spacemacs sets this before knowing our value for `org-directory',
  ;; therefore we are correcting it here.
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory))

  ;; Automatically indent org sections.
  (setq org-startup-indented t)

  ;; Set up agendas and refiling.
  (setq org-agenda-files (list (concat org-directory "agenda/"))
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-refile-use-outline-path 'file)

  ;; Mandate that sub- and superscripts have to be wrapped in curly braces to be
  ;; interpreted as such (eg. a_{1}, 2^{2}). Without this, it often happens that
  ;; words with underscores (eg. variable/function names) get interpreted as an
  ;; expression with subscripts when they are not.
  (setq org-use-sub-superscripts '{}
        org-export-with-sub-superscripts '{})

  ;; Make org documents more WYSIWYG-alike.
  (setq org-hide-emphasis-markers t
        org-pretty-entities t)

  ;; Warn about editing an invisible (folded) area.
  (setq org-catch-invisible-edits 'show-and-error)

  ;; Customize org source block editing.
  (setq org-src-window-setup 'current-window
        org-src-preserve-indentation t)

  ;; Customize org priority faces.
  (add-hook 'spacemacs-post-theme-change-hook #'my-org//adjust-org-priority-faces)
  (my-org//adjust-org-priority-faces)

  ;; Wrap long lines by default.
  (add-hook 'org-mode-hook #'spacemacs/toggle-visual-line-navigation-on)

  ;; Customize the color used for transparent sections of org inline images.
  (advice-add 'org-display-inline-images :around #'my-org//mark-org-display-inline-images)
  (advice-add 'create-image :filter-args #'my-org//create-image-with-background-color)

  ;; additional leader key bindings for org functionality.
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "oy" #'org-copy-special
    "oc" #'org-cut-special
    "op" #'org-paste-special)

  ;; Integrate ditaa converts ascii images to real images.
  (with-eval-after-load 'org
    (push '(ditaa . t) org-babel-load-languages)
    (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar"))

  ;; Enable `calc' for evaluating formulas.
  (with-eval-after-load 'org
    (push '(calc . t) org-babel-load-languages))

  ;; Customize `org-download'.
  (setq org-download-heading-lvl nil)

  ;; Extra keybindings for org functionality.
  (spacemacs/declare-prefix "oo" "org")
  (spacemacs/safe-set-leader-keys
    "ooa" #'my-org/org-agenda
    "oob" #'my-org/org-backlog
    "ooi" #'my-org/org-index
    "pn" #'my-org/projectile-notes)
  (spacemacs/safe-set-leader-keys-for-major-mode 'org-mode
    "or" #'org-redisplay-inline-images
    "oe" #'counsel-org-entity)

  ;; Prevent `expand-region' from altering outline visibility while trying to
  ;; find an expansion of the selection.
  (advice-add 'er/expand-region :around #'my-org//ad-preserve-outline-visibility)

  ;; Automatically redisplay inline images.
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images))

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
        plantuml-jar-path "/usr/share/plantuml/plantuml.jar"
        org-plantuml-jar-path plantuml-jar-path
        plantuml-output-type "png")
  (with-eval-after-load 'org-src
    (push '("plantuml" . plantuml) org-src-lang-modes)))

(defun my-org/init-poporg ()
  (use-package poporg
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "xe" #'poporg-dwim)))

(defun my-org/init-side-notes ()
  (use-package side-notes
    :defer t
    :init
    (progn
      (setq-default side-notes-file "notes.org")
      (spacemacs/safe-set-leader-keys "on" #'side-notes-toggle-notes))))
