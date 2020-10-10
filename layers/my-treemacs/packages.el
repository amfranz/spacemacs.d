;; -*- lexical-binding: t -*-

(defconst my-treemacs-packages '(treemacs
                                 treemacs-evil))

(defun my-treemacs/post-init-treemacs ()
  ;; An extended `treemacs-git-mode' requires python3 to work.
  (setq treemacs-python-executable (executable-find "python3"))

  ;; Make treemacs a bit less noisy.
  (setq treemacs-silent-filewatch t)

  ;; Keep the view centered on the current buffers file.
  (setq treemacs-recenter-after-file-follow t)

  (spacemacs/declare-prefix "pw" "workspace")
  (spacemacs/safe-set-leader-keys
    "pw0" #'treemacs-display-current-project-exclusively
    "pwa" #'treemacs-add-project-to-workspace
    "pwc" #'treemacs-create-workspace
    "pwe" #'treemacs-edit-workspaces
    "pwr" #'treemacs-remove-workspace
    "pws" #'treemacs-switch-workspace)

  (with-eval-after-load 'treemacs
    ;; Hide files that are ignored by Git.
    (add-to-list 'treemacs-pre-file-insert-predicates
                 #'treemacs-is-file-git-ignored?)

    ;; The above hides all files ignored by Git and its data directory, but it
    ;; does not hide the '.git' directory itself. Annoying. Let's fix it.
    (add-to-list 'treemacs-ignored-file-predicates
                 #'my-treemacs//ignore-dot-git)

    ;; Override the key binding for `delete-other-windows' with an
    ;; implementation that avoids deleting the treemacs window as well.
    (spacemacs/replace-leader-key "wm"
      #'spacemacs/toggle-maximize-buffer
      #'my-treemacs/toggle-maximize-buffer)

    ;; Fixes icon background color. It's broken because Treemacs only adjusts it
    ;; by adding a hook to `enable-theme', but due to lazy loading the theme the
    ;; hook will have fired long before treemacs gets loaded. See also:
    ;; https://github.com/Alexander-Miller/treemacs/issues/148
    (treemacs--setup-icon-background-colors)

    ;; Enhance the visibility of the currently selected line.
    (treemacs-fringe-indicator-mode t)))

(defun my-treemacs/post-init-treemacs-evil ()
  (with-eval-after-load 'treemacs-evil
    ;; Moving the root with h/l seems non-sensical to me. This rebinds them to
    ;; functionality that, to me, seems more intuitive to be bound to those
    ;; keys.
    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "h") #'treemacs-goto-parent-node
      (kbd "l") #'treemacs-RET-action)

    ;; Move the rename functionality from "R" to "r". This overrides the binding
    ;; for refresh, but that is not a problem because refresh is available at
    ;; "gr" as well. "gr" should be the canonical binding anyway, judging by the
    ;; Spacemacs key binding conventions.
    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "r") #'treemacs-rename
      (kbd "R") nil)

    ;; `evil-treemacs' does not define an evil key for `treemacs-resort', the
    ;; empty space instead of the binding looks off in the help screen ("?").
    ;; We'll define a binding to tidy up the screen. That being said, the
    ;; functionality is not very useful, so this might just be pedantic.
    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "gs") #'treemacs-resort)

    ;; Enable the right-click context menu.
    (evil-define-key 'treemacs treemacs-mode-map
      [mouse-3] #'treemacs-rightclick-menu)))
