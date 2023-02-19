;; -*- lexical-binding: t -*-

(defconst my-treemacs-packages '(treemacs
                                 treemacs-evil))

(defun my-treemacs/post-init-treemacs ()
  ;; Make treemacs a bit less noisy.
  (setq treemacs-silent-filewatch t)

  ;; I prefer input from the minibuffer.
  (setq treemacs-read-string-input 'from-minibuffer)

  ;; Sometimes I just want to see the current project only.
  (spacemacs/safe-set-leader-keys
    "pT" #'treemacs-add-and-display-current-project-exclusively)

  (with-eval-after-load 'treemacs
    ;; Fixes icon background color. It's broken because Treemacs only adjusts it
    ;; by adding a hook to `enable-theme', but due to lazy loading the theme the
    ;; hook will have fired long before treemacs gets loaded. See also:
    ;; https://github.com/Alexander-Miller/treemacs/issues/148
    (treemacs--setup-icon-background-colors)

    ;; Hide gitignored files by default. It is a known issue that this does not
    ;; work with deferred git mode when the repository is first displayed:
    ;; https://github.com/Alexander-Miller/treemacs/issues/834#issuecomment-902180637
    (treemacs-hide-gitignored-files-mode)

    ;; Enhance the visibility of the currently selected line.
    (treemacs-fringe-indicator-mode)))

(defun my-treemacs/post-init-treemacs-evil ()
  (with-eval-after-load 'treemacs-evil
    ;; Move the rename functionality from "R" to "r". This overrides the binding
    ;; for refresh, but that is not a problem because refresh is available at
    ;; "gr" as well. "gr" should be the canonical binding anyway, judging by the
    ;; Spacemacs key binding conventions.
    (evil-define-key 'treemacs treemacs-mode-map
      (kbd "r") #'treemacs-rename-file
      (kbd "R") nil)))
