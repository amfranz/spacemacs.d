;; -*- lexical-binding: t -*-

(defconst my-projectile-packages '(projectile))

(defun my-projectile/post-init-projectile ()
  ;; Merge all keybindings for project build / run / test commands under a
  ;; central location. These work better for me than the default bindings
  ;; provided by Spacemacs.
  (spacemacs/replace-leader-key "pc" 'projectile-compile-project nil)
  (spacemacs/replace-leader-key "pT" 'projectile-test-project nil)
  (spacemacs/declare-prefix "pm" "make")
  (spacemacs/safe-set-leader-keys
    "pmc" #'projectile-configure-project
    "pmm" #'projectile-compile-project
    "pmt" #'projectile-test-project
    "pmr" #'projectile-run-project)

  (with-eval-after-load 'projectile
    ;; When sorting by recentf, projectile does not remove deleted files or
    ;; files that are on the ignore list. This is a fixed implementation.
    (defun projectile-sort-by-recentf-first (files)
      "Sort FILES by a recent first scheme."
      (let ((project-recentf-files (projectile-recentf-files)))
        (append (-intersection files project-recentf-files)
                (-difference files project-recentf-files))))

    ;; Never consider remote directories a project root. Remote links are
    ;; usually too slow for project-bound functionalitye, eg. source control.
    (defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory) ad-do-it))

    ;; When sorting by recently active files, projectile does not remove
    ;; deleted files or files that are on the ignore list. This is a fixed
    ;; implementation.
    (defun projectile-sort-by-recently-active-first (files)
      "Sort FILES by most recently active buffers or opened files."
      (let ((project-recently-active-files (projectile-recently-active-files)))
        (append (-intersection files project-recently-active-files)
                (-difference files project-recently-active-files))))))
