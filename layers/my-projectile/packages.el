;; -*- lexical-binding: t -*-

(defconst my-projectile-packages '(projectile))

(defun my-projectile/post-init-projectile ()
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
