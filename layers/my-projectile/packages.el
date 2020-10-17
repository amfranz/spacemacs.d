;; -*- lexical-binding: t -*-

(defconst my-projectile-packages '(persp-mode
                                   projectile))

;; See also: https://github.com/Bad-ptr/persp-mode.el#custom-saveload-buffer-function-example
(defun my-projectile/post-init-persp-mode ()
  (setq persp-autokill-buffer-on-remove t
        persp-auto-save-persps-to-their-file t)

  ;; Loading a perspective can trigger many packages to load at once. Ensure
  ;; that garbage collection is turned on during loading to avoid the Emacs
  ;; memory usage to balloon.
  (gc-idle-exempt 'persp-load-state-from-file)

  ;; Turn on auto saving of perspectives. To avoid multiple Emacs instances to
  ;; overwrite each others config, we make an opinionated decision to designate
  ;; a specific Emacs instance that will auto save, and all other will not. The
  ;; instance that runs the Emacs daemon is the logical choice.
  (if (daemonp)
      (setq persp-auto-save-persps-to-their-file-before-kill 'persp-file)
    (setq persp-auto-save-opt 0))

  (spacemacs/replace-leader-key
    "pl" #'spacemacs/helm-persp-switch-project #'helm-persp-resume-project))

(defun my-projectile/post-init-projectile ()
  (el-patch-feature projectile)
  (with-eval-after-load 'projectile
    ;; TODO: is this still needed?
    ;; Never consider remote directories a project root. Remote links are
    ;; usually too slow for project-bound functionality, eg. source control.
    (defadvice projectile-project-root (around ignore-remote first activate)
      (unless (file-remote-p default-directory) ad-do-it))

    ;; When sorting by recentf, projectile does not remove deleted files or
    ;; files that are on the ignore list. This is a fixed implementation.
    (el-patch-defun projectile-sort-by-recentf-first (files)
      "Sort FILES by a recent first scheme."
      (let ((project-recentf-files (projectile-recentf-files)))
        (el-patch-swap
          (append project-recentf-files
                  (projectile-difference files project-recentf-files))
          (append (-intersection files project-recentf-files)
                  (-difference files project-recentf-files)))))

    ;; When sorting by recently active files, projectile does not remove
    ;; deleted files or files that are on the ignore list. This is a fixed
    ;; implementation.
    (el-patch-defun projectile-sort-by-recently-active-first (files)
      "Sort FILES by most recently active buffers or opened files."
      (let ((project-recently-active-files (projectile-recently-active-files)))
        (el-patch-swap
          (append project-recently-active-files
                  (projectile-difference files project-recently-active-files))
          (append (-intersection files project-recently-active-files)
                  (-difference files project-recently-active-files)))))))
