;; -*- lexical-binding: t -*-

(defun helm-persp--resume-project-action (project)
  (if (persp-with-name-exists-p project)
      (persp-switch project)
    (let ((layout-file (concat project ".layout.el")))
      (if (file-exists-p layout-file)
          (persp-load-from-file-by-names layout-file *persp-hash* (list project)))
      (if (persp-with-name-exists-p project)
          (persp-switch project)
        (spacemacs//helm-persp-switch-project-action project)
        (if (and (persp-with-name-exists-p project) (> persp-auto-save-opt 0))
            (persp-save-to-file-by-names layout-file *persp-hash* (list project)))))))

(defun helm-persp-resume-project (arg)
  "Select a persistent project layout to switch to."
  (interactive "P")
  (helm
   :sources
   (helm-build-in-buffer-source "*Helm Switch Project Layout*"
     :data (lambda ()
             (if (projectile-project-p)
                 (projectile-relevant-known-projects)
               projectile-known-projects))
     :fuzzy-match helm-projectile-fuzzy-match
     :mode-line helm-read-file-name-mode-line-string
     :action `(("Switch to Project Perspective" .
                helm-persp--resume-project-action)))
   :buffer "*Helm Projectile Layouts*"))
