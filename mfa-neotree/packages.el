(defconst mfa-neotree-packages '(neotree))

(defun mfa-neotree/post-init-neotree ()
  ;; Bind neotree-find to a key.
  (spacemacs/set-leader-keys "fd" #'neotree-find)

  (with-eval-after-load 'neotree
    ;; Enlarge the NeoTree window.
    (setq neo-window-width 40)

    ;; Make neotree-projectile select the current file when opened.
    (defun neotree-find-project-root-descend-advice (orig-fun &rest args)
      (let ((cur-file (buffer-file-name)))
        (prog1
            (apply orig-fun args)
          (when (and (neo-global--window-exists-p) cur-file)
            (neotree-find cur-file)))))
    (advice-add 'neotree-find-project-root :around #'neotree-find-project-root-descend-advice)))
