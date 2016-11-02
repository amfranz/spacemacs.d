(defconst mfa-neotree-packages '(neotree))

(defun mfa-neotree/post-init-neotree ()
  ;; Bind neotree-find to a key.
  (spacemacs/set-leader-keys "fd" #'neotree-find)

  (with-eval-after-load 'neotree
    ;; Enlarge the NeoTree window.
    (setq neo-window-width 40)

