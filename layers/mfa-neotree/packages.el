(defconst mfa-neotree-packages '(all-the-icons neotree))

(defun mfa-neotree/init-all-the-icons ()
  (use-package all-the-icons
    :defer t
    :config
    ;; Don't waste valuable window real-estate.
    (setq all-the-icons-scale-factor 1.0)))

(defun mfa-neotree/post-init-neotree ()
  ;; Bind neotree-find to a key.
  (spacemacs/set-leader-keys "fd" #'neotree-find)

  (with-eval-after-load 'neotree

    ;; Make the tree more fancy.
    (setq neo-theme (if window-system 'icons 'arrow))

    ;; Enlarge the NeoTree window.
    (setq neo-window-width 40)))
