(defconst mfa-evil-mc-packages '(evil-mc))

(defun mfa-evil-mc/post-init-evil-mc ()
  (dolist (mode-hook '(conf-mode-hook
                       prog-mode-hook
                       text-mode-hook))
    (add-hook mode-hook #'turn-on-evil-mc-mode)))
