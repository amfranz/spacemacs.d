(defconst mfa-direnv-packages '(direnv))

(defun mfa-direnv/init-direnv ()
  (use-package direnv
    :config
    (progn
      (setq direnv-always-show-summary nil)
      (direnv-mode))))
