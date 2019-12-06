;; -*- lexical-binding: t -*-

(defconst my-yadm-packages '(magit))

(defun my-yadm/post-init-magit ()
  (spacemacs/set-leader-keys "oy" #'yadm-status)

  (when (configuration-layer/package-used-p 'magit-todos)
    (advice-add 'magit-todos--insert-todos :around
                #'yadm--magit-todos-manual-scan-ad))

  (with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
                 '("yadm"
                   (tramp-login-program "yadm")
                   (tramp-login-args (("enter")))
                   (tramp-login-env (("SHELL") ("/bin/sh")))
                   (tramp-remote-shell "/bin/sh")
                   (tramp-remote-shell-args ("-c"))))))
