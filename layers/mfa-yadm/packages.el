;; -*- lexical-binding: t -*-

(defconst mfa-yadm-packages '((tramp :location built-in)))

(defun mfa-yadm/init-tramp ()
  (when (or t (configuration-layer/package-used-p 'magit))
    (spacemacs/safe-set-leader-keys "oy" #'yadm-status)
    (with-eval-after-load 'tramp
      (add-to-list 'tramp-methods
                   '("yadm"
                     (tramp-login-program "yadm")
                     (tramp-login-args (("enter")))
                     (tramp-login-env (("SHELL") ("/bin/sh")))
                     (tramp-remote-shell "/bin/sh")
                     (tramp-remote-shell-args ("-c")))))))
