;; -*- lexical-binding: t -*-

(defconst mfa-alpine-packages '(sh-script))

(defun mfa-alpine/post-init-sh-script ()
  (dolist (elem '(("APKBUILD\\'" . sh-mode)
                  ("\\.confd\\'" . sh-mode)
                  ("\\.initd\\'" . sh-mode)
                  ("\\.pre-install\\'" . sh-mode)
                  ("\\.post-install\\'" . sh-mode)
                  ("\\.pre-upgrade\\'" . sh-mode)
                  ("\\.post-upgrade\\'" . sh-mode)
                  ("\\.pre-deinstall\\'" . sh-mode)
                  ("\\.post-deinstall\\'" . sh-mode)))
    (add-to-list 'auto-mode-alist elem)))
