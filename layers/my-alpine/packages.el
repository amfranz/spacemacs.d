;; -*- lexical-binding: t -*-

(defconst my-alpine-packages '(sh-script))

(defun my-alpine/post-init-sh-script ()
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
