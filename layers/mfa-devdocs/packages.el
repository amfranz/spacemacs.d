;; -*- lexical-binding: t -*-

(defconst mfa-devdocs-packages '(devdocs))

(defun mfa-devdocs/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (spacemacs/safe-set-leader-keys "o?" #'devdocs-search)))
