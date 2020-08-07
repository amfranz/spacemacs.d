;; -*- lexical-binding: t -*-

(defconst my-dash-packages '(counsel-dash
                             helm-dash
                             zeal-at-point))

(defun my-dash/post-init-counsel-dash ()
  (my-dash//configure-dash-docs))

(defun my-dash/post-init-helm-dash ()
  (my-dash//configure-dash-docs))

(defun my-dash/post-init-zeal-at-point ()
  (with-eval-after-load 'zeal-at-point
    (add-to-list 'zeal-at-point-docsets "cmake")
    (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake"))))
