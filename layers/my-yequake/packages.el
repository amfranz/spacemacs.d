;; -*- lexical-binding: t -*-

(defconst my-yequake-packages '((yequake :location (recipe :fetcher github
                                                           :repo "alphapapa/yequake"))))

(defun my-yequake/init-yequake ()
  (use-package yequake
    :defer t
    :config
    (setq yequake-frames
          `(("Org Index" .
             ((name . "Org Index")
              (width . 0.75)
              (height . 0.5)
              (alpha . 0.95)
              (buffer-fns . (,(concat org-directory "index.org")))
              (frame-parameters . ((undecorated . t)))))))))
