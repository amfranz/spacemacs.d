;; -*- lexical-binding: t -*-

(defconst my-ztree-packages '(ztree))

(defun my-ztree/init-ztree ()
  (use-package ztree
    :defer t
    :init
    (with-eval-after-load 'ztree-diff
      (add-hook 'spacemacs-post-theme-change-hook #'my--adjust-ztree-faces)
      (my--adjust-ztree-faces)
      (evil-define-key 'evilified ztreediff-mode-map
        "E" #'ztree-diff-toggle-show-equal-files))))
