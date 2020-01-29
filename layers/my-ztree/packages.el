;; -*- lexical-binding: t -*-

(defconst my-ztree-packages '(ztree))

(defun my-ztree/init-ztree ()
  (use-package ztree
    :defer t
    :init
    (progn
      (spacemacs/after-load-theme 'zenburn
        (zenburn-with-color-variables
          (custom-theme-alter-faces
           'zenburn
           '(ztreep-diff-model-add-face ((t (:inherit diff-added))))
           '(ztreep-diff-model-diff-face ((t (:inherit diff-changed))))
           '(ztreep-diff-model-normal-face ((t (:inherit diff-context))))
           `(ztreep-expand-sign-face ((t (:foreground ,zenburn-blue-2)))))))
      (with-eval-after-load 'ztree-diff
        (evil-define-key 'evilified ztreediff-mode-map
          "E" #'ztree-diff-toggle-show-equal-files)))))
