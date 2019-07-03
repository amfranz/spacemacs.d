;; -*- lexical-binding: t -*-

(defun my--adjust-ztree-faces ()
  (when (memq 'zenburn custom-enabled-themes)
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       '(ztreep-diff-model-add-face ((t (:inherit diff-added))))
       '(ztreep-diff-model-diff-face ((t (:inherit diff-changed))))
       '(ztreep-diff-model-normal-face ((t (:inherit diff-context))))
       `(ztreep-expand-sign-face ((t (:foreground ,zenburn-blue-2))))))))
