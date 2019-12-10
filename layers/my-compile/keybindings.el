;; -*- lexical-binding: t -*-

(spacemacs/safe-set-leader-keys
  "je" #'spacemacs/goto-error-transient-state/body
  "jm" #'display-compilation-buffer)
