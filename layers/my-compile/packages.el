;; -*- lexical-binding: t -*-

(defconst my-compile-packages '(projectile))

(defun my-compile/post-init-projectile ()
  ;; Merge all keybindings for project build / run / test commands under a
  ;; central location. These work better for me than the default bindings
  ;; provided by Spacemacs.
  (spacemacs/replace-leader-key "pc" 'projectile-compile-project nil)
  (spacemacs/replace-leader-key "pT" 'projectile-test-project nil)
  (spacemacs/declare-prefix "pm" "make")
  (spacemacs/safe-set-leader-keys
    "pmc" #'projectile-configure-project
    "pmm" #'projectile-compile-project
    "pmt" #'projectile-test-project
    "pmr" #'projectile-run-project))
