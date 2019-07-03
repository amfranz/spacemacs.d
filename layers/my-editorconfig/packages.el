;; -*- lexical-binding: t -*-

(defconst my-editorconfig-packages '(editorconfig))

;; Needs to be pre-init, Spacemacs enables the mode in the init section. The
;; mode hook would not run if we add it after the mode is already enabled.
(defun my-editorconfig/pre-init-editorconfig ()
  (advice-add 'hack-local-variables
              :before #'run-before-hack-local-variables-hook)
  (add-hook 'editorconfig-mode-hook #'my-editorconfig//mode-hook)
  (add-hook 'editorconfig-after-apply-functions #'my-editorconfig//custom-hook))
