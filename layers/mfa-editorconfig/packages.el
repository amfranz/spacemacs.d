;; -*- lexical-binding: t -*-

(defconst mfa-editorconfig-packages '(editorconfig))

;; Needs to be pre-init, Spacemacs enables the mode in the init section. The
;; mode hook would not run if we add it after the mode is already enabled.
(defun mfa-editorconfig/pre-init-editorconfig ()
  (advice-add 'hack-local-variables
              :before #'run-before-hack-local-variables-hook)
  (add-hook 'editorconfig-mode-hook #'mfa-editorconfig//mode-hook)
  (add-hook 'editorconfig-custom-hooks #'mfa-editorconfig//custom-hook))
