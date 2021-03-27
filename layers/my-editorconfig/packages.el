;; -*- lexical-binding: t -*-

(defconst my-editorconfig-packages '(editorconfig))

;; Needs to be pre-init, Spacemacs enables the mode in the init section. The
;; customization do not work properly if the mode is already enabled.
(defun my-editorconfig/pre-init-editorconfig ()
  ;; This new experimental mode fixes an issue that caused mode-local-vars-hook
  ;; being called twice. This issue broke functionality other modes, for example
  ;; the vault auto-decrypt functionality of `ansible'. For details, see
  ;; https://github.com/editorconfig/editorconfig-emacs/issues/242
  (setq editorconfig--enable-20210221-testing t)

  ;; Add a hook that may announce the settings changed by editorconfig in the
  ;; messages buffer, see `my-editorconfig-verbose'.
  (add-hook 'editorconfig-after-apply-functions #'my-editorconfig//announce-changes))
