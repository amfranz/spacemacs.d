(defconst mfa-editorconfig-packages '(editorconfig))

(defun mfa-editorconfig/init-editorconfig ()
  (use-package editorconfig
    :diminish
    :init
    (editorconfig-mode)
    :config
    (progn
      (spacemacs|hide-lighter editorconfig-mode)
      (add-hook 'editorconfig-custom-hooks #'mfa-editorconfig//hook))))
