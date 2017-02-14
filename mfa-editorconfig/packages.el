(defconst mfa-editorconfig-packages '(editorconfig))

(defun mfa-editorconfig/init-editorconfig ()
  (use-package editorconfig
    :diminish
    :init
    (editorconfig-mode)
    :config
    (progn
      (spacemacs|hide-lighter editorconfig-mode)
      (defvar mfa/edconf-verbose nil)
      (add-to-list 'edconf-custom-hooks
                   '(lambda (props)
                      (maphash (lambda (key value)
                                 (when mfa/edconf-verbose
                                   (message "editorconfig set %s to %s" key value)))
                               props))))))
