;; -*- lexical-binding: t -*-

(defun run-before-hack-local-variables-hook (&rest ignored)
  (run-hooks 'before-hack-local-variables-hook))

(defun mfa-editorconfig//mode-hook ()
  (if (not editorconfig-mode)
      (remove-hook 'before-hack-local-variables-hook #'editorconfig-mode-apply)
    (remove-hook 'change-major-mode-after-body-hook #'editorconfig-mode-apply)
    (add-hook 'before-hack-local-variables-hook #'editorconfig-mode-apply)))

(defun mfa-editorconfig//custom-hook (props)
  (maphash (lambda (key value)
             (when mfa-editorconfig-verbose
               (message "editorconfig set %s to %s" key value)))
           props))
