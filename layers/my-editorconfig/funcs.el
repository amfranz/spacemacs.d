;; -*- lexical-binding: t -*-

(defun run-before-hack-local-variables-hook (&rest ignored)
  (run-hooks 'before-hack-local-variables-hook))

(defun my-editorconfig//mode-hook ()
  (if (not editorconfig-mode)
      (remove-hook 'before-hack-local-variables-hook #'editorconfig-mode-apply)
    (remove-hook 'change-major-mode-after-body-hook #'editorconfig-mode-apply)
    (add-hook 'before-hack-local-variables-hook #'editorconfig-mode-apply)))

(defun my-editorconfig//custom-hook (props)
  (maphash (lambda (key value)
             (when my-editorconfig-verbose
               (message "editorconfig set %s to %s" key value)))
           props))
