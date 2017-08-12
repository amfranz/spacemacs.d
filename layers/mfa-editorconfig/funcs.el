(defun mfa-editorconfig//hook (props)
  (maphash (lambda (key value)
             (when editorconfig-verbose
               (message "editorconfig set %s to %s" key value)))
           props))
