(defun mfa-ansible//auto-decrypt-encrypt-vault ()
  (when ansible
    (ansible::auto-decrypt-encrypt)))

(defun mfa-ansible//update-imenu-expression ()
  (when ansible
    (set (make-local-variable 'imenu-generic-expression)
         '(("var" "^\\(:?[a-zA-Z0-9_-]+\\):" 1)
           ("task" "^ *- +name: +\\(:?.*\\)" 1)))))
