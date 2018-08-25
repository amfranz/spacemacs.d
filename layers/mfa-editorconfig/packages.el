(defconst mfa-editorconfig-packages '(editorconfig))

(defun mfa-editorconfig/post-init-editorconfig ()
  (with-eval-after-load 'editorconfig
    (add-hook 'editorconfig-custom-hooks #'mfa-editorconfig//hook)))
