(defconst mfa-editorconfig-packages '(editorconfig))

(defun mfa-editorconfig/init-editorconfig ()
  (use-package editorconfig
    :config
    (add-to-list 'edconf-custom-hooks
                 '(lambda (props)
                    (maphash (lambda (key value)
                               (progn
                                 ;; undo require-final-newline to get compatible with ethan-wspace
                                 ;; should probably be moved to ethan-wspace layer
                                 (when (eq 'insert_final_newline key)
                                   (setq require-final-newline nil
                                         mode-require-final-newline nil))
                                 (message "editorconfig set %s to %s" key value)))
                             props)))))
