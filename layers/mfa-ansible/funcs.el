(defun mfa-ansible//auto-decrypt-encrypt-vault ()
  (when ansible
    (ansible::auto-decrypt-encrypt)
    (when (memq 'ansible::encrypt-buffer before-save-hook)
      (remove-hook 'before-save-hook 'ansible::encrypt-buffer t)
      (add-hook 'before-save-hook 'ansible::encrypt-buffer t t))))

(defun mfa-ansible//update-imenu-expression ()
  (when ansible
    (set (make-local-variable 'imenu-generic-expression)
         '(("var" "^\\(:?[a-zA-Z0-9_-]+\\):" 1)
           ("task" "^ *- +name: +\\(:?.*\\)" 1)))))

(defvar-local mfa-ansible-saved-coord '(1 1 1))

(defun mfa-ansible//save-coord ()
  (setq-local mfa-ansible-saved-coord
              (list (line-number-at-pos (window-start))
                    (line-number-at-pos (point))
                    (current-column))))

(defun mfa-ansible//restore-coord ()
  (goto-char (point-min))
  (forward-line (1- (car mfa-ansible-saved-coord)))
  (set-window-start nil (point))
  (forward-line (- (cadr mfa-ansible-saved-coord) (car mfa-ansible-saved-coord)))
  (move-to-column (caddr mfa-ansible-saved-coord) t)
  (kill-local-variable 'mfa-ansible-saved-coord))
