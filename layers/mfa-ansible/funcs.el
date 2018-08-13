(defun mfa-ansible//ansible-company-maybe-enable ()
  (when (spacemacs//ansible-should-enable?)
    (setq-local company-backends '(company-files
                                   (company-ansible company-dabbrev-code)))))

(defun mfa-ansible//vault-encrypt-advice (orig-fun mode str)
  (when (string= mode "encrypt")
    (setq mode "encrypt --encrypt-vault-id=default"))
  (funcall orig-fun mode str))

(defun mfa-ansible//auto-decrypt-encrypt-vault ()
  (when (and ansible
             (string-match-p "\\`\$ANSIBLE_VAULT;[0-9]+\.[0-9]+"
                             (buffer-substring-no-properties (point-min) (point-max))))
    (ansible::auto-decrypt-encrypt)
    (add-hook 'ws-butler-mode-hook #'mfa-ansible//fix-ws-butler-decrypt-order t t)))

(defun mfa-ansible//fix-ws-butler-decrypt-order ()
  (when (and ws-butler-mode (memq #'ansible::encrypt-buffer before-save-hook))
    (remove-hook 'before-save-hook #'ansible::encrypt-buffer t)
    (add-hook 'before-save-hook #'ansible::encrypt-buffer t t)))

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

(defun mfa-ansible//vault-string (mode beginning end)
  (when (use-region-p)
    (let ((output (ansible::vault mode (buffer-substring-no-properties beginning end))))
      (delete-region beginning end)
      (insert output))))

(defun mfa-ansible/encrypt-string (beginning end)
  (interactive "r")
  (mfa-ansible//vault-string "encrypt" beginning end))

(defun mfa-ansible/decrypt-string (beginning end)
  (interactive "r")
  (mfa-ansible//vault-string "decrypt" beginning end))

(defun mfa-ansible//quote-yaml (input)
  (if (string-prefix-p "{" input)
      (concat "'" input "'")
    input))

(defun mfa-ansible/upgrade-syntax (beginning end)
  (interactive "*r")
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties beginning end))
          (attr-pattern "\\([A-Za-z0-9_]+\\)=\\(.*?\\)\\(?:\\'\\| +[A-Za-z0-9_]+=\\)"))
      (setq input (replace-regexp-in-string "\n *" " " input t t))
      (let ((match-start (string-match attr-pattern input)))
        (let ((key (match-string 1 input))
              (value (match-string 2 input))
              (scan-start (match-end 2)))
          (when match-start
            (delete-region beginning end)
            (goto-char beginning)
            (insert "\n")
            (goto-char beginning)
            (insert (s-trim-right (substring input 0 match-start)))
            (while match-start
              (insert "\n")
              (indent-according-to-mode)
              (insert key ": " (mfa-ansible//quote-yaml value))
              (setq match-start (string-match attr-pattern input scan-start))
              (when match-start
                (setq key (match-string 1 input)
                      value (match-string 2 input)
                      scan-start (match-end 2))))))))))
