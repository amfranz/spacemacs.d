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

(defun mfa-ansible//quote-yaml (input)
  (if (string-prefix-p "{" input)
      (concat "'" input "'")
    input))

(defun mfa-ansible/upgrade-syntax (beginning end)
  (interactive "r")
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties beginning end))
          (attr-pattern "\\([A-Za-z0-9_]+\\)=\\(.*?\\)\\(?:\\'\\| +[A-Za-z0-9_]+=\\)"))
      (setq input (replace-regexp-in-string "\n *" " " input t t))
      (delete-region beginning end)
      (let ((match-start (string-match attr-pattern input)))
        (when match-start
          (insert (s-trim-right (substring input 0 match-start)))
          (while match-start
            (let ((key (match-string 1 input))
                  (value (match-string 2 input))
                  (scan-start (match-end 2)))
              (newline)
              (indent-according-to-mode)
              (insert key ": " (mfa-ansible//quote-yaml value))
              (setq match-start (string-match attr-pattern input scan-start))))))
      (newline))))
