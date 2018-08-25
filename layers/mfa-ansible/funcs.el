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

(defun mfa-ansible/encrypt-region (beginning end)
  (interactive "r")
  (when (use-region-p)
    (let ((output (ansible::vault "encrypt" (buffer-substring-no-properties beginning end))))
      (delete-region beginning end)
      (insert "!vault |\n")
      (indent-according-to-mode)
      (insert (replace-regexp-in-string
               "\n"
               (concat "\n" (make-string (current-column) ?\s))
               (string-trim-right output))))
    (deactivate-mark)
    (set-mark beginning)
    (backward-char)
    (activate-mark)))

(defun mfa-ansible/decrypt-region (beginning end)
  (interactive "r")
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties beginning end)))
      (setq input (replace-regexp-in-string "\\`!vault |\n" "" input))
      (setq input (replace-regexp-in-string "^ +" "" input))
      (let ((output (ansible::vault "decrypt" input)))
        (delete-region beginning end)
        (insert output))
      (deactivate-mark)
      (set-mark beginning)
      (backward-char)
      (activate-mark))))

(defun mfa-ansible//quote-yaml (input)
  (if (string-prefix-p "{" input)
      (concat "'" input "'")
    input))

(defun mfa-ansible/upgrade-syntax (beginning end)
  (interactive "*r")
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties beginning end))
          (attr-pattern "\\([A-Za-z0-9_]+\\)=\\(.*?\\)[ \n]*\\(?:\\'\\|[A-Za-z0-9_]+=\\)"))
      (setq input (replace-regexp-in-string "\n *" " " input t t))
      (let ((match-start (string-match attr-pattern input)))
        (when match-start
          (let ((key (match-string 1 input))
                (value (match-string 2 input))
                (scan-start (match-end 2)))
            (delete-region beginning end)
            (goto-char beginning)
            (when (zerop (current-column))
              (insert "\n")
              (goto-char beginning))
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

(defun er//find-start-of-yaml-dict-or-array-value ()
  (let ((needle1 " *\\([a-zA-Z0-9_\\-\\.]+:\\)")
        (needle2 " *- *\\([a-zA-Z0-9_\\-\\.]+:\\)?"))
    (beginning-of-line)
    (while (not (or (eq (point) (point-min))
                    (looking-at needle1)
                    (looking-at needle2)))
      (forward-line -1))
    (when (or (looking-at needle1)
              (looking-at needle2))
      (prog1
          (- (or (match-beginning 1)
                 (match-beginning 0))
             (match-beginning 0))
        (goto-char (match-end 0))
        (while (not (looking-at " *\\([^ \n]\\)"))
          (forward-line 1))
        (goto-char (match-beginning 1))))))

(defun er/mark-yaml-value ()
  "Marks one YAML value."
  (interactive)
  (let ((here (point)))
    (let ((level (er//find-start-of-yaml-dict-or-array-value)))
      (if (not level)
          (goto-char here)
        (set-mark (point))
        (er//find-edge-of-yaml-indentation (+ level 1) 1 (point-max))
        (end-of-line)
        (exchange-point-and-mark)))))

(defun er//find-start-of-yaml-dict-or-array ()
  (let ((needle " *\\(-\\|[a-zA-Z0-9_\\-\\.]+:\\)"))
    (beginning-of-line)
    (while (not (or (eq (point) (point-min))
                    (looking-at needle)))
      (forward-line -1))
    (if (looking-at needle)
        (- (match-beginning 1) (match-beginning 0)))))

(defun er//find-edge-of-yaml-indentation (level stride limit)
  (let ((last-nonempty-line (point)))
    (while (progn
             (forward-line stride)
             (and (or (looking-at " *$")
                      (progn
                        (looking-at " *\\(.\\)")
                        (when (<= level (- (match-beginning 1) (match-beginning 0)))
                          (setq last-nonempty-line (point)))))
                  (not (eq (point) limit)))))
    (goto-char last-nonempty-line)))

(defun er/mark-yaml-this-block ()
  (interactive)
  (let ((here (point)))
    (let ((level (er//find-start-of-yaml-dict-or-array)))
      (if (not level)
          (goto-char here)
        (set-mark (point))
        (er//find-edge-of-yaml-indentation (+ level 1) 1 (point-max))
        (forward-line 1)
        (exchange-point-and-mark)))))

(defun er//mark-yaml-block (include-parent)
  (let ((here (point)))
    (let ((level (er//find-start-of-yaml-dict-or-array)))
      (if (not level)
          (goto-char here)
        (er//find-edge-of-yaml-indentation level -1 (point-min))
        (when include-parent
          (forward-line -1))
        (set-mark (point))
        (er//find-edge-of-yaml-indentation level 1 (point-max))
        (forward-line 1)
        (exchange-point-and-mark)))))

(defun er/mark-yaml-sibling-block ()
  "Marks all YAML lines of the same indentation level."
  (interactive)
  (er//mark-yaml-block nil))

(defun er/mark-yaml-parent-block ()
  "Marks all YAML lines of the same indentation level and their parent."
  (interactive)
  (er//mark-yaml-block t))

(defun er/add-yaml-mode-expansions ()
  "Add expansions for buffers in `yaml-mode'."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(er/mark-yaml-value
                 er/mark-yaml-this-block
                 er/mark-yaml-sibling-block
                 er/mark-yaml-parent-block))))
