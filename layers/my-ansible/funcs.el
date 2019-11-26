;; -*- lexical-binding: t -*-

(defun my-ansible//flycheck-setup ()
  (flycheck-yamllint-setup)
  (setq flycheck-checker 'yaml-yamllint)
  (flycheck-mode))

(defun my-ansible//ansible-company-maybe-enable ()
  (when (spacemacs//ansible-should-enable?)
    (setq-local company-backends '(company-files
                                   (company-ansible company-dabbrev-code)))))

(defun my-ansible/jump-to-template ()
  (interactive)
  (when (bound-and-true-p ansible)
    (let ((filename (thing-at-point 'filename 'no-properties)))
      (unless (or (file-remote-p filename)
                  (f-absolute? filename))
        (when-let (file (cl-loop for section in '("files" "tasks" "templates")
                                 for file = (concat "../" section "/" filename)
                                 if (file-exists-p file) return file))
          (find-file file))))))

(defun my-ansible/jump-to-variable ()
  (interactive)
  (when (bound-and-true-p ansible)
    (let ((symbol (thing-at-point 'symbol 'no-properties))
          (candidate-files '("../defaults/main.yaml"
                             "../defaults/main.yml"
                             "../vars/main.yaml"
                             "../vars/main.yml"))
          dest-file dest-offset)
      (with-temp-buffer
        (save-match-data
          (while (and candidate-files (not dest-file))
            (let ((file (pop candidate-files)))
              (when (file-readable-p file)
                (insert-file-contents file nil nil nil 'replace)
                (goto-char (point-min))
                (when (re-search-forward
                       (concat "^" (regexp-quote symbol) ":")
                       nil 'noerror)
                  (setq dest-file file
                        dest-offset (match-beginning 0))))))))
      (when dest-file
        (find-file dest-file)
        (goto-char dest-offset)))))

(defun my-ansible//encrypt-with-default-vault-id (args)
  (if (string= (car args) "encrypt")
      (cons "encrypt --encrypt-vault-id=default" (cdr args))
    args))

(defun my-ansible//auto-decrypt-encrypt-vault ()
  (when (and ansible
             (string-match-p "\\`\$ANSIBLE_VAULT;[0-9]+\.[0-9]+"
                             (buffer-substring-no-properties (point-min) (point-max))))
    (ansible-auto-decrypt-encrypt)
    (add-hook 'ws-butler-mode-hook #'my-ansible//fix-ws-butler-decrypt-order t t)))

(defun my-ansible//fix-ws-butler-decrypt-order ()
  (when (and ws-butler-mode (memq #'ansible-encrypt-buffer before-save-hook))
    (remove-hook 'before-save-hook #'ansible-encrypt-buffer t)
    (add-hook 'before-save-hook #'ansible-encrypt-buffer t t)))

(defun my-ansible//update-imenu-expression ()
  (when ansible
    (set (make-local-variable 'imenu-generic-expression)
         '(("var" "^\\(:?[a-zA-Z0-9_-]+\\):" 1)
           ("task" "^ *- +name: +\\(:?.*\\)" 1)))))

(defvar-local my-ansible-saved-coord '(1 1 1))

(defun my-ansible//save-coord ()
  (setq-local my-ansible-saved-coord
              (list (line-number-at-pos (window-start))
                    (line-number-at-pos (point))
                    (current-column))))

(defun my-ansible//restore-coord ()
  (goto-char (point-min))
  (forward-line (1- (car my-ansible-saved-coord)))
  (set-window-start nil (point))
  (forward-line (- (cadr my-ansible-saved-coord) (car my-ansible-saved-coord)))
  (move-to-column (caddr my-ansible-saved-coord) t)
  (kill-local-variable 'my-ansible-saved-coord))

(defun my-ansible/encrypt-region (beginning end)
  (interactive "r")
  (when (use-region-p)
    (let ((output (ansible-vault "encrypt" (buffer-substring-no-properties beginning end))))
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

(defun my-ansible/decrypt-region (beginning end)
  (interactive "r")
  (when (use-region-p)
    (let ((input (buffer-substring-no-properties beginning end)))
      (setq input (replace-regexp-in-string "\\`!vault |\n" "" input))
      (setq input (replace-regexp-in-string "^ +" "" input))
      (let ((output (ansible-vault "decrypt" input)))
        (delete-region beginning end)
        (insert output))
      (deactivate-mark)
      (set-mark beginning)
      (backward-char)
      (activate-mark))))

(defun my-ansible//quote-yaml (input)
  (if (or (zerop (length input))
          (memq (string-to-char input)
                (list ?# ?{ ?* ?!)))
      (concat "'" input "'")
    input))

(defun my-ansible/upgrade-syntax (beginning end)
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
              (insert key ": " (my-ansible//quote-yaml value))
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
                    (looking-at-p needle1)
                    (looking-at-p needle2)))
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
