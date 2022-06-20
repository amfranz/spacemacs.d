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
                  (file-name-absolute-p filename))
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
  (when (and (bound-and-true-p ansible)
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

(defvar molecule--directory nil)
(defvar molecule--instances nil)
(defun molecule--record-instances (command)
  (setq molecule--directory default-directory
        molecule--instances (shell-command-to-string
                             (concat command " --format plain"))))

(defun molecule-instances ()
  (cl-letf (((symbol-function 'compile) #'molecule--record-instances))
    (molecule-list))
  (let ((hosts nil))
    (dolist (line (split-string molecule--instances "\n" t))
      (let* ((columns (split-string line))
             (host (nth 0 columns))
             (created (nth 4 columns)))
        (if (equal created "true") (push host hosts))))
    ;; TODO: consider natsort https://stackoverflow.com/questions/1942045/natural-order-sort-for-emacs-lisp
    (cl-sort hosts 'string-lessp :key 'downcase)))

(defun molecule-dired (&optional as-root)
  (interactive "P")
  (let ((box-name (let* ((names (molecule-instances)))
                    (if (eq 1 (length names))
                        (car names)
                      (completing-read (concat (if as-root "[root]" "[vagrant]") " molecule dired to: ") names)))))
    (find-file (concat "/molecule:" (base64-encode-string molecule--directory t)
                       "@" box-name (if as-root (concat "|sudo:" box-name ":/root/") ":/home/vagrant/")))))

(defun molecule-login ()
  (interactive)
  (let* ((box-name (let* ((names (molecule-instances)))
                     (if (eq 1 (length names))
                         (car names)
                       (completing-read "molecule login to: " names)))))
    (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
      (call-process "konsole" nil 0 nil
                    "--workdir" molecule--directory "-e" "tmux" "new-session"
                    "/bin/zsh" "-i" "-c" (concat "molecule login --host " box-name)))))

(defvar molecule--converge-with-playbook-args-history nil)
(defun molecule-converge-with-playbook-args (playbook-args)
  (interactive
   (list (read-string-with-history "ansible-playbook arguments: "
                                   'molecule--converge-with-playbook-args-history)))
  (let ((orig-compile (symbol-function 'compile)))
    (cl-letf (((symbol-function 'compile)
               (lambda (command &rest orig-args)
                 (apply orig-compile (concat command " -- " playbook-args) orig-args))))
      (molecule-converge))))

(defun molecule-cache-dired ()
  (interactive)
  (when-let (project-dir (locate-dominating-file default-directory "molecule/default/molecule.yml"))
    (find-file (concat "~/.cache/molecule/"
                       (file-name-as-directory (file-name-nondirectory (directory-file-name project-dir)))))))

(defun yaml-syntax-propertize-extend-region (beg end)
  "Extend the region so its boundaries do not fall within a YAML block literal."
  (let ((changed nil))

    ;; Search backwards for the beginning of a non-blank, zero indentation line.
    (goto-char beg)
    (while (and (not (bobp))
                (or (looking-at yaml-blank-line-re)
                    (> (current-indentation) 0)))
      (forward-line -1))

    ;; Find lines that start a block literal before the original beginning of
    ;; the font lock region.
    (while (> beg (point))
      (if (looking-at yaml-block-literal-re)
          (let ((min-level (current-indentation))
                (block-beg (point)))

            ;; Find the line that ends the block literal.
            (goto-char (match-end 0))
            (while (and (not (eobp))
                        (or (looking-at yaml-blank-line-re)
                            (> (current-indentation) min-level)))
              (forward-line))

            ;; If the original beginning of the font lock region falls within
            ;; the block literal, move it to the line that starts the block.
            (when (and (> beg block-beg)
                       (< beg (point)))
              (setq changed t
                    beg block-beg)))
        (forward-line)))

    ;; Find lines that start a block literal before the original end of
    ;; the font lock region.
    (while (> end (point))
      (if (looking-at yaml-block-literal-re)
          (let ((min-level (current-indentation))
                (block-beg (point)))

            ;; Find the line that ends the block literal.
            (goto-char (match-end 0))
            (while (and (not (eobp))
                        (or (looking-at yaml-blank-line-re)
                            (> (current-indentation) min-level)))
              (forward-line))

            ;; If the original end of the font lock region falls within the
            ;; block literal, move it to the line that ends the block.
            (when (and (> end block-beg)
                       (< end (point)))
              (setq changed t
                    end (point))))
        (forward-line)))

    (when changed
      (cons beg end))))

(defun yaml-font-lock-extend-region ()
  "Extend the region so its boundaries do not fall within a YAML block literal."
  (when-let (new-region (yaml-syntax-propertize-extend-region font-lock-beg
                                                              font-lock-end))
    (setq font-lock-beg (car new-region)
          font-lock-end (cdr new-region))
    t))

(defun apply-yaml-font-lock-fixes ()
  ;; Extending the syntax-propertize / font-lock region correctly is key to
  ;; getting syntax highlighting for YAML block literals to work correctly.
  (add-to-list 'syntax-propertize-extend-region-functions #'yaml-syntax-propertize-extend-region 'append)
  (add-to-list 'font-lock-extend-region-functions #'yaml-font-lock-extend-region 'append)

  ;; This property needs to be registered as a managed so that it is properly
  ;; removed before each re-fontification and when font-lock is turned off.
  (add-to-list 'font-lock-extra-managed-props 'yaml-block-literal))

(defun yaml-backward-indent ()
  "Go to previous line with less indent than the current one."
  (let ((indent (current-indentation)))
    (while (and (not (bobp))
                (or (looking-at yaml-blank-line-re)
                    (>= (current-indentation) indent)))
      (forward-line -1))))

(defun ansible-task-level (line-beg-pos)
  "Calculate the indent level of the line at LINE-BEG-POS inside an Ansible task
construct. Will return -1 if no task construct can be identified."
  (save-excursion
    (goto-char line-beg-pos)
    (let ((indent (current-indentation))
          (level 0))
      (unless (looking-at "^ *- ")
        (let ((prior-indent (current-indentation)))
          (yaml-backward-indent)
          (while (and (not (bobp))
                      (not (looking-at "^ *- ")))
            (setq prior-indent (current-indentation)
                  level (1+ level))
            (yaml-backward-indent))
          ;; This fixes the indent level of "second" for this construct:
          ;; - task:
          ;;     second:
          (when (> (- prior-indent (current-indentation)) 2)
            (setq level (1+ level)))))
      (if (and (bobp)
               (or (> (current-indentation) indent)
                   (not (looking-at "^ *- "))))
          -1
        level))))

(defun ansible-section-keywords-regex-search (bound)
  (let ((found nil))
    (while (and (setq found (re-search-forward ansible-section-keywords-regex bound t))
                (or (get-text-property (match-beginning 1) 'yaml-block-literal)
                    (save-match-data
                      (not (eq 0 (ansible-task-level (match-beginning 0))))))))
    found))

(defun ansible-task-keywords-regex-search (bound)
  (let ((found nil))
    (while (and (setq found (re-search-forward ansible-task-keywords-regex bound t))
                (or (get-text-property (match-beginning 1) 'yaml-block-literal)
                    (save-match-data
                      (not (eq 0 (ansible-task-level (match-beginning 0))))))))
    found))

(defconst ansible-task-label-regex "^ *- \\(name\\): \\(.*\\)"
  "Regular expression to match an Ansible task label.")

(defun ansible-task-label-regex-search (bound)
  (let ((found nil))
    (while (and (setq found (re-search-forward ansible-task-label-regex bound t))
                (get-text-property (match-beginning 1) 'yaml-block-literal)))
    found))

(defun ansible-keywords-regex-search (bound)
  (let ((found nil))
    (while (and (setq found (re-search-forward ansible-keywords-regex bound t))
                (or (get-text-property (match-beginning 1) 'yaml-block-literal)
                    (save-match-data
                      (not (eq 0 (ansible-task-level (match-beginning 0))))))))
    found))
