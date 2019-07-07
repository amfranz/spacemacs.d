;; -*- lexical-binding: t -*-

(defconst my-ansible-packages '(ansible
                                 ansible-doc company
                                 company-ansible
                                 expand-region
                                 flycheck
                                 flycheck-yamllint
                                 yaml-mode))

(defun my-ansible/post-init-ansible ()
  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir)))
  ;; This is very similar to the default value, the changes are to anchor file
  ;; names with a slash, the addition of the '*.yaml' file extension and the
  ;; addition of the 'plays' directory.
  (setq spacemacs--ansible-filename-re
        (concat ".*/\\(?:"
                "main\.ya?ml\\|"
                "site\.ya?ml\\|"
                "encrypted\.ya?ml\\|"
                "roles/.+\.ya?ml\\|"
                "group_vars/.+\\|"
                "host_vars/.+\\|"
                "plays/.+\\)"))
  (with-eval-after-load 'ansible
    (add-to-list 'ansible-playbook-font-lock
                 '("\\({%\\)\\(.*?\\)\\(%}\\)"
                   (1 font-lock-builtin-face t)
                   (2 font-lock-function-name-face t)
                   (3 font-lock-builtin-face t)))
    (spacemacs|hide-lighter ansible)
    (add-hook 'ansible-hook
              #'my-ansible//update-imenu-expression)
    (add-hook 'yaml-mode-local-vars-hook
              #'my-ansible//auto-decrypt-encrypt-vault))
  (advice-add 'ansible-vault :around
              #'my-ansible//vault-encrypt-advice)
  (advice-add 'ansible-encrypt-buffer :before
              #'my-ansible//save-coord)
  (advice-add 'ansible-decrypt-buffer :after
              #'my-ansible//restore-coord)
  (with-eval-after-load 'ansible
    (spacemacs/set-leader-keys-for-minor-mode 'ansible
      "ml" #'molecule-login
      "re" #'my-ansible/encrypt-region
      "rd" #'my-ansible/decrypt-region
      "u" #'my-ansible/upgrade-syntax)))

(defun my-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(defun my-ansible/post-init-company ()
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'yaml-mode)))

(defun my-ansible/post-init-company-ansible ()
  (remove-hook 'yaml-mode-hook
               'spacemacs/ansible-company-maybe-enable)
  (add-hook 'yaml-mode-hook
            'my-ansible//ansible-company-maybe-enable t))

(defun my-ansible/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'yaml-mode #'er/add-yaml-mode-expansions)))

(defun my-ansible/post-init-flycheck ()
  (spacemacs/enable-flycheck 'yaml-mode)
  ;; The above does not seem to work when a YAML file is the first file to be
  ;; visited. This is a temporary workaround for now.
  (add-hook 'yaml-mode-hook #'flycheck-mode))

(defun my-ansible/init-flycheck-yamllint ()
  (use-package flycheck-yamllint
    :after (flycheck yaml-mode)
    :config
    (flycheck-yamllint-setup)))

(defun my-yaml--indent ()
  (when (looking-at "\\( *\\)[^ #\n]")
    (- (match-end 1) (match-beginning 1))))

(defun my-yaml--any-indent-backwards ()
  (let (indent)
    (while (not (or indent (bobp)))
      (forward-line -1)
      (setq indent (my-yaml--indent)))
    indent))

(defun my-yaml--lesser-indent-backwards (guide)
  (let ((indent (my-yaml--any-indent-backwards)))
    (while (and indent (>= indent guide))
      (setq indent (my-yaml--any-indent-backwards)))
    indent))

;; TODO: handle comments as well as other block literal types
(defun my-yaml--looking-at-block-start-p ()
  (seq-some #'looking-at-p '(" *[a-zA-Z0-9_\\-\\.]+: |"
                             " *- *\\(?:[a-zA-Z0-9_\\-\\.]+:\\)? |")))

(defun my-yaml--block-start ()
  (let (block-start block-indent)
    (goto-char (point-at-bol))
    (let ((indent (or (my-yaml--indent)
                      (my-yaml--any-indent-backwards))))
      (while indent
        (when (my-yaml--looking-at-block-start-p)
          (setq block-start (point)
                block-indent indent))
        (setq indent (my-yaml--lesser-indent-backwards indent))))
    (when block-start
      (cons block-start block-indent))))

(defun my-yaml--block-end (guide)
  (let* ((block-end (point))
         (indent (my-yaml--any-indent-forwards (point-max))))
    (while (and indent (> indent guide))
      (setq block-end (point))
      (setq indent (my-yaml--any-indent-forwards (point-max))))
    (goto-char block-end)
    (point-at-eol)))

(defun my-yaml--surrounding-block ()
  (when-let* ((start (my-yaml--block-start)))
    (goto-char (car start))
    (cons (car start) (my-yaml--block-end (cdr start)))))

(defun my-yaml--any-indent-forwards (bound)
  (let (indent)
    (while (not (or indent (>= (point) bound)))
      (forward-line 1)
      (setq indent (my-yaml--indent)))
    indent))

(defun my-yaml--line-end-position-at (pos)
  (let ((saved-point (point))
        end-point)
    (goto-char pos)
    (goto-char (line-end-position))
    (setq end-point (point))
    (goto-char saved-point)
    end-point))

(defun my-yaml--font-lock-block-literals (bound)
  ;; (let ((block-bounds (save-excursion (my-yaml--surrounding-block))))
  ;;   (when block-bounds
  ;;     (when (< (car block-bounds) (point)))
  ;;     ))
  (message "called for %s until %s" (point) bound)
  (let (block-start block-last)
    (while (not (or block-last (>= (point) bound)))
      (if (not (my-yaml--looking-at-block-start-p))
          (forward-line 1)
        (let* ((guide (my-yaml--indent))
               (indent (my-yaml--any-indent-forwards bound)))
          (setq block-start (point))
          (while (and indent (> indent guide))
            (setq block-last (point))
            (setq indent (my-yaml--any-indent-forwards bound))))))
    (when block-last
      (let ((block-end (min bound (my-yaml--line-end-position-at block-last))))
        (message "returning %s %s with point at %d" block-start block-end (point))
        (put-text-property block-start block-end 'yaml-block-literal t)
        (put-text-property block-start block-end 'font-lock-multiline t)
        (set-match-data (list block-start block-end)))
      t)))

(defun my-ansible//font-lock-extend-region ()
  (eval-when-compile
    (defvar font-lock-beg)
    (defvar font-lock-end))
  (let (altered)
    (save-excursion
      (goto-char font-lock-beg)
      (when-let* ((block-bounds (my-yaml--surrounding-block)))
        (unless (= font-lock-beg (car block-bounds))
          (message "altered font-lock-beg to %d" (car block-bounds))
          (setq font-lock-beg (car block-bounds)
                altered t)))
      (goto-char font-lock-end)
      (when-let* ((block-bounds (my-yaml--surrounding-block)))
        (unless (= font-lock-end (cdr block-bounds))
          (message "altered font-lock-end to %d" (cdr block-bounds))
          (setq font-lock-end (cdr block-bounds)
                altered t))))
    altered)
  ;; (save-excursion
  ;;   (let ((needle1 " *[a-zA-Z0-9_\\-\\.]+: |")
  ;;         (needle2 " *- *\\(?:[a-zA-Z0-9_\\-\\.]+:\\)? |")
  ;;         (orig-point (point))
  ;;         block-ind)
  ;;     (beginning-of-line)
  ;;     (looking-at " *")
  ;;     (let ((indent (- (match-end 0)
  ;;                      (match-beginning 0))))
  ;;       (while (> indent 0)
  ;;         (let ((line-indent indent))
  ;;           (while (>= line-indent indent)
  ;;             (forward-line -1)
  ;;             (looking-at " *")
  ;;             (setq line-indent (- (match-end 0)
  ;;                                  (match-beginning 0))))
  ;;           (when (or (looking-at-p needle1)
  ;;                     (looking-at-p needle2))
  ;;             (setq font-lock-beg (point)
  ;;                   block-ind line-indent))
  ;;           (setq indent line-indent))))
  ;;     (when block-ind
  ;;       (goto-char orig-point)
  ;;       (let ((line-indent block-ind))
  ;;         (while (>= line-indent block-ind)
  ;;           (forward-line 1)
  ;;           (looking-at " *")
  ;;           (setq line-indent (- (match-end 0)
  ;;                                (match-beginning 0)))))
  ;;       (setq font-lock-end (point)))))
  )

;; (defun my-ansible/test-font-lock-extend-region ()
;;   (interactive)
;;   (when (my-ansible//font-lock-extend-region)
;;     (set-mark font-lock-beg)
;;     (goto-char font-lock-end)
;;     (backward-char)
;;     (activate-mark)))

(defun my-ansible/test-font-lock-extend-region ()
  (interactive)
  (when (yaml-font-lock-block-literals (point-max))
    (message "%s" (match-data)))
  ;; (if-let* ((bounds (my-yaml--surrounding-block)))
  ;;     (progn
  ;;       (set-mark (car bounds))
  ;;       (goto-char (cdr bounds))
  ;;       (activate-mark))
  ;;   (message "nothing found"))
  )

;; (advice-add 'yaml-font-lock-block-literals :override #'my-yaml-font-lock-block-literals)
;; (advice-remove 'yaml-font-lock-block-literals 'my-yaml-font-lock-block-literals)

(defun my-ansible//font-lock-setup ()
  ;; Preserve highlight of multiline Jinja2 blocks.
  (setq font-lock-multiline t)
  ;; (push #'my-ansible//font-lock-extend-region font-lock-extend-region-functions)
  )

(defun my-ansible/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.yamllint\\'" . yaml-mode))
  ;; Attempt to fix syntax highlighting of multi-line Jinja expressions.
  (add-hook 'yaml-mode-hook #'my-ansible//font-lock-setup)
  ;; Fontify URLs in Ansible buffers and make them interactive.
  (add-hook 'yaml-mode-hook #'goto-address-prog-mode)
  (with-eval-after-load 'yaml-mode
    (spacemacs/declare-prefix-for-mode 'yaml-mode "mr" "region")
    (spacemacs/set-leader-keys-for-major-mode 'yaml-mode
      "w" #'my-ansible/test-font-lock-extend-region
      "t" #'describe-text-properties)

    ;; Fixes auto-completion of words containing "_" or "-" by company. Without
    ;; this, entering "_" or "-" of a completion candidate would close the
    ;; company popup.
    (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)
    (modify-syntax-entry ?- "w" yaml-mode-syntax-table)))
