;; -*- lexical-binding: t -*-

(defconst my-ansible-packages '(ansible
                                ansible-doc
                                company
                                company-ansible
                                expand-region
                                flycheck
                                flycheck-yamllint
                                molecule
                                yaml-mode))

(defun my-ansible/post-init-ansible ()
  ;; Customize the YAML files in which Ansible mode will be enabled. The value
  ;; of this regular expression is based on the default value provided by
  ;; Spacemacs. The differences are:
  ;;   1) use `rx' to make the regular expression more readable
  ;;   2) avoid capturing groups since they are not going to be needed
  ;;   3) anchor the end of the regular expression
  ;;   4) the addition of the "plays" directory
  (setq spacemacs--ansible-filename-re
        (rx "/"
            (or "main"
                "site"
                "encrypted"
                (and (or "roles"
                         "tasks"
                         "handlers"
                         "vars"
                         "defaults"
                         "meta"
                         "group_vars"
                         "host_vars"
                         "plays")
                     "/" (one-or-more not-newline)))
            ".y" (optional "a") "ml"
            string-end))

  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir)))

  ;; Automatically decrypt files containing encrypted Ansible Vault content when
  ;; they are opened, and automatically encrypt them when they are saved.
  (add-hook 'yaml-mode-local-vars-hook
            #'my-ansible//auto-decrypt-encrypt-vault)
  (advice-add 'ansible-vault :filter-args
              #'my-ansible//encrypt-with-default-vault-id)
  (advice-add 'ansible-encrypt-buffer :before
              #'my-ansible//save-coord)
  (advice-add 'ansible-decrypt-buffer :after
              #'my-ansible//restore-coord)

  ;; Make imenu useful by teaching it parsing rules to extract the names of
  ;; top level variables and tasks in Ansible playbooks and roles.
  (add-hook 'ansible-hook
            #'my-ansible//update-imenu-expression)

  ;; Register jump handlers for Ansible role templates, files and variables.
  (add-to-list 'spacemacs-jump-handlers-yaml-mode #'my-ansible/jump-to-template)
  (add-to-list 'spacemacs-jump-handlers-yaml-mode #'my-ansible/jump-to-variable)

  ;; Make syntax highlighting semantically more accurate.
  (eval
   '(el-patch-defvar ansible-playbook-font-lock
      (el-patch-swap
        ;; Original ruleset.
        `(("\\({{\\)\\([^}]+\\)\\(}}\\)"
           (1 font-lock-builtin-face t)
           (2 font-lock-function-name-face t)
           (3 font-lock-builtin-face t))
          (,ansible-section-keywords-regex (1 ansible-section-face t))
          (,ansible-task-keywords-regex (1 font-lock-keyword-face t))
          ("^ *- \\(name\\):\\(.*\\)"
           (1 font-lock-builtin-face t)
           (2 ansible-task-label-face t))
          (,ansible-keywords-regex (1 font-lock-builtin-face t)))

        ;; Improved ruleset.
        '(("\\({{\\)\\([^}]+\\)\\(}}\\)"
           (1 font-lock-builtin-face t)
           (2 font-lock-function-name-face t)
           (3 font-lock-builtin-face t))

          ;; Also syntax highlight Jinja code blocks in Ansible playbooks and
          ;; roles. This is based on the rules for Jinja expressions above.
          ("\\({%\\)\\([^%]+\\)\\(%}\\)"
           (1 font-lock-builtin-face t)
           (2 font-lock-function-name-face t)
           (3 font-lock-builtin-face t))

          ;; Also syntax highlight Jinja comment blocks in Ansible playbooks and
          ;; roles. This is based on the rules for Jinja expressions above.
          ("\\({#\\)\\([^#]+\\)\\(#}\\)"
           (1 font-lock-builtin-face t)
           (2 font-lock-comment-face t)
           (3 font-lock-builtin-face t))

          ;; All of the following regular expressions have been replaced by
          ;; functions that will filter out keywords if they are not on the
          ;; right task indent level, eg. "group" is a task keyword, but not
          ;; if passed as argument to the "file" task.
          (ansible-section-keywords-regex-search (1 ansible-section-face t))
          (ansible-task-keywords-regex-search (1 font-lock-keyword-face t))
          (ansible-task-label-regex-search
           (1 font-lock-builtin-face t)
           (2 ansible-task-label-face t))
          (ansible-keywords-regex-search (1 font-lock-builtin-face t))))
      "Font lock definitions for ansible playbooks."))

  (el-patch-feature ansible)
  (with-eval-after-load 'ansible
    (el-patch-defconst ansible-section-keywords-regex
      (concat
       "^ *-? "
       (regexp-opt
        '("hosts" "vars" "vars_prompt" "vars_files" "role" "include" "include_tasks"
          "roles" "tasks" "import_tasks" "handlers" "pre_tasks" "post_tasks" "environment"
          (el-patch-add "import_playbook" "import_role" "include_role" "include_vars"))
        t)
       ":")
      "Special keywords used to identify toplevel information in a playbook.")

    (eval
     '(el-patch-define-minor-mode ansible
        "Ansible minor mode."
        :lighter " Ansible"
        :group 'ansible
        (if ansible
            (progn
              ;; Avoid adding to `minor-mode-map-alist' when the entry is
              ;; already present.
              (el-patch-swap
                (setq minor-mode-map-alist
                      (cons (cons 'ansible ansible-key-map)
                            minor-mode-map-alist))
                (add-to-list 'minor-mode-map-alist (cons 'ansible ansible-key-map)))
              (ansible-dict-initialize)
              (ansible-remove-font-lock)
              (ansible-add-font-lock)
              ;; Make integration into yasnippet behave consistent regardless
              ;; whether or not `yasnippet' has been loaded yet. (part 1)
              (el-patch-remove
                (when (featurep 'yasnippet)
                  (add-to-list 'yas-snippet-dirs ansible-snip-dir t)
                  (yas-load-directory ansible-snip-dir)))
              ;; The snippets have been moved to `yaml-mode', there is no need
              ;; to unload them any more. (part 1)
              (el-patch-remove
                (add-hook 'kill-buffer-hook #'ansible-maybe-unload-snippets nil t))
              (run-hooks 'ansible-hook))
          (ansible-remove-font-lock)
          ;; The snippets have been moved to `yaml-mode', there is no need to
          ;; unload them any more. (part 2)
          (el-patch-remove
            (ansible-maybe-unload-snippets 0))))
     t)

    ;; Make integration into yasnippet behave consistent regardless
    ;; whether or not `yasnippet' has been loaded yet. (part 2)
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs ansible-snip-dir t))
    (when (bound-and-true-p yas-global-mode)
      (yas-load-directory ansible-snip-dir))

    ;; Make the modeline less verbose.
    (spacemacs|hide-lighter ansible)

    ;; Additional keybindings for Ansible playbooks and roles.
    (spacemacs/set-leader-keys-for-minor-mode 'ansible
      "rd" #'my-ansible/decrypt-region
      "re" #'my-ansible/encrypt-region
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
  (add-hook 'yaml-mode-hook #'my-ansible//flycheck-setup))

(defun my-ansible/init-flycheck-yamllint ()
  (use-package flycheck-yamllint
    :defer t))

(defun my-ansible/init-molecule ()
  (use-package molecule
    :defer t
    :init
    (progn
      (with-eval-after-load 'tramp
        (add-to-list 'tramp-methods
                     `("molecule"
                       (tramp-login-program ,(concat dotspacemacs-directory "bin/molecule-tramp"))
                       (tramp-login-args (("%u") ("%h")))
                       (tramp-remote-shell "/bin/sh")
                       (tramp-remote-shell-args ("-c")))))
      (with-eval-after-load 'recentf
        (add-to-list 'recentf-exclude "\\`/molecule:")))
    :config
    (setq molecule-command "PY_COLORS=1 ANSIBLE_FORCE_COLOR=1 molecule")))

(defun my-ansible/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.yamllint\\'" . yaml-mode))

  ;; Apply miscellaneous fixes to the font lock logic of yaml-mode.
  (add-hook 'yaml-mode-hook #'apply-yaml-font-lock-fixes)

  ;; Fontify URLs in Ansible buffers and make them interactive.
  (add-hook 'yaml-mode-hook #'goto-address-prog-mode)

  ;; This replaces the broken logic that tries to prevent string detection
  ;; inside YAML block literals with different logic that is hopefully more
  ;; reliable. The issue we are trying to fix is that `font-lock-string-face'
  ;; sometimes spans multiple lines past the end of the YAML block literal.
  (el-patch-feature yaml-mode)
  (with-eval-after-load 'yaml-mode
    (eval
     '(el-patch-defun yaml-mode-syntax-propertize-function (beg end)
        "Override buffer's syntax table for special syntactic constructs."

        ;; Unhighlight foo#bar tokens between BEG and END.
        (save-excursion
          (goto-char beg)
          (while (search-forward "#" end t)
            (save-excursion
              (forward-char -1)
              ;; both ^# and [ \t]# are comments
              (when (and (not (bolp))
                         (not (memq (preceding-char) '(?\s ?\t))))
                (put-text-property (point) (1+ (point))
                                   'syntax-table (string-to-syntax "_"))))))

        (el-patch-swap
          ;; Original logic.
          (save-excursion
            (goto-char beg)
            (while (and
                    (> end (point))
                    (re-search-forward "['\"]" end t))
              (when (get-text-property (point) 'yaml-block-literal)
                (put-text-property (1- (point)) (point)
                                   'syntax-table (string-to-syntax "w")))
              (let* ((pt (point))
                     (sps (save-excursion (syntax-ppss (1- pt)))))
                (when (not (nth 8 sps))
                  (cond
                   ((and (char-equal ?' (char-before (1- pt)))
                         (char-equal ?' (char-before pt)))
                    (put-text-property (- pt 2) pt
                                       'syntax-table (string-to-syntax "w"))
                    ;; Workaround for https://debbugs.gnu.org/41195.
                    (let ((syntax-propertize--done syntax-propertize--done))
                      ;; Carefully invalidate the last cached ppss.
                      (syntax-ppss-flush-cache (- pt 2))))
                   ;; If quote is detected as a syntactic string start but appeared
                   ;; after a non-whitespace character, then mark it as syntactic word.
                   ((and (char-before (1- pt))
                         (char-equal ?w (char-syntax (char-before (1- pt)))))
                    (put-text-property (1- pt) pt
                                       'syntax-table (string-to-syntax "w")))
                   (t
                    ;; We're right after a quote that opens a string literal.
                    ;; Skip over it (big speedup for long JSON strings).
                    (goto-char (1- pt))
                    (condition-case nil
                        (forward-sexp)
                      (scan-error
                       (goto-char end)))))))))

          ;; Improved logic.
          (save-excursion
            (goto-char beg)
            (while (> end (point))
              (if (looking-at yaml-block-literal-re)
                  (let ((min-level (current-indentation)))
                    (goto-char (match-end 0))
                    (while (and (> end (point))
                                (or (looking-at yaml-blank-line-re)
                                    (when (> (current-indentation) min-level)
                                      (let ((bound (min end (let ((inhibit-field-text-motion t))
                                                              (line-end-position)))))
                                        (while (re-search-forward "['\"]" bound t)
                                          (put-text-property (1- (point)) (point)
                                                             'syntax-table (string-to-syntax "w"))))
                                      t)))
                      (forward-line)))
                (forward-line)))))))

    ;; Buffers with the `ansible' minor mode enabled will add bindings under
    ;; this prefix. Prefixes are associated with major modes though, so we need
    ;; to define it here instead of the Ansible mode config.
    (spacemacs/declare-prefix-for-mode 'yaml-mode "mr" "region")

    ;; Fixes auto-completion of words containing "_" or "-" by company. Without
    ;; this, entering "_" or "-" of a completion candidate would close the
    ;; company popup.
    (modify-syntax-entry ?_ "w" yaml-mode-syntax-table)
    (modify-syntax-entry ?- "w" yaml-mode-syntax-table)))
