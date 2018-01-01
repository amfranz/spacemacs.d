(defconst mfa-ansible-packages '(ansible ansible-doc company el-patch flycheck))

(defun mfa-ansible/post-init-ansible ()
  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode/"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode/")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir)))
  ;; This is very similar to the default value, the only changes are
  ;; to anchor file names with a slash and the addition of the
  ;; '*.yaml' file extension.
  (setq spacemacs--ansible-filename-re
        (concat ".*/\\(?:"
                "main\.ya?ml\\|"
                "site\.ya?ml\\|"
                "encrypted\.ya?ml\\|"
                "roles/.+\.ya?ml\\|"
                "group_vars/.+\\|"
                "host_vars/.+\\)"))
  (with-eval-after-load 'ansible
    (spacemacs|hide-lighter ansible)
    (add-hook 'ansible-hook
              #'mfa-ansible//update-imenu-expression)
    (add-hook 'yaml-mode-local-vars-hook
              #'mfa-ansible//auto-decrypt-encrypt-vault))
  (advice-add 'ansible::encrypt-buffer :before
              #'mfa-ansible//save-coord)
  (advice-add 'ansible::decrypt-buffer :after
              #'mfa-ansible//restore-coord)
  (spacemacs/set-leader-keys-for-minor-mode 'ansible
    "u" #'mfa-ansible/upgrade-syntax))

(defun mfa-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(defun mfa-ansible/post-init-company ()
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'yaml-mode)))

(defun mfa-ansible/post-init-el-patch ()
  (eval '(el-patch-defun spacemacs/ansible-company-maybe-enable ()
           "Add the ansible company backend only for when ansible mode is active."
           (when (spacemacs//ansible-should-enable?)
             (el-patch-swap
               (add-to-list 'company-backends 'company-ansible)
               (setq-local company-backends
                           '(company-files
                             (company-ansible company-dabbrev-code))))))))

(defun mfa-ansible/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'yaml-mode))
