(defconst mfa-ansible-packages '(ansible ansible-doc company expand-region flycheck flycheck-yamllint yaml-mode))

(defun mfa-ansible/post-init-ansible ()
  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode")))
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
  (spacemacs|use-package-add-hook ansible
    :post-config
    (progn
      (remove-hook 'yaml-mode-hook
                   'spacemacs/ansible-company-maybe-enable)
      (add-hook 'yaml-mode-hook
                'mfa-ansible//ansible-company-maybe-enable t)))
  (with-eval-after-load 'ansible
    (spacemacs|hide-lighter ansible)
    (add-hook 'ansible-hook
              #'mfa-ansible//update-imenu-expression)
    (add-hook 'yaml-mode-local-vars-hook
              #'mfa-ansible//auto-decrypt-encrypt-vault))
  (advice-add 'ansible::vault :around
              #'mfa-ansible//vault-encrypt-advice)
  (advice-add 'ansible::encrypt-buffer :before
              #'mfa-ansible//save-coord)
  (advice-add 'ansible::decrypt-buffer :after
              #'mfa-ansible//restore-coord)
  (with-eval-after-load 'ansible
    (spacemacs/set-leader-keys-for-minor-mode 'ansible
      "re" #'mfa-ansible/encrypt-region
      "rd" #'mfa-ansible/decrypt-region
      "u" #'mfa-ansible/upgrade-syntax)))

(defun mfa-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(defun mfa-ansible/post-init-company ()
  (with-eval-after-load 'company-dabbrev-code
    (add-to-list 'company-dabbrev-code-modes 'yaml-mode)))

(defun mfa-ansible/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (er/enable-mode-expansions 'yaml-mode #'er/add-yaml-mode-expansions)))

(defun mfa-ansible/post-init-flycheck ()
  (spacemacs/enable-flycheck 'yaml-mode))

(defun mfa-ansible/init-flycheck-yamllint ()
  (use-package flycheck-yamllint
    :after (flycheck yaml-mode)
    :config
    (flycheck-yamllint-setup)))

(defun mfa-ansible/post-init-yaml-mode ()
  (spacemacs/declare-prefix-for-mode 'yaml-mode "mr" "region"))
