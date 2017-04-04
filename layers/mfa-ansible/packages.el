(defconst mfa-ansible-packages '(ansible ansible-doc company-ansible flycheck))

(defun mfa-ansible/post-init-ansible ()
  ;; Ugly ugly hack that addresses https://github.com/k1LoW/emacs-ansible/issues/5
  (let* ((pkg-dir (package-desc-dir (cadr (assq 'ansible package-alist))))
         (txt-dir (concat pkg-dir "/snippets/text-mode/"))
         (yml-dir (concat pkg-dir "/snippets/yaml-mode/")))
    (when (file-directory-p txt-dir)
      (when (file-directory-p yml-dir)
        (delete-directory yml-dir t))
      (rename-file txt-dir yml-dir)))
  (with-eval-after-load 'ansible
    (spacemacs|hide-lighter ansible)
    (add-hook 'ansible-hook
              #'mfa-ansible//update-imenu-expression)
    (add-hook 'hack-local-variables-hook
              #'mfa-ansible//auto-decrypt-encrypt-vault)))

(defun mfa-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(defun mfa-ansible/post-init-company-ansible ()
  (setq company-backends-yaml-mode
        '(company-files
          (company-ansible company-dabbrev))))

(defun mfa-ansible/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'yaml-mode))
