(defconst mfa-ansible-packages '(ansible ansible-doc company-ansible flycheck))

(defun mfa-ansible//auto-decrypt-encrypt-vault ()
  (when ansible
    (ansible::auto-decrypt-encrypt)))

(defun mfa-ansible//mark-buffer-not-modified ()
  (set-buffer-modified-p nil))

(defun mfa-ansible//update-imenu-expression ()
  (when ansible
      (set (make-local-variable 'imenu-generic-expression)
           '(("var" "^\\(:?[a-zA-Z0-9_-]+\\):" 1)
             ("task" "^ *- +name: +\\(:?.*\\)" 1)))))

(defun mfa-ansible/post-init-ansible ()
  (with-eval-after-load 'ansible
    (spacemacs|hide-lighter ansible)
    (add-hook 'ansible-hook
              #'mfa-ansible//update-imenu-expression)
    (add-hook 'hack-local-variables-hook
              #'mfa-ansible//auto-decrypt-encrypt-vault)
    (advice-add 'ansible::decrypt-buffer
                :after #'mfa-ansible//mark-buffer-not-modified)
    (spacemacs/declare-prefix-for-mode 'yaml-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'yaml-mode "mv" "vault")
    (spacemacs/set-leader-keys-for-major-mode 'yaml-mode
      "ve" #'ansible::encrypt-buffer
      "vd" #'ansible::decrypt-buffer)))

(defun mfa-ansible/post-init-ansible-doc ()
  (with-eval-after-load 'ansible-doc
    (spacemacs|hide-lighter ansible-doc-mode)
    (evilified-state-evilify-map ansible-doc-module-mode-map
      :mode ansible-doc-module-mode)))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun mfa-ansible/init-company-ansible ()
    (use-package company-ansible
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (setq company-backends-yaml-mode
                  '(company-files
                    (company-ansible company-dabbrev))))))

(defun mfa-ansible/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'yaml-mode))
