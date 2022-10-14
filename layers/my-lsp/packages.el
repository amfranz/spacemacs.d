;; -*- lexical-binding: t -*-

(defconst my-lsp-packages '(company-lsp
                            lsp-mode
                            lsp-treemacs))

(defun my-lsp/post-init-company-lsp ()
  (setq company-lsp-cache-candidates 'auto))

(defun my-lsp/post-init-lsp-mode ()
  (setq lsp-restart 'ignore)
  (with-eval-after-load 'lsp-mode
    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "hd" #'lsp-ui-doc-glance)
    (add-to-list 'lsp-language-id-configuration
                 '(direnv-envrc-mode . "shellscript"))))

(defun my-lsp/post-init-lsp-treemacs ()
  (with-eval-after-load 'treemacs
    (with-eval-after-load 'lsp-mode
      (lsp-treemacs-sync-mode))))
