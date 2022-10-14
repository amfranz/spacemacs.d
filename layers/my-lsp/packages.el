;; -*- lexical-binding: t -*-

(defconst my-lsp-packages '(company-lsp
                            lsp-mode
                            lsp-treemacs))

(defun my-lsp/post-init-company-lsp ()
  (setq company-lsp-cache-candidates 'auto))

(defun my-lsp/post-init-lsp-mode ()
  ;; There is no UI that shows the list of actions beforehand which makes
  ;; automatic execution often feel like a gamble - sometimes you just don't
  ;; know what you will get. To avoid this, let's always show the list of
  ;; actions first, even if there is just one.
  (setq lsp-auto-execute-action nil)

  ;; If the LSP backend shuts down, just leave it shut down. Don't interactively
  ;; ask whether to restart it (interrupts flow of thought), don't automatically
  ;; restart it (if it was due to a crash it will likely just crash again, if it
  ;; was requested to shut down well then it should stay shut down).
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
