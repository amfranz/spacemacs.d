;; -*- lexical-binding: t -*-

(defconst my-go-packages '(compile
                           company-go
                           go-eldoc
                           go-dlv
                           go-mode
                           fill-function-arguments
                           flycheck-golangci-lint
                           prodigy
                           projectile
                           ws-butler))

(defun my-go/post-init-compile ()
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(go-testify "^    \\(\\([a-zA-Z0-9_-]+\\.go\\):\\([0-9]+\\):\\) $" 2 3 nil nil 1))
    (add-to-list 'compilation-error-regexp-alist 'go-testify)))

(defun my-go/post-init-company-go ()
  (setq company-go-gocode-command "gocode-gomod"))

(defun my-go/post-init-go-eldoc ()
  (setq go-eldoc-gocode "gocode-gomod"))

(defun my-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun my-go/post-init-go-mode ()
  ;; Avoid a window popping up when gofmt fails. It breaks the flow.
  (setq gofmt-show-errors 'echo)

  ;; Use gogetdoc, which is more feature-rich than gocode, for auto-completion.
  (setq godoc-at-point-function 'godoc-gogetdoc)

  ;; This will adjust `evil-shift-width' to the value of `tab-width'.
  ;; `tab-width' can be set by editorconfig or directory-local variables.
  (setf (alist-get 'go-mode spacemacs--indent-variable-alist) 'tab-width)

  ;; Shorten the names of vendored packages when adding imports with
  ;; `go-import-add'.
  (setq go-packages-function #'my-go/go-packages-gopkgs)

  ;; Use both lsp-ui as well as golangci-lint as linters.
  (add-hook 'go-mode-hook 'my-go//lsp-ui-flycheck-enable 'append)

  (with-eval-after-load 'go-mode
    ;; Define which-key prefixes when Spacemacs does not.
    (spacemacs/declare-prefix-for-mode 'go-mode "mT" "tags")
    (spacemacs/declare-prefix-for-mode 'go-mode "mtg" "go-gen-test")

    ;; Add key bindings for running benchmarks.
    (spacemacs/declare-prefix-for-mode 'go-mode "mb" "benchmarks")
    (spacemacs/safe-set-leader-keys-for-major-mode 'go-mode
      "bb" #'spacemacs/go-run-benchmark-current-function
      "bp" #'spacemacs/go-run-package-benchmarks)))

(defun my-go/init-fill-function-arguments ()
  (use-package fill-function-arguments
    :defer t
    :init
    (progn
      (add-hook 'go-mode-hook #'my-go--ffa-trailing-separator)
      (spacemacs/safe-set-leader-keys "aw" #'fill-function-arguments-dwim))))

(defun my-go//lsp-ui-flycheck-enable ()
  ;; Make flycheck less eager to lint.
  ;; Linting after every keystroke is making the editor sluggish.
  (setq-local lsp-ui-flycheck-live-reporting nil)
  (setq-local flycheck-idle-change-delay 2)
  (setq-local flycheck-idle-buffer-switch-delay 2)
  (setq-local flycheck-check-syntax-automatically
              '(save idle-change idle-buffer-switch mode-enabled))

  ;; Mark LSP as the linter for flycheck to use.
  (require 'lsp-ui-flycheck)
  (lsp-ui-flycheck-enable nil))

(defun my-go/post-init-flycheck-golangci-lint ()
  (setq flycheck-golangci-lint-fast t)
  (with-eval-after-load 'flycheck-golangci-lint
    (with-eval-after-load 'lsp-ui
      (flycheck-add-next-checker 'lsp-ui '(warning . golangci-lint)))))

(defun my-go/post-init-prodigy()
  (with-eval-after-load 'prodigy
    (prodigy-define-service
      :name "Go Documentation Server"
      :command "godoc"
      :args '("-http" "127.0.0.1:6060" "-v")
      :port 6060
      :url "http://localhost:6060/"
      :cwd user-home-directory)))

(defun my-go/post-init-projectile ()
  ;; Customize the default build and test commands for Go projects.
  (when nil ;; FIXME: broken
    (with-eval-after-load 'projectile
      (let ((goprjtype (gethash 'go projectile-project-types)))
        (dolist (cmd '((compile-command . "go build -i")
                       (test-command . "go test")))
          (setq goprjtype (plist-put goprjtype (car cmd) (cdr cmd))))
        (puthash 'go goprjtype projectile-project-types)))))

(defun my-go/post-init-ws-butler ()
  ;; Disable ws-butler for go source code, go fmt will do the job instead.
  (with-eval-after-load 'ws-butler
    (push 'go-mode ws-butler-global-exempt-modes)))
