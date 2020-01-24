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
  (add-hook 'go-mode-hook #'my-go//configure-flycheck)

  (with-eval-after-load 'go-mode
    ;; Add key bindings for running benchmarks.
    (spacemacs/safe-set-leader-keys-for-major-mode 'go-mode
      "tb" #'spacemacs/go-run-benchmark-current-function
      "tB" #'spacemacs/go-run-package-benchmarks)))

(defun my-go/init-fill-function-arguments ()
  (use-package fill-function-arguments
    :defer t
    :init
    (progn
      (add-hook 'go-mode-hook #'my-go--ffa-trailing-separator)
      (spacemacs/safe-set-leader-keys "aw" #'fill-function-arguments-dwim))))

(defun my-go/post-init-flycheck-golangci-lint ()
  ;; Only run fast tests to prevent the laptop getting hot.
  (setq flycheck-golangci-lint-fast t)

  ;; golangci-lint will report nasty and useless errors if the code does not
  ;; compile. To avoid that, we make sure go-build is the first checker to be
  ;; invoked, if it did not report any errors only then golangci-lint is to be
  ;; invoked.
  (add-hook 'go-mode-hook #'my-go//reenable-go-build t)
  (with-eval-after-load 'flycheck-golangci-lint
    (flycheck-add-next-checker 'go-build '(warning . golangci-lint))))

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
  (with-eval-after-load 'ws-butler
    ;; Disable `ws-butler' in `go-mode' buffers, gofmt will do the job instead.
    (add-to-list 'ws-butler-global-exempt-modes 'go-mode)))
