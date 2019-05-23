;; -*- lexical-binding: t -*-

(defconst mfa-go-packages '(compile
                            company-go
                            go-eldoc
                            go-dlv
                            go-mode
                            fill-function-arguments
                            ;; flycheck
                            flycheck-golangci-lint
                            prodigy
                            projectile
                            ws-butler))

(defun mfa-go/post-init-compile ()
  (with-eval-after-load 'compile
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(go-testify "^    \\(\\([a-zA-Z0-9_-]+\\.go\\):\\([0-9]+\\):\\) $" 2 3 nil nil 1))
    (add-to-list 'compilation-error-regexp-alist 'go-testify)))

(defun mfa-go/post-init-company-go ()
  (setq company-go-gocode-command "gocode-gomod"))

(defun mfa-go/post-init-go-eldoc ()
  (setq go-eldoc-gocode "gocode-gomod")
  ;; go-eldoc causes significant lag in go mode. This setup function also
  ;; overrides `eldoc-documentation-function', which lsp mode tries to
  ;; customize.
  (remove-hook 'go-mode-hook #'go-eldoc-setup))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun mfa-go/post-init-go-mode ()
  ;; Recommended by the Spacemacs manual.
  (setq gofmt-show-errors 'echo
        ;; godoc-at-point-function 'godoc-gogetdoc
        ;; gofmt-command "goimports"
        )

  ;; This function clobbers the match data in `before-change-functions',
  ;; which breaks other things (eg. iedit-mode, evil regex replace)
  (advice-add 'go--reset-dangling-cache-before-change
              :around #'my--ad-save-match-data)

  ;; This will cause the value of go-tab-width to carry over to evil-shift-width.
  (push '(go-mode . go-tab-width) spacemacs--indent-variable-alist)

  ;; Shorten the names of vendored packages when adding imports with
  ;; `go-import-add'.
  (advice-add 'go-packages :filter-return
              #'mfa-go//shorten-vendored-package-names)

  (with-eval-after-load 'go-mode
    ;; Define which-key prefixes when Spacemacs does not.
    (spacemacs/declare-prefix-for-mode 'go-mode "mT" "tags")
    (spacemacs/declare-prefix-for-mode 'go-mode "mtg" "go-gen-test")

    ;; Add key bindings for running benchmarks.
    (spacemacs/declare-prefix-for-mode 'go-mode "mb" "benchmarks")
    (spacemacs/safe-set-leader-keys-for-major-mode 'go-mode
      "bb" #'spacemacs/go-run-benchmark-current-function
      "bp" #'spacemacs/go-run-package-benchmarks)))

(defun mfa-go/init-fill-function-arguments ()
  (use-package fill-function-arguments
    :defer t
    :init
    (progn
      (add-hook 'go-mode-hook #'mfa-go--ffa-trailing-separator)
      (spacemacs/safe-set-leader-keys "aw" #'fill-function-arguments-dwim))))

;; (defun mfa-go/post-init-flycheck ()
;;   ;; The dependencies need to be installed for `company-go' to be able to offer
;;   ;; meaningful source code completion options.
;;   (setq flycheck-go-build-install-deps t))

(defun mfa-go/post-init-flycheck-golangci-lint ()
  (setq flycheck-golangci-lint-fast t))

(defun mfa-go/post-init-prodigy()
  (with-eval-after-load 'prodigy
    (prodigy-define-service
      :name "Go Documentation Server"
      :command "godoc"
      :args '("-http" "127.0.0.1:6060" "-v")
      :port 6060
      :url "http://localhost:6060/"
      :cwd user-home-directory)))

(defun mfa-go/post-init-projectile ()
  ;; Customize the default build and test commands for Go projects.
  (when nil ;; FIXME broken
  (with-eval-after-load 'projectile
    (let ((goprjtype (gethash 'go projectile-project-types)))
      (dolist (cmd '((compile-command . "go build -i")
                     (test-command . "go test")))
        (setq goprjtype (plist-put goprjtype (car cmd) (cdr cmd))))
      (puthash 'go goprjtype projectile-project-types)))))

(defun mfa-go/post-init-ws-butler ()
  ;; Disable ws-butler for go source code, go fmt will do the job instead.
  (with-eval-after-load 'ws-butler
    (push 'go-mode ws-butler-global-exempt-modes)))
