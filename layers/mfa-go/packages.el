;; -*- lexical-binding: t -*-

(defconst mfa-go-packages '(go-dlv go-mode flycheck prodigy projectile ws-butler))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun mfa-go/post-init-go-mode ()
  ;; Recommended by the Spacemacs manual.
  (setq godoc-at-point-function 'godoc-gogetdoc
        gofmt-command "goimports"
        gofmt-show-errors 'echo)

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

(defun mfa-go/post-init-flycheck ()
  ;; The dependencies need to be installed for `company-go' to be able to offer
  ;; meaningful source code completion options.
  (setq flycheck-go-build-install-deps t))

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
  (with-eval-after-load 'projectile
    (let ((goprjtype (gethash 'go projectile-project-types)))
      (dolist (cmd '((compile-command . "go build -i")
                     (test-command . "go test")))
        (setq goprjtype (plist-put goprjtype (car cmd) (cdr cmd))))
      (puthash 'go goprjtype projectile-project-types))))

(defun mfa-go/post-init-ws-butler ()
  ;; Disable ws-butler for go source code, go fmt will do the job instead.
  (with-eval-after-load 'ws-butler
    (push 'go-mode ws-butler-global-exempt-modes)))
