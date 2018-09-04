;; -*- lexical-binding: t -*-

(defconst mfa-go-packages '(go-dlv go-mode flycheck prodigy projectile))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun mfa-go/post-init-go-mode ()
  (setq godoc-at-point-function 'godoc-gogetdoc
        gofmt-command "goimports"))

(defun mfa-go/post-init-flycheck ()
  (setq flycheck-go-build-install-deps t
        flycheck-gometalinter-fast t
        flycheck-gometalinter-vendor t))

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
