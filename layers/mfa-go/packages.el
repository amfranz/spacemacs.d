(defconst mfa-go-packages '(go-dlv projectile prodigy))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun mfa-go/post-init-projectile ()
  (push '(projectile-project-test-cmd . "go test") safe-local-variable-values)
  (push '(projectile-project-compilation-cmd . "go build") safe-local-variable-values))

(defun mfa-go/post-init-prodigy()
  (with-eval-after-load 'prodigy
    (prodigy-define-service
      :name "Go Documentation Server"
      :command "godoc"
      :args '("-http" "127.0.0.1:6060" "-v")
      :port 6060
      :url "http://localhost:6060/"
      :cwd user-home-directory)))
