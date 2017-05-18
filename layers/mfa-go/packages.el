(defconst mfa-go-packages '(go-dlv projectile))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))

(defun mfa-go/post-init-projectile ()
  (push '(projectile-project-test-cmd . "go test") safe-local-variable-values)
  (push '(projectile-project-compilation-cmd . "go build") safe-local-variable-values))
