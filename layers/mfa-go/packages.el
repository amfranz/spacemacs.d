(defconst mfa-go-packages '(go-dlv))

(defun mfa-go/init-go-dlv ()
  (use-package go-dlv
    :defer t))
