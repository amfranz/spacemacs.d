(defconst mfa-vagrant-packages '(vagrant-tramp))

(defun mfa-vagrant/init-vagrant-tramp ()
  (use-package vagrant-tramp
    :defer t))
