(defconst mfa-checkbashisms-packages '(flycheck flycheck-checkbashisms))

(defun mfa-checkbashisms/init-flycheck-checkbashisms ()
  (use-package flycheck-checkbashisms
    :if (configuration-layer/package-usedp 'flycheck)
    :defer t))

(defun mfa-checkbashisms/post-init-flycheck ()
  (with-eval-after-load 'sh-script
    (flycheck-checkbashisms-setup)))
