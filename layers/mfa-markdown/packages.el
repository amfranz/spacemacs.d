(defconst mfa-markdown-packages '(flycheck))

(defun mfa-markdown/post-init-flycheck ()
  (spacemacs/enable-flycheck 'markdown-mode)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'markdown-markdownlint-cli 'proselint)))
