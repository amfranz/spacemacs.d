(defconst mfa-dash-packages '(zeal-at-point))

(defun mfa-dash/post-init-zeal-at-point ()
  (spacemacs/declare-prefix "d" "dash/zeal")
  (with-eval-after-load 'zeal-at-point
    (push '(emacs-lisp-mode . "elisp") zeal-at-point-mode-alist)))
