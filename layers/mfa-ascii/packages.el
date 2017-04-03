(defconst mfa-ascii-packages '(ascii))

(defun mfa-ascii/init-ascii ()
  (use-package ascii
    :defer t
    :init
    (spacemacs/set-leader-keys "xz" #'ascii-display)
    :config
    (push `(,(concat "^" (regexp-quote ascii-buffer-name) "$") . motion)
          evil-buffer-regexps)))
