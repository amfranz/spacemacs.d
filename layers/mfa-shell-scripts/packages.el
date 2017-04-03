(defconst mfa-shell-scripts-packages '(sh-script))

(defun mfa-shell-scripts/post-init-sh-script ()
  (with-eval-after-load 'sh-script
    (add-hook 'sh-mode-hook
              (lambda ()
                (setq tab-width 4
                      indent-tabs-mode t
                      evil-shift-width sh-indentation)))))
