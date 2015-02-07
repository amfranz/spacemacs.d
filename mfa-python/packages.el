(defconst mfa-python-packages '(python))

(defun mfa-python/post-init-python ()
  (with-eval-after-load 'python
    (add-hook 'python-mode-hook (lambda ()
                                  (setq evil-shift-width python-indent-offset)))))
