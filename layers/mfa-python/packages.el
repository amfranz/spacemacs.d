(defconst mfa-python-packages '(python))

(defun mfa-python/post-init-python ()
  (with-eval-after-load 'python
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    (add-hook 'python-mode-hook (lambda ()
                                  (setq evil-shift-width python-indent-offset)))
    (autoload 'pyvenv-track-virtualenv "pyvenv")
    (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)))
