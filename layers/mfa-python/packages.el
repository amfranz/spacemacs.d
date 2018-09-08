;; -*- lexical-binding: t -*-

(defconst mfa-python-packages '(python filetree))

(defun mfa-python/post-init-python ()
  (with-eval-after-load 'python
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    (autoload 'pyvenv-track-virtualenv "pyvenv")
    (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)))

(defun mfa-python/post-init-filetree ()
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-ignored-file-predicates
                 #'mfa-python//treemacs-ignored-file-predicates)))
