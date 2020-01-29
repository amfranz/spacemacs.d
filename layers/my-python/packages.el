;; -*- lexical-binding: t -*-

(defconst my-python-packages '(python treemacs))

(defun my-python/post-init-python ()
  (with-eval-after-load 'python
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    (autoload 'pyvenv-track-virtualenv "pyvenv")
    (add-hook 'python-mode-local-vars-hook #'pyvenv-track-virtualenv)))

(defun my-python/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-ignored-file-predicates
                 #'my-python//treemacs-ignored-file-predicates)))
