;; -*- lexical-binding: t -*-

(defconst my-python-packages '(python treemacs))

;; TODO: this is global, can this be made buffer-local?
;; maybe use `pyvenv-mode' vs `pyvenv-tracking-mode'?
(defun my-python//activate-virtualenv()
  (pyvenv-track-virtualenv)
  (when pyvenv-virtual-env
    (setq lsp-clients-python-library-directories (list "/usr/" pyvenv-virtual-env))))

(defun my-python/post-init-python ()
  (with-eval-after-load 'python
    (modify-syntax-entry ?_ "w" python-mode-syntax-table)
    (autoload 'pyvenv-track-virtualenv "pyvenv")
    (add-hook 'python-mode-local-vars-hook #'my-python//activate-virtualenv)))

(defun my-python/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    (add-to-list 'treemacs-ignored-file-predicates
                 #'my-python//treemacs-ignored-file-predicates)))
