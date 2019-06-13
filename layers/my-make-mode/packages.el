;; -*- lexical-binding: t -*-

(defconst my-make-mode-packages '(company
                                  (make-mode :location built-in)))

(defun my-make-mode/post-init-company ()
  (add-hook 'makefile-mode-hook #'company-mode))

(defun my-make-mode/init-make-mode ()
  (use-package make-mode
    :defer t
    :config
    (progn
      ;; Adjust `tab-width'.
      (add-hook 'makefile-mode-hook #'my-make-mode//adjust-tab-width)

      ;; Adjust `evil-shift-width'.
      (push '(makefile-mode . tab-width) spacemacs--indent-variable-alist))))
