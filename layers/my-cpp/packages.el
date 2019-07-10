;; -*- lexical-binding: t -*-

(defconst my-cpp-packages '(cc-mode
                            cmake-font-lock
                            cmake-mode
                            eldoc-cmake
                            lsp-mode
                            qt-pro-mode
                            yaml-mode))

(defun my-cpp/post-init-cc-mode ()
  (spacemacs/add-to-hooks #'my-cpp//disable-escape-quotes-after-insert
                          c-c++-mode-hooks))

(defun my-cpp/init-eldoc-cmake ()
  (use-package eldoc-cmake
    :hook (cmake-mode . eldoc-cmake-enable)))

(defun my-cpp/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :init (advice-add 'cmake-font-lock-activate :override
                      #'my-cpp//cmake-font-lock-activate)
    :defer t))

(defun my-cpp/init-cmake-mode ()
  (use-package cmake-mode
    :defer t))

(defun my-cpp/post-init-lsp-mode ()
  (advice-add 'lsp-register-client :filter-args
              #'my-lsp//ccls-add-library-folders-fn))

(defun my-cpp/init-qt-pro-mode ()
  (use-package qt-pro-mode
    :mode ("\\.pro\\'" "\\.pri\\'")))

(defun my-cpp/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode)))
