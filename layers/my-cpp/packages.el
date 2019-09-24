;; -*- lexical-binding: t -*-

(defconst my-cpp-packages '(cc-mode
                            ccls
                            cmake-font-lock
                            cmake-mode
                            eldoc-cmake
                            lsp-mode
                            modern-cpp-font-lock
                            qt-pro-mode
                            yaml-mode))

(defun my-cpp/post-init-cc-mode ()
  (spacemacs/add-to-hooks #'my-cpp//disable-escape-quotes-after-insert
                          c-c++-mode-hooks)
  (with-eval-after-load 'cc-mode
    (setf (alist-get 'other c-default-style) "linux")))

(defun my-cpp/post-init-ccls ()
  (spacemacs/add-to-hooks #'my-cpp//ccls-use-project-build-directory
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
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))
  (advice-add 'lsp-register-client :filter-args
              #'my-lsp//ccls-add-library-folders-fn))

(defun my-cpp/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :hook (c++-mode . modern-c++-font-lock-mode)
    :diminish modern-c++-font-lock-mode))

(defun my-cpp/init-qt-pro-mode ()
  (use-package qt-pro-mode
    :mode ("\\.pro\\'" "\\.pri\\'")))

(defun my-cpp/post-init-yaml-mode ()
  (add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-mode)))
