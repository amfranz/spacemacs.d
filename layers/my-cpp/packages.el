;; -*- lexical-binding: t -*-

(defconst my-cpp-packages '(eldoc-cmake cmake-font-lock cmake-mode qt-pro-mode))

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

(defun my-cpp/init-qt-pro-mode ()
  (use-package qt-pro-mode
    :mode ("\\.pro\\'" "\\.pri\\'")))
