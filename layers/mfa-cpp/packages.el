;; -*- lexical-binding: t -*-

(defconst mfa-cpp-packages '(eldoc-cmake cmake-font-lock cmake-mode qt-pro-mode))

(defun mfa-cpp/init-eldoc-cmake ()
  (use-package eldoc-cmake
    :hook (cmake-mode . eldoc-cmake-enable)))

(defun mfa-cpp/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :init (advice-add 'cmake-font-lock-activate :override
                      #'mfa-cpp//cmake-font-lock-activate)
    :defer t))

(defun mfa-cpp/init-cmake-mode ()
  (use-package cmake-mode
    :defer t))

(defun mfa-cpp/init-qt-pro-mode ()
  (use-package qt-pro-mode
    :mode ("\\.pro\\'" "\\.pri\\'")))
