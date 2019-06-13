;; -*- lexical-binding: t -*-

;; https://github.com/Lindydancer/cmake-font-lock/issues/6
(defun my-cpp//cmake-font-lock-activate()
  "Activate advanced CMake colorization.

To activate this every time a CMake file is opened, use the following:

    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)"
  (interactive)
  (cmake-font-lock-setup)
  (when (boundp 'font-lock-set-defaults)
    (kill-local-variable 'font-lock-set-defaults))
  (when (and (boundp 'font-lock-mode) font-lock-mode)
    (font-lock-mode -1)
    (font-lock-mode 1)))
