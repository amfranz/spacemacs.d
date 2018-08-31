;; -*- lexical-binding: t -*-

(defconst mfa-magit-packages '(magit))

(defun mfa-magit/post-init-magit ()
  ;; Fixes issues caused by auto-revert-buffers during rebases. For details, see
  ;; https://github.com/magit/magit/issues/2708
  (advice-add 'auto-revert-buffers :around #'postpone-auto-revert-buffers)
  (advice-add 'magit-process-filter :before #'postpone-auto-revert-buffers-on)
  (advice-add 'magit-process-finish :before #'postpone-auto-revert-buffers-off))
