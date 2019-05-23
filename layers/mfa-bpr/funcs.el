;; -*- lexical-binding: t -*-

;; (defun bpr-create-window ()
;;   (let ((wnd (funcall split-window-preferred-function)))
;;     (when wnd
;;       (select-window wnd))
;;     wnd))

(defun bpr-go-build ()
  "Spawns 'go build' process"
  (interactive)
  (bpr-spawn "go build -i"))

(defun bpr-go-test ()
  "Spawns 'go test' process"
  (interactive)
  (bpr-spawn "go test -v"))

;; This needs improvement
(defun bpr-projectile-build ()
  "Spawns the projects build command"
  (interactive)
  (let ((default-directory (concat (projectile-project-root) projectile-project-compilation-dir)))
    (bpr-spawn projectile-project-compilation-cmd)))

;; (with-eval-after-load 'bpr
;;   (require 'shell)

;;   (defun bpr-buffer-quit ()
;;     (interactive)
;;     (bury-buffer)
;;     (delete-window))

;;   (define-derived-mode bpr-shell-mode
;;     shell-mode "BPR shell"
;;     "Major mode for BPR process output.
;;   \\{bpr-shell-mode-map}"
;;     (evil-motion-state))

;;   (define-key bpr-shell-mode-map "q" #'bpr-buffer-quit))
