;; -*- lexical-binding: t -*-

(defun my-treemacs/toggle-maximize-buffer ()
  "Maximize buffer but avoid deleting the Treemacs window."
  (interactive)
  (let ((single-window-count (if (treemacs-get-local-window) 2 1)))
    (if (and (= single-window-count (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (treemacs-delete-other-windows)))))

(defun my-treemacs//ignore-dot-git (file _)
  "Predicate to detect the git data directory.
Will return t when FILE is '.git'"
  (string-equal file ".git"))

(defun my-treemacs//enable-icons-dired-mode ()
  "Delays enabling `treemacs-icons-dired-mode' until the there is a buffer in
which `dired-mode' is enabled."
  (unless (bound-and-true-p treemacs-icons-dired-mode)
    (treemacs-icons-dired-mode)))
