;; -*- lexical-binding: t -*-

(defun mfa-treemacs/toggle-maximize-buffer ()
  "Maximize buffer but avoid deleting the Treemacs window."
  (interactive)
  (let ((single-window-count (if (treemacs-get-local-window) 2 1)))
    (if (and (= single-window-count (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (treemacs-delete-other-windows)))))

(defun mfa-treemacs//ignore-dot-git (file _)
  "Predicate to detect the git data directory.
Will return t when FILE is '.git'"
  (string-equal file ".git"))
