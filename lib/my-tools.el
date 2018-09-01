;; -*- lexical-binding: t -*-

;;;###autoload
(defun open-terminal ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "konsole" nil 0 nil "--workdir" (expand-file-name default-directory))))

;;;###autoload
(defun projectile-open-terminal ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (open-terminal)))

;;;###autoload
(defun open-file-manager ()
  (interactive)
  (let ((process-environment (cons "EMACS_SOCKET_NAME" initial-environment)))
    (call-process "xdg-open" nil 0 nil (expand-file-name default-directory))))
