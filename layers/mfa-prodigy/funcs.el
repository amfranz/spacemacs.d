;; -*- lexical-binding: t -*-

(defun mfa-prodigy//load-private-directory ()
  "Loads prodigy service definitions in ~/.emacs.d/private/prodigy/*.el"
  (let ((prodigy-directory (concat spacemacs-private-directory "prodigy/")))
    (when (file-directory-p prodigy-directory)
      (dolist (prodigy-el (directory-files prodigy-directory nil "\\.el\\'"))
        (load-file (concat prodigy-directory prodigy-el))))))
