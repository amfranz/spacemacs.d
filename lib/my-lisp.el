;; -*- lexical-binding: t -*-

;;;###autoload
(defun lisp-sandbox ()
  "Create a scratch buffer in lisp interaction mode"
  (interactive)
  (switch-to-buffer (get-buffer-create "*lisp-sandbox*"))
  (lisp-interaction-mode))

(provide 'my-lisp)
