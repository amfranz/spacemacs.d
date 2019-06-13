;; -*- lexical-binding: t -*-

(spacemacs|define-jump-handlers yaml-mode)

(defun spacemacs//init-jump-handlers-yaml-mode ()
  (setq spacemacs-jump-handlers spacemacs-jump-handlers-yaml-mode))

(defun ansible-jump-to-template ()
  (interactive)
  (when (bound-and-true-p ansible)
    (let ((symbol (thing-at-point 'filename 'no-properties)))
      (unless (or (file-remote-p symbol)
                  (f-absolute? symbol))
        (when-let (file (cl-loop for section in '("files" "tasks" "templates")
                                 for file = (concat "../" section "/" symbol)
                                 if (f-exists? file) return file))
          (find-file file))))))

(add-to-list 'spacemacs-jump-handlers-yaml-mode #'ansible-jump-to-template)
