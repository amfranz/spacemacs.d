;; -*- lexical-binding: t -*-

(defun my-editorconfig//announce-changes (props)
  (maphash (lambda (key value)
             (when my-editorconfig-verbose
               (message "editorconfig set %s to %s" key value)))
           props))
