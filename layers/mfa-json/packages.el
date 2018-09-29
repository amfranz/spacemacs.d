;; -*- lexical-binding: t -*-

(defconst mfa-json-packages '(json-mode))

(defun mfa-json/post-init-json-mode ()
  ;; highlight numbers mode makes a mess out of JSONs syntax highlighting.
  (add-hook 'json-mode-hook #'mfa-json//disable-highlight-numbers-mode))
