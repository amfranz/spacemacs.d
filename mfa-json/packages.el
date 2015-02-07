(defconst mfa-json-packages '(json-mode))

(defun mfa-json/post-init-json-mode ()
  ;; highlight numbers mode makes a mess out of JSONs syntax highlighting.
  (with-eval-after-load 'json-mode
    (add-hook 'json-mode-hook (lambda () (highlight-numbers-mode -1)))))
