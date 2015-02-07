(defconst mfa-xml-packages '(nxml-mode))

(defun mfa-xml/post-init-nxml-mode ()
  ;; XML mode customizations
  (with-eval-after-load 'nxml-mode
    (setq-default nxml-child-indent 4)
    (add-hook 'nxml-mode-hook
              (lambda ()
                (setq evil-shift-width nxml-child-indent)))))
