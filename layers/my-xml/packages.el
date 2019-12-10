;; -*- lexical-binding: t -*-

(defconst my-xml-packages '((nxml-mode :location built-in)))

(defun my-xml/init-nxml-mode ()
  ;; XML mode customizations
  (with-eval-after-load 'nxml-mode
    (setq-default nxml-child-indent 4)
    (add-hook 'nxml-mode-hook
              (lambda ()
                (setq evil-shift-width nxml-child-indent)))))
