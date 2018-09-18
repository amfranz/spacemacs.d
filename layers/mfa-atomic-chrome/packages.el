;; -*- lexical-binding: t -*-

(defconst mfa-atomic-chrome-packages '(atomic-chrome))

(defun mfa-atomic-chrome/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t
    :init
    (when (and (daemonp) (display-assume-graphic-p))
      (add-hook 'spacemacs-post-user-config-hook #'atomic-chrome-start-server))
    :config
    (setq atomic-chrome-buffer-open-style 'frame
          atomic-chrome-extension-type-list '(atomic-chrome)
          atomic-chrome-url-major-mode-alist '(("github" . gfm-mode)
                                               ("wiki" . dokuwiki-mode)))))
