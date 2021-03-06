;; -*- lexical-binding: t -*-

(defconst my-atomic-chrome-packages '(atomic-chrome))

(defun my-atomic-chrome/init-atomic-chrome ()
  (use-package atomic-chrome
    :defer t
    :init
    (when (daemonp)
      (add-hook 'spacemacs-post-user-config-hook #'atomic-chrome-start-server))
    :config
    (progn
      (setq atomic-chrome-buffer-open-style 'frame)

      (when (configuration-layer/package-used-p 'markdown-mode)
        (add-to-list 'atomic-chrome-url-major-mode-alist '("github" . gfm-mode)))

      (when (configuration-layer/package-used-p 'dokuwiki-mode)
        (add-to-list 'atomic-chrome-url-major-mode-alist '("wiki" . dokuwiki-mode))))))
