;; -*- lexical-binding: t -*-

(defconst my-markdown-packages '(flycheck
                                 grip-mode
                                 markdown-mode))

(defun my-markdown/post-init-flycheck ()
  (spacemacs/enable-flycheck 'markdown-mode)
  (with-eval-after-load 'flycheck
    (flycheck-add-next-checker 'markdown-markdownlint-cli 'proselint)))

(defun my-markdown/init-grip-mode ()
  (use-package grip-mode
    :init
    (with-eval-after-load 'markdown-mode
      (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
        "g" #'grip-mode))))

(defun my-markdown/post-init-markdown-mode ()
  (add-hook 'markdown-mode-hook #'spacemacs/toggle-visual-line-navigation-on))
