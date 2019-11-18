;; -*- lexical-binding: t -*-

;; IntelliJ-alike indentation rules
;; use C-c C-o to set offset
;; use C-c C-s to show syntactic information (show the variable that needs to be set)
;; (add-hook 'java-mode-hook (lambda ()
;;                             (setq c-default-style "java")
;;                             (c-set-offset 'arglist-intro '+)
;;                             (c-set-offset 'arglist-close '0)
;;                             (c-set-offset 'case-label '+)
;;                             (display-line-numbers-mode 1)
;;                             (auto-complete-mode t)
;;                             ))

(defconst my-java-packages '(cc-mode
                             flycheck
                             kotlin-mode
                             lsp-java))

(defun my-java/post-init-cc-mode ()
  (add-hook 'java-mode-hook #'my-java//setup-java-mode t))

(defun my-java/post-init-flycheck ()
  (add-hook 'java-mode-hook #'flycheck-mode)
  (add-hook 'kotlin-mode-hook #'flycheck-mode))

(defun my-java/init-kotlin-mode ()
  (use-package kotlin-mode
    :defer t))

(defun my-java/post-init-lsp-java ()
  (setq lsp-java-autobuild-enabled nil))
