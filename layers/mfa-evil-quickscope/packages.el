(defconst mfa-evil-quickscope-packages '(evil-quickscope))

(defun mfa-evil-quickscope/init-evil-quickscope ()
  (use-package evil-quickscope
    :config
    (global-evil-quickscope-mode 1)))
