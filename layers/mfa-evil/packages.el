(defconst mfa-evil-packages '(evil evil-escape evil-matchit evil-nerd-commenter evil-replace-with-register))

(defun mfa-evil/post-init-evil ()
  (with-eval-after-load 'evil
    ;; Do not move cursor back when exiting insert mode.
    (setq evil-move-cursor-back nil)))

(defun mfa-evil/post-init-evil-escape ()
  (with-eval-after-load 'evil-escape
    ;; A remapped Caps Lock to be ESC/Ctrl via xcape makes evil-escape unnecessary for me.
    (evil-escape-mode -1)))

(defun mfa-evil/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    ;; Turn on evils advanced tag matching.
    (global-evil-matchit-mode 1)))

(defun mfa-evil/post-init-evil-nerd-commenter ()
  ;; Register text object for comments.
  (autoload 'evilnc-get-comment-bounds "evil-nerd-commenter-operator")
  (define-key evil-inner-text-objects-map "c" #'evilnc-inner-comment)
  (define-key evil-outer-text-objects-map "c" #'evilnc-outer-commenter))

(defun mfa-evil/init-evil-replace-with-register ()
  (use-package evil-replace-with-register
    :defer t
    :init
    (progn
      (setq evil-replace-with-register-key (kbd "gR"))
      (define-key evil-normal-state-map
        evil-replace-with-register-key #'evil-replace-with-register)
      (define-key evil-visual-state-map
        evil-replace-with-register-key #'evil-replace-with-register))))
