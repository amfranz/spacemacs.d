(defconst mfa-evil-packages '(evil evil-escape evil-matchit))

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
