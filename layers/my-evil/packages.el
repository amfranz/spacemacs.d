;; -*- lexical-binding: t -*-

(defconst my-evil-packages '(drag-stuff
                             evil
                             (evil-escape :excluded t)
                             evil-nerd-commenter
                             evil-numbers
                             evil-vimish-fold))

(defun my-evil/init-drag-stuff ()
  (use-package drag-stuff
    :defer t
    :init
    (with-eval-after-load 'evil-maps
      (define-key evil-visual-state-map "J" #'drag-stuff-down)
      (define-key evil-visual-state-map "K" #'drag-stuff-up))))

(defun my-evil/post-init-evil ()
  ;; Do not move cursor back when exiting insert mode, and allow the cursor to
  ;; go past the last character of the line. This is more in line with default
  ;; Emacs behavior and feels more natural to me.
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t))

(defun my-evil/post-init-evil-nerd-commenter ()
  ;; Register text object for comments.
  (autoload 'evilnc-inner-comment "evil-nerd-commenter-operator")
  (define-key evil-inner-text-objects-map "c" #'evilnc-inner-comment)
  (autoload 'evilnc-outer-commenter "evil-nerd-commenter-operator")
  (define-key evil-outer-text-objects-map "c" #'evilnc-outer-commenter))

(defun my-evil/post-init-evil-numbers ()
  ;; The transient state for `evil-numbers' sounds like a good idea, but it is
  ;; not possible to use it with `evil-repeat', and that is a dealbreaker for
  ;; me. This rebinds `evil-numbers' functionaly without using the transient
  ;; state.
  (spacemacs/safe-set-leader-keys
    "nk" #'evil-numbers/inc-at-pt
    "nj" #'evil-numbers/dec-at-pt)
  ;; Disable key bindings that would activate the transient state.
  (spacemacs/set-leader-keys
    "n+" nil
    "n=" nil
    "n-" nil
    "n_" nil))

(defun my-evil/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :defer t
    :diminish
    :init
    (progn
      (setq vimish-fold-dir (concat spacemacs-cache-directory "vimish-fold/"))
      (add-hook 'prog-mode-hook #'evil-vimish-fold-mode))))
