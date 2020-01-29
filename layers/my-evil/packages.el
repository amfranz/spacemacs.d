;; -*- lexical-binding: t -*-

(defconst my-evil-packages '(drag-stuff
                             evil
                             (evil-escape :excluded t)
                             evil-nerd-commenter))

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
