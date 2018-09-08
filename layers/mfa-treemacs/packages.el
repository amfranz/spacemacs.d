;; -*- lexical-binding: t -*-

(defconst mfa-treemacs-packages '(treemacs treemacs-evil))

(defun mfa-treemacs/post-init-treemacs ()
  (with-eval-after-load 'treemacs
    ;; Adjust icon sizes to the DPI of the display.
    (spacemacs|do-after-display-system-init
     (treemacs-resize-icons (* 22 (display-scaling-factor))))

    ;; Fixes icon background color. It's broken because Treemacs only adjusts it
    ;; as an after hook of `enable-theme', but due to lazy loading the theme
    ;; will have enabled long before treemacs gets loaded. See also:
    ;; https://github.com/Alexander-Miller/treemacs/issues/148
    (treemacs--setup-icon-background-colors)

    ;; Enhance the visibility of the currently selected line.
    (treemacs-fringe-indicator-mode t)))

(defun mfa-treemacs/post-init-treemacs-evil ()
  (with-eval-after-load 'treemacs-evil
    ;; Moving the root with h/l is non-sensical to me. This rebinds them to
    ;; functionality that, to me, seems more intuitive to be bound to those
    ;; keys.
    (evil-define-key 'treemacs treemacs-mode-map (kbd "h") #'treemacs-goto-parent-node)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-RET-action)))
