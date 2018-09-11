;; -*- lexical-binding: t -*-

(defconst mfa-treemacs-packages '(treemacs treemacs-evil))

(defun mfa-treemacs/post-init-treemacs ()
  ;; Make the list of files a bit more tidy by default.
  (setq treemacs-show-hidden-files nil)

  ;; Make treemacs a bit less noisy.
  (setq treemacs-silent-refresh t)

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
    (evil-define-key 'treemacs treemacs-mode-map (kbd "l") #'treemacs-RET-action)

    ;; Move the rename functionality from "R" to "r". This overrides the binding
    ;; for refresh, but that is not a problem because refresh is available at
    ;; "gr" as well. "gr" should be the canonical binding anyway, based on the
    ;; Spacemacs key binding conventions.
    (evil-define-key 'treemacs treemacs-mode-map (kbd "r") #'treemacs-rename)
    (evil-define-key 'treemacs treemacs-mode-map (kbd "R") nil)

    ;; evil-treemacs does not define an evil specific key for `treemacs-resort',
    ;; so we'll define a key ourselves.
    (evil-define-key 'treemacs treemacs-mode-map (kbd "gs") #'treemacs-resort)))
