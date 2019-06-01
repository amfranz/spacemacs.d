;; -*- lexical-binding: t -*-

(defconst mfa-bookmarks-packages '(bm helm-bm))

(defun mfa-bookmarks/post-init-bm ()
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  ;; Customize the bm transient state.
  (add-hook 'spacemacs-post-user-config-hook
            #'mfa-bookmarks//customize-transient-state
            'append)

  ;; The Spacemacs `bm' layer registers this exact same hook in the use-package
  ;; config section. By registering the hook early, in the use-package init
  ;; section, we ensure that the `bm' package gets lazily loaded when the user
  ;; opens the first file.
  (add-hook 'find-file-hook #'bm-buffer-restore)

  ;; Calling `bm-buffer-restore' to load the bookmark repository is unnecessary
  ;; extra work for Emacs, a `require' of the `bm' package is sufficient.
  (advice-remove 'spacemacs/bm-transient-state/body #'bm-buffer-restore)
  (advice-add 'spacemacs/bm-transient-state/body
              :before #'mfa-bookmarks//load-repository)

  (with-eval-after-load 'bm
    (evil-set-initial-state 'bm-show-mode 'motion)
    (evil-define-key 'motion bm-show-mode-map
      "q" #'bm-show-quit-window
      "j" #'bm-show-next
      "k" #'bm-show-prev
      "v" #'bm-show-bookmark
      (kbd "RET") #'bm-show-goto-bookmark)))

(defun mfa-bookmarks/init-helm-bm ()
  (use-package helm-bm
    :defer t))
