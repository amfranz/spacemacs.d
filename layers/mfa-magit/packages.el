;; -*- lexical-binding: t -*-

(defconst mfa-magit-packages '((diff-mode :location built-in)
                               diffview
                               evil-collection
                               magit
                               magit-gitflow
                               magit-todos
                               vdiff
                               vdiff-magit
                               whitespace))

(defun mfa-magit/post-init-diff-mode ()
  ;; Do not automatically trim trailing whitespace when saving edited diff
  ;; buffers, in those buffers whitespace is significant.
  (with-eval-after-load 'ws-butler
    (push 'diff-mode ws-butler-global-exempt-modes))

  ;; Granted there is no correct tab width for diff buffers, but arguably
  ;; the default width of 2 is too short.
  (add-hook 'diff-mode-hook #'mfa-magit//set-diff-tab-width))

(defun mfa-magit/init-diffview ()
  (use-package diffview
    :defer t
    :init
    (dolist (mode '(diff-mode magit-diff-mode))
      (spacemacs/safe-set-leader-keys-for-major-mode mode
        "v" #'diffview-current))))

(defun mfa-magit/init-evil-collection()
  (use-package evil-collection
    :defer t))

(defun mfa-magit/post-init-magit ()
  ;; Display `magit-status' in the current instead of other window.
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)

  ;; Always show word-granularity differences within diff hunks.
  (setq magit-diff-refine-hunk 'all)

  ;; Keep the cursor vertically centered in the status buffer. It avoids the
  ;; unexpected and visually scarring scrolling of the buffer content when hunks
  ;; are (un-)staged.
  (add-hook 'magit-status-mode-hook #'centered-cursor-mode)

  ;; Theme diff faces.
  (with-eval-after-load 'magit-diff
    (add-hook 'spacemacs-post-theme-change-hook
              #'mfa-magit//adjust-diff-theme-faces)
    (mfa-magit//adjust-diff-theme-faces))

  ;; Fixes git index lock errors by `auto-revert-mode' during concurrent rebase
  ;; operations. For details see https://github.com/magit/magit/issues/2708.
  (when (< emacs-major-version 27)
    (advice-add 'auto-revert-handler :around #'auto-revert-handler--ad-bug21559)
    (advice-add 'vc-git--call :around #'vc-git--ad-bug21559)
    (advice-add 'vc-git-command :around #'vc-git--ad-bug21559))

  ;; Fixes issues caused by auto-revert-buffers during rebases. For details, see
  ;; https://github.com/magit/magit/issues/2708
  (advice-add 'auto-revert-buffers :around #'postpone-auto-revert-buffers)
  (advice-add 'magit-process-filter :before #'postpone-auto-revert-buffers-on)
  (advice-add 'magit-process-finish :before #'postpone-auto-revert-buffers-off)

  (with-eval-after-load 'magit
    ;; Using different purposes for each Magit window ensures that the fixup
    ;; commit uses a split window layout: `magit-log-select' / `magit-diff'
    (purpose-x-magit-multi-on)))

(defun mfa-magit/pre-init-magit-gitflow ()
  (spacemacs|use-package-add-hook magit-gitflow
    :post-config
    (spacemacs|diminish magit-gitflow-mode)))

(defun mfa-magit/init-magit-todos ()
  (use-package magit-todos
    :after magit
    :config
    (progn
      (evil-collection-init 'magit-todos)
      (magit-todos-mode))))

(defun mfa-magit/init-vdiff ()
  (use-package vdiff
    :defer t
    :init
    (advice-add 'spacemacs/ediff-dotfile-and-template
                :around #'ad-use-vdiff-instead-of-ediff)
    :config
    (progn
      (evil-define-minor-mode-key 'normal 'vdiff-mode ",d" vdiff-mode-prefix-map)
      (evil-collection-init 'vdiff))))

(defun mfa-magit/init-vdiff-magit ()
  (use-package vdiff-magit
    :defer t
    :init
    (with-eval-after-load 'magit
      (define-key magit-mode-map "e" #'vdiff-magit-dwim)
      (define-key magit-mode-map "E" #'vdiff-magit)
      (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
      (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
      (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
      (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))))

(defun mfa-magit/post-init-whitespace ()
  ;; Don't enable visual whitespace in diffs, the extra symbols are distracting.
  (remove-hook 'diff-mode-hook 'whitespace-mode))
