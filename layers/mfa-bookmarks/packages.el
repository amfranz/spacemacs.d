(defconst mfa-bookmarks-packages '(bm helm-bm))

(defun mfa-bookmarks/post-init-bm ()
  (setq bm-cycle-all-buffers nil
        bm-highlight-style 'bm-highlight-only-fringe)

  ;; To re-define "ab" without triggering a safety check by Spacemacs we need to
  ;; clear the binding it first.
  (evil-leader/set-key "ab" nil)

  (spacemacs/declare-prefix "ab" "bookmarks")
  (spacemacs/set-leader-keys
    "aba" #'bm-bookmark-annotate
    "abb" #'helm-bm
    "abi" #'bm-bookmark-show-annotation
    "abc" #'bm-remove-all-current-buffer
    "abC" #'bm-remove-all-all-buffers
    "abn" #'spacemacs/bm-transient-state/bm-next
    "abp" #'spacemacs/bm-transient-state/bm-previous
    "abt" #'spacemacs/bm-transient-state/bm-toggle
    "abs" #'bm-show
    "abS" #'bm-show-all)

  ;; I can never remember when it is `N' or `p', let's just make both work.
  (spacemacs/transient-state-register-add-bindings 'bm
    '(("p" #'bm-previous)))

  ;; Eagerly load bookmarks when Emacs starts.
  (add-hook 'spacemacs-post-user-config-hook
            (lambda () (require 'bm)))

  ;; This advice is unnecessary when bookmarks are loaded eagerly.
  (advice-remove 'spacemacs/bm-transient-state/body #'bm-buffer-restore)

  (evil-set-initial-state 'bm-show-mode 'motion)
  (evil-define-key 'motion bm-show-mode-map
    "q" #'bm-show-quit-window
    (kbd "C-g") #'bm-show-quit-window
    (kbd "ESC") #'bm-show-quit-window
    "j" #'bm-show-next
    "k" #'bm-show-prev
    "v" #'bm-show-bookmark
    (kbd "RET") #'bm-show-goto-bookmark))

(defun mfa-bookmarks/init-helm-bm ()
  (use-package helm-bm
    :defer t))
