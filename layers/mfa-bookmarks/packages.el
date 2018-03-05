(defconst mfa-bookmarks-packages '(helm-bm bm))

(defun mfa-bookmarks/init-helm-bm ()
  (use-package helm-bm
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "[" "bookmarks")
      (spacemacs/set-leader-keys "[[" #'helm-bm))))

(defun mfa-bookmarks/init-bm ()
  (use-package bm
    :init
    (progn
      (defun bm-toggle-and-annotate ()
        "Toggle bookmark at point and annotate it."
        (interactive)
        (let ((bm-annotate-on-create t))
          (bm-toggle)))

      (spacemacs|define-micro-state bm
        :doc "[t]oggle [a]nnotate [n]ext [p]revious [q]uit"
        :evil-leader "[."
        :bindings
        ("t" bm-toggle)
        ("n" bm-next)
        ("p" bm-previous)
        ("q" nil :exit t))

      (spacemacs/set-leader-keys
        "[a" #'bm-bookmark-annotate
        "[i" #'bm-bookmark-show-annotation
        "[c" #'bm-remove-all-current-buffer
        "[C" #'bm-remove-all-all-buffers
        "[b" #'bm-toggle-cycle-all-buffers
        "[n" #'bm-next
        "[p" #'bm-previous
        "[P" #'bm-toggle-buffer-persistence
        "[t" #'bm-toggle
        "[T" #'bm-toggle-and-annotate
        "[r" #'bm-bookmark-regexp
        "[l" #'bm-bookmark-line
        "[s" #'bm-show
        "[S" #'bm-show-all
        "[h" #'helm-bookmarks))
    :config
    (progn
      (setq-default bm-buffer-persistence t)
      (setq bm-highlight-style 'bm-highlight-only-fringe
            bm-repository-file (concat spacemacs-cache-directory "bm-repository.el")
            bm-repository-size 200)

      (evil-set-initial-state 'bm-show-mode 'motion)
      (evil-define-key 'motion bm-show-mode-map
        "q" #'bm-show-quit-window
        (kbd "C-g") #'bm-show-quit-window
        (kbd "ESC") #'bm-show-quit-window
        "j" #'bm-show-next
        "k" #'bm-show-prev
        "v" #'bm-show-bookmark
        (kbd "RET") #'bm-show-goto-bookmark)

      (add-hook 'find-file-hook #'bm-buffer-restore)
      (add-hook 'kill-buffer-hook #'bm-buffer-save)
      (add-hook 'kill-emacs-hook #'bm-save)
      (add-hook 'after-save-hook #'bm-buffer-save)
      (add-hook 'after-revert-hook #'bm-buffer-restore)
      (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

      (bm-repository-load))))
