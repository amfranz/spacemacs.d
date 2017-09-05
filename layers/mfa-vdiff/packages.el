(defconst mfa-vdiff-packages '(vdiff vdiff-magit))

(defun mfa-vdiff/init-vdiff ()
  (use-package vdiff
    :defer t
    :config
    (progn
      (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "]c" #'vdiff-next-hunk)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "[c" #'vdiff-previous-hunk)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zc" #'vdiff-close-fold)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zM" #'vdiff-close-all-folds)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zo" #'vdiff-open-fold)
      (evil-define-minor-mode-key 'normal 'vdiff-mode "zR" #'vdiff-open-all-folds)
      (evil-define-minor-mode-key 'motion 'vdiff-mode "go" #'vdiff-receive-changes)
      (evil-define-minor-mode-key 'motion 'vdiff-mode "gp" #'vdiff-send-changes))))

(defun mfa-vdiff/init-vdiff-magit ()
  (use-package vdiff-magit
    :defer t
    :init
    (with-eval-after-load 'magit
      (define-key magit-mode-map "e" #'vdiff-magit-dwim)
      (define-key magit-mode-map "E" #'vdiff-magit-popup)
      (setcdr (assoc ?e (plist-get magit-dispatch-popup :actions))
              '("vdiff dwim" #'vdiff-magit-dwim))
      (setcdr (assoc ?E (plist-get magit-dispatch-popup :actions))
              '("vdiff popup" #'vdiff-magit-popup)))))
