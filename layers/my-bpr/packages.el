(defconst my-bpr-packages '(bpr))

(defun my-bpr/init-bpr ()
  (use-package bpr
    :defer t
    :init
    (progn
      (setq ;; bpr-colorize-output t
       bpr-use-projectile nil
       bpr-process-mode #'compilation-mode
       ;; bpr-window-creator #'bpr-create-window
       bpr-scroll-direction -1)

      (with-eval-after-load 'go-mode
        (spacemacs/safe-set-leader-keys-for-major-mode 'go-mode
          "qb" #'bpr-go-build
          "qt" #'bpr-go-test
          "qq" #'bpr-open-last-buffer)))))
