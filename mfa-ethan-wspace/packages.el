(defconst mfa-ethan-wspace-packages '(ethan-wspace))

(defun mfa-ethan-wspace/init-ethan-wspace ()
  (use-package ethan-wspace
    :diminish ethan-wspace-mode
    :init
    (spacemacs/set-leader-keys "xe" #'ethan-wspace-clean-all)
    :config
    (progn
      ;; Major modes shouldn't try to control the final newline any more.
      ;; ethan-wspace is doing that job now.
      (setq mode-require-final-newline nil)

      ;; Disable ethan-wspace when the buffer contains an image or is a process
      ;; buffer (eg. git commit messages).
      (defun ethan-wspace--inappropriate-buffer-advice (orig-fun &rest args)
        "Disables ethan-wspace in image and git commit message buffers."
        (unless
          (or (string-suffix-p "/COMMIT_EDITMSG" buffer-file-name)
              (memq major-mode '(image-mode)))
          (apply orig-fun args)))
      (advice-add 'ethan-wspace-is-buffer-appropriate :around
                  #'ethan-wspace--inappropriate-buffer-advice)

      (defun ethan-wspace--tabs-are-ok ()
        (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))

      ;; Tabs are ok in Makefiles.
      (with-eval-after-load 'make-mode
        (add-hook 'makefile-mode-hook #'ethan-wspace--tabs-are-ok))

      ;; Tabs are ok in Shell scripts.
      (with-eval-after-load 'sh-script
        (add-hook 'sh-mode-hook #'ethan-wspace--tabs-are-ok))

      ;; Tabs are ok in Go source code.
      (with-eval-after-load 'go-mode
        (add-hook 'go-mode-hook #'ethan-wspace--tabs-are-ok))

      ;; Use Ethan to clean whitespace.
      (global-ethan-wspace-mode 1))))
