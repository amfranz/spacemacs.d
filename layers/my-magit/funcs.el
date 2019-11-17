;; -*- lexical-binding: t -*-

(defvar postpone-auto-revert-buffers nil)
(defvar postpone-auto-revert-interval nil)

(defun postpone-auto-revert-buffers (orig-fun &rest args)
  "Delay `auto-revert-buffers' if `postpone-auto-revert-buffers' is non-nil."
  (if postpone-auto-revert-buffers
      ;; Do not run `auto-revert-buffers', but make its timer run more
      ;; frequently in the meantime, so that it will run promptly once
      ;; it's safe.  Remember the original `auto-revert-interval'.
      (unless postpone-auto-revert-interval
        (setq postpone-auto-revert-interval auto-revert-interval)
        (setq auto-revert-interval 0.5)
        (auto-revert-set-timer))
    ;; We are no longer postponed, so restore the original
    ;; `auto-revert-interval', and run `auto-revert-buffers'.
    (when postpone-auto-revert-interval
      (setq auto-revert-interval postpone-auto-revert-interval)
      (setq postpone-auto-revert-interval nil)
      (auto-revert-set-timer))
    (apply orig-fun args))) ;; Run `auto-revert-buffers'.

(defun postpone-auto-revert-buffers-on (&rest args)
  (setq postpone-auto-revert-buffers t))

(defun postpone-auto-revert-buffers-off (&rest args)
  (setq postpone-auto-revert-buffers nil))

(defun auto-revert-handler--ad-bug21559 (orig-fun)
  "Backport https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559."
  (let ((revert-buffer-in-progress-p t))
    (funcall orig-fun)))

(defun vc-git--ad-bug21559 (orig-fn &rest args)
  "Backport https://debbugs.gnu.org/cgi/bugreport.cgi?bug=21559."
  (let ((process-environment process-environment))
    (when revert-buffer-in-progress-p
      (push "GIT_OPTIONAL_LOCKS=0" process-environment))
    (apply orig-fn args)))

(defun my-magit//adjust-diff-theme-faces ()
  (when (memq 'zenburn custom-enabled-themes)
    (custom-theme-set-faces
     'zenburn
     `(magit-diff-added-highlight ((t (:inherit diff-added))))
     `(magit-diff-removed-highlight ((t (:inherit diff-removed)))))))

(defun ad-use-vdiff-instead-of-ediff (orig-fun &rest args)
  (cl-letf (((symbol-function 'ediff-files) #'vdiff-files))
    (apply orig-fun args)))

(defun my-magit//set-diff-tab-width ()
  (setq tab-width 4))

(defun my-magit/helm-magit-todos ()
  (interactive)
  ;; Both of these packages need to be loaded before the method becomes
  ;; available. This is because `magit-todos' uses `with-eval-after-load'
  ;; to wait for `helm' to be loaded before defining `helm-magit-todos'.
  (require 'helm)
  (require 'magit-todos)
  (helm-magit-todos))
