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
