;; -*- lexical-binding: t -*-

(defconst my-pass-packages '(auth-source-pass helm-pass pass))

(defun my-pass/init-auth-source-pass ()
  (use-package auth-source-pass
    :after (auth-source)
    :config
    (auth-source-pass-enable)))

(defun my-pass/init-helm-pass ()
  (use-package helm-pass
    :defer t
    :init
    (spacemacs/set-leader-keys "oapy" #'helm-pass)))

(defun my-pass/init-pass ()
  (use-package pass
    :defer t
    :init
    (spacemacs/set-leader-keys "oapp" #'pass)
    :config
    (progn
      ;; Evil-friendly key bindings.
      (evilified-state-evilify pass-mode pass-mode-map
        "[" #'pass-prev-directory
        "]" #'pass-next-directory
        "j" #'pass-next-entry
        "k" #'pass-prev-entry
        "d" #'pass-kill
        "y" #'pass-copy
        "gr" #'pass-update-buffer)

      ;; For proper user interface flow, when killing a pass-view buffer the
      ;; focus should go back to the password-store directory. In order for this
      ;; to happen, the password-store directory needs to be considered a
      ;; "useful" buffer.
      (push (regexp-quote pass-buffer-name) spacemacs-useful-buffers-regexp))))
