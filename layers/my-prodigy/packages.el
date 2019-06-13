;; -*- lexical-binding: t -*-

(defconst my-prodigy-packages '(prodigy))

(defun my-prodigy/post-init-prodigy ()
  (with-eval-after-load 'prodigy
    ;; TODO submit this fix upstream
    (evil-define-key 'evilified prodigy-mode-map "c" nil)
    (evil-define-key 'evilified prodigy-view-mode-map
      "c" #'prodigy-view-clear-buffer)
    (my-prodigy//load-services)))
