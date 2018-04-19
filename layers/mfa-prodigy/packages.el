;; -*- lexical-binding: t -*-

(defconst mfa-prodigy-packages '(prodigy))

(defun mfa-prodigy/post-init-prodigy ()
  (with-eval-after-load 'prodigy
    (evilified-state-evilify prodigy-view-mode prodigy-view-mode-map
      "K" #'prodigy-view-clear-buffer)
    (mfa-prodigy//load-private-directory)))
