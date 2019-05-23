;; -*- lexical-binding: t -*-

(defconst mfa-layouts-packages '(eyebrowse))

(defun mfa-layouts/post-init-eyebrowse ()
  (with-eval-after-load 'evil-evilified-state
    (dolist (map `(,evil-evilified-state-map
                   ,evil-evilified-state-map-original))
      (define-key map "gt" #'eyebrowse-next-window-config)
      (define-key map "gT" #'eyebrowse-prev-window-config))))
