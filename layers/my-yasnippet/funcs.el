;; -*- lexical-binding: t -*-

(defun my-yasnippet//preserve-point (orig-fun &rest args)
  (save-excursion
    (apply orig-fun args)))

(defun my-yasnippet//load-yasnippet-unless-scratch ()
  (unless (string= (buffer-name) "*scratch*")
    (spacemacs/load-yasnippet)))

(defun my-yasnippet//verify-lazy-load ()
  (when (featurep 'yasnippet)
    (warn "Yasnippet has been loaded during the startup sequence. This will \
break initialization of snippets. Please find what causes yasnippet to load \
eagerly and alter it to ensure yasnippet loads lazily.")))
