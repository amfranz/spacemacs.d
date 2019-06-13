;; -*- lexical-binding: t -*-

(defconst my-help-packages '(help-fns+
                             ;; This Conflicts with the location in `spacemacs-default-packages'
                             ;; which fetches an older version of `help-fns+' from emacsmirror.
                             ;; (help-fns+ :location (recipe :fetcher url
                             ;;                              :url "https://www.emacswiki.org/emacs/download/help-fns+.el"))
                             ))

(defun my-help/post-init-help-fns+ ()
  ;; `help-fns+' customizes the faces in `help-mode' and `Info-mode' buffers.
  ;; I want to avoid the buffers looking differently depending on whether
  ;; it was already loaded or not. Let's load it early.
  (with-eval-after-load 'help-fns
    (require 'help-fns+)))
